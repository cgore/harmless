;;; harmless-auto.el --- Review a tool call before asking -*- lexical-binding: t; -*-

;;;; Copyright (c) 2026, Christopher Mark Gore,
;;;; Soli Deo Gloria,
;;;; All rights reserved.
;;;;
;;;; 22 Forest Glade Court, Saint Charles, Missouri 63304 USA.
;;;; Web: http://cgore.com
;;;; Email: cgore@cgore.com
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are met:
;;;;
;;;;     * Redistributions of source code must retain the above copyright
;;;;       notice, this list of conditions and the following disclaimer.
;;;;
;;;;     * Redistributions in binary form must reproduce the above copyright
;;;;       notice, this list of conditions and the following disclaimer in the
;;;;       documentation and/or other materials provided with the distribution.
;;;;
;;;;     * Neither the name of Christopher Mark Gore nor the names of other
;;;;       contributors may be used to endorse or promote products derived from
;;;;       this software without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;;; LIABLE FOR DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;;
;; In `auto' permission mode, a call that would ask is reviewed first.
;; `harmless-auto-review-function', when set, is the classifier.
;; Otherwise the session's model is asked and is given no tools.  Only
;; `allow' skips the question.  Anything else escalates.  The reviewer
;; does not run the tool and is not saved on the session.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-provider)
(require 'harmless-session)
(require 'harmless-tools)

(defconst harmless-auto-review-instructions
  (concat "You review one tool call. Reply with one word: "
          "allow or escalate. allow means ordinary project work. "
          "escalate means a person should decide. "
          "Do not explain. Do not call a tool.")
  "Instructions for the reviewer model.
The reply is one word, allow or escalate.")

(defcustom harmless-auto-review-function nil
  "Classifier for `auto' permission mode, or nil.
Called as (SESSION TOOL ARGS).  Return `allow' or `escalate'.
Nil asks the reviewer model.  Any other return, or an error,
escalates.  This function is not saved on the session."
  :type '(choice (const nil) function)
  :group 'harmless)

(defvar harmless-auto-review-model-function
  #'harmless-auto-review-model
  "Function (SESSION TOOL ARGS CALLBACK) that asks the reviewer.
CALLBACK receives `allow' or `escalate'.  Bind this in tests.")

(defun harmless-auto-review-parse (text)
  "Return `allow' or `escalate' for TEXT.
Only a reply that is exactly one of those words is kept.  Case
and surrounding whitespace do not matter.  Nil, \"\", and any
other reply escalate."
  (let ((word (downcase (string-trim (if (stringp text) text "")))))
    (if (member word '("allow" "escalate"))
        (intern word)
      'escalate)))

(defun harmless-auto-review--value (value)
  "Return VALUE as one reviewer token.
A string is quoted.  Nil is the word nil.  `:false' is false."
  (cond
   ((null value) "nil")
   ((eq value :false) "false")
   ((stringp value) (format "%S" value))
   ((or (symbolp value) (numberp value)) (format "%s" value))
   (t (format "%S" value))))

(defun harmless-auto-review-prompt (tool args)
  "Return the user text that describes TOOL and ARGS.
Nil arguments are the word none.  A string is copied.  A plist
keeps its order.  The text is not resolved as a path."
  (let ((name (cond
               ((and (harmless-tool-p tool))
                (harmless-tool-name tool))
               ((stringp tool) tool)
               (t "")))
        (body
         (cond
          ((null args) "none")
          ((stringp args) args)
          ((and (listp args) (keywordp (car args)))
           (let (parts)
             (while args
               (push (format "%s=%s"
                             (substring (symbol-name (car args)) 1)
                             (harmless-auto-review--value (cadr args)))
                     parts)
               (setq args (cddr args)))
             (mapconcat #'identity (nreverse parts) " ")))
          (t (format "%S" args)))))
    (concat "Tool: " name "\nArguments: " body)))

(defun harmless-auto-review-model (session tool args callback)
  "Ask SESSION's model about TOOL and ARGS, then call CALLBACK.
The request has no tools.  A missing provider, an error, a tool
call, or a reply other than allow or escalate calls CALLBACK
with `escalate'.  CALLBACK is called once.  An error raised by
CALLBACK is not turned into an escalation."
  (let ((provider (and session (harmless-session-provider session)))
        (text "")
        (saw-tool nil)
        (done nil)
        (entered nil)
        (proc nil))
    (cl-labels
        ((finish (decision)
           (unless done
             (setq done t)
             (when (and session (processp proc)
                        (eq (harmless-session-process session) proc))
               (setf (harmless-session-process session) nil))
             (funcall callback
                      (if (eq decision 'allow) 'allow 'escalate)))))
      (if (not provider)
          (finish 'escalate)
        (condition-case err
            (let ((harmless-current-model
                   (harmless-session-model session))
                  (harmless-current-reasoning-effort
                   (harmless-session-effective-reasoning-effort
                    session)))
              (setq proc
                    (harmless-provider-complete
                     provider
                     (list (list :role :system
                                 :content
                                 harmless-auto-review-instructions)
                           (list :role :user
                                 :content (harmless-auto-review-prompt
                                           tool args)))
                     nil
                     (lambda (event)
                       (setq entered t)
                       (pcase event
                         (`(:text ,chunk)
                          (when (stringp chunk)
                            (setq text (concat text chunk))))
                         (`(:tool-call . ,_)
                          (setq saw-tool t))
                         (`(:error ,_)
                          (finish 'escalate))
                         (`(:stop ,_)
                          (finish (if saw-tool
                                      'escalate
                                    (harmless-auto-review-parse
                                     text))))))))
              (when (and session (processp proc) (not done))
                (setf (harmless-session-process session) proc)))
          (error
           (when entered
             (signal (car err) (cdr err)))
           (finish 'escalate)))))))

(defun harmless-auto-review (session tool args callback)
  "Review TOOL with ARGS on SESSION, then call CALLBACK.
CALLBACK receives `allow' or `escalate'.  A classifier is used
when `harmless-auto-review-function' is a function.  Otherwise
the reviewer model is asked.  An error or any other answer
escalates.  This does not ask the user and does not run TOOL.
An error raised by CALLBACK is not turned into an escalation."
  (let ((done nil))
    (cl-labels
        ((finish (decision)
           (unless done
             (setq done t)
             (funcall callback
                      (if (eq decision 'allow) 'allow 'escalate)))))
      (cond
       ((functionp harmless-auto-review-function)
        (finish
         (condition-case nil
             (funcall harmless-auto-review-function session tool args)
           (error 'escalate))))
       ((functionp harmless-auto-review-model-function)
        (let ((called nil))
          (condition-case err
              (funcall harmless-auto-review-model-function
                       session tool args
                       (lambda (decision)
                         (setq called t)
                         (finish decision)))
            (error
             (when called
               (signal (car err) (cdr err)))
             (finish 'escalate)))))
       (t (finish 'escalate))))))

(provide 'harmless-auto)

;;; harmless-auto.el ends here
