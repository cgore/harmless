;;; harmless-turn.el --- Agent turn loop for Harmless -*- lexical-binding: t; -*-

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
;; One user message starts a turn: call the provider, stream events, run
;; tools with permission, call the provider again until a text stop.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-log)
(require 'harmless-provider)
(require 'harmless-session)
(require 'harmless-instructions)
(require 'harmless-skills)
(require 'harmless-tools)
(require 'harmless-perm)

(declare-function harmless-tools-shell-kill "harmless-tools-shell")

(cl-defstruct (harmless-turn-acc
               (:constructor harmless-turn-acc-create)
               (:copier nil))
  (text "")
  (reasoning "")
  (tools (make-hash-table :test 'equal))
  (order nil)
  stop-reason
  prompt-tokens
  completion-tokens
  (finished nil))

(defun harmless-turn--acc-tool (acc id name)
  "Return the tool-call plist for ID in ACC, creating it if needed."
  (or (gethash id (harmless-turn-acc-tools acc))
      (let ((entry (list :id id :name name :args "")))
        (puthash id entry (harmless-turn-acc-tools acc))
        (setf (harmless-turn-acc-order acc)
              (append (harmless-turn-acc-order acc) (list id)))
        entry)))

(defun harmless-turn-abort (session)
  "Cancel SESSION's in-flight HTTP or shell process."
  (when-let* ((proc (harmless-session-process session)))
    (when (process-live-p proc)
      (if (string-prefix-p "harmless-shell" (process-name proc))
          (progn
            (require 'harmless-tools-shell)
            (harmless-tools-shell-kill proc))
        (harmless-provider-abort (harmless-session-provider session) proc))))
  (setf (harmless-session-process session) nil)
  (harmless-session-set-status session 'idle)
  (harmless-emit session '(:error "aborted")))

(defun harmless-turn-run (session text)
  "Append user TEXT to SESSION and start the agent loop."
  (when (memq (harmless-session-status session) '(streaming waiting-permission))
    (harmless-turn-abort session))
  (harmless-session-append-user session text)
  (harmless-turn--call session))

(defun harmless-turn--call (session)
  "Send SESSION messages to the provider."
  (let ((acc (harmless-turn-acc-create))
        (harmless-current-model (harmless-session-model session))
        (harmless-current-reasoning-effort
         (harmless-session-effective-reasoning-effort session)))
    (harmless-session-set-status session 'streaming)
    (let ((proc (harmless-provider-complete
                 (harmless-session-provider session)
                 (harmless-context-messages
                  (harmless-session-cwd session)
                  (harmless-session-messages session))
                 (harmless-tools-enabled)
                 (lambda (event)
                   (harmless-turn--on-event session acc event)))))
      (setf (harmless-session-process session) proc)
      proc)))

(defun harmless-turn--on-event (session acc event)
  "Handle one canonical EVENT for SESSION, accumulating into ACC."
  (unless (harmless-turn-acc-finished acc)
  (harmless-emit session event)
  (pcase event
    (`(:text ,s)
     (setf (harmless-turn-acc-text acc)
           (concat (harmless-turn-acc-text acc) s)))
    (`(:reasoning ,s)
     (setf (harmless-turn-acc-reasoning acc)
           (concat (harmless-turn-acc-reasoning acc) s)))
    (`(:tool-call ,id ,name ,delta)
     (let ((entry (harmless-turn--acc-tool acc (or id (format "call-%s" (random)))
                                           name)))
       (when name
         (setf (plist-get entry :name) name))
       (when (stringp delta)
         (setf (plist-get entry :args)
               (concat (plist-get entry :args) delta)))))
    (`(:usage ,p ,c)
     (setf (harmless-turn-acc-prompt-tokens acc) p
           (harmless-turn-acc-completion-tokens acc) c)
     (cl-incf (harmless-session-prompt-tokens session) (or p 0))
     (cl-incf (harmless-session-completion-tokens session) (or c 0)))
    (`(:error ,err)
     (setf (harmless-turn-acc-finished acc) t)
     (harmless-session-set-status session 'error)
     (harmless-log "turn error: %s" err))
    (`(:stop ,reason)
     (setf (harmless-turn-acc-stop-reason acc) reason
           (harmless-turn-acc-finished acc) t)
     (setf (harmless-session-process session) nil)
     (harmless-turn--after-stop session acc)))))

(defun harmless-turn--tool-calls (acc)
  "Return completed tool-call plists from ACC in order."
  (cl-loop for id in (harmless-turn-acc-order acc)
           collect (gethash id (harmless-turn-acc-tools acc))))

(defun harmless-turn--after-stop (session acc)
  "Finish the assistant message on SESSION and maybe run tools."
  (let ((calls (harmless-turn--tool-calls acc))
        (text (harmless-turn-acc-text acc))
        (reasoning (harmless-turn-acc-reasoning acc)))
    (harmless-session-append
     session
     (nconc (list :role :assistant :content text)
            (and (not (string-empty-p reasoning))
                 (list :reasoning reasoning))
            (and calls (list :tool-calls calls))))
    (if calls
        (harmless-turn--run-tools session calls
                                  (lambda ()
                                    (harmless-turn--call session)))
      (harmless-session-set-status session 'idle))))

(defun harmless-turn--run-tools (session calls k)
  "Run CALLS sequentially on SESSION, then call K."
  (if (null calls)
      (funcall k)
    (harmless-turn--run-one
     session (car calls)
     (lambda ()
       (harmless-turn--run-tools session (cdr calls) k)))))

(defun harmless-turn--item-for (tool args)
  "Short description of TOOL with ARGS for a permission prompt."
  (pcase (harmless-tool-class tool)
    ('edit (or (harmless-tool-arg args :path) (harmless-tool-name tool)))
    ('shell (or (harmless-tool-arg args :command) (harmless-tool-name tool)))
    (_ (harmless-tool-name tool))))

(defun harmless-turn--invoke (tool session args callback)
  "Invoke TOOL and call CALLBACK with the string result."
  (if (eq (harmless-tool-class tool) 'shell)
      (funcall (harmless-tool-fn tool) session args callback)
    (funcall callback (harmless-tool-call tool session args))))

(defun harmless-turn--run-one (session call k)
  "Run one CALL plist on SESSION, then K."
  (let* ((id (plist-get call :id))
         (name (plist-get call :name))
         (raw (plist-get call :args))
         (args (harmless-tool-parse-args raw))
         (tool (harmless-tool-by-name name)))
    (if (not tool)
        (progn
          (harmless-session-append
           session (list :role :tool :id id :name name
                         :content (format "Error: unknown tool %s" name)))
          (funcall k))
    (harmless-perm-confirm
     session
     (harmless-tool-class tool)
     (harmless-turn--item-for tool args)
     (lambda (decision)
       (if (eq decision 'deny)
           (progn
             (harmless-session-append
              session
              (list :role :tool :id id :name name
                    :content "Error: user denied this tool call"))
             (funcall k))
         (harmless-session-set-status session 'streaming)
         (harmless-turn--invoke
          tool session args
          (lambda (result)
            (harmless-session-append
             session
             (list :role :tool :id id :name name :content result))
            (harmless-emit session (list :tool-result id name result))
            (funcall k)))))))))

(provide 'harmless-turn)

;;; harmless-turn.el ends here
