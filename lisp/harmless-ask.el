;;; harmless-ask.el --- One question with choices for Harmless -*- lexical-binding: t; -*-

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
;; `ask_user' asks one question.  The user picks one of the given
;; choices.  Replace `harmless-ask-decide-function' in tests.

;;; Code:

(require 'subr-x)
(require 'harmless-tools)

(defvar harmless-ask-decide-function #'harmless-ask-decide-default
  "Function (SESSION QUESTION CHOICES) that returns the chosen string.
QUESTION is a non-empty string.  CHOICES is a list of non-empty unique
strings.  Signal `quit' when the user cancels.")

(defun harmless-ask-decide-default (_session question choices)
  "Ask QUESTION, requiring an answer from CHOICES.
This refuses in batch Emacs, where `completing-read' has nobody to ask."
  (when noninteractive
    (error "Cannot ask a question in batch Emacs"))
  (completing-read (concat question " ") choices nil t))

(defun harmless-ask-question (args)
  "Return the trimmed question in ARGS, or signal."
  (let ((question (harmless-tool-arg args :question)))
    (unless (stringp question)
      (error "question is required"))
    (setq question (string-trim question))
    (when (string-empty-p question)
      (error "question is required"))
    question))

(defun harmless-ask-choices (args)
  "Return the trimmed unique choices in ARGS, or signal.
A missing list, an empty list, a blank choice, and a repeated choice
are refused.  A choice is text.  It is not a filesystem path."
  (let ((raw (harmless-tool-arg args :choices)))
    (cond
     ((vectorp raw) (setq raw (append raw nil)))
     ((not (listp raw)) (error "choices are required")))
    (unless raw
      (error "choices are required"))
    (let (choices)
      (dolist (item raw)
        (unless (stringp item)
          (error "each choice must be a non-empty string"))
        (let ((text (string-trim item)))
          (when (string-empty-p text)
            (error "each choice must be a non-empty string"))
          (when (string-match-p "[\n\r]" text)
            (error "each choice must be one line"))
          (when (member text choices)
            (error "choices must be unique"))
          (push text choices)))
      (nreverse choices))))

(defun harmless-ask--tool (session args)
  "ask_user implementation."
  (let ((question (harmless-ask-question args))
        (choices (harmless-ask-choices args)))
    (condition-case nil
        (let ((answer (funcall harmless-ask-decide-function
                               session question choices)))
          (unless (and (stringp answer) (member answer choices))
            (error "The question was not answered"))
          (format "The user chose: %s" answer))
      (quit "The user cancelled the question."))))

(defun harmless-ask-register ()
  "Register the ask_user tool."
  (harmless-register-tool
   (harmless-tool-create
    :name "ask_user"
    :description "Ask the user one question. QUESTION is the prompt. CHOICES are the answers they can pick. The result is the choice they made."
    :class 'read
    :schema '(:type "object"
              :properties (:question (:type "string"
                                     :description "The question to ask")
                           :choices (:type "array"
                                     :items (:type "string")
                                     :description "Answers the user can pick. One is returned."))
              :required ["question" "choices"])
    :fn #'harmless-ask--tool)))

(harmless-ask-register)

(provide 'harmless-ask)

;;; harmless-ask.el ends here
