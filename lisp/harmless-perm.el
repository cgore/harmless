;;; harmless-perm.el --- Permission gate for Harmless -*- lexical-binding: t; -*-

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
;; Decide whether a tool class needs a prompt, and ask through
;; `harmless-perm-ask-function'.

;;; Code:

(require 'cl-lib)
(require 'harmless-util)
(require 'harmless-session)

(defvar harmless-perm-ask-function #'harmless-perm-ask-default
  "Function (SESSION CLASS ITEM CALLBACK) used to confirm a tool call.
CALLBACK is called with `allow', `always', or `deny'.")

(defun harmless-perm-needed-p (session class)
  "Return non-nil if SESSION must confirm a tool of CLASS."
  (let ((mode (harmless-session-permission-mode session))
        (allowed (harmless-session-allow-classes session)))
    (cond
     ((memq class allowed) nil)
     ((eq mode 'always-approve) nil)
     ((and (eq mode 'accept-edits) (memq class '(read edit))) nil)
     ((eq class 'read) nil)
     (t t))))

(defun harmless-perm-ask-default (session class item callback)
  "Default confirmer: deny in batch, `y-or-n-p' interactively.
SESSION is unused.  CLASS and ITEM describe the call.  CALLBACK
receives `allow' or `deny'."
  (ignore session)
  (if noninteractive
      (funcall callback 'deny)
    (funcall callback
             (if (y-or-n-p (format "Harmless: allow %s %s? " class item))
                 'allow
               'deny))))

(defun harmless-perm-confirm (session class item callback)
  "If SESSION allows CLASS, call CALLBACK with `allow', else ask.
ITEM is a short description or plist shown to the user."
  (if (not (harmless-perm-needed-p session class))
      (funcall callback 'allow)
    (harmless-session-set-status session 'waiting-permission)
    (harmless-emit session (list :permission class item))
    (funcall harmless-perm-ask-function
             session class item
             (lambda (decision)
               (when (eq decision 'always)
                 (setf (harmless-session-allow-classes session)
                       (cl-adjoin class
                                  (harmless-session-allow-classes session))))
               (funcall callback decision)))))

(provide 'harmless-perm)

;;; harmless-perm.el ends here
