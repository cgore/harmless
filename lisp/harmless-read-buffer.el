;;; harmless-read-buffer.el --- Read one visible buffer -*- lexical-binding: t; -*-

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
;; `harmless-tool-read-buffer' returns one buffer's accessible text.
;; The model names the buffer.  The minibuffer, a buffer whose name
;; starts with a space, and a Harmless process buffer are refused.
;; Another session's transcript is refused.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-session)
(require 'harmless-tools)
(require 'harmless-tools-fs)

(defvar harmless--session)

(defun harmless-read-buffer--harmless-process-p (proc)
  "Return non-nil when PROC is a Harmless process."
  (and (processp proc)
       (let ((name (process-name proc)))
         (and (stringp name) (string-prefix-p "harmless" name)))))

(defun harmless-read-buffer--process-p (buf)
  "Return non-nil when BUF belongs to a Harmless process."
  (or (harmless-read-buffer--harmless-process-p (get-buffer-process buf))
      (cl-some (lambda (proc)
                 (and (eq (process-buffer proc) buf)
                      (harmless-read-buffer--harmless-process-p proc)))
               (process-list))))

(defun harmless-read-buffer--other-session-p (buf session)
  "Return non-nil when BUF is another session's transcript or prompt."
  (with-current-buffer buf
    (and (derived-mode-p 'harmless-session-mode 'harmless-prompt-mode)
         (not (and (harmless-session-p session)
                   (eq harmless--session session))))))

(defun harmless-read-buffer (session name)
  "Return the accessible text of the buffer named NAME.
SESSION is the session making the request.  The text is cut at
`harmless-read-file-max-bytes'.  Nil, \"\", and whitespace signal
\"buffer name is empty\".  A non-string signals \"buffer name
must be a string\".  A missing buffer signals \"buffer is not
live\".  The minibuffer, a buffer whose name starts with a
space, and a Harmless process buffer signal \"buffer is not
readable\".  Another session's transcript or prompt signals
\"buffer is another session\"."
  (cond
   ((null name) (error "buffer name is empty"))
   ((not (stringp name)) (error "buffer name must be a string"))
   ((string-blank-p name) (error "buffer name is empty")))
  (let ((buf (get-buffer name)))
    (unless (buffer-live-p buf)
      (error "buffer is not live"))
    (when (or (minibufferp buf)
              (harmless-read-buffer--process-p buf)
              (let ((bname (buffer-name buf)))
                (and (stringp bname)
                     (not (string-empty-p bname))
                     (eq (aref bname 0) ?\s))))
      (error "buffer is not readable"))
    (when (harmless-read-buffer--other-session-p buf session)
      (error "buffer is another session"))
    (with-current-buffer buf
      (harmless-tools-fs--text-window
       (buffer-substring-no-properties (point-min) (point-max))
       nil nil))))

(defun harmless-read-buffer--tool (session args)
  "harmless-tool-read-buffer implementation."
  (harmless-read-buffer session (harmless-tool-arg args :name)))

(defun harmless-read-buffer-register ()
  "Register `harmless-tool-read-buffer'."
  (harmless-register-tool
   (harmless-tool-create
    :name "harmless-tool-read-buffer"
    :description "Read one buffer the user can see. NAME is the buffer name. The result is that buffer's accessible text, cut at the file read cap."
    :class 'read
    :schema '(:type "object"
              :properties (:name (:type "string"
                                 :description "Buffer name"))
              :required ["name"])
    :fn #'harmless-read-buffer--tool)))

(harmless-read-buffer-register)

(provide 'harmless-read-buffer)

;;; harmless-read-buffer.el ends here
