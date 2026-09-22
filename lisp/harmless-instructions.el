;;; harmless-instructions.el --- Project instructions for Harmless -*- lexical-binding: t; -*-

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
;; Project instructions live in `AGENTS.md', `HARMLESS.md', and
;; `.harmless/HARMLESS.md'.  Each applies to that directory and everything
;; under it.  Files are read from the home directory down to the session
;; directory.  A later file wins when two of them disagree.

;;; Code:

(require 'subr-x)

(defun harmless-message-system-p (msg)
  "Return non-nil if MSG is a system message."
  (or (memq (plist-get msg :role) '(:system system))
      (equal (plist-get msg :role) "system")))

(defun harmless-messages-system-text (messages)
  "Return the combined text of system MESSAGES, or nil."
  (let ((parts (delq nil
                     (mapcar (lambda (msg)
                               (when (harmless-message-system-p msg)
                                 (let ((text (string-trim
                                              (or (plist-get msg :content) ""))))
                                   (unless (string-empty-p text)
                                     text))))
                             messages))))
    (and parts (mapconcat #'identity parts "\n\n"))))

(defun harmless-instruction-directories (dir &optional stop)
  "Return directories from the outermost ancestor of DIR through DIR.
The walk includes STOP, which defaults to the user's home directory,
and does not continue above it.  It also stops at the filesystem root."
  (let ((dir (directory-file-name (expand-file-name (or dir default-directory))))
        (stop (directory-file-name (expand-file-name (or stop "~/"))))
        (root (directory-file-name (expand-file-name "/")))
        (acc nil))
    (while dir
      (push dir acc)
      (if (or (string= dir stop) (string= dir root))
          (setq dir nil)
        (let ((parent (file-name-directory dir)))
          (setq dir (and parent
                         (directory-file-name parent)
                         (unless (string= (directory-file-name parent) dir)
                           (directory-file-name parent)))))))
    acc))

(defun harmless-instruction-candidates (dir)
  "Return instruction paths for DIR, in the order they should be read.
`AGENTS.md' comes first, then `HARMLESS.md', then `.harmless/HARMLESS.md'."
  (list (expand-file-name "AGENTS.md" dir)
        (expand-file-name "HARMLESS.md" dir)
        (expand-file-name "HARMLESS.md"
                          (expand-file-name ".harmless" dir))))

(defun harmless-instruction-files (dir &optional stop)
  "Return instruction files that apply to DIR, outermost first.
At each directory, `AGENTS.md' comes before `HARMLESS.md', which comes
before `.harmless/HARMLESS.md'.  STOP is passed to
`harmless-instruction-directories'."
  (let (files)
    (dolist (ancestor (harmless-instruction-directories dir stop))
      (dolist (path (harmless-instruction-candidates ancestor))
        (when (file-readable-p path)
          (push path files))))
    (nreverse files)))

(defun harmless-instruction--read (path)
  "Return the trimmed contents of PATH."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (string-trim (buffer-string))))

(defun harmless-instructions-text (dir &optional stop)
  "Return the project-instruction prompt for DIR, or nil if none exist.
STOP is passed to `harmless-instruction-files'."
  (let ((chunks
         (delq nil
               (mapcar (lambda (path)
                         (let ((body (harmless-instruction--read path)))
                           (unless (string-empty-p body)
                             (format "## %s\n%s" path body))))
                       (harmless-instruction-files dir stop)))))
    (when chunks
      (concat
       "Project instructions for this session.  Files are listed from the outermost directory to the innermost.  When they disagree, prefer the later file.\n\n"
       (mapconcat #'identity chunks "\n\n")))))

(defun harmless-instructions-apply (dir messages &optional stop)
  "Return MESSAGES with project instructions for DIR prepended.
MESSAGES is unchanged when DIR has no instruction files.  STOP is
passed to `harmless-instructions-text'."
  (if-let* ((text (harmless-instructions-text dir stop)))
      (cons (list :role :system :content text) messages)
    messages))

(provide 'harmless-instructions)

;;; harmless-instructions.el ends here
