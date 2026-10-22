;;; harmless-editor.el --- Editor context for Harmless prompts -*- lexical-binding: t; -*-

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
;; A prompt sent from a file buffer names that buffer.  A prompt sent
;; from the transcript or the prompt window names the other window in
;; the selected frame when that window shows a file.  A non-file
;; buffer is named and contributes no file text.  The attachment is
;; rendered when the user sends, then appended after @ references
;; expand.  `harmless-turn-run' alone does not look at a buffer.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'which-func)
(require 'harmless-session)
(require 'harmless-tools-fs)
(require 'harmless-refs)

(defvar harmless-editor-note nil
  "Editor attachment for the prompt being sent, or nil.
A string is appended after @ references expand.  Nil leaves the
prompt unchanged, which is how a programmatic turn stays plain.")

(defun harmless-editor--internal-p (buf)
  "Return non-nil when BUF is the minibuffer or an internal buffer.
An internal buffer's name starts with a space."
  (or (minibufferp buf)
      (let ((name (buffer-name buf)))
        (and (stringp name)
             (> (length name) 0)
             (eq (aref name 0) ?\s)))))

(defun harmless-editor--file-p (buf)
  "Return non-nil when BUF, or its base buffer, visits a file."
  (and (buffer-live-p buf)
       (let* ((base (or (buffer-base-buffer buf) buf))
              (file (buffer-file-name base)))
         (and (stringp file) (not (string-empty-p file))))))

(defun harmless-editor--harmless-p (buf)
  "Return non-nil when BUF is SESSION's transcript or prompt."
  (with-current-buffer buf
    (or (derived-mode-p 'harmless-session-mode 'harmless-prompt-mode)
        (and (boundp 'harmless--session)
             (harmless-session-p harmless--session)
             (or (eq buf (harmless-session-buffer harmless--session))
                 (eq buf (harmless-session-prompt-buffer
                          harmless--session)))))))

(defun harmless-editor--excerpt (text)
  "Cut TEXT the way `harmless-tool-read-file' cuts a file."
  (harmless-tools-fs--text-window (or text "") nil nil))

(defun harmless-editor--function-name ()
  "Return the name `which-function' can find at point, or nil."
  (let ((name (ignore-errors (which-function))))
    (cond
     ((stringp name)
      (setq name (substring-no-properties name))
      (unless (string-blank-p name) name))
     ((null name) nil)
     (t (format "%s" name)))))

(defun harmless-editor--describe (buf)
  "Return the attachment for BUF.
A file buffer names the buffer, the file, the major mode, the
absolute line, the 1-based column, and whether it is modified or
narrowed.  An active region is included.  With no region, the line
at point and the function name are included.  Any other buffer is
named, with its major mode, and contributes no file text."
  (with-current-buffer buf
    (if (not (harmless-editor--file-p buf))
        (format "buffer: %s\nmode: %s" (buffer-name) major-mode)
      (let* ((base (or (buffer-base-buffer) (current-buffer)))
             (lines (list (format "buffer: %s" (buffer-name))
                          (format "file: %s" (buffer-file-name base))
                          (format "mode: %s" major-mode)
                          (format "line: %d" (line-number-at-pos nil t))
                          (format "column: %d" (1+ (current-column)))
                          (format "modified: %s"
                                  (if (buffer-modified-p) "yes" "no"))
                          (format "narrowed: %s"
                                  (if (buffer-narrowed-p) "yes" "no"))))
             (body (if (use-region-p)
                       (concat "region:\n"
                               (harmless-editor--excerpt
                                (buffer-substring-no-properties
                                 (region-beginning) (region-end))))
                     (let ((fn (harmless-editor--function-name)))
                       (concat "line-text: "
                               (harmless-editor--excerpt
                                (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position)))
                               "\n"
                               (if fn
                                   (format "function: %s" fn)
                                 "function:"))))))
        (concat (mapconcat #'identity lines "\n") "\n" body)))))

(defun harmless-editor-note ()
  "Return the editor attachment for the current prompt, or nil.
A file buffer describes itself.  The transcript and the prompt
describe the other window in the selected frame when that window
shows a file buffer, using that window's point.  A non-file buffer
is named.  The minibuffer and a buffer whose name starts with a
space contribute nothing.  This function does not write a file."
  (let ((buf (current-buffer)))
    (cond
     ((harmless-editor--internal-p buf) nil)
     ((harmless-editor--harmless-p buf)
      (let* ((win (selected-window))
             (other (next-window win nil (selected-frame))))
        (when (and other (window-live-p other) (not (eq other win)))
          (let ((obuf (window-buffer other)))
            (when (and (not (harmless-editor--internal-p obuf))
                       (harmless-editor--file-p obuf))
              (with-selected-window other
                (harmless-editor--describe (current-buffer))))))))
     ((harmless-editor--file-p buf)
      (harmless-editor--describe buf))
     (t (harmless-editor--describe buf)))))

(defun harmless-editor-prompt-source (session)
  "Return the buffer whose text is the prompt, or nil.
The current buffer is that text, except a file buffer and the
transcript.  Those keep their text and use SESSION's prompt buffer
when it is live."
  (let ((prompt (and (harmless-session-p session)
                     (harmless-session-prompt-buffer session))))
    (cond
     ((eq (current-buffer) prompt) (current-buffer))
     ((or (derived-mode-p 'harmless-session-mode)
          (harmless-editor--file-p (current-buffer)))
      (when (buffer-live-p prompt) prompt))
     (t (current-buffer)))))

(defun harmless-editor-join (text note)
  "Return TEXT with NOTE appended, or TEXT when NOTE is empty.
A non-string TEXT is returned unchanged.  NOTE follows under
`harmless-refs-header' when TEXT does not already have that header."
  (cond
   ((not (stringp text)) text)
   ((not (and (stringp note) (not (string-empty-p note)))) text)
   ((string-search harmless-refs-header text)
    (concat text "\n\n" note))
   (t (concat text "\n\n" harmless-refs-header "\n\n" note))))

(provide 'harmless-editor)

;;; harmless-editor.el ends here
