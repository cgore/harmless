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
;; buffer is named and contributes no file text.  Dired attaches the
;; marked file names, and Org attaches the subtree at point.  The
;; attachment is
;; rendered when the user sends, then appended after @ references
;; expand.  `harmless-turn-run' alone does not look at a buffer.
;; `harmless-visit-location' opens FILE:LINE from a transcript button
;; in the other window.  It does not set `next-error-function'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function org-before-first-heading-p "org" ())
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-end-of-subtree "org" (&optional invisible-ok to-heading))
(declare-function dired-get-marked-files "dired"
                  (&optional localp arg filter distinguish-one-marked error))
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

(defun harmless-editor--session ()
  "Return the session for this buffer, or nil."
  (or (and (boundp 'harmless--session)
           (harmless-session-p harmless--session)
           harmless--session)
      (ignore-errors
        (car (harmless-session-for-cwd (harmless-current-cwd))))))

(defun harmless-editor--check-mark (session path)
  "Return PATH when SESSION may attach it.
Nil and \"\" signal \"Path is empty\".  Whitespace is empty.
A non-string signals \"path must be a string\".  The filesystem
root signals \"path is the filesystem root\".  A path outside
the project signals \"Path escapes project: PATH\".  A refusal
attaches nothing."
  (cond
   ((null path) (error "Path is empty"))
   ((not (stringp path)) (error "path must be a string"))
   (t (setq path (string-trim path))))
  (cond
   ((string-empty-p path) (error "Path is empty"))
   ((or (harmless-filesystem-root-p path)
        (harmless-extra-root-text-p path))
    (error "path is the filesystem root"))
   (t
    (let ((full (harmless-tools-fs-resolve session path)))
      (when (or (harmless-filesystem-root-p full)
                (harmless-extra-root-text-p full))
        (error "path is the filesystem root"))
      path))))

(defun harmless-editor--dired-note ()
  "Return the marked-file attachment for this Dired buffer.
No mark uses the file at point.  Every mark is checked.  The
list is capped at `harmless-refs-folder-max'.  A refusal
attaches nothing."
  (let* ((session (or (harmless-editor--session)
                      (error "No Harmless session")))
         (files (dired-get-marked-files))
         (checked nil))
    (unless files
      (error "Path is empty"))
    (dolist (path files)
      (push (harmless-editor--check-mark session path) checked))
    (setq checked (nreverse checked))
    (when (> (length checked) harmless-refs-folder-max)
      (setq checked (cl-subseq checked 0 harmless-refs-folder-max)))
    (concat (format "buffer: %s\nmode: %s\nfiles:\n" (buffer-name) major-mode)
            (mapconcat #'identity checked "\n"))))

(defun harmless-editor--org-note ()
  "Return the current Org subtree, cut like a file read.
An empty subtree signals \"region is empty\" and attaches
nothing.  Point stays where it was."
  (save-excursion
    (when (org-before-first-heading-p)
      (error "region is empty"))
    (org-back-to-heading t)
    (let ((beg (point)))
      (org-end-of-subtree t t)
      (when (<= (point) beg)
        (error "region is empty"))
      (concat (format "buffer: %s\nmode: %s\nsubtree:\n"
                      (buffer-name) major-mode)
              (harmless-editor--excerpt
               (buffer-substring-no-properties beg (point)))))))

(defun harmless-editor-note ()
  "Return the editor attachment for the current prompt, or nil.
A file buffer describes itself.  The transcript and the prompt
describe the other window in the selected frame when that window
shows a file buffer, using that window's point.  Dired attaches
marked file names.  Org attaches the subtree at point.  A
non-file buffer is named.  The minibuffer and a buffer whose
name starts with a space contribute nothing.  This function
does not write a file."
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
     ((derived-mode-p 'dired-mode)
      (harmless-editor--dired-note))
     ((derived-mode-p 'org-mode)
      (harmless-editor--org-note))
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

(defconst harmless-visit-location-regexp
  (concat "\\(?:[A-Za-z]:\\)?"
          "[^[:space:]\n:]+"
          "[./\\\\]"
          "[^[:space:]\n:]*"
          ":[0-9]+"
          "\\(?::[0-9]+\\)?")
  "Regexp matching one FILE:LINE or FILE:LINE:COLUMN token.
A drive colon is part of the path.  The match stops before any
character that is not part of that token.")

(defun harmless-visit-parse (text)
  "Return (PATH LINE COLUMN) parsed from TEXT, or nil.
TEXT is one FILE:LINE or FILE:LINE:COLUMN token.  COLUMN is nil
when TEXT names no column.  LINE and COLUMN are integers."
  (when (and (stringp text)
             (string-match
              (concat "\\`\\(\\(?:[A-Za-z]:\\)?[^[:space:]\n:]+"
                      "[./\\\\][^[:space:]\n:]*\\)"
                      ":\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?\\'")
              text))
    (list (match-string 1 text)
          (string-to-number (match-string 2 text))
          (and (match-string 3 text)
               (string-to-number (match-string 3 text))))))

(defun harmless-visit--show (buf line column)
  "Show BUF at LINE and COLUMN.
COLUMN nil is column 1.  The window is not the selected window
at the start of this call.  A narrowing that hides the position
is widened."
  (let ((win (display-buffer
              buf
              '((display-buffer-reuse-window
                 display-buffer-use-some-window
                 display-buffer-pop-up-window)
                (inhibit-same-window . t)))))
    (when (window-live-p win)
      (select-window win)))
  (with-current-buffer buf
    (let ((pos (save-excursion
                 (save-restriction
                   (widen)
                   (goto-char (point-min))
                   (forward-line (1- line))
                   (move-to-column (if column (1- column) 0))
                   (point)))))
      (unless (<= (point-min) pos (point-max))
        (widen))
      (goto-char pos))))

(defun harmless-visit-location (session path line &optional column)
  "Visit PATH at LINE and COLUMN in the other window.
COLUMN starts at 1.  Nil COLUMN opens the line at column 1.
PATH must stay inside SESSION's project or an extra directory.
A path outside both signals \"Path escapes project: PATH\".
Nil and \"\" signal \"Path is empty\".  Whitespace is empty.
A non-string signals \"path must be a string\".  The filesystem
root signals \"path is the filesystem root\" and visits nothing.
A missing file signals \"Cannot read PATH\".  A directory and a
trailing slash signal \"path is not a file\".  Line 0, a negative
line, and a line that is not a positive integer signal
\"line is not a line\".  A column that is not a positive integer
signals \"column is not a column\".  This does not set
`next-error-function'."
  (cond
   ((null path) (error "Path is empty"))
   ((not (stringp path)) (error "path must be a string"))
   (t (setq path (string-trim path))))
  (cond
   ((string-empty-p path) (error "Path is empty"))
   ((or (harmless-filesystem-root-p path)
        (harmless-extra-root-text-p path))
    (error "path is the filesystem root"))
   ((not (and (integerp line) (> line 0)))
    (error "line is not a line"))
   ((not (or (null column) (and (integerp column) (> column 0))))
    (error "column is not a column"))
   ((or (string-suffix-p "/" path) (string-suffix-p "\\" path))
    (error "path is not a file"))
   ((not (harmless-session-p session))
    (error "No Harmless session")))
  (let ((full (harmless-tools-fs-resolve session path)))
    (cond
     ((or (harmless-filesystem-root-p full)
          (harmless-extra-root-text-p full))
      (error "path is the filesystem root"))
     ((or (string-suffix-p "/" full)
          (string-suffix-p "\\" full)
          (file-directory-p full))
      (error "path is not a file"))
     ((not (and (file-regular-p full) (file-readable-p full)))
      (error "Cannot read %s" path)))
    (harmless-visit--show (find-file-noselect full) line column)
    full))

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
