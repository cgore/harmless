;;; harmless-tools-fs.el --- Filesystem tools for Harmless -*- lexical-binding: t; -*-

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
;; read_file, write_file, replace, apply_patch, list_dir, grep, glob.
;; Paths must stay inside the session cwd or a directory added with
;; `harmless-add-directory'.  A buffer visiting the file is read and
;; edited in place, then saved.  apply_patch writes nothing when any
;; hunk fails.  grep uses ripgrep, then ag, then an Elisp scan.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-tools)

(defvar harmless--session)
(declare-function harmless--context-session "harmless")

(defcustom harmless-read-file-max-bytes 100000
  "Maximum number of bytes `read_file' returns to the model."
  :type 'integer
  :group 'harmless)

(defun harmless-tools-fs--textual-p (path)
  "Return non-nil if PATH must be compared as text, not `file-truename'.
On Windows a leading slash is text: `file-truename' pastes the current
drive, and a missing file keeps the slash while \"/\" becomes \"d:/\".
Elsewhere a drive path is text, because `file-truename' would join it
onto this system."
  (if (memq system-type '(windows-nt ms-dos))
      (harmless-leading-slash-p path)
    (harmless-foreign-absolute-p path)))

(defun harmless-tools-fs-inside-p (root path)
  "Return non-nil if PATH is inside ROOT after resolving `..'.
A Windows drive path is never inside a leading-slash path.  Two
drive paths on Windows are compared with `file-truename'.  Two
leading slashes are compared as text."
  (let ((text (or (harmless-tools-fs--textual-p root)
                  (harmless-tools-fs--textual-p path))))
    (when (or (not text)
              (and (harmless-tools-fs--textual-p root)
                   (harmless-tools-fs--textual-p path)))
      (let* ((root (file-name-as-directory
                    (if text
                        (harmless-collapse-path (harmless-expand-kept root))
                      (file-truename root))))
             (path (if text
                       (harmless-collapse-path (harmless-expand-kept path))
                     (file-truename path))))
        (or (string-prefix-p root path)
            (string-prefix-p root (file-name-as-directory path))
            (string= (directory-file-name root) path))))))

(defun harmless-extra-root-text-p (text)
  "Return non-nil if TEXT is \"/\" or a drive root.
TEXT is not expanded.  A relative path and \"~\" are not roots.
A string of only backslashes is the root."
  (and (stringp text)
       (or (equal text "/")
           (string-match-p "\\`[A-Za-z]:[/\\]?\\'" text)
           (string-match-p "\\`\\\\+\\'" text)
           (and (harmless-path-absolute-p text)
                (not (string-prefix-p "~" text))
                (harmless-filesystem-root-p text)))))

(defun harmless-extra-normalize (session directory)
  "Return DIRECTORY as a directory SESSION may read and edit.
Nil, \"\", and whitespace signal \"extra directory is empty\".
A non-string signals \"extra directory must be a string\".
A relative path, including \"~\", signals \"extra directory must
be absolute\" and is not expanded.  \"/\", \"c:/\", and a drive
root signal \"extra directory is the filesystem root\".  A missing
path signals \"extra directory is missing\".  A file signals
\"extra directory is not a directory\".  The project, and a
directory inside it, signal \"extra directory is inside the
project\".  DIRECTORY is not created."
  (unless (harmless-session-p session)
    (error "No Harmless session"))
  (cond
   ((null directory) (error "extra directory is empty"))
   ((not (stringp directory))
    (error "extra directory must be a string")))
  (setq directory (string-trim directory))
  (cond
   ((string-empty-p directory) (error "extra directory is empty"))
   ((harmless-extra-root-text-p directory)
    (error "extra directory is the filesystem root"))
   ((or (not (file-name-absolute-p directory))
        (string-prefix-p "~" directory))
    (error "extra directory must be absolute")))
  (let* ((cwd (harmless-session-require-project session))
         (full (file-name-as-directory
                (file-truename (directory-file-name directory)))))
    (cond
     ((or (harmless-filesystem-root-p full)
          (harmless-extra-root-text-p full))
      (error "extra directory is the filesystem root"))
     ((not (file-exists-p (directory-file-name full)))
      (error "extra directory is missing"))
     ((not (file-directory-p full))
      (error "extra directory is not a directory"))
     ((harmless-tools-fs-inside-p cwd full)
      (error "extra directory is inside the project"))
     (t full))))

(defun harmless-extra--usable (session)
  "Return SESSION extra directories that are safe to search.
A filesystem root and a drive root are left out."
  (let (out)
    (dolist (root (harmless-session-extra-dirs session))
      (when (and (stringp root)
                 (not (string-empty-p root))
                 (file-name-absolute-p root)
                 (not (string-prefix-p "~" root))
                 (not (harmless-extra-root-text-p root))
                 (not (harmless-filesystem-root-p root))
                 (file-directory-p root))
        (push root out)))
    (nreverse out)))

(defun harmless-extra-add (session directory)
  "Add DIRECTORY to SESSION, save, and return the stored list.
The same directory is stored once.  A trailing slash does not
make a second entry."
  (let ((norm (harmless-extra-normalize session directory))
        (dirs (copy-sequence (harmless-session-extra-dirs session))))
    (unless (member norm dirs)
      (setq dirs (append dirs (list norm)))
      (setf (harmless-session-extra-dirs session) dirs)
      (harmless-session-save session))
    (harmless-session-extra-dirs session)))

(defun harmless-extra-restore (session raw)
  "Install valid directories from RAW onto SESSION.
\"/\", a drive root, a relative path, nil, and \"\" are dropped
and are not expanded.  A missing directory is dropped.  Nothing
is created."
  (let (kept)
    (when (listp raw)
      (dolist (dir raw)
        (let ((norm (condition-case nil
                        (harmless-extra-normalize session dir)
                      (error nil))))
          (when (and norm (not (member norm kept)))
            (push norm kept)))))
    (setf (harmless-session-extra-dirs session) (nreverse kept))))

(defun harmless-extra-summary (session)
  "Return a :extra-dirs entry, or nil when SESSION has none."
  (let ((dirs (harmless-session-extra-dirs session)))
    (when dirs
      (list :extra-dirs (copy-sequence dirs)))))

(defun harmless-extra--session ()
  "Return the session `harmless-add-directory' should change."
  (cond
   ((and (boundp 'harmless--session)
         (harmless-session-p harmless--session))
    harmless--session)
   ((and (fboundp 'harmless--context-session)
         (harmless-session-p (harmless--context-session)))
    (harmless--context-session))
   (t (error "No Harmless session"))))

;;;###autoload
(defun harmless-add-directory (directory)
  "Let the current session read and edit DIRECTORY.
DIRECTORY must be absolute and outside the project.  See
`harmless-extra-normalize' for the refusals.  The list is saved
with the session."
  (interactive
   (progn
     (harmless-extra--session)
     (list (read-directory-name "Extra directory: " nil nil t))))
  (let ((dirs (harmless-extra-add (harmless-extra--session) directory)))
    (message "Added %s" (car (last dirs)))))

(defun harmless-tools-fs-allowed-p (session path)
  "Return non-nil if PATH is in SESSION's project or an extra directory."
  (let ((cwd (harmless-session-require-project session)))
    (or (harmless-tools-fs-inside-p cwd path)
        (cl-some (lambda (root)
                   (harmless-tools-fs-inside-p root path))
                 (harmless-extra--usable session)))))

(defun harmless-tools-fs--display-path (file root)
  "Return FILE relative to ROOT, or FILE when it is outside ROOT."
  (if (harmless-tools-fs-inside-p root file)
      (file-relative-name file (directory-file-name root))
    file))

(defun harmless-tools-fs-resolve (session path)
  "Resolve PATH against SESSION, or signal if it escapes.
PATH may stay inside the project or an extra directory."
  (unless (and path (not (string-empty-p path)))
    (error "Path is empty"))
  (let* ((cwd (harmless-session-require-project session))
         (full (harmless-expand-file-name path cwd)))
    (unless (harmless-tools-fs-allowed-p session full)
      (error "Path escapes project: %s" path))
    full))

(defun harmless-tools-fs-revert-visiting (path)
  "Revert any buffer visiting PATH."
  (when-let* ((buf (find-buffer-visiting path)))
    (with-current-buffer buf
      (revert-buffer t t t))))

(defvar-local harmless-tools-fs--read-tick nil
  "Character tick of this buffer at the last `read_file'.
Nil means the tools have not read the buffer.  A later edit refuses
when the tick has moved.")

(defun harmless-tools-fs--unusable-buffer-p (buf)
  "Return non-nil if BUF must not be read or edited as a project file."
  (or (minibufferp buf)
      (with-current-buffer buf
        (or (derived-mode-p 'harmless-session-mode 'harmless-prompt-mode)
            (and (stringp (buffer-name))
                 (not (string-empty-p (buffer-name)))
                 (eq (aref (buffer-name) 0) ?\s))))))

(defun harmless-tools-fs--file-buffer (path)
  "Return the base buffer visiting PATH, or nil.
An indirect buffer is returned as its base.  The transcript, the
prompt, the minibuffer, and a buffer whose name starts with a space
signal \"buffer is not a file\"."
  (let ((buf (and (stringp path) (find-buffer-visiting path))))
    (when (buffer-live-p buf)
      (setq buf (or (buffer-base-buffer buf) buf))
      (when (harmless-tools-fs--unusable-buffer-p buf)
        (error "buffer is not a file"))
      buf)))

(defun harmless-tools-fs--note (narrowed)
  "Return the report suffix when NARROWED is non-nil."
  (if narrowed " buffer was narrowed" ""))

(defun harmless-tools-fs--annotate (text modified narrowed)
  "Return TEXT plus unsaved and narrowed markers."
  (when modified
    (setq text (concat text "\n[unsaved]")))
  (when narrowed
    (setq text (concat text "\n[narrowed]")))
  text)

(defun harmless-tools-fs--text-window (text offset limit)
  "Return TEXT from line OFFSET for LIMIT lines, then apply the cap."
  (with-temp-buffer
    (insert (or text ""))
    (when (and offset (numberp offset) (> offset 1))
      (goto-char (point-min))
      (forward-line (1- offset))
      (delete-region (point-min) (point)))
    (when (and limit (numberp limit) (> limit 0))
      (goto-char (point-min))
      (forward-line limit)
      (delete-region (point) (point-max)))
    (when (> (buffer-size) harmless-read-file-max-bytes)
      (goto-char (1+ harmless-read-file-max-bytes))
      (delete-region (point) (point-max))
      (goto-char (point-max))
      (insert "\n[truncated]"))
    (buffer-string)))

(defun harmless-tools-fs--refuse-read-only (start end)
  "Signal when the current buffer, or START to END, is read-only."
  (when (or buffer-read-only
            (and (integerp start) (integerp end) (< start end)
                 (text-property-not-all start end 'read-only nil)))
    (error "buffer is read-only")))

(defun harmless-tools-fs--file-bytes (path)
  "Return the raw bytes of PATH, or nil when it is not a file."
  (when (and (stringp path) (file-regular-p path))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally path)
      (buffer-string))))

(defun harmless-tools-fs--restore-file (path bytes existed)
  "Put PATH back to BYTES.  Delete it when EXISTED is nil."
  (if (not existed)
      (when (and (stringp path) (file-exists-p path))
        (delete-file path))
    (when (and (stringp path) bytes
               (not (equal bytes (harmless-tools-fs--file-bytes path))))
      (let ((coding-system-for-write 'no-conversion))
        (write-region bytes nil path nil 'silent)))))

(defun harmless-tools-fs--restore-narrow (narrowed start end)
  "Restore a narrowing from START to END when NARROWED is non-nil."
  (if (not narrowed)
      (widen)
    (let ((beg (max (point-min) (min start (point-max))))
          (tail (max (point-min) (min end (point-max)))))
      (if (< beg tail)
          (narrow-to-region beg tail)
        (widen)))))

(defun harmless-tools-fs--show-change (buf pos)
  "Move windows that show BUF to POS.  The selected window stays selected."
  (when (and (buffer-live-p buf) (integerp pos))
    (dolist (win (get-buffer-window-list buf nil t))
      (when (window-live-p win)
        (set-window-point win (with-current-buffer buf
                                (max (point-min) (min pos (point-max)))))))))

(declare-function vc-backend "vc-hooks" (file))
(declare-function vc-refresh-state "vc-hooks" ())

(defun harmless-tools-fs--refresh-vc (buf)
  "Refresh VC state for BUF after a save."
  (when (and (require 'vc nil t) (buffer-live-p buf))
    (with-current-buffer buf
      (when (and buffer-file-name (vc-backend buffer-file-name))
        (condition-case nil
            (vc-refresh-state)
          (error nil))))))

(defun harmless-tools-fs--ensure-current-editable ()
  "Signal when this buffer changed after a read or is read-only."
  (when (and (numberp harmless-tools-fs--read-tick)
             (/= harmless-tools-fs--read-tick
                 (buffer-chars-modified-tick)))
    (error "buffer changed since it was read"))
  (when buffer-read-only
    (error "buffer is read-only")))

(defun harmless-tools-fs--ensure-buffer-writable (buf &optional spans)
  "Signal when BUF must not be edited.
SPANS is a list of (BEG . END) in the widened buffer.  With no
spans, the whole widened buffer is checked for read-only text."
  (with-current-buffer buf
    (harmless-tools-fs--ensure-current-editable)
    (save-restriction
      (widen)
      (if spans
          (dolist (span spans)
            (harmless-tools-fs--refuse-read-only (car span) (cdr span)))
        (harmless-tools-fs--refuse-read-only (point-min) (point-max))))))

(defun harmless-tools-fs--edit-buffer (buf mutator)
  "Apply MUTATOR in BUF as one undo entry and save BUF.
MUTATOR runs widened and returns the position of the first change.
Return (POS . NARROWED).  A save hook that signals restores the
buffer and the file and signals \"buffer save failed:\"."
  (with-current-buffer buf
    (harmless-tools-fs--ensure-current-editable)
    ;; A command loop would have done this.  Keep the tool's edit one
    ;; undo entry behind the buffer's earlier edits.
    (undo-boundary)
    (let* ((narrowed (buffer-narrowed-p))
           (start (point-min))
           (end (point-max))
           (modified (buffer-modified-p))
           (tick harmless-tools-fs--read-tick)
           (path buffer-file-name)
           (existed (and path (file-exists-p path)))
           (bytes (and existed (harmless-tools-fs--file-bytes path)))
           (handle (prepare-change-group buf))
           (settled nil)
           (pos nil))
      (activate-change-group handle)
      (unwind-protect
          (progn
            (widen)
            (setq pos (funcall mutator))
            (condition-case err
                ;; `save-buffer' demotes a `before-save-hook' error.
                ;; Run both save hooks here so a signal refuses the edit.
                (let ((before before-save-hook)
                      (after after-save-hook))
                  (let ((before-save-hook nil)
                        (after-save-hook nil))
                    (setq before-save-hook before)
                    (run-hooks 'before-save-hook)
                    (setq before-save-hook nil)
                    (if buffer-file-coding-system
                        (let ((coding-system-for-write
                               buffer-file-coding-system))
                          (save-buffer))
                      (save-buffer))
                    (setq after-save-hook after)
                    (run-hooks 'after-save-hook)))
              (error
               (ignore-errors (cancel-change-group handle))
               (setq settled t)
               (set-buffer-modified-p modified)
               (when (numberp tick)
                 (setq harmless-tools-fs--read-tick
                       (buffer-chars-modified-tick)))
               (harmless-tools-fs--restore-narrow narrowed start end)
               (harmless-tools-fs--restore-file path bytes existed)
               (error "buffer save failed: %s" (error-message-string err))))
            (accept-change-group handle)
            (setq settled t)
            (undo-amalgamate-change-group handle)
            (setq harmless-tools-fs--read-tick (buffer-chars-modified-tick))
            (harmless-tools-fs--restore-narrow narrowed start end)
            (harmless-tools-fs--refresh-vc buf)
            (harmless-tools-fs--show-change buf pos)
            (cons pos narrowed))
        (unless settled
          (ignore-errors (cancel-change-group handle))
          (set-buffer-modified-p modified)
          (when (numberp tick)
            (setq harmless-tools-fs--read-tick (buffer-chars-modified-tick)))
          (harmless-tools-fs--restore-narrow narrowed start end))))))

(defun harmless-tools-fs-diff (old new path)
  "Return a unified diff string from OLD to NEW labeled as PATH."
  (let ((a (make-temp-file "harmless-old"))
        (b (make-temp-file "harmless-new")))
    (unwind-protect
        (progn
          (write-region old nil a nil 'silent)
          (write-region new nil b nil 'silent)
          (with-temp-buffer
            (let ((status (call-process "diff" nil t nil "-u" a b)))
              (if (and (integerp status) (<= status 1))
                  (let ((s (buffer-string)))
                    (if (string-empty-p s)
                        "(no changes)"
                      (replace-regexp-in-string
                       (regexp-quote a) path
                       (replace-regexp-in-string (regexp-quote b) path s))))
                (format "old (%d bytes) -> new (%d bytes) in %s"
                        (length old) (length new) path)))))
      (ignore-errors (delete-file a))
      (ignore-errors (delete-file b)))))

(defun harmless-tools-fs--read-resolved (session path args)
  "Return the text of PATH for SESSION.
ARGS supplies the optional line offset and limit.  A visiting
buffer supplies its accessible text, labeled when it is unsaved
or narrowed."
  (ignore session)
  (let ((buf (harmless-tools-fs--file-buffer path))
        (offset (harmless-tool-arg args :offset))
        (limit (harmless-tool-arg args :limit)))
    (if (not buf)
        (progn
          (unless (file-readable-p path)
            (error "Cannot read %s" path))
          (with-temp-buffer
            (insert-file-contents path)
            (harmless-tools-fs--text-window (buffer-string) offset limit)))
      (with-current-buffer buf
        (let ((text (buffer-substring-no-properties (point-min) (point-max)))
              (modified (buffer-modified-p))
              (narrowed (buffer-narrowed-p)))
          (setq harmless-tools-fs--read-tick (buffer-chars-modified-tick))
          (harmless-tools-fs--annotate
           (harmless-tools-fs--text-window text offset limit)
           modified narrowed))))))

(defun harmless-tools-fs--read (session args)
  "read_file implementation.
An image, a PDF, or a notebook is read by `harmless-media-read'
when that feature is loaded.  Other files stay text."
  (let ((path (harmless-tools-fs-resolve
               session (harmless-tool-arg args :path))))
    (if (and (fboundp 'harmless-media-read-p)
             (fboundp 'harmless-media-read)
             (harmless-media-read-p path))
        (harmless-media-read session path args)
      (harmless-tools-fs--read-resolved session path args))))

(defun harmless-tools-fs--reject-plan-edit (session)
  "Signal when SESSION is in plan mode and this edit is not allowed."
  (when (harmless-session-plan-mode session)
    (error "Plan mode is on. Only plan.md may be edited, with write_plan")))

(defun harmless-tools-fs--write (session args)
  "write_file implementation."
  (harmless-tools-fs--reject-plan-edit session)
  (let* ((path (harmless-tools-fs-resolve session (harmless-tool-arg args :path)))
         (contents (or (harmless-tool-arg args :contents)
                       (harmless-tool-arg args :content)
                       ""))
         (buf (harmless-tools-fs--file-buffer path)))
    (unless (stringp contents)
      (setq contents (format "%s" contents)))
    (when buf
      (harmless-tools-fs--ensure-buffer-writable buf))
    (if (not buf)
        (let ((parent (file-name-directory path)))
          ;; / already exists.  Refusing to create it must not block a file
          ;; whose project really is the filesystem root.
          (unless (and parent (file-directory-p parent))
            (harmless-ensure-directory parent))
          (when (fboundp 'harmless-checkpoint-before)
            (harmless-checkpoint-before session path))
          (write-region contents nil path nil 'silent)
          (format "Wrote %s (%d bytes)" path (string-bytes contents)))
      (when (fboundp 'harmless-checkpoint-before)
        (harmless-checkpoint-before session path))
      (let ((narrowed (cdr (harmless-tools-fs--edit-buffer
                            buf
                            (lambda ()
                              (harmless-tools-fs--refuse-read-only
                               (point-min) (point-max))
                              (erase-buffer)
                              (insert contents)
                              (point-min))))))
        (format "Wrote %s (%d bytes)%s"
                path
                (with-current-buffer buf
                  (save-restriction
                    (widen)
                    (string-bytes (buffer-string))))
                (harmless-tools-fs--note narrowed))))))

(defun harmless-tools-fs--buffer-text (buf)
  "Return BUF's full text, widened."
  (with-current-buffer buf
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun harmless-tools-fs--source-text (path)
  "Return the text an edit should change for PATH.
A visiting buffer supplies its widened text and refuses a buffer
that changed after it was read.  Otherwise the file is read."
  (let ((buf (harmless-tools-fs--file-buffer path)))
    (if (not buf)
        (progn
          (unless (and (file-regular-p path) (file-readable-p path))
            (error "Cannot read %s" path))
          (cons nil
                (with-temp-buffer
                  (insert-file-contents path)
                  (buffer-string))))
      (with-current-buffer buf
        (when (and (numberp harmless-tools-fs--read-tick)
                   (/= harmless-tools-fs--read-tick
                       (buffer-chars-modified-tick)))
          (error "buffer changed since it was read"))
        (cons buf (harmless-tools-fs--buffer-text buf))))))

(defun harmless-tools-fs--replace (session args)
  "replace implementation (unique old_string -> new_string)."
  (harmless-tools-fs--reject-plan-edit session)
  (let* ((path (harmless-tools-fs-resolve session (harmless-tool-arg args :path)))
         (old (harmless-tool-arg args :old_string))
         (new (or (harmless-tool-arg args :new_string) ""))
         (replace-all (harmless-tool-arg args :replace_all)))
    (unless (and old (not (string-empty-p old)))
      (error "old_string is empty"))
    (let* ((source (harmless-tools-fs--source-text path))
           (buf (car source))
           (original (cdr source))
           (all (harmless-json-true-p replace-all))
           (count 0)
           (start 0)
           (first nil)
           pos
           out)
      (while (setq pos (string-search old original start))
        (setq count (1+ count)
              first (or first pos)
              start (+ pos (max 1 (length old)))))
      (when (= count 0)
        (error "old_string not found in %s" path))
      (when (and (not all) (> count 1))
        (error "old_string matched %d times in %s; pass replace_all or a unique string"
               count path))
      (setq out (if all
                    (replace-regexp-in-string
                     (regexp-quote old) new original t t)
                  (concat (substring original 0 first)
                          new
                          (substring original (+ first (length old))))))
      (let ((spans (if all
                       (let ((found nil)
                             (from 0)
                             at)
                         (while (setq at (string-search old original from))
                           (push at found)
                           (setq from (+ at (max 1 (length old)))))
                         (nreverse found))
                     (list first))))
        (when buf
          (harmless-tools-fs--ensure-buffer-writable
           buf
           (mapcar (lambda (at)
                     (cons (1+ at) (+ 1 at (length old))))
                   spans)))
        (when (fboundp 'harmless-checkpoint-before)
          (harmless-checkpoint-before session path))
        (if (not buf)
            (progn
              (write-region out nil path nil 'silent)
              (format "Replaced %d occurrence(s) in %s" count path))
          (let ((narrowed (cdr (harmless-tools-fs--edit-buffer
                                buf
                                (lambda ()
                                  (condition-case nil
                                      (dolist (at (reverse spans))
                                        (let ((beg (1+ at))
                                              (end (+ 1 at (length old))))
                                          (harmless-tools-fs--refuse-read-only
                                           beg end)
                                          (goto-char beg)
                                          (delete-region beg end)
                                          (insert new)))
                                    (text-read-only
                                     (error "buffer is read-only")))
                                  (1+ first))))))
            (format "Replaced %d occurrence(s) in %s%s"
                    count path (harmless-tools-fs--note narrowed))))))))

(defun harmless-tools-fs--hunk-list (args)
  "Return the hunks from ARGS, or signal when none were given.
A vector is accepted and returned as a list.  Nil, \"\", an empty
list, and an empty vector all signal."
  (let ((hunks (harmless-tool-arg args :hunks)))
    (when (vectorp hunks)
      (setq hunks (append hunks nil)))
    (unless (consp hunks)
      (error "hunks is empty"))
    hunks))

(defun harmless-tools-fs--replace-once (text old new path n)
  "Return TEXT with one match of OLD replaced by NEW.
PATH and N identify the hunk in an error.  OLD must occur once."
  (unless (and (stringp old) (not (string-empty-p old)))
    (error "hunk %d: old_string is empty" n))
  (unless (stringp new)
    (error "hunk %d: new_string is not a string" n))
  (let ((count 0)
        (start 0)
        (at nil)
        pos)
    (while (setq pos (string-search old text start))
      (setq count (1+ count)
            at (or at pos)
            start (+ pos (max 1 (length old)))))
    (cond
     ((= count 0)
      (error "hunk %d: old_string not found in %s" n path))
     ((> count 1)
      (error "hunk %d: old_string matched %d times in %s; a hunk needs a unique string"
             n count path))
     (t (concat (substring text 0 at)
                new
                (substring text (+ at (length old))))))))

(defun harmless-tools-fs--snapshot (path buf)
  "Return the state of PATH and BUF before an edit."
  (let ((existed (and (stringp path) (file-regular-p path))))
    (list :path path
          :existed existed
          :bytes (and existed (harmless-tools-fs--file-bytes path))
          :buf (and (buffer-live-p buf) buf)
          :modified (and (buffer-live-p buf)
                         (with-current-buffer buf (buffer-modified-p)))
          :tick (and (buffer-live-p buf)
                     (with-current-buffer buf harmless-tools-fs--read-tick))
          :narrowed (and (buffer-live-p buf)
                         (with-current-buffer buf (buffer-narrowed-p)))
          :start (and (buffer-live-p buf)
                      (with-current-buffer buf (point-min)))
          :end (and (buffer-live-p buf)
                    (with-current-buffer buf (point-max)))
          :text (and (buffer-live-p buf)
                     (harmless-tools-fs--buffer-text buf)))))

(defun harmless-tools-fs--rollback (snaps)
  "Restore SNAPS taken before a patch.
A buffer the failing edit already restored is left as it is."
  (dolist (snap snaps)
    (let ((buf (plist-get snap :buf))
          (text (plist-get snap :text)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (unless (equal text (harmless-tools-fs--buffer-text buf))
            (save-restriction
              (widen)
              (when (listp buffer-undo-list)
                (setq buffer-undo-list
                      (primitive-undo 1 buffer-undo-list)))))
          (unless (equal text (harmless-tools-fs--buffer-text buf))
            (let ((buffer-undo-list t)
                  (inhibit-read-only t))
              (save-restriction
                (widen)
                (erase-buffer)
                (insert (or text "")))))
          (set-buffer-modified-p (plist-get snap :modified))
          (when (numberp (plist-get snap :tick))
            (setq harmless-tools-fs--read-tick
                  (buffer-chars-modified-tick)))
          (harmless-tools-fs--restore-narrow
           (plist-get snap :narrowed)
           (plist-get snap :start)
           (plist-get snap :end))))
      (harmless-tools-fs--restore-file
       (plist-get snap :path)
       (plist-get snap :bytes)
       (plist-get snap :existed)))))

(defun harmless-tools-fs--refuse-hunk-text (buf hunks)
  "Signal when a hunk in HUNKS would edit read-only text in BUF.
HUNKS are (OLD . NEW) in application order."
  (with-current-buffer buf
    (save-restriction
      (widen)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (dolist (hunk hunks)
          (let ((at (string-search (car hunk) text)))
            (when at
              (harmless-tools-fs--refuse-read-only
               (1+ at) (+ 1 at (length (car hunk))))
              (setq text (concat (substring text 0 at)
                                 (cdr hunk)
                                 (substring text
                                            (+ at (length (car hunk)))))))))))))

(defun harmless-tools-fs--install-hunks (buf hunks)
  "Apply HUNKS in BUF as one undo entry and save BUF.
Each hunk is (OLD . NEW).  Return (POS . NARROWED)."
  (harmless-tools-fs--edit-buffer
   buf
   (lambda ()
     (let ((first nil)
           (case-fold-search nil))
       (condition-case nil
           (dolist (hunk hunks)
             (goto-char (point-min))
             (unless (search-forward (car hunk) nil t)
               (error "old_string not found in %s" (or buffer-file-name "")))
             (let ((beg (match-beginning 0))
                   (end (match-end 0)))
               (harmless-tools-fs--refuse-read-only beg end)
               (unless first
                 (setq first beg))
               (goto-char beg)
               (delete-region beg end)
               (insert (cdr hunk))))
         (text-read-only
          (error "buffer is read-only")))
       (or first (point-min))))))

(defun harmless-tools-fs--apply-patch (session args)
  "apply_patch implementation.
Each hunk replaces one unique string.  Later hunks on the same file
see earlier replacements.  Nothing is written if any hunk fails.
A visiting buffer is edited as one undo entry and saved.  A save
that fails restores every file this patch already wrote."
  (harmless-tools-fs--reject-plan-edit session)
  (let ((hunks (harmless-tools-fs--hunk-list args))
        (texts (make-hash-table :test 'equal))
        (buffers (make-hash-table :test 'equal))
        (edits (make-hash-table :test 'equal))
        (counts (make-hash-table :test 'equal))
        (order nil)
        (n 0))
    (harmless-session-require-project session)
    (dolist (hunk hunks)
      (setq n (1+ n))
      (unless (harmless-plist-p hunk)
        (error "hunk %d is not an object" n))
      (let* ((raw (harmless-tool-arg hunk :path))
             (path (condition-case err
                       (harmless-tools-fs-resolve session raw)
                     (error (error "hunk %d: %s" n (error-message-string err)))))
             (old (harmless-tool-arg hunk :old_string))
             (new (harmless-tool-arg hunk :new_string)))
        (when (null new)
          (setq new ""))
        (unless (gethash path texts)
          (let* ((source (condition-case err
                             (harmless-tools-fs--source-text path)
                           (error
                            (let ((msg (error-message-string err)))
                              (if (member msg
                                          '("buffer is not a file"
                                            "buffer changed since it was read"
                                            "buffer is read-only"))
                                  (error "%s" msg)
                                (error "hunk %d: %s" n msg))))))
                 (buf (car source)))
            (puthash path (cdr source) texts)
            (puthash path buf buffers)
            (push path order)
            (puthash path 0 counts)))
        (puthash path
                 (harmless-tools-fs--replace-once
                  (gethash path texts) old new path n)
                 texts)
        (push (cons old new) (gethash path edits))
        (puthash path (1+ (gethash path counts)) counts)))
    (setq order (nreverse order))
    (dolist (path order)
      (puthash path (nreverse (gethash path edits)) edits)
      (let ((buf (gethash path buffers)))
        (when buf
          (with-current-buffer buf
            (harmless-tools-fs--ensure-current-editable))
          (harmless-tools-fs--refuse-hunk-text buf (gethash path edits)))))
    (when (fboundp 'harmless-checkpoint-before)
      (dolist (path order)
        (harmless-checkpoint-before session path)))
    (let ((snaps nil)
          (narrowed nil))
      (condition-case err
          (dolist (path order)
            (let ((buf (gethash path buffers)))
              (push (harmless-tools-fs--snapshot path buf) snaps)
              (if (not buf)
                  (write-region (gethash path texts) nil path nil 'silent)
                (when (cdr (harmless-tools-fs--install-hunks
                            buf (gethash path edits)))
                  (setq narrowed t)))))
        (error
         (harmless-tools-fs--rollback snaps)
         (error "%s" (error-message-string err))))
      (format "Applied %d hunk%s in %s%s"
              n
              (if (= n 1) "" "s")
              (mapconcat (lambda (path)
                           (format "%s (%d)" path (gethash path counts)))
                         order ", ")
              (harmless-tools-fs--note narrowed)))))

(defun harmless-tools-fs--list (session args)
  "list_dir implementation."
  (let* ((path (harmless-tools-fs-resolve
                session
                (or (harmless-tool-arg args :path) "."))))
    (unless (file-directory-p path)
      (error "Not a directory: %s" path))
    (mapconcat (lambda (name)
                 (let ((full (harmless-expand-file-name name path)))
                   (if (file-directory-p full)
                       (concat name "/")
                     name)))
               (directory-files path nil directory-files-no-dot-files-regexp)
               "\n")))

(defun harmless-tools-fs--glob-one (root cwd pattern)
  "Return files under ROOT whose names match PATTERN.
A name inside CWD is relative to CWD.  Any other name is absolute."
  (let (out)
    (dolist (f (directory-files-recursively root ".*" t))
      (when (and (not (file-directory-p f))
                 (harmless-tools-fs-inside-p root f)
                 (string-match-p
                  (wildcard-to-regexp (file-name-nondirectory pattern))
                  (file-name-nondirectory f))
                 (or (not (string-search "/" pattern))
                     (and (harmless-tools-fs-inside-p cwd f)
                          (string-match-p
                           (wildcard-to-regexp pattern)
                           (file-relative-name f cwd)))))
        (push (if (harmless-tools-fs-inside-p cwd f)
                  (file-relative-name f cwd)
                f)
              out)))
    (nreverse out)))

(defun harmless-tools-fs--glob (session args)
  "glob implementation.
The project and each extra directory are searched."
  (let* ((cwd (harmless-session-require-project session))
         (pattern (or (harmless-tool-arg args :pattern)
                      (harmless-tool-arg args :glob)
                      "*"))
         (roots (cons cwd (harmless-extra--usable session))))
    (when (harmless-filesystem-root-p cwd)
      (error "Refusing to search the filesystem root"))
    (mapconcat #'identity
               (apply #'append
                      (mapcar (lambda (root)
                                (harmless-tools-fs--glob-one
                                 root cwd pattern))
                              roots))
               "\n")))

(defvar harmless-tools-fs--file-type-cache (make-hash-table :test 'equal)
  "File type names keyed by search program, \"rg\" or \"ag\".")

(defun harmless-tools-fs-grep-engine (name)
  "Return a grep engine for NAME.
NAME is \"rg\", \"ag\", \"elisp\", or nil.  Nil selects ripgrep, then
ag, then the Elisp scan."
  (let ((name (if (stringp name) (downcase (string-trim name)) name)))
    (cond
     ((or (null name) (equal name ""))
      (cond ((executable-find "rg") "rg")
            ((executable-find "ag") "ag")
            (t "elisp")))
     ((member name '("rg" "ag" "elisp")) name)
     (t (error "Unknown grep engine: %s" name)))))

(defun harmless-tools-fs-grep-pattern (args)
  "Return the required pattern string from ARGS."
  (let ((pattern (or (harmless-tool-arg args :pattern)
                     (harmless-tool-arg args :query))))
    (unless (and (stringp pattern) (not (string-empty-p pattern)))
      (error "pattern is required"))
    pattern))

(defun harmless-tools-fs-grep-context (value)
  "Return a non-negative context count for VALUE."
  (cond
   ((null value) 0)
   ((and (integerp value) (>= value 0)) value)
   (t (error "context must be a non-negative integer"))))

(defun harmless-tools-fs-rg-args (pattern path context type multiline)
  "Return ripgrep arguments for PATTERN under relative PATH.
CONTEXT is a non-negative integer.  TYPE is a file type or nil.
MULTILINE enables a match across lines."
  (append
   (list "--line-number" "--no-heading" "--color" "never"
         "--hidden" "--glob" "!.git/**")
   (when (and context (> context 0))
     (list "--context" (number-to-string context)))
   (when multiline
     (list "--multiline" "--multiline-dotall"))
   (when (and type (not (string-empty-p type)))
     (list "--type" type))
   (list "--" pattern (or path "."))))

(defun harmless-tools-fs-ag-args (pattern path context type)
  "Return ag arguments for PATTERN under relative PATH.
CONTEXT is a non-negative integer.  TYPE is a file type or nil.
ag does not treat \"--\" as the end of options, and its long
context option does not take a separate argument, so context is -C."
  (append
   (list "-s" "--nocolor" "--nogroup" "--numbers" "--filename"
         "--hidden" "--ignore-dir" ".git")
   (when (and context (> context 0))
     (list "-C" (number-to-string context)))
   (when (and type (not (string-empty-p type)))
     (list (concat "--" type)))
   (list pattern (or path "."))))

(defun harmless-tools-fs--parse-rg-types (text)
  "Return ripgrep file type names listed in TEXT."
  (let (types)
    (dolist (line (split-string text "\n" t))
      (when (string-match "^\\([A-Za-z0-9_+-]+\\):" line)
        (push (match-string 1 line) types)))
    (nreverse types)))

(defun harmless-tools-fs--parse-ag-types (text)
  "Return ag file type names listed in TEXT."
  (let (types)
    (dolist (line (split-string text "\n" t))
      (when (string-match
             "^[[:space:]]*--\\([A-Za-z0-9_+-]+\\)[[:space:]]*$"
             line)
        (push (match-string 1 line) types)))
    (nreverse types)))

(defun harmless-tools-fs--read-program (program args)
  "Return stdout from PROGRAM with ARGS.  Signal when it fails."
  (with-temp-buffer
    (let* ((coding-system-for-read 'utf-8-unix)
           (coding-system-for-write 'utf-8-unix)
           (status (apply #'call-process program nil t nil args)))
      (unless (eq status 0)
        (error "Cannot list file types for %s" program))
      (harmless-ensure-utf8 (buffer-string)))))

(defun harmless-tools-fs-file-types (program)
  "Return file type names for PROGRAM, \"rg\" or \"ag\"."
  (or (gethash program harmless-tools-fs--file-type-cache)
      (let* ((text (harmless-tools-fs--read-program
                    program
                    (if (equal program "ag")
                        '("--list-file-types")
                      '("--type-list"))))
             (types (if (equal program "ag")
                        (harmless-tools-fs--parse-ag-types text)
                      (harmless-tools-fs--parse-rg-types text))))
        (unless types
          (error "Cannot list file types for %s" program))
        (puthash program types harmless-tools-fs--file-type-cache)
        types)))

(defun harmless-tools-fs-grep-require-program (engine)
  "Return the program for ENGINE, or nil for the Elisp scan.
Signal when the named program is not installed."
  (pcase engine
    ("rg" (unless (executable-find "rg")
            (error "ripgrep is not installed"))
          "rg")
    ("ag" (unless (executable-find "ag")
            (error "ag is not installed"))
          "ag")
    (_ nil)))

(defun harmless-tools-fs-grep-check-type (engine type)
  "Signal when TYPE is set and ENGINE cannot apply it."
  (when (and (stringp type) (not (string-empty-p type)))
    (if (equal engine "elisp")
        (error "file types require ripgrep or ag")
      (unless (member type (harmless-tools-fs-file-types engine))
        (error "Unknown file type for %s: %s" engine type)))))

(defun harmless-tools-fs-grep-glob-p (glob relative-path)
  "Return non-nil if RELATIVE-PATH's basename matches GLOB.
A nil or empty GLOB matches every name."
  (or (null glob)
      (not (stringp glob))
      (string-empty-p glob)
      (string-match-p (wildcard-to-regexp (file-name-nondirectory glob))
                      (file-name-nondirectory relative-path))))

(defun harmless-tools-fs-grep-normalize-line (line)
  "Return LINE with a leading ./ removed and ag context rewritten.
A match stays path:line:text.  An ag context line path:line-text
becomes path-line-text, which is ripgrep's context form."
  (let ((line (if (string-prefix-p "./" line) (substring line 2) line)))
    (if (and (not (string-match "^\\(.*\\):\\([0-9]+\\):\\(.*\\)$" line))
             (string-match "^\\(.*\\):\\([0-9]+\\)-\\(.*\\)$" line))
        (format "%s-%s-%s"
                (match-string 1 line)
                (match-string 2 line)
                (match-string 3 line))
      line)))

(defun harmless-tools-fs-grep-line-path (line)
  "Return the relative path at the start of grep LINE, or nil."
  (cond
   ((string-match "^\\(.*\\):\\([0-9]+\\):" line) (match-string 1 line))
   ((string-match "^\\(.*\\):\\([0-9]+\\)-" line) (match-string 1 line))
   ((string-match "^\\(.*\\)-\\([0-9]+\\)-" line) (match-string 1 line))
   (t nil)))

(defun harmless-tools-fs-grep-collapse (lines)
  "Join grep LINES, dropping a leading, trailing, or repeated \"--\"."
  (let (out prev)
    (dolist (line lines)
      (unless (and (string= line "--")
                   (or (null prev) (string= prev "--")))
        (push line out)
        (setq prev line)))
    (while (and out (string= (car out) "--"))
      (setq out (cdr out)))
    (if out
        (mapconcat #'identity (nreverse out) "\n")
      "")))

(defun harmless-tools-fs-grep-normalize (text glob)
  "Normalize ripgrep or ag TEXT and keep lines whose path matches GLOB."
  (let (lines)
    (dolist (line (split-string text "\n" t))
      (let* ((norm (harmless-tools-fs-grep-normalize-line line))
             (path (unless (string= norm "--")
                     (harmless-tools-fs-grep-line-path norm))))
        (when (or (string= norm "--")
                  (null path)
                  (harmless-tools-fs-grep-glob-p glob path))
          (push norm lines))))
    (harmless-tools-fs-grep-collapse (nreverse lines))))

(defun harmless-tools-fs-grep-finish (text)
  "Return TEXT to the model, or \"No matches\" when it is empty.
Long output is cut to `harmless-read-file-max-bytes'."
  (let ((text (or text "")))
    (when (string-suffix-p "\n" text)
      (setq text (substring text 0 -1)))
    (cond
     ((string-empty-p text) "No matches")
     ((> (string-bytes text) harmless-read-file-max-bytes)
      (concat (substring text 0 (min (length text)
                                     harmless-read-file-max-bytes))
              "\n[truncated]"))
     (t text))))

(defun harmless-tools-fs--search-external (program args directory)
  "Run PROGRAM with ARGS in DIRECTORY.  Return stdout.
Exit status 1 with empty stderr means there were no matches.
Stderr goes to a temporary file: this Emacs accepts a file name
there, not a buffer."
  (let ((errfile (make-temp-file "harmless-grep-err")))
    (unwind-protect
        (with-temp-buffer
          (let* ((default-directory directory)
                 (coding-system-for-read 'utf-8-unix)
                 (coding-system-for-write 'utf-8-unix)
                 (status (apply #'call-process program nil
                                (list (current-buffer) errfile) nil args))
                 (out (harmless-ensure-utf8 (buffer-string)))
                 (err (with-temp-buffer
                        (let ((coding-system-for-read 'utf-8-unix))
                          (insert-file-contents errfile))
                        (harmless-ensure-utf8 (buffer-string)))))
            (cond
             ((not (integerp status))
              (error "%s" status))
             ((and (eq status 1) (string-empty-p (string-trim err)))
              "")
             ((eq status 0) out)
             (t
              (let ((msg (string-trim err)))
                (when (> (length msg) 400)
                  (setq msg (substring msg 0 400)))
                (error "%s failed (%s): %s" program status
                       (if (string-empty-p msg) "no error output" msg)))))))
      (when (file-exists-p errfile)
        (delete-file errfile)))))

(defun harmless-tools-fs-grep-directory (session cwd path)
  "Return (absolute . relative) for PATH inside SESSION's CWD.
A nil PATH is CWD.  An empty PATH is refused."
  (let ((abs (if (null path)
                 cwd
               (harmless-tools-fs-resolve session path))))
    (unless (file-directory-p abs)
      (error "Not a directory: %s" (or path abs)))
    (cons abs
          (if (harmless-same-directory-p abs cwd)
              "."
            (file-relative-name (directory-file-name abs)
                                (directory-file-name cwd))))))

(defun harmless-tools-fs--grep-format (rel lines matches context)
  "Format MATCHES in LINES of REL.
CONTEXT includes neighboring lines.  Match lines are path:line:text
and context lines are path-line-text."
  (let* ((n (length lines))
         (show (make-vector n nil)))
    (dolist (m matches)
      (let ((start (max 1 (- m context)))
            (end (min n (+ m context)))
            (i nil))
        (setq i start)
        (while (<= i end)
          (aset show (1- i)
                (if (= i m)
                    'match
                  (or (aref show (1- i)) 'context)))
          (setq i (1+ i)))))
    (let (out any gap)
      (dotimes (idx n)
        (let* ((i (1+ idx))
               (kind (aref show idx)))
          (if (null kind)
              (setq gap any)
            (when gap
              (push "--" out)
              (setq gap nil))
            (setq any t)
            (push (format (if (eq kind 'match) "%s:%d:%s" "%s-%d-%s")
                          rel i (nth idx lines))
                  out))))
      (nreverse out))))

(defun harmless-tools-fs--grep-file (file display pattern context)
  "Return formatted grep lines for FILE, or nil when it has no match.
DISPLAY is the path printed for FILE."
  (let ((rel display)
        (lines nil)
        (matches nil)
        (n 0))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (setq n (1+ n))
        (let ((text (buffer-substring (line-beginning-position)
                                     (line-end-position))))
          (push text lines)
          (when (let ((case-fold-search nil))
                  (string-match-p pattern text))
            (push n matches)))
        (forward-line 1)))
    (when matches
      (harmless-tools-fs--grep-format
       rel (nreverse lines) (nreverse matches) context))))

(defun harmless-tools-fs--grep-elisp (session root scan pattern glob context)
  "Search SCAN for PATTERN with an Elisp line scan.
SCAN is inside SESSION's project or an extra directory.  A path
inside ROOT is printed relative to ROOT.  A path outside ROOT is
absolute."
  (let (parts)
    (dolist (file (directory-files-recursively scan ".*" nil))
      (when (and (file-regular-p file)
                 (harmless-tools-fs-allowed-p session file)
                 (harmless-tools-fs-grep-glob-p
                  glob (file-relative-name file (directory-file-name root))))
        (let ((lines (harmless-tools-fs--grep-file
                      file
                      (harmless-tools-fs--display-path file root)
                      pattern context)))
          (when lines
            (setq parts (append parts
                                (when (and parts (> context 0))
                                  (list "--"))
                                lines))))))
    (mapconcat #'identity parts "\n")))

(defun harmless-tools-fs--grep (session args)
  "grep implementation.
ENGINE selects ripgrep, ag, or the Elisp scan.  With no engine, use
the first installed program, and otherwise scan in Elisp.  Ripgrep
and ag skip ignored files and .git, and they read hidden files.
MULTILINE is ripgrep only.  With no PATH, the project and each
extra directory are searched."
  (let* ((cwd (harmless-session-require-project session))
         (pattern (harmless-tools-fs-grep-pattern args))
         (glob (harmless-tool-arg args :glob))
         (engine (harmless-tools-fs-grep-engine
                  (harmless-tool-arg args :engine)))
         (context (harmless-tools-fs-grep-context
                   (harmless-tool-arg args :context)))
         (type (harmless-tool-arg args :type))
         (multiline (harmless-json-true-p
                     (harmless-tool-arg args :multiline)))
         (raw-path (harmless-tool-arg args :path)))
    (when (harmless-filesystem-root-p cwd)
      (error "Refusing to search the filesystem root"))
    (when (and multiline (equal engine "ag"))
      (error "ag does not support multiline search"))
    (when (and (equal engine "ag") (string-prefix-p "-" pattern))
      (error "ag patterns cannot start with -"))
    (when (and multiline (equal engine "elisp"))
      (error "multiline search requires ripgrep"))
    (let* ((program (harmless-tools-fs-grep-require-program engine))
           (places (progn
                     (harmless-tools-fs-grep-check-type engine type)
                     (if raw-path
                         (list (harmless-tools-fs-grep-directory
                                session cwd raw-path))
                       (mapcar
                        (lambda (directory)
                          (cons directory
                                (if (harmless-same-directory-p
                                     directory cwd)
                                    "."
                                  (file-relative-name
                                   (directory-file-name directory)
                                   (directory-file-name cwd)))))
                        (cons cwd (harmless-extra--usable session))))))
           (parts nil))
      (dolist (place places)
        (let ((text (if (null program)
                        (harmless-tools-fs--grep-elisp
                         session cwd (car place) pattern glob context)
                      (harmless-tools-fs-grep-normalize
                       (harmless-tools-fs--search-external
                        program
                        (if (equal engine "ag")
                            (harmless-tools-fs-ag-args
                             pattern (cdr place) context type)
                          (harmless-tools-fs-rg-args
                           pattern (cdr place) context type multiline))
                        cwd)
                       glob))))
          (unless (string-empty-p text)
            (push text parts))))
      (harmless-tools-fs-grep-finish
       (mapconcat #'identity (nreverse parts) "\n")))))

(defun harmless-tools-fs-register ()
  "Register filesystem tools."
  (harmless-register-tool
   (harmless-tool-create
    :name "read_file"
    :description "Read a file in the project or an added directory. Path is relative to the project root, or absolute inside an added directory. Optional 1-based offset and limit select lines of a text file. An image returns its media type and bytes. A PDF returns extracted text. A notebook returns its cells."
    :class 'read
    :schema '(:type "object"
              :properties (:path (:type "string" :description "Path relative to the project root, or absolute inside an added directory")
                           :offset (:type "integer" :description "1-based starting line")
                           :limit (:type "integer" :description "Maximum number of lines"))
              :required ["path"])
    :fn #'harmless-tools-fs--read))
  (harmless-register-tool
   (harmless-tool-create
    :name "write_file"
    :description "Write CONTENTS to PATH, creating or replacing the file."
    :class 'edit
    :schema '(:type "object"
              :properties (:path (:type "string")
                           :contents (:type "string"))
              :required ["path" "contents"])
    :fn #'harmless-tools-fs--write))
  (harmless-register-tool
   (harmless-tool-create
    :name "replace"
    :description "Replace a unique OLD_STRING with NEW_STRING in PATH. Set replace_all to replace every match."
    :class 'edit
    :schema '(:type "object"
              :properties (:path (:type "string")
                           :old_string (:type "string")
                           :new_string (:type "string")
                           :replace_all (:type "boolean"))
              :required ["path" "old_string" "new_string"])
    :fn #'harmless-tools-fs--replace))
  (harmless-register-tool
   (harmless-tool-create
    :name "apply_patch"
    :description "Apply HUNKS in one call. Each hunk replaces one unique OLD_STRING with NEW_STRING in PATH. Later hunks on the same file see earlier replacements. If any hunk fails, no file is changed."
    :class 'edit
    :schema '(:type "object"
              :properties (:hunks
                           (:type "array"
                            :description "Replacements to apply, in order"
                            :items (:type "object"
                                    :properties (:path (:type "string")
                                                 :old_string (:type "string")
                                                 :new_string (:type "string"))
                                    :required ["path" "old_string" "new_string"])))
              :required ["hunks"])
    :fn #'harmless-tools-fs--apply-patch))
  (harmless-register-tool
   (harmless-tool-create
    :name "list_dir"
    :description "List files and subdirectories in PATH. PATH is relative to the project root, or an added directory."
    :class 'read
    :schema '(:type "object"
              :properties (:path (:type "string"))
              :required ["path"])
    :fn #'harmless-tools-fs--list))
  (harmless-register-tool
   (harmless-tool-create
    :name "glob"
    :description "Find files under the project root or an added directory matching a glob PATTERN (basename or relative path)."
    :class 'read
    :schema '(:type "object"
              :properties (:pattern (:type "string"))
              :required ["pattern"])
    :fn #'harmless-tools-fs--glob))
  (harmless-register-tool
   (harmless-tool-create
    :name "grep"
    :description "Search files for a regular expression PATTERN. ENGINE is rg, ag, or elisp. With no engine, use ripgrep, then ag, then an Elisp scan. rg and ag skip ignored files and .git, and they search hidden files. The Elisp scan reads every file. CONTEXT is the number of context lines. TYPE is a file type name from that engine. MULTILINE matches across lines and requires ripgrep. GLOB limits basenames. PATH is a directory inside the project or an added directory. With no PATH, the project and each added directory are searched."
    :class 'read
    :schema '(:type "object"
              :properties (:pattern (:type "string")
                           :glob (:type "string"
                                  :description "Basename glob, such as *.el")
                           :path (:type "string"
                                  :description "Directory in the project or an added directory")
                           :engine (:type "string"
                                    :description "rg, ag, or elisp")
                           :context (:type "integer"
                                     :description "Lines of context around each match")
                           :type (:type "string"
                                  :description "File type known to the selected engine")
                           :multiline (:type "boolean"
                                       :description "Match across lines. Ripgrep only."))
              :required ["pattern"])
    :fn #'harmless-tools-fs--grep)))

(harmless-tools-fs-register)

(provide 'harmless-tools-fs)

;;; harmless-tools-fs.el ends here
