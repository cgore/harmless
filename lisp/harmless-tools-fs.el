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
;; read_file, write_file, replace, list_dir, grep, glob.  Paths must stay
;; inside the session cwd.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-tools)

(defcustom harmless-read-file-max-bytes 100000
  "Maximum number of bytes `read_file' returns to the model."
  :type 'integer
  :group 'harmless)

(defun harmless-tools-fs-inside-p (root path)
  "Return non-nil if PATH is inside ROOT after resolving `..'."
  (let* ((root (file-name-as-directory (file-truename root)))
         (path (file-truename path)))
    (or (string-prefix-p root path)
        (string-prefix-p root (file-name-as-directory path))
        (string= (directory-file-name root) path))))

(defun harmless-tools-fs-resolve (session path)
  "Resolve PATH against SESSION cwd, or signal if it escapes the project."
  (unless (and path (not (string-empty-p path)))
    (error "Path is empty"))
  (let* ((cwd (harmless-session-cwd session))
         (full (expand-file-name path cwd)))
    (unless (harmless-tools-fs-inside-p cwd full)
      (error "Path escapes project: %s" path))
    full))

(defun harmless-tools-fs-revert-visiting (path)
  "Revert any buffer visiting PATH."
  (when-let* ((buf (find-buffer-visiting path)))
    (with-current-buffer buf
      (revert-buffer t t t))))

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

(defun harmless-tools-fs--read (session args)
  "read_file implementation."
  (let* ((path (harmless-tools-fs-resolve session (harmless-tool-arg args :path)))
         (offset (harmless-tool-arg args :offset))
         (limit (harmless-tool-arg args :limit)))
    (unless (file-readable-p path)
      (error "Cannot read %s" path))
    (with-temp-buffer
      (insert-file-contents path)
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
      (buffer-string))))

(defun harmless-tools-fs--write (session args)
  "write_file implementation."
  (let* ((path (harmless-tools-fs-resolve session (harmless-tool-arg args :path)))
         (contents (or (harmless-tool-arg args :contents)
                       (harmless-tool-arg args :content)
                       "")))
    (harmless-ensure-directory (file-name-directory path))
    (write-region contents nil path nil 'silent)
    (harmless-tools-fs-revert-visiting path)
    (format "Wrote %s (%d bytes)" path (string-bytes contents))))

(defun harmless-tools-fs--replace (session args)
  "replace implementation (unique old_string -> new_string)."
  (let* ((path (harmless-tools-fs-resolve session (harmless-tool-arg args :path)))
         (old (harmless-tool-arg args :old_string))
         (new (or (harmless-tool-arg args :new_string) ""))
         (replace-all (harmless-tool-arg args :replace_all)))
    (unless (and old (not (string-empty-p old)))
      (error "old_string is empty"))
    (unless (file-readable-p path)
      (error "Cannot read %s" path))
    (let* ((original (with-temp-buffer
                       (insert-file-contents path)
                       (buffer-string)))
           (all (harmless-json-true-p replace-all))
           (count 0)
           (start 0)
           pos
           out)
      (while (setq pos (string-search old original start))
        (setq count (1+ count)
              start (+ pos (max 1 (length old)))))
      (when (= count 0)
        (error "old_string not found in %s" path))
      (when (and (not all) (> count 1))
        (error "old_string matched %d times in %s; pass replace_all or a unique string"
               count path))
      (setq out (if all
                    (replace-regexp-in-string (regexp-quote old) new original t t)
                  (let ((at (string-search old original)))
                    (concat (substring original 0 at)
                            new
                            (substring original (+ at (length old)))))))
      (write-region out nil path nil 'silent)
      (harmless-tools-fs-revert-visiting path)
      (format "Replaced %d occurrence(s) in %s" count path))))

(defun harmless-tools-fs--list (session args)
  "list_dir implementation."
  (let* ((path (harmless-tools-fs-resolve
                session
                (or (harmless-tool-arg args :path) "."))))
    (unless (file-directory-p path)
      (error "Not a directory: %s" path))
    (mapconcat (lambda (name)
                 (let ((full (expand-file-name name path)))
                   (if (file-directory-p full)
                       (concat name "/")
                     name)))
               (directory-files path nil directory-files-no-dot-files-regexp)
               "\n")))

(defun harmless-tools-fs--glob (session args)
  "glob implementation."
  (let* ((cwd (harmless-session-cwd session))
         (pattern (or (harmless-tool-arg args :pattern)
                      (harmless-tool-arg args :glob)
                      "*"))
         (files (directory-files-recursively cwd ".*" t)))
    (mapconcat (lambda (f)
                 (file-relative-name f cwd))
               (seq-filter
                (lambda (f)
                  (and (harmless-tools-fs-inside-p cwd f)
                       (not (file-directory-p f))
                       (string-match-p
                        (wildcard-to-regexp (file-name-nondirectory pattern))
                        (file-name-nondirectory f))
                       (or (not (string-search "/" pattern))
                           (string-match-p
                            (wildcard-to-regexp pattern)
                            (file-relative-name f cwd)))))
                files)
               "\n")))

(defun harmless-tools-fs--grep (session args)
  "grep implementation, Elisp so tests do not need ripgrep."
  (let* ((cwd (harmless-session-cwd session))
         (pattern (or (harmless-tool-arg args :pattern)
                      (harmless-tool-arg args :query)))
         (glob (harmless-tool-arg args :glob))
         (re (or pattern (error "pattern is required")))
         (hits nil)
         (count 0))
    (dolist (file (directory-files-recursively cwd ".*" nil))
      (when (and (file-regular-p file)
                 (harmless-tools-fs-inside-p cwd file)
                 (or (null glob)
                     (string-match-p (wildcard-to-regexp
                                      (file-name-nondirectory glob))
                                     (file-name-nondirectory file))))
        (let ((line-no 0))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (while (not (eobp))
              (setq line-no (1+ line-no))
              (when (string-match-p re (buffer-substring
                                        (line-beginning-position)
                                        (line-end-position)))
                (push (format "%s:%d:%s"
                              (file-relative-name file cwd)
                              line-no
                              (buffer-substring (line-beginning-position)
                                                (line-end-position)))
                      hits)
                (setq count (1+ count)))
              (forward-line 1))))))
    (if hits
        (mapconcat #'identity (nreverse hits) "\n")
      "No matches")))

(defun harmless-tools-fs-register ()
  "Register filesystem tools."
  (harmless-register-tool
   (harmless-tool-create
    :name "read_file"
    :description "Read a file in the project. Path is relative to the project root. Optional 1-based offset and limit select lines."
    :class 'read
    :schema '(:type "object"
              :properties (:path (:type "string" :description "Path relative to the project root")
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
    :name "list_dir"
    :description "List files and subdirectories in PATH (relative to the project root)."
    :class 'read
    :schema '(:type "object"
              :properties (:path (:type "string"))
              :required ["path"])
    :fn #'harmless-tools-fs--list))
  (harmless-register-tool
   (harmless-tool-create
    :name "glob"
    :description "Find files under the project root matching a glob PATTERN (basename or relative path)."
    :class 'read
    :schema '(:type "object"
              :properties (:pattern (:type "string"))
              :required ["pattern"])
    :fn #'harmless-tools-fs--glob))
  (harmless-register-tool
   (harmless-tool-create
    :name "grep"
    :description "Search project files for a regular expression PATTERN. Optional GLOB limits by filename."
    :class 'read
    :schema '(:type "object"
              :properties (:pattern (:type "string")
                           :glob (:type "string"))
              :required ["pattern"])
    :fn #'harmless-tools-fs--grep)))

(harmless-tools-fs-register)

(provide 'harmless-tools-fs)

;;; harmless-tools-fs.el ends here
