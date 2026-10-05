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
;; Paths must stay inside the session cwd.  apply_patch writes nothing
;; when any hunk fails.  grep uses ripgrep, then ag, then an Elisp scan.

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
  (let* ((cwd (harmless-session-require-project session))
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
                       "")))
    (let ((parent (file-name-directory path)))
      ;; / already exists.  Refusing to create it must not block a file
      ;; whose project really is the filesystem root.
      (unless (and parent (file-directory-p parent))
        (harmless-ensure-directory parent)))
    (write-region contents nil path nil 'silent)
    (harmless-tools-fs-revert-visiting path)
    (format "Wrote %s (%d bytes)" path (string-bytes contents))))

(defun harmless-tools-fs--replace (session args)
  "replace implementation (unique old_string -> new_string)."
  (harmless-tools-fs--reject-plan-edit session)
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

(defun harmless-tools-fs--apply-patch (session args)
  "apply_patch implementation.
Each hunk replaces one unique string.  Later hunks on the same file
see earlier replacements.  Nothing is written if any hunk fails."
  (harmless-tools-fs--reject-plan-edit session)
  (let ((hunks (harmless-tools-fs--hunk-list args))
        (texts (make-hash-table :test 'equal))
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
          (unless (and (file-regular-p path) (file-readable-p path))
            (error "hunk %d: Cannot read %s" n path))
          (puthash path
                   (with-temp-buffer
                     (insert-file-contents path)
                     (buffer-string))
                   texts)
          (push path order)
          (puthash path 0 counts))
        (puthash path
                 (harmless-tools-fs--replace-once
                  (gethash path texts) old new path n)
                 texts)
        (puthash path (1+ (gethash path counts)) counts)))
    (setq order (nreverse order))
    (dolist (path order)
      (write-region (gethash path texts) nil path nil 'silent)
      (harmless-tools-fs-revert-visiting path))
    (format "Applied %d hunk%s in %s"
            n
            (if (= n 1) "" "s")
            (mapconcat (lambda (path)
                         (format "%s (%d)" path (gethash path counts)))
                       order ", "))))

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
  (let* ((cwd (harmless-session-require-project session))
         (pattern (or (harmless-tool-arg args :pattern)
                      (harmless-tool-arg args :glob)
                      "*")))
    (when (harmless-filesystem-root-p cwd)
      (error "Refusing to search the filesystem root"))
    (let ((files (directory-files-recursively cwd ".*" t)))
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
                 "\n"))))

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

(defun harmless-tools-fs--grep-file (file cwd pattern context)
  "Return formatted grep lines for FILE, or nil when it has no match."
  (let ((rel (file-relative-name file (directory-file-name cwd)))
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

(defun harmless-tools-fs--grep-elisp (root scan pattern glob context)
  "Search SCAN, which is inside ROOT, with an Elisp line scan.
Result paths are relative to ROOT."
  (let (parts)
    (dolist (file (directory-files-recursively scan ".*" nil))
      (when (and (file-regular-p file)
                 (harmless-tools-fs-inside-p root file)
                 (harmless-tools-fs-grep-glob-p
                  glob (file-relative-name file (directory-file-name root))))
        (let ((lines (harmless-tools-fs--grep-file file root pattern context)))
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
MULTILINE is ripgrep only."
  (let* ((cwd (harmless-session-require-project session))
         (pattern (harmless-tools-fs-grep-pattern args))
         (glob (harmless-tool-arg args :glob))
         (engine (harmless-tools-fs-grep-engine
                  (harmless-tool-arg args :engine)))
         (context (harmless-tools-fs-grep-context
                   (harmless-tool-arg args :context)))
         (type (harmless-tool-arg args :type))
         (multiline (harmless-json-true-p
                     (harmless-tool-arg args :multiline))))
    (when (harmless-filesystem-root-p cwd)
      (error "Refusing to search the filesystem root"))
    (when (and multiline (equal engine "ag"))
      (error "ag does not support multiline search"))
    (when (and (equal engine "ag") (string-prefix-p "-" pattern))
      (error "ag patterns cannot start with -"))
    (when (and multiline (equal engine "elisp"))
      (error "multiline search requires ripgrep"))
    (let* ((program (harmless-tools-fs-grep-require-program engine))
           (place (progn
                    (harmless-tools-fs-grep-check-type engine type)
                    (harmless-tools-fs-grep-directory
                     session cwd (harmless-tool-arg args :path))))
           (text (if (null program)
                     (harmless-tools-fs--grep-elisp
                      cwd (car place) pattern glob context)
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
      (harmless-tools-fs-grep-finish text))))

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
    :description "Search project files for a regular expression PATTERN. ENGINE is rg, ag, or elisp. With no engine, use ripgrep, then ag, then an Elisp scan. rg and ag skip ignored files and .git, and they search hidden files. The Elisp scan reads every file. CONTEXT is the number of context lines. TYPE is a file type name from that engine. MULTILINE matches across lines and requires ripgrep. GLOB limits basenames. PATH is a directory inside the project."
    :class 'read
    :schema '(:type "object"
              :properties (:pattern (:type "string")
                           :glob (:type "string"
                                  :description "Basename glob, such as *.el")
                           :path (:type "string"
                                  :description "Directory relative to the project root")
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
