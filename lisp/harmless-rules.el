;;; harmless-rules.el --- Path-scoped project rules for Harmless -*- lexical-binding: t; -*-

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
;; Claude Code path rules live in `.claude/rules/' under the session
;; directory, including subdirectories.  The subdirectory name does not
;; scope a rule.  A Markdown file with no `paths' field is sent on every
;; turn.  A file with `paths' is sent only when a tool call in the
;; transcript names a matching file.  `*' is one path segment.  `**' is
;; zero or more segments.  Parent directories, home, nil, "", a relative
;; path, and the filesystem root are not searched.  Nothing here is
;; written into the transcript or the session summary.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-tools)
(require 'harmless-tools-fs)

(defun harmless-path-rule--unquote (value)
  "Strip one pair of matching quotes from VALUE."
  (if (and (stringp value)
           (>= (length value) 2)
           (member (aref value 0) '(?\" ?'))
           (eq (aref value 0) (aref value (1- (length value)))))
      (substring value 1 -1)
    value))

(defun harmless-path-rule--project (dir)
  "Return absolute DIR, or nil when it cannot hold rules.
Nil, \"\", a relative path, \"~\", a drive-relative path, the
filesystem root, and a drive root are nil.  A trailing slash is the
same directory.  This does not consult `default-directory'."
  (when (stringp dir)
    (setq dir (string-trim dir))
    (unless (or (string-empty-p dir)
                (string-prefix-p "~" dir)
                (not (file-name-absolute-p dir))
                (harmless-extra-root-text-p dir)
                (string-match-p "\\`[A-Za-z]:[^/\\]" dir))
      (let ((full (directory-file-name
                   (expand-file-name
                    (subst-char-in-string ?\\ ?/ dir)))))
        (unless (or (harmless-filesystem-root-p full)
                    (harmless-extra-root-text-p full))
          (subst-char-in-string ?\\ ?/ full))))))

(defun harmless-path-rule--normalize-pattern (pattern)
  "Return PATTERN as a project-relative glob, or nil.
A leading `./' and one trailing slash are removed.  Backslashes
become slashes.  An empty result is nil."
  (setq pattern (string-trim (subst-char-in-string ?\\ ?/ pattern)))
  (while (string-prefix-p "./" pattern)
    (setq pattern (substring pattern 2)))
  (when (and (not (string-empty-p pattern))
             (string-suffix-p "/" pattern))
    (setq pattern (substring pattern 0 -1)))
  (unless (string-empty-p pattern)
    pattern))

(defun harmless-path-rule--regexp (pattern)
  "Return a regexp matching the whole of a relative path to PATTERN.
`*' is one segment.  `**/' is zero or more segments, so `**/*.el'
matches `a.el'.  Other characters are literal."
  (let ((i 0)
        (out "\\`")
        (n (length pattern)))
    (while (< i n)
      (cond
       ((and (<= (+ i 3) n)
             (eq (aref pattern i) ?*)
             (eq (aref pattern (1+ i)) ?*)
             (eq (aref pattern (+ i 2)) ?/))
        (setq out (concat out "\\(?:.*/\\)?")
              i (+ i 3)))
       ((and (< (1+ i) n)
             (eq (aref pattern i) ?*)
             (eq (aref pattern (1+ i)) ?*))
        (setq out (concat out ".*")
              i (+ i 2)))
       ((eq (aref pattern i) ?*)
        (setq out (concat out "[^/]*")
              i (1+ i)))
       ((eq (aref pattern i) ?/)
        (setq out (concat out "/")
              i (1+ i)))
       (t
        (setq out (concat out
                          (regexp-quote (string (aref pattern i)))))
        (setq i (1+ i)))))
    (concat out "\\'")))

(defun harmless-path-rule--match-p (dir pattern relative)
  "Return non-nil if RELATIVE matches PATTERN under DIR.
The match folds case when DIR's filesystem does.  An absolute
pattern, \"~\", and the filesystem root do not match."
  (when (and (stringp pattern)
             (stringp relative)
             (not (string-empty-p pattern))
             (not (string-empty-p relative))
             (not (string-prefix-p "~" pattern))
             (not (harmless-extra-root-text-p pattern))
             (not (file-name-absolute-p pattern)))
    (let ((norm (harmless-path-rule--normalize-pattern pattern))
          (case-fold-search
           (and (stringp dir)
                (file-name-case-insensitive-p dir))))
      (and norm
           (not (file-name-absolute-p norm))
           (not (harmless-extra-root-text-p norm))
           (string-match-p (harmless-path-rule--regexp norm) relative)))))

(defun harmless-path-rule--collapse-relative (text)
  "Return relative TEXT with dot segments removed, or nil.
A path that climbs above the project is nil.  TEXT is not expanded."
  (let ((parts nil)
        (escape nil))
    (dolist (part (split-string text "/" t))
      (cond
       ((string= part ".") nil)
       ((string= part "..")
        (if parts
            (setq parts (cdr parts))
          (setq escape t)))
       (t (push part parts))))
    (when (and parts (not escape))
      (mapconcat #'identity (nreverse parts) "/"))))

(defun harmless-path-rule--relative-absolute (dir text)
  "Return absolute TEXT relative to DIR, or nil when it is outside.
The filesystem root and a drive root are nil."
  (let* ((root (file-name-as-directory
                (harmless-collapse-path
                 (directory-file-name
                  (subst-char-in-string ?\\ ?/ dir)))))
         (full (condition-case nil
                   (harmless-collapse-path
                    (subst-char-in-string ?\\ ?/ text))
                 (error nil))))
    (when (and full
               (not (harmless-filesystem-root-p full))
               (not (harmless-extra-root-text-p full))
               (string-prefix-p
                root full (file-name-case-insensitive-p dir))
               (> (length full) (length root)))
      (substring full (length root)))))

(defun harmless-path-rule--relative (dir text)
  "Return TEXT as a project-relative path under DIR, or nil.
Relative TEXT is not expanded.  \"~\" is not expanded.  Nil, \"\",
the filesystem root, and a drive root are nil."
  (when (and (stringp dir) (stringp text))
    (setq text (string-trim (subst-char-in-string ?\\ ?/ text)))
    (cond
     ((string-empty-p text) nil)
     ((string-prefix-p "~" text) nil)
     ((harmless-extra-root-text-p text) nil)
     ((file-name-absolute-p text)
      (harmless-path-rule--relative-absolute dir text))
     (t (harmless-path-rule--collapse-relative text)))))

(defun harmless-path-rule--path-key-p (key)
  "Return non-nil if KEY is a file path argument."
  (or (eq key :path)
      (and (stringp key) (string= key "path"))))

(defun harmless-path-rule--walk (node dir acc)
  "Return ACC plus project-relative paths found in NODE."
  (cond
   ((harmless-plist-p node)
    (let ((rest node))
      (while rest
        (let ((key (car rest))
              (val (cadr rest)))
          (when (harmless-path-rule--path-key-p key)
            (let ((rel (harmless-path-rule--relative dir val)))
              (when rel (push rel acc))))
          (when (consp val)
            (setq acc (harmless-path-rule--walk val dir acc))))
        (setq rest (cddr rest))))
    acc)
   ((consp node)
    (dolist (item node)
      (setq acc (harmless-path-rule--walk item dir acc)))
    acc)
   (t acc)))

(defun harmless-path-rule--touched (dir messages)
  "Return project-relative paths named by tool calls in MESSAGES."
  (let ((acc nil))
    (when (listp messages)
      (dolist (msg messages)
        (when (listp msg)
          (dolist (call (plist-get msg :tool-calls))
            (when (listp call)
              (setq acc
                    (harmless-path-rule--walk
                     (harmless-tool-parse-args (plist-get call :args))
                     dir acc)))))))
    acc))

(defun harmless-path-rule--scan (dir seen)
  "Return Markdown files under DIR.
SEEN records truename keys so a symlink cycle stops.  The filesystem
root and a drive root contribute nothing."
  (let ((key (condition-case nil
                 (file-truename dir)
               (error dir))))
    (cond
     ((or (not (stringp dir))
          (not (stringp key))
          (gethash key seen)
          (harmless-filesystem-root-p dir)
          (harmless-extra-root-text-p dir)
          (harmless-filesystem-root-p key)
          (harmless-extra-root-text-p key)
          (not (file-directory-p dir)))
      nil)
     (t
      (puthash key t seen)
      (let (out)
        (dolist (name (condition-case nil
                          (directory-files dir nil "\\`[^.]" t)
                        (error nil)))
          (let ((path (expand-file-name name dir)))
            (cond
             ((and (file-directory-p path)
                   (file-exists-p path))
              (setq out (append (harmless-path-rule--scan path seen) out)))
             ((and (file-regular-p path)
                   (file-exists-p path)
                   (not (file-directory-p path))
                   (let ((case-fold-search t))
                     (string-match-p "\\.md\\'" name)))
              (push path out)))))
        out)))))

(defun harmless-path-rule--files (dir)
  "Return Markdown files under DIR's `.claude/rules/', sorted.
A missing directory is nil.  This does not create it."
  (let ((rules (expand-file-name ".claude/rules" dir)))
    (when (and (file-directory-p rules)
               (not (harmless-filesystem-root-p rules))
               (not (harmless-extra-root-text-p rules)))
      (let ((real (condition-case nil
                      (file-truename rules)
                    (error rules))))
        (unless (or (harmless-filesystem-root-p real)
                    (harmless-extra-root-text-p real))
          (sort (harmless-path-rule--scan
                 rules (make-hash-table :test 'equal))
                #'string<))))))

(defun harmless-path-rule--parse (text)
  "Return a plist describing rule TEXT.
`:scoped' is non-nil when a `paths' field is present.  `:patterns'
lists those globs, possibly empty.  `:body' is the markdown.  A
missing frontmatter is not scoped."
  (save-match-data
    (if (not (string-match
              "\\`---[ \t]*\n\\(\\(?:.*\n\\)*?\\)---[ \t]*\n?"
              text))
        (list :scoped nil :patterns nil :body (string-trim text))
      (let ((raw (match-string 1 text))
            (body (string-trim (substring text (match-end 0))))
            (patterns nil)
            (scoped nil)
            (in-list nil))
        (dolist (line (split-string raw "\n"))
          (cond
           ((and in-list
                 (string-match "\\`[ \t]*-[ \t]*\\(.*\\)\\'" line))
            (let ((item (match-string 1 line)))
              (push (harmless-path-rule--unquote (string-trim item))
                    patterns)))
           ((string-match "\\`paths[ \t]*:[ \t]*\\(.*\\)\\'" line)
            (setq scoped t
                  in-list nil)
            (let ((value (string-trim (match-string 1 line))))
              (cond
               ((or (string-empty-p value) (string= value "[]"))
                (when (string-empty-p value)
                  (setq in-list t)))
               ((member value '(">" "|" ">-" "|-"))
                nil)
               ((string-match "\\`\\[\\(.*\\)\\]\\'" value)
                (let ((inner (match-string 1 value)))
                  (dolist (part (split-string inner "," t))
                    (push (harmless-path-rule--unquote (string-trim part))
                          patterns))))
               (t
                (push (harmless-path-rule--unquote value) patterns)))))
           (t (setq in-list nil))))
        (list :scoped scoped
              :patterns (delq nil
                              (mapcar
                               (lambda (item)
                                 (and (stringp item)
                                      (not (string-empty-p item))
                                      item))
                               (nreverse patterns)))
              :body body)))))

(defun harmless-path-rule--read (path)
  "Return the parsed rule at PATH, or nil when it cannot be read."
  (let ((text (condition-case nil
                  (with-temp-buffer
                    (let ((coding-system-for-read 'utf-8-unix))
                      (insert-file-contents path))
                    (buffer-string))
                (error nil))))
    (when (stringp text)
      (harmless-path-rule--parse
       (replace-regexp-in-string "\r" "" text)))))

(defun harmless-path-rule--applies-p (dir parsed touched)
  "Return non-nil if PARSED applies to TOUCHED paths under DIR.
A rule with no paths field always applies.  A rule with paths
applies when one pattern matches one touched file."
  (if (not (plist-get parsed :scoped))
      t
    (cl-some
     (lambda (pattern)
       (cl-some
        (lambda (relative)
          (harmless-path-rule--match-p dir pattern relative))
        touched))
     (plist-get parsed :patterns))))

(defun harmless-path-rules-text (dir messages)
  "Return `.claude/rules/' text for DIR, or nil.
A rule with no paths field is included on every turn.  A rule with
paths is included when a tool call in MESSAGES names a matching
file.  Nil, \"\", a relative DIR, \"~\", and the filesystem root are
not searched, and a missing directory is not /.  The transcript is
not modified."
  (let* ((project (harmless-path-rule--project dir))
         (files (and project (harmless-path-rule--files project)))
         (touched (and files
                       (harmless-path-rule--touched project messages)))
         (chunks nil))
    (dolist (file files)
      (let ((parsed (harmless-path-rule--read file)))
        (when (and parsed
                   (not (string-empty-p
                         (or (plist-get parsed :body) "")))
                   (harmless-path-rule--applies-p
                    project parsed touched))
          (push (format "## %s\n%s" file (plist-get parsed :body))
                chunks))))
    (setq chunks (nreverse chunks))
    (when chunks
      (concat
       "Path-scoped rules for this session.  "
       "A rule with no paths field applies on every turn.  "
       "A rule with paths applies when a tool call names a matching file.  "
       "When two rules disagree, prefer the later file.\n\n"
       (mapconcat #'identity chunks "\n\n")))))

(provide 'harmless-rules)

;;; harmless-rules.el ends here
