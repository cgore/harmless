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
;; `harmless-perm-ask-function'.  Allow and deny rules can skip that
;; question or refuse the call.  Emacs settings and
;; `.harmless/permissions' are copied onto the session.  A rule set
;; only on the session is not saved.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-tools)

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

(defcustom harmless-permission-rules nil
  "Allow rules copied onto a session.
Each string is Bash(PATTERN), Edit(PATTERN), or
WebFetch(domain:HOST).  Copied when a session is created and when
it is loaded.  A rule set only on the session is not saved."
  :type '(repeat string)
  :group 'harmless)

(defcustom harmless-permission-denies nil
  "Deny rules copied onto a session.
Same strings as `harmless-permission-rules'.  A deny wins over an
allow.  These rules are not a per-project list."
  :type '(repeat string)
  :group 'harmless)

(defun harmless-perm--parse (text decision)
  "Return a rule plist for TEXT with DECISION, or signal.
DECISION is `allow' or `deny'.  The stored text is TEXT trimmed."
  (unless (stringp text)
    (error "rule must be a string"))
  (setq text (string-trim text))
  (when (string-empty-p text)
    (error "rule is empty"))
  (save-match-data
    (unless (string-match
             "\\`\\(Bash\\|Edit\\|WebFetch\\)(\\(.*\\))\\'" text)
      (error "rule is not a rule"))
    (let ((kind (match-string 1 text))
          (body (match-string 2 text)))
      (cond
       ((string= kind "Bash")
        (setq body (string-trim body))
        (when (string-empty-p body)
          (error "rule pattern is empty"))
        (list :decision decision :kind 'bash
              :pattern body :text text))
       ((string= kind "Edit")
        (list :decision decision :kind 'edit
              :pattern (harmless-perm--edit-pattern body)
              :text text))
       (t
        (list :decision decision :kind 'fetch
              :pattern (harmless-perm--domain body)
              :text text))))))

(defun harmless-perm--edit-pattern (raw)
  "Return the project-relative Edit pattern in RAW, or signal.
A leading slash is the project.  It is not the filesystem root."
  (setq raw (string-trim raw))
  (when (string-empty-p raw)
    (error "rule pattern is empty"))
  (when (string-prefix-p "/" raw)
    (setq raw (substring raw 1)))
  (when (or (string-empty-p raw)
            (string-match-p "\\`/*\\'" raw)
            (string-match-p "\\`[A-Za-z]:" raw))
    (error "rule pattern is not a path"))
  (dolist (seg (split-string raw "[\\\\/]" t))
    (when (member seg '(".." "~"))
      (error "rule pattern escapes the project")))
  raw)

(defun harmless-perm--domain (raw)
  "Return the downcased host in a WebFetch body RAW, or signal."
  (setq raw (string-trim raw))
  (unless (string-prefix-p "domain:" raw)
    (error "rule domain is missing"))
  (setq raw (string-trim (substring raw (length "domain:"))))
  (when (string-empty-p raw)
    (error "rule domain is empty"))
  (when (or (string-match-p "[ /\\\\]" raw)
            (string-prefix-p "." raw)
            (string-suffix-p "." raw)
            (string-match-p "\\`[A-Za-z]:" raw))
    (error "rule domain is not a domain"))
  (downcase raw))

(defun harmless-perm--parse-list (texts decision)
  "Return rule plists for TEXTS with DECISION, or signal.
Nil is an empty list.  Any other non-list is refused."
  (cond
   ((null texts) nil)
   ((not (listp texts)) (error "rules must be a list"))
   (t (mapcar (lambda (text)
                (harmless-perm--parse text decision))
              texts))))

(defun harmless-perm--project-file (session)
  "Return SESSION's `.harmless/permissions' path, or nil.
Nil, \"\", a relative directory, and the filesystem root have
none.  The path is not expanded against `default-directory'."
  (let ((cwd (harmless-session-cwd session)))
    (when (and (stringp cwd)
               (not (string-empty-p cwd))
               (file-name-absolute-p cwd)
               (not (harmless-filesystem-root-p cwd)))
      (let ((file (expand-file-name ".harmless/permissions" cwd)))
        (when (and (file-name-absolute-p file)
                   (not (harmless-filesystem-root-p file))
                   (not (harmless-filesystem-root-p
                         (file-name-directory file))))
          file)))))

(defun harmless-perm--read-project-file (file)
  "Return allow rules read from FILE.
A missing file has none.  A directory is refused.  A bad line
names FILE and the rule error."
  (cond
   ((not (file-exists-p file)) nil)
   ((file-directory-p file)
    (error "permissions file is a directory"))
   ((not (file-readable-p file))
    (error "Cannot read %s" file))
   (t
    (let (lines)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8-unix))
          (insert-file-contents file))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (string-trim
                       (buffer-substring
                        (line-beginning-position)
                        (line-end-position)))))
            (when (and (not (string-empty-p line))
                       (not (string-prefix-p "#" line)))
              (condition-case err
                  (push (harmless-perm--parse line 'allow) lines)
                (error
                 (error "%s: %s"
                        file (error-message-string err))))))
          (forward-line 1)))
      (nreverse lines)))))

(defun harmless-perm--project-rules (session)
  "Return allow rules from SESSION's project file, or nil."
  (let ((file (harmless-perm--project-file session)))
    (and file (harmless-perm--read-project-file file))))

(defun harmless-perm-install-configured-rules (session)
  "Copy configured and project rules onto SESSION.
Allow rules from Emacs come first, then `.harmless/permissions',
then deny rules.  The previous in-memory rules are replaced.
Nothing is saved."
  (let ((rules (append
                (harmless-perm--parse-list
                 harmless-permission-rules 'allow)
                (harmless-perm--project-rules session)
                (harmless-perm--parse-list
                 harmless-permission-denies 'deny))))
    (setf (harmless-session-rules session) rules)))

(defun harmless-perm-set-rules (session allows denies)
  "Replace SESSION's rules with ALLOWS and DENIES.
Both lists are validated before the slot changes.  Nothing is
saved.  Return the new rules."
  (let ((rules (append (harmless-perm--parse-list allows 'allow)
                       (harmless-perm--parse-list denies 'deny))))
    (setf (harmless-session-rules session) rules)))

(defun harmless-perm--path-regexp (pattern)
  "Return a regexp for a project-relative Edit PATTERN.
`*' is one path segment.  `**' crosses segments.  A trailing
slash means that directory and everything under it."
  (let ((body pattern)
        (i 0)
        (out "\\`"))
    (when (and (string-suffix-p "/" body)
               (not (string-suffix-p "**" body)))
      (setq body (concat body "**")))
    (while (< i (length body))
      (let ((ch (aref body i)))
        (cond
         ((and (eq ch ?*)
               (< (1+ i) (length body))
               (eq (aref body (1+ i)) ?*))
          (setq out (concat out ".*")
                i (+ i 2)))
         ((eq ch ?*)
          (setq out (concat out "[^/]*")
                i (1+ i)))
         ((memq ch '(?/ ?\\))
          (setq out (concat out "/")
                i (1+ i)))
         (t
          (setq out (concat out (regexp-quote (string ch)))
                i (1+ i))))))
    (concat out "\\'")))

(defun harmless-perm--path-match-p (session pattern relative)
  "Return non-nil if RELATIVE matches Edit PATTERN for SESSION.
The match folds case when SESSION's project directory does."
  (let* ((cwd (harmless-session-cwd session))
         (case-fold-search
          (and (stringp cwd)
               (file-name-case-insensitive-p cwd))))
    (string-match-p (harmless-perm--path-regexp pattern) relative)))

(defun harmless-perm--bash-match-p (pattern command)
  "Return non-nil if COMMAND matches Bash PATTERN.
The match covers the whole trimmed command.  `*' matches any
characters, including spaces and slashes."
  (when (and (stringp pattern) (stringp command))
    (string-match-p
     (concat "\\`" (wildcard-to-regexp pattern) "\\'")
     (string-trim command))))

(defun harmless-perm--host (url)
  "Return the downcased host of URL, or nil when URL has none."
  (save-match-data
    (when (and (stringp url)
               (string-match
                "\\`https?://\\(?:[^/@]+@\\)?\\([^/:]+\\)" url))
      (downcase (match-string 1 url)))))

(defun harmless-perm--domain-match-p (host domain)
  "Return non-nil if HOST is DOMAIN or a subdomain of it."
  (and (stringp host) (stringp domain)
       (or (string= host domain)
           (string-suffix-p (concat "." domain) host))))

(defun harmless-perm--slash (path)
  "Return PATH with backslashes written as slashes."
  (subst-char-in-string ?\\ ?/ path))

(defun harmless-perm--clean-relative (path)
  "Return relative PATH with slashes normalized, or nil.
`..' and `~' do not match.  PATH is not expanded."
  (let ((norm (harmless-perm--slash path)))
    (if (cl-some (lambda (seg) (member seg '(".." "~")))
                 (split-string norm "/" t))
        nil
      norm)))

(defun harmless-perm--under-root (cwd path)
  "Return PATH relative to CWD, or nil when it is not strictly under.
PATH is absolute.  It is expanded on its own, so a relative name
is not resolved against `default-directory'.  The filesystem root
and CWD itself do not match."
  (when (and (stringp cwd) (file-name-absolute-p cwd)
             (stringp path) (file-name-absolute-p path)
             (not (harmless-filesystem-root-p cwd)))
    (let* ((ignore (file-name-case-insensitive-p cwd))
           (root (harmless-perm--slash
                  (file-name-as-directory
                   (directory-file-name (expand-file-name cwd)))))
           (full (harmless-perm--slash (expand-file-name path))))
      (when (and (string-prefix-p root full ignore)
                 (not (string-empty-p (substring full (length root)))))
        (substring full (length root))))))

(defun harmless-perm--relative (session path)
  "Return PATH relative to SESSION's project, or nil.
A relative path is not expanded.  Nil, \"\", the project itself,
and a path outside the project do not match."
  (cond
   ((not (and (stringp path) (not (string-empty-p path)))) nil)
   ((or (file-name-absolute-p path) (string-prefix-p "~" path))
    (harmless-perm--under-root (harmless-session-cwd session) path))
   (t (harmless-perm--clean-relative path))))

(defun harmless-perm--subjects (session tool args)
  "Return (KIND . VALUES) for TOOL on SESSION, or nil.
KIND is `bash', `edit', or `fetch'.  ARGS that are not a subject
return nil, and no rule applies."
  (ignore session)
  (let ((name (harmless-tool-name tool)))
    (cond
     ((string= name "run_shell")
      (let ((command (harmless-tool-arg args :command)))
        (when (stringp command)
          (cons 'bash (list command)))))
     ((string= name "monitor")
      (when (and (null (harmless-tool-arg args :path))
                 (null (harmless-tool-arg args :shell))
                 (stringp (harmless-tool-arg args :command)))
        (cons 'bash (list (harmless-tool-arg args :command)))))
     ((member name '("write_file" "replace"))
      (let ((path (harmless-tool-arg args :path)))
        (when (stringp path)
          (cons 'edit (list path)))))
     ((string= name "apply_patch")
      (let ((hunks (harmless-tool-arg args :hunks))
            paths)
        (when (vectorp hunks)
          (setq hunks (append hunks nil)))
        (when (listp hunks)
          (dolist (hunk hunks)
            (when (harmless-plist-p hunk)
              (let ((path (harmless-tool-arg hunk :path)))
                (when (stringp path)
                  (push path paths))))))
        (when paths
          (cons 'edit (nreverse paths)))))
     ((string= name "web_fetch")
      (let ((url (harmless-tool-arg args :url)))
        (when (stringp url)
          (cons 'fetch (list url))))))))

(defun harmless-perm--match-p (session rule value)
  "Return non-nil if RULE matches VALUE for SESSION."
  (let ((pattern (plist-get rule :pattern)))
    (pcase (plist-get rule :kind)
      ('bash (harmless-perm--bash-match-p pattern value))
      ('edit
       (let ((rel (harmless-perm--relative session value)))
         (and rel (harmless-perm--path-match-p session pattern rel))))
      ('fetch
       (harmless-perm--domain-match-p
        (harmless-perm--host value) pattern)))))

(defun harmless-perm-rule-refusal (session tool args)
  "Return a denial when a deny rule matches TOOL on SESSION.
The string includes \"Error: \".  The first matching deny wins.
Nil means no deny matched."
  (let ((subjects (harmless-perm--subjects session tool args))
        (hit nil))
    (when subjects
      (dolist (rule (harmless-session-rules session))
        (when (and (not hit)
                   (eq (plist-get rule :decision) 'deny)
                   (eq (plist-get rule :kind) (car subjects))
                   (cl-some
                    (lambda (value)
                      (harmless-perm--match-p session rule value))
                    (cdr subjects)))
          (setq hit rule))))
    (when hit
      (format "Error: Permission rule denied %s"
              (plist-get hit :text)))))

(defun harmless-perm-rule-allows-p (session tool args)
  "Return non-nil when every subject of TOOL matches an allow rule.
Nil when TOOL has no subject, or any subject is unmatched.  A
deny is a separate check and still wins."
  (let ((subjects (harmless-perm--subjects session tool args)))
    (and subjects
         (cl-every
          (lambda (value)
            (cl-some
             (lambda (rule)
               (and (eq (plist-get rule :decision) 'allow)
                    (eq (plist-get rule :kind) (car subjects))
                    (harmless-perm--match-p session rule value)))
             (harmless-session-rules session)))
          (cdr subjects)))))

(provide 'harmless-perm)

;;; harmless-perm.el ends here
