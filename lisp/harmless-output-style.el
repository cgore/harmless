;;; harmless-output-style.el --- Session role, tone, and format -*- lexical-binding: t; -*-

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
;; An output style is a role, tone, and format for one session.  Switching
;; it does not edit project instructions.  The chosen text is saved with
;; the session.  `default' clears it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-instructions)
(require 'harmless-tools-fs)

(defvar harmless-output-style-active (make-hash-table :test 'equal)
  "Output style plists, keyed by session id.
Each value is :name, :body, and optionally :description.")

(defun harmless-output-style-name-p (name)
  "Return non-nil if NAME is a style id, not a path."
  (and (stringp name)
       (string-match-p "\\`[a-z0-9]+\\(?:-[a-z0-9]+\\)*\\'" name)))

(defun harmless-output-style--normalize (name)
  "Return NAME as a lowercase hyphenated style id."
  (let ((slug (replace-regexp-in-string
               "[^a-z0-9]+" "-"
               (downcase (string-trim (or name ""))))))
    (setq slug (replace-regexp-in-string "\\`-+" "" slug))
    (replace-regexp-in-string "-+\\'" "" slug)))

(defun harmless-output-style--clear-p (name)
  "Return non-nil if NAME clears the output style.
Nil, \"\", and \"default\" clear.  Whitespace does not."
  (or (null name)
      (and (stringp name) (equal name ""))
      (and (stringp name)
           (equal (harmless-output-style--normalize (string-trim name))
                  "default"))))

(defun harmless-output-style-name (name)
  "Return NAME as a style id, or signal.
Nil and \"\" are empty.  A path, \"~\", and the filesystem root are
not names and are not expanded.  \"default\" is a style id here;
`harmless-output-style-set' treats it as clear."
  (cond
   ((null name) (error "output style name is empty"))
   ((not (stringp name)) (error "output style name must be a string"))
   (t
    (let ((raw (string-trim name)))
      (cond
       ((string-empty-p raw) (error "output style name is empty"))
       ((or (string-prefix-p "~" raw)
            (string-prefix-p "/" raw)
            (string-prefix-p "\\" raw)
            (string-match-p "[/\\\\]" raw)
            (string-match-p "\\`[A-Za-z]:[/\\]?\\'" raw)
            (file-name-absolute-p raw))
        (error "output style name is not a name"))
       (t
        (setq raw (harmless-output-style--normalize raw))
        (if (harmless-output-style-name-p raw)
            raw
          (error "output style name is not a name"))))))))

(defun harmless-output-style--root-text-p (text)
  "Return non-nil if TEXT is \"/\" or a drive root."
  (or (harmless-filesystem-root-p text)
      (and (stringp text)
           (or (string-match-p "\\`[A-Za-z]:[/\\]?\\'" (string-trim text))
               (harmless-extra-root-text-p text)))))

(defun harmless-output-style--safe-dir (dir)
  "Return DIR as a directory, or nil when it is not safe.
This does not create DIR and does not consult `default-directory'."
  (when (and (stringp dir)
             (not (string-empty-p dir))
             (file-name-absolute-p dir)
             (not (string-prefix-p "~" dir))
             (not (harmless-output-style--root-text-p dir)))
    (file-name-as-directory dir)))

(defun harmless-output-style--cwd (session)
  "Return SESSION's project directory, or signal.
A blank cwd signals \"This chat is not attached to a project\".
A relative path signals \"output style directory must be absolute\".
The filesystem root signals \"output style directory is the
filesystem root\".  This does not consult `default-directory'."
  (let ((cwd (harmless-session-cwd session)))
    (cond
     ((or (not (stringp cwd))
          (string-empty-p (string-trim cwd)))
      (error "This chat is not attached to a project"))
     ((harmless-output-style--root-text-p cwd)
      (error "output style directory is the filesystem root"))
     ((or (string-prefix-p "~" (string-trim cwd))
          (not (file-name-absolute-p cwd)))
      (error "output style directory must be absolute"))
     (t (file-name-as-directory (directory-file-name cwd))))))

(defun harmless-output-style--data-dir ()
  "Return the user output-style directory, or nil when it is not safe.
This does not create the directory."
  (condition-case nil
      (harmless-output-style--safe-dir
       (expand-file-name "output-styles" (harmless-data-directory)))
    (error nil)))

(defun harmless-output-style--home-claude ()
  "Return ~/.claude/output-styles when that path is safe, or nil.
This does not create the directory."
  (condition-case nil
      (harmless-output-style--safe-dir
       (expand-file-name
        "output-styles"
        (expand-file-name ".claude" "~")))
    (error nil)))

(defun harmless-output-style--user-dirs ()
  "Return user style directories, lowest priority first."
  (delq nil (list (harmless-output-style--home-claude)
                  (harmless-output-style--data-dir))))

(defun harmless-output-style--project-dirs (cwd)
  "Return project style directories under CWD, lowest priority first."
  (delq nil
        (mapcar #'harmless-output-style--safe-dir
                (list (expand-file-name ".claude/output-styles" cwd)
                      (expand-file-name ".harmless/output-styles" cwd)))))

(defun harmless-output-style--dirs (session)
  "Return style directories for SESSION, lowest priority first.
A detached chat has the user directories only."
  (if (harmless-session-detached-p session)
      (harmless-output-style--user-dirs)
    (append (harmless-output-style--user-dirs)
            (harmless-output-style--project-dirs
             (harmless-output-style--cwd session)))))

(defun harmless-output-style--unquote (value)
  "Strip one pair of matching quotes from VALUE."
  (if (and (stringp value)
           (>= (length value) 2)
           (member (aref value 0) '(?\" ?'))
           (eq (aref value 0) (aref value (1- (length value)))))
      (substring value 1 -1)
    value))

(defun harmless-output-style--read (file)
  "Return the text of FILE."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents file))
    (buffer-string)))

(defun harmless-output-style--front (text)
  "Return (NAME DESCRIPTION BODY) from TEXT.
NAME and DESCRIPTION are nil when the field is absent.  BODY is the
markdown after the frontmatter, or all of TEXT when there is none.
CRLF is read as a newline."
  (save-match-data
    (setq text (replace-regexp-in-string "\r\n?" "\n" (or text "")))
    (if (not (string-match
              "\\`---[ \t]*\n\\(\\(?:.*\n\\)*?\\)---[ \t]*\n?"
              text))
        (list nil nil text)
      (let ((raw (match-string 1 text))
            (body (substring text (match-end 0)))
            (name nil)
            (description nil))
        (dolist (line (split-string raw "\n"))
          (when (string-match
                 "\\`\\([A-Za-z0-9_-]+\\):[ \t]*\\(.*\\)\\'"
                 line)
            (let ((key (match-string 1 line))
                  (value (match-string 2 line)))
              (setq value (harmless-output-style--unquote
                           (string-trim value)))
              (cond
               ((string= key "name") (setq name value))
               ((and (string= key "description")
                     (not (string-empty-p value)))
                (setq description value))))))
        (list name description body)))))

(defun harmless-output-style--parse (file)
  "Return a style plist for FILE, or nil when it is not a style.
A `default.md' file is not a style.  A frontmatter name that
normalizes to a different id is not a style."
  (let* ((id (harmless-output-style--normalize
              (file-name-sans-extension
               (file-name-nondirectory file))))
         (parsed (harmless-output-style--front
                  (harmless-output-style--read file)))
         (named (nth 0 parsed))
         (description (nth 1 parsed))
         (body (or (nth 2 parsed) "")))
    (when (and (harmless-output-style-name-p id)
               (not (equal id "default"))
               (or (null named)
                   (and (stringp named)
                        (equal (harmless-output-style--normalize named)
                               id))))
      (list :name id
            :description (and (stringp description)
                              (not (string-empty-p
                                    (string-trim description)))
                              (string-trim description))
            :body body
            :path file))))

(defun harmless-output-style--collect (dirs)
  "Return a hash of styles in DIRS.  Later directories win."
  (let ((by-name (make-hash-table :test 'equal)))
    (dolist (dir dirs)
      (when (and (stringp dir)
                 (file-directory-p dir)
                 (not (harmless-output-style--root-text-p dir)))
        (dolist (name (sort (directory-files dir nil "\\.md\\'")
                            #'string<))
          (unless (or (string-empty-p name) (eq (aref name 0) ?.))
            (let ((path (expand-file-name name dir)))
              (when (file-regular-p path)
                (let ((style (harmless-output-style--parse path)))
                  (when style
                    (puthash (plist-get style :name) style by-name)))))))))
    by-name))

(defun harmless-output-style-list (session)
  "Return styles SESSION can switch to, sorted by name.
A detached chat sees user styles only.  This does not create
directories and does not edit project instructions."
  (unless (harmless-session-p session)
    (error "No Harmless session"))
  (let ((styles nil))
    (maphash (lambda (_name style) (push style styles))
             (harmless-output-style--collect
              (harmless-output-style--dirs session)))
    (sort styles (lambda (a b)
                   (string< (plist-get a :name)
                            (plist-get b :name))))))

(defun harmless-output-style-find (session name)
  "Return the style named NAME for SESSION, or nil."
  (setq name (harmless-output-style-name name))
  (cl-find name (harmless-output-style-list session)
           :key (lambda (style) (plist-get style :name))
           :test #'equal))

(defun harmless-output-style-active (session)
  "Return the style switched on for SESSION, or nil."
  (and (harmless-session-p session)
       (gethash (harmless-session-id session)
                harmless-output-style-active)))

(defun harmless-output-style--store (session style)
  "Remember STYLE for SESSION and save.  Nil clears it.
A failed save puts the previous style back."
  (let* ((id (harmless-session-id session))
         (old (gethash id harmless-output-style-active)))
    (if style
        (puthash id style harmless-output-style-active)
      (remhash id harmless-output-style-active))
    (condition-case err
        (harmless-session-save session)
      (error
       (if old
           (puthash id old harmless-output-style-active)
         (remhash id harmless-output-style-active))
       (signal (car err) (cdr err))))))

(defun harmless-output-style-set (session name)
  "Switch SESSION to style NAME and return that style.
Nil, \"\", and \"default\" clear it and return nil.  The text is
saved with the session.  Project instruction files are not edited.
A missing style signals \"no output style named NAME\" and leaves
the previous style in place."
  (unless (harmless-session-p session)
    (error "No Harmless session"))
  (cond
   ((harmless-output-style--clear-p name)
    (harmless-output-style--store session nil)
    nil)
   (t
    (setq name (harmless-output-style-name name))
    (let ((found (harmless-output-style-find session name)))
      (unless found
        (error "no output style named %s" name))
      (when (string-empty-p
             (string-trim (or (plist-get found :body) "")))
        (error "output style body is empty"))
      (let ((style (list :name (plist-get found :name)
                         :body (string-trim (plist-get found :body))
                         :description (plist-get found :description))))
        (harmless-output-style--store session style)
        style)))))

(defun harmless-output-style-block (session)
  "Return SESSION's output-style text, or nil."
  (let ((style (harmless-output-style-active session)))
    (when style
      (concat "Output style " (plist-get style :name) ".\n"
              "This is the role, tone, and format for this session.\n"
              "Project instructions, path-scoped rules, and plan mode "
              "win when they disagree.\n\n"
              (or (plist-get style :body) "")))))

(defun harmless-output-style-place (messages session)
  "Return MESSAGES with SESSION's style before later project text.
MESSAGES is not modified.  The caller puts the harness prompt first."
  (let ((block (harmless-output-style-block session)))
    (cond
     ((or (null block) (not (stringp block)) (string-empty-p block))
      messages)
     ((and (consp messages)
           (harmless-message-system-p (car messages)))
      (let ((msg (copy-sequence (car messages))))
        (plist-put msg :content
                   (concat block
                           "\n\n"
                           (or (and (stringp (plist-get msg :content))
                                    (plist-get msg :content))
                               "")))
        (cons msg (cdr messages))))
     ((listp messages)
      (cons (list :role :system :content block) messages))
     (t messages))))

(defun harmless-output-style-summary (session)
  "Return an :output-style entry, or nil when SESSION has none."
  (let ((style (harmless-output-style-active session)))
    (when (and style
               (harmless-output-style-name-p (plist-get style :name))
               (stringp (plist-get style :body))
               (not (string-empty-p (plist-get style :body))))
      (list :output-style
            (list :name (plist-get style :name)
                  :body (plist-get style :body))))))

(defun harmless-output-style-restore (session raw)
  "Remember RAW as SESSION's output style when it is usable.
A bad value is ignored.  Nothing is written."
  (when (and (harmless-session-p session) (harmless-plist-p raw))
    (let ((name (plist-get raw :name))
          (body (plist-get raw :body)))
      (when (and (harmless-output-style-name-p name)
                 (not (equal name "default"))
                 (stringp body)
                 (not (string-empty-p (string-trim body))))
        (puthash (harmless-session-id session)
                 (list :name name
                       :body (string-trim body))
                 harmless-output-style-active)))))

(defun harmless-output-style-copy (session child)
  "Copy SESSION's output style onto CHILD.
Nothing is written here.  The caller saves CHILD."
  (let ((style (harmless-output-style-active session)))
    (when (and style (harmless-session-p child))
      (puthash (harmless-session-id child)
               (copy-tree style)
               harmless-output-style-active))))

(provide 'harmless-output-style)

;;; harmless-output-style.el ends here
