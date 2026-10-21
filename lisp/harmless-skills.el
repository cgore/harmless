;;; harmless-skills.el --- Skill discovery for Harmless -*- lexical-binding: t; -*-

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
;; A skill is a directory containing SKILL.md.  Each turn lists the
;; name, description, and path.  The user or the model can load one
;; skill.  Later turns include its body.  allowed-tools limits those
;; turns.  Discovery walks the same directories as project
;; instructions and reads Harmless, Grok, Claude, and Codex layouts.
;; A directory closer to the session wins.  In one directory the
;; later vendor wins: agents, codex, claude, grok, then harmless.
;; A loadout is `.harmless/loadout' in one working directory.  It
;; names the skills that directory wants.  Reconciling reports
;; which are present.  Applying writes only
;; `.harmless/loadout.managed'.  Ancestors are not searched.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-instructions)
(require 'harmless-harness)
(require 'harmless-memory)
(require 'harmless-tools)
(require 'harmless-tools-fs)

(declare-function harmless-plugin-skill-directories "harmless-plugin" (ancestor))
(declare-function harmless-plugin-user-skill-directories "harmless-plugin" ())
(declare-function harmless-output-style-place "harmless-output-style"
                  (messages session))

(defconst harmless-skill-vendors
  '("agents" "codex" "claude" "grok" "harmless")
  "Dot-directory names that contain a skills folder, lowest priority first.")

(defconst harmless-skill-admin-root "/etc/codex/skills"
  "Machine-wide Codex skill directory, scanned before any user or project skill.")

(defun harmless-skill-roots (dir &optional stop)
  "Return skill directories that apply to DIR, lowest priority first.
STOP is passed to `harmless-instruction-directories'."
  (let (roots)
    (when (file-directory-p harmless-skill-admin-root)
      (push harmless-skill-admin-root roots))
    (when (and (null stop)
               (fboundp 'harmless-plugin-user-skill-directories))
      (dolist (dir (harmless-plugin-user-skill-directories))
        (push dir roots)))
    (dolist (ancestor (harmless-instruction-directories dir stop))
      (dolist (vendor harmless-skill-vendors)
        (push (expand-file-name
               "skills"
               (expand-file-name (concat "." vendor) ancestor))
              roots))
      (when (fboundp 'harmless-plugin-skill-directories)
        (dolist (dir (harmless-plugin-skill-directories ancestor))
          (push dir roots))))
    (nreverse roots)))

(defun harmless-skills--find (dir seen)
  "Return SKILL.md files under DIR.  SEEN records visited truename keys."
  (when (file-directory-p dir)
    (let ((key (file-truename dir)))
      (unless (gethash key seen)
        (puthash key t seen)
        (let (found)
          (dolist (name (directory-files dir))
            (unless (or (member name '("." ".."))
                        (eq (aref name 0) ?.)
                        (string= name "node_modules"))
              (let ((path (expand-file-name name dir)))
                (cond
                 ((file-directory-p path)
                  (setq found (append found (harmless-skills--find path seen))))
                 ((and (string= name "SKILL.md") (file-readable-p path))
                  (setq found (append found (list path))))))))
          found)))))

(defun harmless-skill--unquote (value)
  "Strip one pair of matching quotes from VALUE."
  (if (and (>= (length value) 2)
           (member (aref value 0) '(?\" ?'))
           (eq (aref value 0) (aref value (1- (length value)))))
      (substring value 1 -1)
    value))

(defun harmless-skill--normalize-name (name)
  "Return NAME as a lowercase hyphenated skill id."
  (let ((slug (replace-regexp-in-string
               "[^a-z0-9]+" "-"
               (downcase (string-trim (or name ""))))))
    (setq slug (replace-regexp-in-string "\\`-+" "" slug))
    (replace-regexp-in-string "-+\\'" "" slug)))

(defun harmless-skill--first-paragraph (text)
  "Return the first paragraph of TEXT as one line, or nil."
  (let ((lines nil)
        (done nil))
    (dolist (line (split-string (or text "") "\n"))
      (unless done
        (setq line (string-trim line))
        (cond
         ((string-empty-p line)
          (when lines (setq done t)))
         (t (push line lines)))))
    (when lines
      (string-join (nreverse lines) " "))))

(defun harmless-skill--frontmatter (text)
  "Return (NAME DESCRIPTION BODY TOOLS) from TEXT.
NAME and DESCRIPTION are nil when the field is absent.  BODY is the
markdown after the frontmatter, or all of TEXT when there is none.
TOOLS is the symbol `all' when `allowed-tools' is absent, or a list
of tool names when that field is present.  CRLF is read as a newline."
  (save-match-data
    (setq text (replace-regexp-in-string "\r\n?" "\n" (or text "")))
    (if (not (string-match
              "\\`---[ \t]*\n\\(\\(?:.*\n\\)*?\\)---[ \t]*\n?"
              text))
        (list nil nil text 'all)
      (let ((raw (match-string 1 text))
            (body (substring text (match-end 0)))
            (name nil)
            (description nil)
            (tools nil)
            (saw-tools nil)
            (in-tools nil))
        (dolist (line (split-string raw "\n"))
          (cond
           ((and in-tools
                 (string-match "\\`[ \t]*-[ \t]*\\(.*\\)\\'" line))
            (let ((item (harmless-skill--unquote
                         (string-trim (match-string 1 line)))))
              (when (and (stringp item) (not (string-empty-p item)))
                (setq tools
                      (append tools (split-string item "[, \t]+" t))))))
           ((string-match
             "\\`\\([A-Za-z0-9_-]+\\):[ \t]*\\(.*\\)\\'"
             line)
            (let ((key (match-string 1 line))
                  (value (match-string 2 line)))
              (setq value (harmless-skill--unquote (string-trim value)))
              (setq in-tools nil)
              (cond
               ((string= key "name") (setq name value))
               ((and (string= key "description")
                     (not (member value '("" ">" "|" ">-" "|-"))))
                (setq description value))
               ((string= key "allowed-tools")
                (setq saw-tools t)
                (cond
                 ((or (string-empty-p value) (string= value "[]"))
                  (when (string-empty-p value)
                    (setq in-tools t)))
                 ((string-match "\\`\\[\\(.*\\)\\]\\'" value)
                  (let ((inner (match-string 1 value)))
                    (setq tools
                          (append
                           tools
                           (mapcar
                            (lambda (part)
                              (harmless-skill--unquote (string-trim part)))
                            (split-string inner "," t))))))
                 (t
                  (setq tools
                        (append tools
                                (split-string value "[, \t]+" t)))))))))
           (t (setq in-tools nil))))
        (list name description body (if saw-tools tools 'all))))))

(defun harmless-skill--parse (path)
  "Return a skill plist for PATH, or nil when it has no usable summary.
`:restricted' is non-nil when `allowed-tools' is present.  `:tools'
is that list, possibly empty.  `:body' is the markdown."
  (let* ((raw (with-temp-buffer
                (let ((coding-system-for-read 'utf-8-unix))
                  (insert-file-contents path))
                (buffer-string)))
         (parsed (harmless-skill--frontmatter raw))
         (name (harmless-skill--normalize-name
                (or (nth 0 parsed)
                    (file-name-nondirectory
                     (directory-file-name (file-name-directory path))))))
         (description (or (nth 1 parsed)
                          (harmless-skill--first-paragraph (nth 2 parsed))))
         (tools (nth 3 parsed)))
    (when (and (not (string-empty-p name))
               description
               (not (string-empty-p (string-trim description))))
      (list :name name
            :description (harmless-truncate (string-trim description) 500)
            :path path
            :body (nth 2 parsed)
            :restricted (not (eq tools 'all))
            :tools (and (not (eq tools 'all)) tools)))))

(defun harmless-skills-discover (dir &optional stop)
  "Return skills that apply to DIR, sorted by name.
A closer directory overrides a farther one.  STOP bounds the walk."
  (let ((by-name (make-hash-table :test 'equal))
        (seen (make-hash-table :test 'equal))
        (skills nil))
    (dolist (root (harmless-skill-roots dir stop))
      (dolist (file (harmless-skills--find root seen))
        (when-let* ((skill (harmless-skill--parse file)))
          (puthash (plist-get skill :name) skill by-name))))
    (maphash (lambda (_name skill) (push skill skills)) by-name)
    (sort skills (lambda (a b)
                   (string< (plist-get a :name) (plist-get b :name))))))

(defun harmless-skills-catalog (dir &optional stop)
  "Return the skill list for DIR, or nil when there are no skills.
STOP is passed to `harmless-skills-discover'."
  (let ((skills (harmless-skills-discover dir stop)))
    (when skills
      (concat
       "Skills available for this session.  Read a skill's SKILL.md with read_file when it applies to the task.  Load one with the skill tool, or start the prompt with /name.\n\n"
       (mapconcat
        (lambda (skill)
          (format "- %s (%s): %s"
                  (plist-get skill :name)
                  (plist-get skill :path)
                  (plist-get skill :description)))
        skills
        "\n")))))

(defconst harmless-detached-chat-instructions
  "This is a detached chat.  It is not attached to a project or a working directory.  Do not assume a repository or files on disk.  Workspace memory is unavailable.  Global memory is available."
  "System text for a chat that has no project.")

(defun harmless-context--with-harness (messages)
  "Return MESSAGES with the standing harness prompt first, when loaded."
  (if (fboundp 'harmless-harness-prepend)
      (harmless-harness-prepend messages)
    messages))

(defun harmless-context--finish (messages session)
  "Return MESSAGES with SOUL.md, the harness, and SESSION's style.
SOUL.md, when it has text, is first.  A missing file leaves the
harness first.  The style is a role, tone, and format.  Project
text that follows it wins when they disagree."
  (when (fboundp 'harmless-output-style-place)
    (setq messages (harmless-output-style-place messages session)))
  (setq messages (harmless-context--with-harness messages))
  (if (fboundp 'harmless-soul-prepend)
      (harmless-soul-prepend messages)
    messages))

(defun harmless-context-messages (dir messages &optional stop model session)
  "Return MESSAGES with the harness prompt, instructions, and context.
STOP bounds the instruction and skill walks.  MODEL selects whether
`CLAUDE.md' is included.  Path rules, when loaded, come from
`.claude/rules/' in DIR.  SESSION's loaded skill, if any, is included.
SESSION's output style, if any, comes after the harness and before
project text.  SOUL.md is first when that file has text.  Otherwise
the harness prompt is first.  Later project text wins when it
disagrees.  The transcript is not modified.  A missing or relative
DIR is a detached chat: it does not walk `default-directory', so a
missing directory is not /."
  (if (not (and (stringp dir)
                (not (string-empty-p dir))
                (file-name-absolute-p dir)))
      (harmless-context--finish
       (harmless-context-append
        (harmless-context-append
         (harmless-context-append
          messages harmless-detached-chat-instructions)
         (harmless-memory-catalog nil))
        (harmless-skill-context-block session))
       session)
    (harmless-context--finish
     (harmless-context-append
      (harmless-context-append
       (harmless-context-append
        (harmless-context-append
         (harmless-instructions-apply dir messages stop model)
         (and (fboundp 'harmless-path-rules-text)
              (harmless-path-rules-text dir messages)))
        (harmless-skills-catalog dir stop))
       (harmless-skill-context-block session))
      (harmless-memory-catalog dir))
     session)))

(defvar harmless-skill-active (make-hash-table :test 'equal)
  "Loaded skill plists, keyed by session id.
The entry is not saved and is not copied to a forked session.")

(defun harmless-skill-active (session)
  "Return the skill loaded into SESSION, or nil."
  (and (harmless-session-p session)
       (gethash (harmless-session-id session) harmless-skill-active)))

(defun harmless-skill-name-p (name)
  "Return non-nil if NAME is a skill id, not a path."
  (and (stringp name)
       (string-match-p "\\`[a-z0-9]+\\(?:-[a-z0-9]+\\)*\\'" name)))

(defun harmless-skill--name (name)
  "Return NAME as a skill id, or signal.
Nil and \"\" are empty.  A path, \"~\", and the filesystem root are
not names and are not expanded."
  (cond
   ((null name) (error "skill name is empty"))
   ((not (stringp name)) (error "skill name must be a string"))
   (t
    (let ((raw (string-trim name)))
      (cond
       ((string-empty-p raw) (error "skill name is empty"))
       ((or (string-prefix-p "~" raw)
            (string-prefix-p "/" raw)
            (string-prefix-p "\\" raw)
            (string-match-p "[/\\\\]" raw)
            (string-match-p "\\`[A-Za-z]:[/\\]?\\'" raw)
            (file-name-absolute-p raw))
        (error "skill name is not a name"))
       (t
        (setq raw (harmless-skill--normalize-name raw))
        (if (harmless-skill-name-p raw)
            raw
          (error "skill name is not a name"))))))))

(defun harmless-skill--directory (session)
  "Return SESSION's project directory, or signal.
A detached chat, a relative path, \"~\", and the filesystem root
are refused.  This does not consult `default-directory'."
  (unless (harmless-session-p session)
    (error "No Harmless session"))
  (when (harmless-session-detached-p session)
    (error "This chat is not attached to a project"))
  (let ((dir (harmless-session-cwd session)))
    (cond
     ((or (not (stringp dir))
          (string-empty-p (string-trim dir)))
      (error "This chat is not attached to a project"))
     ((or (harmless-filesystem-root-p dir)
          (string-match-p "\\`[A-Za-z]:[/\\]?\\'" (string-trim dir))
          (harmless-extra-root-text-p dir))
      (error "skill directory is the filesystem root"))
     ((or (string-prefix-p "~" (string-trim dir))
          (not (file-name-absolute-p dir)))
      (error "skill directory must be absolute"))
     (t (harmless-absolute-directory dir)))))

(defun harmless-skill-find (dir name &optional stop)
  "Return the skill named NAME for DIR, or nil.
STOP is passed to `harmless-skills-discover'."
  (car (cl-remove-if-not
        (lambda (skill) (equal (plist-get skill :name) name))
        (harmless-skills-discover dir stop))))

(defun harmless-skill--check-tools (tools)
  "Signal when TOOLS names a tool Harmless does not have."
  (dolist (tool tools)
    (unless (and (stringp tool) (harmless-tool-by-name tool))
      (error "skill tool is unknown: %s" tool))))

(defun harmless-skill-load (session name)
  "Load the skill NAME into SESSION and return its plist.
A later turn sends the body.  When the skill lists allowed-tools,
those turns are offered only that list.  The transcript is not
changed.  Nothing is written."
  (setq name (harmless-skill--name name))
  (let* ((dir (harmless-skill--directory session))
         (skill (harmless-skill-find dir name)))
    (unless skill
      (error "no skill named %s" name))
    (when (plist-get skill :restricted)
      (harmless-skill--check-tools (plist-get skill :tools)))
    (setq skill (list :name (plist-get skill :name)
                      :body (string-trim (or (plist-get skill :body) ""))
                      :path (plist-get skill :path)
                      :restricted (plist-get skill :restricted)
                      :tools (plist-get skill :tools)))
    (puthash (harmless-session-id session) skill harmless-skill-active)
    skill))

(defun harmless-skill-prompt (session text)
  "Load a `/NAME' skill from TEXT, or return nil when TEXT is not one.
A matching name that is not a skill signals `no skill named NAME'.
TEXT that does not start with one skill id is left alone."
  (when (and (stringp text)
             (string-match
              "\\`/\\([A-Za-z0-9][A-Za-z0-9_-]*\\)\\(?:[ \t]+\\(.*\\)\\)?\\'"
              text))
    (let ((name (match-string 1 text)))
      (harmless-skill-load session name)
      name)))

(defun harmless-skill-context-block (session)
  "Return the loaded skill text for SESSION, or nil."
  (let ((skill (harmless-skill-active session)))
    (when skill
      (format "Loaded skill %s.\n\n%s"
              (plist-get skill :name)
              (or (plist-get skill :body) "")))))

(defun harmless-skill-filter-tools (session tools)
  "Return TOOLS allowed by SESSION's loaded skill.
A skill with no allowed-tools list does not narrow TOOLS."
  (let ((skill (harmless-skill-active session)))
    (if (or (null skill) (not (plist-get skill :restricted)))
        tools
      (cl-remove-if-not
       (lambda (tool)
         (member (harmless-tool-name tool) (plist-get skill :tools)))
       tools))))

(defun harmless-skill--tool (session args)
  "skill implementation.  Load the named skill and return its body."
  (let ((skill (harmless-skill-load session (harmless-tool-arg args :name))))
    (format "Loaded skill %s\n\n%s"
            (plist-get skill :name)
            (or (plist-get skill :body) ""))))

(defun harmless-skill-register ()
  "Register the skill tool."
  (harmless-register-tool
   (harmless-tool-create
    :name "skill"
    :description "Load a skill by name. The body is included on later turns. allowed-tools in the skill limits which tools those turns can call. The name is a skill id, not a path."
    :class 'read
    :schema '(:type "object"
              :properties (:name (:type "string"
                                  :description "Skill id."))
              :required ["name"])
    :fn #'harmless-skill--tool)))

(harmless-skill-register)

(defun harmless-skill--loadout-directory (directory)
  "Return DIRECTORY for a skill loadout, or signal.
Nil and \"\" are empty.  A non-string is refused.  A relative
path and \"~\" are not expanded.  \"/\" and a drive root are
refused.  A missing path and a file are refused."
  (cond
   ((null directory) (error "skill loadout directory is empty"))
   ((not (stringp directory))
    (error "skill loadout directory must be a string"))
   (t
    (setq directory (string-trim directory))
    (cond
     ((string-empty-p directory)
      (error "skill loadout directory is empty"))
     ((or (harmless-filesystem-root-p directory)
          (harmless-extra-root-text-p directory))
      (error "skill loadout directory is the filesystem root"))
     ((or (string-prefix-p "~" directory)
          (not (file-name-absolute-p directory)))
      (error "skill loadout directory must be absolute"))
     (t
      (setq directory (directory-file-name (expand-file-name directory)))
      (cond
       ((or (harmless-filesystem-root-p directory)
            (harmless-extra-root-text-p directory))
        (error "skill loadout directory is the filesystem root"))
       ((not (file-exists-p directory))
        (error "skill loadout directory is missing"))
       ((not (file-directory-p directory))
        (error "skill loadout directory is not a directory"))
       (t directory)))))))

(defun harmless-skill--loadout-flag (apply)
  "Return APPLY as nil or t, or signal."
  (cond
   ((null apply) nil)
   ((eq apply t) t)
   (t (error "skill loadout apply is not a flag"))))

(defun harmless-skill--loadout-file (directory name)
  "Return NAME under DIRECTORY's `.harmless' directory.
Signal when that path is the filesystem root or leaves DIRECTORY."
  (let ((file (expand-file-name
               name (expand-file-name ".harmless" directory))))
    (when (or (harmless-filesystem-root-p file)
              (harmless-extra-root-text-p file)
              (not (harmless-directory-strictly-under-p file directory)))
      (error "skill loadout directory is the filesystem root"))
    file))

(defun harmless-skill--loadout-parse (text)
  "Return skill ids in TEXT, in file order, or signal.
Blank lines are skipped.  A non-name, including a comment, and
a duplicate are not valid.  A file with no names is empty."
  (let ((names nil)
        (seen (make-hash-table :test 'equal)))
    (dolist (line (split-string (or text "") "\n"))
      (setq line (string-trim line))
      (unless (string-empty-p line)
        (unless (harmless-skill-name-p line)
          (error "skill loadout is not valid"))
        (when (gethash line seen)
          (error "skill loadout is not valid"))
        (puthash line t seen)
        (push line names)))
    (setq names (nreverse names))
    (unless names
      (error "skill loadout is empty"))
    names))

(defun harmless-skill--loadout-names (directory)
  "Return skill ids declared in DIRECTORY, in file order."
  (let ((file (harmless-skill--loadout-file directory "loadout")))
    (cond
     ((not (file-exists-p file))
      (error "skill loadout is missing"))
     ((not (file-regular-p file))
      (error "skill loadout is not valid"))
     (t
      (harmless-skill--loadout-parse
       (with-temp-buffer
         (let ((coding-system-for-read 'utf-8-unix))
           (insert-file-contents file))
         (buffer-string)))))))

(defun harmless-skill--loadout-roots (directory)
  "Return skill directories in DIRECTORY only, lowest priority first.
Ancestors, the admin skill directory, and user plugins are not
included.  A plugin in this directory wins over a vendor skill."
  (let ((roots nil))
    (dolist (vendor harmless-skill-vendors)
      (let ((root (expand-file-name
                   "skills"
                   (expand-file-name (concat "." vendor) directory))))
        (unless (or (harmless-filesystem-root-p root)
                    (harmless-extra-root-text-p root))
          (setq roots (append roots (list root))))))
    (when (fboundp 'harmless-plugin-skill-directories)
      (dolist (root (harmless-plugin-skill-directories directory))
        (unless (or (harmless-filesystem-root-p root)
                    (harmless-extra-root-text-p root))
          (setq roots (append roots (list root))))))
    roots))

(defun harmless-skill--loadout-found (directory)
  "Return a hash table of skill ids discovered in DIRECTORY."
  (let ((found (make-hash-table :test 'equal))
        (seen (make-hash-table :test 'equal)))
    (dolist (root (harmless-skill--loadout-roots directory))
      (dolist (file (harmless-skills--find root seen))
        (when-let* ((skill (harmless-skill--parse file)))
          (puthash (plist-get skill :name) t found))))
    found))

(defun harmless-skill--loadout-write (directory names)
  "Write NAMES to DIRECTORY's managed loadout and return that path.
An empty NAMES writes an empty file.  Nothing else is written."
  (let ((file (harmless-skill--loadout-file directory "loadout.managed")))
    (when (equal (file-truename file) (file-truename "/"))
      (error "skill loadout directory is the filesystem root"))
    (make-directory (file-name-directory file) t)
    (let ((coding-system-for-write 'utf-8-unix)
          (text (if names
                    (concat (mapconcat #'identity names "\n") "\n")
                  "")))
      (write-region text nil file nil 'silent))
    file))

(defun harmless-skill--loadout (directory apply)
  "Return the reconciliation of DIRECTORY.
APPLY nil writes nothing.  APPLY t writes the present names."
  (let* ((declared (harmless-skill--loadout-names directory))
         (found (harmless-skill--loadout-found directory))
         (present nil)
         (missing nil)
         (extra nil))
    (dolist (name declared)
      (if (gethash name found)
          (push name present)
        (push name missing)))
    (setq present (nreverse present))
    (setq missing (nreverse missing))
    (maphash (lambda (name _value)
               (unless (member name declared)
                 (push name extra)))
             found)
    (setq extra (sort extra #'string<))
    (when apply
      (harmless-skill--loadout-write directory present))
    (list :directory directory
          :declared declared
          :present present
          :missing missing
          :extra extra
          :applied apply)))

;;;###autoload
(defun harmless-skill-loadout (directory &optional apply)
  "Reconcile the skill loadout of one working directory.
DIRECTORY is absolute.  `.harmless/loadout' lists skill ids, one
per line.  The result says which are present, which are missing,
and which discovered skills were not declared.  Ancestors are
not searched.  APPLY nil writes nothing.  APPLY t writes
`.harmless/loadout.managed' with the present names and does not
copy a skill or install a plugin."
  (interactive
   (list (read-string "Directory: ")
         (and current-prefix-arg t)))
  (setq directory (harmless-skill--loadout-directory directory))
  (setq apply (harmless-skill--loadout-flag apply))
  (let ((result (harmless-skill--loadout directory apply)))
    (message "Loadout %s present, %s missing, %s extra"
             (length (plist-get result :present))
             (length (plist-get result :missing))
             (length (plist-get result :extra)))
    result))

(provide 'harmless-skills)

;;; harmless-skills.el ends here
