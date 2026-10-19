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

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-instructions)
(require 'harmless-memory)
(require 'harmless-tools)
(require 'harmless-tools-fs)

(declare-function harmless-plugin-skill-directories "harmless-plugin" (ancestor))
(declare-function harmless-plugin-user-skill-directories "harmless-plugin" ())

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

(defun harmless-context-messages (dir messages &optional stop model session)
  "Return MESSAGES with the harness prompt, instructions, and context.
STOP bounds the instruction and skill walks.  MODEL selects whether
`CLAUDE.md' is included.  Path rules, when loaded, come from
`.claude/rules/' in DIR.  SESSION's loaded skill, if any, is included.
The harness prompt is first.  Later project text wins when it
disagrees.  The transcript is not modified.  A missing or relative
DIR is a detached chat: it does not walk `default-directory', so a
missing directory is not /."
  (if (not (and (stringp dir)
                (not (string-empty-p dir))
                (file-name-absolute-p dir)))
      (harmless-context--with-harness
       (harmless-context-append
        (harmless-context-append
         (harmless-context-append
          messages harmless-detached-chat-instructions)
         (harmless-memory-catalog nil))
        (harmless-skill-context-block session)))
    (harmless-context--with-harness
     (harmless-context-append
      (harmless-context-append
       (harmless-context-append
        (harmless-context-append
         (harmless-instructions-apply dir messages stop model)
         (and (fboundp 'harmless-path-rules-text)
              (harmless-path-rules-text dir messages)))
        (harmless-skills-catalog dir stop))
       (harmless-skill-context-block session))
      (harmless-memory-catalog dir)))))

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

(provide 'harmless-skills)

;;; harmless-skills.el ends here
