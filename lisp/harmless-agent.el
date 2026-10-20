;;; harmless-agent.el --- Subagent sessions for Harmless -*- lexical-binding: t; -*-

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
;; `spawn_agent' starts a child session.  The child has its own
;; transcript and reports its last reply to the parent.  A definition
;; file can set the child's prompt, tools, model, and sandbox.  A
;; subagent cannot start another subagent.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-instructions)
(require 'harmless-skills)
(require 'harmless-tools)
(require 'harmless-turn)

(declare-function harmless-plugin-agent-files "harmless-plugin" (ancestor))
(declare-function harmless-plugin-user-agent-files "harmless-plugin" ())

(declare-function harmless-worktree-for-child "harmless-worktree"
                  (session args))
(declare-function harmless-worktree-requested-p "harmless-worktree" (args))

(defconst harmless-agent-read-only-refusal
  "Error: This subagent is read-only. A tool that can write is refused"
  "Result when a read-only subagent is asked to write.")

(defcustom harmless-agent-max-turns 8
  "Provider calls one subagent may make before it stops.
The count includes the first reply.  The next call returns the limit
instead of asking the model again."
  :type 'integer
  :group 'harmless)

(defvar harmless-agent-stop nil
  "Directory where agent discovery stops.
Nil stops at the user's home directory, as project instructions do.")

(defun harmless-agent-directory-p (dir)
  "Return non-nil if DIR can hold agent definitions.
Nil, \"\", a relative path, and the filesystem root cannot.  A
trailing slash is the same directory.  A drive root is not /."
  (and (stringp dir)
       (not (string-empty-p dir))
       (file-name-absolute-p dir)
       (not (harmless-filesystem-root-p dir))))

(defun harmless-agent-name-p (name)
  "Return non-nil if NAME is an agent name, not a path."
  (and (stringp name)
       (string-match-p "\\`[A-Za-z0-9][A-Za-z0-9_-]*\\'" name)))

(defun harmless-agent--unquote (value)
  "Strip one pair of matching quotes from VALUE."
  (if (and (stringp value)
           (>= (length value) 2)
           (member (aref value 0) '(?\" ?'))
           (eq (aref value 0) (aref value (1- (length value)))))
      (substring value 1 -1)
    value))

(defun harmless-agent--front (text)
  "Return (KEYS BODY) from TEXT.
KEYS is a plist of frontmatter fields.  BODY is the rest of TEXT."
  (save-match-data
    (let ((body text)
          (keys nil))
      (when (string-match
             "\\`---[ \t]*\n\\(\\(?:.*\n\\)*?\\)---[ \t]*\n?"
             text)
        (setq body (substring text (match-end 0)))
        (dolist (line (split-string (match-string 1 text) "\n" t))
          (when (string-match
                 "\\`\\([A-Za-z0-9_-]+\\):[ \t]*\\(.*\\)\\'"
                 line)
            (setq keys
                  (plist-put keys
                             (intern (concat ":" (match-string 1 line)))
                             (harmless-agent--unquote
                              (string-trim (match-string 2 line))))))))
      (list keys (string-trim (or body ""))))))

(defun harmless-agent--read (path)
  "Return (KEYS BODY) for PATH.  A missing file is an error."
  (unless (and (stringp path) (file-regular-p path) (file-readable-p path))
    (error "no agent file %s" path))
  (harmless-agent--front
   (replace-regexp-in-string
    "\r" ""
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix))
        (insert-file-contents path))
      (buffer-string)))))

(defun harmless-agent--file-name (path)
  "Return the agent name declared by PATH, or nil."
  (condition-case nil
      (let* ((parsed (harmless-agent--read path))
             (keys (car parsed))
             (named (plist-get keys :name))
             (name (if (and (stringp named) (not (string-empty-p named)))
                       named
                     (file-name-sans-extension
                      (file-name-nondirectory path)))))
        (and (harmless-agent-name-p name) name))
    (error nil)))

(defun harmless-agent--files (dir stop)
  "Return agent files for DIR, lowest priority first.
STOP is passed to `harmless-instruction-directories'."
  (let (files)
    (when (and (null stop)
               (fboundp 'harmless-plugin-user-agent-files))
      (dolist (path (harmless-plugin-user-agent-files))
        (push path files)))
    (dolist (ancestor (harmless-instruction-directories dir stop))
      (dolist (vendor harmless-skill-vendors)
        (let ((root (expand-file-name
                     "agents"
                     (expand-file-name (concat "." vendor) ancestor))))
          (when (and (file-directory-p root)
                     (not (harmless-filesystem-root-p root)))
            (dolist (name (directory-files root nil "\\.md\\'"))
              (unless (eq (aref name 0) ?.)
                (let ((path (expand-file-name name root)))
                  (when (and (file-regular-p path)
                             (file-readable-p path))
                    (push path files))))))))
      (when (fboundp 'harmless-plugin-agent-files)
        (dolist (path (harmless-plugin-agent-files ancestor))
          (push path files))))
    (nreverse files)))

(defun harmless-agent--identified (dir stop)
  "Return (NAME . PATH) pairs for DIR, lowest priority first."
  (let (pairs)
    (dolist (path (harmless-agent--files dir stop))
      (let ((name (harmless-agent--file-name path)))
        (when name
          (push (cons name path) pairs))))
    (nreverse pairs)))

(defun harmless-agent--tool-names (value)
  "Return tool names listed in VALUE."
  (let (names)
    (dolist (part (split-string
                   (replace-regexp-in-string "[][]" " " (or value ""))
                   "[, \t]+" t))
      (setq part (string-trim part))
      (unless (string-empty-p part)
        (push part names)))
    (delete-dups (nreverse names))))

(defun harmless-agent--parse (path)
  "Return a spec plist for PATH, or signal when it is invalid."
  (let* ((parsed (harmless-agent--read path))
         (keys (car parsed))
         (body (cadr parsed))
         (name (harmless-agent--file-name path))
         (sandbox (plist-get keys :sandbox))
         (model (plist-get keys :model))
         (tools (plist-get keys :tools))
         (saw-tools (plist-member keys :tools))
         (allow 'all))
    (unless name
      (error "agent name is not a name"))
    (setq sandbox (if (or (null sandbox) (string-empty-p sandbox))
                      "workspace"
                    sandbox))
    (unless (member sandbox '("workspace" "read-only"))
      (error "sandbox must be workspace or read-only"))
    (when (and (stringp model) (or (string-empty-p model)
                                   (string= model "inherit")))
      (setq model nil))
    (when (and model (not (stringp model)))
      (error "model must be a string"))
    (when saw-tools
      (setq allow (harmless-agent--tool-names tools))
      (if (null allow)
          (setq allow 'none)
        (dolist (tool allow)
          (unless (harmless-tool-by-name tool)
            (error "unknown tool %s" tool))
          (when (string= tool "spawn_agent")
            (error "a subagent cannot start another subagent"))
          (when (string= tool "run_workflow")
            (error "a subagent cannot start a workflow")))))
    (list :name name
          :description (let ((desc (plist-get keys :description)))
                         (and (stringp desc)
                              (not (string-empty-p desc))
                              desc))
          :prompt (and (not (string-empty-p body)) body)
          :allow allow
          :sandbox (if (string= sandbox "read-only") 'read-only 'workspace)
          :model model
          :path path)))

(defun harmless-agent-discover (dir &optional stop)
  "Return agent specs that apply to DIR, sorted by name.
Nil, \"\", a relative path, and the filesystem root return nil.
STOP bounds the walk and defaults to `harmless-agent-stop'."
  (if (not (harmless-agent-directory-p dir))
      nil
    (let ((table (make-hash-table :test 'equal)))
      (dolist (pair (harmless-agent--identified
                     dir (or stop harmless-agent-stop)))
        (condition-case nil
            (puthash (car pair) (harmless-agent--parse (cdr pair)) table)
          (error nil)))
      (let (specs)
        (maphash (lambda (_name spec) (push spec specs)) table)
        (sort specs (lambda (a b)
                      (string< (plist-get a :name)
                               (plist-get b :name))))))))

(defun harmless-agent-find (dir name &optional stop)
  "Return the spec named NAME for DIR, or nil.
A definition that fails to parse signals.  STOP defaults to
`harmless-agent-stop'."
  (when (harmless-agent-directory-p dir)
    (let (path)
      (dolist (pair (harmless-agent--identified
                     dir (or stop harmless-agent-stop)))
        (when (string= (car pair) name)
          (setq path (cdr pair))))
      (when path
        (harmless-agent--parse path)))))

(defun harmless-agent-catalog (dir &optional stop)
  "Return the agent list for DIR, or nil when there are none."
  (let ((specs (harmless-agent-discover dir stop)))
    (when specs
      (concat
       "Agents available for this session.  spawn_agent runs one by name.  The child has its own transcript and reports a summary back.\n\n"
       (mapconcat
        (lambda (spec)
          (format "- %s (%s): %s"
                  (plist-get spec :name)
                  (plist-get spec :path)
                  (or (plist-get spec :description) "")))
        specs
        "\n")))))

(defun harmless-agent-prompt-block (session)
  "Return SESSION's extra subagent prompt, or nil."
  (let ((agent (harmless-session-agent session)))
    (and agent (plist-get agent :prompt))))

(defun harmless-agent-context-suffix (session messages)
  "Return MESSAGES plus SESSION's agent prompt and agent catalog.
A subagent does not receive the catalog."
  (setq messages
        (harmless-context-append
         messages (harmless-agent-prompt-block session)))
  (if (eq (harmless-session-source session) 'agent)
      messages
    (harmless-context-append
     messages
     (harmless-agent-catalog (harmless-session-cwd session)
                             harmless-agent-stop))))

(defun harmless-agent--name (args)
  "Return the agent name in ARGS, or nil when it was omitted.
\"\" and whitespace signal \"agent name is empty\".  A path signals
\"agent name is not a name\"."
  (let ((name (harmless-tool-arg args :agent)))
    (cond
     ((null name) nil)
     ((not (stringp name))
      (error "agent name is not a name"))
     (t
      (setq name (string-trim name))
      (cond
       ((string-empty-p name) (error "agent name is empty"))
       ((harmless-agent-name-p name) name)
       (t (error "agent name is not a name")))))))

(defun harmless-agent--prompt (args)
  "Return the trimmed task in ARGS.
Nil, \"\", and whitespace signal \"prompt is empty\"."
  (let ((prompt (harmless-tool-arg args :prompt)))
    (unless (stringp prompt)
      (error "prompt is empty"))
    (setq prompt (string-trim prompt))
    (when (string-empty-p prompt)
      (error "prompt is empty"))
    prompt))

(defun harmless-agent--description (args)
  "Return the title in ARGS, or nil when it was omitted."
  (let ((desc (harmless-tool-arg args :description)))
    (cond
     ((null desc) nil)
     ((not (stringp desc))
      (error "description must be a string"))
     (t
      (when (string-match-p "[\n\r]" desc)
        (error "description must be one line"))
      (setq desc (string-trim desc))
      (and (not (string-empty-p desc)) desc)))))

(defun harmless-agent--expected-sandbox (session args)
  "Return the sandbox ARGS would use on SESSION, or nil."
  (condition-case nil
      (let ((name (harmless-agent--name args)))
        (if (null name)
            'workspace
          (let ((spec (harmless-agent-find (harmless-session-cwd session)
                                           name)))
            (and spec (plist-get spec :sandbox)))))
    (error nil)))

(defun harmless-agent-class (session args)
  "Return `read' for a read-only subagent, otherwise `edit'.
A requested worktree is `edit' even when the child is read-only.
A failure to resolve the sandbox is `edit'."
  (condition-case nil
      (if (or (and (fboundp 'harmless-worktree-requested-p)
                   (harmless-worktree-requested-p args))
              (not (eq 'read-only
                       (harmless-agent--expected-sandbox session args))))
          'edit
        'read)
    (error 'edit)))

(defun harmless-agent-plan-refuses-p (session tool args)
  "Return a refusal when plan mode blocks this spawn, or nil."
  (when (and (string= (harmless-tool-name tool) "spawn_agent")
             (harmless-session-plan-mode session))
    (cond
     ((and (fboundp 'harmless-worktree-requested-p)
           (harmless-worktree-requested-p args))
      "Error: Plan mode is on. A worktree is refused")
     ((eq 'workspace (harmless-agent--expected-sandbox session args))
      "Error: Plan mode is on. A subagent that can write is refused"))))

(defun harmless-agent--blank-shell-p (tool args)
  "Return non-nil when TOOL is run_shell with an empty command."
  (and (string= (harmless-tool-name tool) "run_shell")
       (let ((command (harmless-tool-arg args :command)))
         (or (null command)
             (and (stringp command)
                  (not (string-match-p "[^ \t\n\r]" command)))))))

(defun harmless-agent-refuse-call (session tool args)
  "Return a refusal for TOOL on a subagent SESSION, or nil."
  (when (eq (harmless-session-source session) 'agent)
    (let* ((agent (or (harmless-session-agent session)
                      (list :allow 'all :sandbox 'workspace)))
           (allow (plist-get agent :allow))
           (name (harmless-tool-name tool)))
      (cond
       ((or (string= name "spawn_agent")
            (string= name "run_workflow"))
        (if (string= name "spawn_agent")
            "Error: a subagent cannot start another subagent"
          "Error: a subagent cannot start a workflow"))
       ((eq allow 'none)
        (format "Error: this subagent cannot use %s" name))
       ((and (listp allow) (not (member name allow)))
        (format "Error: this subagent cannot use %s" name))
       ((and (eq (plist-get agent :sandbox) 'read-only)
             (not (eq (harmless-turn--class tool session args) 'read))
             (not (harmless-agent--blank-shell-p tool args))
             (not (member name '("send_message" "web_search" "web_fetch"
                                 "x_keyword_search" "x_thread_fetch"
                                 "x_user_search"
                                 "mcp_list_resources"
                                 "mcp_read_resource"))))
        harmless-agent-read-only-refusal)
       (t nil)))))

(defun harmless-agent-filter-tools (session tools)
  "Return TOOLS with a subagent's limits applied.
A read-only subagent keeps read tools, `run_shell', `monitor',
`monitor_stop', `send_message', `web_search', `web_fetch', X
search, and MCP resources.  Computer control is left out."
  (if (not (eq (harmless-session-source session) 'agent))
      tools
    (let* ((agent (harmless-session-agent session))
           (allow (if agent (plist-get agent :allow) 'all))
           (sandbox (if agent (plist-get agent :sandbox) 'workspace)))
      (setq tools
            (seq-remove
             (lambda (tool)
               (member (harmless-tool-name tool)
                       '("spawn_agent" "run_workflow")))
             tools))
      (setq tools
            (cond
             ((eq allow 'none) nil)
             ((listp allow)
              (seq-filter (lambda (tool)
                            (member (harmless-tool-name tool) allow))
                          tools))
             (t tools)))
      (if (eq sandbox 'read-only)
          (seq-filter
           (lambda (tool)
             (or (eq (harmless-tool-class tool) 'read)
                 (member (harmless-tool-name tool)
                         '("run_shell" "monitor" "monitor_stop"
                           "send_message"
                           "web_search" "web_fetch"
                           "x_keyword_search" "x_thread_fetch"
                           "x_user_search"
                           "mcp_list_resources"
                           "mcp_read_resource"))))
           tools)
        tools))))

(defun harmless-agent-children (session)
  "Return live subagents whose parent is SESSION."
  (let ((id (harmless-session-id session)))
    (seq-filter
     (lambda (child)
       (and (eq (harmless-session-source child) 'agent)
            (stringp (harmless-session-parent-id child))
            (string= (harmless-session-parent-id child) id)))
     (harmless-session-list))))

(defun harmless-agent-abort-children (session)
  "Abort subagents of SESSION that are still running."
  (dolist (child (harmless-agent-children session))
    (when (memq (harmless-session-status child)
                '(streaming waiting-permission))
      (harmless-turn-abort child))))

(defun harmless-agent-note-error (session text)
  "Remember TEXT as SESSION's subagent failure, once."
  (let ((agent (harmless-session-agent session)))
    (when (and agent
               (not (plist-get agent :error))
               (stringp text)
               (not (string-empty-p text)))
      (plist-put agent :error text))))

(defun harmless-agent-note-abort (session)
  "Remember that SESSION's subagent run was aborted."
  (let ((agent (harmless-session-agent session)))
    (when (and agent
               (plist-get agent :callback)
               (not (plist-get agent :error)))
      (plist-put agent :error "subagent aborted"))))

(defun harmless-agent--last-text (session)
  "Return the last assistant text in SESSION, or nil."
  (let (text)
    (dolist (msg (harmless-session-messages session))
      (when (memq (plist-get msg :role) '(:assistant assistant))
        (setq text (plist-get msg :content))))
    text))

(defun harmless-agent--result (session)
  "Return the summary or error string for SESSION's subagent run."
  (let* ((agent (harmless-session-agent session))
         (err (and agent (plist-get agent :error))))
    (if (and (stringp err) (not (string-empty-p err)))
        (if (string-prefix-p "Error:" err)
            err
          (concat "Error: " err))
      (let ((text (harmless-agent--last-text session)))
        (if (and (stringp text)
                 (not (string-empty-p (string-trim text))))
            text
          "Error: The model returned an empty reply.")))))

(defun harmless-agent-settle (session)
  "Report SESSION's subagent result once, when the run has finished."
  (let ((agent (harmless-session-agent session)))
    (when agent
      (let ((callback (plist-get agent :callback)))
        (when callback
          (plist-put agent :callback nil)
          (funcall callback (harmless-agent--result session)))))))

(defun harmless-agent-limited-p (session)
  "Return non-nil when SESSION's subagent has used its turn budget."
  (let ((agent (harmless-session-agent session)))
    (and agent
         (plist-get agent :callback)
         (integerp (plist-get agent :max))
         (>= (or (plist-get agent :turns) 0)
             (plist-get agent :max)))))

(defun harmless-agent-count-turn (session)
  "Count one provider call against SESSION's subagent budget."
  (let ((agent (harmless-session-agent session)))
    (when agent
      (plist-put agent :turns (1+ (or (plist-get agent :turns) 0))))))

(defun harmless-agent-stop-for-limit (session)
  "Finish SESSION because its subagent turn budget is spent."
  (let ((text (format "subagent stopped after %d turns"
                      (plist-get (harmless-session-agent session) :max))))
    (harmless-agent-note-error session text)
    (harmless-session-set-status session 'error)
    (harmless-emit session (list :error text))))

(defun harmless-agent--max-turns ()
  "Return `harmless-agent-max-turns', or signal when it is invalid."
  (let ((n harmless-agent-max-turns))
    (unless (and (integerp n) (> n 0))
      (error "harmless-agent-max-turns must be a positive integer"))
    n))

(defun harmless-agent--title (args spec prompt)
  "Return a short title for a child started with ARGS and SPEC."
  (harmless-truncate
   (or (harmless-agent--description args)
       (and spec (plist-get spec :description))
       prompt)
   60))

(defun harmless-agent--spec (session args)
  "Return the definition ARGS selects for SESSION.
Nil means a workspace child with the parent's model and every tool."
  (let ((name (harmless-agent--name args)))
    (if (null name)
        (list :name nil :description nil :prompt nil :allow 'all
              :sandbox 'workspace :model nil :path nil)
      (or (harmless-agent-find (harmless-session-cwd session) name)
          (error "no agent named %s" name)))))

(defun harmless-agent-create (session args callback)
  "Create a child of SESSION for ARGS that will call CALLBACK.
The child is not started.  Its directory is under the sessions root.
A worktree in ARGS is an isolated checkout.  The parent stays put."
  (unless (harmless-session-p session)
    (error "No Harmless session"))
  (when (eq (harmless-session-source session) 'agent)
    (error "a subagent cannot start another subagent"))
  (let* ((prompt (harmless-agent--prompt args))
         (spec (harmless-agent--spec session args))
         (max (harmless-agent--max-turns))
         (model-spec (or (plist-get spec :model)
                         (harmless-session-model session)))
         (parsed (harmless-parse-model-spec model-spec))
         (effort (or (cdr parsed)
                     (harmless-session-reasoning-effort session)))
         (worktree (and (fboundp 'harmless-worktree-for-child)
                        (harmless-worktree-for-child session args)))
         (child
          (apply
           #'harmless-session-new
           :provider (harmless-session-provider session)
           :model (car parsed)
           :reasoning-effort effort
           :parent-id (harmless-session-id session)
           :source 'agent
           :permission-mode (harmless-session-permission-mode session)
           :shell-sandbox
           (let ((mode (harmless-session-shell-sandbox session)))
             (if (harmless-shell-sandbox-p mode) mode 'off))
           :title (harmless-agent--title args spec prompt)
           (cond
            (worktree
             (list :cwd worktree))
            ((harmless-session-detached-p session)
             (list :detached t))
            (t
             (list :cwd (harmless-session-cwd session)))))))
    (setf (harmless-session-agent child)
          (list :prompt (plist-get spec :prompt)
                :allow (plist-get spec :allow)
                :sandbox (plist-get spec :sandbox)
                :callback callback
                :turns 0
                :max max
                :error nil
                :task prompt))
    (setf (harmless-session-extra-dirs child)
          (copy-sequence (harmless-session-extra-dirs session)))
    (harmless-session-save child)
    child))

(defun harmless-agent--tool (session args callback)
  "spawn_agent implementation.  CALLBACK receives the summary."
  (let (child err)
    (condition-case caught
        (setq child (harmless-agent-create session args callback))
      (error (setq err (error-message-string caught))))
    (if err
        (funcall callback (format "Error: %s" err))
      (harmless-turn-run child (plist-get (harmless-session-agent child)
                                          :task)))))

(defun harmless-agent-register ()
  "Register the spawn_agent tool."
  (harmless-register-tool
   (harmless-tool-create
    :name "spawn_agent"
    :description
    (concat
     "Start a child session and wait for its summary. "
     "PROMPT is the task. AGENT is an optional definition name. "
     "DESCRIPTION is an optional short title. "
     "WORKTREE is an optional isolated checkout. "
     "The child has its own transcript. "
     "It cannot start another subagent.")
    :class 'edit
    :async t
    :classify #'harmless-agent-class
    :schema '(:type "object"
              :properties (:prompt (:type "string"
                                   :description "The task for the child")
                           :agent (:type "string"
                                  :description "Definition name, not a path")
                           :description (:type "string"
                                        :description "Short title")
                           :worktree (:type "string"
                                     :description "Isolated checkout name"))
              :required ["prompt"])
    :fn #'harmless-agent--tool)))

(harmless-agent-register)

(provide 'harmless-agent)

;;; harmless-agent.el ends here
