;;; harmless-session.el --- Session objects and persistence for Harmless -*- lexical-binding: t; -*-

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
;; A session is the unit of concurrency.  The registry is global.  Disk is
;; the source of truth; buffers are views.  `parent-id' links a fork or a
;; subagent to the session that started it.  `agent' is the in-memory
;; subagent run; the saved fields are its sandbox, tool list, and prompt.
;; `findings' is a queue of findings from other live sessions.  It is not
;; saved.  A flushed finding is an ordinary transcript line.  `schedules'
;; is a list of one-shot prompts still waiting.  Each item is :id,
;; :prompt, and :at.  A timer is not stored on the item.  `rules' is
;; the allow and deny list for this session.  It is not saved.
;; Configured rules and `.harmless/permissions' are copied when the
;; session is created and when it is loaded.  `todos' is the work
;; list the model updates.  Each item is :id, :content, :status,
;; and :depends.  It is saved.  It is not a queue row.  `review'
;; is the latest read-only review: :kind, :base, :commit, and
;; :findings.  It is saved.  Findings are not applied as edits.
;; `shell-sandbox' is `off', `read-only', or `workspace-write'.
;; `off' is the default and is not saved.  A read-only subagent
;; uses `read-only' when the session sandbox is not `off'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'project)
(require 'harmless-util)
(require 'harmless-log)
(require 'harmless-provider)
(require 'harmless-tools)

(declare-function harmless-tools-shell-kill "harmless-tools-shell" (proc))
(declare-function harmless-agent-settle "harmless-agent" (session))
(declare-function harmless-message-flush "harmless-message" (session))
(declare-function harmless-schedule-restore "harmless-schedule"
                  (session summary))
(declare-function harmless-perm-install-configured-rules "harmless-perm"
                  (session))
(declare-function harmless-output-style-summary "harmless-output-style"
                  (session))
(declare-function harmless-output-style-restore "harmless-output-style"
                  (session raw))

(defcustom harmless-directory (locate-user-emacs-file "harmless/")
  "Root directory for Harmless config and sessions.
Defaults to `harmless/' under `user-emacs-directory' (for example
`~/.emacs.d/harmless/').  Nil and the empty string use that same
location.  A relative path or the filesystem root is refused."
  :type 'directory
  :group 'harmless)

(defconst harmless-permission-modes
  '(ask accept-edits always-approve auto)
  "Permission modes a session can store.
`auto' asks a reviewer before a call that would otherwise prompt.")

(defun harmless-session-permission-mode-p (mode)
  "Return non-nil when MODE is a permission mode."
  (and (symbolp mode) (memq mode harmless-permission-modes)))

(defun harmless-session--permission-mode-from-summary (value)
  "Return a permission mode loaded from VALUE.
Nil, \"\", and an unknown name are `ask'.  `auto' is kept."
  (let ((mode (cond
               ((or (null value)
                    (and (stringp value) (string-empty-p value)))
                'ask)
               ((symbolp value) value)
               ((stringp value) (intern value))
               (t 'ask))))
    (if (harmless-session-permission-mode-p mode) mode 'ask)))

(defcustom harmless-permission-mode 'ask
  "Default permission mode for new sessions.
`ask', `accept-edits', `always-approve', or `auto'."
  :type '(choice (const ask)
                 (const accept-edits)
                 (const always-approve)
                 (const auto))
  :group 'harmless)

(defcustom harmless-shell-sandbox 'off
  "Default OS sandbox for the shell of a new session.
`off' does not restrict the shell.  `read-only' and
`workspace-write' use Landlock on Linux.  `off' is the default."
  :type '(choice (const off)
                 (const read-only)
                 (const workspace-write))
  :group 'harmless)

(defun harmless-shell-sandbox-p (mode)
  "Return non-nil when MODE is a shell sandbox mode."
  (memq mode '(off read-only workspace-write)))

(defun harmless-shell-sandbox-from-summary (value)
  "Return a shell sandbox loaded from VALUE.
Nil, \"\", and an unknown name are `off'."
  (let ((mode (cond
               ((or (null value)
                    (and (stringp value) (string-empty-p value)))
                'off)
               ((keywordp value)
                (intern (substring (symbol-name value) 1)))
               ((symbolp value) value)
               ((stringp value) (intern value))
               (t 'off))))
    (if (harmless-shell-sandbox-p mode) mode 'off)))

(defvar harmless--sessions (make-hash-table :test 'equal)
  "Live sessions keyed by id.")

(defvar harmless-event-functions nil
  "Functions called as (SESSION EVENT) for UI and other observers.")

(cl-defstruct (harmless-session
               (:constructor harmless-session--create)
               (:copier nil))
  id
  title
  cwd
  provider
  provider-name
  model
  reasoning-effort
  messages
  status
  parent-id
  source
  permission-mode
  allow-classes
  prompt-tokens
  completion-tokens
  last-prompt-tokens
  last-completion-tokens
  created-at
  updated-at
  directory
  process
  buffer
  prompt-buffer
  title-locked
  plan-mode
  processes
  tool-batch
  shell-jobs
  shell-seq
  shell-cwd
  steers
  agent
  worktree
  findings
  schedules
  rules
  extra-dirs
  todos
  review
  shell-sandbox)

(defun harmless-shell-sandbox-effective (session)
  "Return the Landlock mode SESSION should apply.
Nil and an unknown slot are `off'.  A read-only subagent is
`read-only' when the session sandbox is not `off'."
  (let ((mode (and (harmless-session-p session)
                   (harmless-session-shell-sandbox session))))
    (unless (harmless-shell-sandbox-p mode)
      (setq mode 'off))
    (if (or (eq mode 'off)
            (not (eq (harmless-session-source session) 'agent))
            (not (eq (plist-get (harmless-session-agent session) :sandbox)
                     'read-only)))
        mode
      'read-only)))

(defun harmless-emit (session event)
  "Notify observers that EVENT happened on SESSION."
  (dolist (fn harmless-event-functions)
    (funcall fn session event)))

(defun harmless-session-track-process (session proc)
  "Remember PROC as an in-flight process for SESSION."
  (when (processp proc)
    (setf (harmless-session-processes session)
          (cons proc (delq proc (harmless-session-processes session))))
    (setf (harmless-session-process session) proc))
  proc)

(defun harmless-session-untrack-process (session proc)
  "Forget PROC as an in-flight process for SESSION."
  (setf (harmless-session-processes session)
        (delq proc (harmless-session-processes session)))
  (when (eq (harmless-session-process session) proc)
    (setf (harmless-session-process session)
          (car (harmless-session-processes session))))
  nil)

(defun harmless-session-kill-processes (session)
  "Stop SESSION's in-flight HTTP request, foreground shells, and interpreter."
  (let ((procs (copy-sequence (harmless-session-processes session))))
    (when (and (processp (harmless-session-process session))
               (not (memq (harmless-session-process session) procs)))
      (push (harmless-session-process session) procs))
    (dolist (proc procs)
      (when (process-live-p proc)
        (if (string-prefix-p "harmless-shell" (or (process-name proc) ""))
            (progn
              (require 'harmless-tools-shell)
              (harmless-tools-shell-kill proc))
          (harmless-provider-abort (harmless-session-provider session) proc))))
    (setf (harmless-session-processes session) nil
          (harmless-session-process session) nil))
  (when (fboundp 'harmless-execute-stop)
    (harmless-execute-stop session)))

(defun harmless-session-effective-reasoning-effort (session)
  "Return SESSION's reasoning effort, falling back to the default."
  (or (harmless-session-reasoning-effort session)
      harmless-default-reasoning-effort))

(defun harmless-session-model-label (session)
  "Return SESSION's model with reasoning effort when set."
  (harmless-model-label (harmless-session-model session)
                        (harmless-session-effective-reasoning-effort session)))

(defconst harmless-session-detached-component "_detached"
  "Directory component for chats that have no project.
An encoded project directory always contains a percent sign, so this
name cannot be one.")

(defun harmless-session-detached-p (session)
  "Return non-nil if SESSION is a chat with no project directory.
Only a nil cwd counts.  An empty string is not a detached chat."
  (null (harmless-session-cwd session)))

(defun harmless-place-name (cwd)
  "Return a short label for a session CWD.
Nil is a detached chat.  An empty string stays unlabeled."
  (cond
   ((null cwd) "chat")
   ((not (and (stringp cwd) (not (string-empty-p cwd)))) "?")
   (t (let ((name (file-name-nondirectory (directory-file-name cwd))))
        (if (string-empty-p name) cwd name)))))

(defun harmless-session-project-name (session)
  "Return a short project label for SESSION."
  (harmless-place-name (harmless-session-cwd session)))

(defun harmless-session-require-project (session)
  "Return SESSION's absolute project directory.
A detached chat has none.  A missing directory is not replaced with /."
  (if (harmless-session-detached-p session)
      (error "This chat is not attached to a project")
    (harmless-absolute-directory (harmless-session-cwd session))))

(defun harmless-data-directory ()
  "Return the absolute Harmless data directory.
See `harmless-resolve-data-directory'."
  (harmless-resolve-data-directory harmless-directory))

(defun harmless-sessions-root ()
  "Return the absolute directory that holds saved sessions."
  (file-name-as-directory
   (expand-file-name "sessions" (harmless-data-directory))))

(defun harmless-session-encode-cwd (cwd)
  "Return one relative filename component encoding absolute CWD.
A relative CWD is not expanded against `default-directory'.  Emacs
uses / when that variable is missing."
  (let ((enc (harmless-url-encode
              (directory-file-name (harmless-absolute-directory cwd)))))
    (unless (harmless-safe-path-component-p enc)
      (error "Encoded session cwd is not a safe filename: %S" enc))
    enc))

(defun harmless-session-dir-acceptable-p (dir)
  "Return non-nil if DIR is a session directory under the sessions root.
Empty and root directories are rejected.  An empty string is truthy in
Emacs Lisp and must not be kept as a stored directory."
  (and (stringp dir)
       (not (string-empty-p dir))
       (file-name-absolute-p dir)
       (not (harmless-filesystem-root-p dir))
       (harmless-directory-strictly-under-p dir (harmless-sessions-root))))

(defun harmless-session-dir (session)
  "Return the on-disk directory for SESSION, creating it if needed.
Never returns the filesystem root."
  (let ((stored (harmless-session-directory session))
        (root (harmless-sessions-root)))
    (if (harmless-session-dir-acceptable-p stored)
        (progn
          (harmless-ensure-directory stored)
          stored)
      (let* ((id (harmless-session-id session))
             (dir (progn
                    (unless (harmless-safe-path-component-p id)
                      (error "Harmless session id is not a safe filename: %S" id))
                    (harmless-collapse-path
                     (expand-file-name
                      id
                      (expand-file-name
                       (if (harmless-session-detached-p session)
                           harmless-session-detached-component
                         (harmless-session-encode-cwd
                          (harmless-session-cwd session)))
                       root))))))
        (unless (harmless-directory-strictly-under-p dir root)
          (error "Refusing to store a Harmless session at %s" dir))
        (harmless-ensure-directory dir)
        (setf (harmless-session-directory session) dir)
        dir))))

(defun harmless-session-register (session)
  "Add SESSION to the live registry."
  (puthash (harmless-session-id session) session harmless--sessions)
  session)

(defun harmless-session-unregister (session)
  "Remove SESSION from the live registry."
  (remhash (harmless-session-id session) harmless--sessions))

(defun harmless-session-get (id)
  "Return the live session with ID, or nil."
  (gethash id harmless--sessions))

(defun harmless-session-list ()
  "Return live sessions, newest updated first."
  (let (acc)
    (maphash (lambda (_id s) (push s acc)) harmless--sessions)
    (seq-sort (lambda (a b)
                (string> (or (harmless-session-updated-at a) "")
                         (or (harmless-session-updated-at b) "")))
              acc)))

(defun harmless-session-for-cwd (cwd)
  "Return live sessions whose cwd is CWD.
A missing or relative CWD matches nothing.  It is not expanded to /.
A trailing slash does not keep two names for one directory apart."
  (seq-filter (lambda (s)
                (harmless-same-directory-p (harmless-session-cwd s) cwd))
              (harmless-session-list)))

(defun harmless-current-cwd ()
  "Return the project root of the current buffer, or `default-directory'.
A missing or relative directory is an error.  It is not replaced with /."
  (let ((dir (if-let* ((proj (project-current)))
                 (project-root proj)
               default-directory)))
    (harmless-absolute-directory dir)))

(defun harmless-session-new (&rest args)
  "Create, register, and persist a new session.
Keyword ARGS: :cwd :detached :provider :model :reasoning-effort
:parent-id :source :permission-mode :shell-sandbox :title.
:shell-sandbox is off, read-only, or workspace-write.  Nil is
refused.  Omitted uses `harmless-shell-sandbox'.
:detached starts a chat with no project directory.  It cannot be
combined with :cwd."
  (let* ((provider (or (plist-get args :provider) (harmless-default-provider)))
         (detached (plist-get args :detached))
         (cwd (cond
               (detached
                (when (plist-member args :cwd)
                  (error "A detached chat has no project directory"))
                nil)
               (t (harmless-absolute-directory
                   (or (plist-get args :cwd) (harmless-current-cwd))))))
         (now (harmless-now-iso))
         (parsed (harmless-parse-model-spec
                  (or (plist-get args :model)
                      (and provider
                           (harmless-provider-default-model provider)))))
         (session (harmless-session--create
                   :id (or (plist-get args :id) (harmless-uuid))
                   :title (plist-get args :title)
                   :cwd cwd
                   :provider provider
                   :provider-name (and provider (harmless-provider-name provider))
                   :model (car parsed)
                   :reasoning-effort (or (plist-get args :reasoning-effort)
                                         (cdr parsed)
                                         harmless-default-reasoning-effort)
                   :messages nil
                   :status 'idle
                   :parent-id (plist-get args :parent-id)
                   :source (or (plist-get args :source) 'user)
                   :permission-mode (let ((mode
                                          (or (plist-get args :permission-mode)
                                              harmless-permission-mode)))
                                      (unless (harmless-session-permission-mode-p
                                               mode)
                                        (error "permission mode is not a mode"))
                                      mode)
                   :shell-sandbox (let ((mode
                                         (if (plist-member args :shell-sandbox)
                                             (plist-get args :shell-sandbox)
                                           harmless-shell-sandbox)))
                                     (unless (harmless-shell-sandbox-p mode)
                                       (error "shell sandbox must be off, read-only, or workspace-write"))
                                     mode)
                   :allow-classes nil
                   :prompt-tokens 0
                   :completion-tokens 0
                   :last-prompt-tokens 0
                   :last-completion-tokens 0
                   :created-at now
                   :updated-at now
                   :title-locked (and (plist-get args :title) t))))
    (unless provider
      (error "No Harmless provider configured"))
    (when (fboundp 'harmless-perm-install-configured-rules)
      (harmless-perm-install-configured-rules session))
    (harmless-session-register session)
    (harmless-session-save session)
    session))

(defun harmless-session-set-status (session status)
  "Set SESSION status to STATUS and notify observers.
When STATUS is idle or error, queued findings are written into the
transcript and a subagent run settles and reports."
  (setf (harmless-session-status session) status
        (harmless-session-updated-at session) (harmless-now-iso))
  (harmless-emit session (list :status status))
  (when (memq status '(idle error))
    (when (fboundp 'harmless-message-flush)
      (harmless-message-flush session))
    (when (fboundp 'harmless-agent-settle)
      (harmless-agent-settle session))))

(defun harmless-session-append (session message)
  "Append canonical MESSAGE to SESSION and persist."
  (setf (harmless-session-messages session)
        (append (harmless-session-messages session) (list message))
        (harmless-session-updated-at session) (harmless-now-iso))
  (unless (harmless-session-title-locked session)
    (when (and (memq (plist-get message :role) '(:user user))
               (not (harmless-session-title session)))
      (setf (harmless-session-title session)
            (harmless-truncate
             (replace-regexp-in-string "\n" " " (or (plist-get message :content) ""))
             60))))
  (harmless-session-save session)
  (harmless-emit session (list :message message))
  message)

(defun harmless-message-user-p (msg)
  "Return non-nil if MSG starts a user turn.
A summary is not a user turn.  The roles :user, user, and \"user\" are."
  (or (memq (plist-get msg :role) '(:user user))
      (equal (plist-get msg :role) "user")))

(defun harmless-session-append-user (session text)
  "Append a user TEXT message to SESSION."
  (harmless-session-append session (list :role :user :content text)))

(defun harmless-session--agent-summary (session)
  "Return summary fields for SESSION's subagent limits, or nil.
The callback and the turn counter stay in memory."
  (let ((agent (harmless-session-agent session)))
    (when agent
      (list :agent-sandbox (symbol-name
                            (or (plist-get agent :sandbox) 'workspace))
            :agent-allow (cond
                          ((eq (plist-get agent :allow) 'none) "none")
                          ((listp (plist-get agent :allow))
                           (plist-get agent :allow))
                          (t "all"))
            :agent-prompt (plist-get agent :prompt)))))

(defun harmless-session--agent-from-summary (summary)
  "Return the in-memory agent plist stored in SUMMARY, or nil."
  (let ((sandbox (plist-get summary :agent-sandbox))
        (allow (plist-get summary :agent-allow))
        (prompt (plist-get summary :agent-prompt))
        (source (or (plist-get summary :source) "")))
    (when (or sandbox allow prompt (equal source "agent"))
      (list :prompt (and (stringp prompt)
                         (not (string-empty-p prompt))
                         prompt)
            :allow (cond
                    ((equal allow "none") 'none)
                    ((consp allow) allow)
                    (t 'all))
            :sandbox (if (equal sandbox "read-only") 'read-only 'workspace)
            :callback nil
            :turns 0
            :max nil
            :error nil
            :task nil))))

(defun harmless-session--worktree-summary (session)
  "Return summary fields for SESSION's worktree, or nil."
  (let ((wt (harmless-session-worktree session)))
    (when (and wt (plist-get wt :name) (plist-get wt :path))
      (list :worktree-name (plist-get wt :name)
            :worktree-path (plist-get wt :path)
            :worktree-branch (plist-get wt :branch)
            :worktree-original (plist-get wt :original)))))

(defun harmless-session--worktree-from-summary (summary)
  "Return the worktree plist stored in SUMMARY, or nil."
  (let ((name (plist-get summary :worktree-name))
        (path (plist-get summary :worktree-path))
        (branch (plist-get summary :worktree-branch))
        (original (plist-get summary :worktree-original)))
    (when (and (stringp name) (not (string-empty-p name))
               (stringp path) (not (string-empty-p path)))
      (list :name name
            :path path
            :branch (and (stringp branch)
                         (not (string-empty-p branch))
                         branch)
            :original (and (stringp original)
                           (not (string-empty-p original))
                           original)))))

(defun harmless-session--schedules-from-summary (session summary)
  "Copy schedules from SUMMARY onto SESSION without arming them.
Used when the schedule feature is not loaded, so a saved prompt is
not dropped.  Timers are not stored."
  (let ((raw (plist-get summary :schedules))
        items)
    (when (listp raw)
      (dolist (item raw)
        (when (harmless-plist-p item)
          (push (list :id (plist-get item :id)
                      :prompt (plist-get item :prompt)
                      :at (plist-get item :at))
                items))))
    (setf (harmless-session-schedules session) (nreverse items))))

(defun harmless-session--schedules-summary (session)
  "Return a :schedules entry, or nil when SESSION has none.
Timers are not included."
  (let ((items (harmless-session-schedules session)))
    (when items
      (list :schedules
            (mapcar (lambda (item)
                      (list :id (plist-get item :id)
                            :prompt (plist-get item :prompt)
                            :at (plist-get item :at)))
                    items)))))

(defun harmless-session-summary-plist (session)
  "Return a plist suitable for summary.json."
  (append
   (list :id (harmless-session-id session)
         :title (harmless-session-title session)
         :cwd (harmless-session-cwd session)
         :provider (harmless-session-provider-name session)
         :model (harmless-session-model session)
         :reasoning-effort (harmless-session-reasoning-effort session)
         :status (format "%s" (harmless-session-status session))
         :parent-id (harmless-session-parent-id session)
         :source (format "%s" (harmless-session-source session))
         :permission-mode (format "%s" (harmless-session-permission-mode session))
         :prompt-tokens (harmless-session-prompt-tokens session)
         :completion-tokens (harmless-session-completion-tokens session)
         :last-prompt-tokens (or (harmless-session-last-prompt-tokens session) 0)
         :last-completion-tokens
         (or (harmless-session-last-completion-tokens session) 0)
         :created-at (harmless-session-created-at session)
         :updated-at (harmless-session-updated-at session)
         :title-locked (and (harmless-session-title-locked session) t)
         :plan-mode (and (harmless-session-plan-mode session) t)
         :shell-cwd (harmless-session-shell-cwd session))
   (harmless-session--shell-sandbox-summary session)
   (harmless-session--agent-summary session)
   (harmless-session--worktree-summary session)
   (harmless-session--schedules-summary session)
   (when (fboundp 'harmless-extra-summary)
     (harmless-extra-summary session))
   (when (fboundp 'harmless-todo-summary)
     (harmless-todo-summary session))
   (when (fboundp 'harmless-review-summary)
     (harmless-review-summary session))
   (when (fboundp 'harmless-output-style-summary)
     (harmless-output-style-summary session))))

(defun harmless-session--shell-sandbox-summary (session)
  "Return a :shell-sandbox entry, or nil when SESSION is off.
Nil and an unknown mode are omitted, the same as `off'."
  (let ((mode (harmless-session-shell-sandbox session)))
    (when (and (harmless-shell-sandbox-p mode)
               (not (eq mode 'off)))
      (list :shell-sandbox (symbol-name mode)))))

(defun harmless-session--file (session name)
  "Return the path of NAME inside SESSION's directory.
Signal when that path would be /NAME or outside the sessions root."
  (let* ((dir (harmless-session-dir session))
         (file (expand-file-name name dir)))
    (unless (and (harmless-directory-strictly-under-p dir (harmless-sessions-root))
                 (not (harmless-filesystem-root-p (file-name-directory file))))
      (error "Refusing to write %s" file))
    file))

(defun harmless-session-save (session)
  "Write SESSION to disk."
  (let ((coding-system-for-write 'utf-8-unix)
        (buffer-file-coding-system 'utf-8-unix))
    (with-temp-file (harmless-session--file session "summary.json")
      (setq buffer-file-coding-system 'utf-8-unix)
      (insert (harmless-json-text (harmless-session-summary-plist session))
              "\n"))
    (with-temp-file (harmless-session--file session "messages.jsonl")
      (setq buffer-file-coding-system 'utf-8-unix)
      (dolist (msg (harmless-session-messages session))
        (insert (harmless-json-text msg) "\n")))))

(defun harmless-session--provider-by-name (name)
  "Find a registered provider named NAME."
  (cl-find name harmless-providers
           :key #'harmless-provider-name :test #'string=))

(defun harmless-session--parse-status (s)
  "Convert summary status string S to a symbol."
  (let ((sym (intern (or s "idle"))))
    (if (memq sym '(idle streaming waiting-permission error))
        (if (eq sym 'streaming) 'idle sym)
      'idle)))

(defun harmless-session--keywordize (value)
  "Turn a JSON string or symbol VALUE into a keyword."
  (cond
   ((keywordp value) value)
   ((symbolp value) (intern (concat ":" (symbol-name value))))
   ((stringp value) (intern (concat ":" value)))
   (t value)))

(defun harmless-session--normalize-message (msg)
  "Keywordize :role on MSG after a JSON round-trip."
  (let ((role (plist-get msg :role)))
    (if role
        (plist-put (copy-sequence msg) :role (harmless-session--keywordize role))
      msg)))

(defun harmless-session-load (dir)
  "Load a session from DIR and register it.  Return the session."
  (unless (harmless-session-dir-acceptable-p dir)
    (error "Refusing to load a Harmless session from %S" dir))
  (let* ((summary (harmless-json-decode
                   (with-temp-buffer
                     (insert-file-contents (expand-file-name "summary.json" dir))
                     (buffer-string))))
         (messages nil)
         (msg-file (expand-file-name "messages.jsonl" dir)))
    (when (file-exists-p msg-file)
      (with-temp-buffer
        (insert-file-contents msg-file)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (string-trim (buffer-substring (line-beginning-position)
                                                     (line-end-position)))))
            (unless (string-empty-p line)
              (push (harmless-session--normalize-message
                     (harmless-json-decode line))
                    messages)))
          (forward-line 1))))
    (let* ((provider-name (plist-get summary :provider))
           (provider (or (harmless-session--provider-by-name provider-name)
                         (harmless-default-provider)))
           (session (harmless-session--create
                     :id (plist-get summary :id)
                     :title (plist-get summary :title)
                     :cwd (plist-get summary :cwd)
                     :provider provider
                     :provider-name provider-name
                     :model (plist-get summary :model)
                     :reasoning-effort (plist-get summary :reasoning-effort)
                     :messages (nreverse messages)
                     :status (harmless-session--parse-status
                              (plist-get summary :status))
                     :parent-id (plist-get summary :parent-id)
                     :source (intern (or (plist-get summary :source) "user"))
                     :permission-mode
                     (harmless-session--permission-mode-from-summary
                      (plist-get summary :permission-mode))
                     :allow-classes nil
                     :prompt-tokens (or (plist-get summary :prompt-tokens) 0)
                     :completion-tokens (or (plist-get summary :completion-tokens) 0)
                     :last-prompt-tokens (or (plist-get summary :last-prompt-tokens) 0)
                     :last-completion-tokens (or (plist-get summary :last-completion-tokens) 0)
                     :created-at (plist-get summary :created-at)
                     :updated-at (plist-get summary :updated-at)
                     :directory dir
                     :title-locked (plist-get summary :title-locked)
                     :plan-mode (harmless-json-true-p
                                 (plist-get summary :plan-mode))
                     :shell-cwd (let ((cwd (plist-get summary :shell-cwd)))
                                  (and (stringp cwd)
                                       (not (string-empty-p cwd))
                                       cwd))
                     :shell-sandbox
                     (harmless-shell-sandbox-from-summary
                      (plist-get summary :shell-sandbox))
                     :agent (harmless-session--agent-from-summary summary)
                     :worktree (harmless-session--worktree-from-summary
                                summary))))
      (harmless-session-register session)
      (if (fboundp 'harmless-schedule-restore)
          (harmless-schedule-restore session summary)
        (harmless-session--schedules-from-summary session summary))
      (when (fboundp 'harmless-perm-install-configured-rules)
        (harmless-perm-install-configured-rules session))
      (when (fboundp 'harmless-extra-restore)
        (harmless-extra-restore session (plist-get summary :extra-dirs)))
      (when (fboundp 'harmless-todo-restore)
        (harmless-todo-restore session (plist-get summary :todos)))
      (when (fboundp 'harmless-review-restore)
        (harmless-review-restore session (plist-get summary :review)))
      (when (fboundp 'harmless-output-style-restore)
        (harmless-output-style-restore
         session (plist-get summary :output-style)))
      session)))

(defun harmless-session-list-on-disk ()
  "Return summary plists for every saved session."
  (let ((root (harmless-sessions-root))
        acc)
    (when (file-directory-p root)
      (dolist (cwd-dir (directory-files root t directory-files-no-dot-files-regexp))
        (when (file-directory-p cwd-dir)
          (dolist (sid-dir (directory-files cwd-dir t directory-files-no-dot-files-regexp))
            (let ((sum (expand-file-name "summary.json" sid-dir)))
              (when (file-exists-p sum)
                (push (cons sid-dir
                            (harmless-json-decode
                             (with-temp-buffer
                               (insert-file-contents sum)
                               (buffer-string))))
                      acc)))))))
    (nreverse acc)))

(defun harmless-session-resume (id)
  "Return a live session for ID, loading it from disk if needed."
  (or (harmless-session-get id)
      (cl-loop for (dir . summary) in (harmless-session-list-on-disk)
               when (string= (plist-get summary :id) id)
               return (harmless-session-load dir))))

(defun harmless-session--root-text-p (text)
  "Return non-nil if TEXT is \"/\" or a drive root."
  (or (harmless-filesystem-root-p text)
      (and (stringp text)
           (string-match-p "\\`[A-Za-z]:[/\\]?\\'" (string-trim text)))))

(defun harmless-session-rename (session title)
  "Set SESSION's title to TITLE and keep later prompts from replacing it.
Return the stored title.  The session id and directory stay.  A
relative project directory is not involved."
  (unless (harmless-session-p session)
    (error "No Harmless session"))
  (cond
   ((null title) (error "session title is empty"))
   ((not (stringp title))
    (error "session title must be a string")))
  (setq title (string-trim
               (replace-regexp-in-string "[\n\r]+" " " title)))
  (when (string-empty-p title)
    (error "session title is empty"))
  (setf (harmless-session-title session) title
        (harmless-session-title-locked session) t
        (harmless-session-updated-at session) (harmless-now-iso))
  (harmless-session-save session)
  (harmless-emit session (list :title title))
  title)

(defun harmless-session-delete (session)
  "Remove SESSION from the registry and delete its directory.
Return the session id.  A turn that is streaming or waiting on
permission is refused.  The filesystem root is not deleted."
  (unless (harmless-session-p session)
    (error "No Harmless session"))
  (when (memq (harmless-session-status session)
              '(streaming waiting-permission))
    (error "session is busy"))
  (when (fboundp 'harmless-execute-stop)
    (harmless-execute-stop session))
  (let ((dir (harmless-session-directory session))
        (id (harmless-session-id session)))
    (when (stringp dir)
      (unless (harmless-session-dir-acceptable-p dir)
        (error "Refusing to delete a Harmless session at %s" dir))
      (when (equal (file-truename dir) (file-truename "/"))
        (error "Refusing to delete a Harmless session at %s" dir))
      (when (file-directory-p dir)
        (delete-directory dir t)))
    (when (buffer-live-p (harmless-session-buffer session))
      (kill-buffer (harmless-session-buffer session)))
    (when (buffer-live-p (harmless-session-prompt-buffer session))
      (kill-buffer (harmless-session-prompt-buffer session)))
    (setf (harmless-session-buffer session) nil
          (harmless-session-prompt-buffer session) nil)
    (harmless-session-unregister session)
    (harmless-emit session (list :deleted id))
    id))

(defun harmless-session--export-project (session)
  "Return SESSION's project directory, or signal.
A relative path is not expanded."
  (when (or (not (harmless-session-p session))
            (harmless-session-detached-p session))
    (error "This chat is not attached to a project"))
  (let ((cwd (harmless-session-cwd session)))
    (cond
     ((or (not (stringp cwd))
          (string-empty-p (string-trim cwd)))
      (error "This chat is not attached to a project"))
     ((harmless-session--root-text-p cwd)
      (error "session directory is the filesystem root"))
     ((or (string-prefix-p "~" (string-trim cwd))
          (not (file-name-absolute-p cwd)))
      (error "session directory must be absolute"))
     (t (file-name-as-directory (directory-file-name cwd))))))

(defun harmless-session--export-on-system-p (path)
  "Return non-nil if PATH names a file on this operating system.
A Windows drive path is on the system only when Emacs is running
on Windows.  Elsewhere it stays text: `file-directory-p' would
join it onto the current directory."
  (or (not (harmless-foreign-absolute-p path))
      (and (memq system-type '(windows-nt ms-dos))
           (string-match-p "\\`[A-Za-z]:" path))))

(defun harmless-session--export-absolute (path)
  "Return absolute file PATH, or signal.
The parent must already exist.  It is not created."
  (when (or (directory-name-p path)
            (string-suffix-p "/" path)
            (string-suffix-p "\\" path))
    (error "session export path is not a file"))
  (when (and (harmless-session--export-on-system-p path)
             (file-exists-p path)
             (equal (file-truename path) (file-truename "/")))
    (error "session export path is the filesystem root"))
  (let ((parent (file-name-directory path)))
    (unless (and (stringp parent)
                 (not (harmless-session--root-text-p parent)))
      (error "session export path is the filesystem root"))
    (unless (or (not (harmless-session--export-on-system-p parent))
                (file-directory-p parent))
      (error "session export path is missing")))
  path)

(defun harmless-session--export-relative (session raw)
  "Return RAW expanded inside SESSION's project, or signal."
  (let* ((project (harmless-session--export-project session))
         (full (harmless-expand-file-name raw project)))
    (when (or (directory-name-p full)
              (string-suffix-p "/" raw)
              (string-suffix-p "\\" raw))
      (error "session export path is not a file"))
    (let ((parent (file-name-directory full)))
      (unless (and (stringp parent)
                   (not (harmless-session--root-text-p parent))
                   (or (harmless-same-directory-p parent project)
                       (harmless-directory-strictly-under-p
                        parent project)))
        (error "session export path escapes the project: %s" raw))
      (unless (file-directory-p parent)
        (make-directory parent t)))
    full))

(defun harmless-session--export-destination (session path)
  "Return the absolute file for exporting SESSION to PATH."
  (cond
   ((null path) (error "session export path is empty"))
   ((not (stringp path))
    (error "session export path must be a string"))
   (t
    (let ((raw (string-trim path)))
      (cond
       ((string-empty-p raw)
        (error "session export path is empty"))
       ((harmless-session--root-text-p raw)
        (error "session export path is the filesystem root"))
       ((string-prefix-p "~" raw)
        (error "session export path must be absolute"))
       ((harmless-path-absolute-p raw)
        (harmless-session--export-absolute
         (if (harmless-foreign-absolute-p raw)
             (harmless-collapse-path raw)
           raw)))
       (t (harmless-session--export-relative session raw)))))))

(defun harmless-session--export-message (msg)
  "Return MSG as one Markdown section."
  (let* ((role (plist-get msg :role))
         (label (cond
                 ((or (memq role '(:user user)) (equal role "user"))
                  "User")
                 ((or (memq role '(:assistant assistant))
                      (equal role "assistant"))
                  "Assistant")
                 ((or (memq role '(:tool tool)) (equal role "tool"))
                  (format "Tool %s" (or (plist-get msg :name) "tool")))
                 ((or (memq role '(:summary summary))
                      (equal role "summary"))
                  "Summary")
                 (t (format "%s" (or role "Message")))))
         (content (or (plist-get msg :content) "")))
    (format "## %s\n\n%s" label content)))

(defun harmless-session--export-text (session)
  "Return SESSION's transcript as Markdown."
  (concat
   (format "# %s\n\n" (or (harmless-session-title session) "Untitled"))
   (format "- id: %s\n" (harmless-session-id session))
   (if (harmless-session-detached-p session)
       "- project: detached\n\n"
     (format "- project: %s\n\n" (or (harmless-session-cwd session) "")))
   (mapconcat #'harmless-session--export-message
              (harmless-session-messages session)
              "\n\n")))

(defun harmless-session-export (session path)
  "Write SESSION's transcript as Markdown at PATH.
Return the absolute path.  The session is not modified.  A relative
PATH stays inside the project.  An absolute PATH is written only
when its parent already exists."
  (unless (harmless-session-p session)
    (error "No Harmless session"))
  (let ((dest (harmless-session--export-destination session path))
        (text (harmless-session--export-text session))
        (coding-system-for-write 'utf-8-unix))
    (write-region text nil dest nil 'silent)
    dest))

(defconst harmless-session-search-limit 5
  "Maximum sessions named in one recall.")

(defconst harmless-session-search-bytes 100000
  "Maximum characters read from one transcript.")

(defconst harmless-session-search-snippet 160
  "Maximum characters of one excerpt before an ellipsis.")

(defun harmless-session-search--root ()
  "Return the sessions directory a search may read.
Nil uses the Harmless data directory.  \"\" and the filesystem
root are refused.  The directory is not created."
  (when (and (stringp harmless-directory)
             (string-empty-p (string-trim harmless-directory)))
    (error "session search path is empty"))
  (when (and (stringp harmless-directory)
             (harmless-session--root-text-p harmless-directory))
    (error "session search path is the filesystem root"))
  (let ((dir (condition-case err
                 (harmless-sessions-root)
               (error
                (if (string-prefix-p "Refusing to store Harmless data"
                                     (error-message-string err))
                    (error "session search path is the filesystem root")
                  (signal (car err) (cdr err)))))))
    (when (or (not (stringp dir))
              (harmless-filesystem-root-p dir)
              (harmless-session--root-text-p dir))
      (error "session search path is the filesystem root"))
    dir))

(defun harmless-session-search--inside-p (root path)
  "Return non-nil if PATH stays inside ROOT after resolving links."
  (condition-case nil
      (let* ((base (file-name-as-directory
                    (file-truename (directory-file-name root))))
             (full (file-truename path)))
        (and (stringp full)
             (or (string-prefix-p base full)
                 (string-prefix-p base (file-name-as-directory full)))))
    (error nil)))

(defun harmless-session-search--read (file)
  "Return the start of FILE, or nil when it is not a readable file."
  (when (and (file-regular-p file) (file-readable-p file))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix))
        (insert-file-contents file nil 0 harmless-session-search-bytes))
      (buffer-string))))

(defun harmless-session-search--strings (value)
  "Return the strings stored in VALUE."
  (cond
   ((stringp value) (list value))
   ((or (null value) (not (listp value))) nil)
   (t (apply #'nconc (mapcar #'harmless-session-search--strings value)))))

(defun harmless-session-search--snippet (text query)
  "Return a short excerpt of TEXT around QUERY, or nil."
  (let* ((case-fold-search t)
         (flat (string-trim
                (replace-regexp-in-string "[ \t\n\r]+" " " (or text ""))))
         (at (string-match (regexp-quote query) flat)))
    (when at
      (let* ((start (max 0 (- at 40)))
             (end (min (length flat) (+ start harmless-session-search-snippet)))
             (piece (substring flat start end)))
        (when (> start 0)
          (setq piece (concat "…" piece)))
        (when (< end (length flat))
          (setq piece (concat piece "…")))
        piece))))

(defun harmless-session-search--summary (dir)
  "Return DIR's summary plist, or nil when it cannot be read."
  (let ((raw (harmless-session-search--read
              (expand-file-name "summary.json" dir))))
    (and raw (harmless-json-decode-safe raw))))

(defun harmless-session-search--transcript (dir query)
  "Return the first excerpt of DIR's transcript that matches QUERY."
  (let ((raw (harmless-session-search--read
              (expand-file-name "messages.jsonl" dir)))
        (found nil))
    (when raw
      (dolist (line (split-string raw "\n" t))
        (unless found
          (let ((msg (harmless-json-decode-safe line)))
            (when msg
              (setq found
                    (harmless-session-search--snippet
                     (mapconcat #'identity
                                (harmless-session-search--strings msg)
                                "\n")
                     query)))))))
    found))

(defun harmless-session-search--one (dir query)
  "Return one hit for DIR, or nil.
The session is not loaded."
  (let* ((summary (harmless-session-search--summary dir))
         (id (or (and summary
                     (let ((raw (plist-get summary :id)))
                       (and (stringp raw)
                            (not (string-empty-p (string-trim raw)))
                            (string-trim raw))))
                 (file-name-nondirectory (directory-file-name dir))))
         (title (and summary (plist-get summary :title)))
         (updated (or (and summary (plist-get summary :updated-at)) ""))
         (snippet (or (harmless-session-search--transcript dir query)
                      (and (stringp title)
                           (harmless-session-search--snippet title query)))))
    (when (and snippet (stringp id) (not (string-empty-p id)))
      (list (if (stringp updated) updated "")
            id
            (and (stringp title) title)
            snippet))))

(defun harmless-session-search--hits (root query)
  "Return hits under ROOT for QUERY.
A directory that leaves ROOT is skipped."
  (let (hits)
    (when (and (file-directory-p root)
               (harmless-session-search--inside-p root root))
      (dolist (cwd-dir (directory-files root t directory-files-no-dot-files-regexp))
        (when (and (file-directory-p cwd-dir)
                   (harmless-session-search--inside-p root cwd-dir))
          (dolist (sid-dir (directory-files
                            cwd-dir t directory-files-no-dot-files-regexp))
            (when (and (file-directory-p sid-dir)
                       (harmless-session-search--inside-p root sid-dir))
              (let ((hit (harmless-session-search--one sid-dir query)))
                (when hit (push hit hits))))))))
    hits))

(defun harmless-session-search--format (hit)
  "Return one recall block for HIT."
  (let ((id (nth 1 hit))
        (title (nth 2 hit))
        (snippet (nth 3 hit)))
    (concat id
            (when (and (stringp title) (not (string-empty-p (string-trim title))))
              (concat " — " (string-trim title)))
            "\n"
            snippet)))

(defun harmless-session-search (query)
  "Return a short recall of saved transcripts that contain QUERY.
Nil, \"\", and whitespace return an empty recall.  A query that
matches nothing returns an empty recall.  A value that is not text
is refused.  The search reads transcripts under the Harmless data
directory.  It does not load a session and it does not install
that session's tools."
  (cond
   ((null query) "")
   ((not (stringp query))
    (error "session search query must be a string"))
   (t
    (let ((needle (string-trim query)))
      (if (string-empty-p needle)
          ""
        (let* ((root (harmless-session-search--root))
               (hits (harmless-session-search--hits root needle))
               (ordered (sort hits
                              (lambda (a b) (string> (car a) (car b)))))
               (kept (seq-take ordered harmless-session-search-limit)))
          (if kept
              (mapconcat #'harmless-session-search--format kept "\n\n")
            "")))))))

(defun harmless-session-search--tool (_session args)
  "Tool wrapper: search saved transcripts for ARGS' query."
  (harmless-session-search (harmless-tool-arg args :query)))

(harmless-register-tool
 (harmless-tool-create
  :name "harmless-tool-session-search"
  :description "Search saved transcripts on this machine and return a short recall. Nil, an empty query, and a query that matches nothing return an empty recall. The search does not load another session or its tools."
  :class 'read
  :schema '(:type "object"
            :properties (:query (:type "string"
                                 :description "Text to find in a saved transcript"))
            :required ["query"])
  :fn #'harmless-session-search--tool))

(provide 'harmless-session)

;;; harmless-session.el ends here
