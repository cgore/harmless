;;; harmless-workflow.el --- One script, many child sessions -*- lexical-binding: t; -*-

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
;; `harmless-tool-run-workflow' runs one script.  The script starts child sessions
;; and the tool returns one string, the value of `complete'.  A fixed
;; script is a NAME.hw file.  The model can also pass the script text
;; for this task.  A subagent cannot run a workflow.

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
(require 'harmless-agent)
(require 'harmless-worktree)

(defvar harmless-agent-stop)

(defcustom harmless-workflow-max-agents 8
  "Child sessions one workflow may start.
The script is refused before the first child when it would start
more."
  :type 'integer
  :group 'harmless)

(defvar harmless-workflow-stop nil
  "Directory where workflow discovery stops.
Nil stops at the user's home directory, as project instructions do.")

(defvar harmless-workflow--sandboxes nil
  "Sandboxes seen by the validator.  Internal.")

(defun harmless-workflow--name (name)
  "Return NAME when it is a workflow name, or signal.
Nil is not a name.  \"\" and whitespace signal \"workflow name is
empty\".  A path signals \"workflow name is not a name\"."
  (cond
   ((not (stringp name))
    (error "workflow name is not a name"))
   (t
    (setq name (string-trim name))
    (cond
     ((string-empty-p name) (error "workflow name is empty"))
     ((harmless-agent-name-p name) name)
     (t (error "workflow name is not a name"))))))

(defun harmless-workflow--source (args)
  "Return (script . TEXT) or (name . NAME) from ARGS.
Neither signals \"workflow needs a name or a script\".  Both signal
\"workflow takes a name or a script\"."
  (let ((name (harmless-tool-arg args :name))
        (script (harmless-tool-arg args :script)))
    (cond
     ((and (null name) (null script))
      (error "workflow needs a name or a script"))
     ((and name script)
      (error "workflow takes a name or a script"))
     (script
      (unless (stringp script)
        (error "workflow script must be a string"))
      (when (string-empty-p (string-trim script))
        (error "workflow script is empty"))
      (cons 'script script))
     (t (cons 'name (harmless-workflow--name name))))))

(defun harmless-workflow-read (text)
  "Return the forms in TEXT.
A non-string signals \"workflow script must be a string\".  Nil,
\"\", and whitespace signal \"workflow script is empty\".  A form
the reader cannot read signals \"workflow script is invalid\"."
  (unless (stringp text)
    (error "workflow script must be a string"))
  (when (string-empty-p (string-trim text))
    (error "workflow script is empty"))
  (condition-case nil
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (let (forms)
          (while (progn
                   (while (progn
                            (skip-chars-forward " \t\n\r\f")
                            (and (not (eobp)) (eq (char-after) ?\;)))
                     (end-of-line))
                   (not (eobp)))
            (push (read (current-buffer)) forms))
          (nreverse forms)))
    (error (error "workflow script is invalid"))))

(defun harmless-workflow--read-file (path)
  "Return (KEYS SCRIPT) for PATH.  A missing file is an error."
  (unless (and (stringp path) (file-regular-p path) (file-readable-p path))
    (error "no workflow file %s" path))
  (harmless-agent--front
   (replace-regexp-in-string
    "\r" ""
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix))
        (insert-file-contents path))
      (buffer-string)))))

(defun harmless-workflow--file-name (path)
  "Return the workflow name declared by PATH, or nil."
  (condition-case nil
      (let* ((parsed (harmless-workflow--read-file path))
             (keys (car parsed))
             (named (plist-get keys :name))
             (name (if (and (stringp named) (not (string-empty-p named)))
                       named
                     (file-name-sans-extension
                      (file-name-nondirectory path)))))
        (and (harmless-agent-name-p name) name))
    (error nil)))

(defun harmless-workflow--files (dir stop)
  "Return workflow files for DIR, lowest priority first.
STOP is passed to `harmless-instruction-directories'."
  (let (files)
    (dolist (ancestor (harmless-instruction-directories dir stop))
      (dolist (vendor harmless-skill-vendors)
        (let ((root (expand-file-name
                     "workflows"
                     (expand-file-name (concat "." vendor) ancestor))))
          (when (and (file-directory-p root)
                     (not (harmless-filesystem-root-p root)))
            (dolist (name (directory-files root nil "\\.hw\\'"))
              (unless (eq (aref name 0) ?.)
                (let ((path (expand-file-name name root)))
                  (when (and (file-regular-p path)
                             (file-readable-p path))
                    (push path files)))))))))
    (nreverse files)))

(defun harmless-workflow--identified (dir stop)
  "Return (NAME . PATH) pairs for DIR, lowest priority first."
  (let (pairs)
    (dolist (path (harmless-workflow--files dir stop))
      (let ((name (harmless-workflow--file-name path)))
        (when name
          (push (cons name path) pairs))))
    (nreverse pairs)))

(defun harmless-workflow--parse (path)
  "Return a spec plist for PATH, or signal when it is invalid."
  (let* ((parsed (harmless-workflow--read-file path))
         (keys (car parsed))
         (body (cadr parsed))
         (name (harmless-workflow--file-name path)))
    (unless name
      (error "workflow name is not a name"))
    (when (or (null body) (string-empty-p body))
      (error "workflow script is empty"))
    (harmless-workflow-read body)
    (list :name name
          :description (let ((desc (plist-get keys :description)))
                         (and (stringp desc)
                              (not (string-empty-p desc))
                              desc))
          :script body
          :path path)))

(defun harmless-workflow-discover (dir &optional stop)
  "Return workflow specs that apply to DIR, sorted by name.
Nil, \"\", a relative path, and the filesystem root return nil.
STOP bounds the walk and defaults to `harmless-workflow-stop'."
  (if (not (harmless-agent-directory-p dir))
      nil
    (let ((table (make-hash-table :test 'equal)))
      (dolist (pair (harmless-workflow--identified
                     dir (or stop harmless-workflow-stop)))
        (condition-case nil
            (puthash (car pair)
                     (harmless-workflow--parse (cdr pair))
                     table)
          (error nil)))
      (let (specs)
        (maphash (lambda (_name spec) (push spec specs)) table)
        (sort specs (lambda (a b)
                      (string< (plist-get a :name)
                               (plist-get b :name))))))))

(defun harmless-workflow-find (dir name &optional stop)
  "Return the spec named NAME for DIR, or nil.
A definition that fails to parse signals.  STOP defaults to
`harmless-workflow-stop'."
  (when (harmless-agent-directory-p dir)
    (let (path)
      (dolist (pair (harmless-workflow--identified
                     dir (or stop harmless-workflow-stop)))
        (when (string= (car pair) name)
          (setq path (cdr pair))))
      (when path
        (harmless-workflow--parse path)))))

(defun harmless-workflow-catalog (dir &optional stop)
  "Return the workflow list for DIR, or nil when there are none."
  (let ((specs (harmless-workflow-discover dir stop)))
    (when specs
      (concat
       "Workflows available for this session.  "
       "harmless-tool-run-workflow runs one by name.  "
       "The script starts child sessions and returns one result.\n\n"
       (mapconcat
        (lambda (spec)
          (format "- %s (%s): %s"
                  (plist-get spec :name)
                  (plist-get spec :path)
                  (or (plist-get spec :description) "")))
        specs
        "\n")))))

(defun harmless-workflow-context-suffix (session messages)
  "Return MESSAGES plus SESSION's workflow catalog.
A subagent does not receive the catalog."
  (if (eq (harmless-session-source session) 'agent)
      messages
    (harmless-context-append
     messages
     (harmless-workflow-catalog (harmless-session-cwd session)
                                harmless-workflow-stop))))

(defun harmless-workflow--body (session name)
  "Return the script text of workflow NAME for SESSION."
  (let ((dir (harmless-session-cwd session)))
    (unless (harmless-agent-directory-p dir)
      (error "no workflow named %s" name))
    (let ((spec (harmless-workflow-find dir name)))
      (unless spec
        (error "no workflow named %s" name))
      (plist-get spec :script))))

(defun harmless-workflow--resolved (session args)
  "Return the forms ARGS selects for SESSION."
  (let ((source (harmless-workflow--source args)))
    (harmless-workflow-read
     (if (eq (car source) 'script)
         (cdr source)
       (harmless-workflow--body session (cdr source))))))

(defun harmless-workflow--check-var (name)
  "Signal unless NAME can be bound in a script."
  (unless (and (symbolp name)
               (not (keywordp name))
               (harmless-agent-name-p (symbol-name name)))
    (error "workflow variable is not a name"))
  name)

(defun harmless-workflow--lookup (env name)
  "Return the value of NAME in ENV, or signal."
  (harmless-workflow--check-var name)
  (let ((cell (assq name env)))
    (unless cell
      (error "workflow variable is unbound: %s" (symbol-name name)))
    (cdr cell)))

(defun harmless-workflow--one-line (value empty label)
  "Return VALUE when it is one line of text.
EMPTY is the error when it is blank.  LABEL names the field."
  (unless (stringp value)
    (error "%s must be a string" label))
  (when (string-match-p "[\n\r]" value)
    (error "%s must be one line" label))
  (setq value (string-trim value))
  (when (string-empty-p value)
    (error "%s" empty))
  value)

(defun harmless-workflow--sandbox-symbol (value)
  "Return `workspace' or `read-only' for VALUE, or signal."
  (let ((name (cond
               ((symbolp value) (symbol-name value))
               ((stringp value) value)
               (t ""))))
    (cond
     ((string= name "read-only") 'read-only)
     ((string= name "workspace") 'workspace)
     (t (error "sandbox must be workspace or read-only")))))

(defun harmless-workflow--agent-name (value)
  "Return the agent name VALUE, or nil when it was omitted."
  (cond
   ((null value) nil)
   ((symbolp value)
    (harmless-workflow--agent-name (symbol-name value)))
   ((stringp value)
    (setq value (string-trim value))
    (cond
     ((string-empty-p value) (error "agent name is empty"))
     ((harmless-agent-name-p value) value)
     (t (error "agent name is not a name"))))
   (t (error "agent name is not a name"))))

(defun harmless-workflow--agent-parts (expr)
  "Return (PROMPT . OPTS) for an agent EXPR.
OPTS is a plist with :sandbox, :agent, :label, :description,
and :worktree."
  (unless (and (proper-list-p expr) (eq (car expr) 'agent))
    (error "unknown workflow expression"))
  (when (null (cdr expr))
    (error "prompt is empty"))
  (let ((rest (cddr expr))
        (opts nil))
    (while rest
      (unless (keywordp (car rest))
        (error "unknown workflow option: %s"
               (if (symbolp (car rest))
                   (symbol-name (car rest))
                 "bad")))
      (unless (cdr rest)
        (error "unknown workflow option: %s"
               (substring (symbol-name (car rest)) 1)))
      (let ((key (car rest))
            (val (cadr rest)))
        (pcase key
          (:sandbox
           (setq opts (plist-put opts :sandbox
                                 (harmless-workflow--sandbox-symbol val))))
          (:agent
           (setq opts (plist-put opts :agent
                                 (harmless-workflow--agent-name val))))
          (:label
           (setq opts (plist-put opts :label
                                 (harmless-workflow--one-line
                                  val "label is empty" "label"))))
          (:description
           (setq opts (plist-put opts :description
                                 (harmless-workflow--one-line
                                  val "description is empty"
                                  "description"))))
          (:worktree
           (setq opts (plist-put opts :worktree
                                 (harmless-worktree-name-required val))))
          (_ (error "unknown workflow option: %s"
                    (substring (symbol-name key) 1)))))
      (setq rest (cddr rest)))
    (cons (cadr expr) opts)))

(defun harmless-workflow--note-sandbox (session opts)
  "Return the sandbox OPTS will use on SESSION, and record it.
An :agent name with no definition signals \"no agent named\" even
when :sandbox is set."
  (let* ((explicit (plist-get opts :sandbox))
         (agent (plist-get opts :agent))
         (spec (and agent
                    (or (harmless-agent-find
                         (harmless-session-cwd session)
                         agent
                         harmless-agent-stop)
                        (error "no agent named %s" agent))))
         (sandbox (or explicit
                      (and spec (plist-get spec :sandbox))
                      'workspace)))
    (push sandbox harmless-workflow--sandboxes)
    (when (plist-get opts :worktree)
      (push 'workspace harmless-workflow--sandboxes))
    sandbox))

(defun harmless-workflow--validate-expr (session env expr)
  "Signal unless EXPR is a script expression in ENV."
  (cond
   ((stringp expr) nil)
   ((symbolp expr) (harmless-workflow--lookup env expr))
   ((not (proper-list-p expr))
    (error "unknown workflow expression"))
   ((eq (car expr) 'agent)
    (let ((parts (harmless-workflow--agent-parts expr)))
      (harmless-workflow--validate-expr session env (car parts))
      (harmless-workflow--note-sandbox session (cdr parts))
      (when (and (stringp (car parts))
                 (string-empty-p (string-trim (car parts))))
        (error "prompt is empty"))))
   ((eq (car expr) 'parallel)
    (when (null (cdr expr))
      (error "workflow parallel is empty"))
    (dolist (item (cdr expr))
      (unless (and (proper-list-p item) (eq (car item) 'agent))
        (error "workflow parallel takes agents"))
      (harmless-workflow--validate-expr session env item)))
   ((eq (car expr) 'join)
    (unless (stringp (cadr expr))
      (error "workflow join needs a separator"))
    (dolist (item (cddr expr))
      (harmless-workflow--validate-expr session env item)))
   (t (error "unknown workflow expression"))))

(defun harmless-workflow--validate-forms (session env forms)
  "Signal unless FORMS are a script in ENV."
  (cond
   ((null forms) (error "workflow has no complete"))
   ((not (and (proper-list-p (car forms)) (symbolp (caar forms))))
    (error "unknown workflow form"))
   ((eq (caar forms) 'complete)
    (when (cdr forms)
      (error "workflow has forms after complete"))
    (unless (= (length (car forms)) 2)
      (error "workflow complete needs a value"))
    (harmless-workflow--validate-expr session env (cadar forms)))
   ((eq (caar forms) 'let)
    (unless (= (length (car forms)) 3)
      (error "workflow let needs a name and a value"))
    (let ((name (harmless-workflow--check-var (nth 1 (car forms)))))
      (when (assq name env)
        (error "workflow variable is already bound: %s"
               (symbol-name name)))
      (harmless-workflow--validate-expr session env (nth 2 (car forms)))
      (harmless-workflow--validate-forms
       session (cons (cons name "") env) (cdr forms))))
   (t (error "unknown workflow form: %s"
             (symbol-name (caar forms))))))

(defun harmless-workflow--validate (session forms)
  "Signal unless FORMS are a script.  Return the sandbox list."
  (let ((harmless-workflow--sandboxes nil))
    (harmless-workflow--validate-forms session nil forms)
    (nreverse harmless-workflow--sandboxes)))

(defun harmless-workflow--count-expr (expr)
  "Return how many agent forms EXPR contains."
  (cond
   ((not (proper-list-p expr)) 0)
   ((eq (car expr) 'agent)
    (1+ (harmless-workflow--count-expr (cadr expr))))
   ((eq (car expr) 'parallel)
    (apply #'+ 0 (mapcar #'harmless-workflow--count-expr (cdr expr))))
   ((eq (car expr) 'join)
    (apply #'+ 0 (mapcar #'harmless-workflow--count-expr (cddr expr))))
   (t 0)))

(defun harmless-workflow--count-form (form)
  "Return how many agent forms FORM contains."
  (cond
   ((and (proper-list-p form) (eq (car form) 'let))
    (harmless-workflow--count-expr (nth 2 form)))
   ((and (proper-list-p form) (eq (car form) 'complete))
    (harmless-workflow--count-expr (cadr form)))
   (t 0)))

(defun harmless-workflow--check-budget (count)
  "Signal when COUNT exceeds `harmless-workflow-max-agents'."
  (let ((max harmless-workflow-max-agents))
    (unless (and (integerp max) (> max 0))
      (error "harmless-workflow-max-agents must be a positive integer"))
    (when (> count max)
      (error "workflow agent budget is %d" max))))

(defun harmless-workflow--writes-p (session args)
  "Return non-nil when ARGS would start a child that can write."
  (memq 'workspace
        (harmless-workflow--validate
         session (harmless-workflow--resolved session args))))

(defun harmless-workflow-class (session args)
  "Return `read' when ARGS cannot write, otherwise `edit'."
  (condition-case nil
      (if (harmless-workflow--writes-p session args)
          'edit
        'read)
    (error 'edit)))

(defun harmless-workflow-plan-refuses-p (session tool args)
  "Return a refusal when plan mode blocks this workflow, or nil."
  (when (and (string= (harmless-tool-name tool) "harmless-tool-run-workflow")
             (harmless-session-plan-mode session))
    (condition-case nil
        (when (harmless-workflow--writes-p session args)
          "Error: Plan mode is on. A workflow that can write is refused")
      (error nil))))

(defun harmless-workflow--stopped-p (batch)
  "Return non-nil when BATCH was aborted."
  (and batch (harmless-turn-batch-aborted batch)))

(defun harmless-workflow--report (state text &optional abort)
  "Send TEXT once through STATE's callback.
ABORT stops children that are still running.  An aborted parent
batch is not reported."
  (unless (plist-get state :done)
    (plist-put state :done t)
    (when abort
      (harmless-agent-abort-children (plist-get state :session)))
    (unless (harmless-workflow--stopped-p (plist-get state :batch))
      (funcall (plist-get state :callback) text))))

(defun harmless-workflow--continue (state thunk)
  "Call THUNK unless STATE is finished or the parent aborted.
A signal becomes the workflow result."
  (unless (or (plist-get state :done)
              (harmless-workflow--stopped-p (plist-get state :batch)))
    (condition-case err
        (funcall thunk)
      (error (harmless-workflow--report
              state
              (format "Error: %s" (error-message-string err))
              t)))))

(defun harmless-workflow--deliver (state done value)
  "Pass VALUE to DONE unless STATE has stopped."
  (harmless-workflow--continue state (lambda () (funcall done value))))

(defun harmless-workflow--texts (value)
  "Return VALUE as a list of strings, or signal."
  (cond
   ((stringp value) (list value))
   ((null value) nil)
   ((and (listp value) (cl-every #'stringp value)) value)
   (t (error "workflow value is not text"))))

(defun harmless-workflow--join-values (sep values)
  "Join VALUES with SEP.  A list value is joined the same way."
  (mapconcat #'identity
             (apply #'append
                    (mapcar #'harmless-workflow--texts values))
             sep))

(defun harmless-workflow--one-string (value)
  "Return VALUE as one string.  A list is joined with newlines."
  (mapconcat #'identity (harmless-workflow--texts value) "\n"))

(defun harmless-workflow--launch (state prompt opts slot)
  "Start one child for PROMPT and OPTS.  SLOT receives its reply."
  (let* ((session (plist-get state :session))
         (sandbox (plist-get opts :sandbox))
         (args (list :prompt prompt
                     :description (or (plist-get opts :description)
                                      (plist-get opts :label))))
         (agent-name (plist-get opts :agent))
         (worktree (plist-get opts :worktree))
         child)
    (when agent-name
      (setq args (plist-put args :agent agent-name)))
    (when worktree
      (setq args (plist-put args :worktree worktree)))
    (setq child
          (harmless-agent-create
           session args
           (lambda (text)
             (harmless-workflow--deliver state slot text))))
    (when sandbox
      (plist-put (harmless-session-agent child) :sandbox sandbox)
      (harmless-session-save child))
    (harmless-turn-run child prompt)
    child))

(defun harmless-workflow--launch-parallel (state items done)
  "Start every (PROMPT . OPTS) in ITEMS, then call DONE with the replies."
  (let ((left (length items))
        (results (make-vector (length items) nil))
        (started nil)
        (index 0))
    (dolist (item items)
      (let ((i index))
        (harmless-workflow--launch
         state (car item) (cdr item)
         (lambda (text)
           (aset results i text)
           (setq left (1- left))
           (when (and started (zerop left))
             (funcall done (append results nil))))))
      (setq index (1+ index)))
    (setq started t)
    (when (zerop left)
      (funcall done (append results nil)))))

(defun harmless-workflow--eval (state env expr done)
  "Evaluate EXPR in ENV and call DONE with the value."
  (harmless-workflow--continue
   state
   (lambda ()
     (cond
      ((stringp expr) (funcall done expr))
      ((symbolp expr)
       (funcall done (harmless-workflow--lookup env expr)))
      ((not (proper-list-p expr))
       (error "unknown workflow expression"))
      ((eq (car expr) 'agent)
       (harmless-workflow--eval-agent state env expr done))
      ((eq (car expr) 'parallel)
       (harmless-workflow--eval-parallel state env (cdr expr) done))
      ((eq (car expr) 'join)
       (harmless-workflow--eval-join state env expr done))
      (t (error "unknown workflow expression"))))))

(defun harmless-workflow--eval-agent (state env expr done)
  "Evaluate an agent EXPR and call DONE with its reply."
  (let* ((parts (harmless-workflow--agent-parts expr))
         (opts (cdr parts)))
    (harmless-workflow--eval
     state env (car parts)
     (lambda (prompt)
       (unless (stringp prompt)
         (error "prompt must be a string"))
       (when (string-empty-p (string-trim prompt))
         (error "prompt is empty"))
       (harmless-workflow--launch
        state (string-trim prompt) opts
        (lambda (text) (funcall done text)))))))

(defun harmless-workflow--prompts (state env exprs acc done)
  "Evaluate the prompts of agent EXPRS, then call DONE with the items."
  (if (null exprs)
      (funcall done (nreverse acc))
    (let* ((parts (harmless-workflow--agent-parts (car exprs)))
           (opts (cdr parts)))
      (harmless-workflow--eval
       state env (car parts)
       (lambda (prompt)
         (unless (stringp prompt)
           (error "prompt must be a string"))
         (when (string-empty-p (string-trim prompt))
           (error "prompt is empty"))
         (harmless-workflow--prompts
          state env (cdr exprs)
          (cons (cons (string-trim prompt) opts) acc)
          done))))))

(defun harmless-workflow--eval-parallel (state env exprs done)
  "Evaluate a parallel list of agent EXPRS."
  (when (null exprs)
    (error "workflow parallel is empty"))
  (dolist (expr exprs)
    (unless (and (proper-list-p expr) (eq (car expr) 'agent))
      (error "workflow parallel takes agents")))
  (harmless-workflow--prompts
   state env exprs nil
   (lambda (items)
     (harmless-workflow--launch-parallel state items done))))

(defun harmless-workflow--eval-list (state env exprs acc done)
  "Evaluate EXPRS in order and call DONE with the values."
  (if (null exprs)
      (funcall done (nreverse acc))
    (harmless-workflow--eval
     state env (car exprs)
     (lambda (value)
       (harmless-workflow--eval-list
        state env (cdr exprs) (cons value acc) done)))))

(defun harmless-workflow--eval-join (state env expr done)
  "Evaluate a join EXPR."
  (unless (stringp (cadr expr))
    (error "workflow join needs a separator"))
  (harmless-workflow--eval-list
   state env (cddr expr) nil
   (lambda (values)
     (funcall done (harmless-workflow--join-values (cadr expr) values)))))

(defun harmless-workflow--forms (state env forms)
  "Run FORMS in ENV.  `complete' reports STATE's result."
  (harmless-workflow--continue
   state
   (lambda ()
     (cond
      ((null forms) (error "workflow has no complete"))
      ((not (and (proper-list-p (car forms)) (symbolp (caar forms))))
       (error "unknown workflow form"))
      ((eq (caar forms) 'complete)
       (when (cdr forms)
         (error "workflow has forms after complete"))
       (unless (= (length (car forms)) 2)
         (error "workflow complete needs a value"))
       (harmless-workflow--eval
        state env (cadar forms)
        (lambda (value)
          (harmless-workflow--report
           state (harmless-workflow--one-string value)))))
      ((eq (caar forms) 'let)
       (unless (= (length (car forms)) 3)
         (error "workflow let needs a name and a value"))
       (let ((name (harmless-workflow--check-var (nth 1 (car forms)))))
         (when (assq name env)
           (error "workflow variable is already bound: %s"
                  (symbol-name name)))
         (harmless-workflow--eval
          state env (nth 2 (car forms))
          (lambda (value)
            (harmless-workflow--forms
             state (cons (cons name value) env) (cdr forms))))))
      (t (error "unknown workflow form: %s"
                (symbol-name (caar forms))))))))

(defun harmless-workflow--state (session callback batch)
  "Return the run state for SESSION, CALLBACK, and BATCH."
  (list :done nil
        :session session
        :callback callback
        :batch batch))

(defun harmless-workflow--tool (session args callback)
  "harmless-tool-run-workflow implementation.  CALLBACK receives one result."
  (let ((state (harmless-workflow--state
                session callback
                (harmless-session-tool-batch session))))
    (harmless-workflow--continue
     state
     (lambda ()
       (when (eq (harmless-session-source session) 'agent)
         (error "a subagent cannot start a workflow"))
       (harmless-agent--description args)
       (let ((forms (harmless-workflow--resolved session args)))
         (harmless-workflow--validate session forms)
         (harmless-workflow--check-budget
          (apply #'+ 0 (mapcar #'harmless-workflow--count-form forms)))
         (harmless-workflow--forms state nil forms))))))

(defun harmless-workflow-register ()
  "Register the harmless-tool-run-workflow tool."
  (harmless-register-tool
   (harmless-tool-create
    :name "harmless-tool-run-workflow"
    :description
    (concat
     "Run one workflow script and return one result. "
     "Pass name to run a fixed script, or script to run text "
     "written for this task. Do not pass both. "
     "A fixed script is NAME.hw under .agents/workflows, "
     ".codex/workflows, .claude/workflows, .grok/workflows, "
     "or .harmless/workflows. "
     "Forms are (let NAME EXPR) and (complete EXPR). "
     "EXPR is a string, a bound name, "
     "(agent PROMPT :sandbox read-only :label TITLE "
     ":agent NAME :worktree NAME), "
     "(parallel (agent ...) ...), or (join SEP EXPR ...). "
     "complete's value is the result. "
     "The children have their own transcripts. "
     "A subagent cannot run a workflow.")
    :class 'edit
    :async t
    :classify #'harmless-workflow-class
    :schema '(:type "object"
              :properties
              (:name (:type "string"
                      :description "Fixed script name, not a path")
               :script (:type "string"
                        :description "Script text written for this task")
               :description (:type "string"
                             :description "Short title"))
              :required [])
    :fn #'harmless-workflow--tool)))

(harmless-workflow-register)

(provide 'harmless-workflow)

;;; harmless-workflow.el ends here
