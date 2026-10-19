;;; harmless-turn.el --- Agent turn loop for Harmless -*- lexical-binding: t; -*-

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
;; One user message starts a turn: call the provider, stream events, run
;; tools with permission, call the provider again until a text stop.
;; Tool calls from one reply start together.  Permission is one question
;; at a time, and an allowed call starts before the next question.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-log)
(require 'harmless-provider)
(require 'harmless-session)
(require 'harmless-instructions)
(require 'harmless-skills)
(require 'harmless-plan)
(require 'harmless-tools)
(require 'harmless-tools-shell)
(require 'harmless-perm)
(require 'harmless-usage)

(defvar harmless--perm-callback)

(cl-defstruct (harmless-turn-acc
               (:constructor harmless-turn-acc-create)
               (:copier nil))
  (text "")
  (reasoning "")
  (tools (make-hash-table :test 'equal))
  (order nil)
  stop-reason
  prompt-tokens
  completion-tokens
  (finished nil))

(defun harmless-turn--acc-tool (acc id name)
  "Return the tool-call plist for ID in ACC, creating it if needed."
  (or (gethash id (harmless-turn-acc-tools acc))
      (let ((entry (list :id id :name name :args "")))
        (puthash id entry (harmless-turn-acc-tools acc))
        (setf (harmless-turn-acc-order acc)
              (append (harmless-turn-acc-order acc) (list id)))
        entry)))

(declare-function harmless-compact-cancel "harmless-compact" (session))
(declare-function harmless-compact--validate-threshold "harmless-compact" ())
(declare-function harmless-compact-before-turn "harmless-compact" (session then))
(declare-function harmless-agent-abort-children "harmless-agent" (session))
(declare-function harmless-agent-note-abort "harmless-agent" (session))
(declare-function harmless-agent-note-error "harmless-agent" (session text))
(declare-function harmless-agent-limited-p "harmless-agent" (session))
(declare-function harmless-agent-stop-for-limit "harmless-agent" (session))
(declare-function harmless-agent-count-turn "harmless-agent" (session))
(declare-function harmless-agent-context-suffix "harmless-agent" (session messages))
(declare-function harmless-agent-refuse-call "harmless-agent" (session tool args))
(declare-function harmless-agent-plan-refuses-p "harmless-agent" (session tool args))
(declare-function harmless-agent-filter-tools "harmless-agent" (session tools))
(declare-function harmless-workflow-context-suffix "harmless-workflow"
                  (session messages))
(declare-function harmless-workflow-plan-refuses-p "harmless-workflow"
                  (session tool args))
(declare-function harmless-worktree-plan-refuses-p "harmless-worktree"
                  (session tool args))
(declare-function harmless-computer-plan-refuses-p "harmless-computer"
                  (session tool args))
(declare-function harmless-monitor-plan-refuses-p "harmless-monitor"
                  (session tool args))
(declare-function harmless-message-flush "harmless-message" (session))

(cl-defstruct (harmless-turn-batch
               (:constructor harmless-turn-batch-create)
               (:copier nil))
  session
  calls
  results
  ready
  (left 0)
  (next 0)
  (flushed 0)
  (launching nil)
  (waiting-perm nil)
  (aborted nil)
  (finishing nil)
  continuation)

(defun harmless-turn--clear-permission-prompt (session)
  "Drop a pending permission question for SESSION."
  (let ((buf (harmless-session-prompt-buffer session)))
    (when (and (bufferp buf)
               (buffer-live-p buf)
               (boundp 'harmless--perm-callback))
      (with-current-buffer buf
        (when harmless--perm-callback
          (setq harmless--perm-callback nil)
          (let ((inhibit-read-only t))
            (setq buffer-read-only nil)
            (erase-buffer)))))))

(defun harmless-turn-abort (session)
  "Cancel SESSION's in-flight HTTP request, foreground shells, and tool batch.
A background shell keeps running.  A subagent that is still running
is aborted too.  Its summary is not recorded on SESSION."
  (when (fboundp 'harmless-compact-cancel)
    (harmless-compact-cancel session))
  (let ((batch (harmless-session-tool-batch session)))
    (when batch
      (setf (harmless-turn-batch-aborted batch) t))
    (setf (harmless-session-tool-batch session) nil))
  (harmless-turn--clear-permission-prompt session)
  (harmless-session-kill-processes session)
  (when (fboundp 'harmless-agent-abort-children)
    (harmless-agent-abort-children session))
  (when (fboundp 'harmless-agent-note-abort)
    (harmless-agent-note-abort session))
  (setf (harmless-session-steers session) nil)
  (harmless-session-set-status session 'idle)
  (harmless-emit session '(:error "aborted")))

(defun harmless-turn-steer (session text)
  "Queue TEXT on SESSION without aborting a running turn.
When SESSION is not streaming and not waiting on permission, start a
turn with TEXT.  Nil, \"\", and whitespace signal \"Prompt is empty\".
Return `queued' or `sent'."
  (unless (harmless-session-p session)
    (error "No Harmless session"))
  (let ((text (if (stringp text) (string-trim text) "")))
    (when (string-empty-p text)
      (error "Prompt is empty"))
    (if (memq (harmless-session-status session)
              '(streaming waiting-permission))
        (progn
          (setf (harmless-session-steers session)
                (append (harmless-session-steers session) (list text)))
          'queued)
      (harmless-turn-run session text)
      'sent)))

(defun harmless-turn--take-steers (session)
  "Append SESSION's queued follow-ups as user messages.
Return non-nil when there was at least one."
  (let ((texts (harmless-session-steers session)))
    (setf (harmless-session-steers session) nil)
    (dolist (text texts)
      (harmless-session-append-user session text))
    texts))

(defun harmless-turn-run (session text)
  "Append user TEXT to SESSION and start the agent loop.
When the last prompt fills the context window, older turns are
summarized before the provider sees TEXT."
  (when (memq (harmless-session-status session) '(streaming waiting-permission))
    (harmless-turn-abort session))
  (when (fboundp 'harmless-compact--validate-threshold)
    (harmless-compact--validate-threshold))
  (harmless-session-append-user session text)
  (if (fboundp 'harmless-compact-before-turn)
      (harmless-compact-before-turn session
                                    (lambda () (harmless-turn--call session)))
    (harmless-turn--call session)))

(defun harmless-tools-for-session (session)
  "Return the tools offered to SESSION.
A detached chat can use global memory, ask the user a question,
start a subagent, run a workflow, message another live session,
search the web, fetch a page, look up X, read MCP resources, and
see the screen, click, type, and open a browser.  It is not
offered project files, a project shell, or plan mode.  A
subagent is not offered `spawn_agent' or `run_workflow'."
  (let ((tools
         (if (not (harmless-session-detached-p session))
             (harmless-tools-enabled)
           (let ((allowed '("ask_user"
                            "browser_open"
                            "computer_click"
                            "computer_key"
                            "computer_screenshot"
                            "computer_type"
                            "list_sessions"
                            "memory_read"
                            "memory_remember"
                            "memory_write_topic"
                            "send_message"
                            "spawn_agent"
                            "run_workflow"
                            "web_fetch"
                            "web_search"
                            "x_keyword_search"
                            "x_thread_fetch"
                            "x_user_search"
                            "mcp_list_resources"
                            "mcp_read_resource")))
             (seq-filter (lambda (tool)
                           (member (harmless-tool-name tool) allowed))
                         (harmless-tools-enabled))))))
    (when (fboundp 'harmless-mcp-tools)
      (setq tools (append tools (harmless-mcp-tools session))))
    (if (fboundp 'harmless-agent-filter-tools)
        (harmless-agent-filter-tools session tools)
      tools)))

(defun harmless-turn--messages (session)
  "Return the provider messages for SESSION, including agent context.
A workflow catalog is included for a session that is not a subagent."
  (let ((messages
         (harmless-context-append
          (harmless-context-messages
           (harmless-session-cwd session)
           (harmless-session-messages session)
           nil
           (harmless-session-model session))
          (when (harmless-session-plan-mode session)
            (harmless-plan-instructions)))))
    (setq messages
          (if (fboundp 'harmless-agent-context-suffix)
              (harmless-agent-context-suffix session messages)
            messages))
    (if (fboundp 'harmless-workflow-context-suffix)
        (harmless-workflow-context-suffix session messages)
      messages)))

(defun harmless-turn--call (session)
  "Send SESSION messages to the provider."
  (when (fboundp 'harmless-message-flush)
    (harmless-message-flush session))
  (if (and (fboundp 'harmless-agent-limited-p)
           (harmless-agent-limited-p session))
      (harmless-agent-stop-for-limit session)
    (when (fboundp 'harmless-agent-count-turn)
      (harmless-agent-count-turn session))
    (let ((acc (harmless-turn-acc-create))
          (harmless-current-model (harmless-session-model session))
          (harmless-current-reasoning-effort
           (harmless-session-effective-reasoning-effort session)))
      (harmless-session-set-status session 'streaming)
      (let ((proc (harmless-provider-complete
                   (harmless-session-provider session)
                   (harmless-turn--messages session)
                   (harmless-tools-for-session session)
                   (lambda (event)
                     (harmless-turn--on-event session acc event)))))
        (setf (harmless-session-process session) proc)
        proc))))

(defun harmless-turn--on-event (session acc event)
  "Handle one canonical EVENT for SESSION, accumulating into ACC.
Events that arrive after an abort are ignored."
  (unless (or (harmless-turn-acc-finished acc)
              (not (eq (harmless-session-status session) 'streaming)))
  (harmless-emit session event)
  (pcase event
    (`(:text ,s)
     (setf (harmless-turn-acc-text acc)
           (concat (harmless-turn-acc-text acc) s)))
    (`(:reasoning ,s)
     (setf (harmless-turn-acc-reasoning acc)
           (concat (harmless-turn-acc-reasoning acc) s)))
    (`(:tool-call ,id ,name ,delta)
     (let ((entry (harmless-turn--acc-tool acc (or id (format "call-%s" (random)))
                                           name)))
       (when name
         (setf (plist-get entry :name) name))
       (when (stringp delta)
         (setf (plist-get entry :args)
               (concat (plist-get entry :args) delta)))))
    (`(:usage ,p ,c)
     ;; Keep the latest snapshot.  A stream can repeat a running total.
     (setf (harmless-turn-acc-prompt-tokens acc) p
           (harmless-turn-acc-completion-tokens acc) c))
    (`(:limits ,plist)
     (harmless-usage-record-limits
      (harmless-session-provider-name session)
      plist))
    (`(:error ,err)
     (setf (harmless-turn-acc-finished acc) t)
     (when (fboundp 'harmless-agent-note-error)
       (harmless-agent-note-error session (format "%s" (or err ""))))
     (harmless-session-set-status session 'error)
     (harmless-log "turn error: %s" err))
    (`(:stop ,reason)
     (setf (harmless-turn-acc-stop-reason acc) reason
           (harmless-turn-acc-finished acc) t)
     (setf (harmless-session-process session) nil)
     (when (or (harmless-turn-acc-prompt-tokens acc)
               (harmless-turn-acc-completion-tokens acc))
       (harmless-usage-note-turn
        session
        (or (harmless-turn-acc-prompt-tokens acc) 0)
        (or (harmless-turn-acc-completion-tokens acc) 0)))
     (harmless-turn--after-stop session acc)))))

(defun harmless-turn--tool-calls (acc)
  "Return completed tool-call plists from ACC in order."
  (cl-loop for id in (harmless-turn-acc-order acc)
           collect (gethash id (harmless-turn-acc-tools acc))))

(defun harmless-turn--after-stop (session acc)
  "Finish the assistant message on SESSION and maybe run tools."
  (let ((calls (harmless-turn--tool-calls acc))
        (text (harmless-turn-acc-text acc))
        (reasoning (harmless-turn-acc-reasoning acc)))
    (cond
     ((and (null calls)
           (string-empty-p (string-trim (or text "")))
           (string-empty-p (string-trim (or reasoning ""))))
      (if (harmless-turn--take-steers session)
          (harmless-turn--call session)
        (when (fboundp 'harmless-agent-note-error)
          (harmless-agent-note-error
           session "The model returned an empty reply."))
        (harmless-session-set-status session 'error)
        (harmless-emit session '(:error "The model returned an empty reply."))))
     (t
      (harmless-session-append
       session
       (nconc (list :role :assistant :content text)
              (and (not (string-empty-p reasoning))
                   (list :reasoning reasoning))
              (and calls (list :tool-calls calls))))
      (if calls
          (harmless-turn--run-tools
           session calls
           (lambda ()
             (harmless-turn--take-steers session)
             (harmless-turn--call session)))
        (if (harmless-turn--take-steers session)
            (harmless-turn--call session)
          (harmless-session-set-status session 'idle)))))))

(defun harmless-turn--run-tools (session calls k)
  "Start CALLS together on SESSION, then call K when every one has finished.
Results are appended in CALLS order.  Permission is still one question
at a time: an allowed call starts before the next question."
  (if (null calls)
      (funcall k)
    (let ((batch (harmless-turn-batch-create
                  :session session
                  :calls calls
                  :results (make-vector (length calls) nil)
                  :ready (make-vector (length calls) nil)
                  :left (length calls)
                  :continuation k)))
      (setf (harmless-session-tool-batch session) batch)
      (harmless-turn--launch batch))))

(defun harmless-turn--class (tool session args)
  "Return the permission class for TOOL with ARGS on SESSION.
A tool's `classify' function may narrow one shell command to `read'."
  (let ((classify (harmless-tool-classify tool)))
    (if (functionp classify)
        (funcall classify session args)
      (harmless-tool-class tool))))

(defun harmless-turn--item-for (tool args)
  "Short description of TOOL with ARGS for a permission prompt."
  (pcase (harmless-tool-class tool)
    ('edit (or (harmless-tool-arg args :path) (harmless-tool-name tool)))
    ('shell (or (harmless-tool-arg args :command)
                (let ((text (harmless-tool-arg args :text)))
                  (and (stringp text) text))
                (let ((url (harmless-tool-arg args :url)))
                  (and (stringp url) url))
                (let ((key (harmless-tool-arg args :key)))
                  (and (stringp key) key))
                (let ((x (harmless-tool-arg args :x))
                      (y (harmless-tool-arg args :y)))
                  (and (integerp x) (integerp y)
                       (format "%d,%d" x y)))
                (harmless-tool-name tool)))
    ('network (or (harmless-tool-arg args :query)
                  (harmless-tool-arg args :url)
                  (harmless-tool-arg args :post_id)
                  (harmless-tool-arg args :uri)
                  (harmless-tool-arg args :server)
                  (harmless-tool-name tool)))
    (_ (harmless-tool-name tool))))

(defun harmless-turn--invoke (tool session args callback)
  "Invoke TOOL and call CALLBACK with the string result.
An async tool receives CALLBACK.  A tool that signals still calls
CALLBACK with an Error string."
  (if (harmless-tool-async tool)
      (condition-case err
          (funcall (harmless-tool-fn tool) session args callback)
        (error (funcall callback
                        (format "Error: %s" (error-message-string err)))))
    (funcall callback (harmless-tool-call tool session args))))

(defun harmless-turn--tool-message (call content)
  "Return the tool-result message for CALL and CONTENT."
  (list :role :tool
        :id (plist-get call :id)
        :name (plist-get call :name)
        :content content))

(defun harmless-turn--flush (batch)
  "Append BATCH results that are ready, in call order."
  (let ((session (harmless-turn-batch-session batch))
        (n (length (harmless-turn-batch-calls batch))))
    (while (and (< (harmless-turn-batch-flushed batch) n)
                (aref (harmless-turn-batch-ready batch)
                      (harmless-turn-batch-flushed batch)))
      (let* ((i (harmless-turn-batch-flushed batch))
             (message (aref (harmless-turn-batch-results batch) i)))
        (harmless-session-append session message)
        (harmless-emit session (list :tool-result
                                     (plist-get message :id)
                                     (plist-get message :name)
                                     (plist-get message :content)))
        (setf (harmless-turn-batch-flushed batch) (1+ i))))))

(defun harmless-turn--maybe-finish (batch)
  "Call BATCH's continuation once every call has been recorded."
  (let ((session (harmless-turn-batch-session batch)))
    (when (and (not (harmless-turn-batch-launching batch))
               (not (harmless-turn-batch-waiting-perm batch))
               (not (harmless-turn-batch-aborted batch))
               (not (harmless-turn-batch-finishing batch))
               (<= (harmless-turn-batch-left batch) 0)
               (eq (harmless-session-tool-batch session) batch))
      (setf (harmless-turn-batch-finishing batch) t)
      (setf (harmless-session-tool-batch session) nil)
      (funcall (harmless-turn-batch-continuation batch)))))

(defun harmless-turn--finish-call (batch index message)
  "Record MESSAGE as call INDEX in BATCH."
  (when (and batch
             (not (harmless-turn-batch-aborted batch))
             (not (aref (harmless-turn-batch-ready batch) index)))
    (aset (harmless-turn-batch-results batch) index message)
    (aset (harmless-turn-batch-ready batch) index t)
    (setf (harmless-turn-batch-left batch)
          (1- (harmless-turn-batch-left batch)))
    (harmless-turn--flush batch)
    (harmless-turn--maybe-finish batch)))

(defun harmless-turn--launch (batch)
  "Start every not-yet-started call in BATCH.
Stops at the first permission question.  A synchronous confirmer runs
inside this loop, so it must not start the loop again."
  (unless (harmless-turn-batch-aborted batch)
    (setf (harmless-turn-batch-launching batch) t)
    (unwind-protect
        (let ((calls (harmless-turn-batch-calls batch)))
          (while (and (< (harmless-turn-batch-next batch) (length calls))
                      (not (harmless-turn-batch-aborted batch))
                      (not (harmless-turn-batch-waiting-perm batch)))
            (harmless-turn--start-one batch (harmless-turn-batch-next batch))
            (setf (harmless-turn-batch-next batch)
                  (1+ (harmless-turn-batch-next batch)))))
      (setf (harmless-turn-batch-launching batch) nil))
    (harmless-turn--maybe-finish batch)))

(defun harmless-turn--tool (session name)
  "Return the tool named NAME for SESSION.
A dynamic MCP tool is found on SESSION.  Built-in tools come
from the registry."
  (or (and (fboundp 'harmless-mcp-tool-by-name)
           (harmless-mcp-tool-by-name session name))
      (harmless-tool-by-name name)))

(defun harmless-turn--start-one (batch index)
  "Start call INDEX in BATCH."
  (let* ((session (harmless-turn-batch-session batch))
         (call (nth index (harmless-turn-batch-calls batch)))
         (args (harmless-tool-parse-args (plist-get call :args)))
         (tool (harmless-turn--tool session (plist-get call :name))))
    (cond
     ((harmless-turn-batch-aborted batch) nil)
     ((not tool)
      (harmless-turn--finish-call
       batch index
       (harmless-turn--tool-message
        call (format "Error: unknown tool %s" (plist-get call :name)))))
     (t
      (let* ((class (harmless-turn--class tool session args))
             (command (and (string= (harmless-tool-name tool) "run_shell")
                           (harmless-tool-arg args :command)))
             (agent-refusal
              (and (fboundp 'harmless-agent-refuse-call)
                   (harmless-agent-refuse-call session tool args)))
             (plan-refusal
              (and (not agent-refusal)
                   (or (and (harmless-shell-plan-refuses-p session command)
                            harmless-shell-plan-refusal)
                       (and (fboundp 'harmless-agent-plan-refuses-p)
                            (harmless-agent-plan-refuses-p
                             session tool args))
                       (and (fboundp 'harmless-workflow-plan-refuses-p)
                            (harmless-workflow-plan-refuses-p
                             session tool args))
                       (and (fboundp 'harmless-worktree-plan-refuses-p)
                            (harmless-worktree-plan-refuses-p
                             session tool args))
                       (and (fboundp 'harmless-computer-plan-refuses-p)
                            (harmless-computer-plan-refuses-p
                             session tool args))
                       (and (fboundp 'harmless-monitor-plan-refuses-p)
                            (harmless-monitor-plan-refuses-p
                             session tool args))))))
        (if (or agent-refusal plan-refusal)
            (harmless-turn--finish-call
             batch index
             (harmless-turn--tool-message
              call (or agent-refusal plan-refusal)))
          (setf (harmless-turn-batch-waiting-perm batch) t)
          (harmless-perm-confirm
           session
           class
           (harmless-turn--item-for tool args)
           (lambda (decision)
             (setf (harmless-turn-batch-waiting-perm batch) nil)
             (cond
              ((harmless-turn-batch-aborted batch) nil)
              ((eq decision 'deny)
               (harmless-turn--finish-call
                batch index
                (harmless-turn--tool-message
                 call "Error: user denied this tool call")))
              (t
               (harmless-session-set-status session 'streaming)
               (harmless-turn--invoke
                tool session args
                (lambda (result)
                  (harmless-turn--finish-call
                   batch index
                   (harmless-turn--tool-message call result))))))
             (unless (or (harmless-turn-batch-launching batch)
                         (harmless-turn-batch-aborted batch))
               (harmless-turn--launch batch))))))))))

(provide 'harmless-turn)

;;; harmless-turn.el ends here
