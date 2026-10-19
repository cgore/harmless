;;; harmless-hook.el --- Block a tool call or replace its result -*- lexical-binding: t; -*-

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
;; `harmless-hooks' runs around a tool call.  A pre hook can block the
;; call.  A post hook can replace the text the model sees.  The tool's
;; own files stay as the tool left them.  A hook is a function, a
;; command, an HTTP request, an MCP call, a prompt, or a subagent.
;; The list is not saved on the session.  Plan mode, a deny rule, and
;; the dangerous-command list still refuse first.  A hook cannot skip
;; the permission question.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'harmless-util)
(require 'harmless-provider)
(require 'harmless-session)
(require 'harmless-tools)

(declare-function harmless-agent-create "harmless-agent"
                  (session args callback))
(declare-function harmless-mcp--find "harmless-mcp" (session name))
(declare-function harmless-mcp--format-call "harmless-mcp" (result))

(defvar harmless-mcp-call-function)

(defconst harmless-hook-events '(pre post)
  "Hook events.  `pre' can block.  `post' can replace.")

(defconst harmless-hook-types
  '(function command http mcp prompt agent)
  "Ways a hook can run.")

(defcustom harmless-hooks nil
  "Hooks that run around a tool call.
A list of plists.  Not saved on the session.  Each hook has
:event `pre' or `post', an optional :tool name (nil matches every
tool), and :type `function', `command', `http', `mcp', `prompt',
or `agent'.  A function hook has :function.  A command hook has
:command.  An HTTP hook has :url.  An MCP hook has :server and
:method.  Prompt and agent hooks may set :prompt.
A pre hook blocks only when it says so.  The model then sees
\"Error: hook blocked this tool call\" or that text, a colon, and
a reason.  The tool does not run.  A post hook's replacement is
the result the model sees.  Nil and an empty list do nothing."
  :type '(repeat sexp)
  :group 'harmless)

(defvar harmless-hook-running nil
  "Non-nil while a hook chain is running.
A nested call does not run hooks again and does not block.")

(defvar harmless-hook-command-function #'harmless-hook-command-default
  "Function (HOOK PAYLOAD CALLBACK) that runs a command hook.
CALLBACK receives a plist (:status N :stdout S :stderr E).
Bind this in tests.  \"/\", \"c:/\", nil, and \"\" are not run.")

(defvar harmless-hook-http-function #'harmless-hook-http-default
  "Function (HOOK PAYLOAD CALLBACK) that runs an HTTP hook.
CALLBACK receives a plist (:status N :stdout S :stderr E).
Bind this in tests.  Only an http or https URL is fetched.")

(defvar harmless-hook-mcp-function #'harmless-hook-mcp-default
  "Function (SESSION HOOK PAYLOAD CALLBACK) that runs an MCP hook.
CALLBACK receives the server's text, or a plist (:error TEXT).
Bind this in tests.")

(defvar harmless-hook-prompt-function #'harmless-hook-prompt-default
  "Function (SESSION HOOK PAYLOAD CALLBACK) that runs a prompt hook.
CALLBACK receives the model's text.  The model is given no tools.
Bind this in tests.")

(defvar harmless-hook-agent-function #'harmless-hook-agent-default
  "Function (SESSION HOOK PAYLOAD CALLBACK) that runs an agent hook.
CALLBACK receives the child's text.  The child is one reply with
no tools.  This does not call `harmless-turn-run'.  Bind this in
tests.")

(defun harmless-hook-url-p (url)
  "Return non-nil if URL is an http or https URL.
Nil, \"\", \"/\", \"c:/\", a drive root, and a relative path are
not URLs.  URL is not expanded and is not fetched."
  (and (stringp url)
       (not (harmless-hook--root-text-p (string-trim url)))
       (string-match-p "\\`https?://[^[:space:]]+\\'" url)))

(defun harmless-hook--root-text-p (text)
  "Return non-nil if TEXT is \"/\" or a drive root.
TEXT is not expanded.  A relative path and \"~\" are not roots."
  (and (stringp text)
       (or (equal text "/")
           (string-match-p "\\`[A-Za-z]:[/\\]\\'" text)
           (and (file-name-absolute-p text)
                (not (string-prefix-p "~" text))
                (harmless-filesystem-root-p text)))))

(defun harmless-hook--command (command)
  "Return COMMAND, or signal when it must not run.
Nil, \"\", and whitespace signal \"hook command is missing\".
\"/\", \"c:/\", and a drive root signal \"hook command is not a
command\".  The text is not expanded."
  (cond
   ((not (stringp command)) (error "hook command is missing"))
   (t
    (setq command (string-trim command))
    (cond
     ((string-empty-p command) (error "hook command is missing"))
     ((harmless-hook--root-text-p command)
      (error "hook command is not a command"))
     (t command)))))

(defun harmless-hook--server (name)
  "Return the MCP server NAME, or signal.
Nil, \"\", \"/\", and \"c:/\" signal \"hook server is missing\".
The text is not expanded and no server file is read."
  (cond
   ((not (stringp name)) (error "hook server is missing"))
   (t
    (setq name (string-trim name))
    (when (or (string-empty-p name)
              (harmless-hook--root-text-p name))
      (error "hook server is missing"))
    name)))

(defun harmless-hook--method (name)
  "Return the MCP method NAME, or signal \"hook method is missing\"."
  (cond
   ((not (stringp name)) (error "hook method is missing"))
   (t
    (setq name (string-trim name))
    (when (string-empty-p name)
      (error "hook method is missing"))
    name)))

(defun harmless-hook-validate-one (hook)
  "Signal unless HOOK is one hook plist.  Return HOOK."
  (unless (and (consp hook) (keywordp (car hook)))
    (error "hook must be a hook"))
  (let ((event (plist-get hook :event))
        (type (plist-get hook :type))
        (tool (plist-get hook :tool)))
    (unless (memq event harmless-hook-events)
      (error "hook event is not an event"))
    (unless (memq type harmless-hook-types)
      (error "hook type is not a type"))
    (when (and tool (not (stringp tool)))
      (error "hook tool is not a name"))
    (pcase type
      ('function
       (unless (functionp (plist-get hook :function))
         (error "hook function is missing")))
      ('command
       (harmless-hook--command (plist-get hook :command)))
      ('http
       (unless (harmless-hook-url-p (plist-get hook :url))
         (error "hook url is not a url")))
      ('mcp
       (harmless-hook--server (plist-get hook :server))
       (harmless-hook--method (plist-get hook :method)))
      ('prompt nil)
      ('agent nil)))
  hook)

(defun harmless-hook-validate (hooks)
  "Signal unless HOOKS is a list of hooks.  Return HOOKS.
Nil is an empty list.  A string, \"/\", and a dotted pair signal
\"hooks must be a list\"."
  (unless (and (listp hooks) (proper-list-p hooks))
    (error "hooks must be a list"))
  (dolist (hook hooks)
    (harmless-hook-validate-one hook))
  hooks)

(defun harmless-hook--parse-json (text)
  "Return a directive plist parsed from JSON TEXT, or nil."
  (let ((obj (harmless-json-decode-safe text)))
    (when (harmless-plist-p obj)
      (let* ((decision (plist-get obj :decision))
             (reason (plist-get obj :reason))
             (replace (plist-get obj :replace))
             (out nil))
        (when (and (stringp decision)
                   (string= (downcase (string-trim decision)) "block"))
          (setq out (plist-put out :block
                               (if (and (stringp reason)
                                        (not (string-blank-p reason)))
                                   (string-trim reason)
                                 nil))))
        (when (stringp replace)
          (setq out (plist-put out :replace replace)))
        out))))

(defun harmless-hook--replace-body (tail rest)
  "Return the replacement made of TAIL and the remaining lines REST."
  (string-trim
   (mapconcat #'identity (cons (or tail "") rest) "\n")))

(defun harmless-hook--parse-lines (text)
  "Return a directive plist from whole lines in TEXT, or nil.
The first line that is block or replace: wins.  A word inside a
sentence does not."
  (let ((lines (split-string text "\n"))
        (case-fold-search t)
        (found nil))
    (while (and lines (not found))
      (let ((line (string-trim (car lines))))
        (cond
         ((string-match
           "\\`block\\(?:[ \t]*:[ \t]*\\(.*\\)\\)?\\'" line)
          (let ((reason (match-string 1 line)))
            (setq found
                  (list :block
                        (if (and reason (not (string-blank-p reason)))
                            (string-trim reason)
                          nil)))))
         ((string-match "\\`replace[ \t]*:[ \t]*\\(.*\\)\\'" line)
          (setq found
                (list :replace
                      (harmless-hook--replace-body
                       (match-string 1 line) (cdr lines)))))))
      (setq lines (cdr lines)))
    found))

(defun harmless-hook-parse-reply (text)
  "Return a directive plist parsed from TEXT, or nil.
JSON `decision' block and `replace', or a whole line `block' or
`replace:', count.  Both keys may be present.  Nil, \"\",
whitespace, \"/\", \"c:/\", and any other reply do nothing."
  (when (and (stringp text) (not (string-blank-p text)))
    (let ((trim (string-trim text)))
      (if (string-prefix-p "{" trim)
          (harmless-hook--parse-json trim)
        (harmless-hook--parse-lines trim)))))

(defun harmless-hook--function-directive (value)
  "Return a directive plist for a function hook's VALUE, or nil.
A string with no block or replace line is an exact replacement.
`allow', nil, and \"\" do nothing.  `block' blocks."
  (cond
   ((or (null value) (eq value 'observe) (eq value 'allow)) nil)
   ((eq value 'block) (list :block nil))
   ((and (stringp value) (string-blank-p value)) nil)
   ((stringp value)
    (or (harmless-hook-parse-reply value)
        (list :replace value)))
   ((and (listp value) (keywordp (car-safe value)))
    (cond
     ((or (eq (plist-get value :decision) 'block)
          (equal (plist-get value :decision) "block"))
      (list :block (plist-get value :reason)))
     ((stringp (plist-get value :replace))
      (list :replace (plist-get value :replace)))
     (t nil)))
   (t nil)))

(defun harmless-hook--failure-reason (raw)
  "Return the block reason for a failed process reply RAW."
  (let ((stderr (plist-get raw :stderr))
        (stdout (plist-get raw :stdout)))
    (cond
     ((and (stringp stderr) (not (string-blank-p stderr)))
      (string-trim stderr))
     ((and (stringp stdout) (not (string-blank-p stdout)))
      (string-trim stdout))
     (t "hook failed"))))

(defun harmless-hook--process-directive (event raw)
  "Return a directive for a command or HTTP reply RAW.
EVENT is `pre' or `post'.  A non-zero status blocks on pre and
is ignored on post.  Otherwise only an explicit reply counts."
  (cond
   ((stringp raw) (harmless-hook-parse-reply raw))
   ((and (listp raw) (keywordp (car-safe raw)))
    (let* ((status (plist-get raw :status))
           (stdout (plist-get raw :stdout))
           (parsed (harmless-hook-parse-reply
                    (if (stringp stdout) stdout ""))))
      (cond
       ((and (numberp status) (not (zerop status)))
        (when (eq event 'pre)
          (list :block (harmless-hook--failure-reason raw))))
       (parsed parsed)
       (t nil))))
   (t nil)))

(defun harmless-hook--directive (hook event raw)
  "Return HOOK's directive for RAW during EVENT, or nil.
A plist (:error TEXT) blocks on pre and does nothing on post."
  (cond
   ((and (listp raw)
         (keywordp (car-safe raw))
         (plist-member raw :error))
    (when (eq event 'pre)
      (list :block (plist-get raw :error))))
   ((eq (plist-get hook :type) 'function)
    (harmless-hook--function-directive raw))
   ((memq (plist-get hook :type) '(command http))
    (harmless-hook--process-directive event raw))
   ((stringp raw) (harmless-hook-parse-reply raw))
   ((and (listp raw)
         (keywordp (car-safe raw))
         (stringp (plist-get raw :stdout)))
    (harmless-hook-parse-reply (plist-get raw :stdout)))
   (t nil)))

(defun harmless-hook-block-message (reason)
  "Return the tool error for a hook block of REASON.
A blank reason omits the colon suffix."
  (if (and (stringp reason) (not (string-blank-p reason)))
      (format "Error: hook blocked this tool call: %s"
              (string-trim reason))
    "Error: hook blocked this tool call"))

(defun harmless-hook--tool-name (tool)
  "Return TOOL's name, or \"\"."
  (cond
   ((and (harmless-tool-p tool)) (harmless-tool-name tool))
   ((stringp tool) tool)
   (t "")))

(defun harmless-hook--value (value)
  "Return VALUE as one token in a hook prompt.
A string is quoted.  Nil is the word nil.  `:false' is false.
The text is not resolved as a path."
  (cond
   ((null value) "nil")
   ((eq value :false) "false")
   ((stringp value) (format "%S" value))
   ((or (symbolp value) (numberp value)) (format "%s" value))
   (t (format "%S" value))))

(defun harmless-hook--args-text (args)
  "Return ARGS as one line of hook prompt text.
Nil is the word none.  A plist keeps its order."
  (cond
   ((null args) "none")
   ((stringp args) args)
   ((and (listp args) (keywordp (car-safe args)))
    (let (parts)
      (while args
        (push (format "%s=%s"
                      (substring (symbol-name (car args)) 1)
                      (harmless-hook--value (cadr args)))
              parts)
        (setq args (cddr args)))
      (mapconcat #'identity (nreverse parts) " ")))
   (t (format "%S" args))))

(defun harmless-hook-payload (event tool args text)
  "Return the plist passed to a command, HTTP, or MCP hook.
EVENT is `pre' or `post'.  TEXT is the current result on post.
Arguments are copied as text and are not resolved as paths."
  (list :event (symbol-name event)
        :tool (harmless-hook--tool-name tool)
        :arguments args
        :result (if (stringp text) text nil)))

(defun harmless-hook-prompt-text (payload)
  "Return the user text that describes PAYLOAD.
Nil arguments are the word none.  \"/\" and \"c:/\" stay text."
  (concat "Event: " (or (plist-get payload :event) "")
          "\nTool: " (or (plist-get payload :tool) "")
          "\nArguments: "
          (harmless-hook--args-text (plist-get payload :arguments))
          (if (equal (plist-get payload :event) "post")
              (concat "\nResult: "
                      (let ((text (plist-get payload :result)))
                        (if (stringp text) text "none")))
            "")))

(defun harmless-hook--instructions ()
  "Return the reply rules for a prompt or agent hook."
  (concat "Reply with one line block: REASON to block the tool, "
          "or replace: TEXT to replace the result. "
          "Any other reply does nothing. Do not call a tool."))

(defun harmless-hook--prompt-body (hook payload)
  "Return the user message for HOOK and PAYLOAD."
  (let ((extra (plist-get hook :prompt)))
    (concat
     (if (and (stringp extra) (not (string-blank-p extra)))
         (concat (string-trim extra) "\n")
       "")
     (harmless-hook-prompt-text payload)
     "\n"
     (harmless-hook--instructions))))

(defun harmless-hook--shell ()
  "Return (PROGRAM ARG) used to run one hook command."
  (if (memq system-type '(windows-nt ms-dos))
      '("cmd.exe" "/c")
    (list (or (and (stringp shell-file-name)
                   (not (string-empty-p shell-file-name))
                   shell-file-name)
              "sh")
          "-c")))

(defun harmless-hook-command-default (hook payload callback)
  "Run HOOK's command with PAYLOAD on stdin, then call CALLBACK.
CALLBACK receives (:status N :stdout S :stderr E).  A root path
is not a command and is not run."
  (let ((command (harmless-hook--command (plist-get hook :command)))
        (stdout (generate-new-buffer " *harmless-hook-out*"))
        (stderr (generate-new-buffer " *harmless-hook-err*"))
        (program (harmless-hook--shell)))
    (unwind-protect
        (let (status out err)
          (with-temp-buffer
            (insert (harmless-json-text payload))
            (setq status
                  (apply #'call-process-region
                         (point-min) (point-max)
                         (car program) nil
                         (list stdout stderr) nil
                         (append (cdr program) (list command)))))
          (setq out (with-current-buffer stdout (buffer-string)))
          (setq err (with-current-buffer stderr (buffer-string)))
          (funcall
           callback
           (if (numberp status)
               (list :status status :stdout out :stderr err)
             (list :status 1 :stdout out
                   :stderr (if (stringp status) status err)))))
      (when (buffer-live-p stdout) (kill-buffer stdout))
      (when (buffer-live-p stderr) (kill-buffer stderr)))))

(defun harmless-hook-http-default (hook payload callback)
  "POST PAYLOAD to HOOK's URL, then call CALLBACK.
CALLBACK receives (:status N :stdout S :stderr E).  A URL that
is not http or https is not fetched."
  (let ((url (plist-get hook :url)))
    (unless (harmless-hook-url-p url)
      (error "hook url is not a url"))
    (let* ((url-request-method "POST")
           (url-request-extra-headers
            '(("Content-Type" . "application/json")))
           (url-request-data
            (encode-coding-string
             (harmless-json-text payload) 'utf-8))
           (buffer (url-retrieve-synchronously url t t 5)))
      (unless (buffer-live-p buffer)
        (error "hook url failed"))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (let ((status 0)
                  (body ""))
              (when (re-search-forward
                     "HTTP/[0-9.]+ \\([0-9][0-9][0-9]\\)" nil t)
                (setq status (string-to-number (match-string 1))))
              (when (re-search-forward "\r?\n\r?\n" nil t)
                (setq body (buffer-substring-no-properties
                            (point) (point-max))))
              (funcall callback
                       (list :status status :stdout body :stderr ""))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(defun harmless-hook-mcp-default (session hook payload callback)
  "Call HOOK's MCP method with PAYLOAD, then call CALLBACK.
A missing server signals before any call.  \"/\" and \"c:/\" are
not server names."
  (let ((name (harmless-hook--server (plist-get hook :server)))
        (method (harmless-hook--method (plist-get hook :method))))
    (unless (fboundp 'harmless-mcp--find)
      (error "hook server is missing"))
    (let ((server (funcall 'harmless-mcp--find session name)))
      (unless (and (boundp 'harmless-mcp-call-function)
                   (functionp harmless-mcp-call-function))
        (error "MCP call failed"))
      (let ((result
             (funcall harmless-mcp-call-function
                      server "tools/call"
                      (list :name method
                            :arguments
                            (list :event (plist-get payload :event)
                                  :tool (plist-get payload :tool)
                                  :arguments
                                  (plist-get payload :arguments)
                                  :result
                                  (plist-get payload :result))))))
        (unless (fboundp 'harmless-mcp--format-call)
          (error "MCP call returned no text"))
        (funcall callback
                 (funcall 'harmless-mcp--format-call result))))))

(defun harmless-hook--complete (provider messages callback)
  "Ask PROVIDER to complete MESSAGES with no tools.
CALLBACK receives the text, or a plist (:error TEXT) on failure.
A tool call is ignored.  CALLBACK is called once."
  (let ((text "")
        (done nil))
    (harmless-provider-complete
     provider messages nil
     (lambda (event)
       (unless done
         (pcase event
           (`(:text ,chunk)
            (when (stringp chunk)
              (setq text (concat text chunk))))
           (`(:error ,message)
            (setq done t)
            (funcall callback
                     (list :error
                           (if (and (stringp message)
                                    (not (string-blank-p message)))
                               (string-trim message)
                             "hook prompt failed"))))
           (`(:stop ,_)
            (setq done t)
            (funcall callback text))))))))

(defun harmless-hook-prompt-default (session hook payload callback)
  "Ask SESSION's model about PAYLOAD, then call CALLBACK.
The request has no tools.  A missing provider signals
\"hook prompt has no provider\" and does not call CALLBACK.
This does not run a tool."
  (let ((provider (and (harmless-session-p session)
                       (harmless-session-provider session))))
    (unless provider
      (error "hook prompt has no provider"))
    (harmless-hook--complete
     provider
     (list (list :role :user
                 :content (harmless-hook--prompt-body hook payload)))
     callback)))

(defun harmless-hook-agent-default (session hook payload callback)
  "Start one child reply for PAYLOAD, then call CALLBACK.
The child is not given tools and `harmless-turn-run' is not
called.  A missing provider signals \"hook agent has no
provider\".  A child cannot start another child."
  (unless (harmless-session-p session)
    (error "No Harmless session"))
  (unless (harmless-session-provider session)
    (error "hook agent has no provider"))
  (unless (fboundp 'harmless-agent-create)
    (error "hook agent is missing"))
  (let* ((body (harmless-hook--prompt-body hook payload))
         (child (funcall 'harmless-agent-create
                         session
                         (list :prompt body :description "hook")
                         #'ignore)))
    (harmless-hook--complete
     (harmless-session-provider child)
     (list (list :role :user :content body))
     callback)))

(defun harmless-hook--select (hooks event tool)
  "Return HOOKS whose event is EVENT and whose tool matches TOOL.
A hook with no :tool matches every tool.  \"\" matches only an
empty tool name."
  (let ((name (harmless-hook--tool-name tool))
        (out nil))
    (dolist (hook hooks)
      (when (and (eq (plist-get hook :event) event)
                 (let ((want (plist-get hook :tool)))
                   (or (null want)
                       (and (stringp want) (string= want name)))))
        (push hook out)))
    (nreverse out)))

(defun harmless-hook--invoke (hook event session tool args text callback)
  "Run HOOK and call CALLBACK with its raw reply.
EVENT is `pre' or `post'.  TEXT is the current post result."
  (pcase (plist-get hook :type)
    ('function
     (let ((fn (plist-get hook :function)))
       (unless (functionp fn)
         (error "hook function is missing"))
       (funcall callback (funcall fn session tool args text))))
    ('command
     (unless (functionp harmless-hook-command-function)
       (error "hook command is missing"))
     (funcall harmless-hook-command-function
              hook (harmless-hook-payload event tool args text)
              callback))
    ('http
     (unless (harmless-hook-url-p (plist-get hook :url))
       (error "hook url is not a url"))
     (unless (functionp harmless-hook-http-function)
       (error "hook url is not a url"))
     (funcall harmless-hook-http-function
              hook (harmless-hook-payload event tool args text)
              callback))
    ('mcp
     (unless (functionp harmless-hook-mcp-function)
       (error "hook server is missing"))
     (funcall harmless-hook-mcp-function
              session hook
              (harmless-hook-payload event tool args text)
              callback))
    ('prompt
     (unless (functionp harmless-hook-prompt-function)
       (error "hook prompt has no provider"))
     (funcall harmless-hook-prompt-function
              session hook
              (harmless-hook-payload event tool args text)
              callback))
    ('agent
     (unless (functionp harmless-hook-agent-function)
       (error "hook agent has no provider"))
     (funcall harmless-hook-agent-function
              session hook
              (harmless-hook-payload event tool args text)
              callback))
    (_ (error "hook type is not a type"))))

(defun harmless-hook--one (hook event session tool args text callback)
  "Run HOOK and call CALLBACK with a directive plist, or nil.
A pre error becomes a block.  A post error becomes nil.  An
error raised by CALLBACK is not turned into a block."
  (let ((called nil))
    (condition-case err
        (harmless-hook--invoke
         hook event session tool args text
         (lambda (raw)
           (setq called t)
           (funcall callback
                    (condition-case err2
                        (harmless-hook--directive hook event raw)
                      (error
                       (if (eq event 'pre)
                           (list :block (error-message-string err2))
                         nil))))))
      (error
       (when called
         (signal (car err) (cdr err)))
       (funcall callback
                (if (eq event 'pre)
                    (list :block (error-message-string err))
                  nil))))))

(defun harmless-hook--chain (hooks event session tool args text callback)
  "Run HOOKS in order and call CALLBACK once.
Pre stops at the first block.  Post threads the replaced text."
  (if (null hooks)
      (funcall callback (if (eq event 'pre) nil text))
    (harmless-hook--one
     (car hooks) event session tool args text
     (lambda (directive)
       (cond
        ((eq event 'pre)
         (if (and directive (plist-member directive :block))
             (funcall callback
                      (harmless-hook-block-message
                       (plist-get directive :block)))
           (harmless-hook--chain
            (cdr hooks) event session tool args text callback)))
        (t
         (when (and directive
                    (stringp (plist-get directive :replace)))
           (setq text (plist-get directive :replace)))
         (harmless-hook--chain
          (cdr hooks) event session tool args text callback)))))))

(defun harmless-hook--dispatch (event session tool args text callback)
  "Run EVENT hooks and call CALLBACK once.
Pre passes nil or a block string.  Post passes the result text.
A nested call does not run hooks again."
  (if harmless-hook-running
      (funcall callback (if (eq event 'post) text nil))
    (setq harmless-hook-running t)
    (let ((done nil))
      (cl-labels
          ((finish (value)
             (unless done
               (setq done t)
               (setq harmless-hook-running nil)
               (funcall callback value))))
        (let (hooks failed)
          (condition-case err
              (setq hooks
                    (harmless-hook--select
                     (harmless-hook-validate harmless-hooks)
                     event tool))
            (error
             (setq failed
                   (if (eq event 'pre)
                       (harmless-hook-block-message
                        (error-message-string err))
                     text))))
          (if failed
              (finish failed)
            (harmless-hook--chain
             hooks event session tool args text #'finish)))))))

(defun harmless-hook-pre (session tool args callback)
  "Run pre hooks for TOOL and ARGS, then call CALLBACK.
CALLBACK receives nil, or the block error the model should see.
The tool is not run here.  A hook error blocks.  An empty list
does not."
  (harmless-hook--dispatch 'pre session tool args nil callback))

(defun harmless-hook-post (session tool args text callback)
  "Run post hooks for TEXT, then call CALLBACK with the result.
TEXT is the tool's own result.  A string replacement is what the
model sees.  A hook error keeps the text it was given."
  (harmless-hook--dispatch 'post session tool args text callback))

(provide 'harmless-hook)

;;; harmless-hook.el ends here
