;;; harmless-execute.el --- Run Emacs Lisp in a separate Emacs -*- lexical-binding: t; -*-

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
;; `harmless-tool-execute-code' evaluates an Emacs Lisp script in a separate Emacs.
;; The parent listens on 127.0.0.1.  The child connects with
;; HARMLESS_EXECUTE_PORT.  Only what the script prints is returned.
;; A tool result is data for the script and is not added to the transcript.
;; A failed script leaves the interpreter up.  `reset' and a timeout kill it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-tools)

(defgroup harmless-execute nil
  "Run Emacs Lisp in a separate Emacs."
  :group 'harmless)

(defcustom harmless-execute-timeout-seconds 300
  "Seconds one script may run before its interpreter is killed.
Time inside a tool call is counted when that call returns."
  :type 'number
  :group 'harmless-execute)

(defcustom harmless-execute-max-calls 50
  "Tool calls one script may make.
The next script starts the count over."
  :type 'integer
  :group 'harmless-execute)

(defcustom harmless-execute-max-bytes 50000
  "Printed output is cut after this many bytes."
  :type 'integer
  :group 'harmless-execute)

(defconst harmless-execute-startup-seconds 15
  "Seconds to wait for the interpreter to connect.")

(defconst harmless-execute--secret-marks
  '("KEY" "TOKEN" "SECRET" "PASSWORD" "CREDENTIAL" "PASSWD" "AUTH")
  "Substrings that remove an environment name from the interpreter.
PATH has none of these, so PATH is kept.")

(defvar harmless-execute--states (make-hash-table :test 'equal)
  "Session id to the interpreter plist :conn :emacs :buffers.")

(defvar harmless-execute--awaiting nil
  "State plist waiting for its child to connect, or nil.")

(defvar harmless-execute--listener nil
  "The one loopback server children connect to.
Deleting a listen socket on Windows can drop the next connection,
so this listener stays up for the Emacs session.")

(defun harmless-execute--secret-name-p (name)
  "Return non-nil if environment NAME looks like a secret."
  (and (stringp name)
       (cl-some (lambda (mark)
                  (string-match-p (regexp-quote mark) name))
                harmless-execute--secret-marks)))

(defun harmless-execute--env-name (entry)
  "Return the variable name in process-environment ENTRY."
  (if (stringp entry)
      (let ((eq (string-search "=" entry)))
        (if eq (substring entry 0 eq) entry))
    ""))

(defun harmless-execute--environment ()
  "Return `process-environment' without secret names."
  (let (out)
    (dolist (entry process-environment)
      (unless (harmless-execute--secret-name-p
               (harmless-execute--env-name entry))
        (push entry out)))
    (nreverse out)))

(defun harmless-execute--root-p (text)
  "Return non-nil if TEXT is \"/\" or a drive root."
  (or (harmless-filesystem-root-p text)
      (and (stringp text)
           (or (string-match-p "\\`[A-Za-z]:[/\\]?\\'" text)
               (string-match-p "\\`\\\\+\\'" text)))))

(defun harmless-execute--directory (session)
  "Return the working directory for SESSION's interpreter.
A detached chat, a missing directory, and a root use
`temporary-file-directory'."
  (let ((cwd (and (harmless-session-p session)
                  (harmless-session-cwd session))))
    (if (and (stringp cwd)
             (not (harmless-execute--root-p cwd))
             (file-directory-p cwd))
        cwd
      temporary-file-directory)))

(defun harmless-execute--emacs ()
  "Return the running Emacs executable, or nil."
  (let ((path (and (stringp invocation-directory)
                   (stringp invocation-name)
                   (expand-file-name invocation-name invocation-directory))))
    (when (and (stringp path) (file-executable-p path))
      path)))

(defun harmless-execute--child-file ()
  "Return the interpreter script, or nil."
  (let ((lib (locate-library "harmless-execute")))
    (when (and (stringp lib) (not (string-empty-p lib)))
      (let ((file (expand-file-name
                   "harmless-execute-child.el"
                   (file-name-directory lib))))
        (when (and (file-regular-p file) (file-readable-p file))
          file)))))

(defun harmless-execute--blocked-p (name)
  "Return non-nil if a script must not call NAME."
  (and (stringp name)
       (or (member name '("harmless-tool-execute-code" "harmless-tool-spawn-agent" "harmless-tool-run-workflow"))
           (string-prefix-p "harmless-tool-mcp-" name))))

(defun harmless-execute--names ()
  "Return tool names the interpreter may define."
  (let (names)
    (dolist (tool harmless-tools)
      (let ((name (harmless-tool-name tool)))
        (unless (harmless-execute--blocked-p name)
          (push name names))))
    (nreverse names)))

(defun harmless-execute--refusal (name calls)
  "Return a refusal for calling NAME after CALLS tool calls, or nil."
  (cond
   ((not (stringp name))
    "execute code cannot call ")
   ((equal name "harmless-tool-execute-code")
    "execute code cannot call harmless-tool-execute-code")
   ((equal name "harmless-tool-spawn-agent")
    "execute code cannot call harmless-tool-spawn-agent")
   ((equal name "harmless-tool-run-workflow")
    "execute code cannot call harmless-tool-run-workflow")
   ((string-prefix-p "harmless-tool-mcp-" name)
    "execute code cannot call an MCP tool")
   ((null (harmless-tool-by-name name))
    (format "execute code cannot call %s" name))
   ((>= calls harmless-execute-max-calls)
    "execute code tool cap")
   (t nil)))

(defun harmless-execute--refusal-p (message)
  "Return non-nil if MESSAGE is a tool refusal the parent must signal."
  (and (stringp message)
       (or (equal message "execute code tool cap")
           (string-prefix-p "execute code cannot call " message))))

(defun harmless-execute--timeout-seconds (timeout)
  "Return TIMEOUT, or the default when TIMEOUT is nil."
  (cond
   ((null timeout) harmless-execute-timeout-seconds)
   ((and (numberp timeout) (> timeout 0)) timeout)
   (t (error "timeout must be a positive number"))))

(defun harmless-execute--timeout-text (seconds)
  "Return the refusal used when a script exceeds SECONDS."
  (format "Script timed out after %ss and was killed."
          (if (and (numberp seconds) (= seconds (truncate seconds)))
              (truncate seconds)
            seconds)))

(defun harmless-execute--state (session)
  "Return SESSION's interpreter plist, or nil."
  (when (harmless-session-p session)
    (gethash (harmless-session-id session) harmless-execute--states)))

(defun harmless-execute--live-p (state)
  "Return non-nil if STATE's child and socket are still running."
  (and state
       (processp (plist-get state :conn))
       (process-live-p (plist-get state :conn))
       (processp (plist-get state :emacs))
       (process-live-p (plist-get state :emacs))))

(defun harmless-execute-stop (session)
  "Kill SESSION's interpreter.  A missing interpreter is not an error."
  (when (harmless-session-p session)
    (let ((state (harmless-execute--state session)))
      (when state
        (dolist (key '(:emacs :conn))
          (let ((proc (plist-get state key)))
            (when (processp proc)
              (set-process-sentinel proc #'ignore)
              (set-process-filter proc #'ignore)
              (when (process-live-p proc)
                (delete-process proc)))))
        (dolist (buf (plist-get state :buffers))
          (when (buffer-live-p buf)
            (kill-buffer buf)))
        (remhash (harmless-session-id session) harmless-execute--states)
        ;; Let Windows finish closing the sockets before the next listen.
        (accept-process-output nil 0.05))))
  nil)

(defun harmless-execute--ignore-filter (_proc _text)
  "Discard TEXT.  The listening socket does not carry script output."
  nil)

(defun harmless-execute--filter (proc text)
  "Append TEXT to PROC's inbox."
  (process-put proc 'inbox
               (concat (or (process-get proc 'inbox) "") text)))

(defun harmless-execute--server-sentinel (proc message)
  "Attach PROC when MESSAGE says a child connected."
  (when (and (string-prefix-p "open" message)
             (listp harmless-execute--awaiting))
    (set-process-coding-system proc 'utf-8-unix 'utf-8-unix)
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel proc #'ignore)
    (process-put proc 'inbox "")
    (set-process-filter proc #'harmless-execute--filter)
    (let* ((state (plist-put harmless-execute--awaiting :conn proc))
           (id (plist-get state :id)))
      (setq harmless-execute--awaiting state)
      (when (stringp id)
        (puthash id state harmless-execute--states)))))

(defun harmless-execute--note-failure (buffers)
  "Message any text the child left in BUFFERS.  The refusal stays exact."
  (dolist (buf buffers)
    (when (buffer-live-p buf)
      (let ((text (with-current-buffer buf (buffer-string))))
        (unless (string-empty-p (string-trim text))
          (message "harmless execute: %s" text))))))

(defun harmless-execute--wait-for-conn (session)
  "Wait until SESSION's child connects."
  (let ((deadline (+ (float-time) harmless-execute-startup-seconds)))
    (while (and (not (harmless-execute--live-p (harmless-execute--state session)))
                (let ((state (harmless-execute--state session)))
                  (and state
                       (processp (plist-get state :emacs))
                       (process-live-p (plist-get state :emacs))))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (unless (harmless-execute--live-p (harmless-execute--state session))
      (harmless-execute--note-failure
       (plist-get (harmless-execute--state session) :buffers))
      (error "execute code failed: interpreter exited"))))

(defun harmless-execute--listen-port ()
  "Return the loopback port, opening the listener if it is down."
  (unless (and (processp harmless-execute--listener)
               (process-live-p harmless-execute--listener))
    (setq harmless-execute--listener
          (make-network-process
           :name "harmless-execute-listen"
           :server t
           :host "127.0.0.1"
           :service t
           :family 'ipv4
           :coding '(utf-8-unix . utf-8-unix)
           :noquery t
           :filter #'harmless-execute--ignore-filter
           :sentinel #'harmless-execute--server-sentinel))
    (set-process-query-on-exit-flag harmless-execute--listener nil))
  (let ((port (process-contact harmless-execute--listener :service)))
    (unless (integerp port)
      (error "execute code failed: interpreter exited"))
    port))

(defun harmless-execute--start (session)
  "Start SESSION's interpreter and return its state plist."
  (let ((emacs (or (harmless-execute--emacs)
                   (error "execute code needs emacs")))
        (child (or (harmless-execute--child-file)
                   (error "execute code needs emacs")))
        (id (harmless-session-id session))
        (outbuf (generate-new-buffer " *harmless-execute-out*"))
        (errbuf (generate-new-buffer " *harmless-execute-err*"))
        (state nil))
    (condition-case err
        (unwind-protect
            (let ((port (harmless-execute--listen-port)))
              (setq state (list :id id
                                :emacs nil
                                :conn nil
                                :buffers (list outbuf errbuf)))
              (puthash id state harmless-execute--states)
              (setq harmless-execute--awaiting state)
              (let ((proc (let ((process-environment
                                 (cons (format "HARMLESS_EXECUTE_PORT=%d" port)
                                       (harmless-execute--environment)))
                                (default-directory
                                 (harmless-execute--directory session)))
                            (make-process
                             :name "harmless-execute"
                             :command (list emacs "-Q" "--batch" "-l" child)
                             :buffer outbuf
                             :stderr errbuf
                             :coding '(utf-8-unix . utf-8-unix)
                             :noquery t
                             :connection-type 'pipe
                             :sentinel #'ignore))))
                (set-process-query-on-exit-flag proc nil)
                (plist-put state :emacs proc))
              (harmless-execute--wait-for-conn session)
              state)
          (setq harmless-execute--awaiting nil))
      (error
       (harmless-execute-stop session)
       (dolist (buf (list outbuf errbuf))
         (when (buffer-live-p buf)
           (kill-buffer buf)))
       (signal (car err) (cdr err))))))

(defun harmless-execute--ensure (session)
  "Return a live interpreter for SESSION, starting one when needed."
  (let ((state (harmless-execute--state session)))
    (if (harmless-execute--live-p state)
        state
      (when state
        (harmless-execute-stop session))
      (harmless-execute--start session))))

(defun harmless-execute--send (proc plist)
  "Send PLIST to PROC as one JSON line."
  (process-send-string proc (concat (harmless-json-text plist) "\n")))

(defun harmless-execute--reply (proc ok text err)
  "Tell PROC that a tool call finished.
OK non-nil returns TEXT.  Otherwise ERR is raised in the script."
  (harmless-execute--send
   proc
   (list :op "tool_result"
         :ok (if ok t :false)
         :text (if (stringp text) text "")
         :error (if (stringp err) err :null))))

(defun harmless-execute--take-line (proc)
  "Return PROC's next complete line, or nil."
  (let* ((inbox (or (process-get proc 'inbox) ""))
         (pos (string-search "\n" inbox)))
    (when pos
      (process-put proc 'inbox (substring inbox (1+ pos)))
      (let ((line (substring inbox 0 pos)))
        (if (string-suffix-p "\r" line)
            (substring line 0 -1)
          line)))))

(defun harmless-execute--read-line (session proc deadline seconds)
  "Return PROC's next line, or signal when SECONDS runs out."
  (let ((line (harmless-execute--take-line proc)))
    (while (not line)
      (let ((state (harmless-execute--state session)))
        (unless (and (process-live-p proc)
                     state
                     (processp (plist-get state :emacs))
                     (process-live-p (plist-get state :emacs)))
          (error "execute code failed: interpreter exited")))
      (when (> (float-time) deadline)
        (harmless-execute-stop session)
        (error "%s" (harmless-execute--timeout-text seconds)))
      (accept-process-output proc 0.1)
      (setq line (harmless-execute--take-line proc)))
    line))

(defun harmless-execute--truncate (text)
  "Cut TEXT to `harmless-execute-max-bytes' and mark the cut."
  (unless (stringp text)
    (setq text (if (null text) "" (format "%s" text))))
  (if (<= (string-bytes text) harmless-execute-max-bytes)
      text
    (let ((low 0)
          (high (length text)))
      (while (< low high)
        (let ((mid (/ (+ low high 1) 2)))
          (if (<= (string-bytes (substring text 0 mid))
                  harmless-execute-max-bytes)
              (setq low mid)
            (setq high (1- mid)))))
      (concat (substring text 0 low) "\n[truncated]"))))

(defun harmless-execute--one-tool (session proc deadline seconds calls msg)
  "Run the tool MSG asks for.  Return the updated call count.
A refusal is sent back and the count stays put.  The deadline is
not checked while the tool itself is running."
  (when (> (float-time) deadline)
    (harmless-execute-stop session)
    (error "%s" (harmless-execute--timeout-text seconds)))
  (let* ((name (harmless-tool-arg msg :name))
         (args (harmless-tool-arg msg :args))
         (refusal (harmless-execute--refusal name calls)))
    (if refusal
        (progn
          (harmless-execute--reply proc nil "" refusal)
          calls)
      (let* ((tool (harmless-tool-by-name name))
             (text (harmless-tool-call
                    tool session (if (listp args) args nil))))
        (unless (stringp text)
          (setq text (if text (format "%s" text) "")))
        (harmless-execute--reply proc t text nil)
        (1+ calls)))))

(defun harmless-execute--finish-error (message)
  "Signal MESSAGE when it is a refusal, otherwise return it as text."
  (if (harmless-execute--refusal-p message)
      (error "%s" message)
    (concat "execute code failed: " (if (stringp message) message ""))))

(defun harmless-execute--exchange (session code seconds)
  "Send CODE to SESSION's interpreter and return the printed text."
  (let* ((state (harmless-execute--ensure session))
         (proc (plist-get state :conn))
         (deadline (+ (float-time) seconds))
         (calls 0)
         (result nil)
         (done nil))
    (harmless-execute--send
     proc (list :op "exec" :code code :names (harmless-execute--names)))
    (while (not done)
      (let* ((line (harmless-execute--read-line session proc deadline seconds))
             (msg (condition-case nil
                      (harmless-json-decode line)
                    (error nil))))
        (unless (and msg (listp msg))
          (error "execute code failed: interpreter exited"))
        (let ((op (harmless-tool-arg msg :op)))
          (cond
           ((equal op "done")
            (setq result (harmless-execute--truncate
                          (harmless-tool-arg msg :output))
                  done t))
           ((equal op "error")
            (setq result (harmless-execute--finish-error
                          (or (harmless-tool-arg msg :message) ""))
                  done t))
           ((equal op "tool")
            (setq calls (harmless-execute--one-tool
                         session proc deadline seconds calls msg)))
           (t (error "execute code failed: interpreter exited"))))))
    result))

(defun harmless-execute (session code &optional timeout reset)
  "Evaluate CODE in SESSION's interpreter and return what it prints.
TIMEOUT is seconds, or nil for `harmless-execute-timeout-seconds'.
RESET kills the interpreter before this script.  Nil, \"\", and
whitespace are refused before a process starts."
  (unless (harmless-session-p session)
    (error "No Harmless session"))
  (cond
   ((null code) (error "code is empty"))
   ((not (stringp code)) (error "code must be a string"))
   ((string-empty-p (string-trim code)) (error "code is empty")))
  (let ((seconds (harmless-execute--timeout-seconds timeout)))
    (when reset
      (harmless-execute-stop session))
    (harmless-execute--exchange session code seconds)))

(defun harmless-execute--tool (session args)
  "Tool wrapper: evaluate ARGS' Emacs Lisp in SESSION."
  (harmless-execute session
                    (harmless-tool-arg args :code)
                    (harmless-tool-arg args :timeout)
                    (harmless-json-true-p (harmless-tool-arg args :reset))))

(harmless-register-tool
 (harmless-tool-create
  :name "harmless-tool-execute-code"
  :description "Evaluate Emacs Lisp in a separate Emacs. CODE is the script. Only what it prints is returned. Call a tool as (harmless-tool-read-file :path \"a.txt\") or (tool \"harmless-tool-read-file\" :path \"a.txt\"). A tool result is not printed and is not added to the transcript. The script cannot call harmless-tool-execute-code, harmless-tool-spawn-agent, harmless-tool-run-workflow, or an MCP tool. TIMEOUT is seconds; nil uses the default of 300. RESET true starts a fresh Emacs first. A failed script leaves the interpreter up until reset or a timeout."
  :class 'shell
  :schema '(:type "object"
            :properties (:code (:type "string"
                                :description "Emacs Lisp to evaluate. Only printed output is returned.")
                         :timeout (:type "number"
                                   :description "Seconds before the interpreter is killed. Nil uses the default.")
                         :reset (:type "boolean"
                                 :description "When true, start a fresh interpreter before this script."))
            :required ["code"])
  :fn #'harmless-execute--tool))

(provide 'harmless-execute)

;;; harmless-execute.el ends here
