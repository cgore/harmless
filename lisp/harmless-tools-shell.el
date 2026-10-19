;;; harmless-tools-shell.el --- Shell tool for Harmless -*- lexical-binding: t; -*-

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
;; run_shell: async `make-process' in the session cwd.  The tool function
;; takes a callback so the turn loop can keep Emacs responsive.  A command
;; that is still running at its timeout stays running in the background.
;; shell_output reads that job.  shell_kill stops it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-tools)

(defcustom harmless-shell-output-max-bytes 20000
  "Maximum bytes of shell output returned to the model.
The UI can still show the full buffer contents."
  :type 'integer
  :group 'harmless)

(defcustom harmless-shell-timeout-seconds 120
  "Seconds to wait before a shell command moves to the background.
The process is not killed.  The model can pass a larger `timeout'."
  :type 'number
  :group 'harmless)

(cl-defstruct (harmless-shell-job
               (:constructor harmless-shell-job--create)
               (:copier nil))
  id
  command
  proc
  buffer
  output
  status
  exit
  notified
  waiters
  timer
  delivered
  background)

(defun harmless-tools-shell-kill (proc)
  "SIGTERM PROC, then SIGKILL shortly after if it is still alive."
  (when (and proc (process-live-p proc))
    (ignore-errors (signal-process proc 'SIGTERM))
    (run-at-time 1.5 nil
                 (lambda (p)
                   (when (process-live-p p)
                     (ignore-errors (delete-process p))))
                 proc)))

(defun harmless-shell--for-model (text)
  "Return TEXT cut at `harmless-shell-output-max-bytes' for the model."
  (setq text (harmless-ensure-utf8 (or text "")))
  (if (> (string-bytes text) harmless-shell-output-max-bytes)
      (concat (substring text 0
                         (min (length text) harmless-shell-output-max-bytes))
              "\n[truncated]")
    text))

(defun harmless-shell--run-timeout (args)
  "Return how long `run_shell' should wait, or an error string.
A missing timeout uses `harmless-shell-timeout-seconds'.  Nil, zero,
a negative number, and a non-number are refused."
  (if (not (harmless-shell--arg-present-p args :timeout))
      harmless-shell-timeout-seconds
    (let ((raw (harmless-tool-arg args :timeout)))
      (if (and (numberp raw) (> raw 0))
          raw
        "Error: timeout must be a positive number"))))

(defun harmless-shell--arg-present-p (args key)
  "Return non-nil if KEY is present in plist ARGS."
  (or (and (listp args) (plist-member args key))
      (let ((name (if (keywordp key) (substring (symbol-name key) 1) key)))
        (and (listp args)
             (cl-loop for k in args by #'cddr
                      thereis (and (stringp k) (string= k name)))))))

(defun harmless-shell--output-timeout (args)
  "Return how long `shell_output' should wait, or an error string.
Nil means return the output already available.  A present timeout that
is not a positive number is refused."
  (if (not (harmless-shell--arg-present-p args :timeout))
      nil
    (let ((raw (harmless-tool-arg args :timeout)))
      (if (and (numberp raw) (> raw 0))
          raw
        "Error: timeout must be a positive number"))))

(defun harmless-shell--id (raw)
  "Return RAW as a shell id string, or nil when it is empty."
  (cond
   ((and (integerp raw) (>= raw 0)) (number-to-string raw))
   ((stringp raw)
    (let ((id (string-trim raw)))
      (and (not (string-empty-p id)) id)))
   (t nil)))

(defun harmless-shell-find (session id)
  "Return SESSION's background job ID, or nil."
  (cl-find id (harmless-session-shell-jobs session)
           :key #'harmless-shell-job-id :test #'equal))

(defun harmless-shell--append-output (buf text)
  "Insert TEXT into BUF, keeping at most `harmless-shell-output-max-bytes'."
  (when (and (buffer-live-p buf) (stringp text) (not (string-empty-p text)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert text)
      (when (> (1- (position-bytes (point-max)))
               harmless-shell-output-max-bytes)
        (let ((note "[earlier output discarded]\n"))
          (goto-char (point-min))
          (when (looking-at (regexp-quote note))
            (delete-region (point) (match-end 0)))
          (let* ((keep (max 0 (- harmless-shell-output-max-bytes
                                 (string-bytes note))))
                 (excess (- (1- (position-bytes (point-max))) keep)))
            (when (> excess 0)
              (goto-char (byte-to-position (1+ excess)))
              (delete-region (point-min) (point))))
          (goto-char (point-min))
          (insert note))))))

(defun harmless-shell--text (job)
  "Return JOB's output so far."
  (if (buffer-live-p (harmless-shell-job-buffer job))
      (with-current-buffer (harmless-shell-job-buffer job)
        (buffer-string))
    (or (harmless-shell-job-output job) "")))

(defun harmless-shell--close-buffer (job)
  "Keep JOB's output text and drop its process buffer."
  (setf (harmless-shell-job-output job) (harmless-shell--text job))
  (let ((proc (harmless-shell-job-proc job))
        (buf (harmless-shell-job-buffer job)))
    (when (processp proc)
      (ignore-errors (set-process-buffer proc nil)))
    (when (buffer-live-p buf)
      (kill-buffer buf))
    (setf (harmless-shell-job-buffer job) nil)))

(defun harmless-shell--foreground-text (job)
  "Return the tool result for a foreground JOB that has exited."
  (harmless-shell--for-model
   (format "exit %s\n%s"
           (or (harmless-shell-job-exit job) "?")
           (harmless-shell--text job))))

(defun harmless-shell--background-text (job)
  "Return the tool result that hands JOB to the model as a background id."
  (let ((raw (harmless-shell--text job))
        (id (harmless-shell-job-id job)))
    (harmless-shell--for-model
     (if (eq (harmless-shell-job-status job) 'running)
         (format "background %s\n%s" id raw)
       (format "background %s\nexit %s\n%s"
               id (or (harmless-shell-job-exit job) "?") raw)))))

(defun harmless-shell--snapshot (job)
  "Return the `shell_output' text for JOB."
  (let ((raw (harmless-shell--text job))
        (id (harmless-shell-job-id job)))
    (harmless-shell--for-model
     (pcase (harmless-shell-job-status job)
       ('running (format "shell %s running\n%s" id raw))
       ('killed (format "shell %s killed\n%s" id raw))
       (_ (format "shell %s exited %s\n%s"
                  id (or (harmless-shell-job-exit job) "?") raw))))))

(defun harmless-shell--fire-waiter (waiter text)
  "Call WAITER once with TEXT."
  (unless (nth 2 waiter)
    (setf (nth 2 waiter) t)
    (when (timerp (nth 1 waiter))
      (cancel-timer (nth 1 waiter)))
    (funcall (car waiter) text)))

(defun harmless-shell--deliver-waiters (job)
  "Give every waiter on JOB the current snapshot."
  (let ((waiters (harmless-shell-job-waiters job))
        (text (harmless-shell--snapshot job)))
    (setf (harmless-shell-job-waiters job) nil)
    (dolist (waiter waiters)
      (harmless-shell--fire-waiter waiter text))))

(defun harmless-shell--notify (session job)
  "Tell the user JOB finished, once.  Do not append a model message."
  (unless (harmless-shell-job-notified job)
    (setf (harmless-shell-job-notified job) t)
    (let* ((id (harmless-shell-job-id job))
           (status (if (eq (harmless-shell-job-status job) 'killed)
                       "killed"
                     (format "exited %s"
                             (or (harmless-shell-job-exit job) "?")))))
      (message "Harmless: background shell %s %s" id status)
      (harmless-emit session
                     (list :background id status
                           (harmless-shell-job-command job))))))

(defun harmless-shell--finish (session job)
  "Record that JOB's process has exited and wake its waiters."
  (when (eq (harmless-shell-job-status job) 'running)
    (setf (harmless-shell-job-status job)
          (if (eq (harmless-shell-job-exit job) 'killed) 'killed 'exited)))
  (let ((proc (harmless-shell-job-proc job)))
    (when (and proc (not (eq (harmless-shell-job-exit job) 'killed)))
      (setf (harmless-shell-job-exit job) (process-exit-status proc)))
    (when (timerp (harmless-shell-job-timer job))
      (cancel-timer (harmless-shell-job-timer job))
      (setf (harmless-shell-job-timer job) nil))
    (harmless-session-untrack-process session proc))
  (harmless-shell--close-buffer job)
  (harmless-shell--deliver-waiters job))

(defun harmless-shell--adopt (session command proc buf background)
  "Register PROC as a shell job of SESSION and return the job."
  (let* ((n (1+ (or (harmless-session-shell-seq session) 0)))
         (job (harmless-shell-job--create
               :id (number-to-string n)
               :command command
               :proc proc
               :buffer buf
               :output ""
               :status 'running
               :background background)))
    (setf (harmless-session-shell-seq session) n)
    (push job (harmless-session-shell-jobs session))
    (set-process-filter
     proc
     (lambda (_proc text)
       (harmless-shell--append-output buf text)))
    job))

(defun harmless-tools-shell--run (session args callback)
  "Run ARGS command in SESSION and call CALLBACK with the output string.
A positive `timeout' waits that many seconds, then leaves the process
running and returns its id.  `background' true returns the id at once.
Nil and \"\" commands, and a timeout that is not a positive number,
are refused."
  (let ((command (harmless-tool-arg args :command)))
    (if (not (and (stringp command) (not (string-empty-p command))))
        (funcall callback "Error: command is empty")
      (let ((cwd (harmless-session-require-project session))
            (timeout (harmless-shell--run-timeout args)))
        (if (stringp timeout)
            (funcall callback timeout)
          (let* ((background (harmless-json-true-p
                              (harmless-tool-arg args :background)))
                 (buf (generate-new-buffer " *harmless-shell*"))
                 (proc (let ((default-directory cwd))
                         (make-process
                          :name "harmless-shell"
                          :buffer buf
                          :command (list shell-file-name
                                         (or shell-command-switch "-c")
                                         command)
                          :coding '(utf-8-unix . utf-8-unix)
                          :connection-type 'pipe
                          :sentinel #'ignore)))
                 (job (harmless-shell--adopt session command proc buf background)))
            (unless background
              (harmless-session-track-process session proc)
              (setf (harmless-shell-job-timer job)
                    (run-at-time
                     timeout nil
                     (lambda ()
                       (when (and (eq (harmless-shell-job-status job) 'running)
                                  (not (harmless-shell-job-delivered job))
                                  (process-live-p proc))
                         (setf (harmless-shell-job-background job) t
                               (harmless-shell-job-delivered job) t)
                         (harmless-session-untrack-process session proc)
                         (funcall callback
                                  (harmless-shell--background-text job)))))))
            (set-process-sentinel
             proc
             (lambda (p _change)
               (unless (or (process-live-p p)
                           (not (eq (harmless-shell-job-status job) 'running)))
                 (harmless-shell--finish session job)
                 (cond
                  ((harmless-shell-job-delivered job)
                   (harmless-shell--notify session job))
                  (t
                   (setf (harmless-shell-job-delivered job) t)
                   (funcall callback
                            (if (harmless-shell-job-background job)
                                (progn
                                  (setf (harmless-shell-job-notified job) t)
                                  (harmless-shell--background-text job))
                              (harmless-shell--foreground-text job))))))))
            (unless (process-live-p proc)
              (funcall (process-sentinel proc) proc "exited"))
            (when (and background
                       (not (harmless-shell-job-delivered job)))
              (setf (harmless-shell-job-delivered job) t)
              (funcall callback (harmless-shell--background-text job)))
            proc))))))

(defun harmless-tools-shell--output (session args callback)
  "Call CALLBACK with the output of the background shell in ARGS.
`id' is required.  `timeout' waits that many seconds for the process
to exit.  An unknown id, an empty id, and a bad timeout are refused."
  (let ((id (harmless-shell--id (harmless-tool-arg args :id)))
        (timeout (harmless-shell--output-timeout args)))
    (cond
     ((null id)
      (funcall callback "Error: shell id is empty"))
     ((stringp timeout)
      (funcall callback timeout))
     (t
      (let ((job (harmless-shell-find session id)))
        (cond
         ((null job)
          (funcall callback (format "Error: no background shell %s" id)))
         ((or (not (eq (harmless-shell-job-status job) 'running))
              (null timeout))
          (funcall callback (harmless-shell--snapshot job)))
         (t
          (let ((waiter (list callback nil nil)))
            (setf (nth 1 waiter)
                  (run-at-time
                   timeout nil
                   (lambda ()
                     (when (memq waiter (harmless-shell-job-waiters job))
                       (setf (harmless-shell-job-waiters job)
                             (delq waiter (harmless-shell-job-waiters job)))
                       (harmless-shell--fire-waiter
                        waiter (harmless-shell--snapshot job))))))
            (push waiter (harmless-shell-job-waiters job))))))))))

(defun harmless-tools-shell--kill (session args)
  "Stop the background shell named in ARGS.  Return a result string.
An unknown id, an empty id, and a shell that is not running are refused."
  (let ((id (harmless-shell--id (harmless-tool-arg args :id))))
    (cond
     ((null id) "Error: shell id is empty")
     (t
      (let ((job (harmless-shell-find session id)))
        (cond
         ((null job) (format "Error: no background shell %s" id))
         ((not (eq (harmless-shell-job-status job) 'running))
          (format "Error: shell %s is not running" id))
         (t
          (setf (harmless-shell-job-exit job) 'killed
                (harmless-shell-job-status job) 'killed)
          (harmless-tools-shell-kill (harmless-shell-job-proc job))
          (harmless-shell--finish session job)
          (harmless-shell--notify session job)
          (format "killed shell %s" id))))))))

(defun harmless-tools-shell-register ()
  "Register the shell tools."
  (harmless-register-tool
   (harmless-tool-create
    :name "run_shell"
    :description "Run a shell command in the project root. Each command starts there. timeout is seconds to wait before the command is left running in the background. background true returns an id immediately. Read it with shell_output and stop it with shell_kill."
    :class 'shell
    :async t
    :schema '(:type "object"
              :properties (:command (:type "string"
                                    :description "Shell command")
                           :timeout (:type "number"
                                    :description "Seconds to wait before leaving the command in the background. A positive number. Omit it for the default.")
                           :background (:type "boolean"
                                       :description "Return a background id immediately."))
              :required ["command"])
    :fn #'harmless-tools-shell--run))
  (harmless-register-tool
   (harmless-tool-create
    :name "shell_output"
    :description "Read a background shell by id. timeout waits that many seconds for it to exit. Omit timeout to return the output already available."
    :class 'read
    :async t
    :schema '(:type "object"
              :properties (:id (:type "string"
                                 :description "Background shell id")
                           :timeout (:type "number"
                                    :description "Seconds to wait for the command to exit."))
              :required ["id"])
    :fn #'harmless-tools-shell--output))
  (harmless-register-tool
   (harmless-tool-create
    :name "shell_kill"
    :description "Stop a background shell by id."
    :class 'shell
    :schema '(:type "object"
              :properties (:id (:type "string"
                                 :description "Background shell id"))
              :required ["id"])
    :fn #'harmless-tools-shell--kill)))

(harmless-tools-shell-register)

(provide 'harmless-tools-shell)

;;; harmless-tools-shell.el ends here
