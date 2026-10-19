;;; harmless-monitor.el --- Wake a session when a line matters -*- lexical-binding: t; -*-

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
;; Watch a file, a command, or a background shell.  A matching line
;; wakes the session.  A file watch is a read.  A command that can
;; write is a shell.  Nothing is written into the project.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-tools)
(require 'harmless-tools-fs)
(require 'harmless-tools-shell)
(require 'harmless-turn)

(defvar harmless-monitor-interval 1
  "Seconds between checks of a file or a background shell.")

(defvar harmless-monitor-schedule-function
  #'harmless-monitor-schedule-default
  "Function (MONITOR) that arranges later polls.
Tests bind this so no timer starts.")

(defvar harmless-monitor-command-function
  #'harmless-monitor-command-default
  "Function (SESSION MONITOR COMMAND) that starts COMMAND.
Tests bind this so no process starts.")

(defvar harmless-monitor-make-process-function #'make-process
  "Function passed to `make-process' by the command watcher.")

(defvar harmless-monitor-kill-function #'harmless-monitor-kill-default
  "Function (MONITOR) that stops a command watcher.")

(defvar harmless-monitor--table (make-hash-table :test 'eq)
  "Live monitors keyed by session.")

(cl-defstruct (harmless-monitor
               (:constructor harmless-monitor--create)
               (:copier nil))
  id
  session
  kind
  pattern
  path
  command
  shell
  status
  position
  pending
  process
  timer
  buffer)

(defun harmless-monitor-plan-refuses-p (session tool args)
  "Return a refusal when plan mode blocks TOOL on SESSION.
A file watch and a read-only command still run.  The caller
refuses before asking for permission."
  (and (string= (harmless-tool-name tool) "monitor")
       (null (harmless-tool-arg args :path))
       (null (harmless-tool-arg args :shell))
       (let ((command (harmless-tool-arg args :command)))
         (and (stringp command)
              (harmless-shell-plan-refuses-p
               session (string-trim command))
              harmless-shell-plan-refusal))))

(defun harmless-monitor-class (session args)
  "Return `read' or `shell' for a monitor call in ARGS.
A file, an existing shell, and a read-only command are `read'."
  (let ((command (harmless-tool-arg args :command))
        (path (harmless-tool-arg args :path))
        (shell (harmless-tool-arg args :shell)))
    (if (and (not (null command))
             (null path)
             (null shell)
             (stringp command)
             (not (string-empty-p (string-trim command))))
        (harmless-shell-run-class
         session (list :command (string-trim command)))
      'read)))

(defun harmless-monitor--state (session)
  "Return SESSION's monitor state, creating it in memory."
  (or (gethash session harmless-monitor--table)
      (puthash session (list :seq 0 :items nil)
               harmless-monitor--table)))

(defun harmless-monitor-find (session id)
  "Return SESSION's monitor ID, or nil."
  (cl-find id (plist-get (harmless-monitor--state session) :items)
           :key #'harmless-monitor-id :test #'equal))

(defun harmless-monitor--next-id (session)
  "Return the next monitor id string for SESSION."
  (number-to-string
   (1+ (or (plist-get (harmless-monitor--state session) :seq) 0))))

(defun harmless-monitor--add (session monitor)
  "Remember MONITOR on SESSION and return it."
  (let ((state (harmless-monitor--state session)))
    (setf (plist-get state :seq)
          (string-to-number (harmless-monitor-id monitor)))
    (setf (plist-get state :items)
          (cons monitor (plist-get state :items)))
    monitor))

(defun harmless-monitor--id (raw what)
  "Return RAW as an id for WHAT, or signal."
  (cond
   ((and (integerp raw) (>= raw 0)) (number-to-string raw))
   ((and (integerp raw) (< raw 0))
    (error "%s id is not an id" what))
   ((stringp raw)
    (let ((id (string-trim raw)))
      (when (string-empty-p id)
        (error "%s id is empty" what))
      id))
   ((null raw) (error "%s id is empty" what))
   (t (error "%s id must be a string" what))))

(defun harmless-monitor--pattern (args)
  "Return the regex in ARGS, or signal.
\"/\" and \"c:/\" are patterns, not paths."
  (let ((pattern (harmless-tool-arg args :pattern)))
    (cond
     ((null pattern) (error "pattern must be a string"))
     ((not (stringp pattern)) (error "pattern must be a string"))
     (t
      (setq pattern (string-trim pattern))
      (when (string-empty-p pattern)
        (error "pattern is empty"))
      (condition-case nil
          (string-match-p pattern "")
        (invalid-regexp (error "pattern is not a pattern")))
      pattern))))

(defun harmless-monitor--file (session raw)
  "Return RAW as an existing file inside SESSION's project."
  (unless (stringp raw)
    (error "path must be a string"))
  (setq raw (string-trim raw))
  (when (string-empty-p raw)
    (error "Path is empty"))
  (let ((full (harmless-tools-fs-resolve session raw)))
    (when (or (directory-name-p raw)
              (directory-name-p full)
              (file-directory-p full))
      (error "monitor path is a directory"))
    (unless (file-regular-p full)
      (error "monitor file is missing"))
    (when (or (harmless-filesystem-root-p full)
              (not (file-name-absolute-p full)))
      (error "Refusing to watch %s" full))
    full))

(defun harmless-monitor--file-size (file)
  "Return the byte size of FILE, or 0 when it is missing."
  (let ((attrs (file-attributes file)))
    (if attrs (nth 7 attrs) 0)))

(defun harmless-monitor--consider (monitor line)
  "Wake MONITOR when LINE matches its pattern."
  (when (and (eq (harmless-monitor-status monitor) 'running)
             (string-suffix-p "\r" line))
    (setq line (substring line 0 -1)))
  (when (and (eq (harmless-monitor-status monitor) 'running)
             (string-match-p (harmless-monitor-pattern monitor) line))
    (let ((session (harmless-monitor-session monitor))
          (text (format "Monitor %s matched:\n%s"
                        (harmless-monitor-id monitor) line)))
      (when (harmless-session-p session)
        (condition-case nil
            (harmless-turn-steer session text)
          (error nil))))))

(defun harmless-monitor-ingest (monitor text)
  "Feed TEXT to MONITOR, one complete line at a time."
  (when (and (eq (harmless-monitor-status monitor) 'running)
             (stringp text)
             (not (string-empty-p text)))
    (let ((chunk (concat (or (harmless-monitor-pending monitor) "") text))
          (lines nil))
      (while (string-match "\\`\\([^\n]*\\)\n" chunk)
        (push (match-string 1 chunk) lines)
        (setq chunk (substring chunk (match-end 0))))
      (setf (harmless-monitor-pending monitor) chunk)
      (dolist (line (nreverse lines))
        (harmless-monitor--consider monitor line)))))

(defun harmless-monitor--poll-file (monitor)
  "Read new bytes from MONITOR's file."
  (let ((file (harmless-monitor-path monitor)))
    (when (and (stringp file) (file-regular-p file))
      (let ((size (harmless-monitor--file-size file))
            (pos (or (harmless-monitor-position monitor) 0)))
        (cond
         ((< size pos)
          (setf (harmless-monitor-position monitor) size
                (harmless-monitor-pending monitor) ""))
         ((> size pos)
          (let ((text (with-temp-buffer
                        (let ((coding-system-for-read 'utf-8-unix))
                          (insert-file-contents file nil pos size))
                        (buffer-string))))
            (setf (harmless-monitor-position monitor) size)
            (harmless-monitor-ingest monitor text))))))))

(defun harmless-monitor--poll-shell (monitor)
  "Read new text from MONITOR's background shell."
  (let* ((job (harmless-shell-find
               (harmless-monitor-session monitor)
               (harmless-monitor-shell monitor)))
         (buf (and job (harmless-shell-job-buffer job))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let* ((end (point-max))
               (start (min (or (harmless-monitor-position monitor) end)
                           end))
               (text (buffer-substring-no-properties start end)))
          (setf (harmless-monitor-position monitor) end)
          (harmless-monitor-ingest monitor text))))))

(defun harmless-monitor-poll (monitor)
  "Check MONITOR once.  A stopped monitor does nothing."
  (when (eq (harmless-monitor-status monitor) 'running)
    (pcase (harmless-monitor-kind monitor)
      ('file (harmless-monitor--poll-file monitor))
      ('shell (harmless-monitor--poll-shell monitor)))))

(defun harmless-monitor-schedule-default (monitor)
  "Poll MONITOR until it stops."
  (when (eq (harmless-monitor-status monitor) 'running)
    (setf (harmless-monitor-timer monitor)
          (run-at-time harmless-monitor-interval
                       harmless-monitor-interval
                       #'harmless-monitor-poll monitor))))

(defun harmless-monitor-command-default (session monitor command)
  "Start COMMAND and feed its output to MONITOR."
  (let* ((buf (generate-new-buffer " *harmless-monitor*"))
         (kind (harmless-shell-kind))
         (proc (let ((default-directory
                      (harmless-session-require-project session)))
                 (funcall harmless-monitor-make-process-function
                          :name "harmless-monitor"
                          :buffer buf
                          :command
                          (harmless-shell--argv
                           kind
                           (harmless-shell--wrap
                            kind
                            (harmless-shell-directory session)
                            command))
                          :coding '(utf-8-unix . utf-8-unix)
                          :connection-type 'pipe
                          :noquery t
                          :sentinel #'ignore))))
    (setf (harmless-monitor-process monitor) proc
          (harmless-monitor-buffer monitor) buf)
    (when (processp proc)
      (set-process-filter
       proc
       (lambda (_proc text)
         (harmless-monitor-ingest monitor text)))
      (set-process-sentinel
       proc
       (lambda (p _change)
         (unless (process-live-p p)
           (when (and (stringp (harmless-monitor-pending monitor))
                      (not (string-empty-p
                            (harmless-monitor-pending monitor))))
             (harmless-monitor-ingest monitor "\n"))
           (when (eq (harmless-monitor-status monitor) 'running)
             (setf (harmless-monitor-status monitor) 'stopped))))))
    proc))

(defun harmless-monitor-kill-default (monitor)
  "Stop MONITOR's process and drop its buffer."
  (let ((proc (harmless-monitor-process monitor)))
    (when (and (processp proc) (process-live-p proc))
      (ignore-errors (delete-process proc))))
  (let ((buf (harmless-monitor-buffer monitor)))
    (when (buffer-live-p buf)
      (kill-buffer buf))
    (setf (harmless-monitor-buffer monitor) nil)))

(defun harmless-monitor-stop (monitor)
  "Stop MONITOR.  A command's process is killed.
A background shell that was only watched keeps running."
  (when (timerp (harmless-monitor-timer monitor))
    (cancel-timer (harmless-monitor-timer monitor))
    (setf (harmless-monitor-timer monitor) nil))
  (when (eq (harmless-monitor-kind monitor) 'command)
    (funcall harmless-monitor-kill-function monitor))
  (setf (harmless-monitor-status monitor) 'stopped)
  monitor)

(defun harmless-monitor--begin (session kind pattern)
  "Create a running monitor and remember it."
  (harmless-monitor--add
   session
   (harmless-monitor--create
    :id (harmless-monitor--next-id session)
    :session session
    :kind kind
    :pattern pattern
    :status 'running
    :pending "")))

(defun harmless-monitor--start-file (session pattern raw)
  "Watch RAW inside SESSION for PATTERN."
  (let* ((file (harmless-monitor--file session raw))
         (monitor (harmless-monitor--begin session 'file pattern)))
    (setf (harmless-monitor-path monitor) file
          (harmless-monitor-position monitor)
          (harmless-monitor--file-size file))
    (funcall harmless-monitor-schedule-function monitor)
    monitor))

(defun harmless-monitor--plan (session command)
  "Signal when plan mode refuses COMMAND."
  (when (harmless-shell-plan-refuses-p session command)
    (error "Plan mode is on. A shell command that can write is refused")))

(defun harmless-monitor--start-command (session pattern raw)
  "Watch RAW's output on SESSION for PATTERN."
  (unless (stringp raw)
    (error "command must be a string"))
  (let ((command (string-trim raw)))
    (when (string-empty-p command)
      (error "command is empty"))
    (harmless-monitor--plan session command)
    (harmless-session-require-project session)
    (let ((monitor (harmless-monitor--begin session 'command pattern)))
      (setf (harmless-monitor-command monitor) command)
      (condition-case err
          (funcall harmless-monitor-command-function
                   session monitor command)
        (error
         (setf (harmless-monitor-status monitor) 'stopped)
         (signal (car err) (cdr err))))
      monitor)))

(defun harmless-monitor--start-shell (session pattern raw)
  "Watch background shell RAW on SESSION for PATTERN."
  (let ((id (harmless-monitor--id raw "shell")))
    (unless (harmless-shell-find session id)
      (error "no background shell %s" id))
    (let ((monitor (harmless-monitor--begin session 'shell pattern))
          (job (harmless-shell-find session id)))
      (setf (harmless-monitor-shell monitor) id)
      (let ((buf (harmless-shell-job-buffer job)))
        (setf (harmless-monitor-position monitor)
              (if (buffer-live-p buf)
                  (with-current-buffer buf (point-max))
                1)))
      (funcall harmless-monitor-schedule-function monitor)
      monitor)))

(defun harmless-monitor--sources (args)
  "Return the source keys present in ARGS."
  (delq nil
        (list (and (not (null (harmless-tool-arg args :path))) 'path)
              (and (not (null (harmless-tool-arg args :command))) 'command)
              (and (not (null (harmless-tool-arg args :shell))) 'shell))))

(defun harmless-monitor-tool (session args)
  "monitor implementation."
  (let ((pattern (harmless-monitor--pattern args))
        (sources (harmless-monitor--sources args)))
    (cond
     ((null sources) (error "monitor source is missing"))
     ((> (length sources) 1) (error "monitor source must be one"))
     ((eq (car sources) 'path)
      (harmless-monitor--started
       (harmless-monitor--start-file
        session pattern (harmless-tool-arg args :path))))
     ((eq (car sources) 'command)
      (harmless-monitor--started
       (harmless-monitor--start-command
        session pattern (harmless-tool-arg args :command))))
     (t
      (harmless-monitor--started
       (harmless-monitor--start-shell
        session pattern (harmless-tool-arg args :shell)))))))

(defun harmless-monitor--started (monitor)
  "Return the tool result for a new MONITOR."
  (format "Watching %s" (harmless-monitor-id monitor)))

(defun harmless-monitor-stop-tool (session args)
  "monitor_stop implementation."
  (let* ((id (harmless-monitor--id (harmless-tool-arg args :id) "monitor"))
         (monitor (harmless-monitor-find session id)))
    (cond
     ((null monitor) (error "no monitor %s" id))
     ((not (eq (harmless-monitor-status monitor) 'running))
      (error "monitor %s is not running" id))
     (t
      (harmless-monitor-stop monitor)
      (format "Stopped monitor %s" id)))))

(defun harmless-monitor-stop-class (session args)
  "Return `shell' when stopping a command monitor, else `read'."
  (let* ((id (condition-case nil
                 (harmless-monitor--id
                  (harmless-tool-arg args :id) "monitor")
               (error nil)))
         (monitor (and id (harmless-monitor-find session id))))
    (if (and monitor (eq (harmless-monitor-kind monitor) 'command))
        'shell
      'read)))

(defun harmless-monitor-register ()
  "Register the monitor tools."
  (harmless-register-tool
   (harmless-tool-create
    :name "monitor"
    :description "Watch a file, a command, or a background shell. PATTERN is a regular expression. A matching line wakes the session. Pass one of path, command, or shell. A file watch does not ask. A command that can write asks, and plan mode refuses it. Nothing is written into the project."
    :class 'shell
    :classify #'harmless-monitor-class
    :schema '(:type "object"
              :properties (:pattern (:type "string"
                                     :description "Regular expression")
                           :path (:type "string"
                                  :description "File in the project or an added directory")
                           :command (:type "string"
                                     :description "Command whose output is watched")
                           :shell (:type "string"
                                   :description "Background shell id"))
              :required ["pattern"])
    :fn #'harmless-monitor-tool))
  (harmless-register-tool
   (harmless-tool-create
    :name "monitor_stop"
    :description "Stop a monitor by id. A command this monitor started is killed. A background shell that was only watched keeps running. Nothing is written into the project."
    :class 'shell
    :classify #'harmless-monitor-stop-class
    :schema '(:type "object"
              :properties (:id (:type "string"
                                 :description "Monitor id"))
              :required ["id"])
    :fn #'harmless-monitor-stop-tool)))

(harmless-monitor-register)

(provide 'harmless-monitor)

;;; harmless-monitor.el ends here
