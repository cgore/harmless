;;; harmless-headless.el --- One prompt with no window -*- lexical-binding: t; -*-

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
;; `harmless-headless-run' sends one prompt and returns the reply.
;; It does not open a window.  `harmless-headless-start' writes a task
;; file and starts another Emacs to run that prompt.  The other process
;; is not a subprocess of this Emacs, so closing this Emacs does not
;; stop it.  The task file is the record a later Emacs reads.  This
;; file does not register another machine.

;;; Code:

(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-tools-fs)
(require 'harmless-turn)

(declare-function w32-shell-execute "w32fns.c")

(defvar harmless-headless-launch-function nil
  "Function of (ID) that starts the worker, or nil.
Nil starts another Emacs and returns without waiting for it.
The prompt stays in the task file.  It is not a command argument.")

(defun harmless-headless--prompt (prompt)
  "Return PROMPT trimmed, or signal.
Nil, \"\", and whitespace signal \"headless prompt is empty\".
A non-string signals \"headless prompt must be a string\"."
  (cond
   ((null prompt) (error "headless prompt is empty"))
   ((not (stringp prompt))
    (error "headless prompt must be a string"))
   ((string-empty-p (string-trim prompt))
    (error "headless prompt is empty"))
   (t (string-trim prompt))))

(defun harmless-headless--root-text-p (text)
  "Return non-nil if TEXT is \"/\" or a drive root.
TEXT is not expanded.  A relative path and \"~\" are not roots."
  (and (stringp text)
       (or (harmless-extra-root-text-p text)
           (harmless-filesystem-root-p text))))

(defun harmless-headless--directory (directory)
  "Return DIRECTORY as an absolute project directory, or signal.
Nil, \"\", and whitespace signal \"headless directory is empty\".
A non-string signals \"headless directory must be a string\".
\"/\", a drive root, and \"c:\" signal \"headless directory is the
filesystem root\".  A relative path, including \"~\", signals
\"headless directory must be absolute\" and is not expanded.  A
missing path signals \"headless directory is missing\".  A file
signals \"headless directory is not a directory\".  A trailing
slash is the same directory.  This does not consult
`default-directory' and does not create DIRECTORY."
  (cond
   ((null directory) (error "headless directory is empty"))
   ((not (stringp directory))
    (error "headless directory must be a string")))
  (setq directory (string-trim directory))
  (cond
   ((string-empty-p directory)
    (error "headless directory is empty"))
   ((harmless-headless--root-text-p directory)
    (error "headless directory is the filesystem root"))
   ((or (string-prefix-p "~" directory)
        (not (file-name-absolute-p directory)))
    (error "headless directory must be absolute")))
  (setq directory (directory-file-name (expand-file-name directory)))
  (cond
   ((harmless-headless--root-text-p directory)
    (error "headless directory is the filesystem root"))
   ((not (file-exists-p directory))
    (error "headless directory is missing"))
   ((not (file-directory-p directory))
    (error "headless directory is not a directory"))
   (t directory)))

(defun harmless-headless--task-name-p (id)
  "Return non-nil if ID is one task-file name."
  (and (stringp id)
       (string-match-p "\\`[A-Za-z0-9]+\\(?:-[A-Za-z0-9]+\\)*\\'" id)))

(defun harmless-headless--task-id (id)
  "Return ID trimmed, or signal.
Nil, \"\", and whitespace signal \"headless task is empty\".
A non-string signals \"headless task must be a string\".  A path
signals \"headless task is not a name\"."
  (cond
   ((null id) (error "headless task is empty"))
   ((not (stringp id))
    (error "headless task must be a string")))
  (setq id (string-trim id))
  (cond
   ((string-empty-p id) (error "headless task is empty"))
   ((not (harmless-headless--task-name-p id))
    (error "headless task is not a name"))
   (t id)))

(defun harmless-headless--refuse-root (place)
  "Signal if PLACE is the filesystem root or a drive root.
The error is \"Refusing to store a headless task at \" followed
by PLACE.  PLACE is not created."
  (let ((text (if (stringp place) (string-trim place) "")))
    (when (or (string-empty-p text)
              (harmless-headless--root-text-p text)
              (and (file-name-absolute-p text)
                   (not (string-prefix-p "~" text))
                   (equal (directory-file-name (file-truename text))
                          (directory-file-name (file-truename "/")))))
      (error "Refusing to store a headless task at %s" text))))

(defun harmless-headless--data-directory ()
  "Return the Harmless data directory, or signal.
A filesystem root and a drive root are refused before a task
file or a session directory is created."
  (let ((raw harmless-directory))
    (when (and (stringp raw) (not (string-empty-p raw)))
      (harmless-headless--refuse-root raw))
    (let ((data (harmless-data-directory)))
      (harmless-headless--refuse-root data)
      data)))

(defun harmless-headless--tasks-root (create)
  "Return the tasks directory under the Harmless data directory.
When CREATE is non-nil, create it.  A filesystem root is refused."
  (let ((root (file-name-as-directory
               (expand-file-name
                "tasks" (harmless-headless--data-directory)))))
    (harmless-headless--refuse-root root)
    (when create
      (harmless-ensure-directory root))
    root))

(defun harmless-headless--task-path (root id)
  "Return the task file for ID under ROOT, or signal."
  (let ((path (expand-file-name (concat id ".json") root)))
    (unless (and (file-name-absolute-p path)
                 (harmless-directory-strictly-under-p path root)
                 (not (harmless-headless--root-text-p path)))
      (error "Refusing to store a headless task at %s" path))
    path))

(defun harmless-headless--task-file (id)
  "Return the absolute task file for ID.
The tasks directory is not created.  ID is checked before any
filesystem root is read."
  (harmless-headless--task-path
   (harmless-headless--tasks-root nil)
   (harmless-headless--task-id id)))

(defun harmless-headless--write (path task)
  "Write TASK as JSON at PATH."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region (harmless-json-text task) nil path nil 'silent))
  path)

(defun harmless-headless--read (path)
  "Return the task plist at PATH, or signal.
A missing file signals \"headless task is missing\".  A file that
is not a task signals \"headless task is not valid\"."
  (unless (and (stringp path)
               (file-name-absolute-p path)
               (file-regular-p path)
               (not (harmless-headless--root-text-p path)))
    (error "headless task is missing"))
  (let ((task (with-temp-buffer
                (let ((coding-system-for-read 'utf-8-unix))
                  (insert-file-contents path))
                (harmless-json-decode-safe (buffer-string)))))
    (unless (and (harmless-plist-p task)
                 (stringp (plist-get task :id))
                 (stringp (plist-get task :status))
                 (stringp (plist-get task :prompt))
                 (stringp (plist-get task :directory)))
      (error "headless task is not valid"))
    task))

(defun harmless-headless--plist-set (task &rest pairs)
  "Return TASK with each key and value in PAIRS replaced."
  (while pairs
    (setq task (plist-put task (car pairs) (cadr pairs)))
    (setq pairs (cddr pairs)))
  task)

(defun harmless-headless--reply (session)
  "Return the last assistant text in SESSION, or signal."
  (let ((text nil)
        (messages (harmless-session-messages session)))
    (dolist (message messages)
      (when (memq (plist-get message :role) '(:assistant assistant))
        (setq text (or (plist-get message :content) ""))))
    (unless (stringp text)
      (error "headless turn failed"))
    text))

(defun harmless-headless--wait (session)
  "Wait until SESSION is no longer streaming or asking.
A turn that is still running after the wait signals
\"headless turn failed\"."
  (let ((left 2400))
    (while (and (> left 0)
                (memq (harmless-session-status session)
                      '(streaming waiting-permission)))
      (accept-process-output nil 0.05)
      (setq left (1- left)))
    (when (memq (harmless-session-status session)
                '(streaming waiting-permission))
      (error "headless turn failed"))))

(defun harmless-headless--execute (directory prompt)
  "Run one prompt and return (SESSION . TEXT).
DIRECTORY is the project.  PROMPT is the user text.  The session
is saved.  No window is opened."
  (setq prompt (harmless-headless--prompt prompt))
  (setq directory (harmless-headless--directory directory))
  (harmless-headless--data-directory)
  (let ((session (harmless-session-new :cwd directory)))
    (harmless-turn-run session prompt)
    (harmless-headless--wait session)
    (unless (eq (harmless-session-status session) 'idle)
      (error "headless turn failed"))
    (cons session (harmless-headless--reply session))))

(defun harmless-headless-run (directory prompt)
  "Run PROMPT in DIRECTORY and return the assistant text.
This does not open a window.  The session is saved under
`harmless-directory'.  A relative DIRECTORY is not expanded."
  (cdr (harmless-headless--execute directory prompt)))

(defun harmless-headless-status (id)
  "Return the stored task plist for ID.
The task file is not modified."
  (setq id (harmless-headless--task-id id))
  (let ((task (harmless-headless--read (harmless-headless--task-file id))))
    (unless (equal (plist-get task :id) id)
      (error "headless task is not valid"))
    task))

(defun harmless-headless-finish (id)
  "Run the stored prompt for task ID and return the assistant text.
The task must be running.  On success the file says \"done\" and
holds the reply and the session id.  On failure the file says
\"error\" and the same error is signaled again.  This does not
need a live session from the Emacs that started the task."
  (setq id (harmless-headless--task-id id))
  (let* ((path (harmless-headless--task-file id))
         (task (harmless-headless--read path)))
    (unless (equal (plist-get task :id) id)
      (error "headless task is not valid"))
    (unless (equal (plist-get task :status) "running")
      (error "headless task is not running"))
    (condition-case err
        (let* ((ran (harmless-headless--execute
                     (plist-get task :directory)
                     (plist-get task :prompt)))
               (session (car ran))
               (text (cdr ran)))
          (setq task (harmless-headless--plist-set
                      task
                      :status "done"
                      :result text
                      :session-id (harmless-session-id session)
                      :error ""
                      :updated-at (harmless-now-iso)))
          (harmless-headless--write path task)
          text)
      (error
       (setq task (harmless-headless--plist-set
                   task
                   :status "error"
                   :error (error-message-string err)
                   :updated-at (harmless-now-iso)))
       (harmless-headless--write path task)
       (signal (car err) (cdr err))))))

(defun harmless-headless--lisp-directory ()
  "Return the directory of the loaded `harmless.el'."
  (let ((lib (locate-library "harmless.el" t)))
    (unless lib
      (error "Cannot find harmless.el on `load-path'"))
    (file-name-directory lib)))

(defun harmless-headless--quote-arg (argument)
  "Quote ARGUMENT for CommandLineToArgvW, without cmd.exe carets."
  (if (or (string-empty-p argument)
          (string-match-p "[\t \"]" argument))
      (concat "\""
              (replace-regexp-in-string
               "\\(\\\\*\\)$" "\\1\\1"
               (replace-regexp-in-string
                "\\(\\\\*\\)\"" "\\1\\1\\\\\""
                argument))
              "\"")
    argument))

(defun harmless-headless--worker-argv (id)
  "Return the Emacs argument list that finishes task ID.
The prompt is not included.  The worker reads the task file."
  (list (expand-file-name invocation-name invocation-directory)
        "-Q" "--batch"
        "-L" (directory-file-name (harmless-headless--lisp-directory))
        "-l" "harmless"
        "-f" "harmless-headless-work"
        "--" id))

(defun harmless-headless--spawn (argv)
  "Start ARGV and return without waiting for it.
ARGV is (PROGRAM ARG...).  On Windows the process is started with
ShellExecute, so it is not a subprocess Emacs kills on exit.  On
Unix it is started with nohup and the shell returns at once."
  (cond
   ((eq system-type 'windows-nt)
    ;; SW_SHOWMINNOACTIVE is 7.  ShellExecute does not add the
    ;; process to Emacs's subprocess job.
    (w32-shell-execute
     "open"
     (car argv)
     (mapconcat #'harmless-headless--quote-arg (cdr argv) " ")
     7))
   (t
    (let ((status
           (call-process
            "sh" nil nil nil "-c"
            (format "nohup %s >/dev/null 2>&1 </dev/null &"
                    (mapconcat #'shell-quote-argument argv " ")))))
      (unless (eq status 0)
        (error "headless task did not start")))))
  nil)

(defun harmless-headless-launch (id)
  "Start the detached worker for ID and return ID.
This does not wait for the reply."
  (setq id (harmless-headless--task-id id))
  (harmless-headless--spawn (harmless-headless--worker-argv id))
  id)

(defun harmless-headless-start (directory prompt)
  "Write a running task for PROMPT in DIRECTORY and start it.
Return the task id.  This does not wait for the reply and does
not run the turn in this process.  A bad DIRECTORY or PROMPT
writes nothing.  When the launch signals, the task file says
\"error\" and the same error is signaled again."
  (setq prompt (harmless-headless--prompt prompt))
  (setq directory (harmless-headless--directory directory))
  (let* ((root (harmless-headless--tasks-root t))
         (id (harmless-uuid))
         (path (harmless-headless--task-path root id))
         (now (harmless-now-iso))
         (task (list :id id
                     :status "running"
                     :prompt prompt
                     :directory directory
                     :session-id ""
                     :result ""
                     :error ""
                     :created-at now
                     :updated-at now)))
    (harmless-headless--write path task)
    (condition-case err
        (progn
          (if (functionp harmless-headless-launch-function)
              (funcall harmless-headless-launch-function id)
            (harmless-headless-launch id))
          id)
      (error
       (setq task (harmless-headless--plist-set
                   task
                   :status "error"
                   :error (error-message-string err)
                   :updated-at (harmless-now-iso)))
       (harmless-headless--write path task)
       (signal (car err) (cdr err))))))

(defun harmless-headless--pop-arg ()
  "Pop the next command-line argument, skipping one leading \"--\"."
  (when (equal (car command-line-args-left) "--")
    (pop command-line-args-left))
  (pop command-line-args-left))

(provide 'harmless-headless)

;;; harmless-headless.el ends here
