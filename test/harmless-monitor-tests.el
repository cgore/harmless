;;; harmless-monitor-tests.el --- Tests for monitors -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(cl-defstruct (harmless-monitor-fake
               (:include harmless-provider)
               (:constructor harmless-monitor-make-fake))
  script)

(cl-defmethod harmless-provider-complete ((provider harmless-monitor-fake)
                                          _messages _tools callback)
  (when (harmless-monitor-fake-script provider)
    (funcall (harmless-monitor-fake-script provider) _messages callback))
  nil)

(defvar harmless-monitor-test-calls 0
  "Provider calls in the current monitor test.")

(defun harmless-monitor-test-wake-script ()
  "Return a provider script that finishes a wake with text."
  (lambda (_messages callback)
    (setq harmless-monitor-test-calls
          (1+ harmless-monitor-test-calls))
    (funcall callback '(:text "done"))
    (funcall callback '(:stop "stop"))))

(defun harmless-monitor-test-tool-script (event)
  "Return a provider script that sends EVENT once, then text."
  (lambda (_messages callback)
    (setq harmless-monitor-test-calls
          (1+ harmless-monitor-test-calls))
    (if (= harmless-monitor-test-calls 1)
        (progn
          (funcall callback event)
          (funcall callback '(:stop "tool_calls")))
      (funcall callback '(:text "done"))
      (funcall callback '(:stop "stop")))))

(defmacro harmless-monitor-test-with (data &rest body)
  "Bind a temp data directory DATA, then run BODY.
No timer starts and no command process starts unless BODY asks."
  (declare (indent 1))
  `(let* ((,data (make-temp-file "harmless-monitor-data-" t))
          (harmless-directory ,data)
          (harmless--sessions (make-hash-table :test 'equal))
          (harmless-monitor-test-calls 0)
          (harmless-compact-threshold nil)
          (harmless-monitor-schedule-function #'ignore)
          (harmless-monitor-command-function
           (lambda (&rest _) (error "command was called")))
          (harmless-monitor-kill-function #'ignore)
          (harmless-perm-ask-function
           (lambda (_session _class _item callback)
             (funcall callback 'allow))))
     ,@body))

(defun harmless-monitor-test-session (&rest args)
  "Return a session.  ARGS are passed to `harmless-session-new'."
  (apply #'harmless-session-new
         (append args
                 (list :provider
                       (harmless-monitor-make-fake
                        :name "fake" :host "none"
                        :script (harmless-monitor-test-wake-script))
                       :model "m"))))

(defun harmless-monitor-test-call (session name args)
  "Call tool NAME on SESSION with ARGS."
  (harmless-tool-call (harmless-tool-by-name name) session args))

(defun harmless-monitor-test-expect (session name args text)
  "Assert tool NAME on SESSION with ARGS returns Error: TEXT."
  (should (equal (harmless-monitor-test-call session name args)
                 (concat "Error: " text))))

(defun harmless-monitor-test-write (file text &optional append)
  "Write TEXT to FILE.  APPEND keeps the bytes already there."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil file append 'silent))
  file)

(defun harmless-monitor-test-files (dir)
  "Return the names directly under DIR, sorted."
  (sort (directory-files dir t) #'string<))

(defun harmless-monitor-test-user-texts (session)
  "Return user message text stored on SESSION."
  (cl-loop for msg in (harmless-session-messages session)
           when (eq (plist-get msg :role) :user)
           collect (plist-get msg :content)))

(defun harmless-monitor-test-tool-texts (session)
  "Return tool results stored on SESSION."
  (cl-loop for msg in (harmless-session-messages session)
           when (eq (plist-get msg :role) :tool)
           collect (plist-get msg :content)))

(defun harmless-monitor-test-assert-session (session)
  "Require SESSION's directory to stay off the filesystem root."
  (let ((dir (harmless-session-dir session)))
    (should (file-name-absolute-p dir))
    (should-not (harmless-filesystem-root-p dir))
    (should (harmless-directory-strictly-under-p dir harmless-directory))
    (should-not (equal (file-truename dir)
                       (file-truename (expand-file-name "/"))))))

(defun harmless-monitor-test-assert-project-file (path repo)
  "Require PATH to be an absolute file inside REPO and not under /."
  (should (file-name-absolute-p path))
  (should (file-regular-p path))
  (should-not (harmless-filesystem-root-p path))
  (should (string-prefix-p
           (file-name-as-directory (file-truename repo))
           (file-truename path)))
  (should-not (equal (file-truename path)
                     (file-truename
                      (expand-file-name
                       (file-name-nondirectory path) "/")))))

(defun harmless-monitor-test-raw (session name)
  "Return NAME from SESSION's directory and require that path."
  (let ((file (expand-file-name name (harmless-session-dir session))))
    (should (file-name-absolute-p file))
    (should-not (harmless-filesystem-root-p file))
    (should (harmless-directory-strictly-under-p
             (file-name-directory file) harmless-directory))
    (should-not (equal (file-truename file)
                       (file-truename (expand-file-name name "/"))))
    (if (file-regular-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (buffer-string))
      "")))

(defun harmless-monitor-test-assert-saved (session text)
  "Assert TEXT was saved on SESSION, including its newline."
  (let ((raw (harmless-monitor-test-raw session "messages.jsonl"))
        (needle (replace-regexp-in-string "\n" "\\n" text t t)))
    (should (string-search needle raw))
    (should (cl-find text (harmless-session-messages session)
                     :test (lambda (wanted msg)
                             (equal wanted
                                    (plist-get msg :content)))))))

(defun harmless-monitor-test-wakes (session)
  "Return how many times SESSION's provider was called."
  harmless-monitor-test-calls)

(ert-deftest harmless-monitor-refuses-bad-arguments ()
  (harmless-monitor-test-with data
    (let* ((repo (make-temp-file "harmless-monitor-repo-" t))
           (log (expand-file-name "log.txt" repo))
           (readme (expand-file-name "README" repo))
           (started nil)
           (root-monitor (file-exists-p "/monitor"))
           (session (harmless-monitor-test-session
                     :cwd repo :id "s" :title "Keep"))
           (other (harmless-monitor-test-session
                   :cwd repo :id "other" :title "Keep"))
           (files nil)
           (monitor nil))
      (setq harmless-monitor-command-function
            (lambda (&rest _) (setq started t)))
      (harmless-monitor-test-write readme "keep\n")
      (harmless-monitor-test-write log "old\n")
      (make-directory (expand-file-name "sub" repo))
      (setq files (harmless-monitor-test-files repo))
      (dolist (pattern '(nil :false 42))
        (harmless-monitor-test-expect
         session "harmless-tool-monitor" (list :pattern pattern)
         "pattern must be a string"))
      (dolist (pattern '("" "   " "\n" "  \t"))
        (harmless-monitor-test-expect
         session "harmless-tool-monitor" (list :pattern pattern)
         "pattern is empty"))
      (harmless-monitor-test-expect
       session "harmless-tool-monitor" '(:pattern "[")
       "pattern is not a pattern")
      (harmless-monitor-test-expect
       session "harmless-tool-monitor" '(:pattern "[" :path "/" :command "rm x")
       "pattern is not a pattern")
      (harmless-monitor-test-expect
       session "harmless-tool-monitor" '(:pattern "error")
       "monitor source is missing")
      (harmless-monitor-test-expect
       session "harmless-tool-monitor" '(:pattern "error" :path nil)
       "monitor source is missing")
      (dolist (args '((:pattern "error" :path "log.txt" :command "echo")
                      (:pattern "error" :path "log.txt" :shell "1")
                      (:pattern "error" :command "echo" :shell "1")
                      (:pattern "error" :path "a" :command "b" :shell "c")))
        (harmless-monitor-test-expect
         session "harmless-tool-monitor" args "monitor source must be one"))
      (harmless-monitor-test-expect
       session "harmless-tool-monitor" '(:pattern "error" :path :false)
       "path must be a string")
      (harmless-monitor-test-expect
       session "harmless-tool-monitor" '(:pattern "error" :path 42)
       "path must be a string")
      (dolist (path '("" "   " "\n"))
        (harmless-monitor-test-expect
         session "harmless-tool-monitor" (list :pattern "error" :path path)
         "Path is empty"))
      (dolist (path '("/" "  /  " "c:/" "../x" "  ../x  "))
        (harmless-monitor-test-expect
         session "harmless-tool-monitor"
         (list :pattern "error" :path path)
         (concat "Path escapes project: " (string-trim path))))
      (harmless-monitor-test-expect
       session "harmless-tool-monitor" '(:pattern "error" :path "log.txt/")
       "monitor path is a directory")
      (harmless-monitor-test-expect
       session "harmless-tool-monitor" '(:pattern "error" :path "sub")
       "monitor path is a directory")
      (harmless-monitor-test-expect
       session "harmless-tool-monitor" '(:pattern "error" :path "sub/")
       "monitor path is a directory")
      (harmless-monitor-test-expect
       session "harmless-tool-monitor" '(:pattern "error" :path "gone.txt")
       "monitor file is missing")
      (let ((missing (expand-file-name "missing-dir" repo)))
        (should-not (file-exists-p missing))
        (harmless-monitor-test-expect
         session "harmless-tool-monitor" '(:pattern "error" :path "missing-dir/log.txt")
         "monitor file is missing")
        (should-not (file-exists-p missing))
        (should-not (file-exists-p (expand-file-name "log.txt" missing))))
      (harmless-monitor-test-expect
       session "harmless-tool-monitor" '(:pattern "error" :command nil)
       "monitor source is missing")
      (dolist (command '(:false 42))
        (harmless-monitor-test-expect
         session "harmless-tool-monitor" (list :pattern "error" :command command)
         "command must be a string"))
      (dolist (command '("" "   " "\n"))
        (harmless-monitor-test-expect
         session "harmless-tool-monitor" (list :pattern "error" :command command)
         "command is empty"))
      (setf (harmless-session-plan-mode session) t)
      (harmless-monitor-test-expect
       session "harmless-tool-monitor" '(:pattern "error" :command "   ")
       "command is empty")
      (harmless-monitor-test-expect
       session "harmless-tool-monitor" '(:pattern "error" :path "log.txt" :command "rm x")
       "monitor source must be one")
      (setf (harmless-session-plan-mode session) nil)
      (dolist (shell '(:false 42.5))
        (harmless-monitor-test-expect
         session "harmless-tool-monitor" (list :pattern "error" :shell shell)
         "shell id must be a string"))
      (harmless-monitor-test-expect
       session "harmless-tool-monitor" '(:pattern "error" :shell -1)
       "shell id is not an id")
      (dolist (shell '("" "   " "\n"))
        (harmless-monitor-test-expect
         session "harmless-tool-monitor" (list :pattern "error" :shell shell)
         "shell id is empty"))
      (dolist (shell '("/" "c:/" "nope" 42))
        (harmless-monitor-test-expect
         session "harmless-tool-monitor" (list :pattern "error" :shell shell)
         (format "no background shell %s"
                 (if (integerp shell) (number-to-string shell) shell))))
      (let ((detached (harmless-monitor-test-session
                       :detached t :id "chat" :title "Keep")))
        (harmless-monitor-test-expect
         detached "harmless-tool-monitor" '(:pattern "error" :path "log.txt")
         "This chat is not attached to a project")
        (harmless-monitor-test-expect
         detached "harmless-tool-monitor" '(:pattern "error" :command "echo hi")
         "This chat is not attached to a project")
        (setf (harmless-session-plan-mode detached) t)
        (harmless-monitor-test-expect
         detached "harmless-tool-monitor" '(:pattern "error" :command "rm x")
         "Plan mode is on. A shell command that can write is refused")
        (harmless-monitor-test-expect
         detached "harmless-tool-monitor" '(:pattern "error" :command "echo hi")
         "This chat is not attached to a project")
        (should-not (member "harmless-tool-monitor"
                            (mapcar #'harmless-tool-name
                                    (harmless-tools-for-session detached))))
        (should-not (member "harmless-tool-monitor-stop"
                            (mapcar #'harmless-tool-name
                                    (harmless-tools-for-session detached))))
        (harmless-monitor-test-assert-session detached))
      (should (equal (harmless-monitor-test-call
                      session "harmless-tool-monitor"
                      '("pattern" "  error  " "path" "  log.txt  "))
                     "Watching 1"))
      (setq monitor (harmless-monitor-find session "1"))
      (should (equal (harmless-monitor-pattern monitor) "error"))
      (should (eq (harmless-monitor-kind monitor) 'file))
      (should (eq (harmless-monitor-status monitor) 'running))
      (should (equal (harmless-monitor-path monitor)
                     (expand-file-name
                      "log.txt"
                      (harmless-session-require-project session))))
      (harmless-monitor-test-assert-project-file
       (harmless-monitor-path monitor) repo)
      (should-not (harmless-monitor-find other "1"))
      (should (equal (harmless-monitor-test-call
                      other "harmless-tool-monitor" '(:pattern "error" :path "log.txt"))
                     "Watching 1"))
      (dolist (id '(nil "" "   " "\n"))
        (harmless-monitor-test-expect
         session "harmless-tool-monitor-stop" (list :id id) "harmless-tool-monitor id is empty"))
      (dolist (id '(:false 42.5))
        (harmless-monitor-test-expect
         session "harmless-tool-monitor-stop" (list :id id)
         "harmless-tool-monitor id must be a string"))
      (harmless-monitor-test-expect
       session "harmless-tool-monitor-stop" '(:id -2) "harmless-tool-monitor id is not an id")
      (dolist (id '("9" "/" "c:/"))
        (harmless-monitor-test-expect
         session "harmless-tool-monitor-stop" (list :id id)
         (concat "no monitor " id)))
      (should (equal (harmless-monitor-test-call
                      session "harmless-tool-monitor-stop" '("id" "  1  "))
                     "Stopped monitor 1"))
      (harmless-monitor-test-expect
       session "harmless-tool-monitor-stop" '(:id 1) "monitor 1 is not running")
      (should (eq (harmless-monitor-status
                   (harmless-monitor-find other "1"))
                  'running))
      (let ((tool (harmless-tool-by-name "harmless-tool-monitor"))
            (stop (harmless-tool-by-name "harmless-tool-monitor-stop")))
        (should (eq (harmless-tool-class tool) 'shell))
        (should (eq (harmless-tool-class stop) 'shell))
        (should-not (harmless-tool-async tool))
        (should-not (harmless-tool-async stop))
        (should (functionp (harmless-tool-classify tool)))
        (should (functionp (harmless-tool-classify stop)))
        (should (equal (plist-get (harmless-tool-schema tool) :required)
                       ["pattern"]))
        (should (equal (plist-get (harmless-tool-schema stop) :required)
                       ["id"])))
      (should (eq (harmless-monitor-class
                   session '(:path "log.txt" :pattern "error"))
                  'read))
      (should (eq (harmless-monitor-class
                   session '(:command "echo hi" :pattern "error"))
                  'read))
      (should (eq (harmless-monitor-class
                   session '(:command "rm x" :pattern "error"))
                  'shell))
      (should (eq (harmless-monitor-class
                   session '(:path "log.txt" :command "rm x"))
                  'read))
      (should (eq (harmless-monitor-stop-class session '(:id "nope"))
                  'read))
      (should (eq (harmless-monitor-stop-class other '(:id "1")) 'read))
      (should (equal (harmless-turn--item-for
                      (harmless-tool-by-name "harmless-tool-monitor")
                      '(:path "log.txt" :pattern "error"))
                     "harmless-tool-monitor"))
      (should (equal (harmless-turn--item-for
                      (harmless-tool-by-name "harmless-tool-monitor")
                      '(:command "rm x" :pattern "error"))
                     "rm x"))
      (should (equal (harmless-turn--item-for
                      (harmless-tool-by-name "harmless-tool-monitor-stop")
                      '(:id "1"))
                     "harmless-tool-monitor-stop"))
      (should (equal (harmless-ui--tool-verb "harmless-tool-monitor") "watch"))
      (should (equal (harmless-ui--tool-verb "harmless-tool-monitor-stop") "stop"))
      (should (equal (harmless-ui--tool-summary
                      "harmless-tool-monitor" '(:pattern "error"))
                     "error"))
      (should (equal (harmless-ui--tool-summary
                      "harmless-tool-monitor-stop" '(:id "1"))
                     "1"))
      (should (equal (harmless-ui--tool-summary
                      "harmless-tool-monitor-stop" '(:id 2))
                     "2"))
      (should (equal (harmless-ui--tool-summary "harmless-tool-monitor" nil) ""))
      (should (= 0 (harmless-monitor-test-wakes session)))
      (should-not started)
      (should (equal (harmless-monitor-test-files repo) files))
      (should (eq root-monitor (file-exists-p "/monitor")))
      (with-temp-buffer
        (insert-file-contents readme)
        (should (equal (buffer-string) "keep\n")))
      (should-not (string-search "Monitor "
                                 (harmless-monitor-test-raw
                                  session "summary.json")))
      (should (equal (harmless-session-title session) "Keep"))
      (harmless-monitor-test-assert-session session)
      (harmless-monitor-test-assert-session other))))

(ert-deftest harmless-monitor-file-wakes-on-a-new-line ()
  (harmless-monitor-test-with _data
    (let* ((repo (make-temp-file "harmless-monitor-repo-" t))
           (log (expand-file-name "log.txt" repo))
           (slash (expand-file-name "slash.txt" repo))
           (cfile (expand-file-name "cfile.txt" repo))
           (readme (expand-file-name "README" repo))
           (session (harmless-monitor-test-session
                     :cwd repo :id "wake" :title "Keep"))
           (files nil)
           (monitor nil)
           (child nil))
      (harmless-monitor-test-write readme "keep\n")
      (harmless-monitor-test-write log "old\n")
      (harmless-monitor-test-write slash "keep\n")
      (harmless-monitor-test-write cfile "keep\n")
      (setq files (harmless-monitor-test-files repo))
      (should (equal (harmless-monitor-test-call
                      session "harmless-tool-monitor"
                      '(:pattern "error" :path "log.txt"))
                     "Watching 1"))
      (setq monitor (harmless-monitor-find session "1"))
      (harmless-monitor-test-assert-project-file
       (harmless-monitor-path monitor) repo)
      (should (equal (harmless-monitor-path monitor)
                     (expand-file-name "log.txt" repo)))
      (setq child (harmless-fork--create
                   session (harmless-session-messages session)))
      (should-not (harmless-monitor-find child "1"))
      (should (eq (harmless-monitor-status monitor) 'running))
      (harmless-monitor-poll monitor)
      (should (= 0 (harmless-monitor-test-wakes session)))
      (should (null (harmless-monitor-test-user-texts session)))
      (harmless-monitor-test-write log "fine\n" t)
      (harmless-monitor-poll monitor)
      (should (= 0 (harmless-monitor-test-wakes session)))
      (harmless-monitor-test-write log "err" t)
      (harmless-monitor-poll monitor)
      (should (= 0 (harmless-monitor-test-wakes session)))
      (should (equal (harmless-monitor-pending monitor) "err"))
      (harmless-monitor-test-write log "or\n" t)
      (harmless-monitor-poll monitor)
      (should (= 1 (harmless-monitor-test-wakes session)))
      (should (equal (harmless-monitor-test-user-texts session)
                     '("Monitor 1 matched:\nerror")))
      (harmless-monitor-test-assert-saved
       session "Monitor 1 matched:\nerror")
      (harmless-monitor-test-write log "error\nerror\n" t)
      (harmless-monitor-poll monitor)
      (should (= 3 (harmless-monitor-test-wakes session)))
      (should (= 3 (cl-count "Monitor 1 matched:\nerror"
                             (harmless-monitor-test-user-texts session)
                             :test #'equal)))
      (harmless-monitor-test-write log "error")
      (harmless-monitor-poll monitor)
      (should (= 3 (harmless-monitor-test-wakes session)))
      (harmless-monitor-test-write log "X\n" t)
      (harmless-monitor-poll monitor)
      (should (= 3 (harmless-monitor-test-wakes session)))
      (harmless-monitor-test-write log "error\n" t)
      (harmless-monitor-poll monitor)
      (should (= 4 (harmless-monitor-test-wakes session)))
      (delete-file log)
      (harmless-monitor-poll monitor)
      (should (= 4 (harmless-monitor-test-wakes session)))
      (should (eq (harmless-monitor-status monitor) 'running))
      (unwind-protect
          (progn
            (let ((harmless-monitor-interval 3600))
              (harmless-monitor-schedule-default monitor))
            (should (timerp (harmless-monitor-timer monitor)))
            (harmless-monitor-stop monitor)
            (should-not (harmless-monitor-timer monitor)))
        (when (timerp (harmless-monitor-timer monitor))
          (cancel-timer (harmless-monitor-timer monitor))))
      (harmless-monitor-test-write log "error\n")
      (harmless-monitor-poll monitor)
      (should (= 4 (harmless-monitor-test-wakes session)))
      (harmless-monitor-test-expect
       session "harmless-tool-monitor-stop" '(:id "1") "monitor 1 is not running")
      (should (equal (harmless-session-title session) "Keep"))
      (should (null (harmless-monitor-test-user-texts child)))
      (should-not (string-search
                   "Monitor "
                   (harmless-monitor-test-raw session "summary.json")))
      (let ((loaded (harmless-session-load (harmless-session-dir session))))
        (should-not (eq loaded session))
        (should-not (harmless-monitor-find loaded "1"))
        (should (cl-find "Monitor 1 matched:\nerror"
                         (harmless-session-messages loaded)
                         :test (lambda (text msg)
                                 (equal text (plist-get msg :content)))))
        (harmless-monitor-test-assert-session loaded))
      (let ((slash-session (harmless-monitor-test-session
                            :cwd repo :id "slash" :title "Keep")))
        (setq harmless-monitor-test-calls 0)
        (should (equal (harmless-monitor-test-call
                        slash-session "harmless-tool-monitor"
                        '(:pattern "/" :path "slash.txt"))
                       "Watching 1"))
        (harmless-monitor-poll (harmless-monitor-find slash-session "1"))
        (should (= 0 (harmless-monitor-test-wakes slash-session)))
        (harmless-monitor-test-write slash "/\n" t)
        (harmless-monitor-poll (harmless-monitor-find slash-session "1"))
        (should (equal (harmless-monitor-test-user-texts slash-session)
                       '("Monitor 1 matched:\n/")))
        (harmless-monitor-test-assert-saved
         slash-session "Monitor 1 matched:\n/")
        (harmless-monitor-test-assert-session slash-session))
      (let ((c-session (harmless-monitor-test-session
                        :cwd repo :id "cpath" :title "Keep")))
        (setq harmless-monitor-test-calls 0)
        (should (equal (harmless-monitor-test-call
                        c-session "harmless-tool-monitor"
                        '(:pattern "c:/" :path "cfile.txt"))
                       "Watching 1"))
        (harmless-monitor-test-write cfile "see c:/ here\n" t)
        (harmless-monitor-poll (harmless-monitor-find c-session "1"))
        (should (equal (harmless-monitor-test-user-texts c-session)
                       '("Monitor 1 matched:\nsee c:/ here")))
        (harmless-monitor-test-assert-saved
         c-session "Monitor 1 matched:\nsee c:/ here"))
      (let ((blank (harmless-monitor-test-session
                    :cwd repo :id "blank" :title "Keep")))
        (setq harmless-monitor-test-calls 0)
        (should (equal (harmless-monitor-test-call
                        blank "harmless-tool-monitor"
                        '(:pattern "^$" :path "README"))
                       "Watching 1"))
        (harmless-monitor-test-write readme "\n" t)
        (harmless-monitor-poll (harmless-monitor-find blank "1"))
        (should (equal (harmless-monitor-test-user-texts blank)
                       '("Monitor 1 matched:"))))
      (let ((busy (harmless-monitor-test-session
                   :cwd repo :id "busy" :title "Keep")))
        (setq harmless-monitor-test-calls 0)
        (should (equal (harmless-monitor-test-call
                        busy "harmless-tool-monitor"
                        '(:pattern "error" :path "log.txt"))
                       "Watching 1"))
        (setf (harmless-session-status busy) 'streaming)
        (harmless-monitor-test-write log "error\n" t)
        (harmless-monitor-poll (harmless-monitor-find busy "1"))
        (should (= 0 (harmless-monitor-test-wakes busy)))
        (should (eq (harmless-session-status busy) 'streaming))
        (should (equal (harmless-session-steers busy)
                       '("Monitor 1 matched:\nerror")))
        (should (null (harmless-monitor-test-user-texts busy)))
        (should (equal (harmless-session-title busy) "Keep")))
      (should (equal (harmless-monitor-test-files repo) files))
      (with-temp-buffer
        (insert-file-contents (expand-file-name "README" repo))
        (should (string-prefix-p "keep\n" (buffer-string))))
      (harmless-monitor-test-assert-session session)
      (harmless-monitor-test-assert-session child))))

(ert-deftest harmless-monitor-watches-a-shell-and-a-command ()
  (harmless-monitor-test-with _data
    (let* ((repo (make-temp-file "harmless-monitor-repo-" t))
           (session (harmless-monitor-test-session
                     :cwd repo :id "shell" :title "Keep"))
           (buf (generate-new-buffer " *harmless-monitor-shell*"))
           (killed nil)
           (scheduled nil)
           (monitor nil))
      (unwind-protect
          (progn
            (with-current-buffer buf (insert "error\n"))
            (push (harmless-shell-job--create
                   :id "1" :command "echo old" :buffer buf
                   :status 'running :background t)
                  (harmless-session-shell-jobs session))
            (setq harmless-monitor-kill-function
                  (lambda (_) (setq killed t)))
            (setq harmless-monitor-schedule-function
                  (lambda (item) (push (harmless-monitor-kind item)
                                       scheduled)))
            (should (equal (harmless-monitor-test-call
                            session "harmless-tool-monitor"
                            '(:pattern "error" :shell "  1  "))
                           "Watching 1"))
            (setq monitor (harmless-monitor-find session "1"))
            (should (equal (harmless-monitor-shell monitor) "1"))
            (should (eq (harmless-monitor-kind monitor) 'shell))
            (should (equal scheduled '(shell)))
            (harmless-monitor-poll monitor)
            (should (= 0 (harmless-monitor-test-wakes session)))
            (with-current-buffer buf
              (goto-char (point-max))
              (insert "fine\n"))
            (harmless-monitor-poll monitor)
            (should (= 0 (harmless-monitor-test-wakes session)))
            (with-current-buffer buf
              (goto-char (point-max))
              (insert "error\r\n"))
            (harmless-monitor-poll monitor)
            (should (equal (harmless-monitor-test-user-texts session)
                           '("Monitor 1 matched:\nerror")))
            (harmless-monitor-test-assert-saved
             session "Monitor 1 matched:\nerror")
            (should (equal (harmless-monitor-test-call
                            session "harmless-tool-monitor-stop" '(:id 1))
                           "Stopped monitor 1"))
            (should-not killed)
            (should (buffer-live-p buf))
            (should (eq (harmless-shell-job-status
                         (harmless-shell-find session "1"))
                        'running))
            (with-current-buffer buf
              (goto-char (point-max))
              (insert "error\n"))
            (harmless-monitor-poll monitor)
            (should (= 1 (harmless-monitor-test-wakes session)))
            (let* ((boom (harmless-monitor-test-session
                          :cwd repo :id "boom" :title "Keep"))
                   (seen nil)
                   (dir nil))
              (setq harmless-monitor-command-function
                    (lambda (&rest _) (error "boom")))
              (harmless-monitor-test-expect
               boom "harmless-tool-monitor" '(:pattern "error" :command "echo hi")
               "boom")
              (should (eq (harmless-monitor-status
                           (harmless-monitor-find boom "1"))
                          'stopped))
              (setq harmless-monitor-command-function
                    (lambda (&rest _) nil))
              (setq scheduled nil)
              (should (equal (harmless-monitor-test-call
                              boom "harmless-tool-monitor"
                              '(:pattern "error" :command "echo hi"))
                             "Watching 2"))
              (should-not scheduled)
              (setq harmless-monitor-test-calls 0)
              (harmless-monitor-ingest
               (harmless-monitor-find boom "2") "no\nerror\nerror\n")
              (should (= 2 (harmless-monitor-test-wakes boom)))
              (should (equal (harmless-monitor-test-user-texts boom)
                             '("Monitor 2 matched:\nerror"
                               "Monitor 2 matched:\nerror")))
              (let ((harmless-monitor-make-process-function
                     (lambda (&rest args)
                       (setq seen args)
                       (setq dir default-directory)
                       nil))
                    (harmless-monitor-command-function
                     #'harmless-monitor-command-default)
                    (harmless-monitor-kill-function
                     #'harmless-monitor-kill-default)
                    (cmd (harmless-monitor-test-session
                          :cwd repo :id "cmd" :title "Keep")))
                (should (equal (harmless-monitor-test-call
                                cmd "harmless-tool-monitor"
                                '(:pattern "error" :command "echo hi"))
                               "Watching 1"))
                (should (equal (plist-get seen :name) "harmless-monitor"))
                (should (eq (plist-get seen :connection-type) 'pipe))
                (should (cl-some
                         (lambda (arg)
                           (and (stringp arg)
                                (string-search "echo hi" arg)))
                         (plist-get seen :command)))
                (should (file-name-absolute-p dir))
                (should-not (harmless-filesystem-root-p dir))
                (should (harmless-same-directory-p dir repo))
                (should-not
                 (equal (directory-file-name (file-truename dir))
                        (directory-file-name (file-truename "/"))))
                (let ((cmd-monitor (harmless-monitor-find cmd "1")))
                  (should (buffer-live-p
                           (harmless-monitor-buffer cmd-monitor)))
                  (should (equal (harmless-monitor-test-call
                                  cmd "harmless-tool-monitor-stop" '(:id "1"))
                                 "Stopped monitor 1"))
                  (should-not (buffer-live-p
                               (harmless-monitor-buffer cmd-monitor))))
                (harmless-monitor-test-assert-session cmd)))
            (should (equal (harmless-session-title session) "Keep"))
            (harmless-monitor-test-assert-session session))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest harmless-monitor-permissions-and-plan ()
  (harmless-monitor-test-with _data
    (let* ((repo (make-temp-file "harmless-monitor-repo-" t))
           (log (expand-file-name "log.txt" repo))
           (asked nil)
           (started nil)
           (scheduled nil))
      (harmless-monitor-test-write log "old\n")
      (setq harmless-monitor-command-function
            (lambda (&rest _) (setq started t) nil))
      (setq harmless-monitor-schedule-function
            (lambda (monitor)
              (push (harmless-monitor-kind monitor) scheduled)))
      (let ((session (harmless-session-new
                      :cwd repo :id "file-ask" :title "Keep" :model "m"
                      :provider
                      (harmless-monitor-make-fake
                       :name "fake" :host "none"
                       :script (harmless-monitor-test-tool-script
                                '(:tool-call "1" "harmless-tool-monitor"
                                  "{\"pattern\":\"error\",\"path\":\"log.txt\"}"))))))
        (setq harmless-perm-ask-function
              (lambda (&rest _) (error "permission asked")))
        (setq harmless-monitor-test-calls 0)
        (harmless-turn-run session "go")
        (should (equal scheduled '(file)))
        (should-not started)
        (should (equal (harmless-monitor-test-tool-texts session)
                       '("Watching 1")))
        (should (equal (harmless-session-title session) "Keep")))
      (setq scheduled nil)
      (setq started nil)
      (let ((session (harmless-session-new
                      :cwd repo :id "echo-ask" :title "Keep" :model "m"
                      :permission-mode 'ask
                      :provider
                      (harmless-monitor-make-fake
                       :name "fake" :host "none"
                       :script (harmless-monitor-test-tool-script
                                '(:tool-call "1" "harmless-tool-monitor"
                                  "{\"pattern\":\"error\",\"command\":\"echo hi\"}"))))))
        (setq harmless-perm-ask-function
              (lambda (&rest _) (error "permission asked")))
        (setq harmless-monitor-test-calls 0)
        (harmless-turn-run session "go")
        (should started)
        (should-not scheduled)
        (should (equal (harmless-monitor-test-tool-texts session)
                       '("Watching 1"))))
      (setq started nil)
      (setq asked nil)
      (let ((session (harmless-session-new
                      :cwd repo :id "rm-ask" :title "Keep" :model "m"
                      :provider
                      (harmless-monitor-make-fake
                       :name "fake" :host "none"
                       :script (harmless-monitor-test-tool-script
                                '(:tool-call "1" "harmless-tool-monitor"
                                  "{\"pattern\":\"error\",\"command\":\"rm x\"}"))))))
        (setq harmless-perm-ask-function
              (lambda (_session class item callback)
                (push (list class item) asked)
                (funcall callback 'deny)))
        (setq harmless-monitor-test-calls 0)
        (harmless-turn-run session "go")
        (should (equal asked '((shell "rm x"))))
        (should-not started)
        (should (equal (harmless-monitor-test-tool-texts session)
                       '("Error: user denied this tool call"))))
      (setq started nil)
      (setq asked nil)
      (let ((session (harmless-session-new
                      :cwd repo :id "rm-edits" :title "Keep" :model "m"
                      :permission-mode 'accept-edits
                      :provider
                      (harmless-monitor-make-fake
                       :name "fake" :host "none"
                       :script (harmless-monitor-test-tool-script
                                '(:tool-call "1" "harmless-tool-monitor"
                                  "{\"pattern\":\"error\",\"command\":\"rm x\"}"))))))
        (setq harmless-perm-ask-function
              (lambda (_session class item callback)
                (push (list class item) asked)
                (funcall callback 'allow)))
        (setq harmless-monitor-test-calls 0)
        (harmless-turn-run session "go")
        (should (equal asked '((shell "rm x"))))
        (should started)
        (should (equal (harmless-monitor-test-tool-texts session)
                       '("Watching 1"))))
      (setq started nil)
      (let ((session (harmless-session-new
                      :cwd repo :id "rm-always" :title "Keep" :model "m"
                      :permission-mode 'always-approve
                      :provider
                      (harmless-monitor-make-fake
                       :name "fake" :host "none"
                       :script (harmless-monitor-test-tool-script
                                '(:tool-call "1" "harmless-tool-monitor"
                                  "{\"pattern\":\"error\",\"command\":\"rm x\"}"))))))
        (setq harmless-perm-ask-function
              (lambda (&rest _) (error "permission asked")))
        (setq harmless-monitor-test-calls 0)
        (harmless-turn-run session "go")
        (should started)
        (should (equal (harmless-monitor-test-tool-texts session)
                       '("Watching 1"))))
      (setq started nil)
      (let ((session (harmless-session-new
                      :cwd repo :id "plan-file" :title "Keep" :model "m"
                      :permission-mode 'always-approve
                      :provider
                      (harmless-monitor-make-fake
                       :name "fake" :host "none"
                       :script (harmless-monitor-test-tool-script
                                '(:tool-call "1" "harmless-tool-monitor"
                                  "{\"pattern\":\"error\",\"path\":\"log.txt\"}"))))))
        (setf (harmless-session-plan-mode session) t)
        (setq harmless-perm-ask-function
              (lambda (&rest _) (error "permission asked")))
        (setq harmless-monitor-test-calls 0)
        (harmless-turn-run session "go")
        (should (harmless-session-plan-mode session))
        (should (equal (harmless-monitor-test-tool-texts session)
                       '("Watching 1"))))
      (setq started nil)
      (let ((session (harmless-session-new
                      :cwd repo :id "plan-echo" :title "Keep" :model "m"
                      :permission-mode 'always-approve
                      :provider
                      (harmless-monitor-make-fake
                       :name "fake" :host "none"
                       :script (harmless-monitor-test-tool-script
                                '(:tool-call "1" "harmless-tool-monitor"
                                  "{\"pattern\":\"error\",\"command\":\"echo hi\"}"))))))
        (setf (harmless-session-plan-mode session) t)
        (setq harmless-perm-ask-function
              (lambda (&rest _) (error "permission asked")))
        (setq harmless-monitor-test-calls 0)
        (harmless-turn-run session "go")
        (should started)
        (should (equal (harmless-monitor-test-tool-texts session)
                       '("Watching 1"))))
      (setq started nil)
      (let ((session (harmless-session-new
                      :cwd repo :id "plan-rm" :title "Keep" :model "m"
                      :permission-mode 'always-approve
                      :provider
                      (harmless-monitor-make-fake
                       :name "fake" :host "none"
                       :script (harmless-monitor-test-tool-script
                                '(:tool-call "1" "harmless-tool-monitor"
                                  "{\"pattern\":\"error\",\"command\":\"rm x\"}"))))))
        (setf (harmless-session-plan-mode session) t)
        (setq harmless-perm-ask-function
              (lambda (&rest _) (error "permission asked")))
        (setq harmless-monitor-test-calls 0)
        (harmless-turn-run session "go")
        (should-not started)
        (should (harmless-session-plan-mode session))
        (should (equal (harmless-monitor-test-tool-texts session)
                       (list harmless-shell-plan-refusal))))
      (let ((session (harmless-monitor-test-session
                      :cwd repo :id "plan-direct" :title "Keep")))
        (setf (harmless-session-plan-mode session) t)
        (should (equal (harmless-monitor-plan-refuses-p
                        session (harmless-tool-by-name "harmless-tool-monitor")
                        '(:pattern "error" :command "rm x"))
                       harmless-shell-plan-refusal))
        (should-not (harmless-monitor-plan-refuses-p
                     session (harmless-tool-by-name "harmless-tool-monitor")
                     '(:pattern "error" :command "echo hi")))
        (should-not (harmless-monitor-plan-refuses-p
                     session (harmless-tool-by-name "harmless-tool-monitor")
                     '(:pattern "error" :path "log.txt")))
        (should-not (harmless-monitor-plan-refuses-p
                     session (harmless-tool-by-name "harmless-tool-monitor")
                     '(:pattern "error" :path "log.txt" :command "rm x")))
        (harmless-monitor-test-expect
         session "harmless-tool-monitor" '(:pattern "error" :command "rm x")
         "Plan mode is on. A shell command that can write is refused")
        (should-not started)))))

(ert-deftest harmless-monitor-read-only-fork-and-detached ()
  (harmless-monitor-test-with _data
    (let* ((repo (make-temp-file "harmless-monitor-repo-" t))
           (log (expand-file-name "log.txt" repo))
           (child (harmless-monitor-test-session
                   :cwd repo :id "child" :source 'agent :parent-id "p"
                   :title "Reader"))
           (killed nil))
      (harmless-monitor-test-write log "old\n")
      (setf (harmless-session-agent child)
            (list :allow 'all :sandbox 'read-only :callback nil
                  :turns 0 :max 8 :error nil :prompt nil :task nil))
      (let ((offered (mapcar #'harmless-tool-name
                             (harmless-tools-for-session child))))
        (should (member "harmless-tool-monitor" offered))
        (should (member "harmless-tool-monitor-stop" offered))
        (should (member "harmless-tool-read-file" offered))
        (should-not (member "harmless-tool-write-file" offered))
        (should-not (member "harmless-tool-computer-screenshot" offered)))
      (should-not (harmless-agent-refuse-call
                   child (harmless-tool-by-name "harmless-tool-monitor")
                   '(:pattern "error" :path "log.txt")))
      (should-not (harmless-agent-refuse-call
                   child (harmless-tool-by-name "harmless-tool-monitor")
                   '(:pattern "error" :command "echo hi")))
      (should (equal (harmless-agent-refuse-call
                      child (harmless-tool-by-name "harmless-tool-monitor")
                      '(:pattern "error" :command "rm x"))
                     harmless-agent-read-only-refusal))
      (setq harmless-monitor-command-function
            (lambda (&rest _) nil))
      (should (equal (harmless-monitor-test-call
                      child "harmless-tool-monitor"
                      '(:pattern "error" :path "log.txt"))
                     "Watching 1"))
      (should-not (harmless-agent-refuse-call
                   child (harmless-tool-by-name "harmless-tool-monitor-stop")
                   '(:id "1")))
      (should (equal (harmless-monitor-test-call
                      child "harmless-tool-monitor" '(:pattern "error" :command "echo hi"))
                     "Watching 2"))
      (should (eq (harmless-monitor-stop-class child '(:id "2")) 'shell))
      (should (equal (harmless-agent-refuse-call
                      child (harmless-tool-by-name "harmless-tool-monitor-stop")
                      '(:id "2"))
                     harmless-agent-read-only-refusal))
      (setq harmless-monitor-kill-function
            (lambda (_) (setq killed t)))
      (let ((stop-session (harmless-session-new
                           :cwd repo :id "stop-ro" :title "Keep" :model "m"
                           :source 'agent :parent-id "p"
                           :permission-mode 'always-approve
                           :provider
                           (harmless-monitor-make-fake
                            :name "fake" :host "none"
                            :script (harmless-monitor-test-tool-script
                                     '(:tool-call "1" "harmless-tool-monitor-stop"
                                       "{\"id\":\"1\"}"))))))
        (setf (harmless-session-agent stop-session)
              (list :allow 'all :sandbox 'read-only))
        (should (equal (harmless-monitor-test-call
                        stop-session "harmless-tool-monitor"
                        '(:pattern "error" :command "echo hi"))
                       "Watching 1"))
        (setq harmless-monitor-test-calls 0)
        (harmless-turn-run stop-session "go")
        (should-not killed)
        (should (eq (harmless-monitor-status
                     (harmless-monitor-find stop-session "1"))
                    'running))
        (should (equal (harmless-monitor-test-tool-texts stop-session)
                       (list harmless-agent-read-only-refusal))))
      (setf (plist-get (harmless-session-agent child) :sandbox) 'workspace)
      (let ((offered (mapcar #'harmless-tool-name
                             (harmless-tools-for-session child))))
        (should (member "harmless-tool-monitor" offered))
        (should (member "harmless-tool-computer-screenshot" offered)))
      (setf (plist-get (harmless-session-agent child) :allow) '("harmless-tool-read-file"))
      (setf (plist-get (harmless-session-agent child) :sandbox) 'read-only)
      (should-not (member "harmless-tool-monitor"
                          (mapcar #'harmless-tool-name
                                  (harmless-tools-for-session child))))
      (should (equal (harmless-agent-refuse-call
                      child (harmless-tool-by-name "harmless-tool-monitor")
                      '(:pattern "error" :path "log.txt"))
                     "Error: this subagent cannot use harmless-tool-monitor"))
      (let* ((buf (generate-new-buffer " *harmless-monitor-detached*"))
             (detached (harmless-monitor-test-session
                        :detached t :id "det" :title "Keep")))
        (unwind-protect
            (progn
              (with-current-buffer buf (insert "old\n"))
              (push (harmless-shell-job--create
                     :id "7" :command "echo old" :buffer buf
                     :status 'running :background t)
                    (harmless-session-shell-jobs detached))
              (setq harmless-monitor-test-calls 0)
              (should (equal (harmless-monitor-test-call
                              detached "harmless-tool-monitor"
                              '(:pattern "error" :shell 7))
                             "Watching 1"))
              (with-current-buffer buf
                (goto-char (point-max))
                (insert "error\n"))
              (harmless-monitor-poll
               (harmless-monitor-find detached "1"))
              (should (equal (harmless-monitor-test-user-texts detached)
                             '("Monitor 1 matched:\nerror")))
              (harmless-monitor-test-assert-saved
               detached "Monitor 1 matched:\nerror")
              (should (equal (harmless-monitor-test-call
                              detached "harmless-tool-monitor-stop" '(:id "1"))
                             "Stopped monitor 1"))
              (should (eq (harmless-shell-job-status
                           (harmless-shell-find detached "7"))
                          'running))
              (harmless-monitor-test-assert-session detached))
          (when (buffer-live-p buf)
            (kill-buffer buf))))
      (harmless-monitor-test-assert-session child))))
