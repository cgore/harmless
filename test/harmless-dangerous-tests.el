;;; harmless-dangerous-tests.el --- Tests for the command denylist -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(cl-defstruct (harmless-dangerous-fake
               (:include harmless-provider)
               (:constructor harmless-dangerous-make-fake))
  script)

(cl-defmethod harmless-provider-complete
    ((provider harmless-dangerous-fake) _messages _tools callback)
  (when (harmless-dangerous-fake-script provider)
    (funcall (harmless-dangerous-fake-script provider) _messages callback))
  nil)

(defvar harmless-dangerous-test-calls 0
  "Provider calls in the current denylist test.")

(defvar harmless-dangerous-test-asked nil
  "Permission questions recorded by the current denylist test.")

(defvar harmless-dangerous-test-commands nil
  "Command monitors started by the current denylist test.")

(defun harmless-dangerous-test-script (event)
  "Return a provider script that sends EVENT once, then text."
  (lambda (_messages callback)
    (setq harmless-dangerous-test-calls
          (1+ harmless-dangerous-test-calls))
    (if (= harmless-dangerous-test-calls 1)
        (progn
          (funcall callback event)
          (funcall callback '(:stop "tool_calls")))
      (funcall callback '(:text "done"))
      (funcall callback '(:stop "stop")))))

(defmacro harmless-dangerous-test-with (data &rest body)
  "Bind a temp data directory DATA, then run BODY."
  (declare (indent 1))
  `(let* ((,data (make-temp-file "harmless-danger-data-" t))
          (harmless-directory ,data)
          (harmless--sessions (make-hash-table :test 'equal))
          (harmless-monitor--table (make-hash-table :test 'eq))
          (harmless-dangerous-test-calls 0)
          (harmless-dangerous-test-asked nil)
          (harmless-dangerous-test-commands nil)
          (harmless-compact-threshold nil)
          (harmless-permission-rules nil)
          (harmless-permission-denies nil)
          (harmless-monitor-schedule-function #'ignore)
          (harmless-monitor-kill-function #'ignore)
          (harmless-monitor-command-function
           (lambda (&rest args)
             (push args harmless-dangerous-test-commands)
             nil))
          (harmless-perm-ask-function
           (lambda (_session class item callback)
             (push (list class item) harmless-dangerous-test-asked)
             (funcall callback 'deny))))
     ,@body))

(defun harmless-dangerous-test-session (&rest args)
  "Return a session.  ARGS are passed to `harmless-session-new'."
  (apply #'harmless-session-new
         (append args
                 (list :provider
                       (harmless-dangerous-make-fake
                        :name "fake" :host "none"
                        :script (harmless-dangerous-test-script
                                 '(:text "idle")))
                       :model "m"
                       :title "Keep"))))

(defun harmless-dangerous-test-run (session name args)
  "Run tool NAME with ARGS on SESSION and wait until it is idle."
  (setq harmless-dangerous-test-calls 0)
  (setq harmless-dangerous-test-asked nil)
  (setf (harmless-dangerous-fake-script
         (harmless-session-provider session))
        (harmless-dangerous-test-script
         (list :tool-call "1" name (harmless-json-text args))))
  (harmless-turn-run session "go")
  (let ((n 0))
    (while (and (not (memq (harmless-session-status session)
                           '(idle error)))
                (< n 50))
      (setq n (1+ n))
      (accept-process-output nil 0.1))))

(defun harmless-dangerous-test-last (session)
  "Return the latest tool result stored on SESSION."
  (car (last (cl-loop for msg in (harmless-session-messages session)
                      when (eq (plist-get msg :role) :tool)
                      collect (plist-get msg :content)))))

(defun harmless-dangerous-test-assert-session (session)
  "Require SESSION's directory to stay off the filesystem root."
  (let ((dir (harmless-session-dir session)))
    (should (file-name-absolute-p dir))
    (should-not (harmless-filesystem-root-p dir))
    (should (harmless-directory-strictly-under-p dir harmless-directory))
    (should-not (equal (file-truename dir)
                       (file-truename (expand-file-name "/"))))))

(ert-deftest harmless-dangerous-commands-match-names-and-roots ()
  (let ((slash (file-exists-p "/.harmless"))
        (cdrive (file-exists-p "c:/.harmless")))
    (should-not (harmless-shell-dangerous-name nil))
    (should-not (harmless-shell-dangerous-name ""))
    (should-not (harmless-shell-dangerous-name "   "))
    (should-not (harmless-shell-dangerous-name "\n"))
    (should-not (harmless-shell-dangerous-name "echo hi"))
    (should-not (harmless-shell-dangerous-name "echo shutdown"))
    (should (equal (harmless-shell-dangerous-name "shutdown -h now")
                   "shutdown"))
    (should (equal (harmless-shell-dangerous-name "SHUTDOWN")
                   "shutdown"))
    (should (equal (harmless-shell-dangerous-name
                    "/usr/sbin/shutdown")
                   "shutdown"))
    (should (equal (harmless-shell-dangerous-name "shutdown.exe")
                   "shutdown"))
    (should (equal (harmless-shell-dangerous-name
                    "sudo -n shutdown")
                   "shutdown"))
    (should (equal (harmless-shell-dangerous-name
                    "doas reboot")
                   "reboot"))
    (should (equal (harmless-shell-dangerous-name
                    "echo hi | shutdown")
                   "shutdown"))
    (should (equal (harmless-shell-dangerous-name "mkfs.ext4 /dev/sda")
                   "mkfs.ext4"))
    (should (equal (harmless-shell-dangerous-name "diskpart")
                   "diskpart"))
    (should (equal (harmless-shell-dangerous-name "rm -rf /") "rm"))
    (should (equal (harmless-shell-dangerous-name "rm -rf /*") "rm"))
    (should (equal (harmless-shell-dangerous-name "rm -rf ~") "rm"))
    (should (equal (harmless-shell-dangerous-name "rm -rf ~/") "rm"))
    (should (equal (harmless-shell-dangerous-name "rm -rf c:/") "rm"))
    (should (equal (harmless-shell-dangerous-name "rm -rf C:\\") "rm"))
    (should (equal (harmless-shell-dangerous-name "del c:/") "del"))
    (should-not (harmless-shell-dangerous-name "rm -rf c:/temp/x"))
    (should-not (harmless-shell-dangerous-name "rm note.txt"))
    (should-not (harmless-shell-dangerous-name "rmdir sub"))
    (let ((harmless-dangerous-commands nil))
      (should-not (harmless-shell-dangerous-name "shutdown"))
      (should (equal (harmless-shell-dangerous-name "rm -rf /") "rm")))
    (let ((harmless-dangerous-commands '("echo")))
      (should (equal (harmless-shell-dangerous-name "echo hi") "echo")))
    (should (eq slash (file-exists-p "/.harmless")))
    (should (eq cdrive (file-exists-p "c:/.harmless")))))

(ert-deftest harmless-dangerous-commands-refuse-before-the-question ()
  (harmless-dangerous-test-with _data
    (let* ((repo (make-temp-file "harmless-danger-repo-" t))
           (keep (expand-file-name "keep.txt" repo))
           (slash (file-exists-p "/keep.txt"))
           (session (harmless-dangerous-test-session
                     :cwd repo :id "danger")))
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region "stay\n" nil keep nil 'silent))
      (should (file-regular-p keep))
      (harmless-dangerous-test-run
       session "run_shell" '(:command "shutdown -h now"))
      (should-not harmless-dangerous-test-asked)
      (should (equal (harmless-dangerous-test-last session)
                     "Error: dangerous command refused: shutdown"))
      (should (equal (with-temp-buffer
                       (insert-file-contents keep)
                       (buffer-string))
                     "stay\n"))
      (harmless-dangerous-test-run
       session "run_shell" '(:command "rm -rf /"))
      (should-not harmless-dangerous-test-asked)
      (should (equal (harmless-dangerous-test-last session)
                     "Error: dangerous command refused: rm"))
      (should (file-regular-p keep))
      (should (file-directory-p "/"))
      (harmless-dangerous-test-run
       session "run_shell" '(:command "rm keep.txt"))
      (should (equal harmless-dangerous-test-asked
                     '((shell "rm keep.txt"))))
      (should (equal (harmless-dangerous-test-last session)
                     "Error: user denied this tool call"))
      (should (file-regular-p keep))
      (let ((harmless-dangerous-commands '("echo")))
        (harmless-perm-set-rules session '("Bash(*)") nil)
        (setf (harmless-session-permission-mode session)
              'always-approve)
        (harmless-dangerous-test-run
         session "run_shell" '(:command "echo hi"))
        (should-not harmless-dangerous-test-asked)
        (should (equal (harmless-dangerous-test-last session)
                       "Error: dangerous command refused: echo")))
      (setf (harmless-session-permission-mode session) 'ask)
      (setf (harmless-session-plan-mode session) t)
      (harmless-dangerous-test-run
       session "run_shell" '(:command "shutdown -h now"))
      (should-not harmless-dangerous-test-asked)
      (should (equal (harmless-dangerous-test-last session)
                     harmless-shell-plan-refusal))
      (setf (harmless-session-plan-mode session) nil)
      (setq harmless-dangerous-test-commands nil)
      (harmless-dangerous-test-run
       session "monitor" '(:pattern "x" :command "shutdown"))
      (should-not harmless-dangerous-test-commands)
      (should (equal (harmless-dangerous-test-last session)
                     "Error: dangerous command refused: shutdown"))
      (harmless-dangerous-test-run
       session "monitor" '(:pattern "nope" :path "keep.txt"))
      (should-not harmless-dangerous-test-commands)
      (should (string-match-p
               "\\`Watching "
               (harmless-dangerous-test-last session)))
      (should (eq slash (file-exists-p "/keep.txt")))
      (should (equal (harmless-session-title session) "Keep"))
      (harmless-dangerous-test-assert-session session))))
