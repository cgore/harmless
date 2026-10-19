;;; harmless-perm-tests.el --- Tests for allow and deny rules -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(cl-defstruct (harmless-perm-fake
               (:include harmless-provider)
               (:constructor harmless-perm-make-fake))
  script)

(cl-defmethod harmless-provider-complete ((provider harmless-perm-fake)
                                          _messages _tools callback)
  (when (harmless-perm-fake-script provider)
    (funcall (harmless-perm-fake-script provider) _messages callback))
  nil)

(defvar harmless-perm-test-calls 0
  "Provider calls in the current permission test.")

(defvar harmless-perm-test-asked nil
  "Permission questions recorded by the current test.")

(defvar harmless-perm-test-commands nil
  "Command monitors started by the current test.")

(defvar harmless-perm-test-fetches nil
  "URLs fetched by the current test.")

(defun harmless-perm-test-tool-script (event)
  "Return a provider script that sends EVENT once, then text."
  (lambda (_messages callback)
    (setq harmless-perm-test-calls (1+ harmless-perm-test-calls))
    (if (= harmless-perm-test-calls 1)
        (progn
          (funcall callback event)
          (funcall callback '(:stop "tool_calls")))
      (funcall callback '(:text "done"))
      (funcall callback '(:stop "stop")))))

(defmacro harmless-perm-test-with (data &rest body)
  "Bind a temp data directory DATA, then run BODY.
No command monitor starts and no page is fetched unless BODY asks."
  (declare (indent 1))
  `(let* ((,data (make-temp-file "harmless-perm-data-" t))
          (harmless-directory ,data)
          (harmless--sessions (make-hash-table :test 'equal))
          (harmless-monitor--table (make-hash-table :test 'eq))
          (harmless-perm-test-calls 0)
          (harmless-perm-test-asked nil)
          (harmless-perm-test-commands nil)
          (harmless-perm-test-fetches nil)
          (harmless-compact-threshold nil)
          (harmless-permission-rules nil)
          (harmless-permission-denies nil)
          (harmless-monitor-schedule-function #'ignore)
          (harmless-monitor-kill-function #'ignore)
          (harmless-monitor-command-function
           (lambda (&rest args)
             (push args harmless-perm-test-commands)
             nil))
          (harmless-web-retrieve-function
           (lambda (url)
             (push url harmless-perm-test-fetches)
             "fetched page"))
          (harmless-perm-ask-function
           (lambda (_session class item callback)
             (push (list class item) harmless-perm-test-asked)
             (funcall callback 'deny))))
     ,@body))

(defun harmless-perm-test-session (&rest args)
  "Return a session.  ARGS are passed to `harmless-session-new'."
  (apply #'harmless-session-new
         (append args
                 (list :provider
                       (harmless-perm-make-fake
                        :name "fake" :host "none"
                        :script (harmless-perm-test-tool-script
                                 '(:text "idle")))
                       :model "m"
                       :title "Keep"))))

(defun harmless-perm-test-signal (function text)
  "Assert FUNCTION signals an error whose message is TEXT."
  (let ((got (condition-case err
                 (progn (funcall function) nil)
               (error (error-message-string err)))))
    (should (equal got text))))

(defun harmless-perm-test-keeps (session allows denies text)
  "Assert set-rules of ALLOWS and DENIES signals TEXT and keeps SESSION."
  (let ((before (copy-tree (harmless-session-rules session))))
    (harmless-perm-test-signal
     (lambda () (harmless-perm-set-rules session allows denies))
     text)
    (should (equal (harmless-session-rules session) before))))

(defun harmless-perm-test-texts (session)
  "Return the rule texts stored on SESSION."
  (mapcar (lambda (rule) (plist-get rule :text))
          (harmless-session-rules session)))

(defun harmless-perm-test-patterns (session)
  "Return the patterns stored on SESSION."
  (mapcar (lambda (rule) (plist-get rule :pattern))
          (harmless-session-rules session)))

(defun harmless-perm-test-refusal (session name args)
  "Return the rule refusal for tool NAME on SESSION with ARGS."
  (harmless-perm-rule-refusal
   session (harmless-tool-by-name name) args))

(defun harmless-perm-test-allows-p (session name args)
  "Return whether an allow rule covers tool NAME on SESSION."
  (harmless-perm-rule-allows-p
   session (harmless-tool-by-name name) args))

(defun harmless-perm-test-tool-texts (session)
  "Return tool results stored on SESSION."
  (cl-loop for msg in (harmless-session-messages session)
           when (eq (plist-get msg :role) :tool)
           collect (plist-get msg :content)))

(defun harmless-perm-test-last-tool (session)
  "Return the latest tool result stored on SESSION."
  (car (last (harmless-perm-test-tool-texts session))))

(defun harmless-perm-test-wait (session)
  "Wait until SESSION is idle or the wait gives up."
  (let ((n 0))
    (while (and (not (memq (harmless-session-status session)
                           '(idle error)))
                (< n 50))
      (setq n (1+ n))
      (accept-process-output nil 0.1))))

(defun harmless-perm-test-run (session name args &optional decision)
  "Run tool NAME with ARGS on SESSION and wait until it is idle.
DECISION is the answer to a question.  Nil denies."
  (setq harmless-perm-test-calls 0)
  (setq harmless-perm-test-asked nil)
  (setq harmless-perm-ask-function
        (lambda (_session class item callback)
          (push (list class item) harmless-perm-test-asked)
          (funcall callback (or decision 'deny))))
  (setf (harmless-perm-fake-script (harmless-session-provider session))
        (harmless-perm-test-tool-script
         (list :tool-call "1" name (harmless-json-text args))))
  (harmless-turn-run session "go")
  (harmless-perm-test-wait session))

(defun harmless-perm-test-assert-session (session)
  "Require SESSION's directory to stay off the filesystem root."
  (let ((dir (harmless-session-dir session)))
    (should (file-name-absolute-p dir))
    (should-not (harmless-filesystem-root-p dir))
    (should (harmless-directory-strictly-under-p dir harmless-directory))
    (should-not (equal (file-truename dir)
                       (file-truename (expand-file-name "/"))))))

(defun harmless-perm-test-read (path)
  "Return the contents of PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun harmless-perm-test-assert-file (path repo contents)
  "Require PATH to hold CONTENTS inside REPO and not under /."
  (setq path (expand-file-name path))
  (setq repo (file-name-as-directory (expand-file-name repo)))
  (should (file-name-absolute-p path))
  (should (file-regular-p path))
  (should-not (harmless-filesystem-root-p path))
  (should (string-prefix-p repo path))
  (should-not (equal (file-truename path)
                     (file-truename
                      (expand-file-name
                       (file-name-nondirectory path) "/"))))
  (should (equal (harmless-perm-test-read path) contents)))

(defun harmless-perm-test-write (path contents)
  "Write CONTENTS to PATH and return PATH."
  (make-directory (file-name-directory path) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region contents nil path nil 'silent))
  path)

(defun harmless-perm-test-raw (session name)
  "Return NAME from SESSION's directory and require that path."
  (let ((file (expand-file-name name (harmless-session-dir session))))
    (should (file-name-absolute-p file))
    (should-not (harmless-filesystem-root-p file))
    (should (harmless-directory-strictly-under-p
             (file-name-directory file) harmless-directory))
    (should-not (equal (file-truename file)
                       (file-truename (expand-file-name name "/"))))
    (if (file-regular-p file)
        (harmless-perm-test-read file)
      "")))

(ert-deftest harmless-perm-rules-refuse-bad-text ()
  (harmless-perm-test-with data
    (let* ((repo (make-temp-file "harmless-perm-repo-" t))
           (slash (file-exists-p "/harmless-perm-rule"))
           (cdrive (file-exists-p "c:/harmless-perm-rule"))
           (session (harmless-perm-test-session :cwd repo :id "rules")))
      (should-not (harmless-session-rules session))
      (dolist (case '((nil "rule must be a string")
                      (:false "rule must be a string")
                      (42 "rule must be a string")
                      ("" "rule is empty")
                      ("   " "rule is empty")
                      ("\n" "rule is empty")
                      ("nope" "rule is not a rule")
                      ("Bash()" "rule pattern is empty")
                      ("Bash(   )" "rule pattern is empty")
                      ("WebFetch(example.com)" "rule domain is missing")
                      ("WebFetch(domain:)" "rule domain is empty")
                      ("WebFetch(domain:   )" "rule domain is empty")
                      ("WebFetch(domain:/)" "rule domain is not a domain")
                      ("WebFetch(domain:c:/)" "rule domain is not a domain")
                      ("WebFetch(domain:c:)" "rule domain is not a domain")
                      ("WebFetch(domain:.com)" "rule domain is not a domain")
                      ("WebFetch(domain:com.)" "rule domain is not a domain")
                      ("WebFetch(domain:ex ample)"
                       "rule domain is not a domain")
                      ("WebFetch(domain:ex\\ample)"
                       "rule domain is not a domain")
                      ("Edit()" "rule pattern is empty")
                      ("Edit(   )" "rule pattern is empty")
                      ("Edit(/)" "rule pattern is not a path")
                      ("Edit(c:/)" "rule pattern is not a path")
                      ("Edit(/c:/src)" "rule pattern is not a path")
                      ("Edit(D:/src)" "rule pattern is not a path")
                      ("Edit(../x)" "rule pattern escapes the project")
                      ("Edit(/../x)" "rule pattern escapes the project")
                      ("Edit(src/../x)" "rule pattern escapes the project")
                      ("Edit(~)" "rule pattern escapes the project")
                      ("Edit(~/x)" "rule pattern escapes the project")
                      ("Edit(src/~)" "rule pattern escapes the project")))
        (harmless-perm-test-keeps
         session (list (car case)) nil (cadr case))
        (harmless-perm-test-keeps
         session nil (list (car case)) (cadr case)))
      (harmless-perm-test-keeps session "Bash(*)" nil
                                "rules must be a list")
      (harmless-perm-test-keeps session nil ["Edit(src/**)"]
                                "rules must be a list")
      (harmless-perm-set-rules session '("Bash(*)") nil)
      (harmless-perm-test-keeps
       session '("Bash(*)" "nope") nil "rule is not a rule")
      (should (equal (harmless-perm-test-texts session) '("Bash(*)")))
      (let ((harmless-permission-rules '("nope"))
            (before (directory-files data t))
            (got nil))
        (setq got
              (condition-case err
                  (progn
                    (harmless-perm-test-session
                     :cwd repo :id "bad-rules")
                    nil)
                (error (error-message-string err))))
        (should (equal got "rule is not a rule"))
        (should-not (harmless-session-get "bad-rules"))
        (should (equal before (directory-files data t))))
      (let ((harmless-permission-rules "Bash(*)")
            (before (directory-files data t)))
        (harmless-perm-test-signal
         (lambda ()
           (harmless-perm-test-session :cwd repo :id "bad-list"))
         "rules must be a list")
        (should-not (harmless-session-get "bad-list"))
        (should (equal before (directory-files data t))))
      (should (eq slash (file-exists-p "/harmless-perm-rule")))
      (should (eq cdrive (file-exists-p "c:/harmless-perm-rule")))
      (harmless-perm-test-assert-session session))))

(ert-deftest harmless-perm-rules-match-commands-paths-and-domains ()
  (harmless-perm-test-with _data
    (let* ((repo (make-temp-file "harmless-perm-repo-" t))
           (session (harmless-perm-test-session :cwd repo :id "match"))
           (abs (expand-file-name "src/a.txt" repo)))
      (let ((harmless-permission-rules '("  Bash(echo *)  "
                                         "Bash(/)"
                                         "Bash(c:/)"))
            (harmless-permission-denies '("Edit(/src/**)"
                                          "WebFetch(domain:Example.COM)")))
        (setq session (harmless-perm-test-session :cwd repo :id "copied"))
        (should (equal (harmless-perm-test-texts session)
                       '("Bash(echo *)" "Bash(/)" "Bash(c:/)"
                         "Edit(/src/**)"
                         "WebFetch(domain:Example.COM)")))
        (should (equal (harmless-perm-test-patterns session)
                       '("echo *" "/" "c:/" "src/**" "example.com"))))
      (harmless-perm-set-rules
       session nil '("Bash(npm run *)" "Bash(echo *)"))
      (should (equal (harmless-perm-test-refusal
                      session "run_shell" '(:command "npm run test"))
                     "Error: Permission rule denied Bash(npm run *)"))
      (should-not (harmless-perm-test-refusal
                   session "run_shell" '(:command "npm run")))
      (should (equal (harmless-perm-test-refusal
                      session "run_shell" '(:command "  echo hi  "))
                     "Error: Permission rule denied Bash(echo *)"))
      (should-not (harmless-perm-test-refusal
                   session "run_shell" '(:command "echo")))
      (harmless-perm-set-rules session nil '("Bash(*)"))
      (should (harmless-perm-test-refusal
               session "run_shell" '(:command "echo hi")))
      (harmless-perm-set-rules session nil '("Edit(src/*)"))
      (should (harmless-perm-test-refusal
               session "write_file" '(:path "src/a.txt")))
      (should-not (harmless-perm-test-refusal
                   session "write_file" '(:path "src/a/b.txt")))
      (should-not (harmless-perm-test-refusal
                   session "write_file" '(:path "src")))
      (harmless-perm-set-rules session nil '("Edit(src/**)"))
      (should (harmless-perm-test-refusal
               session "write_file" '(:path "src/a/b.txt")))
      (should-not (harmless-perm-test-refusal
                   session "write_file" '(:path "src")))
      (harmless-perm-set-rules session nil '("Edit(src/)"))
      (should (harmless-perm-test-refusal
               session "replace" '(:path "src/a.txt")))
      (should-not (harmless-perm-test-refusal
                   session "replace" '(:path "src")))
      (harmless-perm-set-rules session nil '("Edit(src)"))
      (should (harmless-perm-test-refusal
               session "write_file" '(:path "src")))
      (should-not (harmless-perm-test-refusal
                   session "write_file" '(:path "src/a.txt")))
      (harmless-perm-set-rules session nil '("Edit(src/file.txt)"))
      (should (harmless-perm-test-refusal
               session "write_file" '(:path "src/file.txt")))
      (should-not (harmless-perm-test-refusal
                   session "write_file" '(:path "src/file.txt/")))
      (harmless-perm-set-rules session nil '("Edit(**)"))
      (should (harmless-perm-test-refusal
               session "write_file" '(:path "src/a/b.txt")))
      (should (harmless-perm-test-refusal
               session "write_file" (list :path abs)))
      (should-not (harmless-perm-test-refusal
                   session "write_file" '(:path "../outside.txt")))
      (should-not (harmless-perm-test-refusal
                   session "write_file" '(:path "/")))
      (should-not (harmless-perm-test-refusal
                   session "write_file" '(:path "c:/")))
      (should-not (harmless-perm-test-refusal
                   session "write_file" '(:path "")))
      (should-not (harmless-perm-test-refusal
                   session "write_file" '(:path nil)))
      (should-not (harmless-perm-test-refusal
                   session "write_file" '(:path :false)))
      (harmless-perm-set-rules session nil '("Edit(src/a.txt)"))
      (if (file-name-case-insensitive-p repo)
          (should (harmless-perm-test-refusal
                   session "write_file" '(:path "SRC/A.TXT")))
        (should-not (harmless-perm-test-refusal
                     session "write_file" '(:path "SRC/A.TXT"))))
      (harmless-perm-set-rules session nil '("Edit(/src/**)"))
      (should (equal (harmless-perm-test-refusal
                      session "write_file" '(:path "src/a.txt"))
                     "Error: Permission rule denied Edit(/src/**)"))
      (harmless-perm-set-rules
       session '("Edit(src/a.txt)") '("Edit(src/b.txt)"))
      (should (equal (harmless-perm-test-refusal
                      session "apply_patch"
                      (list :hunks
                            (vector '(:path "src/a.txt")
                                    '(:path "src/b.txt"))))
                     "Error: Permission rule denied Edit(src/b.txt)"))
      (should-not (harmless-perm-test-allows-p
                   session "apply_patch"
                   '(:hunks ((:path "src/a.txt")
                             (:path "src/b.txt")))))
      (should (harmless-perm-test-allows-p
               session "apply_patch"
               '(:hunks ((:path "src/a.txt")))))
      (harmless-perm-set-rules session '("Edit(**)") nil)
      (should (harmless-perm-test-allows-p
               session "apply_patch"
               '(:hunks ((:path "src/a.txt")
                         (:path "src/b.txt")))))
      (should-not (harmless-perm-test-allows-p
                   session "write_file" '(:path "../outside.txt")))
      (should-not (harmless-perm-test-allows-p
                   session "write_file" '(:path "/")))
      (harmless-perm-set-rules
       session nil '("WebFetch(domain:example.com)"))
      (dolist (url '("https://example.com/a"
                     "https://a.example.com/a"
                     "https://user:pass@a.example.com:443/x"))
        (should (equal (harmless-perm-test-refusal
                        session "web_fetch" (list :url url))
                       "Error: Permission rule denied WebFetch(domain:example.com)")))
      (dolist (url '("https://notexample.com/a"
                     "https://example.com.evil/a"
                     "/"
                     "c:/"
                     ""))
        (should-not (harmless-perm-test-refusal
                     session "web_fetch" (list :url url))))
      (should-not (harmless-perm-test-allows-p
                   session "web_fetch" '(:url "/")))
      (harmless-perm-set-rules
       session nil
       '("Bash(*)" "Edit(**)" "WebFetch(domain:example.com)"))
      (should-not (harmless-perm-test-refusal
                   session "shell_kill" '(:id "1")))
      (should-not (harmless-perm-test-refusal
                   session "computer_click" '(:x 1 :y 2)))
      (should-not (harmless-perm-test-refusal
                   session "web_search" '(:query "q")))
      (should-not (harmless-perm-test-refusal
                   session "monitor"
                   '(:pattern "x" :path "log.txt" :command "rm x")))
      (should (harmless-perm-test-refusal
               session "monitor"
               '(:pattern "x" :command "rm x")))
      (harmless-perm-set-rules
       session '("Bash(echo *)") '("Bash(echo *)"))
      (should (harmless-perm-test-allows-p
               session "run_shell" '(:command "echo hi")))
      (should (equal (harmless-perm-test-refusal
                      session "run_shell" '(:command "echo hi"))
                     "Error: Permission rule denied Bash(echo *)"))
      (harmless-perm-test-assert-session session))))

(ert-deftest harmless-perm-rules-deny-and-allow ()
  (harmless-perm-test-with _data
    (let* ((repo (make-temp-file "harmless-perm-repo-" t))
           (slash (file-exists-p "/harmless-perm-allowed.txt"))
           (cdrive (file-exists-p "c:/harmless-perm-allowed.txt"))
           (a (expand-file-name "src/a.txt" repo))
           (b (expand-file-name "src/b.txt" repo))
           (note (expand-file-name "note.txt" repo))
           (log (expand-file-name "log.txt" repo))
           (session (harmless-perm-test-session :cwd repo :id "turn")))
      (harmless-perm-test-write a "old\n")
      (harmless-perm-test-write b "old\n")
      (harmless-perm-test-write note "old\n")
      (harmless-perm-test-write log "old\n")
      (harmless-perm-set-rules
       session '("Bash(echo *)") '("Bash(echo denied*)"))
      (harmless-perm-test-run
       session "run_shell"
       '(:command "echo denied>harmless-perm-denied.txt"))
      (should-not harmless-perm-test-asked)
      (should (equal (harmless-perm-test-last-tool session)
                     "Error: Permission rule denied Bash(echo denied*)"))
      (should-not (file-exists-p
                   (expand-file-name "harmless-perm-denied.txt" repo)))
      (setf (harmless-session-permission-mode session) 'always-approve)
      (harmless-perm-test-run
       session "run_shell"
       '(:command "echo denied>harmless-perm-denied.txt"))
      (should-not harmless-perm-test-asked)
      (should (equal (harmless-perm-test-last-tool session)
                     "Error: Permission rule denied Bash(echo denied*)"))
      (setf (harmless-session-permission-mode session) 'accept-edits)
      (harmless-perm-set-rules session nil '("Edit(skipped.txt)"))
      (harmless-perm-test-run
       session "write_file"
       '(:path "skipped.txt" :contents "nope\n"))
      (should-not harmless-perm-test-asked)
      (should (equal (harmless-perm-test-last-tool session)
                     "Error: Permission rule denied Edit(skipped.txt)"))
      (should-not (file-exists-p
                   (expand-file-name "skipped.txt" repo)))
      (setf (harmless-session-permission-mode session) 'ask)
      (harmless-perm-set-rules
       session '("Bash(echo allowed*)") nil)
      (harmless-perm-test-run
       session "run_shell"
       '(:command "echo allowed>harmless-perm-allowed.txt"))
      (should-not harmless-perm-test-asked)
      (should (eq 'idle (harmless-session-status session)))
      (let ((path (expand-file-name "harmless-perm-allowed.txt" repo)))
        (should (file-name-absolute-p path))
        (should (file-regular-p path))
        (should-not (harmless-filesystem-root-p path))
        (should (string-prefix-p
                 (file-name-as-directory (expand-file-name repo))
                 path))
        (should-not (equal
                     (file-truename path)
                     (file-truename
                      (expand-file-name
                       (file-name-nondirectory path) "/"))))
        (should (string-match-p "allowed" (harmless-perm-test-read path))))
      (harmless-perm-set-rules session '("Bash(npm run *)") nil)
      (harmless-perm-test-run
       session "run_shell"
       '(:command "echo other>harmless-perm-other.txt"))
      (should (equal harmless-perm-test-asked
                     '((shell "echo other>harmless-perm-other.txt"))))
      (should (equal (harmless-perm-test-last-tool session)
                     "Error: user denied this tool call"))
      (should-not (file-exists-p
                   (expand-file-name "harmless-perm-other.txt" repo)))
      (harmless-perm-set-rules session nil '("Bash(echo *)"))
      (harmless-perm-test-run session "run_shell" '(:command "echo hi"))
      (should-not harmless-perm-test-asked)
      (should (equal (harmless-perm-test-last-tool session)
                     "Error: Permission rule denied Bash(echo *)"))
      (harmless-perm-set-rules session '("Bash(*)") nil)
      (setf (harmless-session-plan-mode session) t)
      (harmless-perm-test-run
       session "run_shell"
       '(:command "echo plan>harmless-perm-plan.txt"))
      (should-not harmless-perm-test-asked)
      (should (equal (harmless-perm-test-last-tool session)
                     harmless-shell-plan-refusal))
      (should-not (file-exists-p
                   (expand-file-name "harmless-perm-plan.txt" repo)))
      (harmless-perm-set-rules session nil '("Bash(echo *)"))
      (harmless-perm-test-run session "run_shell" '(:command "echo hi"))
      (should (equal (harmless-perm-test-last-tool session)
                     "Error: Permission rule denied Bash(echo *)"))
      (setf (harmless-session-plan-mode session) nil)
      (harmless-perm-set-rules
       session nil '("WebFetch(domain:example.com)"))
      (dolist (url '("https://example.com/a"
                     "https://a.example.com/x"))
        (setq harmless-perm-test-fetches nil)
        (harmless-perm-test-run session "web_fetch" (list :url url))
        (should-not harmless-perm-test-asked)
        (should-not harmless-perm-test-fetches)
        (should (equal (harmless-perm-test-last-tool session)
                       "Error: Permission rule denied WebFetch(domain:example.com)")))
      (harmless-perm-test-run
       session "web_fetch" '(:url "https://example.com.evil/a"))
      (should (equal (car harmless-perm-test-asked)
                     '(network "https://example.com.evil/a")))
      (should-not harmless-perm-test-fetches)
      (harmless-perm-set-rules
       session '("WebFetch(domain:example.com)") nil)
      (setq harmless-perm-test-fetches nil)
      (harmless-perm-test-run
       session "web_fetch" '(:url "https://a.example.com/x"))
      (should-not harmless-perm-test-asked)
      (should (equal harmless-perm-test-fetches
                     '("https://a.example.com/x")))
      (should (equal (harmless-perm-test-last-tool session)
                     "fetched page"))
      (harmless-perm-test-run session "web_fetch" '(:url "/"))
      (should (consp harmless-perm-test-asked))
      (should (equal (harmless-perm-test-last-tool session)
                     "Error: user denied this tool call"))
      (setq harmless-perm-test-commands nil)
      (harmless-perm-set-rules session nil '("Bash(rm *)"))
      (harmless-perm-test-run
       session "monitor" '(:pattern "x" :command "rm x"))
      (should-not harmless-perm-test-commands)
      (should (equal (harmless-perm-test-last-tool session)
                     "Error: Permission rule denied Bash(rm *)"))
      (harmless-perm-test-run
       session "monitor" '(:pattern "nope" :path "log.txt"))
      (should-not harmless-perm-test-commands)
      (should (string-match-p
               "\\`Watching "
               (harmless-perm-test-last-tool session)))
      (harmless-perm-set-rules
       session '("Edit(**)") '("Edit(src/b.txt)"))
      (harmless-perm-test-run
       session "apply_patch"
       (list :hunks
             (list (list :path "src/a.txt"
                         :old_string "old\n" :new_string "new\n")
                   (list :path "src/b.txt"
                         :old_string "old\n" :new_string "new\n"))))
      (should-not harmless-perm-test-asked)
      (should (equal (harmless-perm-test-last-tool session)
                     "Error: Permission rule denied Edit(src/b.txt)"))
      (should (equal (harmless-perm-test-read a) "old\n"))
      (should (equal (harmless-perm-test-read b) "old\n"))
      (harmless-perm-set-rules session '("Edit(src/a.txt)") nil)
      (harmless-perm-test-run
       session "apply_patch"
       (list :hunks
             (list (list :path "src/a.txt"
                         :old_string "old\n" :new_string "new\n")
                   (list :path "src/b.txt"
                         :old_string "old\n" :new_string "new\n"))))
      (should (eq (caar harmless-perm-test-asked) 'edit))
      (should (equal (harmless-perm-test-last-tool session)
                     "Error: user denied this tool call"))
      (should (equal (harmless-perm-test-read a) "old\n"))
      (should (equal (harmless-perm-test-read b) "old\n"))
      (harmless-perm-set-rules session '("Edit(src/**)") nil)
      (harmless-perm-test-run
       session "apply_patch"
       (list :hunks
             (list (list :path "src/a.txt"
                         :old_string "old\n" :new_string "new\n")
                   (list :path "src/b.txt"
                         :old_string "old\n" :new_string "new\n"))))
      (should-not harmless-perm-test-asked)
      (should (equal (harmless-perm-test-read a) "new\n"))
      (should (equal (harmless-perm-test-read b) "new\n"))
      (harmless-perm-test-assert-file a repo "new\n")
      (harmless-perm-test-assert-file b repo "new\n")
      (harmless-perm-set-rules session nil '("Edit(note.txt)"))
      (harmless-perm-test-run
       session "replace"
       '(:path "note.txt" :old_string "old\n" :new_string "new\n"))
      (should (equal (harmless-perm-test-read note) "old\n"))
      (harmless-perm-set-rules session '("Edit(noted.txt)") nil)
      (harmless-perm-test-run
       session "write_file"
       '(:path "noted.txt" :contents "hello\n"))
      (should-not harmless-perm-test-asked)
      (should (string-match
               "\\`Wrote \\(.*\\) ([0-9]+ bytes)\\'"
               (harmless-perm-test-last-tool session)))
      (harmless-perm-test-assert-file
       (match-string 1 (harmless-perm-test-last-tool session))
       repo "hello\n")
      (should (eq slash (file-exists-p "/harmless-perm-allowed.txt")))
      (should (eq cdrive (file-exists-p "c:/harmless-perm-allowed.txt")))
      (should (equal (harmless-session-title session) "Keep"))
      (harmless-perm-test-assert-session session))))

(ert-deftest harmless-perm-rules-stay-on-the-session ()
  (harmless-perm-test-with _data
    (let* ((repo (make-temp-file "harmless-perm-repo-" t))
           (harmless-permission-rules '("Bash(echo *)"))
           (one (harmless-perm-test-session :cwd repo :id "one"))
           (two (harmless-perm-test-session :cwd repo :id "two"))
           (raw nil)
           (child nil)
           (loaded nil))
      (should (equal (harmless-perm-test-texts one) '("Bash(echo *)")))
      (should (equal (harmless-perm-test-texts two) '("Bash(echo *)")))
      (should-not (eq (harmless-session-rules one)
                      (harmless-session-rules two)))
      (harmless-perm-set-rules one '("Edit(keep/**)") nil)
      (should (equal (harmless-perm-test-texts two) '("Bash(echo *)")))
      (harmless-session-save one)
      (setq raw (harmless-perm-test-raw one "summary.json"))
      (should-not (plist-member (harmless-json-decode raw) :rules))
      (should-not (string-search "Edit(keep/**)" raw))
      (should-not (string-search "Bash(echo *)" raw))
      (harmless-session-append-user one "remember this")
      (setq child (harmless-fork-session one 1))
      (should (equal (harmless-perm-test-texts child) '("Edit(keep/**)")))
      (should-not (eq (harmless-session-rules child)
                      (harmless-session-rules one)))
      (setq raw (harmless-perm-test-raw child "summary.json"))
      (should-not (plist-member (harmless-json-decode raw) :rules))
      (should-not (string-search "Edit(keep/**)" raw))
      (harmless-perm-set-rules child nil '("Bash(rm *)"))
      (should (equal (harmless-perm-test-texts one) '("Edit(keep/**)")))
      (should (equal (harmless-perm-test-texts child) '("Bash(rm *)")))
      (let ((harmless-permission-rules '("WebFetch(domain:example.com)")))
        (setq loaded (harmless-session-load (harmless-session-dir one))))
      (should (equal (harmless-perm-test-texts loaded)
                     '("WebFetch(domain:example.com)")))
      (should (equal (harmless-perm-test-texts one) '("Edit(keep/**)")))
      (harmless-perm-test-assert-session one)
      (harmless-perm-test-assert-session two)
      (harmless-perm-test-assert-session child)
      (harmless-perm-test-assert-session loaded))))

(defun harmless-perm-test-project-file (repo)
  "Return the permissions file for REPO and require that path."
  (let* ((session (harmless-session--create :id "path" :cwd repo))
         (file (harmless-perm--project-file session)))
    (should (file-name-absolute-p file))
    (should-not (harmless-filesystem-root-p file))
    (should-not (harmless-filesystem-root-p (file-name-directory file)))
    (should (string-prefix-p
             (file-name-as-directory (expand-file-name repo))
             file))
    (should-not (equal (file-truename file)
                       (file-truename
                        (expand-file-name ".harmless/permissions" "/"))))
    file))

(ert-deftest harmless-perm-project-file-refuses-bad-text ()
  (harmless-perm-test-with data
    (let* ((repo (make-temp-file "harmless-perm-proj-" t))
           (other (make-temp-file "harmless-perm-other-" t))
           (slash (file-exists-p "/.harmless"))
           (cdrive (file-exists-p "c:/.harmless"))
           (file (harmless-perm-test-project-file repo))
           (bad (harmless-perm-test-project-file other))
           (before nil)
           (got nil))
      (should-not (file-exists-p file))
      (should-not (file-directory-p
                   (expand-file-name ".harmless" repo)))
      (let ((default-directory "/")
            (blank (harmless-session--create :id "blank" :cwd nil))
            (empty (harmless-session--create :id "empty" :cwd ""))
            (relative (harmless-session--create :id "rel" :cwd "proj"))
            (root (harmless-session--create :id "root" :cwd "/")))
        (should-not (harmless-perm--project-file blank))
        (should-not (harmless-perm--project-file empty))
        (should-not (harmless-perm--project-file relative))
        (should-not (harmless-perm--project-file root))
        (harmless-perm-install-configured-rules relative)
        (harmless-perm-install-configured-rules root)
        (should-not (harmless-session-rules relative))
        (should-not (harmless-session-rules root))
        (should-not (file-exists-p "/proj"))
        (should-not (file-exists-p "/.harmless")))
      (let ((session (harmless-perm-test-session
                      :cwd (file-name-as-directory repo) :id "slash")))
        (should-not (harmless-session-rules session))
        (should (equal (harmless-perm--project-file session) file))
        (harmless-perm-test-assert-session session))
      (let ((default-directory "/")
            (detached (harmless-perm-test-session
                       :detached t :id "detached")))
        (should-not (harmless-session-cwd detached))
        (should-not (harmless-session-rules detached))
        (should-not (harmless-perm--project-file detached))
        (harmless-perm-test-assert-session detached))
      (setq before (directory-files data t))
      (harmless-perm-test-write bad "nope\n")
      (should (equal (harmless-perm-test-read bad) "nope\n"))
      (setq got
            (condition-case err
                (progn
                  (harmless-perm-test-session :cwd other :id "bad-file")
                  nil)
              (error (error-message-string err))))
      (should (equal got (format "%s: rule is not a rule" bad)))
      (should-not (harmless-session-get "bad-file"))
      (should (equal before (directory-files data t)))
      (should (equal (harmless-perm-test-read bad) "nope\n"))
      (delete-file bad)
      (make-directory bad t)
      (harmless-perm-test-signal
       (lambda ()
         (harmless-perm-test-session :cwd other :id "bad-dir"))
       "permissions file is a directory")
      (should-not (harmless-session-get "bad-dir"))
      (should (eq slash (file-exists-p "/.harmless")))
      (should (eq cdrive (file-exists-p "c:/.harmless"))))))

(ert-deftest harmless-perm-project-allow-survives-the-session ()
  (harmless-perm-test-with _data
    (let* ((repo (make-temp-file "harmless-perm-keep-" t))
           (other (make-temp-file "harmless-perm-elsewhere-" t))
           (file (harmless-perm-test-project-file repo))
           (text (concat "# project allows\n"
                         "\n"
                         "  Bash(echo *)  \n"
                         "Edit(noted2.txt)\n"))
           (session nil)
           (again nil)
           (loaded nil)
           (elsewhere nil)
           (raw nil)
           (noted (expand-file-name "noted2.txt" repo)))
      (harmless-perm-test-write file text)
      (harmless-perm-test-assert-file file repo text)
      (setq session (harmless-perm-test-session :cwd repo :id "keep"))
      (should (equal (harmless-perm-test-texts session)
                     '("Bash(echo *)" "Edit(noted2.txt)")))
      (should (equal (harmless-perm-test-patterns session)
                     '("echo *" "noted2.txt")))
      (setq raw (harmless-perm-test-raw session "summary.json"))
      (should-not (plist-member (harmless-json-decode raw) :rules))
      (should-not (string-search "Bash(echo *)" raw))
      (should-not (string-search "Edit(noted2.txt)" raw))
      (setq again (harmless-perm-test-session :cwd repo :id "again"))
      (should (equal (harmless-perm-test-texts again)
                     (harmless-perm-test-texts session)))
      (should-not (eq (harmless-session-rules again)
                      (harmless-session-rules session)))
      (setq elsewhere (harmless-perm-test-session
                       :cwd other :id "elsewhere"))
      (should-not (harmless-session-rules elsewhere))
      (let ((default-directory "/")
            (harmless-permission-rules '("WebFetch(domain:example.com)")))
        (setq session (harmless-perm-test-session
                       :cwd repo :id "with-setting"))
        (should (equal (harmless-perm-test-texts session)
                       '("WebFetch(domain:example.com)"
                         "Bash(echo *)"
                         "Edit(noted2.txt)"))))
      (let ((harmless-permission-denies '("Bash(echo *)")))
        (setq session (harmless-perm-test-session :cwd repo :id "deny"))
        (should (harmless-perm-test-allows-p
                 session "run_shell" '(:command "echo hi")))
        (should (equal (harmless-perm-test-refusal
                        session "run_shell" '(:command "echo hi"))
                       "Error: Permission rule denied Bash(echo *)"))
        (harmless-perm-test-run
         session "run_shell" '(:command "echo hi"))
        (should-not harmless-perm-test-asked)
        (should (equal (harmless-perm-test-last-tool session)
                       "Error: Permission rule denied Bash(echo *)")))
      (setq session (harmless-perm-test-session :cwd repo :id "write"))
      (harmless-perm-test-run
       session "write_file"
       '(:path "noted2.txt" :contents "hello\n"))
      (should-not harmless-perm-test-asked)
      (harmless-perm-test-assert-file noted repo "hello\n")
      (harmless-perm-set-rules session '("Edit(other.txt)") nil)
      (harmless-session-save session)
      (setq raw (harmless-perm-test-raw session "summary.json"))
      (should-not (string-search "Edit(other.txt)" raw))
      (should-not (string-search "Edit(noted2.txt)" raw))
      (let ((harmless-permission-rules nil))
        (setq loaded (harmless-session-load
                      (harmless-session-dir session))))
      (should (equal (harmless-perm-test-texts loaded)
                     '("Bash(echo *)" "Edit(noted2.txt)")))
      (should (equal (harmless-perm-test-texts session)
                     '("Edit(other.txt)")))
      (should (equal (harmless-perm-test-read file) text))
      (harmless-perm-test-assert-session session)
      (harmless-perm-test-assert-session again)
      (harmless-perm-test-assert-session loaded))))
