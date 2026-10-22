;;; harmless-todo-tests.el --- Tests for the session todo list -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(cl-defstruct (harmless-todo-fake
               (:include harmless-provider)
               (:constructor harmless-todo-make-fake))
  script)

(cl-defmethod harmless-provider-complete ((provider harmless-todo-fake)
                                          _messages _tools callback)
  (when (harmless-todo-fake-script provider)
    (funcall (harmless-todo-fake-script provider) _messages callback))
  nil)

(defvar harmless-todo-test-calls 0
  "Provider calls in the current todo test.")

(defun harmless-todo-test-error (message form)
  "Assert FORM signals MESSAGE."
  (let ((err (should-error (funcall form) :type 'error)))
    (should (equal message (error-message-string err)))))

(defun harmless-todo-test-text (path)
  "Return the contents of PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defmacro harmless-todo-test-env (&rest body)
  "Evaluate BODY with a project, a data directory, and a session."
  (declare (indent 0))
  `(let* ((base (make-temp-file "harmless-todo-" t))
          (dir (expand-file-name "proj" base))
          (data (expand-file-name "data" base))
          (provider (harmless-make-openai-compat
                     "local" :host "127.0.0.1:9" :protocol "http"
                     :key "none" :models '("m")))
          (harmless-directory data)
          (harmless--sessions (make-hash-table :test 'equal))
          (harmless-providers nil)
          (harmless-compact-threshold nil)
          (session nil))
     (make-directory dir t)
     (make-directory data t)
     (setq harmless-providers (list provider))
     (setq session (harmless-session-new
                    :cwd dir :provider provider :model "m" :title "Todo"))
     ,@body))

(defun harmless-todo-test-tool-script (event)
  "Return a provider script that sends EVENT once, then text."
  (lambda (_messages callback)
    (setq harmless-todo-test-calls (1+ harmless-todo-test-calls))
    (if (= harmless-todo-test-calls 1)
        (progn
          (funcall callback event)
          (funcall callback '(:stop "tool_calls")))
      (funcall callback '(:text "done"))
      (funcall callback '(:stop "stop")))))

(defun harmless-todo-test-tool-texts (session)
  "Return tool results stored on SESSION."
  (cl-loop for msg in (harmless-session-messages session)
           when (eq (plist-get msg :role) :tool)
           collect (plist-get msg :content)))

(ert-deftest harmless-todo-refuses-bad-input ()
  (harmless-todo-test-env
    (let ((calls 0))
      (cl-letf (((symbol-function 'expand-file-name)
                 (lambda (&rest _)
                   (setq calls (1+ calls))
                   "/"))
                ((symbol-function 'file-truename)
                 (lambda (&rest _)
                   (setq calls (1+ calls))
                   "/"))
                ((symbol-function 'write-region)
                 (lambda (&rest _)
                   (setq calls (1+ calls)))))
        (dolist (session-case (list nil "" 12))
          (harmless-todo-test-error
           "No Harmless session"
           (lambda ()
             (harmless-todo-update session-case
                                   '(:todos ((:id "a" :content "x")))))))
        (dolist (case '((nil "todos must be a list")
                        ((:merge t) "todos must be a list")
                        ((:todos 12) "todos must be a list")
                        ((:todos (:id "a" :content "x"))
                         "todos must be a list")
                        ((:merge "yes" :todos nil)
                         "todo merge must be a boolean")
                        ((:todos ("nope")) "todo must be an item")
                        ((:todos (12)) "todo must be an item")
                        ((:todos ((:content "x"))) "todo id is empty")
                        ((:todos ((:id nil :content "x")))
                         "todo id is empty")
                        ((:todos ((:id "" :content "x")))
                         "todo id is empty")
                        ((:todos ((:id "  " :content "x")))
                         "todo id is empty")
                        ((:todos ((:id 12 :content "x")))
                         "todo id must be a string")
                        ((:todos ((:id "a"))) "todo content is empty")
                        ((:todos ((:id "a" :content nil)))
                         "todo content is empty")
                        ((:todos ((:id "a" :content "")))
                         "todo content is empty")
                        ((:todos ((:id "a" :content "  ")))
                         "todo content is empty")
                        ((:todos ((:id "a" :content 12)))
                         "todo content must be a string")
                        ((:todos ((:id "a" :content "x" :status "canceled")))
                         "todo status is not a status")
                        ((:todos ((:id "a" :content "x" :status "")))
                         "todo status is not a status")
                        ((:todos ((:id "a" :content "x" :status 12)))
                         "todo status is not a status")
                        ((:todos ((:id "a" :content "one")
                                  (:id "a" :content "two")))
                         "todo id is duplicate")
                        ((:todos ((:id "a" :content "x" :depends "a")))
                         "todo depends must be a list")
                        ((:todos ((:id "a" :content "x" :depends (12))))
                         "todo depends must be a string")
                        ((:todos ((:id "a" :content "x" :depends (""))))
                         "todo depends is empty")
                        ((:todos ((:id "a" :content "x" :depends ("  "))))
                         "todo depends is empty")
                        ((:todos ((:id "a" :content "x" :depends ("a"))))
                         "todo depends on itself")
                        ((:todos ((:id "a" :content "x" :depends ("b"))))
                         "todo depends is missing")
                        ((:todos ((:id "a" :content "one" :depends ("b" "b"))
                                  (:id "b" :content "two")))
                         "todo depends is duplicate")
                        ((:merge :false
                          :todos ((:id "a" :content "one" :depends ("b"))
                                  (:id "b" :content "two" :depends ("a"))))
                         "todo depends is a cycle")))
          (harmless-todo-test-error
           (cadr case)
           (lambda () (harmless-todo-update session (car case)))))
        (should-not (harmless-session-todos session))
        (should (= calls 0))))))

(ert-deftest harmless-todo-updates-status-and-dependencies ()
  (harmless-todo-test-env
    (let ((names (directory-files dir))
          (file nil))
      (should (equal (harmless-todo-update
                      session
                      '(:todos ((:id "a" :content "  write the tests  "))))
                     "[pending] a: write the tests"))
      (should (equal (harmless-todo-update
                      session
                      '(:merge nil
                        :todos ((:id "b" :content "run them"
                                 :status " in_progress "
                                 :depends ("  a  ")))))
                     (concat "[pending] a: write the tests\n"
                             "[in_progress] b: run them (depends a)")))
      (should (equal (harmless-todo-header session) "  todos 2"))
      (should (equal (harmless-todo-update
                      session
                      '(:todos ((:id "a" :status "completed"))))
                     (concat "[completed] a: write the tests\n"
                             "[in_progress] b: run them (depends a)")))
      (should (equal (harmless-todo-header session) "  todos 1"))
      (should (equal (harmless-todo-update
                      session '(:todos ((:id "b" :depends nil))))
                     (concat "[completed] a: write the tests\n"
                             "[in_progress] b: run them")))
      (should (equal (harmless-todo-update
                      session
                      '(:merge :false
                        :todos ((:id "/" :content "c:/"
                                 :status "cancelled"))))
                     "[cancelled] /: c:/"))
      (should (equal (harmless-todo-header session) ""))
      (should (equal (harmless-todo-update
                      session
                      '("todos"
                        (("id" "notes" "content" "sub/"
                          "status" "pending"))))
                     (concat "[cancelled] /: c:/\n"
                             "[pending] notes: sub/")))
      (should (equal (harmless-todo-update
                      session '(:merge :false :todos nil))
                     "No todos"))
      (should-not (harmless-session-todos session))
      (should-not (plist-member (harmless-session-summary-plist session)
                                :todos))
      (should (equal (harmless-todo-update
                      session
                      '(:todos ((:id "a" :content "write the tests")
                                (:id "b" :content "run them"
                                 :status :in_progress
                                 :depends ("a")))))
                     (concat "[pending] a: write the tests\n"
                             "[in_progress] b: run them (depends a)")))
      (setq file (harmless-session--file session "summary.json"))
      (should (file-name-absolute-p file))
      (should-not (harmless-filesystem-root-p file))
      (should-not (harmless-filesystem-root-p (file-name-directory file)))
      (should (harmless-directory-strictly-under-p
               (file-name-directory file) data))
      (should (equal file
                     (expand-file-name
                      "summary.json"
                      (harmless-session-directory session))))
      (should (string-match-p "write the tests"
                              (harmless-todo-test-text file)))
      (should (equal names (directory-files dir)))
      (should-not (file-exists-p (expand-file-name "notes" dir)))
      (should-not (file-exists-p (expand-file-name "sub" dir)))
      (should-not (file-exists-p "/plan.md"))
      (should (eq (harmless-tool-class
                   (harmless-tool-by-name "harmless-tool-todo-write"))
                  'read))
      (should-not (harmless-perm-needed-p session 'read))
      (let ((calls 0)
            (before (harmless-session-todos session)))
        (cl-letf (((symbol-function 'write-region)
                   (lambda (&rest _) (setq calls (1+ calls)))))
          (harmless-todo-test-error
           "todo depends is a cycle"
           (lambda ()
             (harmless-todo-update
              session
              '(:merge :false
                :todos ((:id "a" :content "one" :depends ("b"))
                        (:id "b" :content "two" :depends ("a")))))))
          (should (equal (harmless-session-todos session) before))
          (should (= calls 0)))))))

(ert-deftest harmless-todo-restores-forks-and-renders ()
  (harmless-todo-test-env
    (let ((names (directory-files dir))
          (loaded nil)
          (child nil)
          (agent nil)
          (chat nil)
          (kept nil))
      (should (equal (harmless-todo-update
                      session
                      '(:todos ((:id "a" :content "write the tests"))))
                     "[pending] a: write the tests"))
      (setq kept (harmless-session-todos session))
      (setq loaded (harmless-session-load
                    (harmless-session-directory session)))
      (should (equal (harmless-session-todos loaded) kept))
      (should (equal (harmless-todo-header loaded) "  todos 1"))
      (let ((plist (harmless-json-decode
                    (harmless-todo-test-text
                     (harmless-session--file session "summary.json"))))
            (next nil))
        (while plist
          (unless (eq (car plist) :todos)
            (setq next (plist-put next (car plist) (cadr plist))))
          (setq plist (cddr plist)))
        (let ((coding-system-for-write 'utf-8-unix))
          (with-temp-file (harmless-session--file session "summary.json")
            (insert (harmless-json-text next) "\n"))))
      (setq loaded (harmless-session-load
                    (harmless-session-directory session)))
      (should-not (harmless-session-todos loaded))
      (should (equal kept (harmless-session-todos session)))
      (let ((calls 0))
        (cl-letf (((symbol-function 'write-region)
                   (lambda (&rest _) (setq calls (1+ calls)))))
          (harmless-todo-restore
           loaded
           '((:id "a" :content "keep" :status "pending" :depends nil)))
          (should (= calls 0))
          (should (equal (harmless-session-todos loaded)
                         '((:id "a" :content "keep"
                            :status pending :depends nil))))
          (harmless-todo-restore loaded nil)
          (should-not (harmless-session-todos loaded))
          (should (= calls 0))))
      (harmless-todo-restore
       loaded
       '((:id "/" :content "c:/" :status "pending" :depends ("/"))
         (:id "b" :content "keep" :status "nope")
         "nope"
         (:id "b" :content "second" :status "pending")
         (:id "c" :content "sub/" :status "completed"
          :depends ("missing" "c"))
         (:id "d" :content "cycle" :status "pending" :depends ("e"))
         (:id "e" :content "cycle" :status "in_progress"
          :depends ("d"))))
      (should (equal (harmless-session-todos loaded)
                     '((:id "/" :content "c:/" :status pending :depends nil)
                       (:id "b" :content "second"
                        :status pending :depends nil)
                       (:id "c" :content "sub/"
                        :status completed :depends nil)
                       (:id "d" :content "cycle"
                        :status pending :depends ("e"))
                       (:id "e" :content "cycle"
                        :status in_progress :depends nil))))
      (should (file-name-absolute-p (harmless-session-directory loaded)))
      (should-not (harmless-filesystem-root-p
                   (harmless-session-directory loaded)))
      (should (harmless-directory-strictly-under-p
               (harmless-session-directory loaded) data))
      (setq child (harmless-fork--create session nil))
      (should (equal (harmless-session-todos child) kept))
      (should (file-name-absolute-p (harmless-session-directory child)))
      (should-not (harmless-filesystem-root-p
                   (harmless-session-directory child)))
      (should (harmless-directory-strictly-under-p
               (harmless-session-directory child) data))
      (should (equal (harmless-todo-update
                      child
                      '(:todos ((:id "a" :content "changed"))))
                     "[pending] a: changed"))
      (should (equal (harmless-session-todos session) kept))
      (setq agent (harmless-agent-create
                   session '(:prompt "look") #'ignore))
      (should-not (harmless-session-todos agent))
      (should (file-name-absolute-p (harmless-session-directory agent)))
      (should-not (harmless-filesystem-root-p
                   (harmless-session-directory agent)))
      (should (harmless-directory-strictly-under-p
               (harmless-session-directory agent) data))
      (should (equal (harmless-session-todos session) kept))
      (setf (harmless-session-agent agent)
            (list :allow 'all :sandbox 'read-only :prompt "p"))
      (should (member "harmless-tool-todo-write"
                      (mapcar #'harmless-tool-name
                              (harmless-tools-for-session agent))))
      (should (equal (harmless-todo-update
                      agent '(:todos ((:id "z" :content "child"))))
                     "[pending] z: child"))
      (should (equal (harmless-session-todos session) kept))
      (setf (plist-get (harmless-session-agent agent) :allow)
            '("harmless-tool-read-file"))
      (should (equal (harmless-agent-refuse-call
                      agent (harmless-tool-by-name "harmless-tool-todo-write") nil)
                     "Error: this subagent cannot use harmless-tool-todo-write"))
      (setq chat (harmless-session-new
                  :detached t :provider provider :model "m" :title "Chat"))
      (should (member "harmless-tool-todo-write"
                      (mapcar #'harmless-tool-name
                              (harmless-tools-for-session chat))))
      (should (equal (harmless-todo-update
                      chat '(:todos ((:id "/" :content "c:/"))))
                     "[pending] /: c:/"))
      (should (harmless-directory-strictly-under-p
               (harmless-session-directory chat) data))
      (should (equal (harmless-ui--tool-verb "harmless-tool-todo-write") "todo"))
      (should (equal (harmless-ui--tool-summary
                      "harmless-tool-todo-write"
                      '(:todos ((:id "a" :content "write the tests")
                                (:id "b" :content "run them"))))
                     "write the tests, run them"))
      (should (equal (harmless-ui--tool-summary "harmless-tool-todo-write" nil) ""))
      (unwind-protect
          (let ((harmless--session session))
            (harmless-ui-render-session session)
            (should (string-prefix-p
                     "Todos\n[pending] a: write the tests\n\n"
                     (with-current-buffer (harmless-session-buffer session)
                       (buffer-string))))
            (should (string-suffix-p
                     "  todos 1" (harmless-ui--header-line))))
        (when (buffer-live-p (harmless-session-buffer session))
          (kill-buffer (harmless-session-buffer session))))
      (should (equal names (directory-files dir)))
      (should-not (file-exists-p (expand-file-name "notes" dir)))
      (should-not (file-exists-p (expand-file-name "sub" dir)))
      (should-not (file-exists-p "/plan.md")))))

(ert-deftest harmless-todo-ask-and-plan-do-not-block ()
  (harmless-todo-test-env
    (let ((names (directory-files dir))
          (harmless-todo-test-calls 0))
      (with-temp-file (expand-file-name "keep.txt" dir)
        (insert "keep\n"))
      (setq names (directory-files dir))
      (let ((session (harmless-session-new
                      :cwd dir :title "Ask" :model "m"
                      :provider
                      (harmless-todo-make-fake
                       :name "fake" :host "none"
                       :script
                       (harmless-todo-test-tool-script
                        '(:tool-call "1" "harmless-tool-todo-write"
                          "{\"todos\":[{\"id\":\"a\",\"content\":\"c:/\"}]}"))))))
        (setq harmless-perm-ask-function
              (lambda (&rest _) (error "permission asked")))
        (setq harmless-todo-test-calls 0)
        (harmless-turn-run session "go")
        (should (equal (harmless-todo-test-tool-texts session)
                       '("[pending] a: c:/")))
        (should (harmless-session-todos session))
        (should-not (harmless-session-plan-mode session)))
      (let ((session (harmless-session-new
                      :cwd dir :title "Plan" :model "m"
                      :provider
                      (harmless-todo-make-fake
                       :name "fake" :host "none"
                       :script
                       (harmless-todo-test-tool-script
                        '(:tool-call "1" "harmless-tool-todo-write"
                          "{\"todos\":[{\"id\":\"b\",\"content\":\"/\"}]}"))))))
        (setf (harmless-session-plan-mode session) t)
        (setq harmless-perm-ask-function
              (lambda (&rest _) (error "permission asked")))
        (setq harmless-todo-test-calls 0)
        (harmless-turn-run session "go")
        (should (harmless-session-plan-mode session))
        (should (equal (harmless-todo-test-tool-texts session)
                       '("[pending] b: /")))
        (should-not (file-exists-p (expand-file-name "plan.md" dir))))
      (should (equal "keep\n"
                     (harmless-todo-test-text
                      (expand-file-name "keep.txt" dir))))
      (should (equal names (directory-files dir))))))
