;;; harmless-hook-tests.el --- Tests for tool hooks -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(cl-defstruct (harmless-hook-fake
               (:include harmless-provider)
               (:constructor harmless-hook-make-fake))
  script)

(cl-defmethod harmless-provider-complete ((provider harmless-hook-fake)
                                          messages tools callback)
  (when (functionp (harmless-hook-fake-script provider))
    (funcall (harmless-hook-fake-script provider)
             messages tools callback))
  nil)

(defvar harmless-hook-test-calls 0
  "Provider calls in the current hook turn.")

(defvar harmless-hook-test-asked nil
  "Permission questions recorded by the current turn.")

(defvar harmless-hook-test-decision 'allow
  "Decision the fake permission question returns.")

(defvar harmless-hook-test-seen nil
  "Hook activity recorded by the current test.")

(defun harmless-hook-test-error (fn)
  "Return the error text from calling FN."
  (error-message-string (should-error (funcall fn) :type 'error)))

(defun harmless-hook-test-read (file)
  "Return the contents of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun harmless-hook-test-summary (session)
  "Return SESSION's summary plist from disk."
  (harmless-json-decode
   (harmless-hook-test-read
    (expand-file-name "summary.json" (harmless-session-dir session)))))

(defun harmless-hook-test-assert-session (session)
  "Require SESSION's directory to stay off the filesystem root."
  (let ((dir (harmless-session-dir session)))
    (should (file-name-absolute-p dir))
    (should-not (harmless-filesystem-root-p dir))
    (should (harmless-directory-strictly-under-p dir harmless-directory))
    (should-not (equal (file-truename dir)
                       (file-truename (expand-file-name "/"))))))

(defun harmless-hook-test-assert-written (path root)
  "Require PATH to be a regular file strictly inside ROOT."
  (setq path (expand-file-name path))
  (should (file-name-absolute-p path))
  (should (file-regular-p path))
  (should-not (harmless-filesystem-root-p path))
  (should (string-prefix-p
           (file-name-as-directory (expand-file-name root))
           path))
  (should-not (equal
               (file-truename path)
               (file-truename
                (expand-file-name (file-name-nondirectory path) "/"))))
  path)

(defmacro harmless-hook-test-with (data repo &rest body)
  "Bind temp directories DATA and REPO, then run BODY."
  (declare (indent 2))
  `(let* ((,data (make-temp-file "harmless-hook-data-" t))
          (,repo (make-temp-file "harmless-hook-repo-" t))
          (harmless-directory ,data)
          (harmless--sessions (make-hash-table :test 'equal))
          (harmless-compact-threshold nil)
          (harmless-permission-rules nil)
          (harmless-permission-denies nil)
          (harmless-dangerous-commands
           '("diskpart" "halt" "mkfs" "poweroff" "reboot" "shutdown"))
          (harmless-hooks nil)
          (harmless-auto-review-function nil)
          (harmless-auto-review-model-function
           (lambda (&rest _args) (error "model must not be called")))
          (harmless-hook-command-function
           (lambda (&rest _args) (error "command must not be called")))
          (harmless-hook-http-function
           (lambda (&rest _args) (error "http must not be called")))
          (harmless-hook-mcp-function
           (lambda (&rest _args) (error "mcp must not be called")))
          (harmless-hook-prompt-function
           (lambda (&rest _args) (error "prompt must not be called")))
          (harmless-hook-agent-function
           (lambda (&rest _args) (error "agent must not be called")))
          (harmless-hook-test-calls 0)
          (harmless-hook-test-asked nil)
          (harmless-hook-test-decision 'allow)
          (harmless-hook-test-seen nil)
          (harmless-perm-ask-function
           (lambda (_session class item callback)
             (push (list class item) harmless-hook-test-asked)
             (funcall callback harmless-hook-test-decision))))
     ,@body))

(defun harmless-hook-test-session (repo &rest args)
  "Return a session in REPO.  ARGS go to `harmless-session-new'."
  (apply #'harmless-session-new
         :cwd repo
         :provider (harmless-hook-make-fake
                    :name "fake" :host "none" :script #'ignore)
         :model "m"
         :title "Hook"
         args))

(defun harmless-hook-test-run (session name args)
  "Run tool NAME with ARGS on SESSION and wait until it stops."
  (setq harmless-hook-test-calls 0)
  (setq harmless-hook-test-asked nil)
  (setf (harmless-hook-fake-script (harmless-session-provider session))
        (lambda (_messages _tools callback)
          (setq harmless-hook-test-calls
                (1+ harmless-hook-test-calls))
          (if (= harmless-hook-test-calls 1)
              (progn
                (funcall callback
                         (list :tool-call "1" name
                               (harmless-json-text args)))
                (funcall callback '(:stop "tool_calls")))
            (funcall callback '(:text "done"))
            (funcall callback '(:stop "stop")))))
  (harmless-turn-run session "go")
  (let ((n 0))
    (while (and (not (memq (harmless-session-status session)
                           '(idle error)))
                (< n 80))
      (setq n (1+ n))
      (accept-process-output nil 0.05))))

(defun harmless-hook-test-last (session)
  "Return the latest tool result stored on SESSION."
  (car (last (cl-loop for msg in (harmless-session-messages session)
                      when (eq (plist-get msg :role) :tool)
                      collect (plist-get msg :content)))))

(defun harmless-hook-test-children ()
  "Return child sessions in the current directory."
  (let (out)
    (maphash
     (lambda (_id session)
       (when (eq (harmless-session-source session) 'agent)
         (push session out)))
     harmless--sessions)
    out))

(ert-deftest harmless-hook-parses-and-refuses-bad-config ()
  (let ((slash (file-exists-p "/.harmless"))
        (cdrive (file-exists-p "c:/.harmless"))
        (tool (harmless-tool-by-name "harmless-tool-run-shell")))
    (should (null (eval (car (get 'harmless-hooks 'standard-value)))))
    (should (null (harmless-hook-validate nil)))
    (should (equal (harmless-hook-test-error
                    (lambda () (harmless-hook-validate "no")))
                   "hooks must be a list"))
    (should (equal (harmless-hook-test-error
                    (lambda () (harmless-hook-validate "")))
                   "hooks must be a list"))
    (should (equal (harmless-hook-test-error
                    (lambda () (harmless-hook-validate "/")))
                   "hooks must be a list"))
    (should (equal (harmless-hook-test-error
                    (lambda () (harmless-hook-validate "c:/")))
                   "hooks must be a list"))
    (should (equal (harmless-hook-test-error
                    (lambda () (harmless-hook-validate '(:event pre . :type))))
                   "hooks must be a list"))
    (should (equal (harmless-hook-test-error
                    (lambda () (harmless-hook-validate '("/"))))
                   "hook must be a hook"))
    (should (equal (harmless-hook-test-error
                    (lambda ()
                      (harmless-hook-validate
                       '((:event pre :type function)))))
                   "hook function is missing"))
    (should (equal (harmless-hook-test-error
                    (lambda ()
                      (harmless-hook-validate
                       '((:event no :type function :function ignore)))))
                   "hook event is not an event"))
    (should (equal (harmless-hook-test-error
                    (lambda ()
                      (harmless-hook-validate
                       '((:event "pre" :type function :function ignore)))))
                   "hook event is not an event"))
    (should (equal (harmless-hook-test-error
                    (lambda ()
                      (harmless-hook-validate
                       '((:event pre :type no :function ignore)))))
                   "hook type is not a type"))
    (should (equal (harmless-hook-test-error
                    (lambda ()
                      (harmless-hook-validate
                       '((:event pre :type function :function ignore
                                 :tool 12)))))
                   "hook tool is not a name"))
    (dolist (url '(nil "" "/" "c:/" "c:\\" "relative" "file:///etc/passwd"
                       "http://" "https://" "http://ex ample.com"))
      (should (equal (harmless-hook-test-error
                      (lambda ()
                        (harmless-hook-validate
                         `((:event pre :type http :url ,url)))))
                     "hook url is not a url")))
    (should (harmless-hook-url-p "https://example.com/hook"))
    (should (harmless-hook-url-p "https://example.com/hook/"))
    (should (harmless-hook-url-p "http://127.0.0.1/hooks"))
    (should-not (harmless-hook-url-p nil))
    (should-not (harmless-hook-url-p ""))
    (should-not (harmless-hook-url-p "/"))
    (should-not (harmless-hook-url-p "c:/"))
    (should-not (harmless-hook-url-p "~"))
    (dolist (command '(nil "" "   " "/"))
      (should (equal (harmless-hook-test-error
                      (lambda ()
                        (harmless-hook-validate
                         `((:event pre :type command :command ,command)))))
                     (if (member command '(nil "" "   "))
                         "hook command is missing"
                       "hook command is not a command"))))
    (dolist (command '("c:/" "c:\\" "d:/" "D:\\"))
      (should (equal (harmless-hook-test-error
                      (lambda ()
                        (harmless-hook-validate
                         `((:event pre :type command :command ,command)))))
                     "hook command is not a command")))
    (should (equal (harmless-hook-test-error
                    (lambda ()
                      (harmless-hook-validate
                       '((:event pre :type mcp :server nil :method "x")))))
                   "hook server is missing"))
    (should (equal (harmless-hook-test-error
                    (lambda ()
                      (harmless-hook-validate
                       '((:event pre :type mcp :server "/" :method "x")))))
                   "hook server is missing"))
    (should (equal (harmless-hook-test-error
                    (lambda ()
                      (harmless-hook-validate
                       '((:event pre :type mcp :server "c:/" :method "x")))))
                   "hook server is missing"))
    (should (equal (harmless-hook-test-error
                    (lambda ()
                      (harmless-hook-validate
                       '((:event pre :type mcp :server "demo" :method nil)))))
                   "hook method is missing"))
    (should (equal (harmless-hook-test-error
                    (lambda ()
                      (harmless-hook-validate
                       '((:event pre :type mcp :server "demo" :method "")))))
                   "hook method is missing"))
    (let ((hook (list :event 'pre :type 'function :function #'ignore)))
      (should (equal (harmless-hook-validate (list hook)) (list hook))))
    (should (null (harmless-hook-parse-reply nil)))
    (should (null (harmless-hook-parse-reply "")))
    (should (null (harmless-hook-parse-reply "   ")))
    (should (null (harmless-hook-parse-reply "/")))
    (should (null (harmless-hook-parse-reply "c:/")))
    (should (null (harmless-hook-parse-reply "allow please")))
    (should (null (harmless-hook-parse-reply "I would block this")))
    (should (null (harmless-hook-parse-reply "please block: now")))
    (should (null (harmless-hook-parse-reply "blocking")))
    (should (null (harmless-hook-parse-reply "replace the file")))
    (should (equal (harmless-hook-parse-reply "block") '(:block nil)))
    (should (equal (harmless-hook-parse-reply "block:") '(:block nil)))
    (should (equal (harmless-hook-parse-reply "block: no") '(:block "no")))
    (should (equal (harmless-hook-parse-reply "  BLOCK: No") '(:block "No")))
    (should (equal (harmless-hook-parse-reply "replace: SECRET")
                   '(:replace "SECRET")))
    (should (equal (harmless-hook-parse-reply "replace:\nSECRET")
                   '(:replace "SECRET")))
    (should (equal (harmless-hook-parse-reply "hello\nreplace: SECRET\nmore")
                   '(:replace "SECRET\nmore")))
    (should (equal (harmless-hook-parse-reply "block: a\nreplace: b")
                   '(:block "a")))
    (should (equal (harmless-hook-parse-reply
                    "{\"decision\":\"block\",\"reason\":\"/\"}")
                   '(:block "/")))
    (should (equal (harmless-hook-parse-reply
                    "{\"decision\":\"Block\",\"reason\":\"  no  \"}")
                   '(:block "no")))
    (should (equal (harmless-hook-parse-reply "{\"replace\":\"SECRET\"}")
                   '(:replace "SECRET")))
    (should (equal (harmless-hook-parse-reply "{\"replace\":\"\"}")
                   '(:replace "")))
    (should (null (harmless-hook-parse-reply "{\"decision\":\"allow\"}")))
    (should (null (harmless-hook-parse-reply "{\"decision\":null}")))
    (should (null (harmless-hook-parse-reply "{not json")))
    (let ((prompt (harmless-hook-prompt-text
                   (harmless-hook-payload
                    'pre tool '(:command "/" :path "c:/") nil))))
      (should (string-search "Event: pre" prompt))
      (should (string-search "Tool: harmless-tool-run-shell" prompt))
      (should (string-search "command=\"/\"" prompt))
      (should (string-search "path=\"c:/\"" prompt))
      (should-not (file-exists-p "/echo"))
      (should-not (file-exists-p "/c:")))
    (let* ((nested 'unset)
           (saw nil)
           (got nil)
           (harmless-hooks
           (list (list :event 'pre :type 'function
                       :function
                       (lambda (session hook-tool args text)
                         (setq saw text)
                         (harmless-hook-pre
                          session hook-tool args
                          (lambda (blocked) (setq nested blocked)))
                         'block)))))
      (harmless-hook-pre nil tool '(:command "/")
                         (lambda (blocked) (setq got blocked)))
      (should (null nested))
      (should (null saw))
      (should (equal got "Error: hook blocked this tool call"))
      (should-not harmless-hook-running))
    (let* ((got nil)
           (saw nil)
           (harmless-hooks
            (list
             (list :event 'post :type 'function
                   :function (lambda (&rest _) "one"))
            (list :event 'post :type 'function
                  :function (lambda (&rest _) (error "boom")))
            (list :event 'post :type 'function
                  :function
                  (lambda (_session _tool _args text)
                    (push text saw)
                    "three")))))
      (harmless-hook-post nil tool nil "REAL"
                          (lambda (text) (setq got text)))
      (should (equal saw '("one")))
      (should (equal got "three"))
      (should-not harmless-hook-running))
    (let ((got nil)
          (harmless-hooks
           (list (list :event 'post :type 'function
                       :function (lambda (&rest _) (error "boom"))))))
      (harmless-hook-post nil tool '(:path "/") "REAL"
                          (lambda (text) (setq got text)))
      (should (equal got "REAL")))
    (should (eq slash (file-exists-p "/.harmless")))
    (should (eq cdrive (file-exists-p "c:/.harmless")))))

(ert-deftest harmless-hook-runners-do-not-touch-root ()
  (let ((slash (file-exists-p "/.harmless"))
        (cdrive (file-exists-p "c:/.harmless"))
        (fetched 0)
        (ran 0)
        (found 0)
        (tool (harmless-tool-by-name "harmless-tool-write-file"))
        (payload (harmless-hook-payload
                  'pre "harmless-tool-write-file" '(:path "/" :contents "no\n") nil)))
    (cl-letf (((symbol-function 'call-process-region)
               (lambda (&rest _args)
                 (setq ran (1+ ran))
                 (error "ran")))
              ((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _args)
                 (setq fetched (1+ fetched))
                 (error "fetched")))
              ((symbol-function 'harmless-mcp--find)
               (lambda (&rest _args)
                 (setq found (1+ found))
                 (error "found"))))
      (dolist (command '(nil "" "   " "/" "c:/" "c:\\" "d:/"))
        (let ((err (harmless-hook-test-error
                    (lambda ()
                      (harmless-hook-command-default
                       (list :command command) payload
                       (lambda (_value) (error "cb")))))))
          (should (member err '("hook command is missing"
                                "hook command is not a command")))))
      (dolist (url '(nil "" "/" "c:/" "c:\\" "relative" "file:///tmp/x"
                         "http://" "https://"))
        (should (equal (harmless-hook-test-error
                        (lambda ()
                          (harmless-hook-http-default
                           (list :url url) payload #'ignore)))
                       "hook url is not a url")))
      (dolist (server '(nil "" "/" "c:/" "c:\\"))
        (should (equal (harmless-hook-test-error
                        (lambda ()
                          (harmless-hook-mcp-default
                           nil (list :server server :method "check")
                           payload
                           (lambda (_value) (error "cb")))))
                       "hook server is missing")))
      (should (equal (harmless-hook-test-error
                      (lambda ()
                        (harmless-hook-mcp-default
                         nil '(:server "demo" :method "  ")
                         payload #'ignore)))
                     "hook method is missing"))
      (should (= ran 0))
      (should (= fetched 0))
      (should (= found 0))
      (should-not (file-exists-p "/note.txt"))
      (ignore tool))
    (should (eq slash (file-exists-p "/.harmless")))
    (should (eq cdrive (file-exists-p "c:/.harmless")))))

(ert-deftest harmless-hook-prompt-and-agent ()
  (harmless-hook-test-with data repo
    (let* ((session (harmless-hook-test-session repo))
           (provider (harmless-session-provider session))
           (tool (harmless-tool-by-name "harmless-tool-run-shell"))
           (payload (harmless-hook-payload
                     'pre tool '(:command "/" :path "c:/") nil))
           (hook (list :event 'pre :type 'prompt :prompt "Look"))
           (got nil)
           (body nil)
           (tools t)
           (called nil)
           (turns 0))
      (harmless-hook-test-assert-session session)
      (setf (harmless-hook-fake-script provider)
            (lambda (messages tools-arg callback)
              (setq body (plist-get (car messages) :content))
              (setq tools tools-arg)
              (funcall callback '(:text "replace: next"))
              (funcall callback '(:stop "stop"))))
      (harmless-hook-prompt-default
       session hook payload (lambda (value) (setq got value)))
      (should (equal got "replace: next"))
      (should (null tools))
      (should (string-search "Look" body))
      (should (string-search "command=\"/\"" body))
      (should (string-search "c:/" body))
      (should (string-search "Do not call a tool." body))
      (should (null (harmless-hook-test-children)))
      (should-not (file-exists-p "/c:"))
      (let ((bare (harmless-hook-test-session repo)))
        (setf (harmless-session-provider bare) nil)
        (setq called nil)
        (should (equal (harmless-hook-test-error
                        (lambda ()
                          (harmless-hook-prompt-default
                           bare hook payload
                           (lambda (_value) (setq called t)))))
                       "hook prompt has no provider"))
        (should-not called)
        (should (equal (harmless-hook-test-error
                        (lambda ()
                          (harmless-hook-agent-default
                           bare (list :event 'pre :type 'agent)
                           payload
                           (lambda (_value) (setq called t)))))
                       "hook agent has no provider")))
      (setq got nil)
      (setq tools t)
      (cl-letf (((symbol-function 'harmless-turn-run)
                 (lambda (&rest _args) (setq turns (1+ turns)))))
        (setf (harmless-hook-fake-script provider)
              (lambda (messages tools-arg callback)
                (setq body (plist-get (car messages) :content))
                (setq tools tools-arg)
                (funcall callback '(:text "block: stop"))
                (funcall callback '(:stop "stop"))))
        (harmless-hook-agent-default
         session (list :event 'pre :type 'agent :prompt "Look at /")
         payload (lambda (value) (setq got value))))
      (should (equal got "block: stop"))
      (should (null tools))
      (should (= turns 0))
      (should (string-search "/" body))
      (should (string-search "c:/" body))
      (let ((children (harmless-hook-test-children)))
        (should (= (length children) 1))
        (harmless-hook-test-assert-session (car children))
        (should (harmless-same-directory-p
                     repo (harmless-session-cwd (car children))))
        (should-not (harmless-filesystem-root-p
                     (harmless-session-cwd (car children)))))
      (let ((harmless-hooks
             (list (list :event 'pre :type 'prompt)))
            (harmless-hook-prompt-function
             (lambda (&rest _args)
               (error "hook prompt has no provider"))))
        (setq got nil)
        (harmless-hook-pre
         session tool '(:command "/")
         (lambda (blocked) (setq got blocked)))
        (should (equal got
                       (concat "Error: hook blocked this tool call: "
                               "hook prompt has no provider")))
        (harmless-hook-post
         session tool '(:path "/") "REAL"
         (lambda (text) (setq got text)))
        (should (equal got "REAL")))
      (ignore data))))

(ert-deftest harmless-hook-blocks-before-the-question ()
  (harmless-hook-test-with data repo
    (let ((session (harmless-hook-test-session repo))
          (path (expand-file-name "note.txt" repo)))
      (harmless-hook-test-assert-session session)
      (should-not (plist-member (harmless-hook-test-summary session) :hooks))
      (setq harmless-hook-test-decision 'deny)
      (harmless-hook-test-run
       session "harmless-tool-write-file" '(:path "note.txt" :contents "yes\n"))
      (should (equal harmless-hook-test-asked '((edit "note.txt"))))
      (should (equal (harmless-hook-test-last session)
                     "Error: user denied this tool call"))
      (should-not (file-exists-p path))
      (setq harmless-hooks '())
      (harmless-hook-test-run
       session "harmless-tool-write-file" '(:path "note.txt" :contents "yes\n"))
      (should (equal harmless-hook-test-asked '((edit "note.txt"))))
      (should-not (file-exists-p path))
      (setq harmless-hooks "")
      (setq harmless-hook-test-decision 'allow)
      (harmless-hook-test-run
       session "harmless-tool-write-file" '(:path "note.txt" :contents "yes\n"))
      (should-not harmless-hook-test-asked)
      (should (equal (harmless-hook-test-last session)
                     (concat "Error: hook blocked this tool call: "
                             "hooks must be a list")))
      (should-not (file-exists-p path))
      (setq harmless-hooks
            '((:event pre :type command :command "/")))
      (harmless-hook-test-run
       session "harmless-tool-write-file" '(:path "note.txt" :contents "yes\n"))
      (should (equal (harmless-hook-test-last session)
                     (concat "Error: hook blocked this tool call: "
                             "hook command is not a command")))
      (should-not (file-exists-p path))
      (should-not (file-exists-p "/note.txt"))
      (setq harmless-hooks
            '((:event pre :type http :url "c:/")))
      (harmless-hook-test-run
       session "harmless-tool-write-file" '(:path "/" :contents "yes\n"))
      (should (equal (harmless-hook-test-last session)
                     (concat "Error: hook blocked this tool call: "
                             "hook url is not a url")))
      (should-not (file-exists-p "/yes"))
      (setq harmless-hook-test-seen nil)
      (setq harmless-hooks
            (list
             (list :event 'pre :type 'function
                   :function
                   (lambda (_session _tool args _text)
                     (push (harmless-tool-arg args :path)
                           harmless-hook-test-seen)
                     'block))
             (list :event 'pre :type 'function
                   :function
                   (lambda (&rest _args) (error "second ran")))
             (list :event 'post :type 'function
                   :function
                   (lambda (&rest _args) (error "post ran")))))
      (harmless-hook-test-run
       session "harmless-tool-write-file" '(:path "../note.txt" :contents "yes\n"))
      (should (equal harmless-hook-test-seen '("../note.txt")))
      (should-not harmless-hook-test-asked)
      (should (equal (harmless-hook-test-last session)
                     "Error: hook blocked this tool call"))
      (should-not (file-exists-p (expand-file-name "../note.txt" repo)))
      (should-not (file-exists-p path))
      (setq harmless-hook-test-decision 'deny)
      (setq harmless-hooks
            (list
             (list :event 'pre :tool "harmless-tool-write-file/" :type 'function
                   :function (lambda (&rest _) 'block))
             (list :event 'pre :tool "harmless-tool-read-file" :type 'function
                   :function (lambda (&rest _) 'block))))
      (harmless-hook-test-run
       session "harmless-tool-write-file" '(:path "note.txt/" :contents "yes\n"))
      (should (equal harmless-hook-test-asked '((edit "note.txt/"))))
      (should (equal (harmless-hook-test-last session)
                     "Error: user denied this tool call"))
      (should-not (file-exists-p path))
      (setq harmless-hook-test-decision 'allow)
      (setq harmless-hooks
            (list
             (list :event 'pre :tool "harmless-tool-write-file" :type 'function
                   :function
                   (lambda (_session _tool _args _text)
                     '(:decision block :reason "no")))))
      (harmless-hook-test-run
       session "harmless-tool-write-file" '(:path "note.txt" :contents "yes\n"))
      (should-not harmless-hook-test-asked)
      (should (equal (harmless-hook-test-last session)
                     "Error: hook blocked this tool call: no"))
      (should-not (file-exists-p path))
      (setq harmless-hook-test-decision 'deny)
      (setq harmless-hooks
            (list
             (list :event 'pre :type 'function
                   :function (lambda (&rest _) 'allow))
             (list :event 'post :type 'function
                   :function
                   (lambda (&rest _)
                     (push 'post harmless-hook-test-seen)
                     nil))))
      (setq harmless-hook-test-seen nil)
      (harmless-hook-test-run
       session "harmless-tool-write-file" '(:path "ask.txt" :contents "yes\n"))
      (should (equal harmless-hook-test-asked '((edit "ask.txt"))))
      (should (equal (harmless-hook-test-last session)
                     "Error: user denied this tool call"))
      (should-not (file-exists-p (expand-file-name "ask.txt" repo)))
      (should-not (memq 'post harmless-hook-test-seen))
      (setq harmless-hook-test-seen nil)
      (let ((harmless-dangerous-commands '("echo")))
        (setq harmless-hooks
              (list (list :event 'pre :type 'function
                          :function
                          (lambda (&rest _)
                            (push 'ran harmless-hook-test-seen)
                            'allow))))
        (harmless-hook-test-run
         session "harmless-tool-run-shell"
         '(:command "echo allowed>harmless-hook-danger.txt"))
        (should-not harmless-hook-test-seen)
        (should-not harmless-hook-test-asked)
        (should (equal (harmless-hook-test-last session)
                       "Error: dangerous command refused: echo"))
        (should-not
         (file-exists-p
          (expand-file-name "harmless-hook-danger.txt" repo))))
      (setf (harmless-session-plan-mode session) t)
      (setq harmless-hook-test-seen nil)
      (harmless-hook-test-run
       session "harmless-tool-run-shell"
       '(:command "echo allowed>harmless-hook-plan.txt"))
      (should-not harmless-hook-test-seen)
      (should-not harmless-hook-test-asked)
      (should (equal (harmless-hook-test-last session)
                     harmless-shell-plan-refusal))
      (should-not
       (file-exists-p
        (expand-file-name "harmless-hook-plan.txt" repo)))
      (setf (harmless-session-plan-mode session) nil)
      (harmless-perm-set-rules session nil '("Bash(echo allowed*)"))
      (setq harmless-hook-test-seen nil)
      (harmless-hook-test-run
       session "harmless-tool-run-shell"
       '(:command "echo allowed>harmless-hook-denied.txt"))
      (should-not harmless-hook-test-seen)
      (should-not harmless-hook-test-asked)
      (should (equal (harmless-hook-test-last session)
                     (concat "Error: Permission rule denied "
                             "Bash(echo allowed*)")))
      (should-not
       (file-exists-p
        (expand-file-name "harmless-hook-denied.txt" repo)))
      (harmless-perm-set-rules session '("Bash(echo allowed*)") nil)
      (setq harmless-hook-test-decision 'deny)
      (setq harmless-hook-test-seen nil)
      (setq harmless-hooks
            (list (list :event 'pre :type 'function
                        :function
                        (lambda (&rest _)
                          (push 'ran harmless-hook-test-seen)
                          nil))))
      (harmless-hook-test-run
       session "harmless-tool-run-shell"
       '(:command "echo allowed>harmless-hook-rule.txt"))
      (should (equal harmless-hook-test-seen '(ran)))
      (should-not harmless-hook-test-asked)
      (let ((rule (expand-file-name "harmless-hook-rule.txt" repo)))
        (harmless-hook-test-assert-written rule repo)
        (should (string-match-p "allowed" (harmless-hook-test-read rule))))
      (harmless-perm-set-rules session nil nil)
      (let ((auto (harmless-hook-test-session
                   repo :permission-mode 'auto))
            (reviews nil))
        (setq harmless-auto-review-function
              (lambda (&rest _args)
                (push 'reviewed reviews)
                'allow))
        (setq harmless-hooks
              (list (list :event 'pre :type 'function
                          :function (lambda (&rest _) 'block))))
        (harmless-hook-test-run
         auto "harmless-tool-write-file" '(:path "auto.txt" :contents "yes\n"))
        (should-not reviews)
        (should-not harmless-hook-test-asked)
        (should (equal (harmless-hook-test-last auto)
                       "Error: hook blocked this tool call"))
        (should-not (file-exists-p (expand-file-name "auto.txt" repo)))
        (setq harmless-hooks
              (list (list :event 'pre :type 'function
                          :function (lambda (&rest _) nil))
                    (list :event 'post :type 'function
                          :function
                          (lambda (&rest _)
                            (push 'post harmless-hook-test-seen)
                            nil))))
        (setq harmless-hook-test-seen nil)
        (setq harmless-hook-test-decision 'deny)
        (setq harmless-auto-review-function
              (lambda (&rest _args)
                (push 'reviewed reviews)
                'escalate))
        (harmless-hook-test-run
         auto "harmless-tool-write-file" '(:path "auto.txt" :contents "yes\n"))
        (should (equal reviews '(reviewed)))
        (should (equal harmless-hook-test-asked '((edit "auto.txt"))))
        (should (equal (harmless-hook-test-last auto)
                       "Error: user denied this tool call"))
        (should-not (memq 'post harmless-hook-test-seen)))
      (let ((always (harmless-hook-test-session
                     repo :permission-mode 'always-approve)))
        (setq harmless-hooks
              (list (list :event 'pre :type 'function
                          :function
                          (lambda (&rest _)
                            '(:decision block :reason "/")))))
        (harmless-hook-test-run
         always "harmless-tool-write-file" '(:path "note.txt" :contents "yes\n"))
        (should-not harmless-hook-test-asked)
        (should (equal (harmless-hook-test-last always)
                       "Error: hook blocked this tool call: /"))
        (should-not (file-exists-p path))
        (should-not (file-exists-p "/note.txt")))
      (let ((url nil)
            (payload nil))
        (setq harmless-hook-http-function
              (lambda (hook body callback)
                (setq url (plist-get hook :url))
                (setq payload body)
                (funcall callback
                         '(:status 0 :stdout "block: later" :stderr ""))))
        (setq harmless-hooks
              '((:event pre :type http :url "https://example.com/hook/")))
        (harmless-hook-test-run
         session "harmless-tool-write-file" '(:path "note.txt" :contents "yes\n"))
        (should (equal url "https://example.com/hook/"))
        (should (equal (plist-get payload :tool) "harmless-tool-write-file"))
        (should (equal (plist-get (plist-get payload :arguments) :path)
                       "note.txt"))
        (should (equal (harmless-hook-test-last session)
                       "Error: hook blocked this tool call: later"))
        (should-not (file-exists-p path)))
      (setq harmless-hook-http-function
            (lambda (_hook _payload callback)
              (run-at-time
               0 nil
               (lambda ()
                 (funcall callback
                          '(:status 0 :stdout "block: async"))))))
      (setq harmless-hooks
            '((:event pre :type http :url "https://example.com/hook")))
      (harmless-hook-test-run
       session "harmless-tool-write-file" '(:path "async.txt" :contents "no\n"))
      (should (eq 'idle (harmless-session-status session)))
      (should (equal (harmless-hook-test-last session)
                     "Error: hook blocked this tool call: async"))
      (should-not (file-exists-p (expand-file-name "async.txt" repo)))
      (with-temp-file (expand-file-name "secret.txt" repo)
        (insert "SECRET-BYTES"))
      (setq harmless-hooks
            (list (list :event 'pre :tool "harmless-tool-read-file" :type 'function
                        :function (lambda (&rest _) 'block))))
      (harmless-hook-test-run
       session "harmless-tool-read-file" '(:path "secret.txt"))
      (should-not harmless-hook-test-asked)
      (should (equal (harmless-hook-test-last session)
                     "Error: hook blocked this tool call"))
      (should-not (string-search
                   "SECRET-BYTES" (harmless-hook-test-last session)))
      (ignore data)
      (harmless-hook-test-assert-session session)
      (should-not (plist-member (harmless-hook-test-summary session)
                                :hooks)))))

(ert-deftest harmless-hook-replaces-the-result-the-model-sees ()
  (harmless-hook-test-with data repo
    (let ((session (harmless-hook-test-session repo))
          (path (expand-file-name "kept.txt" repo)))
      (setq harmless-hook-test-seen nil)
      (setq harmless-hooks
            (list
             (list :event 'post :type 'function
                   :function
                   (lambda (_session _tool _args text)
                     (push text harmless-hook-test-seen)
                     "replace: one"))
             (list :event 'post :type 'function
                   :function
                   (lambda (_session _tool _args text)
                     (push text harmless-hook-test-seen)
                     "two"))))
      (harmless-hook-test-run
       session "harmless-tool-write-file" '(:path "kept.txt" :contents "yes\n"))
      (should (equal (car harmless-hook-test-seen) "one"))
      (should (string-prefix-p "Wrote " (cadr harmless-hook-test-seen)))
      (should (string-search "kept.txt" (cadr harmless-hook-test-seen)))
      (should (equal harmless-hook-test-asked '((edit "kept.txt"))))
      (should (equal (harmless-hook-test-last session) "two"))
      (harmless-hook-test-assert-written path repo)
      (should (equal "yes\n" (harmless-hook-test-read path)))
      (setq harmless-hooks
            (list (list :event 'post :type 'function
                        :function (lambda (&rest _) (error "boom")))))
      (harmless-hook-test-run
       session "harmless-tool-write-file" '(:path "kept.txt" :contents "yes\n"))
      (should (string-prefix-p "Wrote " (harmless-hook-test-last session)))
      (should (equal "yes\n" (harmless-hook-test-read path)))
      (setq harmless-hook-test-seen nil)
      (setq harmless-hook-test-decision 'deny)
      (setq harmless-hooks
            (list (list :event 'post :type 'function
                        :function
                        (lambda (&rest _)
                          (push 'post harmless-hook-test-seen)
                          "hidden"))))
      (harmless-hook-test-run
       session "harmless-tool-write-file" '(:path "hidden.txt" :contents "no\n"))
      (should-not harmless-hook-test-seen)
      (should (equal (harmless-hook-test-last session)
                     "Error: user denied this tool call"))
      (should-not (file-exists-p (expand-file-name "hidden.txt" repo)))
      (setf (harmless-session-plan-mode session) t)
      (harmless-hook-test-run
       session "harmless-tool-run-shell"
       '(:command "echo allowed>harmless-hook-post-plan.txt"))
      (should-not harmless-hook-test-seen)
      (should (equal (harmless-hook-test-last session)
                     harmless-shell-plan-refusal))
      (setf (harmless-session-plan-mode session) nil)
      (setq harmless-hook-test-decision 'allow)
      (setq harmless-hook-command-function
            (lambda (_hook payload callback)
              (should (equal (plist-get payload :tool) "harmless-tool-write-file"))
              (funcall callback
                       '(:status 0 :stdout "replace: SECRET" :stderr ""))))
      (setq harmless-hooks
            '((:event post :type command :command "../note.txt/")))
      (harmless-hook-test-run
       session "harmless-tool-write-file" '(:path "kept.txt" :contents "yes\n"))
      (should (equal (harmless-hook-test-last session) "SECRET"))
      (should (equal "yes\n" (harmless-hook-test-read path)))
      (should-not (file-exists-p (expand-file-name "../note.txt/" repo)))
      (setq harmless-hook-command-function
            (lambda (_hook _payload callback)
              (funcall callback
                       '(:status 1 :stdout "replace: NO" :stderr "fail"))))
      (harmless-hook-test-run
       session "harmless-tool-write-file" '(:path "kept.txt" :contents "yes\n"))
      (should (string-prefix-p "Wrote " (harmless-hook-test-last session)))
      (setq harmless-hook-http-function
            (lambda (hook _payload callback)
              (should (equal (plist-get hook :url)
                             "https://example.com/hook"))
              (funcall callback
                       '(:status 200 :stdout "ok" :stderr ""))))
      (setq harmless-hooks
            '((:event post :type http :url "https://example.com/hook")))
      (harmless-hook-test-run
       session "harmless-tool-write-file" '(:path "kept.txt" :contents "yes\n"))
      (should (string-prefix-p "Wrote " (harmless-hook-test-last session)))
      (should (equal "yes\n" (harmless-hook-test-read path)))
      (setq harmless-hook-mcp-function
            (lambda (_session hook payload callback)
              (should (equal (plist-get hook :server) "demo"))
              (should (equal (plist-get hook :method) "check"))
              (should (equal (plist-get payload :event) "post"))
              (should (string-prefix-p
                       "Wrote " (plist-get payload :result)))
              (funcall callback "I would block this\nreplace: from-mcp")))
      (setq harmless-hooks
            '((:event post :type mcp :server "demo" :method "check")))
      (harmless-hook-test-run
       session "harmless-tool-write-file" '(:path "kept.txt" :contents "yes\n"))
      (should (equal (harmless-hook-test-last session) "from-mcp"))
      (should (equal "yes\n" (harmless-hook-test-read path)))
      (setq harmless-hook-test-seen nil)
      (setq harmless-hook-prompt-function
            (lambda (_session hook payload callback)
              (push (list (plist-get hook :prompt) payload)
                    harmless-hook-test-seen)
              (funcall callback "The file looks fine.")))
      (setq harmless-hooks
            '((:event post :type prompt :prompt "c:/")))
      (harmless-hook-test-run
       session "harmless-tool-write-file" '(:path "kept.txt" :contents "yes\n"))
      (should (equal (car (car harmless-hook-test-seen)) "c:/"))
      (should (equal (plist-get (cadr (car harmless-hook-test-seen)) :tool)
                     "harmless-tool-write-file"))
      (should (string-prefix-p "Wrote " (harmless-hook-test-last session)))
      (setq harmless-hook-prompt-function
            (lambda (_session _hook _payload callback)
              (funcall callback "replace: from-prompt")))
      (harmless-hook-test-run
       session "harmless-tool-write-file" '(:path "kept.txt" :contents "yes\n"))
      (should (equal (harmless-hook-test-last session) "from-prompt"))
      (setq harmless-hook-agent-function
            (lambda (_session _hook _payload callback)
              (funcall callback
                       "please block: not a line\nreplace: from-agent")))
      (setq harmless-hooks '((:event post :type agent)))
      (harmless-hook-test-run
       session "harmless-tool-write-file" '(:path "kept.txt" :contents "yes\n"))
      (should (equal (harmless-hook-test-last session) "from-agent"))
      (should (equal "yes\n" (harmless-hook-test-read path)))
      (setq harmless-hook-test-decision 'allow)
      (setq harmless-hooks
            (list (list :event 'pre :type 'prompt
                        :function nil)
                  (list :event 'post :type 'function
                        :function (lambda (&rest _) "kept-out"))))
      (setq harmless-hook-prompt-function
            (lambda (_session _hook _payload callback)
              (funcall callback "I would block this")))
      (harmless-hook-test-run session "harmless-tool-run-shell" '(:command ""))
      (should (equal (harmless-hook-test-last session) "kept-out"))
      (harmless-hook-test-assert-session session)
      (should-not (plist-member (harmless-hook-test-summary session)
                                :hooks))
      (ignore data))))
