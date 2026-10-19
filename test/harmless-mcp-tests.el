;;; harmless-mcp-tests.el --- Tests for the Harmless MCP client -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless-provider)
(require 'harmless-session)
(require 'harmless-tools)
(require 'harmless-perm)
(require 'harmless-turn)
(require 'harmless-agent)
(require 'harmless-mcp)
(require 'harmless-ui)

(cl-defstruct (harmless-mcp-fake
               (:include harmless-provider)
               (:constructor harmless-mcp-make-fake))
  script)

(cl-defmethod harmless-provider-complete ((provider harmless-mcp-fake)
                                          _messages _tools callback)
  (when (harmless-mcp-fake-script provider)
    (funcall (harmless-mcp-fake-script provider) _messages callback))
  nil)

(defmacro harmless-mcp-test-with (data &rest body)
  "Bind a temp data directory DATA, then run BODY."
  (declare (indent 1))
  `(let* ((,data (make-temp-file "harmless-mcp-data-" t))
          (harmless-directory ,data)
          (harmless--sessions (make-hash-table :test 'equal))
          (harmless-mcp-call-function
           (lambda (_server method _params)
             (cond
              ((equal method "tools/list") '(:tools nil))
              ((equal method "resources/list") '(:resources nil))
              ((equal method "resources/read")
               '(:contents ((:text "body"))))
              (t (error "unexpected method %s" method)))))
          (harmless-mcp-exchange-function
           (lambda (&rest _) (error "exchange was called")))
          (harmless-perm-ask-function
           (lambda (_session _class _item callback)
             (funcall callback 'allow))))
     ,@body))

(defun harmless-mcp-test-session (&rest args)
  "Return a session.  ARGS are passed to `harmless-session-new'."
  (apply #'harmless-session-new
         :provider (harmless-mcp-make-fake :name "fake" :host "none")
         :model "m"
         args))

(defun harmless-mcp-test-write (file text)
  "Write TEXT to FILE as UTF-8 and return FILE."
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file file
      (insert text)))
  file)

(defun harmless-mcp-test-error (thunk)
  "Return the error string from THUNK."
  (condition-case err
      (progn (funcall thunk) "no-error")
    (error (error-message-string err))))

(defun harmless-mcp-test-servers (session)
  "Return SESSION's servers as (NAME COMMAND ARGS) lists."
  (mapcar (lambda (server)
            (list (plist-get server :name)
                  (plist-get server :command)
                  (plist-get server :args)))
          (harmless-mcp-servers session)))

(defun harmless-mcp-test-not-root (session)
  "Require SESSION's directory to stay off the filesystem root."
  (let ((dir (harmless-session-directory session)))
    (should (file-name-absolute-p dir))
    (should-not (harmless-filesystem-root-p dir))
    (should (harmless-directory-strictly-under-p dir harmless-directory))
    (should-not (equal (file-truename dir)
                       (file-truename (expand-file-name "/"))))))

(defun harmless-mcp-test-transcript (session)
  "Return SESSION's messages.jsonl, or an empty string."
  (let ((file (expand-file-name "messages.jsonl"
                                (harmless-session-directory session))))
    (if (file-regular-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (buffer-string))
      "")))

(defun harmless-mcp-test-tree (dir)
  "Return the files under DIR, sorted."
  (sort (directory-files-recursively dir ".") #'string<))

(defconst harmless-mcp-test-schema
  '(:type "object"
    :properties (:text (:type "string"))
    :required ("text"))
  "Sample input schema with one required string.")

(ert-deftest harmless-mcp-refuses-a-bad-config ()
  (harmless-mcp-test-with data
    (let* ((repo (make-temp-file "harmless-mcp-repo-" t))
           (session (harmless-mcp-test-session
                     :cwd repo :id "s" :title "Keep"))
           (global (harmless-mcp-global-file))
           (project (harmless-mcp-project-file session)))
      (should (file-name-absolute-p global))
      (should (file-name-absolute-p project))
      (should (equal project
                     (expand-file-name ".harmless/mcp.json" repo)))
      (should (string-suffix-p "mcp.json" global))
      (should-not (equal global "/mcp.json"))
      (should-not (equal project "/.harmless/mcp.json"))
      (should-not (harmless-filesystem-root-p
                   (file-name-directory global)))
      (should (harmless-directory-strictly-under-p global data))
      (should (equal (harmless-mcp-test-servers session) nil))
      (dolist (text '("{" "not json" ""))
        (harmless-mcp-test-write project text)
        (should (equal (harmless-mcp-test-error
                        (lambda () (harmless-mcp-servers session)))
                       "mcp.json is not valid")))
      (harmless-mcp-test-write project "{\"nope\":1}")
      (should (equal (harmless-mcp-test-error
                      (lambda () (harmless-mcp-servers session)))
                     "mcp.json has no servers"))
      (harmless-mcp-test-write project "{\"mcpServers\":\"x\"}")
      (should (equal (harmless-mcp-test-error
                      (lambda () (harmless-mcp-servers session)))
                     "mcp.json has no servers"))
      (harmless-mcp-test-write project "{\"mcpServers\":{}}")
      (should (equal (harmless-mcp-test-servers session) nil))
      (dolist (pair '(("feat/" "MCP server name is not a name")
                      ("/" "MCP server name is not a name")
                      ("c:/" "MCP server name is not a name")
                      ("a/b" "MCP server name is not a name")))
        (harmless-mcp-test-write
         project
         (format "{\"mcpServers\":{\"%s\":{\"command\":\"demo-mcp\"}}}"
                 (car pair)))
        (should (equal (harmless-mcp-test-error
                        (lambda () (harmless-mcp-servers session)))
                       (cadr pair))))
      (harmless-mcp-test-write
       project
       "{\"mcpServers\":{\"demo\":{}}}")
      (should (equal (harmless-mcp-test-error
                      (lambda () (harmless-mcp-servers session)))
                     "MCP command is empty"))
      (harmless-mcp-test-write
       project
       "{\"mcpServers\":{\"demo\":{\"command\":\"   \"}}}")
      (should (equal (harmless-mcp-test-error
                      (lambda () (harmless-mcp-servers session)))
                     "MCP command is empty"))
      (harmless-mcp-test-write
       project
       "{\"mcpServers\":{\"demo\":{\"command\":42}}}")
      (should (equal (harmless-mcp-test-error
                      (lambda () (harmless-mcp-servers session)))
                     "MCP command must be a string"))
      (harmless-mcp-test-write
       project
       "{\"mcpServers\":{\"demo\":{\"command\":\"demo-mcp\",\"args\":[1]}}}")
      (should (equal (harmless-mcp-test-error
                      (lambda () (harmless-mcp-servers session)))
                     "MCP argument must be a string"))
      (harmless-mcp-test-write
       project
       "{\"mcpServers\":{\"sub \":{\"command\":\"  demo-mcp  \"}}}")
      (should (equal (harmless-mcp-test-servers session)
                     '(("sub" "demo-mcp" nil))))
      (should (equal (with-temp-buffer
                       (insert-file-contents project)
                       (buffer-string))
                     "{\"mcpServers\":{\"sub \":{\"command\":\"  demo-mcp  \"}}}"))
      (dolist (cwd '("" "sub" "sub/" "/"))
        (setf (harmless-session-cwd session) cwd)
        (should-not (harmless-mcp-project-file session)))
      (setf (harmless-session-cwd session) nil)
      (should-not (harmless-mcp-project-file session))
      (setf (harmless-session-cwd session) "c:/")
      (let ((drive (harmless-mcp-project-file session)))
        (should (stringp drive))
        (should-not (equal drive "/.harmless/mcp.json"))
        (should-not (equal drive "/mcp.json"))
        (should (string-suffix-p ".harmless/mcp.json"
                                 (subst-char-in-string ?\\ ?/ drive))))
      (setf (harmless-session-cwd session) (file-name-as-directory repo))
      (should (equal (harmless-mcp-project-file session) project))
      (harmless-mcp-test-not-root session))))

(ert-deftest harmless-mcp-merges-global-and-project ()
  (harmless-mcp-test-with data
    (let* ((repo (make-temp-file "harmless-mcp-repo-" t))
           (other (make-temp-file "harmless-mcp-other-" t))
           (session (harmless-mcp-test-session
                     :cwd repo :id "s" :title "Keep"))
           (detached (harmless-mcp-test-session
                      :detached t :id "chat" :title "Chat"))
           (global (harmless-mcp-global-file))
           (project (harmless-mcp-project-file session))
           (secret (expand-file-name ".harmless/mcp.json" other)))
      (harmless-mcp-test-write
       global
       (concat "{\"mcpServers\":{"
               "\"demo\":{\"command\":\"global-mcp\",\"args\":[\"g\"]},"
               "\"only-global\":{\"command\":\"g-mcp\"}}}"))
      (harmless-mcp-test-write
       project
       (concat "{\"mcpServers\":{"
               "\"demo\":{\"command\":\"project-mcp\",\"args\":[\"p\"]},"
               "\"only-project\":{\"command\":\"p-mcp\","
               "\"args\":[\"--stdio\"]}}}"))
      (harmless-mcp-test-write
       secret
       "{\"mcpServers\":{\"secret\":{\"command\":\"secret-mcp\"}}}")
      (should (equal (harmless-mcp-test-servers session)
                     '(("demo" "project-mcp" ("p"))
                       ("only-global" "g-mcp" nil)
                       ("only-project" "p-mcp" ("--stdio")))))
      (let ((default-directory (file-name-as-directory other)))
        (should (equal (harmless-mcp-test-servers detached)
                       '(("demo" "global-mcp" ("g"))
                         ("only-global" "g-mcp" nil))))
        (should-not (harmless-mcp-project-file detached)))
      (should (equal (with-temp-buffer
                       (insert-file-contents global)
                       (buffer-string))
                     (concat "{\"mcpServers\":{"
                             "\"demo\":{\"command\":\"global-mcp\",\"args\":[\"g\"]},"
                             "\"only-global\":{\"command\":\"g-mcp\"}}}")))
      (harmless-mcp-test-not-root session)
      (harmless-mcp-test-not-root detached))))

(ert-deftest harmless-mcp-loads-schema-when-called ()
  (harmless-mcp-test-with _data
    (let* ((repo (make-temp-file "harmless-mcp-repo-" t))
           (calls 0)
           (seen nil)
           (session (harmless-mcp-test-session
                     :cwd repo :id "s" :title "Keep"
                     :provider
                     (harmless-mcp-make-fake
                      :name "fake" :host "none"
                      :script (lambda (&rest _) (setq calls (1+ calls))))))
           (before (harmless-mcp-test-transcript session))
           (tree (harmless-mcp-test-tree repo))
           (harmless-mcp-call-function
            (lambda (server method params)
              (push (list (plist-get server :name) method params) seen)
              (cond
               ((equal (plist-get server :name) "bad")
                (error "down"))
               ((equal method "tools/list")
                (list :tools
                      (list (list :name "echo"
                                  :description "Echo"
                                  :inputSchema harmless-mcp-test-schema)
                            (list :name "" :description "skip"))))
               ((equal method "tools/call")
                (let ((text (plist-get (plist-get params :arguments) :text)))
                  (list :content
                        (list (list :type "text" :text (or text ""))
                              (list :type "text" :text "tail")))))
               (t (error "unexpected %s" method))))))
      (harmless-mcp-test-write
       (harmless-mcp-project-file session)
       (concat "{\"mcpServers\":{"
               "\"bad\":{\"command\":\"bad-mcp\"},"
               "\"demo\":{\"command\":\"demo-mcp\",\"args\":[\"--stdio\"]}}}"))
      (setq tree (harmless-mcp-test-tree repo))
      (setq seen nil)
      (let* ((tools (harmless-mcp-tools session))
             (tool (car tools)))
        (should (= (length tools) 1))
        (should (equal (reverse (mapcar (lambda (item)
                                          (list (nth 0 item) (nth 1 item)))
                                        seen))
                       '(("bad" "tools/list")
                         ("demo" "tools/list"))))
        (should (equal (harmless-tool-name tool) "mcp__demo__echo"))
        (should (equal (harmless-tool-description tool) "Echo"))
        (should (eq (harmless-tool-class tool) 'network))
        (should-not (harmless-tool-async tool))
        (should-not (harmless-tool-classify tool))
        (should (equal (harmless-tool-schema tool) '(:type "object")))
        (should-not (harmless-tool-by-name "mcp__demo__echo"))
        (should (equal (mapcar #'harmless-tool-name
                               (harmless-tools-for-session session))
                       (append (mapcar #'harmless-tool-name
                                       (harmless-tools-enabled))
                               '("mcp__demo__echo"))))
        (setq seen nil)
        (should (equal (harmless-tool-call tool session nil)
                       "Error: MCP argument text is required"))
        (should (equal (reverse seen) '(("demo" "tools/list" nil))))
        (should (equal (harmless-tool-schema tool) harmless-mcp-test-schema))
        (setq seen nil)
        (should (equal (harmless-tool-call tool session '(:text nil))
                       "Error: MCP argument text is required"))
        (should (equal (mapcar #'cadr (reverse seen)) '("tools/list")))
        (setq seen nil)
        (should (equal (harmless-tool-call tool session '("text" "hi"))
                       "hi\ntail"))
        (should (equal (reverse seen)
                       '(("demo" "tools/list" nil)
                         ("demo" "tools/call"
                          (:name "echo" :arguments (:text "hi"))))))
        (setq seen nil)
        (let ((harmless-mcp-call-function
               (lambda (_server method _params)
                 (push method seen)
                 (if (equal method "tools/list")
                     (list :tools
                           (list (list :name "echo"
                                       :inputSchema harmless-mcp-test-schema)))
                   '(:isError t
                     :content ((:type "text" :text "bad")))))))
          (should (equal (harmless-tool-call tool session '(:text "x"))
                         "Error: bad"))
          (should (equal (reverse seen) '("tools/list" "tools/call"))))
        (setq seen nil)
        (let ((harmless-mcp-call-function
               (lambda (_server method _params)
                 (push method seen)
                 (if (equal method "tools/list")
                     (list :tools
                           (list (list :name "echo"
                                       :inputSchema harmless-mcp-test-schema)))
                   '(:content ((:type "blob" :blob "qq")))))))
          (should (equal (harmless-tool-call tool session '(:text "x"))
                         "Error: MCP call returned no text"))
          (should (equal (reverse seen) '("tools/list" "tools/call"))))
        (should (= calls 0))
        (should (eq (harmless-session-status session) 'idle))
        (should (equal (harmless-session-title session) "Keep"))
        (should (equal (harmless-mcp-test-transcript session) before))
        (should (equal (harmless-mcp-test-tree repo) tree))
        (harmless-mcp-test-not-root session)))))

(ert-deftest harmless-mcp-reads-resources ()
  (harmless-mcp-test-with _data
    (let* ((repo (make-temp-file "harmless-mcp-repo-" t))
           (seen nil)
           (session (harmless-mcp-test-session
                     :cwd repo :id "s" :title "Keep"))
           (before (harmless-mcp-test-transcript session))
           (harmless-mcp-call-function
            (lambda (_server method params)
              (push (list method params) seen)
              (cond
               ((equal method "resources/list")
                '(:resources ((:uri "file:///a" :name "Notes")
                              (:uri "file:///b" :name "  ")
                              (:uri "  " :name "skip")
                              (:uri "file:///c" :name "C"))))
               ((equal method "resources/read")
                (if (equal (plist-get params :uri) "blob")
                    '(:contents ((:blob "qq")))
                  '(:contents ((:text "one") (:text "two")))))
               (t (error "unexpected %s" method))))))
      (harmless-mcp-test-write
       (harmless-mcp-project-file session)
       "{\"mcpServers\":{\"demo\":{\"command\":\"demo-mcp\"}}}")
      (let ((tree (harmless-mcp-test-tree repo)))
        (dolist (args '(nil
                        (:server nil)
                        (:server "")
                        (:server "  ")
                        (:server "\n")
                        ("server" "   ")))
          (should (equal (harmless-tool-call
                          (harmless-tool-by-name "mcp_list_resources")
                          session args)
                         "Error: server name is empty")))
        (dolist (args '((:server :false) (:server 42) ("server" :false)))
          (should (equal (harmless-tool-call
                          (harmless-tool-by-name "mcp_list_resources")
                          session args)
                         "Error: server name must be a string")))
        (dolist (name '("feat/" "/" "c:/" "a/b" "a\\b" "." ".."))
          (should (equal (harmless-tool-call
                          (harmless-tool-by-name "mcp_list_resources")
                          session (list :server name))
                         "Error: server name is not a name")))
        (should (equal (harmless-tool-call
                        (harmless-tool-by-name "mcp_list_resources")
                        session '(:server "missing"))
                       "Error: no MCP server missing"))
        (should (equal (harmless-tool-call
                        (harmless-tool-by-name "mcp_list_resources")
                        session '(:server "  demo  "))
                       (concat "file:///a\nNotes\n\n"
                               "file:///b\n\n"
                               "file:///c\nC")))
        (should (equal (car seen) '("resources/list" nil)))
        (setq seen nil)
        (dolist (args '(nil (:uri nil) (:uri "") (:uri "  ")
                           ("uri" "")))
          (should (equal (harmless-tool-call
                          (harmless-tool-by-name "mcp_read_resource")
                          session (append '(:server "demo") args))
                         "Error: resource uri is empty")))
        (should (equal (harmless-tool-call
                        (harmless-tool-by-name "mcp_read_resource")
                        session '(:server "demo" :uri :false))
                       "Error: resource uri must be a string"))
        (should (equal (harmless-tool-call
                        (harmless-tool-by-name "mcp_read_resource")
                        session '(:server "demo" :uri 42))
                       "Error: resource uri must be a string"))
        (should-not seen)
        (dolist (uri '("/" "c:/" "  file:///a  "))
          (setq seen nil)
          (should (equal (harmless-tool-call
                          (harmless-tool-by-name "mcp_read_resource")
                          session (list :server "demo" :uri uri))
                         "one\ntwo"))
          (should (equal (car seen)
                         (list "resources/read"
                               (list :uri (string-trim uri))))))
        (should (equal (harmless-tool-call
                        (harmless-tool-by-name "mcp_read_resource")
                        session '("server" "demo" "uri" "/"))
                       "one\ntwo"))
        (let ((harmless-mcp-call-function
               (lambda (_server _method _params)
                 '(:resources nil))))
          (should (equal (harmless-tool-call
                          (harmless-tool-by-name "mcp_list_resources")
                          session '(:server "demo"))
                         "No resources")))
        (let ((harmless-mcp-call-function
               (lambda (_server _method params)
                 (should (equal (plist-get params :uri) "blob"))
                 '(:contents ((:blob "qq"))))))
          (should (equal (harmless-tool-call
                          (harmless-tool-by-name "mcp_read_resource")
                          session '(:server "demo" :uri "blob"))
                         "Error: resource has no text")))
        (should (equal (harmless-ui--tool-verb "mcp_list_resources")
                       "resources"))
        (should (equal (harmless-ui--tool-verb "mcp_read_resource")
                       "resource"))
        (should (equal (harmless-ui--tool-verb "mcp__demo__echo") "mcp"))
        (should (equal (harmless-ui--tool-summary
                        "mcp_list_resources" '(:server "demo"))
                       "demo"))
        (should (equal (harmless-ui--tool-summary
                        "mcp_read_resource" '(:server "demo" :uri "/"))
                       "/"))
        (should (equal (harmless-ui--tool-summary
                        "mcp__demo__echo" '(:server "demo" :uri "c:/"))
                       "c:/"))
        (should (eq (harmless-session-status session) 'idle))
        (should (equal (harmless-session-title session) "Keep"))
        (should (equal (harmless-mcp-test-transcript session) before))
        (should (equal (harmless-mcp-test-tree repo) tree))
        (harmless-mcp-test-not-root session)))))

(ert-deftest harmless-mcp-exchange-refuses-a-bad-command ()
  (harmless-mcp-test-with _data
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _) (error "spawned"))))
      (dolist (command '(nil "" "   " "\n"))
        (should (equal (harmless-mcp-test-error
                        (lambda ()
                          (harmless-mcp-exchange-default command nil nil)))
                       "MCP command is empty")))
      (dolist (command '(:false 42))
        (should (equal (harmless-mcp-test-error
                        (lambda ()
                          (harmless-mcp-exchange-default command nil nil)))
                       "MCP command must be a string")))
      (dolist (command '("/" "c:/" "c:\\" "C:/" "demo-mcp" "../x" "sub"))
        (should (equal (harmless-mcp-test-error
                        (lambda ()
                          (harmless-mcp-exchange-default command nil nil)))
                       "MCP command is not a program"))))
    (let* ((seen nil)
           (server '(:name "demo" :command "demo-mcp" :args ("--stdio")))
           (harmless-mcp-exchange-function
            (lambda (command args messages)
              (setq seen (list command args
                               (mapcar (lambda (msg)
                                         (plist-get msg :method))
                                       messages)))
              "{\"error\":{\"message\":\"nope\"}}")))
      (should (equal (harmless-mcp-test-error
                      (lambda ()
                        (harmless-mcp-call-default server "tools/list" nil)))
                     "MCP call failed: nope"))
      (should (equal seen
                     '("demo-mcp" ("--stdio")
                       ("initialize" "notifications/initialized"
                        "tools/list")))))
    (let ((harmless-mcp-exchange-function
           (lambda (&rest _)
             (concat "{\"jsonrpc\":\"2.0\",\"id\":2,\"error\":"
                     "{\"message\":\"nope\"}}\n"))))
      (should (equal (harmless-mcp-test-error
                      (lambda ()
                        (harmless-mcp-call-default
                         '(:name "demo" :command "demo-mcp" :args nil)
                         "tools/call" nil)))
                     "MCP call failed: nope")))
    (let ((harmless-mcp-exchange-function
           (lambda (_command _args messages)
             (should (equal (plist-get (car (last messages)) :method)
                            "tools/list"))
             (should (equal (plist-get (car messages) :id) 1))
             (concat "{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{"
                     "\"tools\":[{\"name\":\"echo\",\"description\":\"Echo\","
                     "\"inputSchema\":{\"type\":\"object\","
                     "\"required\":[\"text\"]}}]}}\n"))))
      (let ((result (harmless-mcp-call-default
                     '(:name "demo" :command "demo-mcp" :args nil)
                     "tools/list" nil)))
        (should (equal (plist-get (car (plist-get result :tools)) :name)
                       "echo"))
        (should (equal (plist-get (plist-get (car (plist-get result :tools))
                                             :inputSchema)
                                  :required)
                       '("text")))))
    (let ((harmless-mcp-exchange-function
           (lambda (&rest _) :not-a-string)))
      (should (equal (harmless-mcp-test-error
                      (lambda ()
                        (harmless-mcp-call-default
                         '(:name "demo" :command "demo-mcp" :args nil)
                         "tools/list" nil)))
                     "MCP call failed")))))

(ert-deftest harmless-mcp-permission-and-plan-mode ()
  (harmless-mcp-test-with _data
    (let* ((repo (make-temp-file "harmless-mcp-repo-" t))
           (calls 0)
           (seen nil)
           (asked nil)
           (harmless-mcp-call-function
            (lambda (_server method params)
              (push (list method params) seen)
              (cond
               ((equal method "tools/list")
                (list :tools
                      (list (list :name "echo"
                                  :description "Echo"
                                  :inputSchema harmless-mcp-test-schema))))
               ((equal method "tools/call")
                '(:content ((:type "text" :text "pong"))))
               ((equal method "resources/list")
                '(:resources ((:uri "file:///a" :name "A"))))
               ((equal method "resources/read")
                '(:contents ((:text "page"))))
               (t (error "unexpected %s" method)))))
           (session
            (harmless-session-new
             :cwd repo :id "s" :title "Keep" :model "m"
             :provider
             (harmless-mcp-make-fake
              :name "fake" :host "none"
              :script
              (lambda (_messages callback)
                (setq calls (1+ calls))
                (if (= calls 1)
                    (progn
                      (funcall callback
                               '(:tool-call "1" "mcp_list_resources"
                                 "{\"server\":\"demo\"}"))
                      (funcall callback '(:stop "tool_calls")))
                  (funcall callback '(:text "done"))
                  (funcall callback '(:stop "stop"))))))))
      (harmless-mcp-test-write
       (harmless-mcp-project-file session)
       "{\"mcpServers\":{\"demo\":{\"command\":\"demo-mcp\"}}}")
      (setq harmless-perm-ask-function
            (lambda (_session class item callback)
              (push (list class item) asked)
              (funcall callback 'deny)))
      (harmless-turn-run session "go")
      (should (equal asked '((network "demo"))))
      (should-not (cl-find "resources/list" seen
                           :key #'car :test #'equal))
      (should (equal (cl-loop for msg in (harmless-session-messages session)
                              when (eq (plist-get msg :role) :tool)
                              collect (plist-get msg :content))
                     '("Error: user denied this tool call")))
      (setq calls 0)
      (setq asked nil)
      (setq seen nil)
      (setq session
            (harmless-session-new
             :cwd repo :id "s2" :title "Keep" :model "m"
             :permission-mode 'accept-edits
             :provider
             (harmless-mcp-make-fake
              :name "fake" :host "none"
              :script
              (lambda (_messages callback)
                (setq calls (1+ calls))
                (if (= calls 1)
                    (progn
                      (funcall callback
                               '(:tool-call "1" "mcp_read_resource"
                                 "{\"server\":\"demo\",\"uri\":\"/\"}"))
                      (funcall callback '(:stop "tool_calls")))
                  (funcall callback '(:text "done"))
                  (funcall callback '(:stop "stop")))))))
      (setf (harmless-session-plan-mode session) t)
      (setq harmless-perm-ask-function
            (lambda (_session class item callback)
              (push (list class item) asked)
              (funcall callback 'allow)))
      (harmless-turn-run session "go")
      (should (equal asked '((network "/"))))
      (should (harmless-session-plan-mode session))
      (should (equal (cl-loop for msg in (harmless-session-messages session)
                              when (eq (plist-get msg :role) :tool)
                              collect (plist-get msg :content))
                     '("page")))
      (should-not (cl-find "Plan mode is on"
                           (harmless-session-messages session)
                           :test (lambda (text msg)
                                   (and (stringp (plist-get msg :content))
                                        (string-search text
                                                       (plist-get msg :content))))))
      (setq calls 0)
      (setq asked nil)
      (setq seen nil)
      (setq session
            (harmless-session-new
             :cwd repo :id "s3" :title "Keep" :model "m"
             :permission-mode 'always-approve
             :provider
             (harmless-mcp-make-fake
              :name "fake" :host "none"
              :script
              (lambda (_messages callback)
                (setq calls (1+ calls))
                (if (= calls 1)
                    (progn
                      (funcall callback
                               '(:tool-call "1" "mcp__demo__echo"
                                 "{\"text\":\"hi\"}"))
                      (funcall callback '(:stop "tool_calls")))
                  (funcall callback '(:text "done"))
                  (funcall callback '(:stop "stop")))))))
      (setq harmless-perm-ask-function
            (lambda (&rest _) (error "permission asked")))
      (harmless-turn-run session "go")
      (should-not asked)
      (should (equal (cl-subseq (mapcar #'car (reverse seen)) 0 3)
                     '("tools/list" "tools/list" "tools/call")))
      (should (equal (cl-loop for msg in (harmless-session-messages session)
                              when (eq (plist-get msg :role) :tool)
                              collect (plist-get msg :content))
                     '("pong")))
      (setq calls 0)
      (setq asked nil)
      (setq seen nil)
      (setq session
            (harmless-session-new
             :cwd repo :id "s4" :title "Keep" :model "m"
             :provider
             (harmless-mcp-make-fake
              :name "fake" :host "none"
              :script
              (lambda (_messages callback)
                (setq calls (1+ calls))
                (if (= calls 1)
                    (progn
                      (funcall callback
                               '(:tool-call "1" "mcp__demo__echo"
                                 "{\"text\":\"hi\"}"))
                      (funcall callback '(:stop "tool_calls")))
                  (funcall callback '(:text "done"))
                  (funcall callback '(:stop "stop")))))))
      (setq harmless-perm-ask-function
            (lambda (_session class item callback)
              (push (list class item) asked)
              (funcall callback 'deny)))
      (harmless-turn-run session "go")
      (should (equal asked '((network "mcp__demo__echo"))))
      (should (member "tools/list" (mapcar #'car seen)))
      (should-not (member "tools/call" (mapcar #'car seen)))
      (harmless-mcp-test-not-root session))))

(ert-deftest harmless-mcp-read-only-may-read-resources ()
  (harmless-mcp-test-with _data
    (let* ((repo (make-temp-file "harmless-mcp-repo-" t))
           (child (harmless-mcp-test-session
                   :cwd repo :id "child" :source 'agent :parent-id "p"
                   :title "Reader"))
           (harmless-mcp-call-function
            (lambda (server method _params)
              (cond
               ((equal (plist-get server :name) "bad")
                (error "down"))
               ((equal method "tools/list")
                '(:tools ((:name "echo" :description "Echo"
                           :inputSchema (:type "object")))))
               ((equal method "resources/list")
                '(:resources ((:uri "file:///a" :name "A"))))
               (t (error "unexpected %s" method))))))
      (harmless-mcp-test-write
       (harmless-mcp-project-file child)
       (concat "{\"mcpServers\":{"
               "\"bad\":{\"command\":\"bad-mcp\"},"
               "\"demo\":{\"command\":\"demo-mcp\"}}}"))
      (setf (harmless-session-agent child)
            (list :allow 'all :sandbox 'read-only :callback nil
                  :turns 0 :max 8 :error nil :prompt nil :task nil))
      (let ((names (mapcar #'harmless-tool-name
                           (harmless-tools-for-session child))))
        (should (member "mcp_list_resources" names))
        (should (member "mcp_read_resource" names))
        (should-not (member "mcp__demo__echo" names))
        (should-not (member "mcp__bad__echo" names))
        (should-not (member "write_file" names)))
      (should-not (harmless-agent-refuse-call
                   child (harmless-tool-by-name "mcp_list_resources")
                   '(:server "demo")))
      (should-not (harmless-agent-refuse-call
                   child (harmless-tool-by-name "mcp_read_resource")
                   '(:server "demo" :uri "/")))
      (let ((dynamic (harmless-mcp-tool-by-name child "mcp__demo__echo")))
        (should dynamic)
        (should (equal (harmless-agent-refuse-call
                        child dynamic '(:text "hi"))
                       harmless-agent-read-only-refusal)))
      (should (equal (harmless-tool-call
                      (harmless-tool-by-name "mcp_list_resources")
                      child '(:server "demo"))
                     "file:///a\nA"))
      (setf (plist-get (harmless-session-agent child) :allow)
            '("read_file"))
      (dolist (name '("mcp_list_resources" "mcp_read_resource"))
        (should (equal (harmless-agent-refuse-call
                        child (harmless-tool-by-name name)
                        '(:server "demo" :uri "/"))
                       (format "Error: this subagent cannot use %s" name))))
      (let ((detached (harmless-mcp-test-session
                       :detached t :id "chat")))
        (let ((default-directory (file-name-as-directory repo)))
          (let ((names (mapcar #'harmless-tool-name
                               (harmless-tools-for-session detached))))
            (should (member "mcp_list_resources" names))
            (should (member "mcp_read_resource" names))
            (should-not (member "mcp__demo__echo" names))
            (should-not (member "read_file" names))))
        (should (equal (harmless-tool-call
                        (harmless-tool-by-name "mcp_list_resources")
                        detached '(:server "demo"))
                       "Error: no MCP server demo"))
        (harmless-mcp-test-not-root detached))
      (harmless-mcp-test-not-root child))))
