;;; harmless-execute-tests.el --- Tests for execute_code -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(defun harmless-execute-test-error (text thunk)
  "THUNK must signal an error whose message is exactly TEXT."
  (let ((err (should-error (funcall thunk) :type 'error)))
    (should (equal text (error-message-string err)))))

(defun harmless-execute-test-no-process (text thunk)
  "THUNK must signal TEXT and must not start a process."
  (let ((started nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _)
                 (setq started t)
                 (error "make-process")))
              ((symbol-function 'make-network-process)
               (lambda (&rest _)
                 (setq started t)
                 (error "make-network-process"))))
      (harmless-execute-test-error text thunk)
      (should-not started))))

(defun harmless-execute-test-logical (file)
  "Return FILE as UTF-8 text with carriage returns removed.
`write_file' follows the platform newline convention."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents file))
    (replace-regexp-in-string "\r" "" (buffer-string))))

(defun harmless-execute-test-write (file text)
  "Write TEXT to FILE as UTF-8 with newline bytes unchanged."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil file nil 'silent)))

(defun harmless-execute-test-provider ()
  "Return a provider that does not open a connection."
  (harmless-make-openai-compat
   "local" :host "127.0.0.1:9" :protocol "http"
   :key "none" :models '("m")))

(defun harmless-execute-test-session (project)
  "Return a session whose project is PROJECT.
`harmless-directory' and `harmless--sessions' are the caller's."
  (harmless-session-new :cwd project
                        :provider (harmless-execute-test-provider)
                        :model "m"))

(ert-deftest harmless-execute-ui ()
  (let ((tool (harmless-tool-by-name "execute_code")))
    (should tool)
    (should (eq 'shell (harmless-tool-class tool)))
    (should (equal "run" (harmless-ui--tool-verb "execute_code")))
    (should (equal "(princ 1)"
                   (harmless-ui--tool-summary
                    "execute_code"
                    '(:code "(princ 1)\n(princ 2)"))))
    (should (equal "(princ 1)"
                   (harmless-ui--tool-summary
                    "execute_code"
                    '(:code "  (princ 1)  \n(princ 2)"))))
    (should (equal "" (harmless-ui--tool-summary "execute_code" nil)))
    (should (equal "" (harmless-ui--tool-summary "execute_code" '(:code nil))))
    (should (equal (concat (make-string 71 ?a) (char-to-string #x2026))
                   (harmless-ui--tool-summary
                    "execute_code"
                    (list :code (concat (make-string 81 ?a) "\nrest")))))
    (let* ((session (harmless-session--create
                     :id "execute-ui"
                     :source 'agent
                     :agent '(:sandbox read-only :allow all)))
           (names (mapcar #'harmless-tool-name
                          (harmless-agent-filter-tools
                           session (harmless-tools-enabled)))))
      (should-not (member "execute_code" names))
      (should (member "read_file" names)))))

(ert-deftest harmless-execute-refuses-a-bad-request ()
  (let* ((data (make-temp-file "harmless-ex-bad-" t))
         (project (make-temp-file "harmless-ex-bad-proj-" t))
         (harmless-directory data)
         (harmless--sessions (make-hash-table :test 'equal))
         (session nil))
    (unwind-protect
        (progn
          (setq session (harmless-execute-test-session project))
          (dolist (code (list nil "" "   " " \t\n "))
            (harmless-execute-test-no-process
             "code is empty"
             (lambda () (harmless-execute session code)))
            (harmless-execute-test-no-process
             "code is empty"
             (lambda () (harmless-execute session code 0 t))))
          (harmless-execute-test-no-process
           "code must be a string"
           (lambda () (harmless-execute session 42)))
          (harmless-execute-test-no-process
           "code must be a string"
           (lambda () (harmless-execute session 42 0)))
          (dolist (timeout (list 0 -1 0.0 "1" t))
            (harmless-execute-test-no-process
             "timeout must be a positive number"
             (lambda () (harmless-execute session "(princ 1)" timeout))))
          (harmless-execute-test-no-process
           "No Harmless session"
           (lambda () (harmless-execute nil "(princ 1)")))
          (harmless-execute-test-no-process
           "No Harmless session"
           (lambda () (harmless-execute nil nil 0)))
          (harmless-execute-test-no-process
           "execute code needs emacs"
           (lambda ()
             (cl-letf (((symbol-function 'harmless-execute--emacs)
                        (lambda () nil)))
               (harmless-execute session "(princ 1)"))))
          (harmless-execute-test-no-process
           "execute code needs emacs"
           (lambda ()
             (cl-letf (((symbol-function 'harmless-execute--child-file)
                        (lambda () nil)))
               (harmless-execute session "(princ 1)"))))
          (should (equal "Error: code is empty"
                         (harmless-tool-call
                          (harmless-tool-by-name "execute_code")
                          session '(:code "  "))))
          (should (equal "Error: code must be a string"
                         (harmless-tool-call
                          (harmless-tool-by-name "execute_code")
                          session '(:code 42))))
          (should (equal "Error: timeout must be a positive number"
                         (harmless-tool-call
                          (harmless-tool-by-name "execute_code")
                          session '(:code "(princ 1)" :timeout 0))))
          (should (equal "Error: No Harmless session"
                         (harmless-tool-call
                          (harmless-tool-by-name "execute_code")
                          nil '(:code "(princ 1)"))))
          (should-not (harmless-execute--state session)))
      (when session (harmless-execute-stop session))
      (delete-directory data t)
      (delete-directory project t))))

(ert-deftest harmless-execute-prints-and-calls-tools ()
  (let* ((data (make-temp-file "harmless-ex-" t))
         (project (make-temp-file "harmless-ex-proj-" t))
         (harmless-directory data)
         (harmless--sessions (make-hash-table :test 'equal))
         (session nil)
         (other nil))
    (unwind-protect
        (progn
          (harmless-execute-test-write
           (expand-file-name "a.txt" project) "A\n")
          (setq session (harmless-execute-test-session project))
          (should (equal "hi"
                         (harmless-tool-call
                          (harmless-tool-by-name "execute_code")
                          session '(:code "(princ \"hi\")"))))
          (should (equal "" (harmless-execute session "1")))
          (should (equal "1" (harmless-execute session "(princ 1)" 30)))
          (should (equal "hello\n" (harmless-execute session "(message \"hello\")")))
          (should (equal "" (harmless-execute session "(setq box \"kept\")")))
          (should (equal "kept" (harmless-execute session "(princ box)" nil nil)))
          (let ((proc (plist-get (harmless-execute--state session) :emacs))
                (text (harmless-execute session "(princ missing)")))
            (should (process-live-p proc))
            (should (string-prefix-p "execute code failed: " text))
            (should (string-search "missing" text))
            (should (process-live-p proc)))
          (should (equal "kept" (harmless-execute session "(princ box)")))
          (should (equal "" (harmless-execute session "(defun kept-fn () \"mine\")")))
          (should (equal "mine" (harmless-execute session "(princ (kept-fn))")))
          (should (equal "A\n"
                         (harmless-execute session
                           "(princ (read_file :path \"a.txt\"))")))
          (should (null (harmless-session-messages session)))
          (should (equal "A\n"
                         (harmless-execute session
                           "(princ (tool 'read_file :path \"a.txt\"))")))
          (should (equal "A\n"
                         (harmless-execute session
                           "(princ (tool \"read_file\" :path \"a.txt\"))")))
          (should (null (harmless-session-messages session)))
          (let ((written (expand-file-name "w.txt" project)))
            (should (string-prefix-p
                     "Wrote "
                     (harmless-execute session
                       "(princ (write_file :path \"w.txt\" :contents \"W\\n\"))")))
            (should (equal "W\n" (harmless-execute-test-logical written)))
            (should (null (harmless-session-messages session))))
          (should (equal ""
                         (harmless-execute session "(defun tool (&rest _) \"stolen\")")))
          (should (equal "A\n"
                         (harmless-execute session
                           "(princ (tool \"read_file\" :path \"a.txt\"))")))
          (setf (harmless-session-plan-mode session) t)
          (should (equal "Error: Plan mode is on. Only plan.md may be edited, with write_plan"
                         (harmless-execute session
                           "(princ (write_file :path \"z.txt\" :contents \"Z\\n\"))")))
          (should-not (file-exists-p (expand-file-name "z.txt" project)))
          (setf (harmless-session-plan-mode session) nil)
          (let ((harmless-execute-max-bytes 4))
            (should (equal "abcd"
                           (harmless-execute session "(princ \"abcd\")")))
            (should (equal "abcd\n[truncated]"
                           (harmless-execute session "(princ \"abcdef\")"))))
          (let ((harmless-execute-max-calls 1))
            (harmless-execute-test-error
             "execute code tool cap"
             (lambda ()
               (harmless-execute session
                 "(read_file :path \"a.txt\") (read_file :path \"a.txt\")")))
            (should (equal "Error: execute code tool cap"
                           (harmless-tool-call
                            (harmless-tool-by-name "execute_code")
                            session
                            '(:code "(read_file :path \"a.txt\") (read_file :path \"a.txt\")")))))
          (should (equal "kept" (harmless-execute session "(princ box)")))
          (harmless-execute-test-error
           "execute code cannot call execute_code"
           (lambda ()
             (harmless-execute session
               "(tool \"execute_code\" :code \"(princ 1)\")")))
          (harmless-execute-test-error
           "execute code cannot call spawn_agent"
           (lambda () (harmless-execute session "(tool \"spawn_agent\")")))
          (should (equal "Error: execute code cannot call spawn_agent"
                         (harmless-tool-call
                          (harmless-tool-by-name "execute_code")
                          session '(:code "(tool \"spawn_agent\")"))))
          (harmless-execute-test-error
           "execute code cannot call run_workflow"
           (lambda () (harmless-execute session "(tool \"run_workflow\")")))
          (harmless-execute-test-error
           "execute code cannot call an MCP tool"
           (lambda ()
             (harmless-execute session "(tool \"mcp_list_resources\")")))
          (harmless-execute-test-error
           "execute code cannot call an MCP tool"
           (lambda () (harmless-execute session "(tool \"mcp__demo\")")))
          (harmless-execute-test-error
           "execute code cannot call no_such_tool"
           (lambda () (harmless-execute session "(tool \"no_such_tool\")")))
          (let ((text (harmless-execute session
                        "(execute_code :code \"(princ 1)\")")))
            (should (string-prefix-p "execute code failed: " text))
            (should (string-search "execute_code" text))
            (should-not (string-search "cannot call" text)))
          (should (equal "kept" (harmless-execute session "(princ box)")))
          (setq other (harmless-execute-test-session project))
          (let ((text (harmless-execute other "(princ box)")))
            (should (string-prefix-p "execute code failed: " text))
            (should (string-search "box" text)))
          (should (equal "kept" (harmless-execute session "(princ box)")))
          (should (equal ""
                         (harmless-execute session
                           "(defun read_file (&rest _) \"mine-read\")")))
          (should (equal "mine-read"
                         (harmless-execute session
                           "(princ (read_file :path \"a.txt\"))")))
          (let* ((proc (plist-get (harmless-execute--state session) :emacs))
                 (text (harmless-execute session "(princ box)" nil t)))
            (should-not (process-live-p proc))
            (should (string-prefix-p "execute code failed: " text))
            (should (string-search "box" text)))
          (should (equal "A\n"
                         (harmless-execute session
                           "(princ (read_file :path \"a.txt\"))")))
          (should (equal "after" (harmless-execute session "(princ \"after\")")))
          (let ((proc (plist-get (harmless-execute--state session) :emacs)))
            (should (process-live-p proc))
            (harmless-execute-stop session)
            (should-not (process-live-p proc))
            (should-not (harmless-execute--state session)))
          (should (null (harmless-session-messages session))))
      (when session (harmless-execute-stop session))
      (when other (harmless-execute-stop other))
      (delete-directory data t)
      (delete-directory project t))))

(ert-deftest harmless-execute-strips-the-environment ()
  (let* ((data (make-temp-file "harmless-ex-env-" t))
         (project (make-temp-file "harmless-ex-env-proj-" t))
         (harmless-directory data)
         (harmless--sessions (make-hash-table :test 'equal))
         (session nil)
         (process-environment
          (append '("HARMLESS_SECRET=hidden"
                    "OTHER_TOKEN=hidden"
                    "HARMLESS_AUTHOR=hidden"
                    "HARMLESS_PASSWORD=hidden"
                    "HARMLESS_CREDENTIAL=hidden"
                    "HARMLESS_PASSWD=hidden"
                    "HARMLESS_KEY=hidden"
                    "HARMLESS_PLAIN=shown")
                  process-environment)))
    (unwind-protect
        (progn
          (setq session (harmless-execute-test-session project))
          (should (equal
                   "nosecret notoken noauth nopass nocred nopasswd nokey shown path"
                   (harmless-execute
                    session
                    (concat "(princ (mapconcat #'identity (list "
                            "(or (getenv \"HARMLESS_SECRET\") \"nosecret\") "
                            "(or (getenv \"OTHER_TOKEN\") \"notoken\") "
                            "(or (getenv \"HARMLESS_AUTHOR\") \"noauth\") "
                            "(or (getenv \"HARMLESS_PASSWORD\") \"nopass\") "
                            "(or (getenv \"HARMLESS_CREDENTIAL\") \"nocred\") "
                            "(or (getenv \"HARMLESS_PASSWD\") \"nopasswd\") "
                            "(or (getenv \"HARMLESS_KEY\") \"nokey\") "
                            "(or (getenv \"HARMLESS_PLAIN\") \"noplain\") "
                            "(if (and (getenv \"PATH\") "
                            "(not (equal (getenv \"PATH\") \"\"))) "
                            "\"path\" \"nopath\")) \" \"))")))))
      (when session (harmless-execute-stop session))
      (delete-directory data t)
      (delete-directory project t))))

(ert-deftest harmless-execute-times-out ()
  (let* ((data (make-temp-file "harmless-ex-time-" t))
         (project (make-temp-file "harmless-ex-time-proj-" t))
         (harmless-directory data)
         (harmless--sessions (make-hash-table :test 'equal))
         (session nil))
    (unwind-protect
        (progn
          (setq session (harmless-execute-test-session project))
          (should (equal "" (harmless-execute session "(setq box \"kept\")")))
          (let ((proc (plist-get (harmless-execute--state session) :emacs)))
            (harmless-execute-test-error
             "Script timed out after 1s and was killed."
             (lambda () (harmless-execute session "(sleep-for 30)" 1)))
            (should-not (process-live-p proc))
            (should-not (harmless-execute--state session)))
          (let ((text (harmless-execute session "(princ box)")))
            (should (string-prefix-p "execute code failed: " text))
            (should (string-search "box" text)))
          (should (equal "after" (harmless-execute session "(princ \"after\")"))))
      (when session (harmless-execute-stop session))
      (delete-directory data t)
      (delete-directory project t))))

(ert-deftest harmless-execute-runs-a-detached-chat ()
  (let* ((data (make-temp-file "harmless-ex-chat-" t))
         (harmless-directory data)
         (harmless--sessions (make-hash-table :test 'equal))
         (session nil))
    (unwind-protect
        (progn
          (setq session (harmless-session-new
                         :detached t
                         :provider (harmless-execute-test-provider)
                         :model "m"))
          (should-not (member "execute_code"
                              (mapcar #'harmless-tool-name
                                      (harmless-tools-for-session session))))
          (should (equal "hi" (harmless-execute session "(princ \"hi\")")))
          (should (null (harmless-session-messages session))))
      (when session (harmless-execute-stop session))
      (delete-directory data t))))
