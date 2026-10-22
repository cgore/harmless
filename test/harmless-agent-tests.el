;;; harmless-agent-tests.el --- Tests for Harmless subagents -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless-provider)
(require 'harmless-session)
(require 'harmless-tools)
(require 'harmless-tools-fs)
(require 'harmless-tools-shell)
(require 'harmless-perm)
(require 'harmless-turn)
(require 'harmless-agent)
(require 'harmless-ask)
(require 'harmless-ui)

(cl-defstruct (harmless-agent-fake
               (:include harmless-provider)
               (:constructor harmless-agent-make-fake))
  script)

(defvar harmless-agent-test-tools nil
  "Tools from the latest fake completion, for assertions.")

(cl-defmethod harmless-provider-complete ((provider harmless-agent-fake)
                                          messages tools callback)
  (setq harmless-agent-test-tools tools)
  (funcall (harmless-agent-fake-script provider) messages callback)
  nil)

(defun harmless-agent-test-result (session args)
  "Call harmless-tool-spawn-agent on SESSION with ARGS and return the tool text."
  (let (result)
    (harmless-agent--tool session args (lambda (text) (setq result text)))
    result))

(defun harmless-agent-test-error (session args pattern)
  "Require harmless-tool-spawn-agent on SESSION with ARGS to return PATTERN."
  (let ((result (harmless-agent-test-result session args)))
    (should (string-match-p pattern result))
    result))

(defun harmless-agent-test--contents (messages)
  "Return the joined text of MESSAGES."
  (mapconcat (lambda (msg) (or (plist-get msg :content) ""))
             messages
             "\n"))

(defun harmless-agent-test--tools (session)
  "Return tool-result strings recorded on SESSION."
  (cl-loop for msg in (harmless-session-messages session)
           when (memq (plist-get msg :role) '(:tool tool))
           collect (plist-get msg :content)))

(defun harmless-agent-test--write (path text)
  "Write TEXT to PATH, creating parent directories."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert text)))

(defun harmless-agent-test--not-root (session)
  "Assert SESSION's directory is under `harmless-directory' and not /."
  (let ((dir (harmless-session-directory session)))
    (should (file-name-absolute-p dir))
    (should-not (harmless-filesystem-root-p dir))
    (should (string-prefix-p (file-name-as-directory harmless-directory) dir))
    (should-not (equal (expand-file-name "summary.json" dir) "/summary.json"))
    (should-not (file-exists-p "/summary.json"))))

(defmacro harmless-agent-test-with (dir &rest body)
  "Bind a temp project DIR and run BODY."
  (declare (indent 1))
  `(let* ((,dir (make-temp-file "harmless-agent-" t))
          (harmless-directory (expand-file-name ".harmless" ,dir))
          (harmless--sessions (make-hash-table :test 'equal))
          (harmless-agent-stop ,dir)
          (harmless-perm-ask-function
           (lambda (_session _class _item callback)
             (funcall callback 'allow))))
     ,@body))

(defun harmless-agent-test--session (dir script &rest args)
  "Return a session in DIR whose provider runs SCRIPT."
  (let ((provider (harmless-agent-make-fake
                   :name "fake" :host "none" :script script)))
    (apply #'harmless-session-new
           :cwd (unless (plist-get args :detached) dir)
           :provider provider
           :model "parent-model"
           args)))

(ert-deftest harmless-agent-refuses-an-empty-prompt ()
  (harmless-agent-test-with dir
    (let ((session (harmless-agent-test--session
                    dir (lambda (&rest _) (error "provider ran")))))
      (dolist (prompt (list nil "" "   " "\n" "\t" " \n\t "))
        (harmless-agent-test-error
         session (list :prompt prompt) "\\`Error: prompt is empty\\'"))
      (should-not (harmless-agent-children session))
      (harmless-agent-test--not-root session))))

(ert-deftest harmless-agent-refuses-a-bad-name ()
  (harmless-agent-test-with dir
    (let ((session (harmless-agent-test--session
                    dir (lambda (&rest _) (error "provider ran")))))
      (dolist (name (list "" "  " "\n"))
        (harmless-agent-test-error
         session (list :prompt "look" :agent name)
         "\\`Error: agent name is empty\\'"))
      (dolist (name (list "sub/" "/" "c:/" "../x" "a\\b" "a/b" "." ".."))
        (harmless-agent-test-error
         session (list :prompt "look" :agent name)
         "\\`Error: agent name is not a name\\'"))
      (dolist (name (list "sub" "missing"))
        (harmless-agent-test-error
         session (list :prompt "look" :agent name)
         (format "\\`Error: no agent named %s\\'" name)))
      (should-not (harmless-agent-children session))
      (should-not (file-exists-p "/.claude")))))

(ert-deftest harmless-agent-discover-rejects-non-directories ()
  (should-not (harmless-agent-discover nil))
  (should-not (harmless-agent-discover ""))
  (should-not (harmless-agent-discover "sub"))
  (should-not (harmless-agent-discover "sub/"))
  (should-not (harmless-agent-discover "/"))
  (should-not (file-exists-p "/.claude"))
  (should-not (file-exists-p "/.harmless")))

(ert-deftest harmless-agent-definition-closer-file-wins ()
  (let* ((root (make-temp-file "harmless-agent-root-" t))
         (proj (expand-file-name "proj" root))
         (outer (expand-file-name ".claude/agents/reviewer.md" root))
         (inner (expand-file-name ".harmless/agents/reviewer.md" proj))
         (harmless-agent-stop root))
    (make-directory proj t)
    (harmless-agent-test--write
     outer
     "---\r\nname: reviewer\r\ndescription: Outer\r\n---\r\nOUTER\r\n")
    (harmless-agent-test--write
     inner
     "---\nname: reviewer\ndescription: Inner\n---\nINNER\n")
    (let ((spec (harmless-agent-find proj "reviewer")))
      (should (equal (plist-get spec :prompt) "INNER"))
      (should (equal (plist-get spec :path) inner))
      (should (equal (plist-get spec :description) "Inner")))
    (let ((spec (harmless-agent-find (file-name-as-directory proj) "reviewer")))
      (should (equal (plist-get spec :path) inner)))
    (should (equal (plist-get (harmless-agent-find root "reviewer") :prompt)
                   "OUTER"))))

(ert-deftest harmless-agent-definition-refuses-a-bad-file ()
  (harmless-agent-test-with dir
    (harmless-agent-test--write
     (expand-file-name ".claude/agents/jailed.md" dir)
     "---\nname: jailed\nsandbox: jail\n---\nNo.\n")
    (harmless-agent-test--write
     (expand-file-name ".claude/agents/maker.md" dir)
     "---\nname: maker\ntools: make\n---\nNo.\n")
    (harmless-agent-test--write
     (expand-file-name ".claude/agents/nester.md" dir)
     "---\nname: nester\ntools: harmless-tool-spawn-agent\n---\nNo.\n")
    (let ((session (harmless-agent-test--session
                    dir (lambda (&rest _) (error "provider ran")))))
      (harmless-agent-test-error
       session (list :prompt "look" :agent "jailed")
       "\\`Error: sandbox must be workspace or read-only\\'")
      (harmless-agent-test-error
       session (list :prompt "look" :agent "maker")
       "\\`Error: unknown tool make\\'")
      (harmless-agent-test-error
       session (list :prompt "look" :agent "nester")
       "\\`Error: a subagent cannot start another subagent\\'")
      (should-not (harmless-agent-children session)))))

(ert-deftest harmless-agent-max-turns-must-be-a-positive-integer ()
  (harmless-agent-test-with dir
    (let ((session (harmless-agent-test--session
                    dir (lambda (&rest _) (error "provider ran")))))
      (dolist (bad (list nil 0 -1 1.5 "8" t))
        (let ((harmless-agent-max-turns bad))
          (harmless-agent-test-error
           session (list :prompt "look")
           "\\`Error: harmless-agent-max-turns must be a positive integer\\'")))
      (should-not (harmless-agent-children session)))))

(ert-deftest harmless-agent-reports-its-own-summary ()
  (harmless-agent-test-with dir
    (let* ((calls 0)
           (child-tools nil)
           (session
            (harmless-agent-test--session
             dir
             (lambda (messages callback)
               (setq calls (1+ calls))
               (cond
                ((= calls 1)
                 (should (string-match-p
                          "PARENT-SECRET"
                          (harmless-agent-test--contents messages)))
                 (should (string-match-p
                          "reviewer"
                          (harmless-agent-test--contents messages)))
                 (funcall callback
                          '(:tool-call "1" "harmless-tool-spawn-agent"
                            "{\"prompt\":\"TASK-TEXT\",\"agent\":\"reviewer\",\"description\":\"Find the bug\"}"))
                 (funcall callback '(:stop "tool_calls")))
                ((= calls 2)
                 (setq child-tools
                       (mapcar #'harmless-tool-name
                               harmless-agent-test-tools))
                 (should-not (string-match-p
                              "PARENT-SECRET"
                              (harmless-agent-test--contents messages)))
                 (should (string-match-p
                          "AGENT-PROMPT"
                          (harmless-agent-test--contents messages)))
                 (should (string-match-p
                          "TASK-TEXT"
                          (harmless-agent-test--contents messages)))
                 (funcall callback '(:text "found it"))
                 (funcall callback '(:stop "stop")))
                (t
                 (should (string-match-p
                          "found it"
                          (harmless-agent-test--contents messages)))
                 (funcall callback '(:text "ok"))
                 (funcall callback '(:stop "stop")))))))
           (asked nil))
      (harmless-agent-test--write
       (expand-file-name ".claude/agents/reviewer.md" dir)
       "---\nname: reviewer\ndescription: Reviews code\ntools: harmless-tool-read-file, harmless-tool-grep\nmodel: child-model-xhigh\nsandbox: read-only\n---\nAGENT-PROMPT\n")
      (setq harmless-perm-ask-function
            (lambda (_s class item cb)
              (push (list class item) asked)
              (funcall cb 'allow)))
      (harmless-session-append-user session "PARENT-SECRET")
      (harmless-turn-run session "go")
      (should (= calls 3))
      (should-not asked)
      (should (equal (harmless-agent-test--tools session) '("found it")))
      (let ((child (car (harmless-agent-children session))))
        (should child)
        (should (eq (harmless-session-source child) 'agent))
        (should (equal (harmless-session-parent-id child)
                       (harmless-session-id session)))
        (should (equal (harmless-session-model child) "child-model"))
        (should (equal (harmless-session-reasoning-effort child) "xhigh"))
        (should (equal (harmless-session-title child) "Find the bug"))
        (should (eq (harmless-session-plan-mode child) nil))
        (harmless-agent-test--not-root child)
        (should-not (member "harmless-tool-spawn-agent" child-tools))
        (should (member "harmless-tool-read-file" child-tools))
        (should-not (member "harmless-tool-write-file" child-tools))
        (let* ((file (expand-file-name
                      "summary.json" (harmless-session-directory child)))
               (plist (harmless-json-decode
                       (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string)))))
          (should (equal (plist-get plist :source) "agent"))
          (should (equal (plist-get plist :parent-id)
                         (harmless-session-id session)))
          (should (equal (plist-get plist :agent-sandbox) "read-only"))
          (should (equal (plist-get plist :agent-prompt) "AGENT-PROMPT"))
          (should (equal (plist-get plist :agent-allow) '("harmless-tool-read-file" "harmless-tool-grep")))
          (should-not (plist-member plist :callback))
          (should-not (plist-member plist :task)))
        (let ((dir (harmless-session-directory child)))
          (harmless-session-unregister child)
          (let ((loaded (harmless-session-load dir)))
            (should (eq (harmless-session-source loaded) 'agent))
            (should (equal (harmless-session-parent-id loaded)
                           (harmless-session-id session)))
            (should (equal (plist-get (harmless-session-agent loaded) :prompt)
                           "AGENT-PROMPT"))
            (should (eq (plist-get (harmless-session-agent loaded) :sandbox)
                        'read-only))
            (should (equal (plist-get (harmless-session-agent loaded) :allow)
                           '("harmless-tool-read-file" "harmless-tool-grep")))
            (should-not (plist-get (harmless-session-agent loaded) :callback))
            (let ((names (mapcar #'harmless-tool-name
                                 (harmless-tools-for-session loaded))))
              (should (member "harmless-tool-read-file" names))
              (should-not (member "harmless-tool-write-file" names))
              (should-not (member "harmless-tool-spawn-agent" names)))))))))

(ert-deftest harmless-agent-workspace-write-and-title-edges ()
  (harmless-agent-test-with dir
    (let* ((calls 0)
           (asked nil)
           (session
            (harmless-agent-test--session
             dir
             (lambda (_messages callback)
               (setq calls (1+ calls))
               (cond
                ((= calls 1)
                 (funcall callback
                          '(:tool-call "1" "harmless-tool-spawn-agent"
                            "{\"prompt\":\"write the note\",\"description\":\"/\"}"))
                 (funcall callback '(:stop "tool_calls")))
                ((= calls 2)
                 (funcall callback
                          '(:tool-call "w" "harmless-tool-write-file"
                            "{\"path\":\"note.txt\",\"contents\":\"hi\"}"))
                 (funcall callback '(:stop "tool_calls")))
                ((= calls 3)
                 (funcall callback '(:text "wrote"))
                 (funcall callback '(:stop "stop")))
                (t
                 (funcall callback '(:text "done"))
                 (funcall callback '(:stop "stop"))))))))
      (setq harmless-perm-ask-function
            (lambda (_s class item cb)
              (push (list class item) asked)
              (funcall cb 'allow)))
      (harmless-turn-run session "go")
      (should (equal (harmless-agent-test--tools session) '("wrote")))
      (should (member '(edit "harmless-tool-spawn-agent") asked))
      (let ((path (expand-file-name "note.txt" dir))
            (child (car (harmless-agent-children session))))
        (should (file-exists-p path))
        (should (equal (with-temp-buffer
                         (insert-file-contents path)
                         (buffer-string))
                       "hi"))
        (should (equal (harmless-session-title child) "/"))
        (harmless-agent-test--not-root child)))))

(ert-deftest harmless-agent-read-only-refuses-a-write ()
  (harmless-agent-test-with dir
    (let* ((calls 0)
           (session
            (harmless-agent-test--session
             dir
             (lambda (_messages callback)
               (setq calls (1+ calls))
               (cond
                ((= calls 1)
                 (funcall callback
                          '(:tool-call "1" "harmless-tool-spawn-agent"
                            "{\"prompt\":\"edit\",\"agent\":\"reader\"}"))
                 (funcall callback '(:stop "tool_calls")))
                ((= calls 2)
                 (funcall callback
                          '(:tool-call "w" "harmless-tool-write-file"
                            "{\"path\":\"nope.txt\",\"contents\":\"x\"}"))
                 (funcall callback '(:stop "tool_calls")))
                ((= calls 3)
                 (funcall callback '(:text "blocked"))
                 (funcall callback '(:stop "stop")))
                (t
                 (funcall callback '(:text "ok"))
                 (funcall callback '(:stop "stop"))))))))
      (harmless-agent-test--write
       (expand-file-name ".harmless/agents/reader.md" dir)
       "---\nname: reader\nsandbox: read-only\n---\nRead.\n")
      (harmless-turn-run session "go")
      (let ((child (car (harmless-agent-children session))))
        (should (equal (harmless-agent-test--tools child)
                       '("Error: This subagent is read-only. A tool that can write is refused")))
        (should-not (file-exists-p (expand-file-name "nope.txt" dir)))
        (should (equal (harmless-agent-test--tools session) '("blocked")))))))

(ert-deftest harmless-agent-read-only-shell-classification ()
  (harmless-agent-test-with dir
    (let ((child (harmless-session-new
                  :cwd dir
                  :provider (harmless-agent-make-fake
                             :name "fake" :host "none"
                             :script (lambda (&rest _) nil))
                  :model "m"
                  :source 'agent
                  :parent-id "parent")))
      (setf (harmless-session-agent child)
            (list :allow 'all :sandbox 'read-only :callback nil
                  :turns 0 :max 8 :error nil :prompt nil :task nil))
      (should-not (harmless-agent-refuse-call
                   child (harmless-tool-by-name "harmless-tool-run-shell")
                   '(:command "echo hi")))
      (should (equal (harmless-agent-refuse-call
                      child (harmless-tool-by-name "harmless-tool-run-shell")
                      '(:command "echo hi > x"))
                     "Error: This subagent is read-only. A tool that can write is refused"))
      (should-not (harmless-agent-refuse-call
                   child (harmless-tool-by-name "harmless-tool-run-shell")
                   '(:command "   ")))
      (should-not (harmless-agent-refuse-call
                   child (harmless-tool-by-name "harmless-tool-run-shell")
                   nil))
      (setf (plist-get (harmless-session-agent child) :allow) '("harmless-tool-read-file"))
      (should (equal (harmless-agent-refuse-call
                      child (harmless-tool-by-name "harmless-tool-grep")
                      '(:pattern "x"))
                     "Error: this subagent cannot use harmless-tool-grep"))
      (should (equal (harmless-agent-refuse-call
                      child (harmless-tool-by-name "harmless-tool-spawn-agent")
                      '(:prompt "more"))
                     "Error: a subagent cannot start another subagent")))))

(ert-deftest harmless-agent-cannot-nest ()
  (harmless-agent-test-with dir
    (let* ((calls 0)
           (session
            (harmless-agent-test--session
             dir
             (lambda (_messages callback)
               (setq calls (1+ calls))
               (cond
                ((= calls 1)
                 (funcall callback
                          '(:tool-call "1" "harmless-tool-spawn-agent"
                            "{\"prompt\":\"nest\"}"))
                 (funcall callback '(:stop "tool_calls")))
                ((= calls 2)
                 (funcall callback
                          '(:tool-call "2" "harmless-tool-spawn-agent"
                            "{\"prompt\":\"again\"}"))
                 (funcall callback '(:stop "tool_calls")))
                ((= calls 3)
                 (funcall callback '(:text "nope"))
                 (funcall callback '(:stop "stop")))
                (t
                 (funcall callback '(:text "ok"))
                 (funcall callback '(:stop "stop"))))))))
      (harmless-turn-run session "go")
      (let ((child (car (harmless-agent-children session))))
        (should (= (length (harmless-agent-children session)) 1))
        (should (equal (harmless-agent-test--tools child)
                       '("Error: a subagent cannot start another subagent")))
        (should-not (harmless-agent-children child))))))

(ert-deftest harmless-agent-stops-after-max-turns ()
  (harmless-agent-test-with dir
    (let* ((harmless-agent-max-turns 1)
           (calls 0)
           (session
            (harmless-agent-test--session
             dir
             (lambda (messages callback)
               (setq calls (1+ calls))
               (cond
                ((= calls 1)
                 (funcall callback
                          '(:tool-call "1" "harmless-tool-spawn-agent"
                            "{\"prompt\":\"count\"}"))
                 (funcall callback '(:stop "tool_calls")))
                ((= calls 2)
                 (funcall callback
                          '(:tool-call "r" "harmless-tool-read-file"
                            "{\"path\":\"missing.txt\"}"))
                 (funcall callback '(:stop "tool_calls")))
                (t
                 (should (string-match-p
                          "subagent stopped after 1 turns"
                          (harmless-agent-test--contents messages)))
                 (funcall callback '(:text "stopped"))
                 (funcall callback '(:stop "stop"))))))))
      (harmless-turn-run session "go")
      (should (= calls 3))
      (should (equal (harmless-agent-test--tools session)
                     '("Error: subagent stopped after 1 turns"))))))

(ert-deftest harmless-agent-parent-abort-cancels-the-child ()
  (harmless-agent-test-with dir
    (let* ((calls 0)
           (session
            (harmless-agent-test--session
             dir
             (lambda (_messages callback)
               (setq calls (1+ calls))
               (when (= calls 1)
                 (funcall callback
                          '(:tool-call "1" "harmless-tool-spawn-agent"
                            "{\"prompt\":\"hold\"}"))
                 (funcall callback '(:stop "tool_calls")))))))
      (harmless-turn-run session "go")
      (let ((child (car (harmless-agent-children session))))
        (should (eq (harmless-session-status child) 'streaming))
        (should (eq (harmless-session-status session) 'streaming))
        (harmless-turn-abort session)
        (should (eq (harmless-session-status child) 'idle))
        (should (eq (harmless-session-status session) 'idle))
        (should-not (harmless-agent-test--tools session))
        (should (= calls 2))))))

(ert-deftest harmless-agent-child-abort-returns-to-the-parent ()
  (harmless-agent-test-with dir
    (let* ((calls 0)
           (session
            (harmless-agent-test--session
             dir
             (lambda (messages callback)
               (setq calls (1+ calls))
               (cond
                ((= calls 1)
                 (funcall callback
                          '(:tool-call "1" "harmless-tool-spawn-agent"
                            "{\"prompt\":\"hold\"}"))
                 (funcall callback '(:stop "tool_calls")))
                ((= calls 2) nil)
                (t
                 (should (string-match-p
                          "subagent aborted"
                          (harmless-agent-test--contents messages)))
                 (funcall callback '(:text "saw abort"))
                 (funcall callback '(:stop "stop"))))))))
      (harmless-turn-run session "go")
      (harmless-turn-abort (car (harmless-agent-children session)))
      (should (eq (harmless-session-status session) 'idle))
      (should (equal (harmless-agent-test--tools session)
                     '("Error: subagent aborted"))))))

(ert-deftest harmless-agent-empty-and-failed-replies ()
  (harmless-agent-test-with dir
    (let* ((calls 0)
           (mode 'empty)
           (session
            (harmless-agent-test--session
             dir
             (lambda (_messages callback)
               (setq calls (1+ calls))
               (cond
                ((= calls 1)
                 (funcall callback
                          '(:tool-call "1" "harmless-tool-spawn-agent"
                            "{\"prompt\":\"reply\"}"))
                 (funcall callback '(:stop "tool_calls")))
                ((= calls 2)
                 (if (eq mode 'empty)
                     (funcall callback '(:stop "stop"))
                   (funcall callback '(:error "connection reset"))))
                (t
                 (funcall callback '(:text "parent"))
                 (funcall callback '(:stop "stop"))))))))
      (harmless-turn-run session "go")
      (should (equal (harmless-agent-test--tools session)
                     '("Error: The model returned an empty reply.")))
      (setq mode 'fail
            calls 0)
      (harmless-turn-run session "again")
      (should (equal (car (last (harmless-agent-test--tools session)))
                     "Error: connection reset")))))

(ert-deftest harmless-agent-plan-mode-refuses-a-writing-child ()
  (harmless-agent-test-with dir
    (let* ((calls 0)
           (asked nil)
           (session
            (harmless-agent-test--session
             dir
             (lambda (_messages callback)
               (setq calls (1+ calls))
               (if (= calls 1)
                   (progn
                     (funcall callback
                              '(:tool-call "1" "harmless-tool-spawn-agent"
                                "{\"prompt\":\"edit\"}"))
                     (funcall callback '(:stop "tool_calls")))
                 (funcall callback '(:text "after"))
                 (funcall callback '(:stop "stop")))))))
      (setf (harmless-session-plan-mode session) t)
      (setf (harmless-session-permission-mode session) 'always-approve)
      (setq harmless-perm-ask-function
            (lambda (&rest _) (setq asked t)))
      (harmless-turn-run session "go")
      (should-not asked)
      (should-not (harmless-agent-children session))
      (should (equal (harmless-agent-test--tools session)
                     '("Error: Plan mode is on. A subagent that can write is refused"))))))

(ert-deftest harmless-agent-plan-mode-allows-a-read-only-child ()
  (harmless-agent-test-with dir
    (let* ((calls 0)
           (session
            (harmless-agent-test--session
             dir
             (lambda (messages callback)
               (setq calls (1+ calls))
               (cond
                ((= calls 1)
                 (funcall callback
                          '(:tool-call "1" "harmless-tool-spawn-agent"
                            "{\"prompt\":\"TASK-RO\",\"agent\":\"reader\"}"))
                 (funcall callback '(:stop "tool_calls")))
                ((= calls 2)
                 (should (string-match-p
                          "TASK-RO"
                          (harmless-agent-test--contents messages)))
                 (funcall callback '(:text "read only"))
                 (funcall callback '(:stop "stop")))
                (t
                 (funcall callback '(:text "ok"))
                 (funcall callback '(:stop "stop"))))))))
      (harmless-agent-test--write
       (expand-file-name ".claude/agents/reader.md" dir)
       "---\nname: reader\nsandbox: read-only\n---\nRead.\n")
      (setf (harmless-session-plan-mode session) t)
      (setq harmless-perm-ask-function
            (lambda (&rest _) (error "should not ask")))
      (harmless-turn-run session "go")
      (should (equal (harmless-agent-test--tools session) '("read only"))))))

(ert-deftest harmless-agent-deny-creates-no-child ()
  (harmless-agent-test-with dir
    (let* ((calls 0)
           (session
            (harmless-agent-test--session
             dir
             (lambda (_messages callback)
               (setq calls (1+ calls))
               (if (= calls 1)
                   (progn
                     (funcall callback
                              '(:tool-call "1" "harmless-tool-spawn-agent"
                                "{\"prompt\":\"nope\"}"))
                     (funcall callback '(:stop "tool_calls")))
                 (funcall callback '(:text "denied"))
                 (funcall callback '(:stop "stop")))))))
      (setq harmless-perm-ask-function
            (lambda (_s class item cb)
              (should (eq class 'edit))
              (should (equal item "harmless-tool-spawn-agent"))
              (funcall cb 'deny)))
      (harmless-turn-run session "go")
      (should-not (harmless-agent-children session))
      (should (equal (harmless-agent-test--tools session)
                     '("Error: user denied this tool call"))))))

(ert-deftest harmless-agent-detached-has-no-named-definition ()
  (harmless-agent-test-with dir
    (let* ((calls 0)
           (session
            (harmless-session-new
             :detached t
             :provider (harmless-agent-make-fake
                        :name "fake" :host "none"
                        :script
                        (lambda (messages callback)
                          (setq calls (1+ calls))
                          (cond
                           ((= calls 1)
                            (funcall callback
                                     '(:tool-call "1" "harmless-tool-spawn-agent"
                                       "{\"prompt\":\"think\"}"))
                            (funcall callback '(:stop "tool_calls")))
                           ((= calls 2)
                            (should (string-match-p
                                     "detached chat"
                                     (harmless-agent-test--contents messages)))
                            (funcall callback '(:text "thought"))
                            (funcall callback '(:stop "stop")))
                           (t
                            (funcall callback '(:text "ok"))
                            (funcall callback '(:stop "stop"))))))
             :model "parent-model")))
      (harmless-agent-test--write
       (expand-file-name ".claude/agents/reviewer.md" dir)
       "---\nname: reviewer\n---\nNope.\n")
      (should (equal (harmless-agent-test-result
                      session (list :prompt "look" :agent "reviewer"))
                     "Error: no agent named reviewer"))
      (harmless-turn-run session "go")
      (let ((child (car (harmless-agent-children session))))
        (should (harmless-session-detached-p child))
        (harmless-agent-test--not-root child)
        (let ((names (mapcar #'harmless-tool-name
                             (harmless-tools-for-session child))))
          (should (member "harmless-tool-ask-user" names))
          (should-not (member "harmless-tool-read-file" names))
          (should-not (member "harmless-tool-spawn-agent" names))))
      (should (equal (harmless-agent-test--tools session) '("thought"))))))

(ert-deftest harmless-agent-description-edges-and-ui ()
  (harmless-agent-test-with dir
    (let ((session (harmless-agent-test--session
                    dir (lambda (&rest _) (error "provider ran")))))
      (harmless-agent-test-error
       session (list :prompt "look" :description "two\nlines")
       "\\`Error: description must be one line\\'")
      (let ((child (harmless-agent-create
                    session '(:prompt "look" :description "sub/")
                    (lambda (_) nil))))
        (should (equal (harmless-session-title child) "sub/"))
        (harmless-agent-test--not-root child))))
  (should (equal (harmless-ui--tool-verb "harmless-tool-spawn-agent") "agent"))
  (should (equal (harmless-ui--tool-summary
                  "harmless-tool-spawn-agent" "{\"prompt\":\"Find it\"}")
                 "Find it"))
  (let ((tool (harmless-tool-by-name "harmless-tool-spawn-agent")))
    (should (harmless-tool-async tool))
    (should (eq (harmless-tool-class tool) 'edit))))

(provide 'harmless-agent-tests)
