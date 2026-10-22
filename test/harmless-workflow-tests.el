;;; harmless-workflow-tests.el --- Tests for Harmless workflows -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless-provider)
(require 'harmless-session)
(require 'harmless-tools)
(require 'harmless-perm)
(require 'harmless-turn)
(require 'harmless-agent)
(require 'harmless-workflow)
(require 'harmless-ask)
(require 'harmless-ui)

(cl-defstruct (harmless-workflow-fake
               (:include harmless-provider)
               (:constructor harmless-workflow-make-fake))
  script)

(defvar harmless-workflow-test-tools nil
  "Tools from the latest fake completion, for assertions.")

(cl-defmethod harmless-provider-complete ((provider harmless-workflow-fake)
                                          messages tools callback)
  (setq harmless-workflow-test-tools tools)
  (funcall (harmless-workflow-fake-script provider) messages callback)
  nil)

(defun harmless-workflow-test-result (session args)
  "Call harmless-tool-run-workflow on SESSION with ARGS and return the tool text."
  (let (result)
    (harmless-workflow--tool session args (lambda (text) (setq result text)))
    result))

(defun harmless-workflow-test-error (session args pattern)
  "Require harmless-tool-run-workflow on SESSION with ARGS to return PATTERN."
  (let ((result (harmless-workflow-test-result session args)))
    (should (string-match-p pattern result))
    result))

(defun harmless-workflow-test--contents (messages)
  "Return the joined text of MESSAGES."
  (mapconcat (lambda (msg) (or (plist-get msg :content) ""))
             messages
             "\n"))

(defun harmless-workflow-test--tools (session)
  "Return tool-result strings recorded on SESSION."
  (cl-loop for msg in (harmless-session-messages session)
           when (memq (plist-get msg :role) '(:tool tool))
           collect (plist-get msg :content)))

(defun harmless-workflow-test--write (path text)
  "Write TEXT to PATH, creating parent directories."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert text)))

(defun harmless-workflow-test--text (path)
  "Return the contents of PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun harmless-workflow-test--not-root (session)
  "Assert SESSION's directory is under `harmless-directory' and not /."
  (let ((dir (harmless-session-directory session)))
    (should (file-name-absolute-p dir))
    (should-not (harmless-filesystem-root-p dir))
    (should (string-prefix-p (file-name-as-directory harmless-directory) dir))
    (should-not (equal (expand-file-name "summary.json" dir) "/summary.json"))
    (should-not (file-exists-p "/summary.json"))))

(defmacro harmless-workflow-test-with (dir &rest body)
  "Bind a temp project DIR and run BODY."
  (declare (indent 1))
  `(let* ((,dir (make-temp-file "harmless-workflow-" t))
          (harmless-directory (expand-file-name ".harmless" ,dir))
          (harmless--sessions (make-hash-table :test 'equal))
          (harmless-workflow-stop ,dir)
          (harmless-agent-stop ,dir)
          (harmless-perm-ask-function
           (lambda (_session _class _item callback)
             (funcall callback 'allow))))
     ,@body))

(defun harmless-workflow-test--session (dir script &rest args)
  "Return a session in DIR whose provider runs SCRIPT."
  (let ((provider (harmless-workflow-make-fake
                   :name "fake" :host "none" :script script)))
    (apply #'harmless-session-new
           :cwd (unless (plist-get args :detached) dir)
           :provider provider
           :model "parent-model"
           args)))

(ert-deftest harmless-workflow-refuses-a-bad-request ()
  (harmless-workflow-test-with dir
    (let ((session (harmless-workflow-test--session
                    dir (lambda (&rest _) (error "provider ran")))))
      (harmless-workflow-test-error
       session nil "\\`Error: workflow needs a name or a script\\'")
      (harmless-workflow-test-error
       session '(:name "review" :script "(complete \"ok\")")
       "\\`Error: workflow takes a name or a script\\'")
      (harmless-workflow-test-error
       session '(:script 42)
       "\\`Error: workflow script must be a string\\'")
      (dolist (script (list "" "   " "\n" "\t"))
        (harmless-workflow-test-error
         session (list :script script)
         "\\`Error: workflow script is empty\\'"))
      (harmless-workflow-test-error
       session '(:name "")
       "\\`Error: workflow name is empty\\'")
      (harmless-workflow-test-error
       session '(:name "   ")
       "\\`Error: workflow name is empty\\'")
      (dolist (name '("sub/" "/" "c:/" "../x" "a\\b" "a/b" "." ".."))
        (harmless-workflow-test-error
         session (list :name name)
         "\\`Error: workflow name is not a name\\'"))
      (dolist (name '("sub" "missing"))
        (harmless-workflow-test-error
         session (list :name name)
         (format "\\`Error: no workflow named %s\\'" name)))
      (harmless-workflow-test-error
       session '(:script "(complete \"ok\")" :description "two\nlines")
       "\\`Error: description must be one line\\'")
      (should (equal (harmless-workflow-test-result
                      session '(:script "(complete \"ok\")"
                                :description "/"))
                     "ok"))
      (should-not (harmless-agent-children session))
      (should-not (file-exists-p (expand-file-name ".harmless/workflows" dir)))
      (should-not (file-exists-p "/.harmless"))
      (harmless-workflow-test--not-root session))))

(ert-deftest harmless-workflow-discover-rejects-non-directories ()
  (should-not (harmless-workflow-discover nil))
  (should-not (harmless-workflow-discover ""))
  (should-not (harmless-workflow-discover "sub"))
  (should-not (harmless-workflow-discover "sub/"))
  (should-not (harmless-workflow-discover "/"))
  (should-not (file-exists-p "/.claude"))
  (should-not (file-exists-p "/.harmless"))
  (should-not (file-exists-p "/.grok")))

(ert-deftest harmless-workflow-closer-file-wins ()
  (let* ((root (make-temp-file "harmless-workflow-root-" t))
         (proj (expand-file-name "proj" root))
         (outer (expand-file-name ".claude/workflows/review.hw" root))
         (inner (expand-file-name ".harmless/workflows/review.hw" proj))
         (outer-text
          "---\r\nname: review\r\ndescription: Outer\r\n---\r\n(complete \"OUTER\")\r\n")
         (inner-text
          "---\nname: review\ndescription: Inner\n---\n(complete \"INNER\")\n")
         (harmless-workflow-stop root)
         (harmless-directory (expand-file-name ".harmless" proj))
         (harmless--sessions (make-hash-table :test 'equal)))
    (make-directory proj t)
    (harmless-workflow-test--write outer outer-text)
    (harmless-workflow-test--write inner inner-text)
    (let ((spec (harmless-workflow-find proj "review")))
      (should (equal (plist-get spec :script) "(complete \"INNER\")"))
      (should (equal (plist-get spec :path) inner))
      (should (equal (plist-get spec :description) "Inner")))
    (let ((spec (harmless-workflow-find
                 (file-name-as-directory proj) "review")))
      (should (equal (plist-get spec :path) inner)))
    (should (equal (plist-get (harmless-workflow-find root "review") :script)
                   "(complete \"OUTER\")"))
    (let ((session (harmless-workflow-test--session
                    proj (lambda (&rest _) (error "provider ran")))))
      (should (equal (harmless-workflow-test-result
                      session '(:name "review"))
                     "INNER"))
      (should (equal (harmless-workflow-test--text inner) inner-text))
      (harmless-workflow-test--not-root session))
    (let ((session (harmless-workflow-test--session
                    root (lambda (&rest _) (error "provider ran")))))
      (should (equal (harmless-workflow-test-result
                      session '(:name "review"))
                     "OUTER")))))

(ert-deftest harmless-workflow-bad-file-is-skipped-until-named ()
  (harmless-workflow-test-with dir
    (harmless-workflow-test--write
     (expand-file-name ".claude/workflows/broken.hw" dir)
     "---\nname: broken\ndescription: skip me\n---\n((( \n")
    (should-not (harmless-workflow-discover dir))
    (condition-case err
        (progn
          (harmless-workflow-find dir "broken")
          (ert-fail "find did not signal"))
      (error
       (should (string-match-p
                "\\`workflow script is invalid\\'"
                (error-message-string err)))))
    (let ((session (harmless-workflow-test--session
                    dir (lambda (&rest _) (error "provider ran")))))
      (harmless-workflow-test-error
       session '(:name "broken")
       "\\`Error: workflow script is invalid\\'"))))

(ert-deftest harmless-workflow-refuses-a-bad-script ()
  (harmless-workflow-test-with dir
    (let ((session (harmless-workflow-test--session
                    dir (lambda (&rest _) (error "provider ran")))))
      (dolist (pair '(("(let a \"hi\")" "workflow has no complete")
                      ("(complete \"ok\")\n(let a \"no\")"
                       "workflow has forms after complete")
                      ("(complete missing)"
                       "workflow variable is unbound: missing")
                      ("(let a \"x\")\n(let a \"y\")\n(complete a)"
                       "workflow variable is already bound: a")
                      ("(let sub/ \"x\")\n(complete sub/)"
                       "workflow variable is not a name")
                      ("/" "unknown workflow form")
                      ("sub/" "unknown workflow form")
                      ("(complete (shell-command \"x\"))"
                       "unknown workflow expression")
                      ("(complete (parallel))"
                       "workflow parallel is empty")
                      ("(complete (join 1 \"a\"))"
                       "workflow join needs a separator")
                      ("(complete" "workflow script is invalid")
                      ("(complete (agent \"\" :sandbox read-only))"
                       "prompt is empty")
                      ("(complete (agent \"x\" :sandbox jail))"
                       "sandbox must be workspace or read-only")))
        (harmless-workflow-test-error
         session (list :script (car pair))
         (format "\\`Error: %s\\'" (cadr pair))))
      (should-not (harmless-agent-children session))
      (should-not (file-exists-p "/summary.json")))))

(ert-deftest harmless-workflow-budget ()
  (harmless-workflow-test-with dir
    (let ((session (harmless-workflow-test--session
                    dir (lambda (&rest _) (error "provider ran"))))
          (script "(complete (parallel (agent \"a\") (agent \"b\")))"))
      (dolist (bad (list nil 0 -1 1.5 "8" t))
        (let ((harmless-workflow-max-agents bad))
          (harmless-workflow-test-error
           session (list :script script)
           "\\`Error: harmless-workflow-max-agents must be a positive integer\\'")))
      (let ((harmless-workflow-max-agents 1))
        (harmless-workflow-test-error
         session (list :script script)
         "\\`Error: workflow agent budget is 1\\'"))
      (should-not (harmless-agent-children session)))))

(ert-deftest harmless-workflow-fixed-script-returns-one-result ()
  (harmless-workflow-test-with dir
    (let* ((path (expand-file-name ".harmless/workflows/review.hw" dir))
           (text
            (concat
             "---\nname: review\ndescription: CATALOG-MARK\n---\n"
             "(let first (agent \"FIRST\" :sandbox read-only :label \"first\"))\n"
             "(let second (agent (join \" \" \"SECOND\" first)"
             " :sandbox read-only :label \"second\"))\n"
             "(complete (join \"\\n\" first second))\n"))
           (calls 0)
           (asked nil)
           (session
            (harmless-workflow-test--session
             dir
             (lambda (messages callback)
               (setq calls (1+ calls))
               (cond
                ((= calls 1)
                 (should (string-match-p
                          "CATALOG-MARK"
                          (harmless-workflow-test--contents messages)))
                 (should (string-match-p
                          "PARENT-SECRET"
                          (harmless-workflow-test--contents messages)))
                 (funcall callback
                          '(:tool-call "1" "harmless-tool-run-workflow"
                            "{\"name\":\"review\"}"))
                 (funcall callback '(:stop "tool_calls")))
                ((= calls 2)
                 (should (string-match-p
                          "FIRST"
                          (harmless-workflow-test--contents messages)))
                 (should-not (string-match-p
                              "PARENT-SECRET"
                              (harmless-workflow-test--contents messages)))
                 (should-not (string-match-p
                              "CATALOG-MARK"
                              (harmless-workflow-test--contents messages)))
                 (funcall callback '(:text "ALPHA"))
                 (funcall callback '(:stop "stop")))
                ((= calls 3)
                 (should (string-match-p
                          "SECOND ALPHA"
                          (harmless-workflow-test--contents messages)))
                 (should-not (string-match-p
                              "PARENT-SECRET"
                              (harmless-workflow-test--contents messages)))
                 (funcall callback '(:text "BETA"))
                 (funcall callback '(:stop "stop")))
                (t
                 (funcall callback '(:text "done"))
                 (funcall callback '(:stop "stop"))))))))
      (harmless-workflow-test--write path text)
      (setq harmless-perm-ask-function
            (lambda (&rest _) (setq asked t)))
      (harmless-turn-run session "go PARENT-SECRET")
      (should (= calls 4))
      (should-not asked)
      (should (equal (harmless-workflow-test--tools session)
                     '("ALPHA\nBETA")))
      (should (equal (harmless-workflow-test--text path) text))
      (should (= 2 (length (harmless-agent-children session))))
      (let ((first (cl-find "first" (harmless-agent-children session)
                            :key #'harmless-session-title :test #'equal))
            (second (cl-find "second" (harmless-agent-children session)
                             :key #'harmless-session-title :test #'equal)))
        (should first)
        (should second)
        (should (eq (plist-get (harmless-session-agent first) :sandbox)
                    'read-only))
        (harmless-workflow-test--not-root first)
        (harmless-workflow-test--not-root second)
        (let ((names (mapcar #'harmless-tool-name
                             (harmless-tools-for-session second))))
          (should (member "harmless-tool-read-file" names))
          (should-not (member "harmless-tool-write-file" names))
          (should-not (member "harmless-tool-spawn-agent" names))
          (should-not (member "harmless-tool-run-workflow" names)))))))

(ert-deftest harmless-workflow-parallel-keeps-order ()
  (harmless-workflow-test-with dir
    (let* ((calls 0)
           (held nil)
           (result nil)
           (session
            (harmless-workflow-test--session
             dir
             (lambda (_messages callback)
               (setq calls (1+ calls))
               (setq held (append held (list callback)))))))
      (harmless-workflow--tool
       session
       (list :script
             (concat
              "(let parts (parallel "
              "(agent \"one\" :sandbox read-only :label \"one\") "
              "(agent \"two\" :sandbox read-only :label \"two\"))) "
              "(complete (join \"\\n\" parts))"))
       (lambda (text) (setq result text)))
      (should-not result)
      (should (= calls 2))
      (should (= 2 (length (harmless-agent-children session))))
      (should (eq (harmless-session-status
                   (cl-find "one" (harmless-agent-children session)
                            :key #'harmless-session-title :test #'equal))
                  'streaming))
      (funcall (cadr held) '(:text "BETA"))
      (funcall (cadr held) '(:stop "stop"))
      (should-not result)
      (funcall (car held) '(:text "ALPHA"))
      (funcall (car held) '(:stop "stop"))
      (should (equal result "ALPHA\nBETA"))
      (harmless-workflow-test--not-root session))))

(ert-deftest harmless-workflow-read-only-definition-does-not-ask ()
  (harmless-workflow-test-with dir
    (let* ((calls 0)
           (asked nil)
           (session
            (harmless-workflow-test--session
             dir
             (lambda (messages callback)
               (setq calls (1+ calls))
               (cond
                ((= calls 1)
                 (funcall callback
                          '(:tool-call "1" "harmless-tool-run-workflow"
                            "{\"script\":\"(complete (agent \\\"look\\\" :agent reviewer))\"}"))
                 (funcall callback '(:stop "tool_calls")))
                ((= calls 2)
                 (should (string-match-p
                          "AGENT-PROMPT"
                          (harmless-workflow-test--contents messages)))
                 (should (string-match-p
                          "look"
                          (harmless-workflow-test--contents messages)))
                 (funcall callback '(:text "seen"))
                 (funcall callback '(:stop "stop")))
                (t
                 (funcall callback '(:text "ok"))
                 (funcall callback '(:stop "stop"))))))))
      (harmless-workflow-test--write
       (expand-file-name ".claude/agents/reviewer.md" dir)
       "---\nname: reviewer\nsandbox: read-only\n---\nAGENT-PROMPT\n")
      (setq harmless-perm-ask-function
            (lambda (&rest _) (setq asked t)))
      (harmless-turn-run session "go")
      (should-not asked)
      (should (equal (harmless-workflow-test--tools session) '("seen")))
      (let ((child (car (harmless-agent-children session))))
        (should (eq (plist-get (harmless-session-agent child) :sandbox)
                    'read-only))
        (harmless-workflow-test--not-root child)))))

(ert-deftest harmless-workflow-workspace-asks ()
  (harmless-workflow-test-with dir
    (let* ((calls 0)
           (asked nil)
           (session
            (harmless-workflow-test--session
             dir
             (lambda (_messages callback)
               (setq calls (1+ calls))
               (if (= calls 1)
                   (progn
                     (funcall callback
                              '(:tool-call "1" "harmless-tool-run-workflow"
                                "{\"script\":\"(complete (agent \\\"edit\\\"))\"}"))
                     (funcall callback '(:stop "tool_calls")))
                 (funcall callback '(:text "wrote"))
                 (funcall callback '(:stop "stop")))))))
      (setq harmless-perm-ask-function
            (lambda (_s class item cb)
              (push (list class item) asked)
              (funcall cb 'allow)))
      (harmless-turn-run session "go")
      (should (member '(edit "harmless-tool-run-workflow") asked))
      (should (equal (harmless-workflow-test--tools session) '("wrote")))
      (let ((child (car (harmless-agent-children session))))
        (should (eq (plist-get (harmless-session-agent child) :sandbox)
                    'workspace))
        (harmless-workflow-test--not-root child)))))

(ert-deftest harmless-workflow-plan-mode ()
  (harmless-workflow-test-with dir
    (let* ((calls 0)
           (asked nil)
           (script "(complete (agent \"edit\"))")
           (session
            (harmless-workflow-test--session
             dir
             (lambda (messages callback)
               (setq calls (1+ calls))
               (cond
                ((= calls 1)
                 (funcall callback
                          (list :tool-call "1" "harmless-tool-run-workflow"
                                (format "{\"script\":%S}" script)))
                 (funcall callback '(:stop "tool_calls")))
                ((and (= calls 2)
                      (string-match-p "TASK-RO" script))
                 (should (string-match-p
                          "TASK-RO"
                          (harmless-workflow-test--contents messages)))
                 (funcall callback '(:text "read only"))
                 (funcall callback '(:stop "stop")))
                (t
                 (funcall callback '(:text "after"))
                 (funcall callback '(:stop "stop"))))))))
      (setf (harmless-session-plan-mode session) t)
      (setf (harmless-session-permission-mode session) 'always-approve)
      (setq harmless-perm-ask-function
            (lambda (&rest _) (setq asked t)))
      (harmless-turn-run session "go")
      (should-not asked)
      (should-not (harmless-agent-children session))
      (should (equal (harmless-workflow-test--tools session)
                     '("Error: Plan mode is on. A workflow that can write is refused")))
      (setq script "(complete"
            calls 0
            asked nil)
      (harmless-turn-run session "again")
      (should (equal (car (last (harmless-workflow-test--tools session)))
                     "Error: workflow script is invalid"))
      (should-not (string-match-p
                   "Plan mode"
                   (car (last (harmless-workflow-test--tools session)))))
      (setq script "(complete (agent \"TASK-RO\" :sandbox read-only))"
            calls 0)
      (harmless-turn-run session "read")
      (should-not asked)
      (should (equal (car (last (harmless-workflow-test--tools session)))
                     "read only")))))

(ert-deftest harmless-workflow-subagent-cannot-start-one ()
  (harmless-workflow-test-with dir
    (harmless-workflow-test--write
     (expand-file-name ".claude/agents/nester.md" dir)
     "---\nname: nester\ntools: harmless-tool-run-workflow\n---\nNo.\n")
    (condition-case err
        (progn
          (harmless-agent-find dir "nester")
          (ert-fail "find did not signal"))
      (error
       (should (string-match-p
                "\\`a subagent cannot start a workflow\\'"
                (error-message-string err)))))
    (let* ((session (harmless-workflow-test--session
                     dir (lambda (&rest _) (error "provider ran"))))
           (child (harmless-agent-create
                   session '(:prompt "look") (lambda (_) nil))))
      (harmless-workflow-test-error
       child '(:script "(complete \"ok\")")
       "\\`Error: a subagent cannot start a workflow\\'")
      (should-not (harmless-agent-children child))
      (let ((names (mapcar #'harmless-tool-name
                           (harmless-tools-for-session child))))
        (should-not (member "harmless-tool-run-workflow" names))
        (should-not (member "harmless-tool-spawn-agent" names))))))

(ert-deftest harmless-workflow-parent-abort-cancels-children ()
  (harmless-workflow-test-with dir
    (let* ((calls 0)
           (session
            (harmless-workflow-test--session
             dir
             (lambda (_messages callback)
               (setq calls (1+ calls))
               (when (= calls 1)
                 (funcall callback
                          '(:tool-call "1" "harmless-tool-run-workflow"
                            "{\"script\":\"(complete (parallel (agent \\\"one\\\") (agent \\\"two\\\")))\"}"))
                 (funcall callback '(:stop "tool_calls")))))))
      (harmless-turn-run session "go")
      (should (= calls 3))
      (should (= 2 (length (harmless-agent-children session))))
      (dolist (child (harmless-agent-children session))
        (should (eq (harmless-session-status child) 'streaming)))
      (should (eq (harmless-session-status session) 'streaming))
      (harmless-turn-abort session)
      (should (eq (harmless-session-status session) 'idle))
      (dolist (child (harmless-agent-children session))
        (should (eq (harmless-session-status child) 'idle)))
      (should-not (harmless-workflow-test--tools session))
      (should (= calls 3)))))

(ert-deftest harmless-workflow-detached-and-ui ()
  (harmless-workflow-test-with dir
    (let ((default-directory (file-name-as-directory dir))
          (session (harmless-session-new
                    :detached t
                    :provider (harmless-workflow-make-fake
                               :name "fake" :host "none"
                               :script (lambda (&rest _)
                                         (error "provider ran")))
                    :model "parent-model")))
      (harmless-workflow-test--write
       (expand-file-name ".claude/workflows/review.hw" dir)
       "---\nname: review\n---\n(complete \"nope\")\n")
      (harmless-workflow-test-error
       session '(:name "review")
       "\\`Error: no workflow named review\\'")
      (should (equal (harmless-workflow-test-result
                      session '(:script "(complete \"ok\")"))
                     "ok"))
      (should-not (harmless-agent-children session))
      (harmless-workflow-test--not-root session)))
  (should (equal (harmless-ui--tool-verb "harmless-tool-run-workflow") "workflow"))
  (should (equal (harmless-ui--tool-summary
                  "harmless-tool-run-workflow" "{\"name\":\"review\"}")
                 "review"))
  (should (equal (harmless-ui--tool-summary
                  "harmless-tool-run-workflow"
                  "{\"description\":\"/\",\"name\":\"review\"}")
                 "/"))
  (let ((tool (harmless-tool-by-name "harmless-tool-run-workflow")))
    (should (harmless-tool-async tool))
    (should (eq (harmless-tool-class tool) 'edit))))

(ert-deftest harmless-workflow-json-complete-only ()
  (harmless-workflow-test-with dir
    (let* ((calls 0)
           (session
            (harmless-workflow-test--session
             dir
             (lambda (_messages callback)
               (setq calls (1+ calls))
               (if (= calls 1)
                   (progn
                     (funcall callback
                              '(:tool-call "1" "harmless-tool-run-workflow"
                                "{\"script\":\"(complete \\\"ok\\\")\"}"))
                     (funcall callback '(:stop "tool_calls")))
                 (funcall callback '(:text "after"))
                 (funcall callback '(:stop "stop")))))))
      (harmless-turn-run session "go")
      (should (equal (harmless-workflow-test--tools session) '("ok")))
      (should-not (harmless-agent-children session))
      (should (= calls 2)))))

(provide 'harmless-workflow-tests)
