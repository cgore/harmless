;;; harmless-worktree-tests.el --- Tests for Harmless worktrees -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless-provider)
(require 'harmless-session)
(require 'harmless-tools)
(require 'harmless-tools-fs)
(require 'harmless-perm)
(require 'harmless-turn)
(require 'harmless-agent)
(require 'harmless-workflow)
(require 'harmless-worktree)
(require 'harmless-fork)
(require 'harmless-ui)

(cl-defstruct (harmless-worktree-fake
               (:include harmless-provider)
               (:constructor harmless-worktree-make-fake))
  script)

(defvar harmless-worktree-test-tools nil
  "Tools from the latest fake completion.")

(cl-defmethod harmless-provider-complete ((provider harmless-worktree-fake)
                                          _messages tools callback)
  (setq harmless-worktree-test-tools tools)
  (when (harmless-worktree-fake-script provider)
    (funcall (harmless-worktree-fake-script provider) _messages callback))
  nil)

(defmacro harmless-worktree-test-error (expected &rest body)
  "Require BODY to signal EXPECTED."
  (declare (indent 1))
  `(let ((err (should-error (progn ,@body) :type 'error)))
     (should (equal (error-message-string err) ,expected))))

(defun harmless-worktree-test-git (dir &rest args)
  "Run git ARGS in DIR.  Fail the test when git fails."
  (let ((git (or (executable-find "git")
                 (ert-fail "git is not available"))))
    (with-temp-buffer
      (let* ((process-environment
              (cons "GIT_TERMINAL_PROMPT=0" process-environment))
             (exit (apply #'call-process git nil
                          (list (current-buffer) t) nil
                          (append (list "-C" dir) args))))
        (unless (eq exit 0)
          (ert-fail (format "git %S: %s" args (buffer-string))))))))

(defun harmless-worktree-test-init (repo)
  "Make REPO a git repository with one committed README."
  (harmless-worktree-test-git repo "init")
  (harmless-worktree-test-git repo "config" "user.email" "t@example.com")
  (harmless-worktree-test-git repo "config" "user.name" "t")
  (harmless-worktree-test-git repo "config" "commit.gpgsign" "false")
  (harmless-worktree-test-git repo "config" "core.autocrlf" "false")
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file (expand-file-name "README" repo)
      (insert "hello\n")))
  (harmless-worktree-test-git repo "add" "README")
  (harmless-worktree-test-git repo "commit" "-m" "init"))

(defun harmless-worktree-test-text (path)
  "Return the contents of PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun harmless-worktree-test-summary (session)
  "Return SESSION's summary plist."
  (harmless-json-decode
   (with-temp-buffer
     (insert-file-contents
      (expand-file-name "summary.json"
                        (harmless-session-directory session)))
     (buffer-string))))

(defun harmless-worktree-test-not-root (path)
  "Assert PATH is a checkout under the data directory and not /."
  (should (file-name-absolute-p path))
  (should-not (harmless-filesystem-root-p path))
  (should (harmless-directory-strictly-under-p
           path (harmless-data-directory)))
  (should-not (harmless-filesystem-root-p (harmless-data-directory)))
  (should-not (file-exists-p "/summary.json"))
  (should-not (file-exists-p "/worktrees")))

(defmacro harmless-worktree-test-with (repo data &rest body)
  "Bind a temp git REPO and data directory DATA, then run BODY."
  (declare (indent 2))
  `(let* ((,repo (make-temp-file "harmless-wt-repo-" t))
          (,data (make-temp-file "harmless-wt-data-" t))
          (harmless-directory ,data)
          (harmless--sessions (make-hash-table :test 'equal))
          (harmless-agent-stop ,repo)
          (harmless-workflow-stop ,repo)
          (harmless-perm-ask-function
           (lambda (_session _class _item callback)
             (funcall callback 'allow))))
     (harmless-worktree-test-init ,repo)
     ,@body))

(defun harmless-worktree-test-session (repo &optional script detached)
  "Return a session in REPO, or a detached chat when DETACHED."
  (apply #'harmless-session-new
         :provider (harmless-worktree-make-fake
                    :name "fake" :host "none" :script script)
         :model "m"
         (if detached
             (list :detached t)
           (list :cwd repo))))

(defun harmless-worktree-test-tool-text (session)
  "Return tool-result strings recorded on SESSION."
  (cl-loop for msg in (harmless-session-messages session)
           when (memq (plist-get msg :role) '(:tool tool))
           collect (plist-get msg :content)))

(ert-deftest harmless-worktree-refuses-a-bad-request ()
  (harmless-worktree-test-with repo data
    (let ((session (harmless-worktree-test-session repo))
          (root-wt (file-exists-p "/worktrees")))
      (dolist (name (list nil "" "   " "\n" "\t"))
        (harmless-worktree-test-error "worktree name is empty"
          (harmless-worktree-enter session name)))
      (dolist (name '("sub/" "/" "c:/" "../x" "a\\b" "a/b" "." ".."
                      "feat/"))
        (harmless-worktree-test-error "worktree name is not a name"
          (harmless-worktree-enter session name)))
      (harmless-worktree-test-error "worktree name is not a name"
        (harmless-worktree-enter session 42))
      (harmless-worktree-test-error "worktree name is not a name"
        (harmless-worktree-enter session :false))
      (dolist (cwd (list nil "" "sub" "sub/" "/"))
        (let ((root-before (file-exists-p "/worktrees")))
          (setf (harmless-session-cwd session) cwd)
          (harmless-worktree-test-error
              "This chat is not attached to a project"
            (harmless-worktree-enter session "feat"))
          (should (eq root-before (file-exists-p "/worktrees")))))
      (setf (harmless-session-cwd session) repo)
      (let ((plain (make-temp-file "harmless-wt-plain-" t))
            (detached (harmless-worktree-test-session repo nil t)))
        (harmless-worktree-test-error
            "this project is not a git repository"
          (harmless-worktree-enter
           (harmless-worktree-test-session plain) "feat"))
        (harmless-worktree-test-error
            "This chat is not attached to a project"
          (harmless-worktree-enter detached "feat")))
      (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
        (harmless-worktree-test-error "git is not available"
          (harmless-worktree-enter session "feat")))
      (should (eq root-wt (file-exists-p "/worktrees")))
      (should (equal data harmless-directory))
      (dolist (dir (list nil "" "sub" "sub/" "/"))
        (should-not (harmless-worktree-directory-p dir)))
      (should (harmless-worktree-directory-p "c:/"))
      (should (commandp #'harmless-worktree))
      (should (commandp #'harmless-worktree-exit))
      (should (eq 'edit (harmless-tool-class
                         (harmless-tool-by-name "enter_worktree"))))
      (should-not (harmless-tool-async
                   (harmless-tool-by-name "enter_worktree"))))))

(ert-deftest harmless-worktree-isolates-a-checkout ()
  (harmless-worktree-test-with repo data
    (let* ((session (harmless-worktree-test-session repo))
           (stored (harmless-session-directory session))
           (original (directory-file-name repo))
           (other nil))
      (make-directory (expand-file-name "sub" repo))
      (setf (harmless-session-shell-cwd session)
            (expand-file-name "sub" repo))
      (setf (harmless-session-cwd session) (file-name-as-directory repo))
      (let ((coding-system-for-write 'utf-8-unix))
        (with-temp-file (expand-file-name "NOTES" repo)
          (insert "dirty\n")))
      (let ((path (harmless-worktree-enter session "  feat  ")))
        (harmless-worktree-test-not-root path)
        (should (equal stored (harmless-session-directory session)))
        (should (file-directory-p path))
        (should (equal (harmless-worktree-test-text
                        (expand-file-name "README" path))
                       "hello\n"))
        (should-not (file-exists-p (expand-file-name "NOTES" path)))
        (should-not (harmless-session-shell-cwd session))
        (should (equal (harmless-session-cwd session) path))
        (should (equal (plist-get (harmless-session-worktree session) :name)
                       "feat"))
        (should (equal (plist-get (harmless-session-worktree session)
                                  :branch)
                       "harmless/feat"))
        (should (equal (plist-get (harmless-session-worktree session)
                                  :original)
                       original))
        (let ((summary (harmless-worktree-test-summary session)))
          (should (equal (plist-get summary :cwd) path))
          (should (equal (plist-get summary :worktree-name) "feat"))
          (should (equal (plist-get summary :worktree-path) path))
          (should (equal (plist-get summary :worktree-branch)
                         "harmless/feat"))
          (should (equal (plist-get summary :worktree-original) original))
          (should-not (plist-get summary :shell-cwd)))
        (let ((coding-system-for-write 'utf-8-unix))
          (with-temp-file (expand-file-name "README" path)
            (insert "WORKTREE\n")))
        (should (equal (harmless-worktree-test-text
                        (expand-file-name "README" repo))
                       "hello\n"))
        (should (equal (harmless-tool-call
                        (harmless-tool-by-name "write_file")
                        session
                        (list :path (expand-file-name "README" repo)
                              :contents "nope"))
                       (format "Error: Path escapes project: %s"
                               (expand-file-name "README" repo))))
        (should (equal path (harmless-worktree-enter session "feat")))
        (setq other (harmless-worktree-enter session "other"))
        (should-not (equal other path))
        (harmless-worktree-test-not-root other)
        (should (equal (harmless-worktree-test-text
                        (expand-file-name "README" other))
                       "hello\n"))
        (should (equal (harmless-worktree-test-text
                        (expand-file-name "README" path))
                       "WORKTREE\n"))
        (should (equal (plist-get (harmless-session-worktree session)
                                  :original)
                       original))
        (should (equal (harmless-session-cwd session) other))
        (harmless-session-unregister session)
        (let ((loaded (harmless-session-load stored)))
          (should (equal (harmless-session-cwd loaded) other))
          (should (equal (plist-get (harmless-session-worktree loaded)
                                    :name)
                         "other"))
          (should (equal (plist-get (harmless-session-worktree loaded)
                                    :original)
                         original))
          (setq session loaded)))
      (should (equal (harmless-ui--tool-verb "enter_worktree") "worktree"))
      (should (equal (harmless-ui--tool-verb "exit_worktree") "leave"))
      (should (equal (harmless-ui--tool-summary
                      "enter_worktree" '(:name "feat"))
                     "feat"))
      (let ((harmless-directory (expand-file-name "state" repo)))
        (let ((expected
               (error-message-string
                (should-error
                 (harmless-worktree-path
                  (directory-file-name repo) "inside")
                 :type 'error))))
          (should (string-prefix-p
                   "Refusing to store a Harmless worktree at "
                   expected))
          (harmless-worktree-test-error expected
            (harmless-worktree-enter session "inside")))))))

(ert-deftest harmless-worktree-exit-restores-the-project ()
  (harmless-worktree-test-with repo _data
    (let* ((session (harmless-worktree-test-session repo))
           (path (harmless-worktree-enter session "feat"))
           (fresh (harmless-worktree-test-session repo)))
      (harmless-worktree-test-error "this session is not in a worktree"
        (harmless-worktree--exit fresh))
      (setf (harmless-session-worktree fresh)
            (list :name "feat" :path path :branch "harmless/feat"
                  :original nil))
      (harmless-worktree-test-error
          "worktree original is not a directory"
        (harmless-worktree--exit fresh))
      (should (equal (harmless-session-cwd fresh)
                     (directory-file-name repo)))
      (setf (harmless-session-worktree session)
            (plist-put (harmless-session-worktree session)
                       :original "/"))
      (harmless-worktree-test-error
          "worktree original is not a directory"
        (harmless-worktree--exit session))
      (should (equal (harmless-session-cwd session) path))
      (should-not (file-exists-p "/summary.json"))
      (setf (harmless-session-worktree session)
            (plist-put (harmless-session-worktree session)
                       :original (directory-file-name repo)))
      (let ((missing (expand-file-name "missing" repo)))
        (setf (harmless-session-worktree session)
              (plist-put (harmless-session-worktree session)
                         :original missing))
        (harmless-worktree-test-error
            "worktree original is not a directory"
          (harmless-worktree--exit session))
        (should (equal (harmless-session-cwd session) path))
        (setf (harmless-session-worktree session)
              (plist-put (harmless-session-worktree session)
                         :original (directory-file-name repo))))
      (should (equal (harmless-worktree--exit session)
                     (directory-file-name repo)))
      (should-not (harmless-session-worktree session))
      (should (file-directory-p path))
      (should (equal (harmless-worktree-test-text
                      (expand-file-name "README" path))
                     "hello\n"))
      (let ((summary (harmless-worktree-test-summary session)))
        (should (equal (plist-get summary :cwd)
                       (directory-file-name repo)))
        (should-not (plist-get summary :worktree-name))))))

(ert-deftest harmless-worktree-refuses-a-collision ()
  (harmless-worktree-test-with repo _data
    (let ((session (harmless-worktree-test-session repo)))
      (harmless-worktree-test-git repo "branch" "harmless/taken")
      (harmless-worktree-test-error
          "worktree branch already exists: harmless/taken"
        (harmless-worktree-enter session "taken"))
      (let ((path (harmless-worktree-path
                   (directory-file-name repo) "blocked")))
        (make-directory (file-name-directory path) t)
        (with-temp-file path (insert "x"))
        (harmless-worktree-test-error "worktree path already exists"
          (harmless-worktree-enter session "blocked"))
        (should (equal (harmless-worktree-test-text path) "x"))))))

(ert-deftest harmless-worktree-plan-mode-refuses ()
  (harmless-worktree-test-with repo _data
    (let ((session (harmless-worktree-test-session repo))
          (asked nil))
      (setf (harmless-session-plan-mode session) t)
      (setf (harmless-session-permission-mode session) 'always-approve)
      (setq harmless-perm-ask-function
            (lambda (&rest _) (error "permission asked")))
      (harmless-worktree-test-error
          "Plan mode is on. A worktree is refused"
        (harmless-worktree-enter session "feat"))
      (should (equal (harmless-worktree-plan-refuses-p
                      session (harmless-tool-by-name "enter_worktree")
                      '(:name "feat"))
                     harmless-worktree-plan-refusal))
      (should-not (file-directory-p
                   (harmless-worktree-path
                    (directory-file-name repo) "feat")))
      (should-not asked))))

(ert-deftest harmless-worktree-permission ()
  (harmless-worktree-test-with repo _data
    (let* ((calls 0)
           (asked nil)
           (session
            (harmless-worktree-test-session
             repo
             (lambda (_messages callback)
               (setq calls (1+ calls))
               (if (= calls 1)
                   (progn
                     (funcall callback
                              '(:tool-call "1" "enter_worktree"
                                "{\"name\":\"feat\"}"))
                     (funcall callback '(:stop "tool_calls")))
                 (funcall callback '(:text "done"))
                 (funcall callback '(:stop "stop")))))))
      (setq harmless-perm-ask-function
            (lambda (_session class item callback)
              (push (list class item) asked)
              (funcall callback 'deny)))
      (harmless-turn-run session "go")
      (should (equal asked '((edit "enter_worktree"))))
      (should (equal (harmless-worktree-test-tool-text session)
                     '("Error: user denied this tool call")))
      (should-not (file-directory-p
                   (harmless-worktree-path
                    (directory-file-name repo) "feat")))
      (should (= calls 2))
      (setq calls 0)
      (setq asked nil)
      (setq session
            (harmless-worktree-test-session
             repo
             (lambda (_messages callback)
               (setq calls (1+ calls))
               (if (= calls 1)
                   (progn
                     (funcall callback
                              '(:tool-call "1" "enter_worktree"
                                "{\"name\":\"feat\"}"))
                     (funcall callback '(:stop "tool_calls")))
                 (funcall callback '(:text "done"))
                 (funcall callback '(:stop "stop"))))))
      (setf (harmless-session-permission-mode session) 'accept-edits)
      (setq harmless-perm-ask-function
            (lambda (&rest _) (error "permission asked")))
      (harmless-turn-run session "go")
      (should (= calls 2))
      (let ((path (car (harmless-worktree-test-tool-text session))))
        (should (file-directory-p path))
        (harmless-worktree-test-not-root path)
        (should (equal (harmless-worktree-test-text
                        (expand-file-name "README" path))
                       "hello\n"))))))

(ert-deftest harmless-worktree-child-is-isolated ()
  (harmless-worktree-test-with repo _data
    (let* ((session (harmless-worktree-test-session repo))
           (parent (directory-file-name (harmless-session-cwd session)))
           (one (harmless-agent-create
                 session '(:prompt "one" :worktree "one") #'ignore))
           (two (harmless-agent-create
                 session '(:prompt "two" :worktree "two") #'ignore))
           (again (harmless-agent-create
                   session '(:prompt "one" :worktree "one") #'ignore)))
      (should (harmless-same-directory-p
               (harmless-session-cwd session) parent))
      (should-not (harmless-session-worktree session))
      (should-not (harmless-session-worktree one))
      (should (file-directory-p (harmless-session-cwd one)))
      (should-not (harmless-same-directory-p
                   (harmless-session-cwd one) parent))
      (should-not (harmless-same-directory-p
                   (harmless-session-cwd one)
                   (harmless-session-cwd two)))
      (should (harmless-same-directory-p
               (harmless-session-cwd one)
               (harmless-session-cwd again)))
      (harmless-worktree-test-not-root (harmless-session-cwd one))
      (harmless-worktree-test-not-root (harmless-session-cwd two))
      (should (equal (harmless-worktree-test-text
                      (expand-file-name "README"
                                        (harmless-session-cwd one)))
                     "hello\n"))
      (let ((coding-system-for-write 'utf-8-unix))
        (with-temp-file (expand-file-name
                         "README" (harmless-session-cwd one))
          (insert "ONE\n")))
      (should (equal (harmless-worktree-test-text
                      (expand-file-name "README" repo))
                     "hello\n"))
      (should (equal (harmless-worktree-test-text
                      (expand-file-name
                       "README" (harmless-session-cwd two)))
                     "hello\n"))
      (harmless-worktree-test-error "this session is not in a worktree"
        (harmless-worktree--exit one))
      (should (eq 'edit (harmless-agent-class
                         session '(:prompt "x" :worktree "feat"))))
      (let ((coding-system-for-write 'utf-8-unix))
        (make-directory (expand-file-name ".claude/agents" repo) t)
        (with-temp-file (expand-file-name
                         ".claude/agents/reviewer.md" repo)
          (insert "---\nsandbox: read-only\n---\nLook.\n")))
      (setf (harmless-session-plan-mode session) t)
      (should (equal (harmless-agent-plan-refuses-p
                      session (harmless-tool-by-name "spawn_agent")
                      '(:prompt "x" :agent "reviewer" :worktree "feat"))
                     "Error: Plan mode is on. A worktree is refused"))
      (should-not (file-directory-p
                   (harmless-worktree-path
                    (directory-file-name repo) "feat")))
      (setf (harmless-session-plan-mode session) nil)
      (let ((reader (harmless-agent-create
                     session
                     '(:prompt "look" :agent "reviewer" :worktree "read")
                     #'ignore)))
        (should (eq 'read-only
                    (plist-get (harmless-session-agent reader) :sandbox)))
        (should (harmless-same-directory-p
                 (harmless-session-cwd session) parent))
        (should-not (member "enter_worktree"
                            (mapcar #'harmless-tool-name
                                    (harmless-tools-for-session reader))))
        (should (equal (harmless-agent-refuse-call
                        reader (harmless-tool-by-name "enter_worktree")
                        '(:name "next"))
                       harmless-agent-read-only-refusal))))))

(ert-deftest harmless-worktree-fork-keeps-the-checkout ()
  (harmless-worktree-test-with repo _data
    (let* ((session (harmless-worktree-test-session repo))
           (path (harmless-worktree-enter session "feat"))
           (child (harmless-fork--create
                   session (list (list :role :user :content "hi")))))
      (should (equal (harmless-session-cwd child) path))
      (should (equal (plist-get (harmless-session-worktree child) :name)
                     "feat"))
      (should (equal (plist-get (harmless-session-worktree child) :original)
                     (directory-file-name repo)))
      (should (equal (harmless-worktree--exit child)
                     (directory-file-name repo)))
      (should (equal (harmless-session-cwd session) path))
      (should (harmless-session-worktree session))
      (harmless-worktree-test-not-root (harmless-session-directory child))
      (should-not (equal (harmless-session-directory child)
                         (harmless-session-directory session))))))

(ert-deftest harmless-worktree-workflow-uses-one ()
  (harmless-worktree-test-with repo _data
    (let* ((result nil)
           (calls 0)
           (session
            (harmless-worktree-test-session
            repo
            (lambda (_messages callback)
              (setq calls (1+ calls))
              (funcall callback '(:text "seen"))
              (funcall callback '(:stop "stop"))))))
      (should (eq 'edit
                  (harmless-workflow-class
                   session
                   '(:script
                     "(complete (agent \"look\" :sandbox read-only :worktree feat))"))))
      (should (eq 'read
                  (harmless-workflow-class
                   session
                   '(:script
                     "(complete (agent \"look\" :sandbox read-only))"))))
      (setf (harmless-session-plan-mode session) t)
      (should (equal (harmless-workflow-plan-refuses-p
                      session (harmless-tool-by-name "run_workflow")
                      '(:script
                        "(complete (agent \"look\" :sandbox read-only :worktree feat))"))
                     "Error: Plan mode is on. A workflow that can write is refused"))
      (should-not (harmless-agent-children session))
      (setf (harmless-session-plan-mode session) nil)
      (harmless-workflow--tool
       session
       '(:script "(complete (agent \"look\" :worktree \"sub/\"))")
       (lambda (text) (setq result text)))
      (should (equal result "Error: worktree name is not a name"))
      (should-not (harmless-agent-children session))
      (setq result nil)
      (harmless-workflow--tool
       session
       '(:script
         "(complete (agent \"look\" :sandbox read-only :worktree feat))")
       (lambda (text) (setq result text)))
      (should (equal result "seen"))
      (should (= calls 1))
      (let ((child (car (harmless-agent-children session))))
        (should child)
        (should (equal (harmless-worktree-test-text
                        (expand-file-name
                         "README" (harmless-session-cwd child)))
                       "hello\n"))
        (harmless-worktree-test-not-root (harmless-session-cwd child))
        (should-not (harmless-same-directory-p
                     (harmless-session-cwd child)
                     (harmless-session-cwd session)))
        (should (eq 'read-only
                    (plist-get (harmless-session-agent child) :sandbox)))))))

(provide 'harmless-worktree-tests)
