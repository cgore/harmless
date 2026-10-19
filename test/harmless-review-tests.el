;;; harmless-review-tests.el --- Tests for read-only review -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(cl-defstruct (harmless-review-fake
               (:include harmless-provider)
               (:constructor harmless-review-make-fake))
  script)

(cl-defmethod harmless-provider-complete ((provider harmless-review-fake)
                                          _messages _tools callback)
  (when (harmless-review-fake-script provider)
    (funcall (harmless-review-fake-script provider) _messages callback))
  nil)

(defvar harmless-review-test-calls 0
  "Provider calls in the current review test.")

(defun harmless-review-test-error (message form)
  "Assert FORM signals MESSAGE."
  (let ((err (should-error (funcall form) :type 'error)))
    (should (equal message (error-message-string err)))))

(defun harmless-review-test-text (path)
  "Return the contents of PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun harmless-review-test-write (path text)
  "Write TEXT to PATH with Unix newlines."
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file path
      (insert text))))

(defun harmless-review-test-git (dir &rest args)
  "Run git ARGS in DIR.  Fail the test when git fails."
  (let ((git (or (executable-find "git")
                 (ert-fail "git is not available"))))
    (with-temp-buffer
      (let* ((process-environment
              (cons "GIT_TERMINAL_PROMPT=0" process-environment))
             (exit (apply #'call-process git nil t nil
                          (append (list "-C" dir) args))))
        (unless (eq exit 0)
          (ert-fail (format "git %S: %s" args (buffer-string))))))))

(defun harmless-review-test-init (repo)
  "Make REPO a git repository with one committed README."
  (harmless-review-test-git repo "init")
  (harmless-review-test-git repo "config" "user.email" "t@example.com")
  (harmless-review-test-git repo "config" "user.name" "t")
  (harmless-review-test-git repo "config" "commit.gpgsign" "false")
  (harmless-review-test-git repo "config" "core.autocrlf" "false")
  (harmless-review-test-git repo "config" "core.quotepath" "false")
  (harmless-review-test-write (expand-file-name "README" repo) "hello\n")
  (harmless-review-test-git repo "add" "README")
  (harmless-review-test-git repo "commit" "-m" "init"))

(defun harmless-review-test-status (dir)
  "Return DIR's porcelain status."
  (let ((git (or (executable-find "git")
                 (ert-fail "git is not available"))))
    (with-temp-buffer
      (let ((exit (call-process
                   git nil t nil "-C" dir "--no-optional-locks"
                   "status" "--porcelain=v1" "--untracked-files=all")))
        (unless (eq exit 0)
          (ert-fail (buffer-string)))
        (buffer-string)))))

(defun harmless-review-test-head (dir)
  "Return DIR's current branch name."
  (let ((git (or (executable-find "git")
                 (ert-fail "git is not available"))))
    (with-temp-buffer
      (let ((exit (call-process git nil t nil
                               "-C" dir "rev-parse" "--abbrev-ref" "HEAD")))
        (unless (eq exit 0)
          (ert-fail (buffer-string)))
        (string-trim (buffer-string))))))

(defmacro harmless-review-test-env (&rest body)
  "Evaluate BODY with a git project, a data directory, and a session."
  (declare (indent 0))
  `(let* ((base (make-temp-file "harmless-review-" t))
          (dir (expand-file-name "proj" base))
          (data (expand-file-name "data" base))
          (provider (harmless-make-openai-compat
                     "local" :host "127.0.0.1:9" :protocol "http"
                     :key "none" :models '("m")))
          (harmless-directory data)
          (harmless--sessions (make-hash-table :test 'equal))
          (harmless-providers nil)
          (harmless-compact-threshold nil)
          (harmless-review-function nil)
          (session nil))
     (make-directory dir t)
     (make-directory data t)
     (harmless-review-test-init dir)
     (setq harmless-providers (list provider))
     (setq session (harmless-session-new
                    :cwd dir :provider provider :model "m"
                    :title "Review"))
     ,@body))

(defun harmless-review-test-tool-script (event)
  "Return a provider script that sends EVENT once, then text."
  (lambda (_messages callback)
    (setq harmless-review-test-calls (1+ harmless-review-test-calls))
    (if (= harmless-review-test-calls 1)
        (progn
          (funcall callback event)
          (funcall callback '(:stop "tool_calls")))
      (funcall callback '(:text "done"))
      (funcall callback '(:stop "stop")))))

(defun harmless-review-test-tool-texts (session)
  "Return tool results stored on SESSION."
  (cl-loop for msg in (harmless-session-messages session)
           when (eq (plist-get msg :role) :tool)
           collect (plist-get msg :content)))

(ert-deftest harmless-review-refuses-bad-input ()
  (harmless-review-test-env
    (let ((names (directory-files dir))
          (cwd (harmless-session-cwd session))
          (chat (harmless-session-new
                 :detached t :provider provider :model "m"
                 :title "Chat"))
          (plain (expand-file-name "plain" base))
          (missing (expand-file-name "missing" base))
          (calls 0))
      (make-directory plain)
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
                   (setq calls (1+ calls))))
                ((symbol-function 'call-process)
                 (lambda (&rest _)
                   (setq calls (1+ calls))
                   1)))
        (dolist (bad (list nil "" 12))
          (harmless-review-test-error
           "No Harmless session"
           (lambda () (harmless-review-run bad nil)))
          (harmless-review-test-error
           "No Harmless session"
           (lambda () (harmless-review bad nil))))
        (dolist (case '(((:base nil) "review base is empty")
                        ((:base "") "review base is empty")
                        ((:base "  ") "review base is empty")
                        ((:base 12) "review base must be a string")
                        ((:base :false) "review base must be a string")
                        ((:base "/") "review base is not a branch")
                        ((:base "//") "review base is not a branch")
                        ((:base "/.") "review base is not a branch")
                        ((:base "c:/") "review base is not a branch")
                        ((:base "c:") "review base is not a branch")
                        ((:base "d:/") "review base is not a branch")
                        ((:base "C:/") "review base is not a branch")
                        ((:base "..") "review base is not a branch")
                        ((:base "../x") "review base is not a branch")
                        ((:base "~") "review base is not a branch")
                        ((:base "~/x") "review base is not a branch")
                        ((:base "-p") "review base is not a branch")
                        ((:base "a:b") "review base is not a branch")
                        ((:base "a\\b") "review base is not a branch")
                        ((:commit nil) "review commit is empty")
                        ((:commit "") "review commit is empty")
                        ((:commit " ") "review commit is empty")
                        ((:commit 12) "review commit must be a string")
                        ((:commit "/") "review commit is not a commit")
                        ((:commit "c:/") "review commit is not a commit")
                        ((:commit "..") "review commit is not a commit")
                        ((:commit "HEAD..base")
                         "review commit is not a commit")
                        ((:commit "~") "review commit is not a commit")
                        ((:commit "-p") "review commit is not a commit")
                        ((:base "/" :commit "/")
                         "review has both a base and a commit")
                        ((:base nil :commit nil)
                         "review has both a base and a commit")
                        ((:base "main" :commit "HEAD")
                         "review has both a base and a commit")
                        ((:base "/" :findings ("nope"))
                         "review base is not a branch")
                        ((:findings 12) "review findings must be a list")
                        ((:findings "nope")
                         "review findings must be a list")
                        ((:findings (:severity "high" :text "x"))
                         "review findings must be a list")
                        ((:findings ("nope"))
                         "review finding must be an item")
                        ((:findings (("nope")))
                         "review severity is not a severity")
                        ((:findings ((:text "x")))
                         "review severity is not a severity")
                        ((:findings ((:severity nil :text "x")))
                         "review severity is not a severity")
                        ((:findings ((:severity "" :text "x")))
                         "review severity is not a severity")
                        ((:findings ((:severity "HIGH" :text "x")))
                         "review severity is not a severity")
                        ((:findings ((:severity "info" :text "x")))
                         "review severity is not a severity")
                        ((:findings ((:severity 12 :text "x")))
                         "review severity is not a severity")
                        ((:findings ((:severity "high")))
                         "review finding is empty")
                        ((:findings ((:severity "high" :text nil)))
                         "review finding is empty")
                        ((:findings ((:severity "high" :text "")))
                         "review finding is empty")
                        ((:findings ((:severity "high" :text "  ")))
                         "review finding is empty")
                        ((:findings ((:severity "high" :text 12)))
                         "review finding must be a string")
                        ((:findings ((:severity "high" :text "x"
                                      :file nil)))
                         "review file is empty")
                        ((:findings ((:severity "high" :text "x" :file "")))
                         "review file is empty")
                        ((:findings ((:severity "high" :text "x" :file " ")))
                         "review file is empty")
                        ((:findings ((:severity "high" :text "x" :file 12)))
                         "review file must be a string")
                        ((:findings ((:severity "high" :text "x" :line nil)))
                         "review line is not a line")
                        ((:findings ((:severity "high" :text "x" :line 0)))
                         "review line is not a line")
                        ((:findings ((:severity "high" :text "x" :line -1)))
                         "review line is not a line")
                        ((:findings ((:severity "high" :text "x" :line 1.0)))
                         "review line is not a line")
                        ((:findings ((:severity "high" :text "x" :line "1")))
                         "review line is not a line")))
          (harmless-review-test-error
           (cadr case)
           (lambda () (harmless-review-run session (car case)))))
        (should (equal (harmless-tool-call
                        (harmless-tool-by-name "review")
                        session '(:base "/"))
                       "Error: review base is not a branch"))
        (dolist (bad (list nil "" "  " "notes" "sub/" "/" "c:/" "d:/"
                           "C:/" "~" "~/x" 12))
          (setf (harmless-session-cwd session) bad)
          (harmless-review-test-error
           "This chat is not attached to a project"
           (lambda () (harmless-review-run session nil))))
        (setf (harmless-session-cwd session) cwd)
        (harmless-review-test-error
         "This chat is not attached to a project"
         (lambda () (harmless-review-run chat nil)))
        (should (= calls 0))
        (should-not (harmless-session-review session)))
      (harmless-review-test-error
       "review base is not a branch"
       (lambda () (harmless-review-run session '(:base "no-such"))))
      (harmless-review-test-error
       "review base is not a branch"
       (lambda () (harmless-review-run session '(:base "notes"))))
      (harmless-review-test-error
       "review base is not a branch"
       (lambda () (harmless-review-run session '(:base "sub/"))))
      (harmless-review-test-error
       "review commit is not a commit"
       (lambda () (harmless-review-run session '(:commit "no-such"))))
      (setf (harmless-session-cwd session) plain)
      (harmless-review-test-error
       "this project is not a git repository"
       (lambda () (harmless-review-run session nil)))
      (setf (harmless-session-cwd session) missing)
      (harmless-review-test-error
       "this project is not a git repository"
       (lambda () (harmless-review-run session nil)))
      (should-not (file-exists-p missing))
      (setf (harmless-session-cwd session) cwd)
      (let ((git-calls 0))
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (&rest _) nil))
                  ((symbol-function 'call-process)
                   (lambda (&rest _)
                     (setq git-calls (1+ git-calls))
                     1)))
          (harmless-review-test-error
           "git is not available"
           (lambda () (harmless-review-run session nil)))
          (should (= git-calls 0))))
      (let ((orig (symbol-function 'call-process)))
        (cl-letf (((symbol-function 'call-process)
                   (lambda (&rest args)
                     (if (member "diff" args)
                         1
                       (apply orig args)))))
          (harmless-review-test-error
           "review could not read the diff"
           (lambda () (harmless-review-run session nil)))))
      (should-not (harmless-session-review session))
      (should (equal names (directory-files dir)))
      (should-not (file-exists-p (expand-file-name "notes" dir)))
      (should-not (file-directory-p (expand-file-name "sub" dir)))
      (should-not (file-exists-p "/plan.md"))
      (should (equal "hello\n"
                     (harmless-review-test-text
                      (expand-file-name "README" dir)))))))

(ert-deftest harmless-review-reads-a-diff-and-records-findings ()
  (harmless-review-test-env
    (let* ((readme (expand-file-name "README" dir))
           (seen (expand-file-name "seen.txt" dir))
           (cwd (harmless-session-cwd session))
           (stored '(:kind branch :base "base" :commit nil
                     :findings
                     ((:severity high :file "README" :line 1
                       :text
                       "--- a/README +++ b/README -hello +changed")
                      (:severity medium :file "/" :line 2 :text "c:/")
                      (:severity low :file nil :line nil :text "notes"))))
           (before nil)
           (status nil)
           (head nil)
           (orig (symbol-function 'call-process))
           (seen-calls nil))
      (should (equal (harmless-review-run session nil)
                     "Review target: working tree\nNo diff"))
      (should-not (harmless-session-review session))
      (harmless-review-test-git dir "branch" "base")
      (harmless-review-test-write readme "hello\nSECOND\n")
      (harmless-review-test-git dir "add" "README")
      (harmless-review-test-git dir "commit" "-m" "second")
      (harmless-review-test-write readme "hello\nSECOND\nWORK\n")
      (harmless-review-test-write seen "seen\n")
      (setq before (harmless-review-test-text readme))
      (setq status (harmless-review-test-status dir))
      (setq head (harmless-review-test-head dir))
      (cl-letf (((symbol-function 'call-process)
                 (lambda (&rest args)
                   (push args seen-calls)
                   (apply orig args))))
        (let ((calls 0)
              (tree nil)
              (branch nil)
              (commit nil)
              (parent nil)
              (short nil))
          (cl-letf (((symbol-function 'write-region)
                     (lambda (&rest _) (setq calls (1+ calls)))))
            (setq tree (harmless-review-run session nil))
            (setq branch (harmless-review-run session '(:base " base ")))
            (setq commit (harmless-review-run session '(:commit "HEAD")))
            (setq parent (harmless-review-run
                          session '(:commit "HEAD~1")))
            (should-not (harmless-session-review session))
            (should (= calls 0)))
          (should (string-prefix-p "Review target: working tree\n" tree))
          (should (string-search "WORK" tree))
          (should (string-search "Untracked:\nseen.txt" tree))
          (should (string-prefix-p "Review target: branch base\n" branch))
          (should (string-search "SECOND" branch))
          (should-not (string-search "WORK" branch))
          (should (string-prefix-p "Review target: commit HEAD\n" commit))
          (should (string-search "second" commit))
          (should (string-search "SECOND" commit))
          (should-not (string-search "WORK" commit))
          (should (string-prefix-p
                   "Review target: commit HEAD~1\n" parent))
          (should (string-search "init" parent))
          (should-not (string-search "SECOND" parent))
          (setf (harmless-session-cwd session)
                (file-name-as-directory cwd))
          (should (string-search
                   "WORK" (harmless-review-run session nil)))
          (setf (harmless-session-cwd session) cwd)
          (let ((harmless-review-max-bytes 20))
            (setq short (harmless-review-run session nil))
            (should (= 20 (string-search "\n[truncated]" short))))
          (let ((harmless-review-max-bytes nil))
            (should-not (string-search
                         "[truncated]"
                         (harmless-review-run session nil)))))
        (should (equal (harmless-review-run
                        session
                        '(:base "base"
                          :findings
                          ((:severity "high" :file "README" :line 1
                            :text
                            "--- a/README\n+++ b/README\n-hello\n+changed")
                           (:severity :medium :file " / " :line 2
                            :text "c:/")
                           (:severity "low" :text "notes"))))
                       (concat
                        "[high] README:1: --- a/README +++ b/README -hello +changed\n"
                        "[medium] /:2: c:/\n"
                        "[low] notes")))
        (should (equal (harmless-session-review session) stored))
        (should (equal (harmless-review-header session) "  review 3"))
        (let ((file (harmless-session--file session "summary.json"))
              (calls 0))
          (should (file-name-absolute-p file))
          (should-not (harmless-filesystem-root-p file))
          (should-not (harmless-filesystem-root-p
                       (file-name-directory file)))
          (should (harmless-directory-strictly-under-p
                   (file-name-directory file) data))
          (should (equal file
                         (expand-file-name
                          "summary.json"
                          (harmless-session-directory session))))
          (should (string-search "c:/" (harmless-review-test-text file)))
          (cl-letf (((symbol-function 'write-region)
                     (lambda (&rest _) (setq calls (1+ calls)))))
            (should (equal (harmless-review-run
                            session
                            '(:base "base"
                              :findings
                              ((:severity "high" :file "README" :line 1
                                :text
                                "--- a/README +++ b/README -hello +changed")
                               (:severity "medium" :file "/" :line 2
                                :text "c:/")
                               (:severity "low" :text "notes"))))
                           (concat
                            "[high] README:1: --- a/README +++ b/README -hello +changed\n"
                            "[medium] /:2: c:/\n"
                            "[low] notes")))
            (should (= calls 0))
            (should (equal (harmless-session-review session) stored))))
        (should (equal (harmless-review-run
                        session
                        '("commit" "HEAD"
                          "findings"
                          (("severity" "low" "file" "sub/" "line" 4
                            "text" "notes"))))
                       "[low] sub/:4: notes"))
        (should (equal (plist-get (harmless-session-review session) :kind)
                       'commit))
        (should (equal (plist-get (harmless-session-review session) :commit)
                       "HEAD"))
        (should (equal (harmless-review-run session '(:findings nil))
                       "No findings"))
        (should (equal (harmless-session-review session)
                       '(:kind working-tree :base nil :commit nil
                         :findings nil)))
        (should (equal (harmless-review-header session) ""))
        (should (plist-member (harmless-session-summary-plist session)
                              :review))
        (let ((calls 0))
          (cl-letf (((symbol-function 'write-region)
                     (lambda (&rest _) (setq calls (1+ calls)))))
            (should (equal (harmless-review-run session '(:findings nil))
                           "No findings"))
            (should (= calls 0))))
        (should (eq (harmless-tool-class (harmless-tool-by-name "review"))
                    'read))
        (should-not (harmless-perm-needed-p session 'read))
        (let ((kept (harmless-session-review session))
              (turns 0))
          (cl-letf (((symbol-function 'harmless-turn-run)
                     (lambda (&rest _) (setq turns (1+ turns)))))
            (let ((harmless-review-function
                   (lambda (_session _args _diff) nil)))
              (should (string-search "WORK" (harmless-review session nil)))
              (should (equal kept (harmless-session-review session))))
            (let ((harmless-review-function
                   (lambda (_session _args _diff)
                     '((:severity "high" :file "/" :text "c:/")))))
              (should (equal (harmless-review session nil)
                             "[high] /: c:/"))
              (should (equal (harmless-session-review session)
                             '(:kind working-tree :base nil :commit nil
                               :findings ((:severity high :file "/"
                                           :line nil :text "c:/"))))))
            (let ((harmless-review-function
                   (lambda (_session _args _diff) "nope"))
                  (again (harmless-session-review session)))
              (harmless-review-test-error
               "review findings must be a list"
               (lambda () (harmless-review session nil)))
              (should (equal (harmless-session-review session) again)))
            (should (= turns 0)))))
      (dolist (args seen-calls)
        (should (string-prefix-p
                 "git" (file-name-nondirectory (car args))))
        (dolist (bad '("apply" "checkout" "reset" "clean" "add" "commit"
                       "merge" "rebase" "push" "pull" "switch" "restore"
                       "stash"))
          (should-not (member bad args))))
      (should (equal before (harmless-review-test-text readme)))
      (should (equal "seen\n" (harmless-review-test-text seen)))
      (should (equal status (harmless-review-test-status dir)))
      (should (equal head (harmless-review-test-head dir)))
      (should-not (file-exists-p (expand-file-name "notes" dir)))
      (should-not (file-directory-p (expand-file-name "sub" dir)))
      (should-not (file-exists-p "/plan.md"))
      (should (equal (harmless-ui--tool-summary "review" nil)
                     "working tree"))
      (should (equal (harmless-ui--tool-summary "review" '(:base "base"))
                     "base"))
      (should (equal (harmless-ui--tool-summary
                      "review" '(:commit "HEAD"))
                     "HEAD"))
      (should (equal (harmless-ui--tool-summary
                      "review"
                      '(:findings ((:text "change it")
                                   (:text "c:/"))))
                     "change it, c:/")))))

(ert-deftest harmless-review-restores-forks-and-renders ()
  (harmless-review-test-env
    (let ((names (directory-files dir))
          (loaded nil)
          (child nil)
          (agent nil)
          (chat nil)
          (kept nil))
      (should (equal (harmless-review-run
                      session
                      '(:findings ((:severity "high" :file "/" :line 1
                                    :text "c:/"))))
                     "[high] /:1: c:/"))
      (setq kept (harmless-session-review session))
      (setq loaded (harmless-session-load
                    (harmless-session-directory session)))
      (should (equal (harmless-session-review loaded) kept))
      (should (equal (harmless-review-header loaded) "  review 1"))
      (let ((plist (harmless-json-decode
                    (harmless-review-test-text
                     (harmless-session--file session "summary.json"))))
            (next nil))
        (while plist
          (unless (eq (car plist) :review)
            (setq next (plist-put next (car plist) (cadr plist))))
          (setq plist (cddr plist)))
        (let ((coding-system-for-write 'utf-8-unix))
          (with-temp-file (harmless-session--file session "summary.json")
            (insert (harmless-json-text next) "\n"))))
      (setq loaded (harmless-session-load
                    (harmless-session-directory session)))
      (should-not (harmless-session-review loaded))
      (should (equal kept (harmless-session-review session)))
      (let ((calls 0))
        (cl-letf (((symbol-function 'write-region)
                   (lambda (&rest _) (setq calls (1+ calls)))))
          (harmless-review-restore
           loaded
           '(:kind "working-tree" :base nil :commit nil
             :findings ((:severity "high" :file "README" :line 1
                         :text "keep"))))
          (should (= calls 0))
          (should (equal (harmless-session-review loaded)
                         '(:kind working-tree :base nil :commit nil
                           :findings ((:severity high :file "README"
                                       :line 1 :text "keep")))))
          (harmless-review-restore loaded nil)
          (should-not (harmless-session-review loaded))
          (should (= calls 0))))
      (harmless-review-restore
       loaded
       '(:kind "branch" :base "base" :commit nil
         :findings ((:severity "high" :file "/" :line 2 :text "c:/")
                    (:severity "nope" :file "a" :line 1 :text "bad")
                    "nope"
                    (:severity "low" :text "  keep  ")
                    (:severity "medium" :file "  " :text "x")
                    (:severity "medium" :file "notes" :line 0 :text "x")
                    (:severity "medium" :file "sub/" :line 1 :text "  "))))
      (should (equal (harmless-session-review loaded)
                     '(:kind branch :base "base" :commit nil
                       :findings ((:severity high :file "/" :line 2
                                   :text "c:/")
                                  (:severity low :file nil :line nil
                                   :text "keep")))))
      (harmless-review-restore
       loaded '(:kind "nope" :findings ((:severity "low" :text "x"))))
      (should-not (harmless-session-review loaded))
      (should (file-name-absolute-p (harmless-session-directory loaded)))
      (should-not (harmless-filesystem-root-p
                   (harmless-session-directory loaded)))
      (should (harmless-directory-strictly-under-p
               (harmless-session-directory loaded) data))
      (setq child (harmless-fork--create session nil))
      (should (equal (harmless-session-review child) kept))
      (should (file-name-absolute-p (harmless-session-directory child)))
      (should-not (harmless-filesystem-root-p
                   (harmless-session-directory child)))
      (should (harmless-directory-strictly-under-p
               (harmless-session-directory child) data))
      (should (equal (harmless-review-run
                      child '(:findings ((:severity "low" :text "child"))))
                     "[low] child"))
      (should (equal (harmless-session-review session) kept))
      (setq agent (harmless-agent-create
                   session '(:prompt "look") #'ignore))
      (should-not (harmless-session-review agent))
      (should (file-name-absolute-p (harmless-session-directory agent)))
      (should-not (harmless-filesystem-root-p
                   (harmless-session-directory agent)))
      (should (harmless-directory-strictly-under-p
               (harmless-session-directory agent) data))
      (should (equal (harmless-session-review session) kept))
      (setf (harmless-session-agent agent)
            (list :allow 'all :sandbox 'read-only :prompt "p"))
      (should (member "review"
                      (mapcar #'harmless-tool-name
                              (harmless-tools-for-session agent))))
      (should (member "review"
                      (mapcar #'harmless-tool-name
                              (harmless-tools-for-session session))))
      (should (equal (harmless-review-run
                      agent '(:findings ((:severity "low" :text "agent"))))
                     "[low] agent"))
      (should (equal (harmless-session-review session) kept))
      (setf (plist-get (harmless-session-agent agent) :allow)
            '("read_file"))
      (should (equal (harmless-agent-refuse-call
                      agent (harmless-tool-by-name "review") nil)
                     "Error: this subagent cannot use review"))
      (setq chat (harmless-session-new
                  :detached t :provider provider :model "m" :title "Chat"))
      (should-not (member "review"
                          (mapcar #'harmless-tool-name
                                  (harmless-tools-for-session chat))))
      (harmless-review-test-error
       "This chat is not attached to a project"
       (lambda () (harmless-review-run chat nil)))
      (should (harmless-directory-strictly-under-p
               (harmless-session-directory chat) data))
      (should (equal (harmless-ui--tool-verb "review") "review"))
      (unwind-protect
          (let ((harmless--session session))
            (harmless-ui-render-session session)
            (should (string-prefix-p
                     "Review\n[high] /:1: c:/\n\n"
                     (with-current-buffer (harmless-session-buffer session)
                       (buffer-string))))
            (should (string-suffix-p
                     "  review 1" (harmless-ui--header-line))))
        (when (buffer-live-p (harmless-session-buffer session))
          (kill-buffer (harmless-session-buffer session))))
      (should (equal names (directory-files dir)))
      (should-not (file-exists-p (expand-file-name "notes" dir)))
      (should-not (file-exists-p (expand-file-name "sub" dir)))
      (should-not (file-exists-p "/plan.md")))))

(ert-deftest harmless-review-ask-and-plan-do-not-block ()
  (harmless-review-test-env
    (let ((names nil)
          (readme (expand-file-name "README" dir))
          (harmless-review-test-calls 0))
      (harmless-review-test-write readme "hello\nWORK\n")
      (harmless-review-test-write (expand-file-name "keep.txt" dir)
                                  "keep\n")
      (setq names (directory-files dir))
      (let ((session (harmless-session-new
                      :cwd dir :title "Ask" :model "m"
                      :provider
                      (harmless-review-make-fake
                       :name "fake" :host "none"
                       :script
                       (harmless-review-test-tool-script
                        '(:tool-call "1" "review" "{}"))))))
        (setq harmless-perm-ask-function
              (lambda (&rest _) (error "permission asked")))
        (setq harmless-review-test-calls 0)
        (harmless-turn-run session "go")
        (should (string-search
                 "WORK"
                 (car (harmless-review-test-tool-texts session))))
        (should (string-prefix-p
                 "Review target: working tree\n"
                 (car (harmless-review-test-tool-texts session))))
        (should-not (harmless-session-review session))
        (should-not (harmless-session-plan-mode session)))
      (let ((session (harmless-session-new
                      :cwd dir :title "Plan" :model "m"
                      :provider
                      (harmless-review-make-fake
                       :name "fake" :host "none"
                       :script
                       (harmless-review-test-tool-script
                        '(:tool-call "1" "review"
                          "{\"findings\":[{\"severity\":\"low\",\"file\":\"/\",\"line\":1,\"text\":\"c:/\"}]}"))))))
        (setf (harmless-session-plan-mode session) t)
        (setq harmless-perm-ask-function
              (lambda (&rest _) (error "permission asked")))
        (setq harmless-review-test-calls 0)
        (harmless-turn-run session "go")
        (should (harmless-session-plan-mode session))
        (should (equal (harmless-review-test-tool-texts session)
                       '("[low] /:1: c:/")))
        (should-not (file-exists-p (expand-file-name "plan.md" dir))))
      (should (equal "hello\nWORK\n"
                     (harmless-review-test-text readme)))
      (should (equal "keep\n"
                     (harmless-review-test-text
                      (expand-file-name "keep.txt" dir))))
      (should (equal names (directory-files dir))))))
