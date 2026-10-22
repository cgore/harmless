;;; harmless-checkpoint-tests.el --- Tests for shadow checkpoints -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(defun harmless-checkpoint-test-error (text thunk)
  "THUNK must signal an error whose message is exactly TEXT."
  (let ((err (should-error (funcall thunk) :type 'error)))
    (should (equal text (error-message-string err)))))

(defun harmless-checkpoint-test-text (file)
  "Return FILE as UTF-8 text."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents file)
      (buffer-string))))

(defun harmless-checkpoint-test-logical (file)
  "Return FILE as UTF-8 text with carriage returns removed.
`harmless-tool-write-file' follows the platform newline convention."
  (replace-regexp-in-string "\r" "" (harmless-checkpoint-test-text file)))

(defun harmless-checkpoint-test-write (file text)
  "Write TEXT to FILE as UTF-8."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil file nil 'silent)))

(defun harmless-checkpoint-test-git (dir &rest args)
  "Run git ARGS in DIR with Harmless git variables removed.
Return (STATUS . OUTPUT)."
  (let ((process-environment
         (cons "GIT_CONFIG_NOSYSTEM=1"
               (cl-remove-if (lambda (kv) (string-match-p "\\`GIT_" kv))
                             process-environment))))
    (with-temp-buffer
      (cons (apply #'call-process "git" nil t nil
                   "-C" dir
                   "-c" "user.email=harmless@example.com"
                   "-c" "user.name=Harmless"
                   "-c" "commit.gpgsign=false"
                   "-c" "core.autocrlf=false"
                   args)
            (buffer-string)))))

(defun harmless-checkpoint-test-project (dir)
  "Make DIR a git checkout with README and a.txt."
  (harmless-checkpoint-test-write (expand-file-name "README" dir) "readme\n")
  (harmless-checkpoint-test-write (expand-file-name "a.txt" dir) "A\n")
  (harmless-checkpoint-test-write (expand-file-name "b.txt" dir) "B\n")
  (should (eq 0 (car (harmless-checkpoint-test-git dir "init"))))
  (should (eq 0 (car (harmless-checkpoint-test-git dir "add" "--" "README" "a.txt" "b.txt"))))
  (should (eq 0 (car (harmless-checkpoint-test-git dir "commit" "-m" "start")))))

(defun harmless-checkpoint-test-head (dir)
  "Return DIR's current commit."
  (let ((result (harmless-checkpoint-test-git dir "rev-parse" "HEAD")))
    (should (eq 0 (car result)))
    (string-trim (cdr result))))

(defun harmless-checkpoint-test-session (data project)
  "Return a session whose project is PROJECT and whose data is DATA."
  (let* ((harmless-directory data)
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-checkpoints nil)
         (provider (harmless-make-openai-compat
                    "local" :host "127.0.0.1:9" :protocol "http"
                    :key "none" :models '("m"))))
    (harmless-session-new :cwd project :provider provider :model "m")))

(defun harmless-checkpoint-test-subjects (session)
  "Return checkpoint subjects for SESSION's project, newest first."
  (let* ((work (harmless-session-cwd session))
         (git-dir (expand-file-name ".git" (harmless-checkpoint--store work))))
    (split-string
     (harmless-checkpoint--git git-dir work "log" "--format=%s")
     "\n" t)))

(ert-deftest harmless-checkpoint-snapshots-before-an-edit ()
  (let* ((data (make-temp-file "harmless-cp-data-" t))
         (project (make-temp-file "harmless-cp-proj-" t))
         (harmless-directory data)
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-checkpoints t)
         (head nil)
         (session nil))
    (unwind-protect
        (progn
          (harmless-checkpoint-test-project project)
          (setq head (harmless-checkpoint-test-head project))
          (setq session (harmless-checkpoint-test-session data project))
          (harmless-checkpoint-begin-turn session)
          (should (string-prefix-p
                   "Wrote "
                   (harmless-tools-fs--write
                    session '(:path "a.txt" :contents "A2\n"))))
          (should (equal '("cp-1") (harmless-checkpoint-test-subjects session)))
          (should (equal "A\n"
                         (let* ((work (harmless-session-cwd session))
                                (git-dir (expand-file-name
                                          ".git" (harmless-checkpoint--store work)))
                                (found (harmless-checkpoint--hash work "cp-1")))
                           (harmless-checkpoint--git
                            git-dir work "show" (concat (cdr found) ":a.txt")))))
          (should (equal head (harmless-checkpoint-test-head project)))
          (let ((shadow (cdr (harmless-checkpoint--hash
                              (harmless-session-cwd session) "cp-1"))))
            (should (not (eq 0 (car (harmless-checkpoint-test-git
                                      project "cat-file" "-t" shadow))))))
          (should (string-prefix-p
                   (file-name-as-directory (file-truename data))
                   (file-truename (harmless-checkpoint--store project))))
          (should-not (string-prefix-p
                       (file-name-as-directory (file-truename project))
                       (file-truename (harmless-checkpoint--store project))))
          (harmless-checkpoint-test-write (expand-file-name "b.txt" project) "HAND\n")
          (harmless-checkpoint-test-write (expand-file-name "extra.txt" project) "NEW\n")
          (should (equal "Restored a.txt in cp-1"
                         (harmless-checkpoint-restore session "cp-1" "a.txt")))
          (should (equal "A\n" (harmless-checkpoint-test-text
                                (expand-file-name "a.txt" project))))
          (should (equal "HAND\n" (harmless-checkpoint-test-text
                                   (expand-file-name "b.txt" project))))
          (should (file-exists-p (expand-file-name "extra.txt" project)))
          (should (equal "Restored cp-1"
                         (harmless-checkpoint-restore session "cp-1")))
          (should (equal "A\n" (harmless-checkpoint-test-text
                                (expand-file-name "a.txt" project))))
          (should (equal "B\n" (harmless-checkpoint-test-text
                                (expand-file-name "b.txt" project))))
          (should-not (file-exists-p (expand-file-name "extra.txt" project)))
          (should (equal head (harmless-checkpoint-test-head project)))
          (should (file-exists-p (expand-file-name ".git/HEAD" project)))
          (should (string-prefix-p
                   "Wrote "
                   (harmless-tools-fs--write
                    session '(:path "a.txt" :contents "A2\n"))))
          (should (equal "A2\n"
                         (harmless-checkpoint-test-logical
                          (expand-file-name "a.txt" project))))
          (should (equal '("cp-1") (harmless-checkpoint-test-subjects session)))
          (harmless-checkpoint-begin-turn session)
          (harmless-tools-fs--write session '(:path "a.txt" :contents "A3\n"))
          (should (equal '("cp-2" "cp-1")
                         (harmless-checkpoint-test-subjects session)))
          (should (string-search "A3" (harmless-checkpoint-diff session "cp-2")))
          (should (equal "" (harmless-checkpoint-diff session "cp-2" "b.txt")))
          (harmless-checkpoint-test-error
           "checkpoint file is missing"
           (lambda () (harmless-checkpoint-restore session "cp-1" "extra.txt")))
          (should (eq 'read (harmless-tool-class
                             (harmless-tool-by-name "harmless-tool-checkpoint-diff"))))
          (should (eq 'edit (harmless-tool-class
                             (harmless-tool-by-name "harmless-tool-checkpoint-restore"))))
          (should (equal "diff" (harmless-ui--tool-verb "harmless-tool-checkpoint-diff")))
          (should (equal "restore" (harmless-ui--tool-verb "harmless-tool-checkpoint-restore")))
          (should (equal "cp-1"
                         (harmless-ui--tool-summary
                          "harmless-tool-checkpoint-restore" '(:id "cp-1"))))
          (should (equal "Error: checkpoint id must be a string"
                         (harmless-tool-call
                          (harmless-tool-by-name "harmless-tool-checkpoint-diff")
                          session '(:id 42)))))
      (delete-directory data t)
      (delete-directory project t))))

(ert-deftest harmless-checkpoint-is-off-until-asked ()
  (let* ((data (make-temp-file "harmless-cp-off-" t))
         (project (make-temp-file "harmless-cp-off-proj-" t))
         (harmless-directory data)
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-checkpoints nil)
         (session nil))
    (unwind-protect
        (progn
          (setq session (harmless-checkpoint-test-session data project))
          (should (string-prefix-p
                   "Wrote "
                   (harmless-tools-fs--write
                    session '(:path "a.txt" :contents "off\n"))))
          (should (equal "off\n"
                         (harmless-checkpoint-test-logical
                          (expand-file-name "a.txt" project))))
          (should-not (file-directory-p
                       (expand-file-name "checkpoints" data))))
      (delete-directory data t)
      (delete-directory project t))))

(ert-deftest harmless-checkpoint-skips-a-failed-snapshot ()
  (let* ((data (make-temp-file "harmless-cp-fail-" t))
         (project (make-temp-file "harmless-cp-fail-proj-" t))
         (harmless-directory data)
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-checkpoints t)
         (session nil)
         (orig (symbol-function 'call-process)))
    (unwind-protect
        (progn
          (setq session (harmless-checkpoint-test-session data project))
          (harmless-checkpoint-begin-turn session)
          (cl-letf (((symbol-function 'call-process)
                     (lambda (&rest args)
                       (if (equal (car args) "git")
                           1
                         (apply orig args)))))
            (should (string-prefix-p
                     "Wrote "
                     (harmless-tools-fs--write
                      session '(:path "a.txt" :contents "kept\n")))))
          (should (equal "kept\n"
                         (harmless-checkpoint-test-logical
                          (expand-file-name "a.txt" project)))))
      (delete-directory data t)
      (delete-directory project t))))

(ert-deftest harmless-checkpoint-skips-a-huge-file-and-a-huge-tree ()
  (let* ((data (make-temp-file "harmless-cp-huge-" t))
         (project (make-temp-file "harmless-cp-huge-proj-" t))
         (harmless-directory data)
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-checkpoints t)
         (harmless-checkpoint-max-bytes 4)
         (session nil))
    (unwind-protect
        (progn
          (harmless-checkpoint-test-write
           (expand-file-name "small.txt" project) "ab\n")
          (harmless-checkpoint-test-write
           (expand-file-name "big.txt" project) "12345")
          (setq session (harmless-checkpoint-test-session data project))
          (harmless-checkpoint-begin-turn session)
          (harmless-tools-fs--write session '(:path "c.txt" :contents "c\n"))
          (let* ((work (harmless-session-cwd session))
                 (git-dir (expand-file-name ".git"
                                            (harmless-checkpoint--store work)))
                 (listed (harmless-checkpoint--git git-dir work "ls-files")))
            (should (string-search "small.txt" listed))
            (should-not (string-search "big.txt" listed))
            (should-not (string-search "c.txt" listed)))
          (let ((harmless-checkpoint-max-files 1)
                (again (make-temp-file "harmless-cp-many-" t)))
            (unwind-protect
                (let ((session (harmless-checkpoint-test-session data again)))
                  (harmless-checkpoint-test-write
                   (expand-file-name "one.txt" again) "1\n")
                  (harmless-checkpoint-test-write
                   (expand-file-name "two.txt" again) "2\n")
                  (harmless-checkpoint-begin-turn session)
                  (should (string-prefix-p
                           "Wrote "
                           (harmless-tools-fs--write
                            session '(:path "three.txt" :contents "3\n"))))
                  (should (equal "3\n"
                                 (harmless-checkpoint-test-logical
                                  (expand-file-name "three.txt" again))))
                  (should-not (file-exists-p
                               (expand-file-name
                                ".git/HEAD"
                                (harmless-checkpoint--store again)))))
              (delete-directory again t))))
      (delete-directory data t)
      (delete-directory project t))))

(ert-deftest harmless-checkpoint-refuses-a-bad-request ()
  (let* ((data (make-temp-file "harmless-cp-bad-" t))
         (project (make-temp-file "harmless-cp-bad-proj-" t))
         (rel-root (file-name-as-directory
                    (make-temp-file "harmless-cp-rel-" t)))
         (harmless-directory data)
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-checkpoints t)
         (session nil)
         (guards (cons default-directory
                       (mapcar (lambda (path) (cons path (file-exists-p path)))
                               (list "/checkpoints" "/sessions"
                                     "c:/checkpoints" "c:/sessions"
                                     "c:\\checkpoints" "C:/checkpoints")))))
    (unwind-protect
        (progn
          (harmless-checkpoint-test-write
           (expand-file-name "a.txt" project) "A\n")
          (setq session (harmless-checkpoint-test-session data project))
          (harmless-checkpoint-begin-turn session)
          (harmless-tools-fs--write session '(:path "a.txt" :contents "A2\n"))
          (dolist (blank (list nil "" "   "))
            (harmless-checkpoint-test-error
             "checkpoint id is empty"
             (lambda () (harmless-checkpoint-diff session blank))))
          (harmless-checkpoint-test-error
           "checkpoint id must be a string"
           (lambda () (harmless-checkpoint-restore session 42)))
          (dolist (bad '("nope" "cp-0" "cp-01" "cp-1/x" "../cp-1"))
            (harmless-checkpoint-test-error
             "no such checkpoint"
             (lambda () (harmless-checkpoint-diff session bad))))
          (harmless-checkpoint-test-error
           "no such checkpoint"
           (lambda () (harmless-checkpoint-restore session "cp-9")))
          (dolist (blank '("" "   "))
            (harmless-checkpoint-test-error
             "checkpoint path is empty"
             (lambda () (harmless-checkpoint-restore session "cp-1" blank))))
          (harmless-checkpoint-test-error
           "checkpoint path must be a string"
           (lambda () (harmless-checkpoint-diff session "cp-1" 42)))
          (dolist (root '("/" "c:/" "c:\\" "C:/" "\\" "//" "/."))
            (harmless-checkpoint-test-error
             "checkpoint path is the filesystem root"
             (lambda () (harmless-checkpoint-restore session "cp-1" root))))
          (harmless-checkpoint-test-error
           "checkpoint path escapes the project"
           (lambda () (harmless-checkpoint-restore session "cp-1" "../out.txt")))
          (harmless-checkpoint-test-error
           "No Harmless session"
           (lambda () (harmless-checkpoint-restore nil "cp-1")))
          (let ((harmless-directory data))
            (setf (harmless-session-plan-mode session) t)
            (harmless-checkpoint-test-error
             "Plan mode is on. Only plan.md may be edited, with harmless-tool-write-plan"
             (lambda () (harmless-checkpoint-restore session "cp-1")))
            (setf (harmless-session-plan-mode session) nil))
          (let* ((provider (harmless-make-openai-compat
                            "local" :host "127.0.0.1:9" :protocol "http"
                            :key "none" :models '("m")))
                 (chat (harmless-session-new :detached t
                                             :provider provider
                                             :model "m")))
            (harmless-checkpoint-test-error
             "This chat is not attached to a project"
             (lambda () (harmless-checkpoint-diff chat "cp-1"))))
          (let ((harmless-directory "/"))
            (harmless-checkpoint-test-error
             "checkpoint id is empty"
             (lambda () (harmless-checkpoint-diff session "   "))))
          (dolist (root '("/" "c:/" "c:\\" "C:/" "\\" "//" "/." "D:/" "d:\\"))
            (let ((harmless-directory root))
              (harmless-checkpoint-test-error
               "checkpoint path is the filesystem root"
               (lambda () (harmless-checkpoint-diff session "cp-1")))))
          (let ((default-directory rel-root)
                (harmless-directory "notes"))
            (harmless-checkpoint-test-error
             "Harmless data directory must be absolute, not \"notes\""
             (lambda () (harmless-checkpoint-diff session "cp-1")))
            (should-not (file-exists-p (expand-file-name "notes" rel-root)))
            (should (string-prefix-p
                     "Wrote "
                     (harmless-tools-fs--write
                      session '(:path "a.txt" :contents "still\n"))))
            (should-not (file-exists-p (expand-file-name "notes" rel-root))))
          (let ((harmless-directory "/"))
            (should (string-prefix-p
                     "Wrote "
                     (harmless-tools-fs--write
                      session '(:path "a.txt" :contents "root-dir\n")))))
          (let ((harmless-directory ""))
            (should (string-prefix-p
                     "Wrote "
                     (harmless-tools-fs--write
                      session '(:path "a.txt" :contents "empty-dir\n"))))))
      (let ((default-directory temporary-file-directory))
        (delete-directory data t)
        (delete-directory project t)
        (delete-directory rel-root t)))
    (let ((default-directory (car guards)))
      (dolist (cell (cdr guards))
        (should (equal (cdr cell) (file-exists-p (car cell))))))))

(ert-deftest harmless-checkpoint-skips-a-symlink-escape ()
  (let* ((data (make-temp-file "harmless-cp-link-data-" t))
         (project (make-temp-file "harmless-cp-link-proj-" t))
         (outside (make-temp-file "harmless-cp-link-out-" t))
         (harmless-directory data)
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-checkpoints t)
         (session nil)
         (link nil))
    (unwind-protect
        (progn
          (harmless-checkpoint-test-write
           (expand-file-name "a.txt" project) "A\n")
          (harmless-checkpoint-test-write
           (expand-file-name "secret.txt" outside) "SECRET-CHECKPOINT\n")
          (setq link (expand-file-name "leak.txt" project))
          (condition-case nil
              (make-symbolic-link
               (expand-file-name "secret.txt" outside) link)
            (file-error nil))
          (setq session (harmless-checkpoint-test-session data project))
          (harmless-checkpoint-begin-turn session)
          (should (string-prefix-p
                   "Wrote "
                   (harmless-tools-fs--write
                    session '(:path "b.txt" :contents "B\n"))))
          (when (file-symlink-p link)
            (let* ((work (harmless-session-cwd session))
                   (git-dir (expand-file-name
                             ".git" (harmless-checkpoint--store work)))
                   (listed (harmless-checkpoint--git
                            git-dir work "ls-files")))
              (should (string-search "a.txt" listed))
              (should-not (string-search "leak.txt" listed))
              (should (file-exists-p
                       (expand-file-name "secret.txt" outside))))))
      (let ((default-directory temporary-file-directory))
        (when (and link (file-symlink-p link))
          (delete-file link))
        (delete-directory data t)
        (delete-directory project t)
        (delete-directory outside t)))))

(ert-deftest harmless-checkpoint-shell-snapshots-a-write ()
  (let* ((data (make-temp-file "harmless-cp-sh-data-" t))
         (project (make-temp-file "harmless-cp-sh-proj-" t))
         (harmless-directory data)
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-checkpoints t)
         (session nil))
    (unwind-protect
        (progn
          (setq session (harmless-checkpoint-test-session data project))
          (harmless-checkpoint-test-write
           (expand-file-name "a.txt" project) "A\n")
          (harmless-checkpoint-begin-turn session)
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest _) (error "stop-after-snapshot"))))
            (harmless-checkpoint-test-error
             "stop-after-snapshot"
             (lambda ()
               (harmless-tools-shell--run
                session '(:command "echo hi") #'ignore)))
            (should-not (file-exists-p
                         (expand-file-name
                          ".git/HEAD"
                          (harmless-checkpoint--store project))))
            (harmless-checkpoint-test-error
             "stop-after-snapshot"
             (lambda ()
               (harmless-tools-shell--run
                session '(:command "echo hi > out.txt") #'ignore))))
          (should (equal '("cp-1") (harmless-checkpoint-test-subjects session))))
      (let ((default-directory temporary-file-directory))
        (delete-directory data t)
        (delete-directory project t)))))
