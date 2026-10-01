;;; harmless-tools-tests.el --- Tests for Harmless filesystem tools -*- lexical-binding: t; -*-

(require 'ert)
(require 'harmless-openai)
(require 'harmless-session)
(require 'harmless-tools)
(require 'harmless-tools-fs)
(require 'harmless-tools-shell)

(defun harmless-test--session (dir)
  "Make a session rooted at DIR."
  (let ((provider (harmless-make-openai-compat
                   "local" :host "127.0.0.1:9" :protocol "http"
                   :key "none" :models '("m")))
        (harmless-providers nil)
        (harmless--sessions (make-hash-table :test 'equal))
        (harmless-directory (expand-file-name ".harmless" dir)))
    (setq harmless-providers (list provider))
    (harmless-session-new :cwd dir :provider provider :model "m")))

(ert-deftest harmless-tools-path-sandbox ()
  (let* ((base (make-temp-file "harmless-sand-" t))
         (dir (expand-file-name "proj" base))
         (evil (expand-file-name "proj-evil" base)))
    (make-directory dir)
    (make-directory evil)
    (with-temp-file (expand-file-name "a.txt" evil) (insert "nope\n"))
    (let ((session (harmless-test--session dir)))
      (let ((err (should-error (harmless-tools-fs-resolve session "../passwd")
                               :type 'error)))
        (should (string-match-p "Path escapes project" (error-message-string err))))
      (let ((err (should-error (harmless-tools-fs-resolve session "/etc/passwd")
                               :type 'error)))
        (should (string-match-p "Path escapes project" (error-message-string err))))
      (should-not (harmless-tools-fs-inside-p
                   dir (expand-file-name "a.txt" evil)))
      (should (string= (file-truename (harmless-tools-fs-resolve session "a.txt"))
                       (file-truename (expand-file-name "a.txt" dir))))
      (should (string-match-p "Wrote"
                              (harmless-tools-fs--write
                               session '(:path "nested/dir/c.txt"
                                         :contents "deep\n"))))
      (should (file-exists-p (expand-file-name "nested/dir/c.txt" dir))))))

(ert-deftest harmless-tools-write-read-replace ()
  (let* ((dir (make-temp-file "harmless-proj-" t))
         (session (harmless-test--session dir)))
    (should (string-match-p "Wrote"
                            (harmless-tools-fs--write
                             session '(:path "a.txt" :contents "foo bar foo"))))
    (should (string= "foo bar foo"
                     (harmless-tools-fs--read session '(:path "a.txt"))))
    (should-error (harmless-tools-fs--replace
                   session '(:path "a.txt" :old_string "foo" :new_string "baz")))
    (should (string-match-p "Replaced 2"
                            (harmless-tools-fs--replace
                             session '(:path "a.txt" :old_string "foo"
                                       :new_string "baz" :replace_all t))))
    (should (string= "baz bar baz"
                     (harmless-tools-fs--read session '(:path "a.txt"))))))

(ert-deftest harmless-tools-grep-list ()
  (let* ((dir (make-temp-file "harmless-proj-" t))
         (session (harmless-test--session dir)))
    (harmless-tools-fs--write session '(:path "n.txt" :contents "needle here\n"))
    (harmless-tools-fs--write session '(:path "sub/x.txt" :contents "nothing\n"))
    (should (string-match-p "n.txt:1:needle here"
                            (harmless-tools-fs--grep session '(:pattern "needle"))))
    (should (string-match-p "n.txt" (harmless-tools-fs--list session '(:path "."))))))

(ert-deftest harmless-tools-shell-echo ()
  (let* ((dir (make-temp-file "harmless-proj-" t))
         (session (harmless-test--session dir))
         (done nil)
         result
         (n 0))
    (harmless-tools-shell--run session '(:command "echo hi")
                               (lambda (s)
                                 (setq result s done t)))
    (while (and (not done) (< n 50))
      (setq n (1+ n))
      (accept-process-output nil 0.1))
    (should done)
    (should (string-match-p "hi" result))))

(ert-deftest harmless-tools-missing-cwd-does-not-use-root ()
  (let ((default-directory "/")
        (session (harmless-session--create :id "abcdef0123456789" :cwd nil)))
    (let ((err (should-error (harmless-tools-fs-resolve session "a.txt")
                             :type 'error)))
      (should (string-match-p "must be absolute" (error-message-string err))))
    (let ((err (should-error
                (harmless-tools-shell--run session '(:command "pwd") #'ignore)
                :type 'error)))
      (should (string-match-p "must be absolute" (error-message-string err))))
    (should-not (file-exists-p "/a.txt"))))

(ert-deftest harmless-tools-do-not-search-filesystem-root ()
  (let* ((harmless-directory (make-temp-file "harmless-root-tools-" t))
         (harmless--sessions (make-hash-table :test 'equal))
         (provider (harmless-make-openai-compat
                    "local" :host "127.0.0.1:9" :protocol "http"
                    :key "none" :models '("m")))
         (session (harmless-session-new :cwd "/" :provider provider :model "m"))
         (scanned nil)
         (wrote nil))
    (cl-letf (((symbol-function 'directory-files-recursively)
               (lambda (&rest _) (setq scanned t) nil))
              ((symbol-function 'write-region)
               (lambda (_content _start file &rest _)
                 (setq wrote file))))
      (let ((err (should-error (harmless-tools-fs--glob session '(:pattern "*"))
                               :type 'error)))
        (should (string-match-p "Refusing to search the filesystem root"
                                (error-message-string err))))
      (let ((err (should-error (harmless-tools-fs--grep session '(:pattern "x"))
                               :type 'error)))
        (should (string-match-p "Refusing to search the filesystem root"
                                (error-message-string err))))
      (should-not scanned)
      (should (string-match-p "Wrote"
                              (harmless-tools-fs--write
                               session '(:path "readme" :contents "hi\n"))))
      (should (equal "/readme" wrote))
      (should-not (file-exists-p "/readme"))
      (should-not (file-exists-p "/a.txt")))))

(provide 'harmless-tools-tests)
