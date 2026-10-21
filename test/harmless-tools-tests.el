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
      (should (string-match-p "not attached to a project"
                              (error-message-string err))))
    (let ((err (should-error
                (harmless-tools-shell--run session '(:command "pwd") #'ignore)
                :type 'error)))
      (should (string-match-p "not attached to a project"
                              (error-message-string err))))
    (let ((scanned nil)
          (spawned nil))
      (cl-letf (((symbol-function 'directory-files-recursively)
                 (lambda (&rest _) (setq scanned t) nil))
                ((symbol-function 'call-process)
                 (lambda (&rest _) (setq spawned t) 1)))
        (let ((err (should-error (harmless-tools-fs--glob session '(:pattern "*"))
                                 :type 'error)))
          (should (string-match-p "not attached to a project"
                                  (error-message-string err))))
        (let ((err (should-error (harmless-tools-fs--grep session '(:pattern "x"))
                                 :type 'error)))
          (should (string-match-p "not attached to a project"
                                  (error-message-string err))))
        (should-not scanned)
        (should-not spawned)))
    (should-not (file-exists-p "/a.txt"))))

(ert-deftest harmless-tools-do-not-search-filesystem-root ()
  (let* ((harmless-directory (make-temp-file "harmless-root-tools-" t))
         (harmless--sessions (make-hash-table :test 'equal))
         (provider (harmless-make-openai-compat
                    "local" :host "127.0.0.1:9" :protocol "http"
                    :key "none" :models '("m")))
         (session (harmless-session-new :cwd "/" :provider provider :model "m"))
         (scanned nil)
         (spawned nil)
         (wrote nil))
    (cl-letf (((symbol-function 'directory-files-recursively)
               (lambda (&rest _) (setq scanned t) nil))
              ((symbol-function 'call-process)
               (lambda (&rest _) (setq spawned t) 1))
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
      (should-not spawned)
      (should (string-match-p "Wrote"
                              (harmless-tools-fs--write
                               session '(:path "readme" :contents "hi\n"))))
      (should (equal "/readme" wrote))
      (should-not (file-exists-p "/readme"))
      (should-not (file-exists-p "/a.txt")))))

(ert-deftest harmless-tools-grep-engine-choice ()
  (should (equal (harmless-tools-fs-grep-engine " RG ") "rg"))
  (should (equal (harmless-tools-fs-grep-engine "elisp") "elisp"))
  (let ((err (should-error (harmless-tools-fs-grep-engine "ack") :type 'error)))
    (should (string-match-p "Unknown grep engine: ack"
                            (error-message-string err))))
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd &rest _) (equal cmd "rg"))))
    (should (equal (harmless-tools-fs-grep-engine nil) "rg"))
    (should (equal (harmless-tools-fs-grep-engine "") "rg")))
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd &rest _) (equal cmd "ag"))))
    (should (equal (harmless-tools-fs-grep-engine nil) "ag")))
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
    (should (equal (harmless-tools-fs-grep-engine nil) "elisp"))))

(ert-deftest harmless-tools-grep-command-args ()
  (should (equal (harmless-tools-fs-rg-args "needle" "." 0 nil nil)
                 '("--line-number" "--no-heading" "--color" "never"
                   "--hidden" "--glob" "!.git/**"
                   "--" "needle" ".")))
  (should (equal (harmless-tools-fs-rg-args "needle" "sub" 2 "elisp" t)
                 '("--line-number" "--no-heading" "--color" "never"
                   "--hidden" "--glob" "!.git/**"
                   "--context" "2"
                   "--multiline" "--multiline-dotall"
                   "--type" "elisp"
                   "--" "needle" "sub")))
  (should (equal (harmless-tools-fs-ag-args "needle" "." 0 nil)
                 '("-s" "--nocolor" "--nogroup" "--numbers" "--filename"
                   "--hidden" "--ignore-dir" ".git"
                   "needle" ".")))
  (should (equal (harmless-tools-fs-ag-args "needle" "sub/" 1 "html")
                 '("-s" "--nocolor" "--nogroup" "--numbers" "--filename"
                   "--hidden" "--ignore-dir" ".git"
                   "-C" "1"
                   "--html"
                   "needle" "sub/"))))

(ert-deftest harmless-tools-grep-normalize-output ()
  (should (equal (harmless-tools-fs-grep-normalize
                  "./n.txt:2:needle here\n--\n./.hidden/h.txt:1:dot needle\n"
                  nil)
                 "n.txt:2:needle here\n--\n.hidden/h.txt:1:dot needle"))
  (should (equal (harmless-tools-fs-grep-normalize
                  "n.txt:1-alpha\nn.txt:2:needle here\nn.txt:3-omega\n"
                  nil)
                 "n.txt-1-alpha\nn.txt:2:needle here\nn.txt-3-omega"))
  (should (equal (harmless-tools-fs-grep-normalize
                  "a.txt:1:needle\nb.el:1:needle\n"
                  "*.txt")
                 "a.txt:1:needle"))
  (should (equal (harmless-tools-fs-grep-normalize "" nil) "")))

(ert-deftest harmless-tools-grep-parse-file-types ()
  (should (equal (harmless-tools-fs--parse-rg-types "elisp: *.el\ntxt: *.txt\n")
                 '("elisp" "txt")))
  (should (equal (harmless-tools-fs--parse-ag-types
                  "The following file types are supported:\n  --html\n      .html\n  --lisp\n      .lisp\n")
                 '("html" "lisp"))))

(defun harmless-tools-test-refuse (session args pattern)
  "Search SESSION with ARGS and require an error matching PATTERN."
  (let ((err (should-error (harmless-tools-fs--grep session args) :type 'error)))
    (should (string-match-p pattern (error-message-string err)))))

(ert-deftest harmless-tools-grep-refuses-bad-requests ()
  (let* ((dir (make-temp-file "harmless-grep-" t))
         (session (harmless-test--session dir))
         (spawned nil))
    (with-temp-file (expand-file-name "a.txt" dir) (insert "needle\n"))
    (with-temp-file (expand-file-name "top.txt" dir) (insert "needle\n"))
    (make-directory (expand-file-name "sub" dir))
    (harmless-tools-test-refuse session '(:engine "rg") "pattern is required")
    (harmless-tools-test-refuse session '(:pattern "" :engine "elisp")
                               "pattern is required")
    (harmless-tools-test-refuse session '(:pattern "n" :context -1 :engine "elisp")
                               "context must be a non-negative integer")
    (harmless-tools-test-refuse session '(:pattern "n" :context "1" :engine "elisp")
                               "context must be a non-negative integer")
    (harmless-tools-test-refuse session '(:pattern "n" :path "")
                               "Path is empty")
    (harmless-tools-test-refuse session '(:pattern "n" :path "../outside")
                               "Path escapes project")
    (harmless-tools-test-refuse session '(:pattern "n" :path "top.txt" :engine "elisp")
                               "Not a directory")
    (harmless-tools-test-refuse session '(:pattern "n" :engine "nope")
                               "Unknown grep engine: nope")
    (harmless-tools-test-refuse session '(:pattern "a" :engine "ag" :multiline t)
                               "ag does not support multiline search")
    (harmless-tools-test-refuse session '(:pattern "-n" :engine "ag")
                               "ag patterns cannot start with -")
    (harmless-tools-test-refuse session '(:pattern "a" :engine "elisp" :multiline t)
                               "multiline search requires ripgrep")
    (harmless-tools-test-refuse session '(:pattern "a" :engine "elisp" :type "elisp")
                               "file types require ripgrep or ag")
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil))
              ((symbol-function 'call-process)
               (lambda (&rest _) (setq spawned t) 1)))
      (harmless-tools-test-refuse session '(:pattern "a" :engine "rg")
                                 "ripgrep is not installed")
      (harmless-tools-test-refuse session '(:pattern "a" :engine "ag")
                                 "ag is not installed")
      (should-not spawned))
    (should (string-match-p "No matches"
                            (harmless-tools-fs--grep
                             session '(:pattern "Needle" :engine "elisp"))))))

(ert-deftest harmless-tools-grep-elisp-context-glob-and-truncation ()
  (let* ((dir (make-temp-file "harmless-grep-elisp-" t))
         (session (harmless-test--session (file-name-as-directory dir))))
    (with-temp-file (expand-file-name "n.txt" dir)
      (insert "alpha\nneedle one\nomega\ngap\ngap\nneedle two\ntail\n"))
    (make-directory (expand-file-name "sub" dir))
    (with-temp-file (expand-file-name "sub/c.txt" dir) (insert "needle sub\n"))
    (with-temp-file (expand-file-name "b.el" dir) (insert "needle el\n"))
    (should (equal (harmless-tools-fs--grep
                    session '(:pattern "needle" :engine "elisp" :context 1
                              :glob "n.txt"))
                   (concat "n.txt-1-alpha\n"
                           "n.txt:2:needle one\n"
                           "n.txt-3-omega\n"
                           "--\n"
                           "n.txt-5-gap\n"
                           "n.txt:6:needle two\n"
                           "n.txt-7-tail")))
    (should (equal (harmless-tools-fs--grep
                    session '(:pattern "needle" :engine "elisp" :glob "*.txt"
                              :path "sub/"))
                   "sub/c.txt:1:needle sub"))
    (let ((harmless-read-file-max-bytes 8))
      (should (string-suffix-p
               "[truncated]"
               (harmless-tools-fs--grep
                session '(:pattern "needle" :engine "elisp")))))))

(ert-deftest harmless-tools-grep-rg-and-ag ()
  ;; Missing programs already signal in harmless-tools-grep-refuses-bad-requests.
  (skip-unless (and (executable-find "rg") (executable-find "ag")))
  (let* ((dir (make-temp-file "harmless-grep-ext-" t))
         (session (harmless-test--session dir))
         (context "n.txt-1-alpha\nn.txt:2:needle here\nn.txt-3-omega"))
    (with-temp-file (expand-file-name "n.txt" dir)
      (insert "alpha\nneedle here\nomega\n"))
    (with-temp-file (expand-file-name "b.el" dir) (insert "needle el\n"))
    (with-temp-file (expand-file-name "a.html" dir) (insert "needle html\n"))
    (with-temp-file (expand-file-name ".hidden.txt" dir) (insert "needle hidden\n"))
    (make-directory (expand-file-name "sub" dir))
    (with-temp-file (expand-file-name "sub/keep.txt" dir) (insert "needle sub\n"))
    (make-directory (expand-file-name "multi" dir))
    (with-temp-file (expand-file-name "multi/span.txt" dir)
      (insert "before\nstart middle\nend after\n"))
    (let ((default-directory dir))
      (should (eq 0 (call-process "git" nil nil nil "init" "-q"))))
    (with-temp-file (expand-file-name ".gitignore" dir) (insert "skip.txt\n"))
    (with-temp-file (expand-file-name "skip.txt" dir) (insert "needle ignored\n"))
    (with-temp-file (expand-file-name ".git/secret.txt" dir)
      (insert "needle git\n"))
    (dolist (engine '("rg" "ag"))
      (let ((found (harmless-tools-fs--grep
                    session (list :pattern "needle" :engine engine))))
        (should (string-match-p "n.txt:2:needle here" found))
        (should (string-match-p "\\.hidden.txt:1:needle hidden" found))
        (should (string-match-p "sub/keep.txt:1:needle sub" found))
        (should-not (string-match-p "needle ignored" found))
        (should-not (string-match-p "needle git" found)))
      (should (equal (harmless-tools-fs--grep
                      session (list :pattern "needle here" :engine engine
                                    :context 1 :glob "n.txt"))
                     context))
      (should (equal (harmless-tools-fs--grep
                      session (list :pattern "needle" :engine engine
                                    :glob "*.txt" :path "sub/"))
                     "sub/keep.txt:1:needle sub"))
      (should (equal (harmless-tools-fs--grep
                      session (list :pattern "Needle" :engine engine))
                     "No matches")))
    (should (string-match-p "a.html:1:needle html"
                            (harmless-tools-fs--grep
                             session '(:pattern "needle" :engine "ag" :type "html"))))
    (should-not (string-match-p "b.el"
                                (harmless-tools-fs--grep
                                 session '(:pattern "needle" :engine "ag" :type "html"))))
    (should (string-match-p "b.el:1:needle el"
                            (harmless-tools-fs--grep
                             session '(:pattern "needle" :engine "rg" :type "elisp"))))
    (should-not (string-match-p "a.html"
                                (harmless-tools-fs--grep
                                 session '(:pattern "needle" :engine "rg" :type "elisp"))))
    (harmless-tools-test-refuse session '(:pattern "needle" :engine "rg" :type "notatype")
                               "Unknown file type for rg: notatype")
    (harmless-tools-test-refuse session '(:pattern "needle" :engine "ag" :type "notatype")
                               "Unknown file type for ag: notatype")
    (let ((span (harmless-tools-fs--grep
                 session '(:pattern "start.*end" :engine "rg" :multiline t
                           :path "multi"))))
      (should (string-match-p "multi/span.txt:2:start middle" span))
      (should (string-match-p "multi/span.txt:3:end after" span)))
    (should (equal (harmless-tools-fs--grep
                    session '(:pattern "start.*end" :engine "rg" :multiline :false
                              :path "multi"))
                   "No matches"))
    (let ((err (should-error
                (harmless-tools-fs--grep session '(:pattern "(" :engine "rg"))
                :type 'error)))
      (should (string-match-p "unclosed group" (error-message-string err))))
    (let ((err (should-error
                (harmless-tools-fs--grep session '(:pattern "(" :engine "ag"))
                :type 'error)))
      (should (string-match-p "missing closing parenthesis"
                              (error-message-string err))))
    (let ((elisp (harmless-tools-fs--grep
                  session '(:pattern "needle" :engine "elisp"))))
      (should (string-match-p "skip.txt:1:needle ignored" elisp))
      (should (string-match-p "\\.git/secret.txt:1:needle git" elisp)))))

(defun harmless-tools-patch-error (session args)
  "Return the error string from apply_patch on SESSION with ARGS."
  (error-message-string
   (should-error (harmless-tools-fs--apply-patch session args) :type 'error)))

(defun harmless-tools-test-abs (session path)
  "Return PATH expanded against SESSION's project, as a file tool would."
  (expand-file-name path (harmless-session-require-project session)))

(defun harmless-tools-test-contents (path)
  "Return the contents of PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(ert-deftest harmless-tools-apply-patch ()
  (let* ((dir (make-temp-file "harmless-patch-" t))
         (session (harmless-test--session dir))
         (root (harmless-session-require-project session))
         (a (harmless-tools-test-abs session "a.txt"))
         (b (harmless-tools-test-abs session "b.txt"))
         (seq (harmless-tools-test-abs session "seq.txt"))
         (del (harmless-tools-test-abs session "del.txt"))
         (nested (harmless-tools-test-abs session "sub/c.txt"))
         (tool (harmless-tool-by-name "apply_patch")))
    (should tool)
    (should (eq 'edit (harmless-tool-class tool)))
    (make-directory (expand-file-name "sub" root))
    (with-temp-file a (insert "one"))
    (with-temp-file b (insert "two"))
    (with-temp-file seq (insert "alpha beta"))
    (with-temp-file del (insert "hello world"))
    (with-temp-file nested (insert "old"))
    (let ((args (harmless-json-decode
                 "{\"hunks\":[{\"path\":\"a.txt\",\"old_string\":\"one\",\"new_string\":\"1\"},{\"path\":\"b.txt\",\"old_string\":\"two\",\"new_string\":\"2\"}]}")))
      (should (equal (format "Applied 2 hunks in %s (1), %s (1)" a b)
                     (harmless-tools-fs--apply-patch session args))))
    (should (equal "1" (harmless-tools-test-contents a)))
    (should (equal "2" (harmless-tools-test-contents b)))
    (should (equal "1" (harmless-tools-fs--read session '(:path "a.txt"))))
    (should (equal "2" (harmless-tools-fs--read session '(:path "b.txt"))))
    (should (equal (format "Applied 2 hunks in %s (2)" seq)
                   (harmless-tools-fs--apply-patch
                    session '(:hunks ((:path "seq.txt"
                                       :old_string "alpha"
                                       :new_string "ALPHA")
                                      (:path "./seq.txt"
                                       :old_string "ALPHA beta"
                                       :new_string "done"))))))
    (should (equal "done" (harmless-tools-test-contents seq)))
    (should (equal (format "Applied 1 hunk in %s (1)" del)
                   (harmless-tools-fs--apply-patch
                    session '(:hunks [(:path "del.txt"
                                       :old_string " world"
                                       :new_string nil)]))))
    (should (equal "hello" (harmless-tools-test-contents del)))
    (should (equal (format "Applied 1 hunk in %s (1)" nested)
                   (harmless-tools-fs--apply-patch
                    session '(:hunks ((:path "sub/c.txt"
                                       :old_string "old"
                                       :new_string "new"))))))
    (should (equal "new" (harmless-tools-test-contents nested)))
    (should (equal "new"
                   (harmless-tools-fs--read session '(:path "sub/c.txt"))))))

(ert-deftest harmless-tools-apply-patch-refuses ()
  (let* ((dir (make-temp-file "harmless-patch-no-" t))
         (session (harmless-test--session dir))
         (a (harmless-tools-test-abs session "a.txt"))
         (b (harmless-tools-test-abs session "b.txt"))
         (same (harmless-tools-test-abs session "same.txt"))
         (dup (harmless-tools-test-abs session "dup.txt"))
         (outside (expand-file-name "../harmless-escape" dir))
         (root-before (file-exists-p "/a.txt")))
    (make-directory (harmless-tools-test-abs session "sub"))
    (with-temp-file a (insert "alpha"))
    (with-temp-file b (insert "beta"))
    (with-temp-file same (insert "alpha beta"))
    (with-temp-file dup (insert "foo foo"))
    (should (equal (format "hunk 2: old_string not found in %s" b)
                   (harmless-tools-patch-error
                    session '(:hunks ((:path "a.txt"
                                       :old_string "alpha"
                                       :new_string "ALPHA")
                                      (:path "b.txt"
                                       :old_string "missing"
                                       :new_string "nope"))))))
    (should (equal "alpha" (harmless-tools-test-contents a)))
    (should (equal "beta" (harmless-tools-test-contents b)))
    (should (equal (format "hunk 2: old_string not found in %s" same)
                   (harmless-tools-patch-error
                    session '(:hunks ((:path "same.txt"
                                       :old_string "alpha"
                                       :new_string "ALPHA")
                                      (:path "same.txt"
                                       :old_string "missing"
                                       :new_string "nope"))))))
    (should (equal "alpha beta" (harmless-tools-test-contents same)))
    (should (equal (format "hunk 2: Path escapes project: %s" "../harmless-escape")
                   (harmless-tools-patch-error
                    session '(:hunks ((:path "a.txt"
                                       :old_string "alpha"
                                       :new_string "ALPHA")
                                      (:path "../harmless-escape"
                                       :old_string "x"
                                       :new_string "y"))))))
    (should (equal "alpha" (harmless-tools-test-contents a)))
    (should-not (file-exists-p outside))
    (should (equal (format "hunk 1: old_string matched 2 times in %s; a hunk needs a unique string"
                           dup)
                   (harmless-tools-patch-error
                    session '(:hunks ((:path "dup.txt"
                                       :old_string "foo"
                                       :new_string "bar"))))))
    (should (equal "foo foo" (harmless-tools-test-contents dup)))
    (dolist (old (list nil ""))
      (should (equal "hunk 1: old_string is empty"
                     (harmless-tools-patch-error
                      session (list :hunks
                                    (list (list :path "a.txt"
                                                :old_string old
                                                :new_string "x")))))))
    (should (equal "hunk 1: new_string is not a string"
                   (harmless-tools-patch-error
                    session '(:hunks ((:path "a.txt"
                                       :old_string "alpha"
                                       :new_string 1))))))
    (dolist (args (list nil
                        '(:hunks nil)
                        '(:hunks ())
                        '(:hunks "")
                        '(:hunks [])))
      (should (equal "hunks is empty"
                     (harmless-tools-patch-error session args))))
    (should (equal "hunk 1 is not an object"
                   (harmless-tools-patch-error
                    session '(:hunks ("nope")))))
    (should (equal "hunk 1 is not an object"
                   (harmless-tools-patch-error
                    session '(:hunks ["nope"]))))
    (dolist (path (list nil ""))
      (should (equal "hunk 1: Path is empty"
                     (harmless-tools-patch-error
                      session (list :hunks
                                    (list (list :path path
                                                :old_string "alpha"
                                                :new_string "x")))))))
    (should (equal "hunk 1: Path escapes project: ../x"
                   (harmless-tools-patch-error
                    session '(:hunks ((:path "../x"
                                       :old_string "a"
                                       :new_string "b"))))))
    (should (equal "hunk 1: Path escapes project: /"
                   (harmless-tools-patch-error
                    session '(:hunks ((:path "/"
                                       :old_string "a"
                                       :new_string "b"))))))
    (should (equal (format "hunk 1: Cannot read %s"
                           (harmless-tools-test-abs session "sub/"))
                   (harmless-tools-patch-error
                    session '(:hunks ((:path "sub/"
                                       :old_string "a"
                                       :new_string "b"))))))
    (should (equal (format "hunk 1: Cannot read %s"
                           (harmless-tools-test-abs session "a.txt/"))
                   (harmless-tools-patch-error
                    session '(:hunks ((:path "a.txt/"
                                       :old_string "alpha"
                                       :new_string "x"))))))
    (should (equal (format "hunk 1: Cannot read %s"
                           (harmless-tools-test-abs session "missing.txt"))
                   (harmless-tools-patch-error
                    session '(:hunks ((:path "missing.txt"
                                       :old_string "a"
                                       :new_string "b"))))))
    (should-not (file-exists-p (harmless-tools-test-abs session "missing.txt")))
    (should (equal "alpha" (harmless-tools-test-contents a)))
    (should (equal "beta" (harmless-tools-test-contents b)))
    (should (eq root-before (file-exists-p "/a.txt")))))

(ert-deftest harmless-tools-apply-patch-needs-a-project ()
  (let ((default-directory "/")
        (root-before (file-exists-p "/a.txt"))
        (hunks '(:hunks ((:path "a.txt" :old_string "a" :new_string "b")))))
    (should (equal "This chat is not attached to a project"
                   (harmless-tools-patch-error
                    (harmless-session--create :id "abcdef0123456789" :cwd nil)
                    hunks)))
    (should (equal "Directory must be absolute, not \"\""
                   (harmless-tools-patch-error
                    (harmless-session--create :id "abcdef0123456789" :cwd "")
                    hunks)))
    (should (equal "Directory must be absolute, not \"proj\""
                   (harmless-tools-patch-error
                    (harmless-session--create :id "abcdef0123456789" :cwd "proj")
                    hunks)))
    (let* ((parent (make-temp-file "harmless-patch-miss-" t))
           (missing (expand-file-name "gone" parent))
           (session (harmless-session--create
                     :id "abcdef0123456789" :cwd missing))
           (target (harmless-tools-test-abs session "a.txt")))
      (should-not (file-directory-p missing))
      (should (equal (format "hunk 1: Cannot read %s" target)
                     (harmless-tools-patch-error session hunks)))
      (should-not (file-exists-p target))
      (should-not (file-directory-p missing)))
    (should (eq root-before (file-exists-p "/a.txt")))))

(defun harmless-shell-test-delay (seconds)
  "Return a command that waits about SECONDS in the current shell."
  (let ((name (downcase (file-name-nondirectory (or shell-file-name "")))))
    (cond
     ((string-match-p "powershell\\|pwsh" name)
      (format "Start-Sleep -Seconds %d" seconds))
     ((string-match-p "cmd" name)
      (format "ping -n %d 127.0.0.1 > NUL" (1+ seconds)))
     (t (format "sleep %d" seconds)))))

(defun harmless-shell-test-wait (predicate)
  "Return non-nil when PREDICATE becomes true within about eight seconds."
  (let ((n 0))
    (while (and (not (funcall predicate)) (< n 80))
      (setq n (1+ n))
      (accept-process-output nil 0.1))
    (funcall predicate)))

(defun harmless-shell-test-stop (session)
  "Kill any shell processes SESSION still has."
  (dolist (job (copy-sequence (harmless-session-shell-jobs session)))
    (let ((proc (harmless-shell-job-proc job)))
      (when (and (processp proc) (process-live-p proc))
        (ignore-errors (delete-process proc)))))
  (harmless-shell-test-wait
   (lambda ()
     (not (cl-some (lambda (job)
                     (let ((proc (harmless-shell-job-proc job)))
                       (and (processp proc) (process-live-p proc))))
                   (harmless-session-shell-jobs session))))))

(defun harmless-shell-test--call (session args)
  "Run the shell tool for ARGS and return the callback text."
  (let (result done)
    (harmless-tools-shell--run
     session args
     (lambda (text)
       (setq result text done t)))
    (should (harmless-shell-test-wait (lambda () done)))
    result))

(ert-deftest harmless-shell-refuses-a-bad-command-or-timeout ()
  (let* ((dir (make-temp-file "harmless-shell-" t))
         (session (harmless-test--session dir))
         (default-directory "/")
         (detached (harmless-session--create :id "abcdef0123456789" :cwd nil)))
    (should (equal "Error: command is empty"
                   (harmless-shell-test--call session '(:command ""))))
    (should (equal "Error: command is empty"
                   (harmless-shell-test--call session '(:command nil))))
    (should (equal "Error: command is empty"
                   (harmless-shell-test--call session nil)))
    (dolist (timeout (list 0 -1 0.0 "1" :false nil))
      (should (equal "Error: timeout must be a positive number"
                     (harmless-shell-test--call
                      session (list :command "echo hi" :timeout timeout))))
      (should (equal "Error: timeout must be a positive number"
                     (harmless-shell-test--call
                      session (list "command" "echo hi" "timeout" timeout)))))
    (should-not (harmless-session-shell-jobs session))
    (let ((err (should-error
                (harmless-tools-shell--run
                 detached '(:command "echo hi" :background t :timeout -1)
                 #'ignore)
                :type 'error)))
      (should (string-match-p "not attached to a project"
                              (error-message-string err))))
    (should-not (harmless-session-shell-jobs detached))
    (should-not (file-exists-p "/echo"))
    (should (eq 'read (harmless-tool-class (harmless-tool-by-name "shell_output"))))
    (should (eq 'shell (harmless-tool-class (harmless-tool-by-name "shell_kill"))))
    (let ((asked nil)
          (harmless-perm-ask-function (lambda (&rest _) (setq asked t))))
      (harmless-perm-confirm session 'read "shell_output" #'ignore)
      (should-not asked))))

(ert-deftest harmless-shell-background-is-remembered-only-in-memory ()
  (let* ((dir (make-temp-file "harmless-shell-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (provider (harmless-make-openai-compat
                    "local" :host "127.0.0.1:9" :protocol "http"
                    :key "none" :models '("m")))
         (harmless-providers (list provider))
         (session (harmless-session-new :cwd dir :provider provider :model "m"))
         one two)
    (unwind-protect
        (progn
          (setq one (harmless-shell-test--call
                     session '(:command "echo one" :background t)))
          (setq two (harmless-shell-test--call
                     session '(:command "echo two" :background :false)))
          (should (string-match-p "\\`background 1\n" one))
          (should (harmless-shell-test-wait
                   (lambda ()
                     (not (eq (harmless-shell-job-status
                               (harmless-shell-find session "1"))
                              'running)))))
          (should (string-match-p "exit 0" two))
          (should (string-match-p "two" two))
          (should-not (string-match-p "background" two))
          (let (output)
            (harmless-tools-shell--output
             session '(:id "1")
             (lambda (text) (setq output text)))
            (should (string-match-p "shell 1 exited 0" output))
            (should (string-match-p "one" output)))
          (should (equal "Error: shell id is empty"
                         (harmless-tools-shell--kill session '(:id ""))))
          (should (equal "Error: shell id is empty"
                         (harmless-tools-shell--kill session '(:id "  "))))
          (should (equal "Error: shell id is empty"
                         (harmless-tools-shell--kill session '(:id nil))))
          (should (equal "Error: no background shell missing"
                         (harmless-tools-shell--kill session '(:id "missing"))))
          (should (equal "Error: shell 1 is not running"
                         (harmless-tools-shell--kill session '(:id "1"))))
          (should (equal "Error: shell 1 is not running"
                         (harmless-tools-shell--kill session '(:id 1))))
          (let (output)
            (harmless-tools-shell--output
             session '(:id "" :timeout -1)
             (lambda (text) (setq output text)))
            (should (equal "Error: shell id is empty" output))
            (harmless-tools-shell--output
             session '(:id "missing")
             (lambda (text) (setq output text)))
            (should (equal "Error: no background shell missing" output))
            (harmless-tools-shell--output
             session '(:id "1" :timeout nil)
             (lambda (text) (setq output text)))
            (should (equal "Error: timeout must be a positive number" output))
            (harmless-tools-shell--output
             session '("id" "1" "timeout" "soon")
             (lambda (text) (setq output text)))
            (should (equal "Error: timeout must be a positive number" output)))
          (harmless-session-save session)
          (let ((summary (expand-file-name
                          "summary.json" (harmless-session-directory session)))
                (messages (expand-file-name
                           "messages.jsonl" (harmless-session-directory session))))
            (should (file-name-absolute-p summary))
            (should-not (harmless-filesystem-root-p
                         (file-name-directory summary)))
            (should (string-prefix-p
                     (file-name-as-directory harmless-directory) summary))
            (with-temp-buffer
              (insert-file-contents summary)
              (insert-file-contents messages)
              (should-not (string-search "echo one" (buffer-string)))
              (should-not (string-search "shell-jobs" (buffer-string)))))
          (remhash (harmless-session-id session) harmless--sessions)
          (let ((loaded (harmless-session-resume (harmless-session-id session))))
            (should loaded)
            (should-not (harmless-session-shell-jobs loaded))
            (should-not (harmless-session-shell-seq loaded))))
      (harmless-shell-test-stop session))))

(ert-deftest harmless-shell-timeout-leaves-the-process-running ()
  (let* ((dir (make-temp-file "harmless-shell-" t))
         (session (harmless-test--session dir))
         (events nil)
         (notes nil)
         (hook (lambda (_session event) (push event events)))
         (command (harmless-shell-test-delay 4)))
    (add-hook 'harmless-event-functions hook)
    (unwind-protect
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) notes))))
          (let ((result (harmless-shell-test--call
                         session (list :command command :timeout 0.3)))
                (proc nil))
            (should (string-match-p "\\`background 1\n" result))
            (should-not (string-match-p "exit " result))
            (setq proc (harmless-shell-job-proc
                        (harmless-shell-find session "1")))
            (should (process-live-p proc))
            (should-not (memq proc (harmless-session-processes session)))
            (let (output)
              (harmless-tools-shell--output
               session '(:id "1")
               (lambda (text) (setq output text)))
              (should (string-match-p "shell 1 running" output)))
            (let (output done)
              (harmless-tools-shell--output
               session '(:id "1" :timeout 8)
               (lambda (text) (setq output text done t)))
              (should (harmless-shell-test-wait (lambda () done)))
              (should (string-match-p "shell 1 exited 0" output)))
            (should (harmless-shell-test-wait
                     (lambda ()
                       (cl-find-if (lambda (event)
                                     (equal event (list :background "1"
                                                        "exited 0" command)))
                                   events))))
            (should (member "Harmless: background shell 1 exited 0" notes))
            (should (equal "Error: shell 1 is not running"
                           (harmless-tools-shell--kill session '(:id "1"))))))
      (remove-hook 'harmless-event-functions hook)
      (harmless-shell-test-stop session))))

(ert-deftest harmless-shell-abort-kills-foreground-only ()
  (let* ((dir (make-temp-file "harmless-shell-" t))
         (session (harmless-test--session dir))
         (foreground (harmless-shell-test-delay 15))
         (background (harmless-shell-test-delay 15)))
    (unwind-protect
        (progn
          (harmless-tools-shell--run
           session (list :command foreground :timeout 30) #'ignore)
          (should (harmless-shell-test-wait
                   (lambda ()
                     (let ((proc (car (harmless-session-processes session))))
                       (and proc (process-live-p proc))))))
          (let ((proc (car (harmless-session-processes session))))
            (harmless-turn-abort session)
            (should (harmless-shell-test-wait
                     (lambda () (not (process-live-p proc)))))
            (should-not (harmless-session-processes session)))
          (harmless-tools-shell--run
           session (list :command background :background t :timeout 30)
           #'ignore)
          (should (harmless-shell-test-wait
                   (lambda ()
                     (let ((job (harmless-shell-find session "2")))
                       (and job (process-live-p (harmless-shell-job-proc job)))))))
          (let ((proc (harmless-shell-job-proc
                       (harmless-shell-find session "2"))))
            (should-not (memq proc (harmless-session-processes session)))
            (harmless-turn-abort session)
            (should (process-live-p proc))
            (should (equal "killed shell 2"
                           (harmless-tools-shell--kill session '(:id "2"))))
            (should (harmless-shell-test-wait
                     (lambda () (not (process-live-p proc)))))))
      (harmless-shell-test-stop session))))

(ert-deftest harmless-shell-output-keeps-the-tail ()
  (let ((harmless-shell-output-max-bytes 4))
    (should (equal "" (harmless-shell--for-model nil)))
    (should (equal "" (harmless-shell--for-model "")))
    (should (equal "abcd\n[truncated]"
                   (harmless-shell--for-model "abcdef"))))
  (let ((harmless-shell-output-max-bytes 48)
        (buf (generate-new-buffer " *harmless-shell-cap*")))
    (unwind-protect
        (progn
          (harmless-shell--append-output buf (make-string 80 ?a))
          (harmless-shell--append-output buf "XYZ")
          (with-current-buffer buf
            (should (string-prefix-p "[earlier output discarded]\n"
                                     (buffer-string)))
            (should (string-suffix-p "XYZ" (buffer-string)))
            (should (< (buffer-size) 80))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest harmless-shell-kind-and-cwd-marker ()
  (should (eq 'cmd (harmless-shell-kind "C:/Windows/System32/cmd.exe")))
  (should (eq 'cmd (harmless-shell-kind "cmdproxy.exe")))
  (should (eq 'powershell (harmless-shell-kind "powershell.exe")))
  (should (eq 'powershell (harmless-shell-kind "pwsh")))
  (should (eq 'posix (harmless-shell-kind "/bin/bash")))
  (should (eq 'posix (harmless-shell-kind "sh")))
  (should (eq (harmless-shell-kind) (harmless-shell-kind shell-file-name)))
  (let ((split (harmless-shell--split-cwd
                "__HARMLESS_CWD__fake\nreal\n__HARMLESS_CWD__c:/ok \r\n")))
    (should (equal "c:/ok" (cdr split)))
    (should (string-search "fake" (car split)))
    (should (string-search "real" (car split)))
    (should-not (string-search "c:/ok" (car split))))
  (let ((split (harmless-shell--split-cwd "")))
    (should (equal "" (car split)))
    (should-not (cdr split)))
  (let ((cmd (harmless-shell--wrap 'cmd "c:/proj" "echo hi"))
        (ps (harmless-shell--wrap 'powershell "c:/proj" "echo hi"))
        (sh (harmless-shell--wrap 'posix "c:/proj" "echo hi")))
    (should (string-search "__HARMLESS_CWD__" cmd))
    (should (string-search "cd /d" cmd))
    (should (string-search "echo hi" cmd))
    (should (string-search "Set-Location" ps))
    (should (string-search "__HARMLESS_CWD__" ps))
    (should (string-search "printf" sh))
    (should (string-search "__HARMLESS_CWD__" sh))
    (should (equal (list "cmd.exe" "/v:on" "/c" "echo hi")
                   (let ((shell-file-name "cmd.exe"))
                     (harmless-shell--argv 'cmd "echo hi"))))
    (should (equal (list "bash" "-c" "echo hi")
                   (let ((shell-file-name "bash")
                         (shell-command-switch "-c"))
                     (harmless-shell--argv 'posix "echo hi"))))))

(ert-deftest harmless-shell-remembers-a-cd-inside-the-project ()
  (let* ((dir (make-temp-file "harmless-cd-" t))
         (sub (expand-file-name "sub" dir))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (provider (harmless-make-openai-compat
                    "local" :host "127.0.0.1:9" :protocol "http"
                    :key "none" :models '("m")))
         (harmless-providers (list provider))
         (session (harmless-session-new :cwd dir :provider provider :model "m"))
         (root (directory-file-name (file-truename dir))))
    (make-directory sub)
    (unwind-protect
        (progn
          (should-not (harmless-session-shell-cwd session))
          (dolist (bad (list nil "" "sub" "/" "c:/"
                             (expand-file-name "gone" dir)))
            (setf (harmless-session-shell-cwd session) bad)
            (let ((out (harmless-shell-test--call session '(:command "echo hi"))))
              (should (string-match-p "hi" out))
              (should-not (string-search "__HARMLESS_CWD__" out)))
            (should-not (harmless-session-shell-cwd session)))
          (let ((moved (harmless-shell-test--call
                        session '(:command "cd sub/"))))
            (should-not (string-search "__HARMLESS_CWD__" moved))
            (should (harmless-same-directory-p
                     sub (harmless-session-shell-cwd session))))
          (let ((again (harmless-shell-test--call session '(:command "cd"))))
            (should (string-match-p "sub" again))
            (should-not (string-search "__HARMLESS_CWD__" again)))
          (let ((back (harmless-shell-test--call session '(:command "cd .."))))
            (should-not (string-search "left the project" back))
            (should-not (harmless-session-shell-cwd session)))
          (let ((left (harmless-shell-test--call session '(:command "cd .."))))
            (should (string-search
                     (format "The shell left the project. The next command starts in %s."
                             root)
                     left))
            (should-not (harmless-session-shell-cwd session)))
          (let ((still (harmless-shell-test--call session '(:command "cd"))))
            (should (string-search (file-name-nondirectory root) still))
            (should-not (harmless-session-shell-cwd session)))
          (harmless-shell-test--call session '(:command "cd sub"))
          (harmless-session-save session)
          (let ((summary (expand-file-name
                          "summary.json"
                          (harmless-session-directory session))))
            (should (file-name-absolute-p summary))
            (should-not (harmless-filesystem-root-p (file-name-directory summary)))
            (should (string-prefix-p
                     (file-name-as-directory harmless-directory) summary))
            (with-temp-buffer
              (insert-file-contents summary)
              (should (string-search "sub" (buffer-string)))))
          (remhash (harmless-session-id session) harmless--sessions)
          (let ((loaded (harmless-session-resume (harmless-session-id session))))
            (should (harmless-same-directory-p
                     sub (harmless-session-shell-cwd loaded)))
            (let* ((file (expand-file-name
                          "summary.json" (harmless-session-directory loaded)))
                   (plist (harmless-json-decode
                           (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string)))))
              (setq plist (plist-put plist :shell-cwd "/"))
              (with-temp-file file
                (insert (harmless-json-text plist) "\n")))
            (remhash (harmless-session-id loaded) harmless--sessions)
            (let ((reloaded (harmless-session-resume
                             (harmless-session-id loaded))))
              (should (equal "/" (harmless-session-shell-cwd reloaded)))
              (let ((out (harmless-shell-test--call reloaded '(:command "cd"))))
                (should (string-search (file-name-nondirectory root) out))
                (should-not (string-search "__HARMLESS_CWD__" out)))
              (should-not (harmless-session-shell-cwd reloaded)))))
      (harmless-shell-test-stop session))))

(ert-deftest harmless-shell-command-kind-read-and-write ()
  (dolist (command (list "echo hi" "ECHO hi" "ls" "ls -la" "ls sub/" "ls /"
                         "ls c:/" "cat file" "pwd" "cd sub" "cd sub/" "cd /"
                         "cd .." "DIR" "git status" "git STATUS" "git -C sub status"
                         "git -c foo.bar=1 diff" "git diff --stat" "git log -n 5"
                         "git show HEAD" "git branch" "git branch -vv"
                         "git branch --list feature" "git tag" "git tag -l"
                         "git remote -v" "git stash list" "git config user.name"
                         "git config --get user.name" "git status 2>&1"
                         "rg pattern" "rg -n foo" "find . -name x"
                         "sed -n 1p file" "sed -n '1,20p' file" "sed 's/a/w/b/' f"
                         "sed -e 1p file" "sort file" "echo hi | wc -l"
                         "cd sub && ls" "echo 'rm -rf /'" "echo '$(rm x)'"
                         "echo \"hello > world\"" "echo \"\"" "FOO=1 ls"
                         "ls && echo hi" "echo hi; ls" "echo hi & echo there"
                         (concat "echo hi\nls")))
    (should (equal (list command 'read)
                   (list command (harmless-shell-command-kind command)))))
  (dolist (command (list nil "" "   " "/" "c:/" "rm foo" "rm -rf /"
                         "./ls" "sub/ls" "/bin/ls" "make" "python -c print"
                         "cmd /c echo hi" "echo hi > out" "echo hi > /"
                         "echo hi > c:/out" "echo hi >> out" "echo hi 2> err"
                         "ls >out" "$(ls)" "`ls`" "(ls)" "echo $(ls)"
                         "echo \"hello $(ls)\"" "find . -delete"
                         "find . -exec rm {} ;" "find . -fprint out"
                         "sed -i s/a/b/ file" "sed -i.bak s/a/b/ file"
                         "sed --in-place s/a/b/ f" "sed -e w/out f"
                         "sed 's/a/b/w out' f" "sed -f script"
                         "git commit -m x" "git checkout main" "git push"
                         "git branch feature" "git branch -d feature"
                         "git diff --output=out" "git stash"
                         "git config user.name Chris" "sort -o out file"
                         "rg --pre cat" "ls && rm x" "echo hi; rm x"
                         "FOO=1 rm x" "echo 'hi" "\"\""))
    (should (equal (list command 'write)
                   (list command (harmless-shell-command-kind command)))))
  (should (eq 'shell (harmless-tool-class (harmless-tool-by-name "run_shell"))))
  (should (eq 'read (harmless-shell-run-class nil '(:command "echo hi"))))
  (should (eq 'shell (harmless-shell-run-class nil '(:command "rm x"))))
  (should (eq 'shell (harmless-shell-run-class nil '(:command nil))))
  (should (eq 'shell (harmless-shell-run-class nil nil)))
  (should (eq 'shell (harmless-shell-run-class nil '(:command ""))))
  (should (eq 'shell (harmless-shell-run-class nil '(:command "/"))))
  (should-not (harmless-tool-classify (harmless-tool-by-name "shell_kill"))))

(ert-deftest harmless-shell-plan-mode-refuses-a-write ()
  (let* ((dir (make-temp-file "harmless-shell-plan-" t))
         (session (harmless-test--session dir))
         (out (expand-file-name "out.txt" dir)))
    (unwind-protect
        (progn
          (setf (harmless-session-plan-mode session) t)
          (setf (harmless-session-permission-mode session) 'always-approve)
          (should (equal harmless-shell-plan-refusal
                         (harmless-shell-test--call
                          session '(:command "echo hi > out.txt"))))
          (should-not (file-exists-p out))
          (should-not (file-exists-p "/out.txt"))
          (should-not (harmless-session-shell-jobs session))
          (should (equal harmless-shell-plan-refusal
                         (harmless-shell-test--call session '(:command "/"))))
          (should (equal harmless-shell-plan-refusal
                         (harmless-shell-test--call session '(:command "c:/"))))
          (should (equal harmless-shell-plan-refusal
                         (harmless-shell-test--call
                          session '(:command "sub/ls"))))
          (should (equal "Error: command is empty"
                         (harmless-shell-test--call session '(:command ""))))
          (should (equal "Error: command is empty"
                         (harmless-shell-test--call session '(:command nil))))
          (should (equal "Error: command is empty"
                         (harmless-shell-test--call session nil)))
          (let ((text (harmless-shell-test--call
                       session '(:command "echo hi"))))
            (should (string-match-p "hi" text))
            (should-not (string-search "__HARMLESS_CWD__" text)))
          (should-not (file-exists-p out)))
      (harmless-shell-test-stop session)
      (when (file-exists-p out) (delete-file out)))))

(provide 'harmless-tools-tests)
