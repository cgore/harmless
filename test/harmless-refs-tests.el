;;; harmless-refs-tests.el --- Tests for prompt context references -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'harmless-provider)
(require 'harmless-session)
(require 'harmless-refs)
(require 'harmless-turn)
(require 'harmless-skills)

(cl-defstruct (harmless-refs-fake
               (:include harmless-provider)
               (:constructor harmless-refs-make-fake)))

(cl-defmethod harmless-provider-complete ((_provider harmless-refs-fake)
                                          _messages _tools _callback)
  nil)

(defmacro harmless-refs-test-with (repo &rest body)
  "Bind a temp project REPO and a temp data directory, then run BODY."
  (declare (indent 1))
  `(let* ((,repo (make-temp-file "harmless-refs-repo-" t))
          (harmless-directory (make-temp-file "harmless-refs-data-" t))
          (harmless--sessions (make-hash-table :test 'equal))
          (harmless-providers nil)
          (harmless-web-retrieve-function
           (lambda (_url) (error "network was called"))))
     ,@body))

(defun harmless-refs-test-session (repo &rest args)
  "Return a session at REPO.  ARGS are passed to `harmless-session-new'.
A detached session does not receive REPO as its project."
  (apply #'harmless-session-new
         :provider (harmless-refs-make-fake :name "fake" :host "none")
         :model "refs-model"
         (if (plist-get args :detached)
             args
           (append (list :cwd repo) args))))

(defun harmless-refs-test-not-root (session)
  "Require SESSION's directory to stay off the filesystem root."
  (let ((dir (harmless-session-directory session)))
    (should (file-name-absolute-p dir))
    (should-not (harmless-filesystem-root-p dir))
    (should (harmless-directory-strictly-under-p dir harmless-directory))
    (should-not (equal (file-truename dir)
                       (file-truename (expand-file-name "/"))))))

(defun harmless-refs-test-write (path text)
  "Write TEXT to PATH with Unix newlines and return PATH."
  (let ((coding-system-for-write 'utf-8-unix))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert text)))
  path)

(defmacro harmless-refs-test-err (form)
  "Return the error text signaled by FORM."
  `(error-message-string (should-error ,form :type 'error)))

(defun harmless-refs-test-attached (prompt &rest blocks)
  "Return PROMPT with BLOCKS under the attachment header."
  (concat prompt "\n\n" harmless-refs-header "\n\n"
          (mapconcat #'identity blocks "\n\n")))

(defun harmless-refs-test-messages (session)
  "Return SESSION's messages.jsonl text and require a safe path."
  (let ((file (expand-file-name "messages.jsonl"
                                 (harmless-session-directory session))))
    (should (file-name-absolute-p file))
    (should (file-exists-p file))
    (should-not (harmless-filesystem-root-p (file-name-directory file)))
    (should (harmless-directory-strictly-under-p
             (file-name-directory file) harmless-directory))
    (should-not (equal (file-truename (file-name-directory file))
                       (file-truename (expand-file-name "/"))))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun harmless-refs-test-user (session)
  "Return user message strings recorded on SESSION, in order."
  (mapcar (lambda (message) (plist-get message :content))
          (seq-filter (lambda (message)
                        (eq (plist-get message :role) :user))
                      (harmless-session-messages session))))

(defun harmless-refs-test-git (dir &rest args)
  "Run git ARGS in DIR.  A non-zero status is an error."
  (let ((process-environment
         (cl-remove-if (lambda (item)
                         (string-match-p "\\`GIT_" item))
                       process-environment)))
    (unless (eq 0 (apply #'call-process "git" nil nil nil
                         (append (list "-C" dir) args)))
      (error "git failed: %S" args))))

(defun harmless-refs-test-repo (dir)
  "Make DIR a git checkout with a local identity."
  (harmless-refs-test-git dir "init")
  (harmless-refs-test-git dir "config" "user.email" "harmless@example.com")
  (harmless-refs-test-git dir "config" "user.name" "Harmless")
  (harmless-refs-test-git dir "config" "core.autocrlf" "false")
  (harmless-refs-test-git dir "config" "commit.gpgsign" "false"))

(ert-deftest harmless-refs-leaves-plain-text-unchanged ()
  (should-not (harmless-refs-expand nil nil))
  (should (eq (harmless-refs-expand nil 42) 42))
  (should (eq (harmless-refs-expand nil :kw) :kw))
  (should (equal (harmless-refs-expand nil "") ""))
  (should (equal (harmless-refs-expand nil "hello") "hello"))
  (should (equal (harmless-refs-expand 'not-a-session "user@example.com")
                 "user@example.com"))
  (harmless-refs-test-with repo
    (let ((session (harmless-refs-test-session repo)))
      (dolist (text (list nil "" "   " "hello" "user@example.com"
                          "@diffing" "see@diff" "@diff:" "@staged:"
                          "read @file please" "see @folder please"))
        (should (equal (harmless-refs-expand session text) text)))
      (should (equal (harmless-refs-test-err
                      (harmless-refs-expand nil "@file:a.txt"))
                     "No Harmless session"))
      (should (equal (harmless-refs-test-err
                      (harmless-refs-expand 'not-a-session "@diff"))
                     "No Harmless session"))
      (harmless-refs-test-not-root session))))

(ert-deftest harmless-refs-refuses-empty-roots-and-escapes ()
  (harmless-refs-test-with repo
    (let* ((parent (file-name-directory (directory-file-name repo)))
           (secret (expand-file-name "secret.txt" parent))
           (session (harmless-refs-test-session repo))
           (outside (make-temp-file "harmless-refs-out-" t))
           (extra (make-temp-file "harmless-refs-extra-" t)))
      (harmless-refs-test-write secret "hidden-parent\n")
      (harmless-refs-test-write (expand-file-name "secret.txt" extra)
                                "hidden-extra\n")
      (harmless-refs-test-write (expand-file-name "a.txt" repo) "inside\n")
      (harmless-refs-test-write (expand-file-name "sub/a.txt" repo) "nested\n")
      (dolist (text (list "@file:" "@file:," "@folder:"
                          "@folder:   " "@url:" "@file:   ."))
        (should (equal (harmless-refs-test-err
                        (harmless-refs-expand session text))
                       "context reference is empty")))
      (dolist (text (list "@file:/" "@folder:/" "@file:c:/" "@folder:c:/"
                          "@file:c:" "@folder:C:\\" "@file:\\\\"
                          "@folder:d:/" "@file:D:\\"))
        (should (equal (harmless-refs-test-err
                        (harmless-refs-expand session text))
                       "context reference is the filesystem root")))
      (dolist (text (list "@file:.." "@file:../secret.txt" "@folder:.."
                          "@file:/tmp" "@file:c:/harmless-refs-not-a-dir"
                          "@folder:sub/../../secret.txt" "@file:..,"))
        (should (equal (harmless-refs-test-err
                        (harmless-refs-expand session text))
                       "context reference is outside the project")))
      (should (equal (harmless-refs-test-err
                      (harmless-refs-expand session "@file:."))
                     "context reference is a directory"))
      (should (equal (harmless-refs-expand session "@file:sub/../a.txt")
                     (harmless-refs-test-attached
                      "@file:sub/../a.txt" "@file:sub/../a.txt\ninside\n")))
      (should (equal (harmless-refs-test-err
                      (harmless-refs-expand session "@file:secret.txt"))
                     "context reference is missing"))
      (setf (harmless-session-extra-dirs session) (list extra))
      (let* ((abs (subst-char-in-string
                   ?\\ ?/ (expand-file-name "secret.txt" extra)))
             (prompt (concat "@file:" abs)))
        (should (equal (harmless-refs-expand session prompt)
                       (harmless-refs-test-attached
                        prompt (concat "@file:" abs "\nhidden-extra\n")))))
      (should (equal (harmless-refs-test-err
                      (harmless-refs-expand
                       session
                       (concat "@file:"
                               (subst-char-in-string
                                ?\\ ?/ (expand-file-name "nope.txt" outside)))))
                     "context reference is outside the project"))
      (let ((detached (harmless-refs-test-session nil :detached t :id "chat")))
        (should (equal (harmless-refs-test-err
                        (harmless-refs-expand detached "@file:a.txt"))
                       "This chat is not attached to a project"))
        (should (equal (harmless-refs-test-err
                        (harmless-refs-expand detached "@diff"))
                       "This chat is not attached to a project"))
        (harmless-refs-test-not-root detached))
      (harmless-refs-test-not-root session)
      (should-not (harmless-filesystem-root-p repo)))))

(ert-deftest harmless-refs-reads-a-file-and-a-line-range ()
  (harmless-refs-test-with repo
    (let* ((path (harmless-refs-test-write
                  (expand-file-name "sub/a.txt" repo)
                  "one\ntwo\nthree\n"))
           (abs (subst-char-in-string ?\\ ?/ path))
           (session (harmless-refs-test-session repo)))
      (should (equal (harmless-refs-expand session "see @file:sub/a.txt.")
                     (harmless-refs-test-attached
                      "see @file:sub/a.txt."
                      "@file:sub/a.txt\none\ntwo\nthree\n")))
      (should (equal (harmless-refs-expand session "@file:sub/a.txt:2-3,")
                     (harmless-refs-test-attached
                      "@file:sub/a.txt:2-3,"
                      "@file:sub/a.txt:2-3\ntwo\nthree\n")))
      (should (equal (harmless-refs-expand session "@file:sub/a.txt:1-1")
                     (harmless-refs-test-attached
                      "@file:sub/a.txt:1-1" "@file:sub/a.txt:1-1\none\n")))
      (should (equal (harmless-refs-expand session "@file:sub/a.txt:2-99")
                     (harmless-refs-test-attached
                      "@file:sub/a.txt:2-99"
                      "@file:sub/a.txt:2-99\ntwo\nthree\n")))
      (should (equal (harmless-refs-expand
                      session (format "range @file:%s:1-2" abs))
                     (harmless-refs-test-attached
                      (format "range @file:%s:1-2" abs)
                      (format "@file:%s:1-2\none\ntwo\n" abs))))
      (should (equal (harmless-refs-expand session "hi\n@file:sub/a.txt")
                     (harmless-refs-test-attached
                      "hi\n@file:sub/a.txt"
                      "@file:sub/a.txt\none\ntwo\nthree\n")))
      (should (equal (harmless-refs-expand session "@file:sub/a.txt/")
                     (harmless-refs-test-attached
                      "@file:sub/a.txt/"
                      "@file:sub/a.txt/\none\ntwo\nthree\n")))
      (when (memq system-type '(windows-nt ms-dos))
        (should (equal (harmless-refs-expand session "@file:sub\\a.txt")
                       (harmless-refs-test-attached
                        "@file:sub\\a.txt"
                        "@file:sub\\a.txt\none\ntwo\nthree\n"))))
      (let ((harmless-read-file-max-bytes 4))
        (should (equal (harmless-refs-expand session "@file:sub/a.txt")
                       (harmless-refs-test-attached
                        "@file:sub/a.txt"
                        "@file:sub/a.txt\none\n\n[truncated]"))))
      (harmless-refs-test-not-root session))))

(ert-deftest harmless-refs-refuses-a-bad-file ()
  (harmless-refs-test-with repo
    (let ((session (harmless-refs-test-session repo)))
      (make-directory (expand-file-name "sub" repo))
      (harmless-refs-test-write (expand-file-name "a.txt" repo) "one\ntwo\n")
      (harmless-refs-test-write (expand-file-name "empty.txt" repo) "")
      (let ((coding-system-for-write 'no-conversion))
        (with-temp-file (expand-file-name "bin.dat" repo)
          (set-buffer-multibyte nil)
          (insert "a\0b")))
      (should (equal (harmless-refs-test-err
                      (harmless-refs-expand session "@file:missing.txt"))
                     "context reference is missing"))
      (should (equal (harmless-refs-test-err
                      (harmless-refs-expand session "@file:sub"))
                     "context reference is a directory"))
      (should (equal (harmless-refs-test-err
                      (harmless-refs-expand session "@file:sub/"))
                     "context reference is a directory"))
      (should (equal (harmless-refs-test-err
                      (harmless-refs-expand session "@file:bin.dat"))
                     "context reference is not text"))
      (dolist (text (list "@file:a.txt:0-1" "@file:a.txt:2-1"))
        (should (equal (harmless-refs-test-err
                        (harmless-refs-expand session text))
                       "context reference line range is invalid")))
      (dolist (text (list "@file:a.txt:9-9" "@file:empty.txt:1-1"))
        (should (equal (harmless-refs-test-err
                        (harmless-refs-expand session text))
                       "context reference line range is past the end")))
      (let ((harmless-refs-window-function (lambda (_session) 4)))
        (should (equal (harmless-refs-test-err
                        (harmless-refs-expand session "@file:missing.txt"))
                       "context reference is missing")))
      (harmless-refs-test-not-root session))))

(ert-deftest harmless-refs-lists-a-folder ()
  (harmless-refs-test-with repo
    (let ((session (harmless-refs-test-session repo))
          (tree (expand-file-name "tree" repo)))
      (harmless-refs-test-write (expand-file-name "a.txt" repo) "abcd")
      (harmless-refs-test-write (expand-file-name "tree/.hidden" repo) "h")
      (harmless-refs-test-write (expand-file-name "tree/a.txt" repo) "abcd")
      (harmless-refs-test-write (expand-file-name "tree/sub/b.txt" repo) "z")
      (make-directory (expand-file-name "tree/.git" repo))
      (harmless-refs-test-write (expand-file-name "tree/.git/config" repo) "x")
      (make-directory (expand-file-name "empty" repo))
      (should (equal (harmless-refs-expand session "@folder:empty")
                     (harmless-refs-test-attached
                      "@folder:empty" "@folder:empty\n(empty)")))
      (should (equal (harmless-refs-expand session "@folder:empty/")
                     (harmless-refs-test-attached
                      "@folder:empty/" "@folder:empty/\n(empty)")))
      (should (equal (harmless-refs-expand session "@folder:tree")
                     (harmless-refs-test-attached
                      "@folder:tree"
                      (concat "@folder:tree\n"
                              ".hidden 1\n"
                              "a.txt 4\n"
                              "sub/\n"
                              "sub/b.txt 1"))))
      (should-not (string-search ".git"
                                 (harmless-refs-expand session "@folder:tree")))
      (should (equal (harmless-refs-test-err
                      (harmless-refs-expand session "@folder:a.txt"))
                     "context reference is not a directory"))
      (should (equal (harmless-refs-test-err
                      (harmless-refs-expand session "@folder:a.txt/"))
                     "context reference is not a directory"))
      (should (equal (harmless-refs-test-err
                      (harmless-refs-expand session "@folder:missing"))
                     "context reference is missing"))
      (let ((harmless-refs-folder-max 2))
        (should (equal (harmless-refs-expand session "@folder:tree")
                       (harmless-refs-test-attached
                        "@folder:tree"
                        "@folder:tree\n.hidden 1\na.txt 4\n- ..."))))
      (let ((harmless-refs-folder-max 0))
        (should (equal (harmless-refs-expand session "@folder:tree")
                       (harmless-refs-test-attached
                        "@folder:tree" "@folder:tree\n- ..."))))
      (let ((outside (make-temp-file "harmless-refs-link-" t))
            (link (expand-file-name "link" tree)))
        (harmless-refs-test-write (expand-file-name "secret.txt" outside)
                                  "hidden-link\n")
        (condition-case nil
            (make-symbolic-link outside link)
          (file-error nil))
        (when (file-symlink-p link)
          (let ((listed (harmless-refs-expand session "@folder:tree")))
            (should-not (string-search "link" listed))
            (should-not (string-search "secret.txt" listed))
            (should-not (string-search "hidden-link" listed)))
          (should (equal (harmless-refs-test-err
                          (harmless-refs-expand session "@file:tree/link"))
                         "context reference is outside the project"))))
      (harmless-refs-test-not-root session)
      (ignore tree))))

(ert-deftest harmless-refs-reads-git ()
  (harmless-refs-test-with repo
    (let ((session (harmless-refs-test-session repo)))
      (should (equal (harmless-refs-test-err
                      (harmless-refs-expand session "@diff"))
                     "context reference is not a git checkout"))
      (should (equal (harmless-refs-test-err
                      (harmless-refs-expand session "@staged"))
                     "context reference is not a git checkout"))
      (harmless-refs-test-repo repo)
      (should (equal (harmless-refs-expand session "@diff")
                     (harmless-refs-test-attached
                      "@diff" "@diff\n(no unstaged changes)")))
      (should (equal (harmless-refs-test-err
                      (harmless-refs-expand session "@git:1"))
                     "context reference git failed"))
      (harmless-refs-test-write (expand-file-name "a.txt" repo) "base\n")
      (harmless-refs-test-git repo "add" "--" "a.txt")
      (harmless-refs-test-git repo "commit" "-m" "refs-one")
      (should (equal (harmless-refs-expand session "see @diff.")
                     (harmless-refs-test-attached
                      "see @diff." "@diff\n(no unstaged changes)")))
      (should (equal (harmless-refs-expand session "@staged")
                     (harmless-refs-test-attached
                      "@staged" "@staged\n(no staged changes)")))
      (let ((one (harmless-refs-expand session "@git:1")))
        (should (string-prefix-p
                 (harmless-refs-test-attached "@git:1" "@git:1\n")
                 one))
        (should (string-search "refs-one" one))
        (should (string-search "base" one)))
      (let ((padded (harmless-refs-expand session "@git:01")))
        (should (string-search "refs-one" padded)))
      (let ((ten (harmless-refs-expand session "@git:10")))
        (should (string-search "refs-one" ten)))
      (harmless-refs-test-write (expand-file-name "a.txt" repo) "base\nmore\n")
      (let ((diff (harmless-refs-expand session "@diff")))
        (should (string-search "more" diff))
        (should (string-search "a.txt" diff))
        (should-not (string-search "(no unstaged changes)" diff)))
      (harmless-refs-test-git repo "add" "--" "a.txt")
      (should (equal (harmless-refs-expand session "@diff")
                     (harmless-refs-test-attached
                      "@diff" "@diff\n(no unstaged changes)")))
      (let ((staged (harmless-refs-expand session "@staged")))
        (should (string-search "more" staged))
        (should (string-search "a.txt" staged)))
      (harmless-refs-test-git repo "commit" "-m" "refs-two")
      (let ((two (harmless-refs-expand session "@git:2")))
        (should (string-search "refs-one" two))
        (should (string-search "refs-two" two)))
      (dolist (text (list "@git:" "@git:0" "@git:11" "@git:1a" "@git:1.5"))
        (should (equal (harmless-refs-test-err
                        (harmless-refs-expand session text))
                       "git history must be from 1 to 10")))
      (let ((harmless-read-file-max-bytes 8))
        (should (string-search "\n[truncated]"
                               (harmless-refs-expand session "@git:1"))))
      (let ((orig (symbol-function 'executable-find)))
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (command &optional remote)
                     (if (equal command "git")
                         nil
                       (funcall orig command remote)))))
          (should (equal (harmless-refs-test-err
                          (harmless-refs-expand session "@diff"))
                         "context reference needs git"))
          (should (equal (harmless-refs-test-err
                          (harmless-refs-expand session "@git:1"))
                         "context reference needs git"))))
      (harmless-refs-test-not-root session))))

(ert-deftest harmless-refs-fetches-a-url ()
  (harmless-refs-test-with repo
    (let ((session (harmless-refs-test-session repo))
          (detached (harmless-refs-test-session nil :detached t :id "chat"))
          (seen nil))
      (let ((harmless-web-retrieve-function
             (lambda (url) (push url seen) "page-body")))
        (should (equal (harmless-refs-expand
                        session "read @url:https://example.com/a.")
                       (harmless-refs-test-attached
                        "read @url:https://example.com/a."
                        "@url:https://example.com/a\npage-body")))
        (should (equal seen '("https://example.com/a")))
        (setq seen nil)
        (should (equal (harmless-refs-expand
                        detached "@url:http://example.com/b")
                       (harmless-refs-test-attached
                        "@url:http://example.com/b"
                        "@url:http://example.com/b\npage-body")))
        (should (equal seen '("http://example.com/b"))))
      (let ((harmless-web-retrieve-function
             (lambda (_url) (error "network was called"))))
        (should (equal (harmless-refs-test-err
                        (harmless-refs-expand session "@url:file:///etc/passwd"))
                       "url must be http or https"))
        (should (equal (harmless-refs-test-err
                        (harmless-refs-expand session "@url:"))
                       "context reference is empty")))
      (let ((harmless-read-file-max-bytes 4)
            (harmless-web-retrieve-function (lambda (_url) "abcdefgh")))
        (should (equal (harmless-refs-expand session "@url:https://example.com/c")
                       (harmless-refs-test-attached
                        "@url:https://example.com/c"
                        "@url:https://example.com/c\nabcd\n[truncated]"))))
      (harmless-refs-test-not-root session)
      (harmless-refs-test-not-root detached))))

(ert-deftest harmless-refs-caps-the-attachment ()
  (harmless-refs-test-with repo
    (let ((session (harmless-refs-test-session repo))
          (prompt "see @file:a.txt"))
      (harmless-refs-test-write (expand-file-name "a.txt" repo) "abcde")
      (harmless-refs-test-write (expand-file-name "b.txt" repo) "zz")
      (let* ((body "@file:a.txt\nabcde")
             (attached (concat harmless-refs-header "\n\n" body))
             (tokens (harmless-refs--tokens attached))
             (kept (harmless-refs-test-attached prompt body))
             (warned (concat kept
                             "\n\nAttached context is 25% of the context window or more.")))
        (should (> tokens (harmless-refs--tokens "abcde")))
        (should (= tokens (ceiling (length attached) 4)))
        (let ((harmless-refs-window-function
               (lambda (_session) (* (1- tokens) 2))))
          (should (equal (harmless-refs-expand session prompt) prompt))
          (should-not (string-search harmless-refs-header
                                     (harmless-refs-expand session prompt))))
        (let ((harmless-refs-window-function
               (lambda (_session) (* tokens 2))))
          (should (equal (harmless-refs-expand session prompt) warned)))
        (let ((harmless-refs-window-function
               (lambda (_session) (* tokens 4))))
          (should (equal (harmless-refs-expand session prompt) kept))))
      (should (equal (harmless-refs-default-window session) 128000))
      (should (equal (harmless-refs-expand session prompt)
                     (harmless-refs-test-attached
                      prompt "@file:a.txt\nabcde")))
      (dolist (window (list nil 0 -3 1.5))
        (let ((harmless-refs-window-function (lambda (_session) window)))
          (should (equal (harmless-refs-expand session prompt)
                         (harmless-refs-test-attached
                          prompt "@file:a.txt\nabcde")))))
      (should (equal (harmless-refs-expand
                      session "both @file:a.txt @file:b.txt")
                     (harmless-refs-test-attached
                      "both @file:a.txt @file:b.txt"
                      "@file:a.txt\nabcde"
                      "@file:b.txt\nzz")))
      (harmless-refs-test-not-root session))))

(ert-deftest harmless-refs-turn-stores-the-expansion ()
  (harmless-refs-test-with repo
    (let ((session (harmless-refs-test-session repo)))
      (harmless-refs-test-write (expand-file-name "a.txt" repo) "alpha-line\n")
      (harmless-turn-run session "please @file:a.txt")
      (should (eq 'streaming (harmless-session-status session)))
      (should (equal (harmless-refs-test-user session)
                     (list (harmless-refs-test-attached
                            "please @file:a.txt"
                            "@file:a.txt\nalpha-line\n"))))
      (should (string-search "alpha-line" (harmless-refs-test-messages session)))
      (let ((file (expand-file-name "messages.jsonl"
                                     (harmless-session-directory session))))
        (should (file-exists-p file))
        (should (string-prefix-p
                 (file-name-as-directory (harmless-session-directory session))
                 (file-truename file))))
      (let ((quiet (harmless-refs-test-session repo :id "quiet")))
        (should (equal (harmless-refs-test-err
                        (harmless-turn-run quiet "@file:missing.txt"))
                       "context reference is missing"))
        (should-not (harmless-session-messages quiet))
        (should (eq 'idle (harmless-session-status quiet)))
        (should (equal (harmless-refs-test-messages quiet) ""))
        (harmless-refs-test-not-root quiet))
      (let ((harmless-refs-window-function (lambda (_session) 4))
            (capped (harmless-refs-test-session repo :id "capped")))
        (harmless-turn-run capped "see @file:a.txt")
        (should (equal (harmless-refs-test-user capped) '("see @file:a.txt")))
        (should-not (string-search "alpha-line"
                                   (harmless-refs-test-messages capped))))
      (harmless-refs-test-not-root session))))

(ert-deftest harmless-refs-steers-expand-or-keep-the-queue ()
  (harmless-refs-test-with repo
    (let ((session (harmless-refs-test-session repo)))
      (harmless-refs-test-write (expand-file-name "a.txt" repo) "alpha-line\n")
      (setf (harmless-session-steers session)
            (list "see @file:a.txt" "plain"))
      (should (equal (harmless-turn--take-steers session)
                     (list (harmless-refs-test-attached
                            "see @file:a.txt" "@file:a.txt\nalpha-line\n")
                           "plain")))
      (should-not (harmless-session-steers session))
      (should (equal (harmless-refs-test-user session)
                     (list (harmless-refs-test-attached
                            "see @file:a.txt" "@file:a.txt\nalpha-line\n")
                           "plain")))
      (should (string-search "alpha-line" (harmless-refs-test-messages session)))
      (should (eq 'idle (harmless-session-status session)))
      (let ((stuck (harmless-refs-test-session repo :id "stuck")))
        (setf (harmless-session-steers stuck) (list "fine" "@file:"))
        (should (equal (harmless-refs-test-err
                        (harmless-turn--take-steers stuck))
                       "context reference is empty"))
        (should (equal (harmless-session-steers stuck) (list "fine" "@file:")))
        (should-not (harmless-session-messages stuck))
        (should (equal (harmless-refs-test-messages stuck) ""))
        (harmless-refs-test-not-root stuck))
      (harmless-refs-test-not-root session))))

(ert-deftest harmless-refs-skill-uses-the-typed-prompt ()
  (harmless-refs-test-with repo
    (let ((session (harmless-refs-test-session repo))
          (dir (expand-file-name ".harmless/skills/refsdemo" repo)))
      (make-directory dir t)
      (harmless-refs-test-write
       (expand-file-name "SKILL.md" dir)
       "---\nname: refsdemo\ndescription: Refs demo\n---\nDemo body\n")
      (harmless-refs-test-write (expand-file-name "a.txt" repo) "alpha-line\n")
      (should (equal (harmless-refs-test-err
                      (harmless-turn-run session "/refsdemo @file:missing.txt"))
                     "context reference is missing"))
      (should-not (harmless-skill-active session))
      (should-not (harmless-session-messages session))
      (harmless-turn-run session "/refsdemo @file:a.txt")
      (should (equal (plist-get (harmless-skill-active session) :name)
                     "refsdemo"))
      (should (equal (harmless-refs-test-user session)
                     (list (harmless-refs-test-attached
                            "/refsdemo @file:a.txt"
                            "@file:a.txt\nalpha-line\n"))))
      (should (eq 'streaming (harmless-session-status session)))
      (let ((other (harmless-refs-test-session repo :id "other")))
        (should (equal (harmless-refs-test-err
                        (harmless-turn-run other "/no-such-skill @file:a.txt"))
                       "no skill named no-such-skill"))
        (should-not (harmless-skill-active other))
        (should-not (harmless-session-messages other))
        (should (equal (harmless-refs-test-messages other) ""))
        (harmless-refs-test-not-root other))
      (harmless-refs-test-not-root session))))
