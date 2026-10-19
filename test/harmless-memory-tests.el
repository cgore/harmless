;;; harmless-memory-tests.el --- Tests for Harmless memory -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless-memory)
(require 'harmless-provider)
(require 'harmless-skills)
(require 'harmless-turn)

(cl-defstruct (harmless-memory-fake
               (:include harmless-provider)
               (:constructor harmless-memory-make-fake))
  script)

(cl-defmethod harmless-provider-complete ((provider harmless-memory-fake)
                                          _messages _tools callback)
  (when (harmless-memory-fake-script provider)
    (funcall (harmless-memory-fake-script provider) callback))
  nil)

(defun harmless-memory-test-signal (fn)
  "Return the error text from FN."
  (error-message-string (should-error (funcall fn) :type 'error)))

(defun harmless-memory-test-session (id cwd)
  "Return a session ID at CWD.  It is not registered."
  (harmless-session--create
   :id id
   :cwd cwd
   :model "grok-4.6"
   :source 'agent
   :permission-mode 'ask
   :status 'idle
   :messages nil
   :plan-mode nil))

(defun harmless-memory-test-inbox (scope cwd)
  "Return inbox files for SCOPE at CWD, or nil."
  (let ((dir (expand-file-name
              "observations/_inbox"
              (harmless-memory-scope-dir scope cwd))))
    (when (file-directory-p dir)
      (directory-files dir t "\\.md\\'" t))))

(defun harmless-memory-test-text (path)
  "Return PATH as text."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defmacro harmless-memory-test-with-root (&rest body)
  "Run BODY with `harmless-directory' in a temporary directory."
  `(let ((harmless-directory (make-temp-file "harmless-memory-" t)))
     ,@body))

(ert-deftest harmless-memory-does-not-store-at-filesystem-root ()
  (let ((default-directory "/"))
    (let ((harmless-directory "/"))
      (should-error (harmless-memory-root))
      (should-not (file-directory-p "/memory")))
    (let ((harmless-directory (make-temp-file "harmless-memory-" t)))
      (should-error (harmless-memory-remember "workspace" nil "Title" "Body"))
      (should-error (harmless-memory-remember "workspace" "" "Title" "Body"))
      (should-error (harmless-memory-scope-dir "workspace" nil))
      (let ((dir (harmless-memory-scope-dir "workspace" "/proj/demo")))
        (should (string-prefix-p
                 (file-name-as-directory harmless-directory) dir))
        (should (string-search "%2Fproj%2Fdemo" dir))
        (should-not (harmless-filesystem-root-p dir)))
      (should-not (file-exists-p "/summary.json"))
      (let ((global (harmless-memory-scope-dir "global" nil)))
        (should (string-prefix-p (file-name-as-directory harmless-directory)
                                 global))
        (should (string-search "/memory/global" global))
        (should-not (harmless-filesystem-root-p global)))
      (should (string-prefix-p "observations/_inbox/"
                               (harmless-memory-remember
                                "global" nil "Global" "Applies everywhere."))))
  (let* ((fallback (make-temp-file "harmless-memory-fallback-" t))
         (harmless-directory nil)
         (default-directory "/"))
    (cl-letf (((symbol-function 'locate-user-emacs-file)
               (lambda (&rest _) fallback)))
      (let ((root (harmless-memory-root)))
        (should (string-prefix-p (file-name-as-directory fallback) root))
        (should-not (harmless-filesystem-root-p root))
        (should-not (file-directory-p "/memory")))))))

(ert-deftest harmless-memory-remember-read-and-index ()
  (harmless-memory-test-with-root
   (let ((cwd "/proj/harmless"))
     (let ((rel (harmless-memory-remember
                 "workspace" cwd "Two spaces" "Prefer two spaces in Elisp." "emacs")))
       (should (string-prefix-p "observations/_inbox/" rel))
       (should (string-match-p "Prefer two spaces"
                               (harmless-memory-read "workspace" cwd rel))))
     (should-error (harmless-memory-read "workspace" cwd "../auth.json"))
     (should-error (harmless-memory-read "workspace" cwd "/etc/passwd"))
     (let ((index (harmless-memory-read "workspace" cwd "MEMORY.md"))
           (catalog (harmless-memory-catalog cwd)))
       (should (string-match-p "Do not edit" index))
       (should (string-match-p "Two spaces" index))
       (should (string-match-p "Pending" catalog))
       (should (string-match-p "memory_read" catalog))
       (should-not (string-match-p "### Global" catalog))))))

(ert-deftest harmless-memory-dream-folds-inbox ()
  (harmless-memory-test-with-root
   (let ((cwd "/proj/harmless"))
     (harmless-memory-write-topic
      "workspace" cwd "emacs" "# Emacs\n\nThe editor.\n")
     (harmless-memory-remember
      "workspace" cwd "Indent" "Use two spaces." "emacs")
     (harmless-memory-remember
      "global" cwd "PRs" "Open the pull request after pushing." "git")
     (should (= 1 (harmless-memory-dream "workspace" cwd)))
     (should (= 1 (harmless-memory-dream "global" cwd)))
     (should (= 0 (harmless-memory-dream "workspace" cwd)))
     (let ((topic (harmless-memory-read "workspace" cwd "topics/emacs.md")))
       (should (string-match-p "The editor" topic))
       (should (string-match-p "## Indent" topic))
       (should (string-match-p "Use two spaces" topic)))
     (should (string-match-p "Open the pull request"
                             (harmless-memory-read "global" cwd "topics/git.md")))
     (let ((catalog (harmless-memory-catalog cwd)))
       (should (string-match-p "### Workspace" catalog))
       (should (string-match-p "### Global" catalog))
       (should (string-match-p "topics/emacs.md" catalog))
       (should-not (string-match-p "Pending:" catalog))))))

(ert-deftest harmless-memory-scopes-are-separate ()
  (harmless-memory-test-with-root
   (harmless-memory-remember "workspace" "/proj/a" "A" "workspace a" "place")
   (harmless-memory-remember "workspace" "/proj/b" "B" "workspace b" "place")
   (let ((catalog (harmless-memory-catalog "/proj/a")))
     (should (string-match-p "\\*\\*A\\*\\*" catalog))
     (should-not (string-match-p "\\*\\*B\\*\\*" catalog)))))

(ert-deftest harmless-memory-context-includes-index ()
  (harmless-memory-test-with-root
   (harmless-memory-write-topic
    "workspace" "/proj/harmless" "emacs" "# Emacs\n\nPrefer two spaces.\n")
   (let ((messages (harmless-context-messages "/proj/harmless" nil "/proj/harmless")))
     (should (eq :system (plist-get (car messages) :role)))
     (should (string-match-p "topics/emacs.md"
                             (plist-get (car messages) :content)))
     (should (string-match-p "Prefer two spaces"
                             (plist-get (car messages) :content))))))

(ert-deftest harmless-memory-observe-writes-after-a-turn ()
  (let* ((base (make-temp-file "harmless-memory-obs-" t))
         (root (expand-file-name "proj" base))
         (elsewhere (expand-file-name "elsewhere" base))
         (harmless-directory (expand-file-name ".harmless-state" base))
         (harmless-compact-threshold nil)
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-perm-ask-function
          (lambda (_s _c _i cb) (funcall cb 'allow)))
         (session (harmless-memory-test-session "hmem-write" root))
         (note '(:title "  Noted  " :body " HMEM-BODY " :topic " emacs "))
         (harmless-memory-observe-function
          (lambda (_session) note)))
    (make-directory root t)
    (make-directory elsewhere t)
    (unwind-protect
        (let ((default-directory (file-name-as-directory elsewhere)))
          (should (null (harmless-session-messages session)))
          (let* ((rel (harmless-memory-observe session))
                 (path (expand-file-name
                        rel (harmless-memory-scope-dir "workspace" root)))
                 (text (harmless-memory-test-text path)))
            (should (string-prefix-p "observations/_inbox/" rel))
            (should (file-regular-p path))
            (should (file-name-absolute-p path))
            (should (harmless-directory-strictly-under-p path harmless-directory))
            (should-not (equal (file-truename path) (file-truename "/")))
            (should-not (string-prefix-p
                         (file-name-as-directory root)
                         (file-name-as-directory path)))
            (should (string-search (harmless-session-encode-cwd root) path))
            (should (string-match-p "# Noted" text))
            (should (string-match-p "HMEM-BODY" text))
            (should (string-match-p "topic: emacs" text))
            (should (string-match-p
                     "Noted"
                     (harmless-memory-read "workspace" root "MEMORY.md"))))
          (should (null (harmless-session-messages session)))
          (should (equal (harmless-memory-scope-dir "workspace" root)
                         (harmless-memory-scope-dir
                          "workspace" (file-name-as-directory root))))
          (setf (harmless-session-cwd session)
                (file-name-as-directory root))
          (let ((rel (harmless-memory-observe session)))
            (should (equal 2 (length (harmless-memory-test-inbox
                                      "workspace" root))))
            (should (string-match-p
                     "HMEM-BODY"
                     (harmless-memory-read "workspace" root rel))))
          (setf (harmless-session-cwd session) nil)
          (setq note '(:title "Global" :body "HMEM-GLOBAL"))
          (let* ((rel (harmless-memory-observe session))
                 (path (expand-file-name
                        rel (harmless-memory-scope-dir "global" nil))))
            (should (string-match-p
                     "HMEM-GLOBAL"
                     (harmless-memory-read "global" nil rel)))
            (should (file-name-absolute-p path))
            (should (harmless-directory-strictly-under-p path harmless-directory))
            (should-not (equal (file-truename path) (file-truename "/"))))
          (setf (harmless-session-cwd session) root)
          (setq note '(:title "Pref" :body "HMEM-G" :scope "g"))
          (should (string-match-p
                   "HMEM-G"
                   (harmless-memory-read
                    "global" nil (harmless-memory-observe session))))
          (should (equal 2 (length (harmless-memory-test-inbox
                                    "workspace" root))))
          (setq harmless-memory-observe-function nil)
          (should-not (harmless-memory-observe session))
          (should (equal 2 (length (harmless-memory-test-inbox
                                    "workspace" root))))
          (let* ((calls 0)
                 (seen 0)
                 (provider (harmless-memory-make-fake
                            :name "fake" :host "none"))
                 (live (harmless-session-new
                        :cwd root :provider provider :model "m")))
            (setq harmless-memory-observe-function nil)
            (setf (harmless-memory-fake-script provider)
                  (lambda (callback)
                    (setq calls (1+ calls))
                    (funcall callback '(:text "plain"))
                    (funcall callback '(:stop "stop"))))
            (harmless-turn-run live "hello")
            (should (= calls 1))
            (should (eq 'idle (harmless-session-status live)))
            (should (equal 2 (length (harmless-memory-test-inbox
                                      "workspace" root))))
            (setq harmless-memory-observe-function
                  (lambda (_session)
                    (setq seen (1+ seen))
                    '(:title "Turn" :body "HMEM-TURN" :topic "emacs")))
            (setf (harmless-memory-fake-script provider)
                  (lambda (callback)
                    (setq calls (1+ calls))
                    (if (= calls 2)
                        (progn
                          (funcall callback
                                   '(:tool-call "1" "write_file"
                                     "{\"path\":\"x.txt\",\"contents\":\"hi\"}"))
                          (funcall callback '(:stop "tool_calls")))
                      (funcall callback '(:text "wrote it"))
                      (funcall callback '(:stop "stop")))))
            (harmless-turn-run live "write a file")
            (should (= calls 3))
            (should (= seen 1))
            (should (eq 'idle (harmless-session-status live)))
            (should (file-regular-p (expand-file-name "x.txt" root)))
            (should (equal 3 (length (harmless-memory-test-inbox
                                      "workspace" root))))
            (let ((roles (mapcar (lambda (m) (plist-get m :role))
                                 (harmless-session-messages live))))
              (should (equal '(:user :assistant :user :assistant
                                     :tool :assistant)
                             roles)))
            (setq seen 0)
            (setf (harmless-memory-fake-script provider)
                  (lambda (callback)
                    (setq calls (1+ calls))
                    (if (= calls 4)
                        (progn
                          (setf (harmless-session-steers live)
                                (list "follow up"))
                          (funcall callback '(:text "first"))
                          (funcall callback '(:stop "stop")))
                      (funcall callback '(:text "second"))
                      (funcall callback '(:stop "stop")))))
            (harmless-turn-run live "and then")
            (should (= calls 5))
            (should (= seen 1))
            (should (eq 'idle (harmless-session-status live)))
            (should (equal 4 (length (harmless-memory-test-inbox
                                      "workspace" root))))))
      (ignore-errors (delete-directory base t)))))

(ert-deftest harmless-memory-observe-refuses-a-bad-request ()
  (let* ((base (make-temp-file "harmless-memory-obs-bad-" t))
         (root (expand-file-name "proj" base))
         (harmless-directory (expand-file-name ".harmless-state" base))
         (harmless-compact-threshold nil)
         (harmless--sessions (make-hash-table :test 'equal))
         (session (harmless-memory-test-session "hmem-bad" root))
         (called nil)
         (harmless-memory-observe-function
          (lambda (_session)
            (setq called t)
            '(:title "T" :body "HMEM-NO")))
         (before nil))
    (make-directory root t)
    (dolist (path '("/memory" "/.harmless" "c:/.harmless" "d:/.harmless"))
      (push (cons path (file-exists-p path)) before))
    (unwind-protect
        (let ((default-directory (file-name-as-directory root)))
          (should (equal "No Harmless session"
                         (harmless-memory-test-signal
                          (lambda () (harmless-memory-observe nil)))))
          (should-not called)
          (setq harmless-memory-observe-function
                (lambda (_session)
                  (setq called t)
                  nil))
          (should-not (harmless-memory-observe session))
          (should called)
          (setq called nil)
          (dolist (value (list "" 42 '(:title "  " :body "x" :scope "nope")
                               '(:title "T" :body "  ")))
            (setq harmless-memory-observe-function
                  (lambda (_session)
                    (setq called t)
                    value))
            (should-not (harmless-memory-observe session))
            (should called)
            (setq called nil))
          (should-not (file-directory-p
                       (expand-file-name "memory" harmless-directory)))
          (setq harmless-memory-observe-function
                (lambda (_session) '(:title dot :body "x")))
          (should (equal "memory title must be a string"
                         (harmless-memory-test-signal
                          (lambda () (harmless-memory-observe session)))))
          (setq harmless-memory-observe-function
                (lambda (_session) '(:title "T" :body 42)))
          (should (equal "memory body must be a string"
                         (harmless-memory-test-signal
                          (lambda () (harmless-memory-observe session)))))
          (setq harmless-memory-observe-function
                (lambda (_session) '(:title "T" :body "B" :topic 42)))
          (should (equal "memory topic must be a string"
                         (harmless-memory-test-signal
                          (lambda () (harmless-memory-observe session)))))
          (setq harmless-memory-observe-function
                (lambda (_session) '(:title "T" :body "B" :scope 42)))
          (should (equal "memory scope must be a string"
                         (harmless-memory-test-signal
                          (lambda () (harmless-memory-observe session)))))
          (setq harmless-memory-observe-function
                (lambda (_session) '(:title "T" :body "B" :scope "nope")))
          (should (equal "memory scope is not a scope"
                         (harmless-memory-test-signal
                          (lambda () (harmless-memory-observe session)))))
          (setq harmless-memory-observe-function
                (lambda (_session) '(:title "T" :body "B" :scope "  ")))
          (should (equal "memory scope is not a scope"
                         (harmless-memory-test-signal
                          (lambda () (harmless-memory-observe session)))))
          (should-not (file-directory-p
                       (expand-file-name "memory" harmless-directory)))
          (dolist (dir (list "/" "c:/" "d:/" "c:\\" "C:/" "c:" "\\" "//" "/."))
            (setf (harmless-session-cwd session) dir)
            (setq harmless-memory-observe-function
                  (lambda (_session)
                    (setq called t)
                    '(:title "T" :body "HMEM-ROOT")))
            (should (equal "memory directory is the filesystem root"
                           (harmless-memory-test-signal
                            (lambda () (harmless-memory-observe session))))))
          (dolist (pair before)
            (should (eq (cdr pair) (file-exists-p (car pair)))))
          (should called)
          (setq called nil)
          (setf (harmless-session-cwd session) "/")
          (setq harmless-memory-observe-function
                (lambda (_session)
                  (setq called t)
                  '(:title "G" :body "HMEM-ROOT-G" :scope "global")))
          (let ((rel (harmless-memory-observe session)))
            (should called)
            (should (string-match-p
                     "HMEM-ROOT-G"
                     (harmless-memory-read "global" nil rel)))
            (should (harmless-directory-strictly-under-p
                     (expand-file-name
                      rel (harmless-memory-scope-dir "global" nil))
                     harmless-directory)))
          (dolist (pair before)
            (should (eq (cdr pair) (file-exists-p (car pair)))))
          (setf (harmless-session-cwd session) "notes")
          (setq called nil)
          (setq harmless-memory-observe-function
                (lambda (_session)
                  (setq called t)
                  '(:title "T" :body "HMEM-REL")))
          (should (equal "memory directory must be absolute"
                         (harmless-memory-test-signal
                          (lambda () (harmless-memory-observe session)))))
          (should called)
          (should-not (file-exists-p (expand-file-name "notes" root)))
          (setf (harmless-session-cwd session) "~/proj")
          (should (equal "memory directory must be absolute"
                         (harmless-memory-test-signal
                          (lambda () (harmless-memory-observe session)))))
          (setf (harmless-session-cwd session) "")
          (should (equal "This chat is not attached to a project"
                         (harmless-memory-test-signal
                          (lambda () (harmless-memory-observe session)))))
          (setf (harmless-session-cwd session) "  ")
          (should (equal "This chat is not attached to a project"
                         (harmless-memory-test-signal
                          (lambda () (harmless-memory-observe session)))))
          (let* ((seen 0)
                 (provider (harmless-memory-make-fake
                            :name "fake" :host "none"))
                 (live (harmless-session-new
                        :cwd root :provider provider :model "m")))
            (setq harmless-memory-observe-function
                  (lambda (_session)
                    (setq seen (1+ seen))
                    '(:title 42 :body "x")))
            (setf (harmless-memory-fake-script provider)
                  (lambda (callback)
                    (funcall callback '(:text "kept"))
                    (funcall callback '(:stop "stop"))))
            (harmless-turn-run live "hi")
            (should (= seen 1))
            (should (eq 'idle (harmless-session-status live)))
            (should (equal '(:user :assistant)
                           (mapcar (lambda (m) (plist-get m :role))
                                   (harmless-session-messages live))))
            (setq seen 0)
            (setf (harmless-memory-fake-script provider)
                  (lambda (callback)
                    (funcall callback '(:stop "stop"))))
            (harmless-turn-run live "empty")
            (should (= seen 0))
            (should (eq 'error (harmless-session-status live)))))
      (ignore-errors (delete-directory base t)))))
