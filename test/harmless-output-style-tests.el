;;; harmless-output-style-tests.el --- Tests for output styles -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(cl-defstruct (harmless-output-style-fake
               (:include harmless-provider)
               (:constructor harmless-output-style-make-fake)))

(defun harmless-output-style-test-read (path)
  "Return PATH as a Unix-newline string."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (buffer-string)))

(defun harmless-output-style-test-write (file text)
  "Write TEXT to FILE with Unix newlines."
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file file
      (insert text))))

(defun harmless-output-style-test-session (id cwd)
  "Return a session ID at CWD."
  (harmless-session--create
   :id id
   :cwd cwd
   :model "grok-4.6"
   :source 'agent
   :permission-mode 'ask
   :status 'idle
   :messages nil
   :plan-mode nil))

(defun harmless-output-style-test-signal (fn)
  "Return the error text from FN."
  (error-message-string (should-error (funcall fn) :type 'error)))

(defun harmless-output-style-test-system (session dir)
  "Return the system text for SESSION at DIR."
  (let ((out (harmless-context-messages
              dir
              (or (harmless-session-messages session)
                  (list (list :role :user :content "hi")))
              nil "grok-4.6" session)))
    (plist-get (car out) :content)))

(ert-deftest harmless-output-style-switches-without-editing-instructions ()
  (let* ((base (make-temp-file "harmless-style-" t))
         (root (expand-file-name "proj" base))
         (harmless-directory (expand-file-name ".harmless-state" base))
         (session (harmless-output-style-test-session "hos-switch" root))
         (agents (expand-file-name "AGENTS.md" root))
         (harmless-md (expand-file-name ".harmless/output-styles/concise.md"
                                        root))
         (claude-md (expand-file-name ".claude/output-styles/concise.md"
                                      root))
         (agents-text "Follow HOS-INSTR.\n")
         (messages (list (list :role :user :content "hi")))
         (before nil))
    (unwind-protect
        (progn
          (harmless-output-style-test-write agents agents-text)
          (harmless-output-style-test-write
           claude-md
           "---\nname: concise\ndescription: Claude copy.\n---\nHOS-CLAUDE\n")
          (harmless-output-style-test-write
           harmless-md
           "---\nname: Concise\ndescription: Short answers.\n---\nHOS-HARMLESS\n")
          (harmless-output-style-test-write
           (expand-file-name ".harmless/output-styles/Notes.md" root)
           "---\nname: Notes\ndescription: Notes.\n---\nHOS-NOTES\n")
          (harmless-output-style-test-write
           (expand-file-name ".harmless/output-styles/empty.md" root)
           "---\nname: empty\ndescription: Empty.\n---\n")
          (harmless-output-style-test-write
           (expand-file-name ".harmless/output-styles/verbose.md" root)
           "---\r\nname: verbose\r\ndescription: Longer.\r\n---\r\nHOS-CRLF\r\n")
          (let ((early (harmless-output-style-test-system session root)))
            (should (string-prefix-p harmless-harness-prompt early))
            (should-not (string-match-p "HOS-HARMLESS" early))
            (should (string-match-p "HOS-INSTR" early)))
          (let ((style (harmless-output-style-set session "concise")))
            (should (equal "concise" (plist-get style :name)))
            (should (equal "HOS-HARMLESS" (plist-get style :body)))
            (should (equal "Short answers." (plist-get style :description))))
          (should (equal (harmless-output-style-test-read claude-md)
                         "---\nname: concise\ndescription: Claude copy.\n---\nHOS-CLAUDE\n"))
          (should (equal (harmless-output-style-test-read harmless-md)
                         "---\nname: Concise\ndescription: Short answers.\n---\nHOS-HARMLESS\n"))
          (should (equal (harmless-output-style-test-read agents) agents-text))
          (should-not (file-exists-p (expand-file-name "HARMLESS.md" root)))
          (should-not (file-exists-p (expand-file-name "CLAUDE.md" root)))
          (should-not (file-exists-p
                       (expand-file-name ".harmless/HARMLESS.md" root)))
          (let* ((copy (copy-tree messages))
                 (out (harmless-context-messages
                       root messages nil "grok-4.6" session))
                 (text (plist-get (car out) :content))
                 (style-at (string-match "Output style concise\\." text))
                 (instr-at (string-match
                            "Project instructions for this session" text)))
            (should (equal messages copy))
            (should (string-prefix-p harmless-harness-prompt text))
            (should style-at)
            (should instr-at)
            (should (< 0 style-at))
            (should (< style-at instr-at))
            (should (string-match-p "HOS-HARMLESS" text))
            (should-not (string-match-p "HOS-CLAUDE" text))
            (should (string-match-p "win when they disagree" text)))
          (let* ((dir (harmless-session-dir session))
                 (summary (expand-file-name "summary.json" dir)))
            (should (file-name-absolute-p dir))
            (should (harmless-directory-strictly-under-p dir harmless-directory))
            (should-not (equal (file-truename dir) (file-truename "/")))
            (setq before (harmless-output-style-test-read summary))
            (should (string-match-p "HOS-HARMLESS" before))
            (should (string-match-p "concise" before)))
          (setf (harmless-session-plan-mode session) t)
          (let ((text (plist-get
                       (car (harmless-turn--messages session)) :content)))
            (should (string-match-p "HOS-HARMLESS" text))
            (should (string-match-p "Plan mode is active" text)))
          (should (equal "output style body is empty"
                         (harmless-output-style-test-signal
                          (lambda ()
                            (harmless-output-style-set session "empty")))))
          (should (equal "HOS-HARMLESS"
                         (plist-get (harmless-output-style-active session) :body)))
          (should (equal before
                         (harmless-output-style-test-read
                          (expand-file-name
                           "summary.json" (harmless-session-dir session)))))
          (should (equal "no output style named hos-missing"
                         (harmless-output-style-test-signal
                          (lambda ()
                            (harmless-output-style-set
                             session "hos-missing")))))
          (should (equal "HOS-HARMLESS"
                         (plist-get (harmless-output-style-active session) :body)))
          (let ((style (harmless-output-style-set session "Notes")))
            (should (equal "notes" (plist-get style :name)))
            (should (equal "HOS-NOTES" (plist-get style :body))))
          (let ((style (harmless-output-style-set session "verbose")))
            (should (equal "HOS-CRLF" (plist-get style :body))))
          (should-not (string-match-p
                       "HOS-HARMLESS"
                       (harmless-output-style-test-read
                        (expand-file-name
                         "summary.json" (harmless-session-dir session)))))
          (should (null (harmless-output-style-set session "")))
          (should-not (harmless-output-style-active session))
          (should-not (string-match-p
                       "Output style"
                       (harmless-output-style-test-system session root)))
          (harmless-output-style-set session "concise")
          (should (null (harmless-output-style-set session " Default ")))
          (should-not (harmless-output-style-active session))
          (should (equal (harmless-output-style-test-read agents) agents-text)))
      (ignore-errors (delete-directory base t))
      (remhash "hos-switch" harmless-output-style-active))))

(ert-deftest harmless-output-style-refuses-a-bad-request ()
  (let* ((base (make-temp-file "harmless-style-bad-" t))
         (root (expand-file-name "proj" base))
         (harmless-directory (expand-file-name ".harmless-state" base))
         (session (harmless-output-style-test-session "hos-bad" root))
         (before nil))
    (unwind-protect
        (progn
          (harmless-output-style-test-write
           (expand-file-name ".harmless/output-styles/concise.md" root)
           "---\nname: concise\ndescription: Short.\n---\nHOS-PROJECT\n")
          (dolist (pair (list (cons "  " "output style name is empty")
                              (cons 'concise
                                    "output style name must be a string")
                              (cons "sub/" "output style name is not a name")
                              (cons "notes/" "output style name is not a name")
                              (cons "/" "output style name is not a name")
                              (cons "c:/" "output style name is not a name")
                              (cons "~" "output style name is not a name")
                              (cons ".." "output style name is not a name")))
            (should (equal (cdr pair)
                           (harmless-output-style-test-signal
                            (lambda ()
                              (harmless-output-style-set
                               session (car pair)))))))
          (should (equal "No Harmless session"
                         (harmless-output-style-test-signal
                          (lambda ()
                            (harmless-output-style-set nil "concise")))))
          (should (equal "no output style named hos-missing"
                         (harmless-output-style-test-signal
                          (lambda ()
                            (harmless-output-style-set
                             session "hos-missing")))))
          (let ((default-directory (file-name-as-directory root)))
            (setf (harmless-session-cwd session) "notes")
            (should (equal "output style directory must be absolute"
                           (harmless-output-style-test-signal
                            (lambda ()
                              (harmless-output-style-set session "concise")))))
            (should-not (harmless-output-style-active session)))
          (dolist (path '("/.claude" "/.harmless" "c:/.claude" "c:/.harmless"
                          "d:/.claude" "d:/.harmless"
                          "/output-styles" "/summary.json"))
            (push (cons path (file-exists-p path)) before))
          (dolist (dir (list "/" "c:/" "d:/" "c:\\" "C:/" "c:" "\\" "//" "/."))
            (setf (harmless-session-cwd session) dir)
            (should (equal "output style directory is the filesystem root"
                           (harmless-output-style-test-signal
                            (lambda ()
                              (harmless-output-style-set session "concise"))))))
          (dolist (pair before)
            (should (eq (cdr pair) (file-exists-p (car pair)))))
          (setf (harmless-session-cwd session) nil)
          (should (equal "no output style named concise"
                         (harmless-output-style-test-signal
                          (lambda ()
                            (harmless-output-style-set session "concise")))))
          (harmless-output-style-test-write
           (expand-file-name "output-styles/concise.md" harmless-directory)
           "---\nname: concise\ndescription: User.\n---\nHOS-USER\n")
          (let ((style (harmless-output-style-set session "concise")))
            (should (equal "HOS-USER" (plist-get style :body))))
          (let ((text (harmless-output-style-test-system session nil)))
            (should (string-match-p "HOS-USER" text))
            (should (string-match-p "detached chat" text))
            (should-not (string-match-p "HOS-PROJECT" text)))
          (let ((dir (harmless-session-dir session)))
            (should (harmless-directory-strictly-under-p dir harmless-directory))
            (should-not (equal (file-truename dir) (file-truename "/"))))
          (setf (harmless-session-cwd session) "")
          (should (equal "This chat is not attached to a project"
                         (harmless-output-style-test-signal
                          (lambda ()
                            (harmless-output-style-set session "verbose")))))
          (should (equal "HOS-USER"
                         (plist-get (harmless-output-style-active session) :body)))
          (setf (harmless-session-cwd session) "   ")
          (should (equal "This chat is not attached to a project"
                         (harmless-output-style-test-signal
                          (lambda ()
                            (harmless-output-style-set session "concise"))))))
      (ignore-errors (delete-directory base t))
      (remhash "hos-bad" harmless-output-style-active))))

(ert-deftest harmless-output-style-project-wins-and-is-kept ()
  (let* ((base (make-temp-file "harmless-style-keep-" t))
         (root (expand-file-name "proj" base))
         (harmless-directory (expand-file-name ".harmless-state" base))
         (provider (harmless-output-style-make-fake :name "fake" :host "none"))
         (detached (harmless-output-style-test-session "hos-user" nil))
         (session nil)
         (child nil)
         (project-file (expand-file-name
                        ".harmless/output-styles/concise.md" root))
         (user-file (expand-file-name
                     "output-styles/concise.md" harmless-directory)))
    (unwind-protect
        (progn
          (harmless-output-style-test-write
           user-file
           "---\nname: concise\ndescription: User.\n---\nHOS-USER-BODY\n")
          (harmless-output-style-test-write
           project-file
           "---\nname: concise\ndescription: Project.\n---\nHOS-PROJECT-BODY\n")
          (harmless-output-style-test-write
           (expand-file-name ".harmless/output-styles/hos-other.md" root)
           "---\nname: hos-other\ndescription: Other.\n---\nHOS-OTHER\n")
          (harmless-output-style-test-write
           (expand-file-name "AGENTS.md" root) "Keep this file.\n")
          (let ((style (harmless-output-style-set detached "concise")))
            (should (equal "HOS-USER-BODY" (plist-get style :body))))
          (should-not (harmless-output-style-find detached "hos-other"))
          (setq session
                (harmless-session-new
                 :id "hos-proj"
                 :cwd (file-name-as-directory root)
                 :provider provider
                 :model "grok-4.6"
                 :source 'agent))
          (let* ((dirs (harmless-output-style--dirs session))
                 (user (harmless-output-style--data-dir))
                 (project (car (last (harmless-output-style--project-dirs
                                      (harmless-output-style--cwd session))))))
            (should (and user (cl-position user dirs :test #'equal)))
            (should (< (cl-position user dirs :test #'equal)
                       (cl-position project dirs :test #'equal))))
          (let ((style (harmless-output-style-set session "concise")))
            (should (equal "HOS-PROJECT-BODY" (plist-get style :body))))
          (should (string-match-p
                   "HOS-USER-BODY"
                   (harmless-output-style-test-read user-file)))
          (let ((dir (harmless-session-dir session)))
            (remhash "hos-proj" harmless-output-style-active)
            (delete-file project-file)
            (setq session (harmless-session-load dir))
            (setf (harmless-session-provider session) provider))
          (should (equal "HOS-PROJECT-BODY"
                         (plist-get (harmless-output-style-active session)
                                    :body)))
          (let ((text (harmless-output-style-test-system session root)))
            (should (string-match-p "HOS-PROJECT-BODY" text)))
          (setq child (harmless-fork--create
                       session
                       (list (list :role :user :content "keep"))))
          (should-not (equal (harmless-session-id child)
                             (harmless-session-id session)))
          (should (equal "HOS-PROJECT-BODY"
                         (plist-get (harmless-output-style-active child)
                                    :body)))
          (should (equal (list (list :role :user :content "keep"))
                         (harmless-session-messages child)))
          (should (null (harmless-session-messages session)))
          (harmless-output-style-set session "hos-other")
          (should (equal "HOS-OTHER"
                         (plist-get (harmless-output-style-active session)
                                    :body)))
          (should (equal "HOS-PROJECT-BODY"
                         (plist-get (harmless-output-style-active child)
                                    :body)))
          (should (string-match-p
                   "HOS-PROJECT-BODY"
                   (harmless-output-style-test-read
                    (expand-file-name
                     "summary.json" (harmless-session-dir child)))))
          (should (equal "Keep this file.\n"
                         (harmless-output-style-test-read
                          (expand-file-name "AGENTS.md" root))))
          (let ((other (harmless-output-style-test-session "hos-else" root)))
            (should-not (harmless-output-style-active other))))
      (ignore-errors (delete-directory base t))
      (dolist (id '("hos-user" "hos-proj" "hos-else"))
        (remhash id harmless-output-style-active))
      (dolist (item (list detached session child))
        (when (harmless-session-p item)
          (harmless-session-unregister item)
          (remhash (harmless-session-id item)
                   harmless-output-style-active))))))
