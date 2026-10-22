;;; harmless-harness-tests.el --- Tests for the standing prompt -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless-harness)
(require 'harmless-skills)
(require 'harmless-plan)
(require 'harmless-turn)

(defun harmless-harness-test-write (path text)
  "Write TEXT to PATH with Unix newlines."
  (make-directory (file-name-directory path) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file path
      (insert text))))

(defun harmless-harness-test-system (messages)
  "Return the first system text in MESSAGES, or an empty string."
  (or (harmless-messages-system-text messages) ""))

(ert-deftest harmless-harness-prompt-is-standing-policy ()
  (should (string-match-p "Tool policy" harmless-harness-prompt))
  (should (string-match-p "When to verify" harmless-harness-prompt))
  (should (string-match-p "When to delegate" harmless-harness-prompt))
  (should (string-match-p "Do not invent file contents" harmless-harness-prompt))
  (should (string-match-p "harmless-tool-apply-patch applies several hunks"
                          harmless-harness-prompt))
  (should (string-match-p "harmless-tool-notebook-edit replaces one notebook cell"
                          harmless-harness-prompt))
  (should (string-match-p "does not start a server" harmless-harness-prompt))
  (should (string-match-p "does not apply them" harmless-harness-prompt))
  (should (string-match-p
           "Claim a test passed only when a tool result says so"
           harmless-harness-prompt))
  (should (string-match-p
           "harmless-tool-spawn-agent when a subtask needs its own transcript"
           harmless-harness-prompt))
  (should (string-match-p "A subagent cannot start another subagent"
                          harmless-harness-prompt))
  (should (string-match-p "harmless-tool-run-workflow" harmless-harness-prompt))
  (should (string-match-p "When they disagree with it, follow them"
                          harmless-harness-prompt))
  (let* ((prior '((:role :system :content "PROJECT-RULE" :extra "keep")
                  (:role :user :content "hi")))
         (copy (copy-tree prior))
         (out (harmless-harness-prepend prior))
         (text (plist-get (car out) :content)))
    (should (equal prior copy))
    (should (eq :system (plist-get (car out) :role)))
    (should (equal "keep" (plist-get (car out) :extra)))
    (should (string-prefix-p harmless-harness-prompt text))
    (should (< (string-match "When to delegate" text)
               (string-match "PROJECT-RULE" text)))
    (should (equal '(:user) (mapcar (lambda (msg) (plist-get msg :role))
                                    (cdr out)))))
  (let ((only (harmless-harness-prepend nil)))
    (should (equal harmless-harness-prompt
                   (plist-get (car only) :content)))
    (should-not (cdr only)))
  (let ((odd (harmless-harness-prepend "notes")))
    (should (equal harmless-harness-prompt
                   (plist-get (car odd) :content)))))

(ert-deftest harmless-harness-prompt-goes-out-on-every-turn ()
  (let* ((base (make-temp-file "harmless-harness-" t))
         (root (expand-file-name "proj" base))
         (harmless-directory (expand-file-name ".harmless-state" base))
         (slash-claude (file-exists-p "/.claude"))
         (drive-claude (file-exists-p "c:/.claude"))
         (messages '((:role :user :content "hello"))))
    (harmless-harness-test-write
     (expand-file-name "HARMLESS.md" root) "PROJECT-RULE\n")
    (let ((default-directory (file-name-as-directory root)))
      (dolist (dir (list nil "" "   " "notes" "sub/" "." "./"))
        (let* ((copy (copy-tree messages))
               (out (harmless-context-messages dir messages))
               (text (harmless-harness-test-system out)))
          (should (equal messages copy))
          (should (string-prefix-p harmless-harness-prompt text))
          (should (string-match-p "detached chat" text))
          (should (< (string-match "When to delegate" text)
                     (string-match "detached chat" text)))
          (should-not (string-match-p "PROJECT-RULE" text)))))
    (let* ((copy (copy-tree messages))
           (out (harmless-context-messages
                 (file-name-as-directory root) messages))
           (text (harmless-harness-test-system out)))
      (should (equal messages copy))
      (should (string-prefix-p harmless-harness-prompt text))
      (should (string-match-p "PROJECT-RULE" text))
      (should (< (string-match "When to delegate" text)
                 (string-match "PROJECT-RULE" text)))
      (should-not (string-match-p "detached chat" text)))
    (let* ((session (harmless-session--create
                     :id "harness-test"
                     :cwd root
                     :model "grok-4.6"
                     :source 'agent
                     :messages messages
                     :plan-mode nil))
           (sent (harmless-turn--messages session))
           (text (harmless-harness-test-system sent)))
      (should (equal messages (harmless-session-messages session)))
      (should (string-prefix-p harmless-harness-prompt text))
      (should (string-match-p "PROJECT-RULE" text))
      (should-not (string-match-p "Plan mode is active" text))
      (should (equal '(:user)
                     (mapcar (lambda (msg) (plist-get msg :role))
                             (cdr sent)))))
    (let* ((session (harmless-session--create
                     :id "harness-plan"
                     :cwd root
                     :model "grok-4.6"
                     :source 'agent
                     :messages messages
                     :plan-mode t))
           (text (harmless-harness-test-system
                  (harmless-turn--messages session))))
      (should (string-prefix-p harmless-harness-prompt text))
      (should (string-match-p "PROJECT-RULE" text))
      (should (string-match-p "Plan mode is active" text))
      (should (string-match-p "harmless-tool-write-plan" text))
      (should (< (string-match "PROJECT-RULE" text)
                 (string-match "Plan mode is active" text))))
    (let* ((detached (harmless-session--create
                      :id "harness-detached"
                      :cwd nil
                      :model "grok-4.6"
                      :source 'agent
                      :messages messages
                      :plan-mode nil))
           (text (harmless-harness-test-system
                  (harmless-turn--messages detached))))
      (should (string-prefix-p harmless-harness-prompt text))
      (should (string-match-p "detached chat" text))
      (should-not (string-match-p "PROJECT-RULE" text)))
    (should (eq slash-claude (file-exists-p "/.claude")))
    (should (eq drive-claude (file-exists-p "c:/.claude")))
    (should (string= "PROJECT-RULE\n"
                     (with-temp-buffer
                       (let ((coding-system-for-read 'utf-8-unix))
                         (insert-file-contents
                          (expand-file-name "HARMLESS.md" root)))
                       (buffer-string))))))

(defun harmless-harness-test-signal (fn)
  "Return the error text from FN."
  (error-message-string (should-error (funcall fn) :type 'error)))

(ert-deftest harmless-soul-is-the-identity-block ()
  (let* ((base (make-temp-file "harmless-soul-" t))
         (root (expand-file-name "proj" base))
         (harmless-directory (expand-file-name ".harmless-state" base))
         (messages '((:role :user :content "hello")))
         (soul (expand-file-name "SOUL.md" harmless-directory)))
    (make-directory root t)
    (harmless-harness-test-write
     (expand-file-name "SOUL.md" root) "project-soul\n")
    (harmless-harness-test-write
     (expand-file-name "HARMLESS.md" root) "PROJECT-RULE\n")
    (let* ((copy (copy-tree messages))
           (out (harmless-context-messages root messages root))
           (text (harmless-harness-test-system out)))
      (should (equal messages copy))
      (should (string-prefix-p harmless-harness-prompt text))
      (should (string-match-p "PROJECT-RULE" text))
      (should-not (string-match-p "project-soul" text))
      (should-not (file-exists-p soul)))
    (harmless-harness-test-write soul "Be brief.\n")
    (should (equal "Be brief.\n"
                   (with-temp-buffer
                     (let ((coding-system-for-read 'utf-8-unix))
                       (insert-file-contents soul))
                     (buffer-string))))
    (should (equal "Be brief." (harmless-soul-text)))
    (let* ((prior '((:role :system :content "PROJECT-RULE" :extra "keep")
                    (:role :user :content "hi")))
           (copy (copy-tree prior))
           (out (harmless-soul-prepend prior))
           (text (plist-get (car out) :content)))
      (should (equal prior copy))
      (should (equal "keep" (plist-get (car out) :extra)))
      (should (string-prefix-p "Be brief." text))
      (should (< (string-match (regexp-quote "Be brief.") text)
                 (string-match "PROJECT-RULE" text))))
    (let ((text (harmless-harness-test-system
                 (harmless-context-messages
                  (file-name-as-directory root) messages root))))
      (should (string-prefix-p "Be brief." text))
      (should (< (string-match (regexp-quote "Be brief.") text)
                 (string-match "Harmless harness policy" text)))
      (should (< (string-match "When to delegate" text)
                 (string-match "PROJECT-RULE" text)))
      (should-not (string-match-p "project-soul" text)))
    (harmless-memory-write-user "Likes short answers.")
    (let ((text (harmless-harness-test-system
                 (harmless-context-messages root messages root))))
      (should (< (string-match (regexp-quote "Be brief.") text)
                 (string-match "Harmless harness policy" text)))
      (should (< (string-match "Harmless harness policy" text)
                 (string-match "User profile:" text)))
      (should (string-match-p "Likes short answers." text))
      (should-not (string-match-p "project-soul" text)))
    (let ((text (harmless-harness-test-system
                 (harmless-context-messages nil messages))))
      (should (string-prefix-p "Be brief." text))
      (should (string-match-p "detached chat" text))
      (should (string-match-p "Likes short answers." text))
      (should-not (string-match-p "PROJECT-RULE" text))
      (should-not (string-match-p "project-soul" text)))
    (harmless-harness-test-write soul "  \n")
    (should-not (harmless-soul-text))
    (should (string-prefix-p
             harmless-harness-prompt
             (harmless-harness-test-system
              (harmless-context-messages root messages root))))
    (delete-file soul)
    (make-directory soul t)
    (should (equal "SOUL.md is a directory"
                   (harmless-harness-test-signal #'harmless-soul-text)))
    (should (equal "SOUL.md is a directory"
                   (harmless-harness-test-signal
                    (lambda () (harmless-context-messages root messages root)))))
    (should (file-directory-p soul))
    (should-not (file-regular-p soul))))

(ert-deftest harmless-soul-refuses-empty-relative-and-roots ()
  (let ((paths '("/SOUL.md" "/memory" "c:/SOUL.md" "c:/memory"
                 "c:\\SOUL.md" "c:\\memory" "C:/SOUL.md"))
        (before nil)
        (home-soul (file-exists-p (expand-file-name "~/SOUL.md"))))
    (dolist (path paths)
      (push (cons path (file-exists-p path)) before))
    (dolist (bad (list nil "" "   " 42))
      (should (equal "SOUL.md path is empty"
                     (harmless-harness-test-signal
                      (lambda () (harmless-soul-resolve bad))))))
    (dolist (bad (list "/" "c:/" "c:\\" "C:/" "\\" "c:" "//"))
      (should (equal "SOUL.md path is the filesystem root"
                     (harmless-harness-test-signal
                      (lambda () (harmless-soul-resolve bad))))))
    (let ((missing (expand-file-name
                    "SOUL.md" (make-temp-file "harmless-soul-miss-" t)))
          (dir (make-temp-file "harmless-soul-dir-" t)))
      (should (equal missing (harmless-soul-resolve missing)))
      (should-not (file-exists-p missing))
      (should (equal "SOUL.md is a directory"
                     (harmless-harness-test-signal
                      (lambda () (harmless-soul-resolve dir)))))
      (should (file-directory-p dir)))
    (let ((default-directory (file-name-as-directory
                              (make-temp-file "harmless-soul-rel-" t))))
      (dolist (bad (list "SOUL.md" "notes/SOUL.md" "~/SOUL.md" "~"))
        (should (equal "SOUL.md path must be absolute"
                       (harmless-harness-test-signal
                        (lambda () (harmless-soul-resolve bad))))))
      (should-not (file-exists-p (expand-file-name "SOUL.md" default-directory)))
      (should-not (file-exists-p (expand-file-name "notes" default-directory))))
    (should (eq home-soul (file-exists-p (expand-file-name "~/SOUL.md"))))
    (dolist (dir (list "" "   "))
      (let ((harmless-directory dir))
        (should (equal "SOUL.md path is empty"
                       (harmless-harness-test-signal #'harmless-soul-file)))))
    (dolist (dir (list "/" "c:/" "c:\\" "C:/" "\\" "c:" "//" "/."))
      (let ((harmless-directory dir))
        (should (equal "SOUL.md path is the filesystem root"
                       (harmless-harness-test-signal #'harmless-soul-file)))
        (should (equal "SOUL.md path is the filesystem root"
                       (harmless-harness-test-signal #'harmless-soul-text)))))
    (let ((default-directory (file-name-as-directory
                              (make-temp-file "harmless-soul-dirrel-" t)))
          (harmless-directory "notes"))
      (should (equal "Harmless data directory must be absolute, not \"notes\""
                     (harmless-harness-test-signal #'harmless-soul-file)))
      (should-not (file-exists-p (expand-file-name "notes" default-directory)))
      (should-not (file-exists-p (expand-file-name "SOUL.md" default-directory))))
    (let* ((fallback (make-temp-file "harmless-soul-nil-" t))
           (harmless-directory nil))
      (cl-letf (((symbol-function 'locate-user-emacs-file)
                 (lambda (&rest _) fallback)))
        (let ((path (harmless-soul-file)))
          (should (equal (expand-file-name "SOUL.md" fallback) path))
          (should-not (file-exists-p path))
          (harmless-harness-test-write path "From nil dir.\n")
          (should (equal "From nil dir." (harmless-soul-text)))
          (should (string-prefix-p
                   "From nil dir."
                   (harmless-harness-test-system
                    (harmless-context-messages nil nil)))))))
    (dolist (pair before)
      (should (eq (cdr pair) (file-exists-p (car pair)))))))
