;;; harmless-instructions-tests.el --- Tests for project instructions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless-instructions)
(require 'harmless-openai)
(require 'harmless-anthropic)
(require 'harmless-skills)

(defun harmless-instructions-test-write (path text)
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert text)))

(ert-deftest harmless-instruction-files-outermost-first ()
  (let* ((base (make-temp-file "harmless-instr-" t))
         (root (expand-file-name "proj" base))
         (child (expand-file-name "sub" root)))
    (harmless-instructions-test-write
     (expand-file-name "HARMLESS.md" base) "above-stop\n")
    (harmless-instructions-test-write
     (expand-file-name "HARMLESS.md" root) "parent-top\n")
    (harmless-instructions-test-write
     (expand-file-name ".harmless/HARMLESS.md" root) "parent-dot\n")
    (harmless-instructions-test-write
     (expand-file-name "HARMLESS.md" child) "child-top\n")
    (harmless-instructions-test-write
     (expand-file-name ".harmless/HARMLESS.md" child) "child-dot\n")
    (let ((files (harmless-instruction-files child root)))
      (should (equal (mapcar #'file-name-nondirectory files)
                     '("HARMLESS.md" "HARMLESS.md" "HARMLESS.md" "HARMLESS.md")))
      (should (equal (mapcar (lambda (path)
                               (if (string-match-p "/\\.harmless/" path)
                                   'dot
                                 'top))
                             files)
                     '(top dot top dot)))
      (should (string-prefix-p (file-name-as-directory root)
                               (car files)))
      (should (string-prefix-p (file-name-as-directory child)
                               (car (last files)))))
    (let ((text (harmless-instructions-text child root)))
      (should (string-match-p "parent-top" text))
      (should (string-match-p "parent-dot" text))
      (should (string-match-p "child-top" text))
      (should (string-match-p "child-dot" text))
      (should-not (string-match-p "above-stop" text))
      (should (< (string-match "parent-top" text)
                 (string-match "parent-dot" text)))
      (should (< (string-match "parent-dot" text)
                 (string-match "child-top" text)))
      (should (< (string-match "child-top" text)
                 (string-match "child-dot" text))))
    (let ((empty (make-temp-file "harmless-empty-" t)))
      (should-not (harmless-instructions-text empty empty)))))

(ert-deftest harmless-instructions-agents-before-harmless ()
  (let* ((root (make-temp-file "harmless-agents-" t))
         (child (expand-file-name "pkg" root)))
    (harmless-instructions-test-write
     (expand-file-name "AGENTS.md" root) "root-agents\n")
    (harmless-instructions-test-write
     (expand-file-name "HARMLESS.md" root) "root-harmless\n")
    (harmless-instructions-test-write
     (expand-file-name "AGENTS.md" child) "child-agents\n")
    (let ((text (harmless-instructions-text child root)))
      (should (< (string-match "root-agents" text)
                 (string-match "root-harmless" text)))
      (should (< (string-match "root-harmless" text)
                 (string-match "child-agents" text))))))

(ert-deftest harmless-instructions-skip-blank-files ()
  (let* ((root (make-temp-file "harmless-blank-" t)))
    (harmless-instructions-test-write
     (expand-file-name "HARMLESS.md" root) "  \n")
    (harmless-instructions-test-write
     (expand-file-name ".harmless/HARMLESS.md" root) "keep this\n")
    (let ((text (harmless-instructions-text root root)))
      (should (string-match-p "keep this" text))
      (should-not (string-match-p
                   (regexp-quote (expand-file-name "HARMLESS.md" root))
                   text)))))

(ert-deftest harmless-instructions-reach-providers ()
  (let ((messages '((:role :system :content "Use two spaces.")
                    (:role :user :content "hi"))))
    (let ((chat (harmless-json-decode
                 (harmless-openai--payload nil "grok-4.6" messages nil nil))))
      (should (equal "system"
                     (plist-get (car (plist-get chat :messages)) :role)))
      (should (string-match-p "two spaces"
                              (plist-get (car (plist-get chat :messages))
                                         :content))))
    (let ((responses (harmless-json-decode
                      (harmless-openai--responses-payload
                       nil "gpt-5.4" messages nil t))))
      (should (string-match-p "two spaces" (plist-get responses :instructions)))
      (should-not (cl-find "system"
                           (plist-get responses :input)
                           :key (lambda (item) (plist-get item :role))
                           :test #'equal)))
    (let ((anthropic (harmless-json-decode
                      (harmless-anthropic--payload
                       nil "claude-sonnet-4-6" messages nil t))))
      (should (string-match-p "two spaces" (plist-get anthropic :system)))
      (should (equal "user"
                     (plist-get (car (plist-get anthropic :messages)) :role))))))

(ert-deftest harmless-claude-md-follows-the-setting ()
  (let* ((root (make-temp-file "harmless-claude-" t))
         (child (file-name-as-directory (expand-file-name "sub" root)))
         (claude (expand-file-name "CLAUDE.md" root))
         (agents (expand-file-name "AGENTS.md" root))
         (above (expand-file-name "CLAUDE.md" (file-name-directory
                                               (directory-file-name root))))
         (root-claude-existed (file-exists-p "/CLAUDE.md")))
    (harmless-instructions-test-write claude "claude-rules\n")
    (harmless-instructions-test-write agents "agents-rules\n")
    (harmless-instructions-test-write
     (expand-file-name "CLAUDE.md" child) "   \n")
    (should-not (file-exists-p above))
    (let ((harmless-claude-md t))
      (let ((text (harmless-instructions-text child root nil)))
        (should (string-match-p "claude-rules" text))
        (should (string-match-p "agents-rules" text))
        (should (< (string-match "claude-rules" text)
                   (string-match "agents-rules" text)))
        (should-not (string-match-p
                    (regexp-quote (expand-file-name "CLAUDE.md" child))
                    text)))
      (should (equal (harmless-instruction-files
                      (file-name-as-directory root) root "grok-4.6")
                     (harmless-instruction-files
                      (directory-file-name root) root "grok-4.6")))
      (let ((default-directory root))
        (should (string-match-p "claude-rules"
                                (harmless-instructions-text "sub/" root nil)))
        (should (string-match-p "claude-rules"
                                (harmless-instructions-text "sub" root nil))))
      (harmless-instructions-text "/" "/")
      (should (eq root-claude-existed (file-exists-p "/CLAUDE.md"))))
    (let ((harmless-claude-md nil))
      (let ((text (harmless-instructions-text root root "claude-opus-5")))
        (should (string-match-p "agents-rules" text))
        (should-not (string-match-p "claude-rules" text))))
    (let ((harmless-claude-md 'anthropic)
          (harmless-current-model nil))
      (dolist (model '("claude-sonnet-4-6" "claude-haiku-4-5" "claude-opus-5"))
        (should (string-match-p
                 "claude-rules"
                 (harmless-instructions-text root root model))))
      (dolist (model '(nil "" "claude" "Claude-opus-5" "grok-4.6" "gpt-5.4"))
        (let ((text (harmless-instructions-text root root model)))
          (should (string-match-p "agents-rules" text))
          (should-not (string-match-p "claude-rules" text)))))
    (let ((harmless-claude-md 'always)
          (harmless-current-model "claude-opus-5"))
      (should-not (harmless-claude-md-p nil))
      (should-not (string-match-p
                   "claude-rules"
                   (harmless-instructions-text root root))))
    (let ((harmless-claude-md 'anthropic)
          (harmless-current-model "claude-sonnet-4-6"))
      (should (harmless-claude-md-p nil))
      (should (string-match-p "claude-rules"
                              (harmless-instructions-text root root))))
    (let ((harmless-claude-md 'anthropic)
          (harmless-current-model "grok-4.6"))
      (should-not (harmless-claude-md-p nil))
      (let ((messages (harmless-context-messages root nil root "grok-4.6")))
        (should-not (string-match-p "claude-rules"
                                    (harmless-messages-system-text messages))))
      (let ((messages (harmless-context-messages
                       root nil root "claude-sonnet-4-6")))
        (should (string-match-p "claude-rules"
                                (harmless-messages-system-text messages)))))
    (should-not (file-exists-p above))
    (should (eq root-claude-existed (file-exists-p "/CLAUDE.md")))))
