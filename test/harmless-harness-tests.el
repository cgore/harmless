;;; harmless-harness-tests.el --- Tests for the standing prompt -*- lexical-binding: t; -*-

(require 'ert)
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
  (should (string-match-p "apply_patch applies several hunks"
                          harmless-harness-prompt))
  (should (string-match-p "notebook_edit replaces one notebook cell"
                          harmless-harness-prompt))
  (should (string-match-p "does not start a server" harmless-harness-prompt))
  (should (string-match-p "does not apply them" harmless-harness-prompt))
  (should (string-match-p
           "Claim a test passed only when a tool result says so"
           harmless-harness-prompt))
  (should (string-match-p
           "spawn_agent when a subtask needs its own transcript"
           harmless-harness-prompt))
  (should (string-match-p "A subagent cannot start another subagent"
                          harmless-harness-prompt))
  (should (string-match-p "run_workflow" harmless-harness-prompt))
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
      (should (string-match-p "write_plan" text))
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
