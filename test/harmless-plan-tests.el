;;; harmless-plan-tests.el --- Tests for Harmless plan mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'harmless-openai)
(require 'harmless-plan)
(require 'harmless-tools-fs)
(require 'harmless-skills)

(defun harmless-plan-test-session (dir)
  (let ((provider (harmless-make-openai-compat
                   "local" :host "127.0.0.1:9" :protocol "http"
                   :key "none" :models '("m")))
        (harmless-providers nil)
        (harmless--sessions (make-hash-table :test 'equal))
        (harmless-directory (expand-file-name ".harmless" dir)))
    (setq harmless-providers (list provider))
    (harmless-session-new :cwd dir :provider provider :model "m")))

(ert-deftest harmless-plan-blocks-project-edits ()
  (let* ((dir (make-temp-file "harmless-plan-" t))
         (session (harmless-plan-test-session dir)))
    (harmless-plan-enter session)
    (should (harmless-session-plan-mode session))
    (should-error (harmless-tools-fs--write
                   session '(:path "a.txt" :contents "nope"))
                  :type 'error)
    (should-error (harmless-tools-fs--replace
                   session '(:path "a.txt" :old_string "a" :new_string "b")))
    (should (string-match-p "Wrote plan.md"
                            (harmless-plan--tool-write
                             session '(:contents "# Plan\n\nDo the thing.\n"))))
    (should (string-match-p "Do the thing"
                            (harmless-plan--read session)))
    (let ((harmless-plan-decide-function (lambda (_session _plan) 'approve)))
      (should (string-match-p "Approved"
                              (harmless-plan--tool-exit session nil))))
    (should-not (harmless-session-plan-mode session))
    (should (string-match-p "Wrote"
                            (harmless-tools-fs--write
                             session '(:path "a.txt" :contents "ok"))))))

(ert-deftest harmless-plan-revise-keeps-mode ()
  (let* ((dir (make-temp-file "harmless-plan-revise-" t))
         (session (harmless-plan-test-session dir))
         (harmless-plan-decide-function
          (lambda (_session _plan) "Mention the tests.")))
    (harmless-plan-enter session)
    (harmless-plan--tool-write session '(:contents "# Plan\n\nSketch.\n"))
    (should (string-match-p "Mention the tests"
                            (harmless-plan--tool-exit session nil)))
    (should (harmless-session-plan-mode session))
    (should-error (harmless-plan--tool-write session '(:contents "")))))

(ert-deftest harmless-plan-persists ()
  (let* ((dir (make-temp-file "harmless-plan-save-" t))
         (session (harmless-plan-test-session dir))
         (id (harmless-session-id session)))
    (harmless-plan-enter session)
    (setq harmless--sessions (make-hash-table :test 'equal))
    (let ((loaded (harmless-session-load (harmless-session-directory session))))
      (should (string= id (harmless-session-id loaded)))
      (should (harmless-session-plan-mode loaded)))))

(ert-deftest harmless-plan-prompt-when-active ()
  (let* ((dir (make-temp-file "harmless-plan-prompt-" t))
         (session (harmless-plan-test-session dir)))
    (harmless-plan-enter session)
    (let ((messages (harmless-context-append
                     (harmless-context-messages dir nil dir)
                     (when (harmless-session-plan-mode session)
                       (harmless-plan-instructions)))))
      (should (string-match-p "write_plan"
                              (plist-get (car messages) :content)))
      (should (string-match-p "exit_plan_mode"
                              (plist-get (car messages) :content))))))
