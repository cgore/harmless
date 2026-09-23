;;; harmless-turn-tests.el --- Tests for the Harmless turn loop -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless-provider)
(require 'harmless-openai)
(require 'harmless-session)
(require 'harmless-tools)
(require 'harmless-tools-fs)
(require 'harmless-perm)
(require 'harmless-turn)

(cl-defstruct (harmless-fake-provider
               (:include harmless-provider)
               (:constructor harmless-make-fake))
  script)

(cl-defmethod harmless-provider-complete ((provider harmless-fake-provider)
                                          messages tools callback)
  (ignore tools)
  (funcall (harmless-fake-provider-script provider) messages callback)
  nil)

(ert-deftest harmless-turn-tool-then-text ()
  (let* ((dir (make-temp-file "harmless-proj-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-perm-ask-function
          (lambda (_s _c _i cb) (funcall cb 'allow)))
         (calls 0)
         (provider
          (harmless-make-fake
           :name "fake"
           :host "none"
           :script
           (lambda (messages callback)
             (setq calls (1+ calls))
             (if (= calls 1)
                 (progn
                   (funcall callback '(:tool-call "1" "write_file"
                                       "{\"path\":\"x.txt\",\"contents\":\"hi\"}"))
                   (funcall callback '(:stop "tool_calls")))
               (should (cl-find-if
                        (lambda (m)
                          (memq (plist-get m :role) '(:tool tool)))
                        messages))
               (funcall callback '(:text "wrote it"))
               (funcall callback '(:stop "stop"))))))
         (harmless-providers (list provider))
         (session (harmless-session-new :cwd dir :provider provider :model "m")))
    (harmless-turn-run session "please write")
    (should (= calls 2))
    (should (eq 'idle (harmless-session-status session)))
    (should (file-exists-p (expand-file-name "x.txt" dir)))
    (should (string= "hi"
                     (with-temp-buffer
                       (insert-file-contents (expand-file-name "x.txt" dir))
                       (buffer-string))))
    (let ((roles (mapcar (lambda (m) (plist-get m :role))
                         (harmless-session-messages session))))
      (should (equal '(:user :assistant :tool :assistant) roles)))))

(ert-deftest harmless-turn-counts-latest-usage-once ()
  (let* ((dir (make-temp-file "harmless-usage-turn-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (provider
          (harmless-make-fake
           :name "xAI"
           :host "none"
           :script
           (lambda (_messages callback)
             (funcall callback '(:usage 41 1))
             (funcall callback '(:usage 41 12))
             (funcall callback '(:limits (:tokens-remaining "80"
                                           :tokens-limit "100")))
             (funcall callback '(:text "ok"))
             (funcall callback '(:stop "stop")))))
         (harmless-providers (list provider))
         (session (harmless-session-new :cwd dir :provider provider :model "grok-4.6")))
    (harmless-turn-run session "hi")
    (should (= 41 (harmless-session-prompt-tokens session)))
    (should (= 12 (harmless-session-completion-tokens session)))
    (should (= 41 (harmless-session-last-prompt-tokens session)))
    (let ((limits (harmless-usage-load-limits)))
      (should (equal "80" (plist-get (car limits) :tokens-remaining))))))

(ert-deftest harmless-turn-denied-tool ()
  (let* ((dir (make-temp-file "harmless-proj-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-perm-ask-function
          (lambda (_s _c _i cb) (funcall cb 'deny)))
         (calls 0)
         (provider
          (harmless-make-fake
           :name "fake"
           :host "none"
           :script
           (lambda (messages callback)
             (setq calls (1+ calls))
             (if (cl-find-if (lambda (m) (memq (plist-get m :role) '(:tool tool)))
                             messages)
                 (progn
                   (funcall callback '(:text "ok"))
                   (funcall callback '(:stop "stop")))
               (funcall callback '(:tool-call "1" "write_file"
                                   "{\"path\":\"nope.txt\",\"contents\":\"x\"}"))
               (funcall callback '(:stop "tool_calls"))))))
         (harmless-providers (list provider))
         (session (harmless-session-new :cwd dir :provider provider :model "m")))
    (harmless-turn-run session "write")
    (should-not (file-exists-p (expand-file-name "nope.txt" dir)))
    (should (string-match-p "denied"
                            (plist-get
                             (cl-find-if (lambda (m)
                                           (memq (plist-get m :role) '(:tool tool)))
                                         (harmless-session-messages session))
                             :content)))))

(ert-deftest harmless-turn-sends-project-instructions ()
  (let* ((dir (make-temp-file "harmless-proj-" t))
         (harmless-directory (expand-file-name ".harmless-state" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-perm-ask-function
          (lambda (_s _c _i cb) (funcall cb 'allow)))
         (seen nil)
         (provider
          (harmless-make-fake
           :name "fake"
           :host "none"
           :script
           (lambda (messages callback)
             (setq seen messages)
             (funcall callback '(:text "ok"))
             (funcall callback '(:stop "stop")))))
         (harmless-providers (list provider))
         (session (harmless-session-new :cwd dir :provider provider :model "m")))
    (make-directory (expand-file-name ".harmless" dir))
    (with-temp-file (expand-file-name "HARMLESS.md" dir)
      (insert "Use two spaces.\n"))
    (with-temp-file (expand-file-name ".harmless/HARMLESS.md" dir)
      (insert "Prefer the dotfile.\n"))
    (harmless-turn-run session "hello")
    (let ((text (plist-get (car seen) :content)))
      (should (eq :system (plist-get (car seen) :role)))
      (should (string-match-p "Use two spaces" text))
      (should (string-match-p "Prefer the dotfile" text))
      (should (< (string-match "Use two spaces" text)
                 (string-match "Prefer the dotfile" text))))
    (should-not (cl-find-if #'harmless-message-system-p
                            (harmless-session-messages session)))))

(provide 'harmless-turn-tests)
