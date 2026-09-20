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

(provide 'harmless-turn-tests)
