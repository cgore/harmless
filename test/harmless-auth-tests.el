;;; harmless-auth-tests.el --- Tests for Harmless login dispatch -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless-auth)
(require 'harmless-openai)
(require 'harmless-anthropic)

(ert-deftest harmless-login-register-and-dispatch ()
  (let ((harmless-login-methods nil)
        (got nil))
    (harmless-register-login-method
     'xai
     :name "xAI"
     :login (lambda (prefix) (setq got prefix) 'in)
     :logout (lambda () (setq got 'out)))
    (should (eq 'xai (caar harmless-login-methods)))
    (should (eq 'in (harmless-login)))
    (should (null got))
    (should (eq 'in (harmless-login 'xai 'device)))
    (should (eq 'device got))
    (harmless-logout)
    (should (eq 'out got))))

(ert-deftest harmless-available-providers-includes-logged-in-anthropic ()
  (let* ((harmless-directory (make-temp-file "harmless-providers-" t))
         (harmless-providers
          (list (harmless-make-xai :key nil :key-env "HARMLESS_NO_SUCH_XAI")))
         (harmless-xai-use-grok-auth nil)
         (harmless-anthropic-use-claude-auth nil)
         (harmless-login-methods nil)
         (harmless-oauth-provider nil))
    (harmless-register-login-method
     'anthropic :name "Anthropic" :login #'ignore :logout #'ignore
     :logged-in-p (lambda () (and (harmless-anthropic--read-store) t)))
    (harmless-anthropic--write-store
     (list :access-token "tok-anthropic" :refresh-token "r"
           :expires-at "2099-01-01T00:00:00Z"))
    (let ((names (mapcar #'harmless-provider-name
                         (harmless-available-providers))))
      (should (equal '("Anthropic") names))
      (should (member "claude-sonnet-4-6"
                      (harmless-provider-model-list
                       (car (harmless-available-providers))))))))

(ert-deftest harmless-login-offers-vendors-beside-one-connection ()
  (let ((harmless-login-methods nil)
        (harmless-providers (list (harmless-make-xai))))
    (harmless-register-login-method
     'xai :name "xAI" :login #'ignore :logout #'ignore)
    (harmless-register-login-method
     'anthropic :name "Anthropic" :login #'ignore :logout #'ignore)
    (should (equal '("Anthropic" "xAI")
                   (mapcar #'car (harmless-login--choices))))))

(ert-deftest harmless-login-picks-when-several ()
  (let ((harmless-login-methods nil)
        (got nil))
    (harmless-register-login-method
     'xai :name "xAI" :login (lambda (_) (setq got 'xai))
     :logout #'ignore)
    (harmless-register-login-method
     'openai :name "OpenAI" :login (lambda (_) (setq got 'openai))
     :logout #'ignore)
    (harmless-login 'openai)
    (should (eq 'openai got))
    (should-error (harmless-login 'nope) :type 'user-error)))

(ert-deftest harmless-connection-slug ()
  (should (string= "xai-work" (harmless-connection-slug "xAI work")))
  (should (string= "anthropic-work" (harmless-connection-slug "Anthropic work"))))

(ert-deftest harmless-connection-auth-files-are-per-name ()
  (let* ((harmless-directory (make-temp-file "harmless-auth-" t))
         (a (harmless-make-xai "xAI"))
         (b (harmless-make-xai "xAI work"))
         (c (harmless-make-anthropic "Anthropic"))
         (d (harmless-make-anthropic "Anthropic work")))
    (should (string-suffix-p "auth.json" (harmless-xai-auth-file a)))
    (should (string-match-p "auth-xai-work\\.json\\'" (harmless-xai-auth-file b)))
    (should (string-suffix-p "auth-anthropic.json" (harmless-anthropic-auth-file c)))
    (should (string-match-p "auth-anthropic-work\\.json\\'"
                            (harmless-anthropic-auth-file d)))
    (should-not (equal (harmless-xai-auth-file a) (harmless-xai-auth-file b)))))

(ert-deftest harmless-xai-tokens-are-per-connection ()
  (let* ((harmless-directory (make-temp-file "harmless-auth-" t))
         (a (harmless-make-xai "xAI"))
         (b (harmless-make-xai "xAI work"))
         (harmless-xai-use-grok-auth nil)
         (harmless-oauth-provider nil))
    (let ((harmless-oauth-provider a))
      (harmless-xai--write-store
       (list :access-token "tok-a" :refresh-token "r"
             :expires-at "2099-01-01T00:00:00Z"
             :issuer "https://auth.x.ai" :client-id "c")))
    (let ((harmless-oauth-provider b))
      (harmless-xai--write-store
       (list :access-token "tok-b" :refresh-token "r"
             :expires-at "2099-01-01T00:00:00Z"
             :issuer "https://auth.x.ai" :client-id "c")))
    (should (string= "tok-a" (harmless-xai-token a)))
    (should (string= "tok-b" (harmless-xai-token b)))
    (should (file-exists-p (harmless-xai-auth-file a)))
    (should (file-exists-p (harmless-xai-auth-file b)))))

(ert-deftest harmless-login-accepts-provider ()
  (let ((harmless-login-methods nil)
        (got nil)
        (p (harmless-make-xai "xAI work")))
    (harmless-register-login-method
     'xai :name "xAI"
     :login (lambda (_) (setq got harmless-oauth-provider))
     :logout #'ignore)
    (harmless-login p)
    (should (eq p got))))

(ert-deftest harmless-available-p-is-per-connection ()
  (let* ((harmless-directory (make-temp-file "harmless-auth-" t))
         (a (harmless-make-xai "xAI" :key nil :key-env "HARMLESS_NO_SUCH_XAI_A"))
         (b (harmless-make-xai "xAI work" :key nil :key-env "HARMLESS_NO_SUCH_XAI_B"))
         (harmless-xai-use-grok-auth nil))
    (cl-letf (((symbol-function 'harmless-auth-key) (lambda (&rest _) nil)))
      (should-not (harmless-provider-available-p a))
      (should-not (harmless-provider-available-p b))
      (let ((harmless-oauth-provider a))
        (harmless-xai--write-store
         (list :access-token "tok-a" :refresh-token "r"
               :expires-at "2099-01-01T00:00:00Z"
               :issuer "https://auth.x.ai" :client-id "c")))
      (should (harmless-provider-available-p a))
      (should-not (harmless-provider-available-p b)))))

(provide 'harmless-auth-tests)
