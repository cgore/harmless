;;; harmless-auth-tests.el --- Tests for Harmless login dispatch -*- lexical-binding: t; -*-

(require 'ert)
(require 'harmless-auth)

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

(provide 'harmless-auth-tests)
