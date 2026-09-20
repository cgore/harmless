;;; harmless-anthropic-oauth-tests.el --- Tests for Claude OAuth helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'harmless-anthropic-oauth)

(ert-deftest harmless-anthropic-parse-code-plain ()
  (should (equal '("abc" . nil)
                 (harmless-anthropic--parse-code "abc"))))

(ert-deftest harmless-anthropic-parse-code-hash ()
  (should (equal '("thecode" . "thestate")
                 (harmless-anthropic--parse-code "thecode#thestate"))))

(ert-deftest harmless-anthropic-parse-code-url ()
  (let ((parsed (harmless-anthropic--parse-code
                 "https://platform.claude.com/oauth/code/callback?code=xyz%2F1&state=st")))
    (should (equal (car parsed) "xyz/1"))
    (should (equal (cdr parsed) "st"))))

(ert-deftest harmless-anthropic-authorize-url ()
  (let ((url (harmless-anthropic--authorize-url "st" "challenge")))
    (should (string-match-p "code=true" url))
    (should (string-match-p "code_challenge=challenge" url))
    (should (string-match-p "code_challenge_method=S256" url))
    (should (string-match-p "9d1c250a-e61b-44d9-88ed-5944d1962f5e" url))
    (should (string-match-p "user%3Ainference" url))))

(ert-deftest harmless-anthropic-store-roundtrip ()
  (let* ((harmless-directory (make-temp-file "harmless-auth-" t))
         (plist (list :access-token "tok"
                      :refresh-token "ref"
                      :expires-at "2099-01-01T00:00:00Z"
                      :client-id "cid"))
         (harmless-anthropic-use-claude-auth nil))
    (harmless-anthropic--write-store plist)
    (should (= #o600 (logand (file-modes (harmless-anthropic-auth-file)) #o777)))
    (let ((got (harmless-anthropic--read-store)))
      (should (string= "tok" (plist-get got :access-token))))
    (should (string= "tok" (harmless-anthropic-token)))))

(ert-deftest harmless-anthropic-ms-to-iso ()
  (should (string-match-p "\\`20"
                          (harmless-anthropic--ms-to-iso 1700000000000))))

(provide 'harmless-anthropic-oauth-tests)
