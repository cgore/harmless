;;; harmless-openai-oauth-tests.el --- Tests for ChatGPT OAuth helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless-openai)
(require 'harmless-openai-oauth)
(require 'harmless-auth)

(ert-deftest harmless-openai-authorize-url ()
  (let ((url (harmless-openai--authorize-url "st" "challenge")))
    (should (string-match-p "code_challenge=challenge" url))
    (should (string-match-p "localhost%3A1455" url))
    (should (string-match-p "app_EMoamEEZ73f0CkXaXp7hrann" url))
    (should (string-match-p "codex_cli_simplified_flow=true" url))
    (should (string-match-p "originator=harmless" url))))

(ert-deftest harmless-openai-parse-callback ()
  (let ((parsed (harmless-openai--parse-callback
                 "GET /auth/callback?code=abc%2Fdef&state=xyz HTTP/1.1\r\n\r\n")))
    (should (equal (car parsed) "abc/def"))
    (should (equal (cdr parsed) "xyz"))))

(ert-deftest harmless-openai-parse-pasted ()
  (should (equal "abc"
                 (harmless-openai--parse-pasted "abc#state")))
  (should (equal "xyz/1"
                 (harmless-openai--parse-pasted
                  "http://localhost:1455/auth/callback?code=xyz%2F1&state=st"))))

(ert-deftest harmless-openai-jwt-account-id ()
  (let* ((payload (base64-encode-string
                   "{\"chatgpt_account_id\":\"acct-1\"}" t))
         (payload (replace-regexp-in-string "=+$" "" payload))
         (jwt (concat "aaa." payload ".bbb")))
    (should (equal "acct-1" (harmless-openai--account-id jwt)))))

(ert-deftest harmless-openai-jwt-nested-account-id ()
  (let* ((json "{\"https://api.openai.com/auth\":{\"chatgpt_account_id\":\"acct-2\"}}")
         (payload (replace-regexp-in-string
                   "=+$" "" (base64-encode-string json t)))
         (jwt (concat "aaa." payload ".bbb")))
    (should (equal "acct-2" (harmless-openai--account-id jwt)))))

(ert-deftest harmless-openai-store-roundtrip ()
  (let* ((harmless-directory (make-temp-file "harmless-auth-" t))
         (harmless-openai-use-codex-auth nil)
         (plist (list :access-token "tok"
                      :refresh-token "ref"
                      :account-id "acct"
                      :expires-at "2099-01-01T00:00:00Z"
                      :client-id "cid")))
    (harmless-openai--write-store plist)
    (should (= #o600 (logand (file-modes (harmless-openai-auth-file)) #o777)))
    (should (string= "tok" (harmless-openai-token)))
    (should (string= "acct" (harmless-openai-account-id)))))

(ert-deftest harmless-openai-codex-auth-plist ()
  (let* ((dir (make-temp-file "codex-auth-" t))
         (harmless-openai-codex-auth-file (expand-file-name "auth.json" dir))
         (harmless-directory (make-temp-file "harmless-auth-" t))
         (harmless-openai-use-codex-auth t))
    (with-temp-file harmless-openai-codex-auth-file
      (insert "{\"auth_mode\":\"chatgpt\",\"tokens\":{\"access_token\":\"codex-tok\",\"account_id\":\"acct-c\",\"refresh_token\":\"r\"}}"))
    (should (string= "codex-tok" (harmless-openai-token)))
    (should (string= "acct-c" (harmless-openai-account-id)))))

(ert-deftest harmless-openai-gpt5-supports-effort ()
  (should (harmless-model-supports-effort-p "gpt-5.5"))
  (should (harmless-model-supports-effort-p "gpt-6-astra"))
  (should (member "xhigh" (harmless-model-effort-levels "gpt-5.5")))
  (should (member "ultra" (harmless-model-effort-levels "gpt-6-astra")))
  (should-not (harmless-model-supports-effort-p "gpt-4o")))

(ert-deftest harmless-openai-login-method-registered ()
  (let ((spec (harmless-login-method 'openai)))
    (should spec)
    (should (string= "OpenAI" (plist-get spec :name)))
    (should (eq #'harmless-openai-login (plist-get spec :login)))))

(ert-deftest harmless-openai-codex-model-list ()
  (let ((p (harmless-make-openai :key nil :key-env "HARMLESS_NO_SUCH_OPENAI_KEY")))
    (cl-letf (((symbol-function 'harmless-openai-token)
               (lambda (&rest _) "tok")))
      (should (member "gpt-5.5" (harmless-provider-model-list p)))
      (should (member "gpt-6-astra" (harmless-provider-model-list p)))
      (should-not (member "gpt-5.4" (harmless-provider-model-list p))))))

(ert-deftest harmless-openai-available-p-oauth ()
  (let ((p (harmless-make-openai :key nil :key-env "HARMLESS_NO_SUCH_OPENAI_KEY"))
        (harmless-openai-use-codex-auth nil))
    (cl-letf (((symbol-function 'harmless-auth-key) (lambda (&rest _) nil))
              ((symbol-function 'harmless-openai-token) (lambda (&rest _) nil)))
      (should-not (harmless-provider-available-p p)))
    (cl-letf (((symbol-function 'harmless-auth-key) (lambda (&rest _) nil))
              ((symbol-function 'harmless-openai-token)
               (lambda (&rest _) "tok")))
      (should (harmless-provider-available-p p)))))

(provide 'harmless-openai-oauth-tests)
