;;; harmless-xai-tests.el --- Tests for xAI OAuth helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless-xai)

(ert-deftest harmless-xai-b64url-no-padding ()
  (should (string= "AQID" (harmless-xai--b64url (unibyte-string 1 2 3))))
  (should-not (string-match-p "[=+/]" (harmless-xai--b64url (secure-hash 'sha256 "abc" nil nil t)))))

(ert-deftest harmless-xai-pkce-shape ()
  (let ((pair (harmless-xai--pkce)))
    (should (= 64 (length (car pair))))
    (should (string-match-p "\\`[A-Za-z0-9_-]+\\'" (cdr pair)))
    (should (equal (cdr pair)
                   (harmless-xai--b64url
                    (secure-hash 'sha256 (car pair) nil nil t))))))

(ert-deftest harmless-xai-parse-callback ()
  (let ((parsed (harmless-xai--parse-callback-request
                 "GET /callback?code=abc%2Fdef&state=xyz HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n")))
    (should (equal (car parsed) "abc/def"))
    (should (equal (cdr parsed) "xyz"))))

(ert-deftest harmless-xai-parse-callback-rejects-other-path ()
  (should-not (harmless-xai--parse-callback-request
               "GET /other?code=abc&state=xyz HTTP/1.1\r\n\r\n")))

(ert-deftest harmless-xai-expired-p ()
  (should (harmless-xai--expired-p "2000-01-01T00:00:00Z"))
  (should-not (harmless-xai--expired-p (harmless-xai--expires-at 3600) 300)))

(ert-deftest harmless-xai-store-roundtrip ()
  (let* ((harmless-directory (make-temp-file "harmless-auth-" t))
         (plist (list :access-token "tok"
                      :refresh-token "ref"
                      :expires-at "2099-01-01T00:00:00Z"
                      :issuer "https://auth.x.ai"
                      :client-id "cid")))
    (harmless-xai--write-store plist)
    (should (= #o600 (logand (file-modes (harmless-xai-auth-file)) #o777)))
    (let ((got (harmless-xai--read-store)))
      (should (string= "tok" (plist-get got :access-token)))
      (should (string= "ref" (plist-get got :refresh-token))))
    (let ((harmless-xai-use-grok-auth nil))
      (should (string= "tok" (harmless-xai-token))))))

(ert-deftest harmless-browse-url-ignores-w3m ()
  (let ((browse-url-browser-function 'w3m-browse-url)
        (called nil))
    (cl-letf (((symbol-function 'browse-url-default-browser)
               (lambda (url &rest _) (setq called url))))
      (let ((harmless-browse-url-function nil))
        (harmless-browse-url "https://example.com/")
        (should (equal called "https://example.com/"))))))

(ert-deftest harmless-xai-authorize-url-contains-pkce ()
  (let ((url (harmless-xai--authorize-url "st" "challenge")))
    (should (string-match-p "code_challenge=challenge" url))
    (should (string-match-p "code_challenge_method=S256" url))
    (should (string-match-p (regexp-quote "127.0.0.1%3A56121") url))
    (should (string-match-p "client_id=b1a00492-073a-47ea-816f-4c329264a828" url))))

(provide 'harmless-xai-tests)
