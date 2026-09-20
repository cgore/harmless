;;; harmless-xai.el --- xAI / Grok browser login for Harmless -*- lexical-binding: t; -*-

;;;; Copyright (c) 2026, Christopher Mark Gore,
;;;; Soli Deo Gloria,
;;;; All rights reserved.
;;;;
;;;; 22 Forest Glade Court, Saint Charles, Missouri 63304 USA.
;;;; Web: http://cgore.com
;;;; Email: cgore@cgore.com
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are met:
;;;;
;;;;     * Redistributions of source code must retain the above copyright
;;;;       notice, this list of conditions and the following disclaimer.
;;;;
;;;;     * Redistributions in binary form must reproduce the above copyright
;;;;       notice, this list of conditions and the following disclaimer in the
;;;;       documentation and/or other materials provided with the distribution.
;;;;
;;;;     * Neither the name of Christopher Mark Gore nor the names of other
;;;;       contributors may be used to endorse or promote products derived from
;;;;       this software without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;;; LIABLE FOR DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;;
;; Sign in through SpaceXAI OAuth at auth.x.ai, the same flow Grok Build uses.
;; Authorization Code + PKCE on a loopback callback; device-code if the
;; callback port is busy.  Tokens are stored under `harmless-directory'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'url-util)
(require 'browse-url)
(require 'harmless-util)
(require 'harmless-log)
(require 'harmless-auth)
(require 'harmless-provider)

(defconst harmless-xai-issuer "https://auth.x.ai"
  "xAI OAuth issuer.")

(defconst harmless-xai-client-id "b1a00492-073a-47ea-816f-4c329264a828"
  "Public Grok CLI OAuth client id.  xAI only accepts allowlisted clients.")

(defconst harmless-xai-authorize-url "https://auth.x.ai/oauth2/authorize")
(defconst harmless-xai-token-url "https://auth.x.ai/oauth2/token")
(defconst harmless-xai-device-url "https://auth.x.ai/oauth2/device/code")

(defconst harmless-xai-redirect-host "127.0.0.1")
(defconst harmless-xai-redirect-port 56121)
(defconst harmless-xai-redirect-path "/callback")

(defconst harmless-xai-scope
  "openid profile email offline_access grok-cli:access api:access")

(defconst harmless-xai-referrer "harmless")

(defconst harmless-xai-refresh-skew 300
  "Seconds before expiry to refresh an access token.")

(defcustom harmless-xai-use-grok-auth t
  "When non-nil, reuse `~/.grok/auth.json' if Harmless has no xAI session.
Harmless never writes that file.  A successful refresh is saved under
`harmless-directory' instead."
  :type 'boolean
  :group 'harmless)

(defvar harmless-xai--server nil
  "Live loopback OAuth server process, or nil.")

(defun harmless-xai-redirect-uri ()
  "Return the registered loopback redirect URI."
  (format "http://%s:%s%s"
          harmless-xai-redirect-host
          harmless-xai-redirect-port
          harmless-xai-redirect-path))

(defun harmless-xai-provider-p (provider)
  "Return non-nil if PROVIDER talks to api.x.ai."
  (let ((host (harmless-provider-host provider)))
    (and host (string-match-p "\\`api\\.x\\.ai\\'" host))))

(defun harmless-xai-auth-file ()
  "Return the path of Harmless's xAI token file."
  (expand-file-name "auth.json"
                    (if (boundp 'harmless-directory)
                        harmless-directory
                      (locate-user-emacs-file "harmless/"))))

(defun harmless-xai--b64url (bytes)
  "Base64url-encode BYTES with no padding."
  (let ((s (base64-encode-string bytes t)))
    (setq s (replace-regexp-in-string "+" "-" s))
    (setq s (replace-regexp-in-string "/" "_" s))
    (replace-regexp-in-string "=+$" "" s)))

(defun harmless-xai--random-string (n)
  "Return N characters of PKCE-safe random text."
  (let* ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~")
         (out (make-string n ?x)))
    (dotimes (i n)
      (aset out i (aref chars (random (length chars)))))
    out))

(defun harmless-xai--pkce ()
  "Return (VERIFIER . CHALLENGE) for S256 PKCE."
  (let* ((verifier (harmless-xai--random-string 64))
         (challenge (harmless-xai--b64url
                     (secure-hash 'sha256 verifier nil nil t))))
    (cons verifier challenge)))

(defun harmless-xai--form (alist)
  "Encode ALIST as application/x-www-form-urlencoded."
  (mapconcat
   (lambda (pair)
     (concat (url-hexify-string (format "%s" (car pair)))
             "="
             (url-hexify-string (format "%s" (cdr pair)))))
   alist
   "&"))

(defun harmless-xai--parse-query (qs)
  "Parse query string QS into an alist of decoded strings."
  (let (out)
    (dolist (part (split-string qs "&" t))
      (let ((eq-at (string-search "=" part)))
        (if (not eq-at)
            (push (cons (url-unhex-string part) "") out)
          (push (cons (url-unhex-string (substring part 0 eq-at))
                      (url-unhex-string (substring part (1+ eq-at))))
                out))))
    (nreverse out)))

(defun harmless-xai--parse-callback-request (req)
  "Parse HTTP request REQ.  Return (CODE . STATE) or nil."
  (when (string-match "GET \\([^? ]*\\)\\?\\([^ ]*\\) HTTP" req)
    (let* ((path (match-string 1 req))
           (qs (harmless-xai--parse-query (match-string 2 req))))
      (when (string-prefix-p "/callback" path)
        (cons (cdr (assoc "code" qs))
              (cdr (assoc "state" qs)))))))

(defun harmless-xai--parse-time (iso)
  "Parse ISO-8601 timestamp ISO to a Lisp time value."
  (when (and iso (stringp iso))
    (date-to-time (replace-regexp-in-string "\\.[0-9]+" "" iso))))

(defun harmless-xai--expired-p (iso &optional skew)
  "Return non-nil if ISO is in the past, or within SKEW seconds."
  (let ((time (harmless-xai--parse-time iso)))
    (or (null time)
        (time-less-p time (time-add (current-time) (or skew 0))))))

(defun harmless-xai--expires-at (seconds)
  "Return an ISO timestamp SECONDS from now."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                      (time-add (current-time) (or seconds 21600))
                      t))

(defun harmless-xai--http-ok ()
  "HTTP 200 body telling the user they can close the tab."
  (let ((body "<!DOCTYPE html><html><body><p>Harmless is signed in. You can close this window.</p></body></html>"))
    (format "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=utf-8\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
            (string-bytes body) body)))

(defun harmless-xai--token-request (fields)
  "POST FIELDS to the token endpoint.  Return a decoded plist or nil."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")
           ("Accept" . "application/json")))
        (url-request-data (harmless-xai--form fields))
        (url-show-status nil)
        (url-mime-accept-string "application/json"))
    (with-current-buffer (url-retrieve-synchronously
                          harmless-xai-token-url t t 30)
      (goto-char (point-min))
      (when (re-search-forward "\n\n" nil t)
        (harmless-json-decode-safe
         (buffer-substring-no-properties (point) (point-max)))))))

(defun harmless-xai--plist-from-token-response (payload)
  "Build a stored-token plist from a token-endpoint PAYLOAD."
  (when payload
    (when (plist-get payload :error)
      (error "xAI token error: %s"
             (or (plist-get payload :error_description)
                 (plist-get payload :error))))
    (unless (plist-get payload :access_token)
      (error "xAI token response had no access_token"))
    (list :access-token (plist-get payload :access_token)
          :refresh-token (plist-get payload :refresh_token)
          :expires-at (harmless-xai--expires-at
                       (plist-get payload :expires_in))
          :issuer harmless-xai-issuer
          :client-id harmless-xai-client-id)))

(defun harmless-xai--write-store (plist)
  "Write PLIST to the Harmless auth file with mode 0600."
  (let* ((dir (file-name-directory (harmless-xai-auth-file)))
         (file (harmless-xai-auth-file))
         (tmp (make-temp-file "harmless-auth-")))
    (harmless-ensure-directory dir)
    (set-file-modes tmp #o600)
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file tmp
        (setq buffer-file-coding-system 'utf-8-unix)
        (insert (harmless-json-text plist) "\n")))
    (set-file-modes tmp #o600)
    (rename-file tmp file t)
    (set-file-modes file #o600)
    plist))

(defun harmless-xai--read-store ()
  "Return the stored token plist, or nil."
  (let ((file (harmless-xai-auth-file)))
    (when (file-readable-p file)
      (harmless-json-decode-safe
       (with-temp-buffer
         (insert-file-contents file)
         (buffer-string))))))

(defun harmless-xai--refresh (plist)
  "Refresh PLIST using its refresh token.  Return a new plist or nil."
  (let ((refresh (plist-get plist :refresh-token)))
    (when refresh
      (harmless-log "xAI refreshing access token")
      (condition-case err
          (harmless-xai--plist-from-token-response
           (harmless-xai--token-request
            `(("grant_type" . "refresh_token")
              ("refresh_token" . ,refresh)
              ("client_id" . ,harmless-xai-client-id))))
        (error
         (harmless-log "xAI refresh failed: %s" (error-message-string err))
         nil)))))

(defun harmless-xai--live-token (plist)
  "Return a live access token from PLIST, refreshing if needed."
  (when plist
    (let ((token (plist-get plist :access-token))
          (exp (plist-get plist :expires-at)))
      (cond
       ((and token (not (harmless-xai--expired-p exp harmless-xai-refresh-skew)))
        token)
       (t
        (when-let* ((fresh (harmless-xai--refresh plist)))
          (harmless-xai--write-store fresh)
          (plist-get fresh :access-token)))))))

(defun harmless-xai--token-from-grok ()
  "Read a token from Grok Build's auth.json, if present.
Never writes that file.  A refresh is stored in Harmless's own file."
  (let ((file (expand-file-name "~/.grok/auth.json")))
    (when (file-readable-p file)
      (let* ((data (harmless-json-decode-safe
                    (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string))))
             (entry (cond
                     ((plist-get data :key) data)
                     ((harmless-plist-p (cadr data)) (cadr data))
                     (t nil))))
        (when entry
          (harmless-xai--live-token
           (list :access-token (plist-get entry :key)
                 :refresh-token (plist-get entry :refresh_token)
                 :expires-at (plist-get entry :expires_at)
                 :issuer (or (plist-get entry :oidc_issuer)
                             harmless-xai-issuer)
                 :client-id (or (plist-get entry :oidc_client_id)
                                harmless-xai-client-id))))))))

(defun harmless-xai-token ()
  "Return a live xAI OAuth access token, or nil.
Does not fall back to an API key; callers do that."
  (or (harmless-xai--live-token (harmless-xai--read-store))
      (and harmless-xai-use-grok-auth
           (harmless-xai--token-from-grok))))

(defun harmless-xai--authorize-url (state challenge)
  "Build the browser authorize URL for STATE and PKCE CHALLENGE."
  (concat harmless-xai-authorize-url "?"
          (harmless-xai--form
           `(("response_type" . "code")
             ("client_id" . ,harmless-xai-client-id)
             ("redirect_uri" . ,(harmless-xai-redirect-uri))
             ("scope" . ,harmless-xai-scope)
             ("state" . ,state)
             ("code_challenge" . ,challenge)
             ("code_challenge_method" . "S256")
             ("referrer" . ,harmless-xai-referrer)))))

(defun harmless-xai--exchange-code (code verifier)
  "Exchange authorization CODE + VERIFIER for tokens and store them."
  (let ((plist (harmless-xai--plist-from-token-response
                (harmless-xai--token-request
                 `(("grant_type" . "authorization_code")
                   ("code" . ,code)
                   ("redirect_uri" . ,(harmless-xai-redirect-uri))
                   ("client_id" . ,harmless-xai-client-id)
                   ("code_verifier" . ,verifier))))))
    (unless plist
      (error "xAI did not return an access token"))
    (harmless-xai--write-store plist)
    plist))

(defun harmless-xai--stop-server ()
  "Shut down the loopback OAuth server if it is running."
  (when (and harmless-xai--server (process-live-p harmless-xai--server))
    (ignore-errors (delete-process harmless-xai--server)))
  (setq harmless-xai--server nil))

(defun harmless-xai--start-server (expected-state on-code)
  "Listen on the loopback redirect port.
ON-CODE is called with the authorization code, or nil on failure.
EXPECTED-STATE must match the callback.  Return the server process."
  (harmless-xai--stop-server)
  (setq
   harmless-xai--server
   (make-network-process
    :name "harmless-xai-oauth"
    :server t
    :host harmless-xai-redirect-host
    :service harmless-xai-redirect-port
    :family 'ipv4
    :noquery t
    :filter
    (lambda (client chunk)
      (let ((buf (concat (or (process-get client 'harmless-buf) "") chunk)))
        (process-put client 'harmless-buf buf)
        (when (string-match-p "\r\n\r\n\\|\n\n" buf)
          (let ((parsed (harmless-xai--parse-callback-request buf)))
            (ignore-errors
              (process-send-string client (harmless-xai--http-ok)))
            (ignore-errors (delete-process client))
            (harmless-xai--stop-server)
            (cond
             ((not parsed)
              (funcall on-code nil "malformed OAuth callback"))
             ((not (equal (cdr parsed) expected-state))
              (funcall on-code nil "OAuth state mismatch"))
             ((not (car parsed))
              (funcall on-code nil "OAuth callback had no code"))
             (t (funcall on-code (car parsed) nil)))))))))
  harmless-xai--server)

(defun harmless-xai--wait-for-code (box timeout)
  "Wait until BOX is (CODE . ERR) or TIMEOUT seconds elapse."
  (let ((deadline (time-add (current-time) timeout)))
    (while (and (null (car box))
                (null (cdr box))
                (time-less-p (current-time) deadline))
      (accept-process-output nil 0.2))
    box))

(defun harmless-xai--browser-login ()
  "Run PKCE loopback login.  Return a stored-token plist."
  (let* ((pkce (harmless-xai--pkce))
         (state (harmless-xai--random-string 32))
         (url (harmless-xai--authorize-url state (cdr pkce)))
         (box (cons nil nil)))
    (unwind-protect
        (progn
          (harmless-xai--start-server
           state
           (lambda (code err)
             (setcar box code)
             (setcdr box err)))
          (harmless-log "xAI opening browser for OAuth")
          (message "Harmless: opening browser to sign in with xAI…")
          (harmless-browse-url url)
          (message "Harmless: waiting for xAI login (or open %s)" url)
          (harmless-xai--wait-for-code box 300)
          (cond
           ((car box)
            (harmless-xai--exchange-code (car box) (car pkce)))
           ((cdr box)
            (error "xAI login failed: %s" (cdr box)))
           (t (error "xAI login timed out"))))
      (harmless-xai--stop-server))))

(defun harmless-xai--device-login ()
  "Run the OAuth device-code flow.  Return a stored-token plist."
  (let ((start (json-parse-string
                (let ((url-request-method "POST")
                      (url-request-extra-headers
                       '(("Content-Type" . "application/x-www-form-urlencoded")
                         ("Accept" . "application/json")))
                      (url-request-data
                       (harmless-xai--form
                        `(("client_id" . ,harmless-xai-client-id)
                          ("scope" . ,harmless-xai-scope)
                          ("referrer" . ,harmless-xai-referrer))))
                      (url-show-status nil))
                  (with-current-buffer (url-retrieve-synchronously
                                        harmless-xai-device-url t t 30)
                    (goto-char (point-min))
                    (re-search-forward "\n\n" nil t)
                    (buffer-substring-no-properties (point) (point-max))))
                :object-type 'plist
                :array-type 'list
                :null-object nil
                :false-object :false)))
    (when (plist-get start :error)
      (error "xAI device login failed: %s"
             (or (plist-get start :error_description)
                 (plist-get start :error))))
    (let ((user-code (plist-get start :user_code))
          (uri (or (plist-get start :verification_uri_complete)
                   (plist-get start :verification_uri)))
          (device-code (plist-get start :device_code))
          (interval (or (plist-get start :interval) 5))
          (expires (or (plist-get start :expires_in) 300))
          (deadline nil)
          payload)
      (unless (and user-code uri device-code)
        (error "xAI device-code response was incomplete"))
      (setq deadline (time-add (current-time) expires))
      (message "Harmless: enter code %s at %s" user-code uri)
      (harmless-browse-url uri)
      (while (and (null payload) (time-less-p (current-time) deadline))
        (sit-for interval)
        (let ((resp (harmless-xai--token-request
                     `(("grant_type" . "urn:ietf:params:oauth:grant-type:device_code")
                       ("client_id" . ,harmless-xai-client-id)
                       ("device_code" . ,device-code)))))
          (cond
           ((and resp (plist-get resp :access_token))
            (setq payload resp))
           ((and resp (member (plist-get resp :error)
                              '("authorization_pending" "slow_down"))))
           ((and resp (plist-get resp :error))
            (error "xAI device login failed: %s"
                   (or (plist-get resp :error_description)
                       (plist-get resp :error)))))))
      (unless payload
        (error "xAI device login timed out"))
      (harmless-xai--write-store
       (harmless-xai--plist-from-token-response payload)))))

(defun harmless-xai-login (&optional device)
  "Sign in to xAI / Grok through a web browser.
With a prefix argument, or if DEVICE is non-nil, use the device-code
flow instead of a loopback callback.  If the callback port is already
in use (for example by Grok Build), the device-code flow is used
automatically."
  (interactive "P")
  (let ((plist
         (cond
          (device (harmless-xai--device-login))
          (t
           (condition-case err
               (harmless-xai--browser-login)
             (file-error
              (harmless-log "xAI loopback bind failed (%s); using device code"
                            (error-message-string err))
              (message "Harmless: login port busy, switching to device code")
              (harmless-xai--device-login)))))))
    (message "Harmless: signed in to xAI")
    plist))

(defun harmless-xai-logout ()
  "Forget the stored xAI OAuth session.
Does not delete `~/.grok/auth.json'."
  (interactive)
  (harmless-xai--stop-server)
  (let ((file (harmless-xai-auth-file)))
    (when (file-exists-p file)
      (delete-file file)))
  (message "Harmless: signed out of xAI"))

(harmless-register-login-method
 'xai
 :name "xAI"
 :login #'harmless-xai-login
 :logout #'harmless-xai-logout
 :logged-in-p (lambda () (and (harmless-xai--read-store) t)))

(provide 'harmless-xai)

;;; harmless-xai.el ends here
