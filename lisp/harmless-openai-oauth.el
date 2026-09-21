;;; harmless-openai-oauth.el --- ChatGPT / OpenAI browser login -*- lexical-binding: t; -*-

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
;; Sign in with ChatGPT using Codex CLI's public OAuth client (PKCE loopback
;; on localhost:1455).  Tokens are stored under `harmless-directory'.  ChatGPT
;; subscription tokens are sent to the Codex backend, not api.openai.com.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'url-util)
(require 'harmless-util)
(require 'harmless-log)
(require 'harmless-auth)
(require 'harmless-provider)
(require 'harmless-xai)

(defconst harmless-openai-client-id "app_EMoamEEZ73f0CkXaXp7hrann"
  "Public Codex CLI OAuth client id.")

(defconst harmless-openai-authorize-url "https://auth.openai.com/oauth/authorize")
(defconst harmless-openai-token-url "https://auth.openai.com/oauth/token")
(defconst harmless-openai-redirect-uri "http://localhost:1455/auth/callback")
(defconst harmless-openai-redirect-port 1455)
(defconst harmless-openai-scope "openid profile email offline_access")
(defconst harmless-openai-codex-responses-url
  "https://chatgpt.com/backend-api/codex/responses")
(defconst harmless-openai-codex-models
  '("gpt-6-astra" "gpt-5.6-sol" "gpt-5.6-terra" "gpt-5.6-luna" "gpt-5.5")
  "Models ChatGPT Codex currently offers on a subscription login.
`gpt-5.4' and older Chat Completions ids are rejected with HTTP 400.")
(defconst harmless-openai-refresh-skew 300)

(defvar harmless-openai--server nil
  "Live loopback OAuth server process, or nil.")

(defvar harmless-openai-codex-auth-file
  (expand-file-name "~/.codex/auth.json")
  "Codex CLI auth.json path.  Harmless never writes this file.")

(defcustom harmless-openai-use-codex-auth t
  "When non-nil, reuse Codex CLI `auth.json' if Harmless has no OpenAI session.
Harmless never writes that file.  A successful refresh is saved under
`harmless-directory' instead."
  :type 'boolean
  :group 'harmless)

(defun harmless-openai-official-p (provider)
  "Return non-nil if PROVIDER is official api.openai.com."
  (let ((host (harmless-provider-host provider)))
    (and host (string-match-p "\\`api\\.openai\\.com\\'" host))))

(defun harmless-openai-auth-file (&optional provider)
  "Return the path of the OpenAI token file for PROVIDER.
The default connection named \"OpenAI\" uses `auth-openai.json'.
Other connections use `auth-SLUG.json'."
  (harmless-connection-auth-file provider "auth-openai.json" 'openai))

(defun harmless-openai--authorize-url (state challenge)
  "Build the ChatGPT authorize URL for STATE and PKCE CHALLENGE."
  (concat harmless-openai-authorize-url "?"
          (harmless-xai--form
           `(("response_type" . "code")
             ("client_id" . ,harmless-openai-client-id)
             ("redirect_uri" . ,harmless-openai-redirect-uri)
             ("scope" . ,harmless-openai-scope)
             ("code_challenge" . ,challenge)
             ("code_challenge_method" . "S256")
             ("state" . ,state)
             ("id_token_add_organizations" . "true")
             ("codex_cli_simplified_flow" . "true")
             ("originator" . "harmless")))))

(defun harmless-openai--parse-pasted (raw)
  "Extract an authorization code from pasted RAW text."
  (let ((s (string-trim (or raw ""))))
    (cond
     ((string-empty-p s) nil)
     ((string-match "code=\\([^&#]+\\)" s)
      (url-unhex-string (match-string 1 s)))
     ((string-match "\\`\\([^#[:space:]]+\\)#" s)
      (match-string 1 s))
     (t (car (split-string s "[&#[:space:]]" t))))))

(defun harmless-openai--parse-callback (req)
  "Parse HTTP REQ.  Return (CODE . STATE) or nil."
  (when (string-match "GET \\([^? ]*\\)\\?\\([^ ]*\\) HTTP" req)
    (let ((path (match-string 1 req))
          (qs (harmless-xai--parse-query (match-string 2 req))))
      (when (string-match-p "callback" path)
        (cons (cdr (assoc "code" qs))
              (cdr (assoc "state" qs)))))))

(defun harmless-openai--jwt-payload (token)
  "Decode TOKEN's JWT payload without verifying the signature."
  (when (and token (string-match "\\`[^.]+\\.\\([^.]+\\)\\." token))
    (let* ((b64 (match-string 1 token))
           (pad (mod (- 4 (mod (length b64) 4)) 4))
           (b64 (concat b64 (make-string pad ?=)))
           (b64 (string-replace "-" "+" (string-replace "_" "/" b64)))
           (json (ignore-errors (base64-decode-string b64))))
      (and json (harmless-json-decode-safe json)))))

(defun harmless-openai--account-id (token)
  "Extract a ChatGPT account id from JWT TOKEN."
  (let ((p (harmless-openai--jwt-payload token)))
    (or (plist-get p :chatgpt_account_id)
        (plist-get (plist-get p :https://api.openai.com/auth)
                   :chatgpt_account_id)
        (plist-get (car (plist-get p :organizations)) :id))))

(defun harmless-openai--jwt-exp (token)
  "Return TOKEN's JWT exp claim as an ISO UTC timestamp, or nil."
  (let ((exp (plist-get (harmless-openai--jwt-payload token) :exp)))
    (when (numberp exp)
      (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                          (seconds-to-time exp)
                          t))))

(defun harmless-openai--token-request (fields)
  "POST form FIELDS to the OpenAI token endpoint."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")
           ("Accept" . "application/json")))
        (url-request-data (harmless-xai--form fields))
        (url-show-status nil))
    (with-current-buffer (url-retrieve-synchronously
                          harmless-openai-token-url t t 30)
      (goto-char (point-min))
      (when (re-search-forward "\n\n" nil t)
        (harmless-json-decode-safe
         (buffer-substring-no-properties (point) (point-max)))))))

(defun harmless-openai--plist-from-token-response (payload)
  "Build a stored-token plist from PAYLOAD."
  (when payload
    (when (plist-get payload :error)
      (error "OpenAI token error: %s"
             (or (plist-get payload :error_description)
                 (plist-get payload :error))))
    (unless (plist-get payload :access_token)
      (error "OpenAI token response had no access_token"))
    (let ((access (plist-get payload :access_token))
          (id-token (plist-get payload :id_token)))
      (list :access-token access
            :refresh-token (plist-get payload :refresh_token)
            :id-token id-token
            :account-id (or (harmless-openai--account-id id-token)
                            (harmless-openai--account-id access))
            :expires-at (harmless-xai--expires-at
                         (plist-get payload :expires_in))
            :client-id harmless-openai-client-id))))

(defun harmless-openai--write-store (plist)
  "Write PLIST to the Harmless OpenAI auth file with mode 0600."
  (let* ((dir (file-name-directory (harmless-openai-auth-file)))
         (file (harmless-openai-auth-file))
         (tmp (make-temp-file "harmless-openai-auth-")))
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

(defun harmless-openai--read-store ()
  "Return the stored OpenAI token plist, or nil."
  (let ((file (harmless-openai-auth-file)))
    (when (file-readable-p file)
      (harmless-json-decode-safe
       (with-temp-buffer
         (insert-file-contents file)
         (buffer-string))))))

(defun harmless-openai--refresh (plist)
  "Refresh PLIST.  Return a new plist or nil."
  (let ((refresh (plist-get plist :refresh-token)))
    (when refresh
      (harmless-log "OpenAI refreshing access token")
      (condition-case err
          (let ((fresh (harmless-openai--plist-from-token-response
                        (harmless-openai--token-request
                         `(("grant_type" . "refresh_token")
                           ("refresh_token" . ,refresh)
                           ("client_id" . ,harmless-openai-client-id))))))
            (when fresh
              (unless (plist-get fresh :refresh-token)
                (setq fresh (plist-put (copy-sequence fresh) :refresh-token
                                       refresh)))
              (unless (plist-get fresh :account-id)
                (setq fresh (plist-put (copy-sequence fresh) :account-id
                                       (plist-get plist :account-id)))))
            fresh)
        (error
         (harmless-log "OpenAI refresh failed: %s" (error-message-string err))
         nil)))))

(defun harmless-openai--live-token (plist)
  "Return a live access token from PLIST, refreshing if needed.
A missing expiry is treated as still valid."
  (when plist
    (let ((token (plist-get plist :access-token))
          (exp (plist-get plist :expires-at)))
      (cond
       ((and token
             (or (null exp)
                 (not (harmless-xai--expired-p
                       exp harmless-openai-refresh-skew))))
        token)
       (t
        (when-let* ((fresh (harmless-openai--refresh plist)))
          (harmless-openai--write-store fresh)
          (plist-get fresh :access-token)))))))

(defun harmless-openai--ms-to-iso (ms)
  "Convert a millisecond or second Unix timestamp MS to ISO UTC."
  (when (numberp ms)
    (format-time-string
     "%Y-%m-%dT%H:%M:%SZ"
     (seconds-to-time
      (/ (float (if (> ms 1e12) ms (* ms 1000.0))) 1000.0))
     t)))

(defun harmless-openai--codex-plist ()
  "Read Codex CLI auth.json as a Harmless token plist, or nil.
Never writes that file."
  (let ((file harmless-openai-codex-auth-file))
    (when (and file (file-readable-p file))
      (let* ((data (harmless-json-decode-safe
                    (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string))))
             (tokens (or (plist-get data :tokens) data))
             (access (or (plist-get tokens :access_token)
                         (plist-get tokens :access)))
             (id-token (plist-get tokens :id_token))
             (refresh (or (plist-get tokens :refresh_token)
                          (plist-get tokens :refresh)))
             (account (or (plist-get tokens :account_id)
                          (plist-get tokens :accountId)
                          (harmless-openai--account-id id-token)
                          (harmless-openai--account-id access)))
             (exp (or (plist-get tokens :expires_at)
                      (harmless-openai--ms-to-iso
                       (or (plist-get tokens :expires)
                           (plist-get data :expires)))
                      (harmless-openai--jwt-exp (or id-token access)))))
        (and access
             (list :access-token access
                   :refresh-token refresh
                   :id-token id-token
                   :account-id account
                   :expires-at exp
                   :client-id harmless-openai-client-id))))))

(defun harmless-openai-token (&optional provider)
  "Return a live ChatGPT OAuth access token for PROVIDER, or nil.
Does not fall back to an API key; callers do that.  Codex CLI auth
is reused only for the default \"OpenAI\" connection."
  (let ((harmless-oauth-provider (or provider harmless-oauth-provider)))
    (or (harmless-openai--live-token (harmless-openai--read-store))
        (and (harmless-connection-default-p harmless-oauth-provider 'openai)
             harmless-openai-use-codex-auth
             (harmless-openai--live-token (harmless-openai--codex-plist))))))

(defun harmless-openai-account-id (&optional provider)
  "Return the ChatGPT account id for PROVIDER's OAuth session, if any."
  (let ((harmless-oauth-provider (or provider harmless-oauth-provider)))
    (or (plist-get (harmless-openai--read-store) :account-id)
        (and (harmless-connection-default-p harmless-oauth-provider 'openai)
             harmless-openai-use-codex-auth
             (plist-get (harmless-openai--codex-plist) :account-id)))))

(defun harmless-openai--exchange-code (code verifier)
  "Exchange CODE + VERIFIER for tokens and store them."
  (let ((plist (harmless-openai--plist-from-token-response
                (harmless-openai--token-request
                 `(("grant_type" . "authorization_code")
                   ("code" . ,code)
                   ("redirect_uri" . ,harmless-openai-redirect-uri)
                   ("client_id" . ,harmless-openai-client-id)
                   ("code_verifier" . ,verifier))))))
    (unless plist
      (error "OpenAI did not return an access token"))
    (harmless-openai--write-store plist)
    plist))

(defun harmless-openai--stop-server ()
  "Shut down the OpenAI loopback OAuth server."
  (when (and harmless-openai--server (process-live-p harmless-openai--server))
    (ignore-errors (delete-process harmless-openai--server)))
  (setq harmless-openai--server nil))

(defun harmless-openai--start-server (expected-state on-code)
  "Listen on the Codex loopback port.  ON-CODE gets the auth code."
  (harmless-openai--stop-server)
  (setq
   harmless-openai--server
   (make-network-process
    :name "harmless-openai-oauth"
    :server t
    :host "localhost"
    :service harmless-openai-redirect-port
    :family 'ipv4
    :noquery t
    :filter
    (lambda (client chunk)
      (let ((buf (concat (or (process-get client 'harmless-buf) "") chunk)))
        (process-put client 'harmless-buf buf)
        (when (string-match-p "\r\n\r\n\\|\n\n" buf)
          (let ((parsed (harmless-openai--parse-callback buf)))
            (ignore-errors
              (process-send-string client (harmless-xai--http-ok)))
            (ignore-errors (delete-process client))
            (harmless-openai--stop-server)
            (cond
             ((not parsed)
              (funcall on-code nil "malformed OAuth callback"))
             ((not (equal (cdr parsed) expected-state))
              (funcall on-code nil "OAuth state mismatch"))
             ((not (car parsed))
              (funcall on-code nil "OAuth callback had no code"))
             (t (funcall on-code (car parsed) nil)))))))))
  harmless-openai--server)

(defun harmless-openai--browser-login ()
  "Run PKCE loopback login.  Return a stored-token plist."
  (let* ((pkce (harmless-xai--pkce))
         (state (harmless-xai--random-string 32))
         (url (harmless-openai--authorize-url state (cdr pkce)))
         (box (cons nil nil)))
    (unwind-protect
        (progn
          (harmless-openai--start-server
           state
           (lambda (code err)
             (setcar box code)
             (setcdr box err)))
          (harmless-log "OpenAI opening browser for OAuth")
          (message "Harmless: opening browser to sign in with ChatGPT…")
          (harmless-browse-url url)
          (message "Harmless: waiting for ChatGPT login (or open %s)" url)
          (harmless-xai--wait-for-code box 300)
          (cond
           ((car box)
            (harmless-openai--exchange-code (car box) (car pkce)))
           ((cdr box)
            (error "OpenAI login failed: %s" (cdr box)))
           (t (error "OpenAI login timed out"))))
      (harmless-openai--stop-server))))

(defun harmless-openai--paste-login ()
  "Open the authorize URL and prompt for a pasted callback URL or code."
  (let* ((pkce (harmless-xai--pkce))
         (state (harmless-xai--random-string 32))
         (url (harmless-openai--authorize-url state (cdr pkce))))
    (harmless-browse-url url)
    (let* ((raw (read-from-minibuffer
                 "Paste the ChatGPT redirect URL or code: "))
           (code (harmless-openai--parse-pasted raw)))
      (unless (and code (not (string-empty-p code)))
        (user-error "No authorization code"))
      (harmless-openai--exchange-code code (car pkce)))))

(defun harmless-openai-login (&optional paste)
  "Sign in to ChatGPT / OpenAI through a web browser.
With a prefix argument, or if PASTE is non-nil, paste the redirect
URL instead of using the loopback callback.  If port 1455 is busy
(for example Codex CLI), paste mode is used automatically."
  (interactive "P")
  (let ((plist
         (cond
          (paste (harmless-openai--paste-login))
          (t
           (condition-case err
               (harmless-openai--browser-login)
             (file-error
              (harmless-log "OpenAI loopback bind failed (%s); paste code"
                            (error-message-string err))
              (message "Harmless: login port busy, paste the redirect URL")
              (harmless-openai--paste-login)))))))
    (message "Harmless: signed in to %s"
             (harmless-connection-display-name nil "OpenAI"))
    plist))

(defun harmless-openai-logout ()
  "Forget the stored OpenAI OAuth session for the current connection.
Does not delete `~/.codex/auth.json'."
  (interactive)
  (harmless-openai--stop-server)
  (let ((file (harmless-openai-auth-file)))
    (when (file-exists-p file)
      (delete-file file)))
  (message "Harmless: signed out of %s"
           (harmless-connection-display-name nil "OpenAI")))

(harmless-register-login-method
 'openai
 :name "OpenAI"
 :login #'harmless-openai-login
 :logout #'harmless-openai-logout
 :logged-in-p (lambda () (and (harmless-openai--read-store) t)))

(provide 'harmless-openai-oauth)

;;; harmless-openai-oauth.el ends here
