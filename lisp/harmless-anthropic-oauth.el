;;; harmless-anthropic-oauth.el --- Claude / Anthropic browser login -*- lexical-binding: t; -*-

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
;; Sign in through Claude Code's public OAuth client.  The browser shows a
;; code to paste (no loopback server).  Tokens are stored under
;; `harmless-directory' and sent as Authorization Bearer with the oauth beta
;; header.

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

(defconst harmless-anthropic-client-id "9d1c250a-e61b-44d9-88ed-5944d1962f5e"
  "Public Claude Code OAuth client id.")

(defconst harmless-anthropic-authorize-url
  "https://claude.com/cai/oauth/authorize")

(defconst harmless-anthropic-token-url
  "https://platform.claude.com/v1/oauth/token")

(defconst harmless-anthropic-redirect-uri
  "https://platform.claude.com/oauth/code/callback")

(defconst harmless-anthropic-scope
  (mapconcat #'identity
             '("org:create_api_key"
               "user:profile"
               "user:inference"
               "user:sessions:claude_code"
               "user:mcp_servers"
               "user:file_upload")
             " "))

(defconst harmless-anthropic-oauth-beta "oauth-2025-04-20"
  "anthropic-beta value required for OAuth access tokens.")

(defconst harmless-anthropic-refresh-skew 300)

(defcustom harmless-anthropic-use-claude-auth t
  "When non-nil, reuse `~/.claude/.credentials.json' if Harmless has no session.
Harmless never writes that file, and never refreshes its token (Claude
Code refresh tokens are single-use)."
  :type 'boolean
  :group 'harmless)

(defun harmless-anthropic-provider-p (provider)
  "Return non-nil if PROVIDER talks to api.anthropic.com."
  (let ((host (harmless-provider-host provider)))
    (and host (string-match-p "\\`api\\.anthropic\\.com\\'" host))))

(defun harmless-anthropic-auth-file ()
  "Return the path of Harmless's Anthropic token file."
  (expand-file-name "auth-anthropic.json"
                    (if (boundp 'harmless-directory)
                        harmless-directory
                      (locate-user-emacs-file "harmless/"))))

(defun harmless-anthropic--authorize-url (state challenge)
  "Build the Claude authorize URL for STATE and PKCE CHALLENGE."
  (concat harmless-anthropic-authorize-url "?"
          (harmless-xai--form
           `(("code" . "true")
             ("client_id" . ,harmless-anthropic-client-id)
             ("response_type" . "code")
             ("redirect_uri" . ,harmless-anthropic-redirect-uri)
             ("scope" . ,harmless-anthropic-scope)
             ("code_challenge" . ,challenge)
             ("code_challenge_method" . "S256")
             ("state" . ,state)))))

(defun harmless-anthropic--parse-code (raw)
  "Return (CODE . STATE) from a pasted RAW string.
RAW may be CODE, CODE#STATE, or a callback URL."
  (let ((s (string-trim (or raw ""))))
    (cond
     ((string-empty-p s) (cons nil nil))
     ((string-match "code=\\([^&#]+\\)" s)
      (cons (url-unhex-string (match-string 1 s))
            (and (string-match "state=\\([^&#]+\\)" s)
                 (url-unhex-string (match-string 1 s)))))
     ((string-match "\\`\\([^#[:space:]]+\\)#\\([^[:space:]]+\\)\\'" s)
      (cons (match-string 1 s) (match-string 2 s)))
     (t (cons (car (split-string s "[&#]" t)) nil)))))

(defun harmless-anthropic--token-request (obj)
  "POST JSON OBJ to the Anthropic token endpoint.  Return a decoded plist."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json")
           ("Accept" . "application/json")))
        (url-request-data (harmless-json-encode obj))
        (url-show-status nil)
        (url-mime-accept-string "application/json"))
    (with-current-buffer (url-retrieve-synchronously
                          harmless-anthropic-token-url t t 30)
      (goto-char (point-min))
      (when (re-search-forward "\n\n" nil t)
        (harmless-json-decode-safe
         (buffer-substring-no-properties (point) (point-max)))))))

(defun harmless-anthropic--plist-from-token-response (payload)
  "Build a stored-token plist from a token-endpoint PAYLOAD."
  (when payload
    (when (plist-get payload :error)
      (error "Anthropic token error: %s"
             (or (plist-get payload :error_description)
                 (plist-get payload :error)
                 (plist-get payload :message))))
    (unless (plist-get payload :access_token)
      (error "Anthropic token response had no access_token"))
    (list :access-token (plist-get payload :access_token)
          :refresh-token (plist-get payload :refresh_token)
          :expires-at (harmless-xai--expires-at
                       (plist-get payload :expires_in))
          :client-id harmless-anthropic-client-id)))

(defun harmless-anthropic--write-store (plist)
  "Write PLIST to the Harmless Anthropic auth file with mode 0600."
  (let* ((dir (file-name-directory (harmless-anthropic-auth-file)))
         (file (harmless-anthropic-auth-file))
         (tmp (make-temp-file "harmless-anthropic-auth-")))
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

(defun harmless-anthropic--read-store ()
  "Return the stored Anthropic token plist, or nil."
  (let ((file (harmless-anthropic-auth-file)))
    (when (file-readable-p file)
      (harmless-json-decode-safe
       (with-temp-buffer
         (insert-file-contents file)
         (buffer-string))))))

(defun harmless-anthropic--refresh (plist)
  "Refresh PLIST using its refresh token.  Return a new plist or nil."
  (let ((refresh (plist-get plist :refresh-token)))
    (when refresh
      (harmless-log "Anthropic refreshing access token")
      (condition-case err
          (harmless-anthropic--plist-from-token-response
           (harmless-anthropic--token-request
            (list :grant_type "refresh_token"
                  :refresh_token refresh
                  :client_id harmless-anthropic-client-id)))
        (error
         (harmless-log "Anthropic refresh failed: %s" (error-message-string err))
         nil)))))

(defun harmless-anthropic--live-token (plist)
  "Return a live access token from PLIST, refreshing if needed."
  (when plist
    (let ((token (plist-get plist :access-token))
          (exp (plist-get plist :expires-at)))
      (cond
       ((and token (not (harmless-xai--expired-p
                         exp harmless-anthropic-refresh-skew)))
        token)
       (t
        (when-let* ((fresh (harmless-anthropic--refresh plist)))
          (harmless-anthropic--write-store fresh)
          (plist-get fresh :access-token)))))))

(defun harmless-anthropic--ms-to-iso (ms)
  "Convert epoch milliseconds MS to an ISO-8601 UTC string."
  (when (numberp ms)
    (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                        (seconds-to-time (/ (float ms) 1000.0))
                        t)))

(defun harmless-anthropic--token-from-claude-cli ()
  "Read a still-valid access token from Claude Code credentials, if any.
Never refreshes and never writes that file."
  (let ((file (expand-file-name "~/.claude/.credentials.json")))
    (when (file-readable-p file)
      (let* ((data (harmless-json-decode-safe
                    (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string))))
             (oauth (or (plist-get data :claudeAiOauth) data))
             (token (plist-get oauth :accessToken))
             (exp (harmless-anthropic--ms-to-iso
                   (plist-get oauth :expiresAt))))
        (and token
             (not (harmless-xai--expired-p exp 0))
             token)))))

(defun harmless-anthropic-token ()
  "Return a live Anthropic OAuth access token, or nil.
Does not fall back to an API key; callers do that."
  (or (harmless-anthropic--live-token (harmless-anthropic--read-store))
      (getenv "CLAUDE_CODE_OAUTH_TOKEN")
      (and harmless-anthropic-use-claude-auth
           (harmless-anthropic--token-from-claude-cli))))

(defun harmless-anthropic--exchange-code (code verifier state)
  "Exchange authorization CODE for tokens and store them."
  (let ((plist (harmless-anthropic--plist-from-token-response
                (harmless-anthropic--token-request
                 (list :grant_type "authorization_code"
                       :client_id harmless-anthropic-client-id
                       :code code
                       :redirect_uri harmless-anthropic-redirect-uri
                       :code_verifier verifier
                       :state state)))))
    (unless plist
      (error "Anthropic did not return an access token"))
    (harmless-anthropic--write-store plist)
    plist))

(defun harmless-anthropic-login (&optional _prefix)
  "Sign in to Anthropic / Claude through a web browser.
Claude shows an authorization code in the browser; paste it at the
prompt.  PREFIX is ignored (kept for `harmless-login')."
  (interactive)
  (let* ((pkce (harmless-xai--pkce))
         (state (harmless-xai--random-string 32))
         (url (harmless-anthropic--authorize-url state (cdr pkce))))
    (harmless-log "Anthropic opening browser for OAuth")
    (message "Harmless: opening browser to sign in with Claude…")
    (harmless-browse-url url)
    (let* ((raw (read-from-minibuffer
                 "Paste the Claude authorization code: "))
           (code (car (harmless-anthropic--parse-code raw))))
      (unless (and code (not (string-empty-p code)))
        (user-error "No authorization code"))
      (harmless-anthropic--exchange-code code (car pkce) state)
      (message "Harmless: signed in to Anthropic")
      t)))

(defun harmless-anthropic-logout ()
  "Forget the stored Anthropic OAuth session.
Does not delete `~/.claude/.credentials.json'."
  (interactive)
  (let ((file (harmless-anthropic-auth-file)))
    (when (file-exists-p file)
      (delete-file file)))
  (message "Harmless: signed out of Anthropic"))

(harmless-register-login-method
 'anthropic
 :name "Anthropic"
 :login #'harmless-anthropic-login
 :logout #'harmless-anthropic-logout
 :logged-in-p (lambda () (and (harmless-anthropic--read-store) t)))

(provide 'harmless-anthropic-oauth)

;;; harmless-anthropic-oauth.el ends here
