;;; harmless-auth.el --- API key lookup for Harmless -*- lexical-binding: t; -*-

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
;; Resolve API keys from an explicit value, the environment, then auth-source.
;; Keys are never written to the log.
;;
;; Login methods (browser OAuth, device code, …) register here.  `harmless-login'
;; asks which one to use.  Named connections are listed alongside the
;; vendors, so a single configured xAI account does not hide Anthropic.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)

(declare-function harmless-provider-p "harmless-provider")
(declare-function harmless-provider-name "harmless-provider")
(declare-function harmless-provider-login-vendor "harmless-provider")

(defvar harmless-login-methods nil
  "Registered login methods as an alist of (ID . SPEC).
ID is a symbol such as `xai'.  SPEC is a plist:

  :name         Display name (\"xAI\")
  :login        Function of one argument, the prefix arg
  :logout       Function of no arguments
  :logged-in-p  Optional predicate, no arguments")

(defvar harmless-oauth-provider nil
  "Provider/connection whose OAuth store is in use, or nil for the default.")

(defconst harmless-connection-default-names
  '((xai . "xAI")
    (anthropic . "Anthropic")
    (openai . "OpenAI"))
  "Default connection names that reuse the original per-vendor auth files.")

(defun harmless-connection-slug (name)
  "Return a filesystem slug for connection NAME."
  (let ((s (replace-regexp-in-string
            "[^a-z0-9]+" "-" (downcase (or name "")) t t)))
    (replace-regexp-in-string "\\`-+\\|-+\\'" "" s t t)))

(defun harmless-connection-default-p (provider vendor)
  "Return non-nil if PROVIDER uses VENDOR's original auth file."
  (let ((name (and provider
                   (fboundp 'harmless-provider-name)
                   (harmless-provider-name provider)))
        (default (cdr (assq vendor harmless-connection-default-names))))
    (or (null name)
        (null default)
        (string= name default))))

(defun harmless-connection-display-name (&optional provider fallback)
  "Return PROVIDER's connection name, or FALLBACK."
  (or (and (or provider harmless-oauth-provider)
           (fboundp 'harmless-provider-name)
           (harmless-provider-name (or provider harmless-oauth-provider)))
      fallback
      "account"))

(defun harmless-connection-auth-file (provider default-file vendor)
  "Return the OAuth JSON path for PROVIDER.
DEFAULT-FILE is used for the default VENDOR connection so existing
logins keep working.  Other connections use `auth-SLUG.json'."
  (let ((p (or provider harmless-oauth-provider)))
    (expand-file-name
     (if (harmless-connection-default-p p vendor)
         default-file
       (format "auth-%s.json"
               (harmless-connection-slug
                (and p (fboundp 'harmless-provider-name)
                     (harmless-provider-name p)))))
     (if (boundp 'harmless-directory)
         harmless-directory
       (locate-user-emacs-file "harmless/")))))

(defun harmless-login--id (id)
  "Normalize ID to a symbol."
  (cond
   ((symbolp id) id)
   ((stringp id) (intern (downcase id)))
   (t (error "Invalid login method id: %S" id))))

(defun harmless-register-login-method (id &rest spec)
  "Register a login method ID with SPEC plist.  See `harmless-login-methods'."
  (setq id (harmless-login--id id))
  (unless (functionp (plist-get spec :login))
    (error "Login method %s needs a :login function" id))
  (unless (plist-get spec :name)
    (setq spec (plist-put spec :name (symbol-name id))))
  (setq harmless-login-methods
        (cons (cons id spec)
              (assq-delete-all id (copy-sequence harmless-login-methods))))
  id)

(defun harmless-login-method (id)
  "Return the spec plist for login method ID, or nil."
  (cdr (assq (harmless-login--id id) harmless-login-methods)))

(defun harmless-login--oauth-providers ()
  "Return configured providers that support browser login."
  (and (boundp 'harmless-providers)
       (fboundp 'harmless-provider-login-vendor)
       (cl-remove-if-not #'harmless-provider-login-vendor
                         harmless-providers)))

(defun harmless-login--choices ()
  "Return an alist of (LABEL . TARGET) for the login prompt.
TARGET is a provider object or a vendor symbol.  Every registered
login method is included.  A configured connection replaces the vendor
entry that has the same name."
  (let ((table (make-hash-table :test 'equal))
        names)
    (dolist (method harmless-login-methods)
      (let ((name (plist-get (cdr method) :name)))
        (when (and name (not (string-empty-p name)))
          (puthash name (car method) table))))
    (dolist (conn (harmless-login--oauth-providers))
      (puthash (harmless-provider-name conn) conn table))
    (maphash (lambda (name _target) (push name names)) table)
    (mapcar (lambda (name) (cons name (gethash name table)))
            (sort names #'string<))))

(defun harmless-login--read-target (prompt)
  "Return a provider or a vendor symbol, prompting with PROMPT when needed."
  (let ((choices (harmless-login--choices)))
    (cond
     ((null choices)
      (user-error "No login methods registered"))
     ((null (cdr choices))
      (cdar choices))
     (t
      (cdr (assoc (completing-read prompt (mapcar #'car choices) nil t)
                  choices))))))

(defun harmless-login--provider-for-vendor (vendor)
  "Return a provider for VENDOR, or nil to use the default auth file."
  (let ((conns (cl-remove-if-not
                (lambda (p)
                  (eq vendor (harmless-provider-login-vendor p)))
                (or (harmless-login--oauth-providers) nil)))
        (default (cdr (assq vendor harmless-connection-default-names))))
    (cond
     ((null conns) nil)
     ((null (cdr conns)) (car conns))
     ((and default
           (cl-find default conns :key #'harmless-provider-name :test #'string=)))
     (t (car conns)))))

(defun harmless-login--run (target prefix)
  "Run the login method for TARGET with PREFIX.
TARGET is a provider object or a vendor symbol."
  (cond
   ((and (fboundp 'harmless-provider-p)
         (harmless-provider-p target))
    (let ((vendor (and (fboundp 'harmless-provider-login-vendor)
                       (harmless-provider-login-vendor target)))
          (harmless-oauth-provider target))
      (unless vendor
        (user-error "%s has no browser login"
                    (harmless-provider-name target)))
      (harmless-login--run vendor prefix)))
   (t
    (let* ((id (harmless-login--id target))
           (spec (or (harmless-login-method id)
                     (user-error "Unknown login method: %s" id)))
           (harmless-oauth-provider
            (or harmless-oauth-provider
                (and (fboundp 'harmless-provider-login-vendor)
                     (harmless-login--provider-for-vendor id)))))
      (funcall (plist-get spec :login) prefix)))))

;;;###autoload
(defun harmless-login (&optional target prefix)
  "Log in to TARGET, a named connection or a vendor symbol.
If TARGET is nil, ask.  The choices are the registered login methods
and any named connections.
PREFIX is passed to the vendor login; interactively this is the
prefix arg (xAI device-code, OpenAI paste-redirect)."
  (interactive
   (list (harmless-login--read-target "Log in to: ")
         current-prefix-arg))
  (harmless-login--run
   (or target (harmless-login--read-target "Log in to: "))
   prefix))

;;;###autoload
(defun harmless-logout (&optional target)
  "Log out of TARGET, a named connection or a vendor symbol.
If TARGET is nil, prompt."
  (interactive
   (list (harmless-login--read-target "Log out of: ")))
  (let ((target (or target (harmless-login--read-target "Log out of: "))))
    (cond
     ((and (fboundp 'harmless-provider-p)
           (harmless-provider-p target))
      (let ((vendor (and (fboundp 'harmless-provider-login-vendor)
                         (harmless-provider-login-vendor target)))
            (harmless-oauth-provider target))
        (unless vendor
          (user-error "%s has no browser login"
                      (harmless-provider-name target)))
        (harmless-logout vendor)))
     (t
      (let* ((id (harmless-login--id target))
             (spec (or (harmless-login-method id)
                       (user-error "Unknown login method: %s" id)))
             (fn (plist-get spec :logout))
             (harmless-oauth-provider
              (or harmless-oauth-provider
                  (and (fboundp 'harmless-provider-login-vendor)
                       (harmless-login--provider-for-vendor id)))))
        (unless fn
          (user-error "Login method %s has no logout" id))
        (funcall fn))))))

(defun harmless-auth-key (host key key-env)
  "Return an API key for HOST.
KEY wins if it is a non-empty string or a function of no arguments.
Otherwise KEY-ENV is read with `getenv'.  Finally auth-source is
searched for HOST with user `apikey'."
  (or (cond
       ((functionp key) (funcall key))
       ((and (stringp key) (not (string-empty-p key))) key)
       (t nil))
      (and key-env (getenv key-env))
      (harmless-auth--auth-source host)))

(defun harmless-auth--auth-source (host)
  "Return a secret from auth-source for HOST, if any."
  (when host
    (let ((found (nth 0 (auth-source-search
                         :host host
                         :user "apikey"
                         :max 1
                         :require '(:secret)))))
      (when found
        (let ((secret (plist-get found :secret)))
          (if (functionp secret) (funcall secret) secret))))))

(provide 'harmless-auth)

;;; harmless-auth.el ends here
