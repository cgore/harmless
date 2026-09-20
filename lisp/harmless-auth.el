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
;; picks among them (xAI, Anthropic, …).

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)

(defvar harmless-login-methods nil
  "Registered login methods as an alist of (ID . SPEC).
ID is a symbol such as `xai'.  SPEC is a plist:

  :name         Display name (\"xAI\")
  :login        Function of one argument, the prefix arg
  :logout       Function of no arguments
  :logged-in-p  Optional predicate, no arguments")

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

(defun harmless-login--read-method (prompt)
  "Return a login-method id, prompting with PROMPT if more than one exists."
  (cond
   ((null harmless-login-methods)
    (user-error "No login methods registered"))
   ((null (cdr harmless-login-methods))
    (caar harmless-login-methods))
   (t
    (let* ((names (mapcar (lambda (m) (plist-get (cdr m) :name))
                          harmless-login-methods))
           (choice (completing-read prompt names nil t)))
      (car (cl-find choice harmless-login-methods
                    :key (lambda (m) (plist-get (cdr m) :name))
                    :test #'string=))))))

;;;###autoload
(defun harmless-login (&optional method prefix)
  "Log in with a registered method.
If METHOD is nil and more than one method is registered, prompt.
With one method (currently xAI), that method runs immediately.
PREFIX is passed to the method; interactively this is the prefix arg
(for xAI, a prefix uses the device-code flow; Anthropic ignores it)."
  (interactive
   (list (harmless-login--read-method "Log in to: ")
         current-prefix-arg))
  (let* ((id (or method (harmless-login--read-method "Log in to: ")))
         (spec (or (harmless-login-method id)
                   (user-error "Unknown login method: %s" id))))
    (funcall (plist-get spec :login) prefix)))

;;;###autoload
(defun harmless-logout (&optional method)
  "Log out of a registered method.
If METHOD is nil and more than one method is registered, prompt."
  (interactive
   (list (harmless-login--read-method "Log out of: ")))
  (let* ((id (or method (harmless-login--read-method "Log out of: ")))
         (spec (or (harmless-login-method id)
                   (user-error "Unknown login method: %s" id)))
         (fn (plist-get spec :logout)))
    (unless fn
      (user-error "Login method %s has no logout" id))
    (funcall fn)))

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
