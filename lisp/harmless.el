;;; harmless.el --- AI coding harness inside Emacs -*- lexical-binding: t; -*-

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

;; Author: Christopher Mark Gore <cgore@cgore.com>
;; Maintainer: Christopher Mark Gore <cgore@cgore.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/cgore/harmless

;;; Commentary:
;;
;; Harmless is an AI coding harness that lives entirely inside Emacs.  It is
;; not gptel, and it does not wrap Grok Build or Claude CLI.
;;
;; Several sessions can run at once, each pinned to a provider and model
;; (xAI, OpenAI, Anthropic, or any OpenAI-compatible local server).
;;
;; Entry points:
;;
;;   M-x harmless              ; current project's session
;;   M-x harmless-new          ; new session
;;   M-x harmless-login        ; sign in (picks a provider when several exist)
;;   M-x harmless-dashboard    ; all sessions
;;   M-x harmless-menu         ; transient
;;   M-x harmless-reload-all-harmless ; reload the Lisp checkout

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-log)
(require 'harmless-auth)
(require 'harmless-http)
(require 'harmless-provider)
(require 'harmless-xai)
(require 'harmless-openai)
(require 'harmless-openai-oauth)
(require 'harmless-anthropic)
(require 'harmless-anthropic-oauth)
(require 'harmless-session)
(require 'harmless-instructions)
(require 'harmless-skills)
(require 'harmless-tools)
(require 'harmless-tools-fs)
(require 'harmless-tools-shell)
(require 'harmless-perm)
(require 'harmless-turn)
(require 'harmless-plan)
(require 'harmless-usage)
(require 'harmless-ui)
(require 'harmless-dashboard)
(require 'harmless-transient)

(defvar harmless--config-loaded nil
  "Non-nil after the Harmless config file has been loaded.")

(defun harmless-config-file ()
  "Return the path of the optional Harmless config file.
This is `config.el' under `harmless-directory'."
  (expand-file-name "config.el" harmless-directory))

(defun harmless-load-config ()
  "Load `config.el' from `harmless-directory' once."
  (unless harmless--config-loaded
    (setq harmless--config-loaded t)
    (let ((file (harmless-config-file)))
      (when (file-exists-p file)
        (load file nil t)))))

(defun harmless-setup ()
  "Interactively register a first provider."
  (interactive)
  (let* ((kind (completing-read "Provider: "
                                '("xAI" "OpenAI" "Anthropic" "OpenAI-compatible")
                                nil t))
         (provider
          (pcase kind
            ("xAI" (harmless-make-xai
                    (read-string "Connection name: " "xAI")))
            ("OpenAI" (harmless-make-openai
                       (read-string "Connection name: " "OpenAI")))
            ("Anthropic" (harmless-make-anthropic
                          (read-string "Connection name: " "Anthropic")))
            ("OpenAI-compatible"
             (harmless-make-openai-compat
              (read-string "Name: " "local")
              :host (read-string "Host (host:port): " "127.0.0.1:11434")
              :protocol (completing-read "Protocol: " '("http" "https") nil t "http")
              :endpoint (read-string "Endpoint: " "/v1/chat/completions")
              :key (read-string "Key (empty if none): " "")
              :key-env nil
              :models (split-string (read-string "Models (space-separated): ")
                                    " " t))))))
    (when (and (harmless-provider-key provider)
               (string-empty-p (harmless-provider-key provider)))
      (setf (harmless-provider-key provider) nil))
    (harmless-register-provider provider)
    (when (and (harmless-xai-provider-p provider)
               (not (harmless-xai-token provider))
               (not noninteractive)
               (y-or-n-p (format "Sign in to %s in a browser? "
                                 (harmless-provider-name provider))))
      (harmless-login provider))
    (when (and (fboundp 'harmless-anthropic-provider-p)
               (harmless-anthropic-provider-p provider)
               (not (and (fboundp 'harmless-anthropic-token)
                         (harmless-anthropic-token provider)))
               (not noninteractive)
               (y-or-n-p (format "Sign in to %s in a browser? "
                                 (harmless-provider-name provider))))
      (harmless-login provider))
    (when (and (fboundp 'harmless-openai-official-p)
               (harmless-openai-official-p provider)
               (not (and (fboundp 'harmless-openai-token)
                         (harmless-openai-token provider)))
               (not noninteractive)
               (y-or-n-p (format "Sign in to %s in a browser? "
                                 (harmless-provider-name provider))))
      (harmless-login provider))
    (setq harmless-default-provider-name (harmless-provider-name provider))
    (when-let* ((models (harmless-provider-model-list provider)))
      (setq harmless-default-model
            (completing-read "Default model: " models nil t (car models))))
    provider))

(defun harmless-ensure-configured ()
  "Load config and ensure at least one provider exists."
  (harmless-load-config)
  (unless harmless-providers
    (if noninteractive
        (error "No Harmless providers configured")
      (harmless-setup))))

(defun harmless--context-session ()
  "Return the session attached to the current buffer, if any."
  (or (and (local-variable-p 'harmless--session) harmless--session)
      (car (harmless-session-for-cwd (harmless-current-cwd)))))

;;;###autoload
(defun harmless ()
  "Open Harmless for the current project.
With a prefix argument, open the dashboard instead."
  (interactive)
  (harmless-ensure-configured)
  (if current-prefix-arg
      (harmless-dashboard)
    (harmless-current)))

;;;###autoload
(defun harmless-current ()
  "Open or resume the most recent session for the current project."
  (interactive)
  (harmless-ensure-configured)
  (let* ((cwd (harmless-current-cwd))
         (session (or (car (harmless-session-for-cwd cwd))
                      (let ((id (cl-loop
                                 for (_dir . summary) in (harmless-session-list-on-disk)
                                 when (string= (expand-file-name
                                                (or (plist-get summary :cwd) ""))
                                               cwd)
                                 return (plist-get summary :id))))
                        (and id (harmless-session-resume id)))
                      (harmless-session-new :cwd cwd))))
    (harmless-ui-open-session session)))

;;;###autoload
(defun harmless-new (&optional cwd)
  "Start a new Harmless session in CWD (default: current project)."
  (interactive)
  (harmless-ensure-configured)
  (let* ((cwd (expand-file-name (or cwd (harmless-current-cwd))))
         (choice (and (not noninteractive)
                      (called-interactively-p 'interactive)
                      (harmless--read-provider-model-effort)))
         (provider (or (nth 2 choice) (harmless-default-provider)))
         (model (or (nth 0 choice)
                    harmless-default-model
                    (harmless-provider-default-model provider)))
         (effort (nth 1 choice))
         (session (harmless-session-new :cwd cwd :provider provider
                                        :model model
                                        :reasoning-effort effort)))
    (harmless-ui-open-session session)))

;;;###autoload
(defun harmless-switch ()
  "Switch to a live or saved Harmless session."
  (interactive)
  (harmless-ensure-configured)
  (let* ((items (append
                 (mapcar (lambda (s)
                           (cons (format "%s %s [%s]"
                                         (harmless-session-project-name s)
                                         (or (harmless-session-title s) "(untitled)")
                                         (harmless-session-id s))
                                 (harmless-session-id s)))
                         (harmless-session-list))
                 (cl-loop for (_dir . summary) in (harmless-session-list-on-disk)
                          for id = (plist-get summary :id)
                          unless (harmless-session-get id)
                          collect (cons (format "%s %s [%s]"
                                                (file-name-nondirectory
                                                 (directory-file-name
                                                  (or (plist-get summary :cwd) "?")))
                                                (or (plist-get summary :title) "(untitled)")
                                                id)
                                        id))))
         (choice (completing-read "Session: " items nil t)))
    (harmless-ui-open-session
     (harmless-session-resume (cdr (assoc choice items))))))

(defun harmless--read-provider (&optional default)
  "Prompt for a logged-in provider, defaulting to DEFAULT.
Skip the prompt when only one provider is available."
  (let ((ready (harmless-available-providers)))
    (cond
     ((null ready)
      (user-error "No logged-in providers.  Run M-x harmless-login"))
     ((null (cdr ready))
      (car ready))
     (t
      (let* ((default-name (and default (harmless-provider-name default)))
             (ok (and default-name
                      (cl-find default-name ready
                               :key #'harmless-provider-name
                               :test #'string=)))
             (name (completing-read "Provider: "
                                    (mapcar #'harmless-provider-name ready)
                                    nil t
                                    (and ok default-name))))
        (or (cl-find name ready
                     :key #'harmless-provider-name
                     :test #'string=)
            (car ready)))))))

(defun harmless--read-provider-model-effort ()
  "Read provider, then model, then effort.  Return (MODEL EFFORT PROVIDER)."
  (let* ((session (harmless--context-session))
         (provider (harmless--read-provider
                    (and session (harmless-session-provider session))))
         (models (or (harmless-provider-model-list provider)
                     (user-error "Provider %s has no models"
                                 (harmless-provider-name provider))))
         (same-provider (and session
                             (equal (harmless-provider-name provider)
                                    (harmless-session-provider-name session))))
         (model (if (cdr models)
                    (completing-read
                     "Model: " models nil t
                     (and same-provider (harmless-session-model session)))
                  (car models)))
         (parsed (harmless-parse-model-spec model))
         (model (car parsed))
         (levels (harmless-model-effort-levels model))
         (want (or (and same-provider
                        (harmless-session-effective-reasoning-effort session))
                   harmless-default-reasoning-effort
                   "high"))
         (effort
          (or (cdr parsed)
              (and levels
                   (completing-read
                    "Reasoning effort: "
                    levels
                    nil t
                    (if (member want levels) want (car levels)))))))
    (list model effort provider)))

;;;###autoload
(defun harmless-pick-model ()
  "Choose provider, model, and reasoning effort for the current session."
  (interactive)
  (apply #'harmless-set-model (harmless--read-provider-model-effort)))

;;;###autoload
(defun harmless-set-model (model &optional effort provider)
  "Set the current session's MODEL, optional EFFORT, and optional PROVIDER.
Interactively, prompt provider, then model, then effort."
  (interactive (harmless--read-provider-model-effort))
  (let ((session (or (harmless--context-session)
                     (user-error "No Harmless session")))
        (parsed (harmless-parse-model-spec model)))
    (when provider
      (setf (harmless-session-provider session) provider
            (harmless-session-provider-name session)
            (harmless-provider-name provider)))
    (setf (harmless-session-model session) (car parsed)
          (harmless-session-updated-at session) (harmless-now-iso))
    (cond
     ((cdr parsed)
      (setf (harmless-session-reasoning-effort session) (cdr parsed)))
     (effort
      (setf (harmless-session-reasoning-effort session) effort))
     ((not (harmless-model-supports-effort-p (car parsed)))
      (setf (harmless-session-reasoning-effort session) nil)))
    (harmless-session-save session)
    (force-mode-line-update t)
    (message "Harmless model: %s/%s"
             (or (harmless-session-provider-name session) "?")
             (harmless-session-model-label session))))

;;;###autoload
(defun harmless-set-reasoning-effort (effort)
  "Set the current session's reasoning EFFORT (low, medium, high, xhigh).
Empty input clears the session override so the default is used."
  (interactive
   (list (let* ((session (harmless--context-session))
                (levels (cons ""
                              (or (and session
                                       (harmless-model-effort-levels
                                        (harmless-session-model session)))
                                  harmless-reasoning-efforts)))
                (choice (completing-read
                         "Reasoning effort: "
                         levels
                         nil t
                         (or (and session
                                  (harmless-session-effective-reasoning-effort
                                   session))
                             ""))))
           (and (not (string-empty-p choice)) choice))))
  (let ((session (or (harmless--context-session)
                     (user-error "No Harmless session"))))
    (setf (harmless-session-reasoning-effort session) effort
          (harmless-session-updated-at session) (harmless-now-iso))
    (harmless-session-save session)
    (force-mode-line-update t)
    (message "Harmless effort: %s"
             (or (harmless-session-effective-reasoning-effort session)
                 "provider default"))))

;;;###autoload
(defun harmless-set-permission-mode (mode)
  "Set the current session's permission MODE."
  (interactive
   (list (intern (completing-read "Permission mode: "
                                  '("ask" "accept-edits" "always-approve")
                                  nil t
                                  (format "%s"
                                          (and (harmless--context-session)
                                               (harmless-session-permission-mode
                                                (harmless--context-session))))))))
  (let ((session (or (harmless--context-session)
                     (user-error "No Harmless session"))))
    (setf (harmless-session-permission-mode session) mode)
    (harmless-session-save session)
    (message "Harmless permissions: %s" mode)))

(defun harmless--lisp-directory ()
  "Return the directory holding the Harmless `.el' sources.
This is the directory of `harmless.el' on `load-path', so a reload
picks up the checkout Emacs actually loaded."
  (let ((lib (locate-library "harmless.el" t)))
    (unless lib
      (error "Cannot find harmless.el on `load-path'"))
    (file-name-directory (file-truename lib))))

(defun harmless--source-files ()
  "Return absolute paths of Harmless `.el' sources, in name order."
  (cl-remove-if
   (lambda (file)
     (string= (file-name-nondirectory file) "harmless-autoloads.el"))
   (directory-files (harmless--lisp-directory) t "\\.el\\'")))

;;;###autoload
(defun harmless-reload-all-harmless ()
  "Reload every Harmless Lisp source file into this Emacs.
Loads each `.el' file beside `harmless.el' and sets `load-prefer-newer'
so a newer source file wins over a byte-compiled copy.  Restart Emacs
when a live session object was created before a struct slot was added."
  (interactive)
  (setq load-prefer-newer t)
  (let ((files (harmless--source-files)))
    (unless files
      (error "No Harmless source files in %s" (harmless--lisp-directory)))
    (dolist (file files)
      (load file nil t))
    (message "Harmless reloaded (%d files)" (length files))
    files))

(provide 'harmless)

;;; harmless.el ends here
