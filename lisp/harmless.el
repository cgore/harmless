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
;; Package-Requires: ((emacs "32.0"))
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
;;   M-x harmless-dashboard    ; all sessions
;;   M-x harmless-menu         ; transient

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-log)
(require 'harmless-auth)
(require 'harmless-http)
(require 'harmless-provider)
(require 'harmless-openai)
(require 'harmless-anthropic)
(require 'harmless-session)
(require 'harmless-tools)
(require 'harmless-tools-fs)
(require 'harmless-tools-shell)
(require 'harmless-perm)
(require 'harmless-turn)
(require 'harmless-ui)
(require 'harmless-dashboard)
(require 'harmless-transient)

(defvar harmless--config-loaded nil
  "Non-nil after `~/.harmless/config.el' has been loaded.")

(defun harmless-load-config ()
  "Load `config.el' from `harmless-directory' once."
  (unless harmless--config-loaded
    (setq harmless--config-loaded t)
    (let ((file (expand-file-name "config.el" harmless-directory)))
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
            ("xAI" (harmless-make-xai))
            ("OpenAI" (harmless-make-openai))
            ("Anthropic" (harmless-make-anthropic "Anthropic"))
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
    (setq harmless-default-provider-name (harmless-provider-name provider))
    (when-let* ((models (harmless-provider-models provider)))
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
         (provider (harmless-default-provider))
         (models (harmless-provider-models provider))
         (model (if (and models (not noninteractive) (called-interactively-p 'interactive))
                    (completing-read "Model: " models nil t
                                     (or harmless-default-model (car models)))
                  (or harmless-default-model
                      (harmless-provider-default-model provider))))
         (session (harmless-session-new :cwd cwd :provider provider :model model)))
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

;;;###autoload
(defun harmless-set-model (model)
  "Set the current session's MODEL."
  (interactive
   (let* ((session (or (harmless--context-session)
                       (user-error "No Harmless session")))
          (models (harmless-provider-models
                   (harmless-session-provider session))))
     (list (completing-read "Model: " models nil t
                            (harmless-session-model session)))))
  (let ((session (or (harmless--context-session)
                     (user-error "No Harmless session"))))
    (setf (harmless-session-model session) model
          (harmless-session-updated-at session) (harmless-now-iso))
    (harmless-session-save session)
    (force-mode-line-update t)
    (message "Harmless model: %s" model)))

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

(provide 'harmless)

;;; harmless.el ends here
