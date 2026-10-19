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
;;   M-x harmless-info         ; this manual
;;   M-x harmless-task-start   ; one prompt in another Emacs
;;   M-x harmless-publish      ; publish Markdown or HTML as a page
;;   M-x harmless-team-add-seat ; record one seat in a rig
;;   M-x harmless-team-edge    ; record an edge between seats
;;   M-x harmless-team-preview ; show a boot before it writes
;;   M-x harmless-team-boot    ; record a rig from a spec
;;   M-x harmless-team-grow    ; add a seat to a running rig
;;   M-x harmless-team-remove  ; drop a seat, moving work first
;;   M-x harmless-team-adopt   ; bind a session that is already running
;;   M-x harmless-team-release ; clear that binding without stopping it
;;   M-x harmless-team-send    ; type text onto a seat, a pod, or the rig
;;   M-x harmless-team-capture ; read the text on a seat's screen
;;   M-x harmless-team-transcript ; read text sent across occupants
;;   M-x harmless-team-room-say  ; append a line to the rig's room
;;   M-x harmless-team-room-history ; read that room, or one topic
;;   M-x harmless-team-room-wait ; block until someone speaks
;;   M-x harmless-team-pace    ; start a paced sequence for one seat
;;   M-x harmless-team-pace-add ; append one piece to that sequence
;;   M-x harmless-team-pace-step ; send the next piece, and only that
;;   M-x harmless-team-row     ; open owned work aimed at one seat
;;   M-x harmless-team-rows    ; read those rows, or one seat's rows
;;   M-x harmless-team-row-read ; read one row by its id
;;   M-x harmless-team-row-claim ; hold an open queue row
;;   M-x harmless-team-row-note ; append a note to a row
;;   M-x harmless-team-row-close ; close a held row with a reason
;;   M-x harmless-team-row-hand ; hand a held row to another seat
;;   M-x harmless-team-row-park ; park a held row on one wake
;;   M-x harmless-team-row-resolve ; record the decision and wake it
;;   M-x harmless-team-intake  ; record an observation nobody owns
;;   M-x harmless-team-board   ; held, stalled, or waiting rows
;;   M-x harmless-team-workflow ; declare steps; close opens the next
;;   M-x harmless-team-workflows ; read those declared workflows
;;   M-x harmless-team-watchdog ; record a job that survives a restart
;;   M-x harmless-team-watchdog-check ; read the jobs that are due
;;   M-x harmless-team-snapshot ; store a named copy of the rig
;;   M-x harmless-team-snapshot-check ; report each seat, and do not write
;;   M-x harmless-team-snapshot-restore ; write the copy back when it can
;;   M-x harmless-reload-all-harmless ; reload the Lisp checkout

;;; Code:

(require 'cl-lib)
(require 'info)
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
(require 'harmless-harness)
(require 'harmless-rules)
(require 'harmless-skills)
(require 'harmless-tools)
(require 'harmless-tools-fs)
(require 'harmless-output-style)
(require 'harmless-media)
(require 'harmless-image)
(require 'harmless-tools-shell)
(require 'harmless-perm)
(require 'harmless-turn)
(require 'harmless-plan)
(require 'harmless-ask)
(require 'harmless-agent)
(require 'harmless-workflow)
(require 'harmless-worktree)
(require 'harmless-message)
(require 'harmless-web)
(require 'harmless-x)
(require 'harmless-mcp)
(require 'harmless-plugin)
(require 'harmless-computer)
(require 'harmless-monitor)
(require 'harmless-schedule)
(require 'harmless-todo)
(require 'harmless-review)
(require 'harmless-lsp)
(require 'harmless-usage)
(require 'harmless-ui)
(require 'harmless-compact)
(require 'harmless-rewind)
(require 'harmless-edit)
(require 'harmless-fork)
(require 'harmless-dashboard)
(require 'harmless-transient)
(require 'harmless-headless)
(require 'harmless-page)
(require 'harmless-team)

(defvar harmless--config-loaded nil
  "Non-nil after the Harmless config file has been loaded.")

(defun harmless-config-file ()
  "Return the path of the optional Harmless config file.
This is `config.el' under `harmless-directory'."
  (expand-file-name "config.el" (harmless-data-directory)))

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
(defun harmless-load-skill (session name)
  "Load skill NAME into SESSION.
The skill body is sent on later turns.  Its allowed-tools list, when
present, is the only set of tools those turns can call.  NAME is a
skill id, not a path.  A prompt that starts with /NAME loads it too."
  (interactive
   (progn
     (harmless-ensure-configured)
     (list (or (harmless--context-session) (harmless-current))
           (read-string "Skill: "))))
  (unless (harmless-session-p session)
    (error "No Harmless session"))
  (harmless-skill-load session name))

;;;###autoload
(defun harmless-plugin-install (session marketplace plugin &optional scope)
  "Install PLUGIN from MARKETPLACE into SESSION.
MARKETPLACE is an absolute directory.  PLUGIN is a plugin id, not
a path.  SCOPE is `project' or `user'.  Nil is `project'.  The
install is one copy of the plugin directory.  Skills, agents,
hooks, and MCP servers in that copy are what the session sees."
  (interactive
   (progn
     (harmless-ensure-configured)
     (list (or (harmless--context-session) (harmless-current))
           (read-directory-name "Marketplace: ")
           (read-string "Plugin: ")
           (completing-read "Scope: " '("project" "user")
                            nil t nil nil "project"))))
  (harmless-plugin--install session marketplace plugin scope))

;;;###autoload
(defun harmless-output-style (session name)
  "Switch SESSION to output style NAME.
NAME is a style id, not a path.  Nil, \"\", and \"default\" clear
the style.  The body is the role, tone, and format for later
turns.  Project instruction files are not edited."
  (interactive
   (progn
     (harmless-ensure-configured)
     (let ((session (or (harmless--context-session) (harmless-current))))
       (list session
             (completing-read
              "Output style: "
              (cons "default"
                    (mapcar (lambda (style)
                              (plist-get style :name))
                            (harmless-output-style-list session)))
              nil t nil nil "default")))))
  (harmless-output-style-set session name))

;;;###autoload
(defun harmless-rename (title &optional session)
  "Set SESSION's title to TITLE.
The session id stays.  Later prompts do not replace a title set here."
  (interactive
   (let ((session (or (harmless--context-session) (harmless-current))))
     (list (read-string "Session title: "
                        (or (harmless-session-title session) ""))
           session)))
  (unless (harmless-session-p session)
    (setq session (or (harmless--context-session) (harmless-current))))
  (harmless-session-rename session title))

;;;###autoload
(defun harmless-delete-session (&optional session)
  "Delete SESSION from the registry and from disk.
An interactive call asks first.  A busy turn is refused."
  (interactive)
  (unless (harmless-session-p session)
    (setq session (or (harmless--context-session) (harmless-current))))
  (when (and (called-interactively-p 'any)
             (not (yes-or-no-p
                   (format "Delete session %s? "
                           (or (harmless-session-title session)
                               (harmless-session-id session))))))
    (user-error "session delete was cancelled"))
  (harmless-session-delete session))

;;;###autoload
(defun harmless-export-session (path &optional session)
  "Write SESSION's transcript as Markdown at PATH.
A relative path stays inside the project."
  (interactive
   (list (read-file-name "Export session to: ")
         (or (harmless--context-session) (harmless-current))))
  (unless (harmless-session-p session)
    (setq session (or (harmless--context-session) (harmless-current))))
  (harmless-session-export session path))

(define-key harmless-session-mode-map (kbd "t") #'harmless-rename)
(define-key harmless-session-mode-map (kbd "x") #'harmless-export-session)
(define-key harmless-session-mode-map (kbd "X") #'harmless-delete-session)

;;;###autoload
(defun harmless-task-start (directory prompt)
  "Start PROMPT in DIRECTORY in another Emacs and return the task id.
That process keeps running after this Emacs exits.  The task file
is under `harmless-directory'.  This command does not wait for
the reply and does not open a window for the worker."
  (interactive
   (progn
     (harmless-ensure-configured)
     (list (read-directory-name "Project: " nil nil t)
           (read-string "Prompt: "))))
  (harmless-ensure-configured)
  (harmless-headless-start directory prompt))

;;;###autoload
(defun harmless-task-status (id)
  "Show the stored status of task ID and return the task plist."
  (interactive (list (read-string "Task id: ")))
  (let ((task (harmless-headless-status id)))
    (message "Harmless task %s: %s"
             (plist-get task :id)
             (plist-get task :status))
    task))

(defun harmless-headless-batch ()
  "Run one prompt from the remaining arguments and exit.
The next two arguments are the project directory and the prompt.
A leading \"--\" is skipped.  The reply is printed.  This quits
Emacs."
  (harmless-ensure-configured)
  (condition-case err
      (let ((directory (harmless-headless--pop-arg))
            (prompt (harmless-headless--pop-arg)))
        (princ (harmless-headless-run directory prompt))
        (princ "\n")
        (kill-emacs 0))
    (error
     (princ (error-message-string err))
     (princ "\n")
     (kill-emacs 1))))

;;;###autoload
(defun harmless-publish (source &optional directory path)
  "Publish SOURCE as an HTML page and return the page path.
DIRECTORY is the project.  Nil DIRECTORY uses the current
project.  Nil PATH writes `.harmless/pages/NAME.html' inside
the project.  Markdown is rendered.  HTML is copied.  An
interactive call opens the page."
  (interactive
   (progn
     (harmless-ensure-configured)
     (list (read-file-name "Publish: "))))
  (unless directory
    (setq directory (harmless-current-cwd)))
  (let ((page (harmless-page-publish directory source path)))
    (when (called-interactively-p 'any)
      (browse-url
       (if (string-match-p "\\`[A-Za-z]:" page)
           (concat "file:///" page)
         (concat "file://" page))))
    (message "Published %s" page)
    page))

(defun harmless-headless-work ()
  "Finish the task id in the remaining arguments and exit.
A leading \"--\" is skipped.  This is the entry the detached
Emacs runs.  This quits Emacs."
  (harmless-ensure-configured)
  (condition-case err
      (progn
        (harmless-headless-finish (harmless-headless--pop-arg))
        (kill-emacs 0))
    (error
     (princ (error-message-string err))
     (princ "\n")
     (kill-emacs 1))))

;;;###autoload
(defun harmless-current ()
  "Open or resume the most recent session for the current project."
  (interactive)
  (harmless-ensure-configured)
  (let* ((cwd (harmless-current-cwd))
         (session (or (car (harmless-session-for-cwd cwd))
                      (let ((id (cl-loop
                                 for (_dir . summary) in (harmless-session-list-on-disk)
                                 when (harmless-same-directory-p
                                       (plist-get summary :cwd) cwd)
                                 return (plist-get summary :id))))
                        (and id (harmless-session-resume id)))
                      (harmless-session-new :cwd cwd))))
    (harmless-ui-open-session session)))

;;;###autoload
(defun harmless-new (&optional cwd)
  "Start a new Harmless session in CWD (default: current project)."
  (interactive)
  (harmless-ensure-configured)
  (let* ((cwd (harmless-absolute-directory
               (if (and (stringp cwd) (not (string-empty-p cwd)))
                   cwd
                 (harmless-current-cwd))))
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
(defun harmless-chat ()
  "Start a new Harmless chat with no project and no working directory."
  (interactive)
  (harmless-ensure-configured)
  (let* ((choice (and (not noninteractive)
                      (called-interactively-p 'interactive)
                      (harmless--read-provider-model-effort)))
         (provider (or (nth 2 choice) (harmless-default-provider)))
         (model (or (nth 0 choice)
                    harmless-default-model
                    (harmless-provider-default-model provider)))
         (effort (nth 1 choice))
         (session (harmless-session-new :detached t
                                        :provider provider
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
                                                (harmless-place-name
                                                 (plist-get summary :cwd))
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
  "Set the current session's permission MODE.
MODE is `ask', `accept-edits', `always-approve', or `auto'.
An unknown mode is refused and the session is left unchanged."
  (interactive
   (list (intern (completing-read
                  "Permission mode: "
                  '("ask" "accept-edits" "always-approve" "auto")
                  nil t
                  (format "%s"
                          (and (harmless--context-session)
                               (harmless-session-permission-mode
                                (harmless--context-session))))))))
  (let ((session (or (harmless--context-session)
                     (user-error "No Harmless session"))))
    (unless (harmless-session-permission-mode-p mode)
      (error "permission mode is not a mode"))
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

(defun harmless-manual-directory (sources)
  "Return the directory of the Harmless Info manual.
SOURCES is the directory that holds `harmless.el'.  A trailing slash
does not change the result.  Nil, the empty string, a relative path,
and the filesystem root are refused.  A missing directory is not the
filesystem root, and this function does not create one."
  (unless (and (stringp sources)
               (not (string-empty-p sources))
               (file-name-absolute-p sources))
    (error "Harmless Lisp directory must be absolute, not %S" sources))
  (let* ((lisp (file-name-as-directory
                (harmless-collapse-path (expand-file-name sources))))
         (root (file-name-directory (directory-file-name lisp))))
    (when (or (not (stringp root))
              (harmless-filesystem-root-p lisp)
              (harmless-filesystem-root-p root))
      (error "Cannot place the Harmless manual at the filesystem root"))
    (file-name-as-directory (expand-file-name "doc" root))))

(defun harmless-manual-file (&optional sources)
  "Return the absolute path of the Harmless Info file.
SOURCES defaults to the directory of the loaded `harmless.el'."
  (expand-file-name
   "harmless.info"
   (harmless-manual-directory
    (or sources (harmless--lisp-directory)))))

(defun harmless-manual-register (&optional sources)
  "Add the Harmless manual directory to `Info-additional-directory-list'.
SOURCES defaults to the directory of the loaded `harmless.el'.
The same directory is not added twice.  Info searches this list after
`Info-directory-list', including when `C-h i' builds the directory."
  (let ((dir (harmless-manual-directory
              (or sources (harmless--lisp-directory)))))
    (unless (cl-some (lambda (existing)
                       (harmless-same-directory-p existing dir))
                     Info-additional-directory-list)
      (add-to-list 'Info-additional-directory-list dir))
    dir))

;;;###autoload
(defun harmless-info ()
  "Open the Harmless Info manual."
  (interactive)
  (let ((file (harmless-manual-file)))
    (unless (file-readable-p file)
      (error "Harmless manual is missing: %s" file))
    (harmless-manual-register)
    (info file)))

(harmless-manual-register)

(provide 'harmless)

;;; harmless.el ends here
