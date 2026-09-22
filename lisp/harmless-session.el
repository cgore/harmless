;;; harmless-session.el --- Session objects and persistence for Harmless -*- lexical-binding: t; -*-

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
;; A session is the unit of concurrency.  The registry is global.  Disk is
;; the source of truth; buffers are views.  `parent-id' is stored for a
;; later child-session slice.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'project)
(require 'harmless-util)
(require 'harmless-log)
(require 'harmless-provider)

(defcustom harmless-directory (locate-user-emacs-file "harmless/")
  "Root directory for Harmless config and sessions.
Defaults to `harmless/' under `user-emacs-directory' (for example
`~/.emacs.d/harmless/')."
  :type 'directory
  :group 'harmless)

(defcustom harmless-permission-mode 'ask
  "Default permission mode for new sessions.
`ask', `accept-edits', or `always-approve'."
  :type '(choice (const ask)
                 (const accept-edits)
                 (const always-approve))
  :group 'harmless)

(defvar harmless--sessions (make-hash-table :test 'equal)
  "Live sessions keyed by id.")

(defvar harmless-event-functions nil
  "Functions called as (SESSION EVENT) for UI and other observers.")

(cl-defstruct (harmless-session
               (:constructor harmless-session--create)
               (:copier nil))
  id
  title
  cwd
  provider
  provider-name
  model
  reasoning-effort
  messages
  status
  parent-id
  source
  permission-mode
  allow-classes
  prompt-tokens
  completion-tokens
  created-at
  updated-at
  directory
  process
  buffer
  prompt-buffer
  title-locked
  plan-mode)

(defun harmless-emit (session event)
  "Notify observers that EVENT happened on SESSION."
  (dolist (fn harmless-event-functions)
    (funcall fn session event)))

(defun harmless-session-effective-reasoning-effort (session)
  "Return SESSION's reasoning effort, falling back to the default."
  (or (harmless-session-reasoning-effort session)
      harmless-default-reasoning-effort))

(defun harmless-session-model-label (session)
  "Return SESSION's model with reasoning effort when set."
  (harmless-model-label (harmless-session-model session)
                        (harmless-session-effective-reasoning-effort session)))

(defun harmless-session-project-name (session)
  "Return a short project label for SESSION."
  (file-name-nondirectory
   (directory-file-name (or (harmless-session-cwd session) default-directory))))

(defun harmless-session-encode-cwd (cwd)
  "Return a filesystem-safe encoding of CWD."
  (harmless-url-encode (expand-file-name cwd)))

(defun harmless-session-dir (session)
  "Return the on-disk directory for SESSION, creating it if needed."
  (or (harmless-session-directory session)
      (let ((dir (expand-file-name
                  (harmless-session-id session)
                  (expand-file-name
                   (harmless-session-encode-cwd (harmless-session-cwd session))
                   (expand-file-name "sessions" harmless-directory)))))
        (harmless-ensure-directory dir)
        (setf (harmless-session-directory session) dir)
        dir)))

(defun harmless-session-register (session)
  "Add SESSION to the live registry."
  (puthash (harmless-session-id session) session harmless--sessions)
  session)

(defun harmless-session-unregister (session)
  "Remove SESSION from the live registry."
  (remhash (harmless-session-id session) harmless--sessions))

(defun harmless-session-get (id)
  "Return the live session with ID, or nil."
  (gethash id harmless--sessions))

(defun harmless-session-list ()
  "Return live sessions, newest updated first."
  (let (acc)
    (maphash (lambda (_id s) (push s acc)) harmless--sessions)
    (seq-sort (lambda (a b)
                (string> (or (harmless-session-updated-at a) "")
                         (or (harmless-session-updated-at b) "")))
              acc)))

(defun harmless-session-for-cwd (cwd)
  "Return live sessions whose cwd is CWD."
  (let ((root (expand-file-name cwd)))
    (seq-filter (lambda (s)
                  (string= (expand-file-name (harmless-session-cwd s)) root))
                (harmless-session-list))))

(defun harmless-current-cwd ()
  "Return the project root of the current buffer, or `default-directory'."
  (if-let* ((proj (project-current)))
      (expand-file-name (project-root proj))
    (expand-file-name default-directory)))

(defun harmless-session-new (&rest args)
  "Create, register, and persist a new session.
Keyword ARGS: :cwd :provider :model :reasoning-effort :parent-id :source
:permission-mode :title."
  (let* ((provider (or (plist-get args :provider) (harmless-default-provider)))
         (cwd (expand-file-name (or (plist-get args :cwd)
                                    (harmless-current-cwd))))
         (now (harmless-now-iso))
         (parsed (harmless-parse-model-spec
                  (or (plist-get args :model)
                      (and provider
                           (harmless-provider-default-model provider)))))
         (session (harmless-session--create
                   :id (or (plist-get args :id) (harmless-uuid))
                   :title (plist-get args :title)
                   :cwd cwd
                   :provider provider
                   :provider-name (and provider (harmless-provider-name provider))
                   :model (car parsed)
                   :reasoning-effort (or (plist-get args :reasoning-effort)
                                         (cdr parsed)
                                         harmless-default-reasoning-effort)
                   :messages nil
                   :status 'idle
                   :parent-id (plist-get args :parent-id)
                   :source (or (plist-get args :source) 'user)
                   :permission-mode (or (plist-get args :permission-mode)
                                        harmless-permission-mode)
                   :allow-classes nil
                   :prompt-tokens 0
                   :completion-tokens 0
                   :created-at now
                   :updated-at now
                   :title-locked (and (plist-get args :title) t))))
    (unless provider
      (error "No Harmless provider configured"))
    (harmless-session-register session)
    (harmless-session-save session)
    session))

(defun harmless-session-set-status (session status)
  "Set SESSION status to STATUS and notify observers."
  (setf (harmless-session-status session) status
        (harmless-session-updated-at session) (harmless-now-iso))
  (harmless-emit session (list :status status)))

(defun harmless-session-append (session message)
  "Append canonical MESSAGE to SESSION and persist."
  (setf (harmless-session-messages session)
        (append (harmless-session-messages session) (list message))
        (harmless-session-updated-at session) (harmless-now-iso))
  (unless (harmless-session-title-locked session)
    (when (and (memq (plist-get message :role) '(:user user))
               (not (harmless-session-title session)))
      (setf (harmless-session-title session)
            (harmless-truncate
             (replace-regexp-in-string "\n" " " (or (plist-get message :content) ""))
             60))))
  (harmless-session-save session)
  (harmless-emit session (list :message message))
  message)

(defun harmless-session-append-user (session text)
  "Append a user TEXT message to SESSION."
  (harmless-session-append session (list :role :user :content text)))

(defun harmless-session-summary-plist (session)
  "Return a plist suitable for summary.json."
  (list :id (harmless-session-id session)
        :title (harmless-session-title session)
        :cwd (harmless-session-cwd session)
        :provider (harmless-session-provider-name session)
        :model (harmless-session-model session)
        :reasoning-effort (harmless-session-reasoning-effort session)
        :status (format "%s" (harmless-session-status session))
        :parent-id (harmless-session-parent-id session)
        :source (format "%s" (harmless-session-source session))
        :permission-mode (format "%s" (harmless-session-permission-mode session))
        :prompt-tokens (harmless-session-prompt-tokens session)
        :completion-tokens (harmless-session-completion-tokens session)
        :created-at (harmless-session-created-at session)
        :updated-at (harmless-session-updated-at session)
        :title-locked (and (harmless-session-title-locked session) t)
        :plan-mode (and (harmless-session-plan-mode session) t)))

(defun harmless-session-save (session)
  "Write SESSION to disk."
  (let ((dir (harmless-session-dir session))
        (coding-system-for-write 'utf-8-unix)
        (buffer-file-coding-system 'utf-8-unix))
    (with-temp-file (expand-file-name "summary.json" dir)
      (setq buffer-file-coding-system 'utf-8-unix)
      (insert (harmless-json-text (harmless-session-summary-plist session))
              "\n"))
    (with-temp-file (expand-file-name "messages.jsonl" dir)
      (setq buffer-file-coding-system 'utf-8-unix)
      (dolist (msg (harmless-session-messages session))
        (insert (harmless-json-text msg) "\n")))))

(defun harmless-session--provider-by-name (name)
  "Find a registered provider named NAME."
  (cl-find name harmless-providers
           :key #'harmless-provider-name :test #'string=))

(defun harmless-session--parse-status (s)
  "Convert summary status string S to a symbol."
  (let ((sym (intern (or s "idle"))))
    (if (memq sym '(idle streaming waiting-permission error))
        (if (eq sym 'streaming) 'idle sym)
      'idle)))

(defun harmless-session--keywordize (value)
  "Turn a JSON string or symbol VALUE into a keyword."
  (cond
   ((keywordp value) value)
   ((symbolp value) (intern (concat ":" (symbol-name value))))
   ((stringp value) (intern (concat ":" value)))
   (t value)))

(defun harmless-session--normalize-message (msg)
  "Keywordize :role on MSG after a JSON round-trip."
  (let ((role (plist-get msg :role)))
    (if role
        (plist-put (copy-sequence msg) :role (harmless-session--keywordize role))
      msg)))

(defun harmless-session-load (dir)
  "Load a session from DIR and register it.  Return the session."
  (let* ((summary (harmless-json-decode
                   (with-temp-buffer
                     (insert-file-contents (expand-file-name "summary.json" dir))
                     (buffer-string))))
         (messages nil)
         (msg-file (expand-file-name "messages.jsonl" dir)))
    (when (file-exists-p msg-file)
      (with-temp-buffer
        (insert-file-contents msg-file)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (string-trim (buffer-substring (line-beginning-position)
                                                     (line-end-position)))))
            (unless (string-empty-p line)
              (push (harmless-session--normalize-message
                     (harmless-json-decode line))
                    messages)))
          (forward-line 1))))
    (let* ((provider-name (plist-get summary :provider))
           (provider (or (harmless-session--provider-by-name provider-name)
                         (harmless-default-provider)))
           (session (harmless-session--create
                     :id (plist-get summary :id)
                     :title (plist-get summary :title)
                     :cwd (plist-get summary :cwd)
                     :provider provider
                     :provider-name provider-name
                     :model (plist-get summary :model)
                     :reasoning-effort (plist-get summary :reasoning-effort)
                     :messages (nreverse messages)
                     :status (harmless-session--parse-status
                              (plist-get summary :status))
                     :parent-id (plist-get summary :parent-id)
                     :source (intern (or (plist-get summary :source) "user"))
                     :permission-mode (intern
                                       (or (plist-get summary :permission-mode)
                                           "ask"))
                     :allow-classes nil
                     :prompt-tokens (or (plist-get summary :prompt-tokens) 0)
                     :completion-tokens (or (plist-get summary :completion-tokens) 0)
                     :created-at (plist-get summary :created-at)
                     :updated-at (plist-get summary :updated-at)
                     :directory dir
                     :title-locked (plist-get summary :title-locked)
                     :plan-mode (harmless-json-true-p
                                 (plist-get summary :plan-mode)))))
      (harmless-session-register session)
      session)))

(defun harmless-session-list-on-disk ()
  "Return summary plists for every saved session."
  (let ((root (expand-file-name "sessions" harmless-directory))
        acc)
    (when (file-directory-p root)
      (dolist (cwd-dir (directory-files root t directory-files-no-dot-files-regexp))
        (when (file-directory-p cwd-dir)
          (dolist (sid-dir (directory-files cwd-dir t directory-files-no-dot-files-regexp))
            (let ((sum (expand-file-name "summary.json" sid-dir)))
              (when (file-exists-p sum)
                (push (cons sid-dir
                            (harmless-json-decode
                             (with-temp-buffer
                               (insert-file-contents sum)
                               (buffer-string))))
                      acc)))))))
    (nreverse acc)))

(defun harmless-session-resume (id)
  "Return a live session for ID, loading it from disk if needed."
  (or (harmless-session-get id)
      (cl-loop for (dir . summary) in (harmless-session-list-on-disk)
               when (string= (plist-get summary :id) id)
               return (harmless-session-load dir))))

(provide 'harmless-session)

;;; harmless-session.el ends here
