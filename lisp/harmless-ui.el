;;; harmless-ui.el --- Session and prompt buffers for Harmless -*- lexical-binding: t; -*-

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
;; Read-only session transcript plus a separate prompt window.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-turn)
(require 'harmless-perm)
(require 'harmless-md)

(declare-function harmless-dashboard "harmless-dashboard")
(declare-function harmless-menu "harmless-transient")
(declare-function harmless-new "harmless")

(defface harmless-user-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for user labels."
  :group 'harmless)

(defface harmless-assistant-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for assistant labels."
  :group 'harmless)

(defface harmless-tool-face
  '((t :inherit font-lock-comment-face))
  "Face for tool cards."
  :group 'harmless)

(defface harmless-error-face
  '((t :inherit error))
  "Face for Harmless errors."
  :group 'harmless)

(defvar-local harmless--session nil)
(defvar-local harmless--stream-marker nil)
(defvar-local harmless--stream-start nil)
(defvar-local harmless--perm-callback nil)
(defvar-local harmless--perm-class nil)

(defvar harmless-session-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "a") #'harmless-abort)
    (define-key map (kbd "g") #'harmless-dashboard)
    (define-key map (kbd "n") #'harmless-new)
    (define-key map (kbd "m") #'harmless-menu)
    (define-key map (kbd "i") #'harmless-ui-goto-prompt)
    (define-key map (kbd "RET") #'harmless-ui-goto-prompt)
    map)
  "Keymap for `harmless-session-mode'.")

(defvar harmless-prompt-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'harmless-prompt-send)
    (define-key map (kbd "C-c RET") #'harmless-prompt-send)
    (define-key map (kbd "C-c C-a") #'harmless-abort)
    (define-key map (kbd "C-c C-k") #'harmless-menu)
    (define-key map (kbd "y") #'harmless-perm-allow)
    (define-key map (kbd "n") #'harmless-perm-deny)
    (define-key map (kbd "!") #'harmless-perm-always)
    map)
  "Keymap for `harmless-prompt-mode'.")

(define-derived-mode harmless-session-mode special-mode "Harmless"
  "Major mode for a Harmless session transcript."
  :interactive nil
  (setq buffer-read-only t
        truncate-lines nil)
  (add-to-invisibility-spec 'markdown-markup)
  (setq-local header-line-format '(:eval (harmless-ui--header-line))))

(define-derived-mode harmless-prompt-mode text-mode "Harmless-Prompt"
  "Major mode for composing a Harmless prompt."
  :interactive nil
  (setq-local header-line-format " C-c C-c send   C-c C-a abort   y/n/! permission"))

(defun harmless-ui--header-line ()
  "Header line for the session buffer."
  (when harmless--session
    (let ((s harmless--session))
      (format " %s  %s/%s  %s"
              (or (harmless-session-title s)
                  (harmless-session-project-name s))
              (or (harmless-session-provider-name s) "?")
              (harmless-session-model-label s)
              (harmless-session-status s)))))

(defun harmless-ui--session-buffer-name (session)
  "Buffer name for SESSION's transcript."
  (let* ((proj (harmless-session-project-name session))
         (peers (harmless-session-for-cwd (harmless-session-cwd session))))
    (if (> (length peers) 1)
        (format "*harmless: %s %s*"
                proj
                (substring (harmless-session-id session) 0 8))
      (format "*harmless: %s*" proj))))

(defun harmless-ui--prompt-buffer-name (session)
  "Buffer name for SESSION's prompt."
  (concat (harmless-ui--session-buffer-name session) " prompt"))

(defun harmless-ui-ensure-session-buffer (session)
  "Return SESSION's transcript buffer, creating it if needed."
  (or (and (buffer-live-p (harmless-session-buffer session))
           (harmless-session-buffer session))
      (let ((buf (get-buffer-create (harmless-ui--session-buffer-name session))))
        (setf (harmless-session-buffer session) buf)
        (with-current-buffer buf
          (harmless-session-mode)
          (setq harmless--session session
                default-directory (harmless-session-cwd session)))
        buf)))

(defun harmless-ui-ensure-prompt-buffer (session)
  "Return SESSION's prompt buffer, creating it if needed."
  (or (and (buffer-live-p (harmless-session-prompt-buffer session))
           (harmless-session-prompt-buffer session))
      (let ((buf (get-buffer-create (harmless-ui--prompt-buffer-name session))))
        (with-current-buffer buf
          (harmless-prompt-mode)
          (setq harmless--session session
                default-directory (harmless-session-cwd session)))
        (setf (harmless-session-prompt-buffer session) buf)
        buf)))

(defun harmless-ui-show (session)
  "Display SESSION's transcript and prompt."
  (let ((sbuf (harmless-ui-ensure-session-buffer session))
        (pbuf (harmless-ui-ensure-prompt-buffer session)))
    (pop-to-buffer sbuf)
    (display-buffer pbuf '((display-buffer-at-bottom)
                           (window-height . 8)))
    (when-let* ((win (get-buffer-window pbuf)))
      (select-window win))))

(defun harmless-ui-goto-prompt ()
  "Select this session's prompt window."
  (interactive)
  (when-let* ((s harmless--session))
    (harmless-ui-show s)))

(defun harmless-ui-render-session (session)
  "Redraw SESSION's transcript from stored messages."
  (with-current-buffer (harmless-ui-ensure-session-buffer session)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq harmless--stream-marker nil
            harmless--stream-start nil)
      (dolist (msg (harmless-session-messages session))
        (harmless-ui--insert-message msg))
      (goto-char (point-max)))))

(defun harmless-ui--insert-label (label face)
  "Insert LABEL with FACE."
  (insert (propertize label 'face face) "\n"))

(defun harmless-ui--insert-message (msg)
  "Insert canonical MSG at point."
  (pcase (plist-get msg :role)
    ((or :user 'user "user")
     (harmless-ui--insert-label "You" 'harmless-user-face)
     (insert (or (plist-get msg :content) "") "\n\n"))
    ((or :assistant 'assistant "assistant")
     (harmless-ui--insert-label "Assistant" 'harmless-assistant-face)
     (when-let* ((r (plist-get msg :reasoning)))
       (unless (string-empty-p r)
         (insert (propertize (concat "Reasoning: " (harmless-truncate r 200) "\n")
                             'face 'harmless-tool-face))))
     (harmless-md-insert (or (plist-get msg :content) ""))
     (dolist (tc (plist-get msg :tool-calls))
       (insert (propertize
                (format "\n[tool %s %s]\n"
                        (or (plist-get tc :name) "?")
                        (harmless-truncate (format "%s" (plist-get tc :args)) 80))
                'face 'harmless-tool-face)))
     (insert "\n\n"))
    ((or :tool 'tool "tool")
     (insert (propertize
              (format "[result %s]\n%s\n\n"
                      (or (plist-get msg :name) "?")
                      (harmless-truncate (or (plist-get msg :content) "") 400))
              'face 'harmless-tool-face)))))

(defun harmless-ui--with-session-buffer (session fn)
  "Call FN in SESSION's transcript with writes allowed."
  (when-let* ((buf (harmless-session-buffer session)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (at-end (eobp)))
          (save-excursion
            (funcall fn))
          (when at-end
            (goto-char (point-max))))))))

(defun harmless-ui--ensure-stream (session)
  "Make sure SESSION has an assistant stream insertion point."
  (with-current-buffer (harmless-ui-ensure-session-buffer session)
    (unless (and harmless--stream-marker
                 (marker-position harmless--stream-marker))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (harmless-ui--insert-label "Assistant" 'harmless-assistant-face)
        (setq harmless--stream-start (point-marker)
              harmless--stream-marker (point-marker))
        (set-marker-insertion-type harmless--stream-marker t)))))

(defun harmless-ui--finalize-assistant (session msg)
  "Replace the streamed assistant body with displayed Markdown from MSG."
  (harmless-ui--with-session-buffer
   session
   (lambda ()
     (let ((start (and harmless--stream-start
                       (marker-position harmless--stream-start)))
           (end (and harmless--stream-marker
                     (marker-position harmless--stream-marker))))
       (if (and start end)
           (progn
             (delete-region start end)
             (goto-char start))
         (goto-char (point-max))
         (harmless-ui--insert-label "Assistant" 'harmless-assistant-face))
       (harmless-md-insert (or (plist-get msg :content) ""))
       (dolist (tc (plist-get msg :tool-calls))
         (insert (propertize
                  (format "\n[tool %s %s]\n"
                          (or (plist-get tc :name) "?")
                          (harmless-truncate (format "%s" (plist-get tc :args)) 80))
                  'face 'harmless-tool-face)))
       (insert "\n\n")
       (setq harmless--stream-start nil
             harmless--stream-marker nil)))))

(defun harmless-ui--stream-text (session text)
  "Append TEXT to SESSION's live assistant stream."
  (harmless-ui--ensure-stream session)
  (harmless-ui--with-session-buffer
   session
   (lambda ()
     (goto-char harmless--stream-marker)
     (insert text)
     (set-marker harmless--stream-marker (point)))))

(defun harmless-ui--on-event (session event)
  "Update SESSION buffers for EVENT."
  (pcase event
    (`(:message ,msg)
     (pcase (plist-get msg :role)
       ((or :user 'user "user")
        (with-current-buffer (harmless-ui-ensure-session-buffer session)
          (setq harmless--stream-marker nil
                harmless--stream-start nil))
        (harmless-ui--with-session-buffer
         session
         (lambda ()
           (goto-char (point-max))
           (harmless-ui--insert-message msg))))
       ((or :assistant 'assistant "assistant")
        (harmless-ui--finalize-assistant session msg))
       ((or :tool 'tool "tool")
        (harmless-ui--with-session-buffer
         session
         (lambda ()
           (goto-char (point-max))
           (harmless-ui--insert-message msg))))
       (_ nil)))
    (`(:text ,s)
     (harmless-ui--stream-text session s))
    (`(:reasoning ,s)
     (harmless-ui--stream-text session (propertize s 'face 'harmless-tool-face)))
    (`(:tool-call ,id ,name ,_delta)
     (ignore id)
     (harmless-ui--stream-text
      session
      (propertize (format "\n[tool %s]\n" (or name "?"))
                  'face 'harmless-tool-face)))
    (`(:error ,err)
     (harmless-ui--with-session-buffer
      session
      (lambda ()
        (goto-char (point-max))
        (insert (propertize (format "Error: %s\n\n" err)
                            'face 'harmless-error-face))))
     (with-current-buffer (harmless-ui-ensure-session-buffer session)
       (setq harmless--stream-marker nil
             harmless--stream-start nil)))
    (`(:stop ,_reason)
     ;; Markers stay until the assistant :message event rewrites the
     ;; streamed body as displayed Markdown.
     nil)
    (`(:status ,_st) nil)
    (_ nil))
  (force-mode-line-update t))

(defun harmless-ui-perm-ask (session class item callback)
  "Ask about CLASS ITEM for SESSION in the prompt window."
  (let ((pbuf (harmless-ui-ensure-prompt-buffer session)))
    (harmless-ui-show session)
    (with-current-buffer pbuf
      (setq harmless--perm-callback callback
            harmless--perm-class class)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Allow %s: %s\n\ny = allow   n = deny   ! = always this class\n"
                        class item))
        (setq buffer-read-only t)))
    (when (and (eq class 'edit) (stringp item))
      (let ((path (expand-file-name item (harmless-session-cwd session))))
        (when (file-exists-p path)
          (ignore path))))))

(defun harmless-ui--perm-decide (decision)
  "Resolve a pending permission prompt with DECISION."
  (interactive)
  (if (not harmless--perm-callback)
      (when (called-interactively-p 'interactive)
        (user-error "No pending permission prompt"))
    (let ((cb harmless--perm-callback))
      (setq harmless--perm-callback nil
            buffer-read-only nil)
      (erase-buffer)
      (funcall cb decision))))

(defun harmless-perm-allow ()
  "Allow the pending tool call."
  (interactive)
  (if harmless--perm-callback
      (harmless-ui--perm-decide 'allow)
    (self-insert-command 1)))

(defun harmless-perm-deny ()
  "Deny the pending tool call."
  (interactive)
  (if harmless--perm-callback
      (harmless-ui--perm-decide 'deny)
    (self-insert-command 1)))

(defun harmless-perm-always ()
  "Allow this class for the rest of the session."
  (interactive)
  (if harmless--perm-callback
      (harmless-ui--perm-decide 'always)
    (self-insert-command 1)))

(defun harmless-prompt-send ()
  "Send the prompt buffer contents as the next user turn."
  (interactive)
  (when harmless--perm-callback
    (user-error "Finish the permission prompt first (y/n/!)"))
  (let* ((session harmless--session)
         (text (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (when (string-empty-p text)
      (user-error "Prompt is empty"))
    (erase-buffer)
    (harmless-turn-run session text)))

;;;###autoload
(defun harmless-abort ()
  "Abort the current session's in-flight turn."
  (interactive)
  (let ((session (or harmless--session
                     (car (harmless-session-for-cwd (harmless-current-cwd))))))
    (unless session
      (user-error "No Harmless session"))
    (harmless-turn-abort session)))

(defun harmless-ui-open-session (session)
  "Render and display SESSION."
  (harmless-ui-ensure-session-buffer session)
  (harmless-ui-ensure-prompt-buffer session)
  (harmless-ui-render-session session)
  (harmless-ui-show session)
  session)

(add-hook 'harmless-event-functions #'harmless-ui--on-event)
(setq harmless-perm-ask-function #'harmless-ui-perm-ask)

(provide 'harmless-ui)

;;; harmless-ui.el ends here
