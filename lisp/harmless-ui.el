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
;; Tool runs are collapsed folds.  TAB toggles the entry at point,
;; Left closes it, Right opens it, and S-TAB toggles every entry.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-tools)
(require 'harmless-turn)
(require 'harmless-perm)
(require 'harmless-md)

(declare-function harmless-dashboard "harmless-dashboard")
(declare-function harmless-menu "harmless-transient")
(declare-function harmless-new "harmless")
(declare-function harmless-pick-model "harmless")

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
(defvar-local harmless--tool-block-start nil
  "Marker at the provisional tool block for the turn being streamed.")
(defvar-local harmless--tool-ids nil
  "Tool ids already given a provisional header during this stream.")

(defvar harmless-ui-fold-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'harmless-ui-toggle-fold)
    map)
  "Keymap for a fold header.  Mouse-1 toggles that fold.")

(defvar harmless-session-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "a") #'harmless-abort)
    (define-key map (kbd "g") #'harmless-dashboard)
    (define-key map (kbd "n") #'harmless-new)
    (define-key map (kbd "m") #'harmless-menu)
    (define-key map (kbd "M") #'harmless-pick-model)
    (define-key map (kbd "i") #'harmless-ui-goto-prompt)
    (define-key map (kbd "RET") #'harmless-ui-goto-prompt)
    (define-key map (kbd "TAB") #'harmless-ui-toggle-fold)
    (define-key map (kbd "<backtab>") #'harmless-ui-toggle-all-folds)
    (define-key map (kbd "<left>") #'harmless-ui-fold-close)
    (define-key map (kbd "<right>") #'harmless-ui-fold-open)
    map)
  "Keymap for `harmless-session-mode'.")

(defvar harmless-ui--model-header-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'harmless-pick-model)
    (define-key map [header-line mouse-1] #'harmless-pick-model)
    (define-key map [header-line mouse-2] #'harmless-pick-model)
    map)
  "Keymap for the clickable model label in the session header line.")

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
  (add-to-invisibility-spec 'harmless)
  (setq-local header-line-format '(:eval (harmless-ui--header-line))))

(define-derived-mode harmless-prompt-mode text-mode "Harmless-Prompt"
  "Major mode for composing a Harmless prompt."
  :interactive nil
  (setq-local header-line-format " C-c C-c send   C-c C-a abort   y/n/! permission"))

(defun harmless-ui--header-line ()
  "Header line for the session buffer."
  (when harmless--session
    (let* ((s harmless--session)
           (model (concat (or (harmless-session-provider-name s) "?")
                          "/"
                          (harmless-session-model-label s))))
      (concat
       " "
       (or (harmless-session-title s)
           (harmless-session-project-name s))
       "  "
       (propertize model
                   'face 'link
                   'mouse-face 'highlight
                   'follow-link t
                   'help-echo "mouse-1: change provider, model, and reasoning effort"
                   'keymap harmless-ui--model-header-map)
       "  "
       (format "%s" (harmless-session-status s))))))

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

(defconst harmless-ui--groupable-tools
  '("read_file" "list_dir" "glob" "grep")
  "Tool names that fold together when they run in a row.")

(defun harmless-ui--insert-label (label face)
  "Insert LABEL with FACE."
  (insert (propertize label 'face face) "\n"))

(defun harmless-ui--tool-role-p (role)
  "Return non-nil if ROLE is a tool-result message."
  (or (memq role '(:tool tool))
      (equal role "tool")))

(defun harmless-ui--tool-verb (name)
  "Short verb for tool NAME."
  (pcase name
    ("read_file" "read")
    ("list_dir" "list")
    ("glob" "glob")
    ("grep" "grep")
    ("write_file" "edit")
    ("replace" "edit")
    ("run_shell" "shell")
    (_ (or name "?"))))

(defun harmless-ui--tool-groupable-p (name)
  "Return non-nil if NAME joins an explored group."
  (member name harmless-ui--groupable-tools))

(defun harmless-ui--tool-summary (name raw)
  "Return the argument worth showing for tool NAME called with RAW."
  (let* ((args (harmless-tool-parse-args raw))
         (summary
          (pcase name
            ("grep"
             (let ((pat (or (harmless-tool-arg args :pattern) ""))
                   (glob (harmless-tool-arg args :glob)))
               (if (and glob (not (string-empty-p glob)))
                   (concat pat "  " glob)
                 pat)))
            ("glob" (or (harmless-tool-arg args :pattern) ""))
            ("run_shell" (or (harmless-tool-arg args :command) ""))
            (_ (or (harmless-tool-arg args :path)
                   (harmless-tool-arg args :pattern)
                   (harmless-tool-arg args :command)
                   "")))))
    (setq summary (string-trim (or summary "")))
    (if (string-empty-p summary)
        ""
      (harmless-truncate summary 72))))

(defun harmless-ui--tool-detail (content)
  "Return a short count for result CONTENT, or nil while it is running."
  (cond
   ((null content) nil)
   ((string-prefix-p "Error:" content) "error")
   ((string= content "No matches") "0")
   (t (number-to-string (length (split-string content "\n" t))))))

(defun harmless-ui--header-string (label detail)
  "Return LABEL, appending DETAIL in the shadow face when non-nil."
  (concat (propertize label 'face 'harmless-tool-face)
          (if detail
              (propertize (format "  (%s)" detail)
                          'face 'shadow
                          'harmless-fold-detail t)
            "")))

(defun harmless-ui--tool-header (name raw content)
  "Header text for a call of NAME with RAW arguments and result CONTENT."
  (let ((summary (harmless-ui--tool-summary name raw))
        (verb (harmless-ui--tool-verb name)))
    (harmless-ui--header-string
     (if (string-empty-p summary) verb (concat verb "  " summary))
     (harmless-ui--tool-detail content))))

(defun harmless-ui--arrow-string (id collapsed parent)
  "Return the disclosure triangle for fold ID."
  (propertize (if collapsed "▸" "▾")
              'harmless-fold id
              'harmless-fold-state (if collapsed 'collapsed 'open)
              'harmless-fold-arrow t
              'harmless-fold-parent parent
              'face 'harmless-tool-face
              'keymap harmless-ui-fold-map
              'mouse-face 'highlight
              'follow-link t
              'help-echo "mouse-1 or TAB: fold"
              'rear-nonsticky t))

(defun harmless-ui--insert-header-line (id header parent indent)
  "Insert a collapsed header for fold ID at point.
HEADER is the text after the triangle.  PARENT is the enclosing group
id, or nil.  INDENT is a string of spaces."
  (insert (propertize indent
                      'harmless-fold id
                      'harmless-fold-state 'collapsed
                      'harmless-fold-parent parent
                      'keymap harmless-ui-fold-map
                      'rear-nonsticky t))
  (insert (harmless-ui--arrow-string id t parent))
  (insert (propertize " "
                      'harmless-fold id
                      'harmless-fold-state 'collapsed
                      'harmless-fold-parent parent
                      'face 'harmless-tool-face
                      'keymap harmless-ui-fold-map
                      'mouse-face 'highlight
                      'help-echo "mouse-1 or TAB: fold"
                      'rear-nonsticky t))
  (let ((text-start (point)))
    (insert header)
    (add-text-properties
     text-start (point)
     `(harmless-fold ,id
       harmless-fold-state collapsed
       harmless-fold-parent ,parent
       keymap ,harmless-ui-fold-map
       mouse-face highlight
       follow-link t
       help-echo "mouse-1 or TAB: fold"
       rear-nonsticky t)))
  (insert "\n"))

(defun harmless-ui--format-body (indent content)
  "Return CONTENT indented under a header that begins with INDENT."
  (let* ((prefix (concat (or indent "") "    "))
         (text (cond
                ((null content) "(running)")
                ((string-empty-p content) "")
                (t content)))
         (body (if (string-empty-p text)
                   "\n"
                 (concat prefix
                         (replace-regexp-in-string
                          "\n" (concat "\n" prefix) text t t)
                         (if (string-suffix-p "\n" text) "" "\n")))))
    (when (and (not (string-empty-p text))
               (string-prefix-p "Error:" text))
      (put-text-property 0 (length body) 'face 'harmless-error-face body))
    body))

(defun harmless-ui--insert-tool-fold (call results indent parent)
  "Insert a collapsed fold for CALL at point.
RESULTS is an alist of id to result text.  INDENT prefixes the header.
PARENT is the enclosing group id, or nil."
  (let* ((id (or (plist-get call :id) (format "tool-%d" (point))))
         (content (and results (cdr (assoc id results))))
         (indent (or indent "")))
    (harmless-ui--insert-header-line
     id
     (harmless-ui--tool-header (plist-get call :name)
                               (plist-get call :args)
                               content)
     parent indent)
    (let ((body-beg (point))
          (text (harmless-ui--format-body indent content)))
      (insert text)
      (add-text-properties body-beg (point)
                           `(harmless-fold-body ,id
                             invisible harmless
                             rear-nonsticky t))
      (when parent
        (put-text-property body-beg (point) 'harmless-group-body parent)))))

(defun harmless-ui--partition-calls (calls)
  "Split CALLS into (group . CALLS) runs and (single . CALL) items."
  (let (runs current)
    (dolist (call calls)
      (if (harmless-ui--tool-groupable-p (plist-get call :name))
          (push call current)
        (when current
          (push (cons 'group (nreverse current)) runs)
          (setq current nil))
        (push (cons 'single call) runs)))
    (when current
      (push (cons 'group (nreverse current)) runs))
    (nreverse runs)))

(defun harmless-ui--insert-group (calls results)
  "Insert one collapsed explored fold for groupable CALLS."
  (let ((id (format "group-%s" (or (plist-get (car calls) :id) "calls"))))
    (harmless-ui--insert-header-line
     id (harmless-ui--header-string
         (format "explored  %d calls" (length calls)) nil)
     nil "")
    (let ((body-beg (point)))
      (dolist (call calls)
        (harmless-ui--insert-tool-fold call results "    " id))
      (when (> (point) body-beg)
        (add-text-properties body-beg (point)
                             `(harmless-group-body ,id
                               invisible harmless
                               rear-nonsticky t))))))

(defun harmless-ui--insert-tool-block (calls results)
  "Insert collapsed folds for CALLS at point.
RESULTS is an alist of call id to result text."
  (dolist (run (harmless-ui--partition-calls calls))
    (if (eq (car run) 'group)
        (if (> (length (cdr run)) 1)
            (harmless-ui--insert-group (cdr run) results)
          (harmless-ui--insert-tool-fold (cadr run) results "" nil))
      (harmless-ui--insert-tool-fold (cdr run) results "" nil))))

(defun harmless-ui--next-prop (pos prop)
  "Return the next change of PROP after POS, or `point-max'."
  (let ((next (next-single-property-change pos prop nil (point-max))))
    (if (and next (> next pos)) next (point-max))))

(defun harmless-ui--region-for (prop id)
  "Return (START . END) where PROP equals ID, or nil."
  (let ((pos (point-min))
        start end)
    (while (< pos (point-max))
      (let ((next (harmless-ui--next-prop pos prop)))
        (when (equal (get-text-property pos prop) id)
          (unless start (setq start pos))
          (setq end next))
        (setq pos next)))
    (and start end (cons start end))))

(defun harmless-ui--fold-header-pos (id)
  "Return the position of fold ID's triangle, or nil."
  (let ((pos (point-min))
        found)
    (while (and (not found) (< pos (point-max)))
      (when (and (get-text-property pos 'harmless-fold-arrow)
                 (equal (get-text-property pos 'harmless-fold) id))
        (setq found pos))
      (setq pos (harmless-ui--next-prop pos 'harmless-fold-arrow)))
    found))

(defun harmless-ui--fold-extent (id)
  "Return the body region of fold ID."
  (or (harmless-ui--region-for 'harmless-group-body id)
      (harmless-ui--region-for 'harmless-fold-body id)))

(defun harmless-ui--fold-at (pos)
  "Return the fold id at POS, or nil."
  (or (get-text-property pos 'harmless-fold)
      (get-text-property pos 'harmless-fold-body)
      (and (> pos (point-min))
           (or (get-text-property (1- pos) 'harmless-fold)
               (get-text-property (1- pos) 'harmless-fold-body)))))

(defun harmless-ui--header-indent (pos)
  "Return the leading spaces on the header line at POS."
  (save-excursion
    (goto-char pos)
    (buffer-substring-no-properties (line-beginning-position) pos)))

(defun harmless-ui--rehide-nested (start end)
  "Hide bodies of collapsed folds inside START END."
  (let ((pos start))
    (while (< pos end)
      (when (and (get-text-property pos 'harmless-fold-arrow)
                 (eq (get-text-property pos 'harmless-fold-state) 'collapsed))
        (let* ((id (get-text-property pos 'harmless-fold))
               (body (harmless-ui--region-for 'harmless-fold-body id)))
          (when body
            (put-text-property (car body) (cdr body) 'invisible 'harmless))))
      (setq pos (harmless-ui--next-prop pos 'harmless-fold-arrow)))))

(defun harmless-ui--set-collapsed (id collapsed)
  "Set fold ID collapsed when COLLAPSED is non-nil."
  (let ((header (harmless-ui--fold-header-pos id)))
    (unless header
      (error "No fold %s" id))
    (let ((inhibit-read-only t)
          (parent (get-text-property header 'harmless-fold-parent))
          (invis (get-text-property header 'invisible)))
      (save-excursion
        (goto-char header)
        (delete-char 1)
        (insert (harmless-ui--arrow-string id collapsed parent))
        (when invis
          (put-text-property header (1+ header) 'invisible invis))
        (put-text-property header (line-end-position)
                           'harmless-fold-state
                           (if collapsed 'collapsed 'open)))
      (let ((extent (harmless-ui--fold-extent id)))
        (when extent
          (if collapsed
              (put-text-property (car extent) (cdr extent) 'invisible 'harmless)
            (remove-text-properties (car extent) (cdr extent) '(invisible nil))
            (harmless-ui--rehide-nested (car extent) (cdr extent))))))))

(defun harmless-ui--refresh-detail (id content)
  "Replace the count on fold ID's header from CONTENT."
  (let ((header (harmless-ui--fold-header-pos id))
        (detail (harmless-ui--tool-detail content))
        (inhibit-read-only t))
    (when header
      (save-excursion
        (goto-char header)
        (let* ((eol (line-end-position))
               (dstart (text-property-any header eol 'harmless-fold-detail t)))
          (when dstart
            (delete-region dstart
                           (harmless-ui--next-prop dstart 'harmless-fold-detail)))
          (when detail
            (goto-char (line-end-position))
            (insert (propertize (format "  (%s)" detail)
                                'face 'shadow
                                'harmless-fold-detail t
                                'harmless-fold id
                                'harmless-fold-state
                                (get-text-property header 'harmless-fold-state)
                                'harmless-fold-parent
                                (get-text-property header 'harmless-fold-parent)
                                'invisible
                                (get-text-property header 'invisible)
                                'harmless-group-body
                                (get-text-property header 'harmless-group-body)
                                'keymap harmless-ui-fold-map
                                'rear-nonsticky t))))))))

(defun harmless-ui--set-fold-body (id content)
  "Replace fold ID's body with CONTENT and refresh its count."
  (let ((header (harmless-ui--fold-header-pos id))
        (region (harmless-ui--region-for 'harmless-fold-body id)))
    (when (and header region)
      (let* ((inhibit-read-only t)
             (parent (get-text-property header 'harmless-fold-parent))
             (indent (harmless-ui--header-indent header))
             (text (harmless-ui--format-body indent content))
             (child-collapsed
              (eq (get-text-property header 'harmless-fold-state) 'collapsed))
             (parent-header (and parent (harmless-ui--fold-header-pos parent)))
             (parent-collapsed
              (and parent-header
                   (eq (get-text-property parent-header 'harmless-fold-state)
                       'collapsed))))
        (save-excursion
          (delete-region (car region) (cdr region))
          (goto-char (car region))
          (insert text)
          (add-text-properties
           (car region) (point)
           `(harmless-fold-body ,id
             rear-nonsticky t
             ,@(when parent `(harmless-group-body ,parent))))
          (when (or child-collapsed parent-collapsed)
            (put-text-property (car region) (point) 'invisible 'harmless)))
        (harmless-ui--refresh-detail id content)))))

(defun harmless-ui--all-fold-ids ()
  "Return fold ids in buffer order."
  (let ((pos (point-min))
        ids)
    (while (< pos (point-max))
      (when (get-text-property pos 'harmless-fold-arrow)
        (push (get-text-property pos 'harmless-fold) ids))
      (setq pos (harmless-ui--next-prop pos 'harmless-fold-arrow)))
    (nreverse ids)))

(defun harmless-ui-toggle-fold (&optional event)
  "Toggle the fold at point, or at mouse EVENT."
  (interactive (list (and (mouse-event-p last-command-event)
                          last-command-event)))
  (when event
    (mouse-set-point event))
  (let ((id (harmless-ui--fold-at (point))))
    (unless id
      (user-error "No fold here"))
    (harmless-ui--set-collapsed
     id
     (eq (get-text-property (harmless-ui--fold-header-pos id)
                            'harmless-fold-state)
         'open))))

(defun harmless-ui-fold-open ()
  "Expand the fold at point."
  (interactive)
  (let ((id (harmless-ui--fold-at (point))))
    (unless id
      (user-error "No fold here"))
    (harmless-ui--set-collapsed id nil)))

(defun harmless-ui-fold-close ()
  "Collapse the fold at point.
If that fold is already collapsed, collapse its parent group."
  (interactive)
  (let ((id (harmless-ui--fold-at (point))))
    (unless id
      (user-error "No fold here"))
    (let ((header (harmless-ui--fold-header-pos id)))
      (if (eq (get-text-property header 'harmless-fold-state) 'open)
          (harmless-ui--set-collapsed id t)
        (let ((parent (get-text-property header 'harmless-fold-parent)))
          (when parent
            (harmless-ui--set-collapsed parent t)))))))

(defun harmless-ui-toggle-all-folds ()
  "Expand every fold, or collapse them if all are open."
  (interactive)
  (let* ((ids (harmless-ui--all-fold-ids))
         (any-collapsed
          (cl-some (lambda (id)
                     (eq (get-text-property (harmless-ui--fold-header-pos id)
                                            'harmless-fold-state)
                         'collapsed))
                   ids)))
    (dolist (id ids)
      (harmless-ui--set-collapsed id (not any-collapsed)))))

(defun harmless-ui--insert-assistant-body (msg)
  "Insert MSG's reasoning and Markdown at point."
  (when-let* ((r (plist-get msg :reasoning)))
    (unless (string-empty-p r)
      (insert (propertize (concat "Reasoning: " (harmless-truncate r 200) "\n")
                          'face 'harmless-tool-face))))
  (let ((content (plist-get msg :content)))
    (when (and content (not (string-empty-p content)))
      (harmless-md-insert content)
      (unless (bolp)
        (insert "\n"))
      (insert "\n"))))

(defun harmless-ui--insert-message (msg)
  "Insert a user MSG at point."
  (harmless-ui--insert-label "You" 'harmless-user-face)
  (insert (or (plist-get msg :content) "") "\n\n"))

(defun harmless-ui-render-session (session)
  "Redraw SESSION's transcript from stored messages."
  (with-current-buffer (harmless-ui-ensure-session-buffer session)
    (let ((inhibit-read-only t)
          (msgs (harmless-session-messages session)))
      (dolist (marker (list harmless--stream-marker
                            harmless--stream-start
                            harmless--tool-block-start))
        (when (markerp marker)
          (set-marker marker nil)))
      (erase-buffer)
      (setq harmless--stream-marker nil
            harmless--stream-start nil
            harmless--tool-block-start nil
            harmless--tool-ids nil)
      (while msgs
        (let ((msg (car msgs)))
          (pcase (plist-get msg :role)
            ((or :user 'user "user")
             (harmless-ui--insert-message msg))
            ((or :assistant 'assistant "assistant")
             (harmless-ui--insert-label "Assistant" 'harmless-assistant-face)
             (harmless-ui--insert-assistant-body msg)
             (let ((calls (plist-get msg :tool-calls))
                   (results nil))
               (while (and (cdr msgs)
                           (harmless-ui--tool-role-p
                            (plist-get (cadr msgs) :role)))
                 (let ((tool-msg (cadr msgs)))
                   (push (cons (plist-get tool-msg :id)
                               (plist-get tool-msg :content))
                         results)
                   (setq msgs (cdr msgs))))
               (when calls
                 (harmless-ui--insert-tool-block calls results))))
            ((or :tool 'tool "tool")
             (harmless-ui--insert-tool-fold
              (list :id (plist-get msg :id)
                    :name (plist-get msg :name)
                    :args nil)
              (list (cons (plist-get msg :id)
                          (plist-get msg :content)))
              "" nil))))
        (setq msgs (cdr msgs)))
      (goto-char (point-max)))))

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

(defun harmless-ui--drop-marker (marker)
  "Detach MARKER and return nil."
  (when (markerp marker)
    (set-marker marker nil))
  nil)

(defun harmless-ui--rebuild-tool-block (calls)
  "Replace the provisional tool block with folds for CALLS."
  (let ((inhibit-read-only t))
    (when (and harmless--tool-block-start
               (marker-position harmless--tool-block-start))
      (delete-region harmless--tool-block-start (point-max)))
    (setq harmless--tool-block-start
          (harmless-ui--drop-marker harmless--tool-block-start)
          harmless--tool-ids nil)
    (when calls
      (goto-char (point-max))
      (harmless-ui--insert-tool-block calls nil))))

(defun harmless-ui--insert-outside-stream (fn)
  "Call FN at point-max without pulling the stream marker forward.
The stream marker has insertion type t, so an insert at its position
would swallow the following tool block into the streamed prose."
  (goto-char (point-max))
  (if (and harmless--stream-marker
           (marker-position harmless--stream-marker))
      (let ((type (marker-insertion-type harmless--stream-marker)))
        (set-marker-insertion-type harmless--stream-marker nil)
        (unwind-protect
            (funcall fn)
          (set-marker-insertion-type harmless--stream-marker type)))
    (funcall fn)))

(defun harmless-ui--note-streaming-tool (session id name)
  "Insert one collapsed header for ID named NAME, once per call."
  (when name
    (let ((key (or id name)))
      (harmless-ui-ensure-session-buffer session)
      (harmless-ui--ensure-stream session)
      (harmless-ui--with-session-buffer
       session
       (lambda ()
         (unless (member key harmless--tool-ids)
           (push key harmless--tool-ids)
           (harmless-ui--insert-outside-stream
            (lambda ()
              (unless (and harmless--tool-block-start
                           (marker-position harmless--tool-block-start))
                (setq harmless--tool-block-start (point-marker))
                (set-marker-insertion-type harmless--tool-block-start nil))
              (harmless-ui--insert-tool-fold
               (list :id key :name name :args nil) nil "" nil)
              (set-marker-insertion-type harmless--tool-block-start t)))))))))

(defun harmless-ui--attach-tool-result (msg)
  "Fill the fold for tool MSG, appending a fold if none exists yet."
  (let ((id (plist-get msg :id))
        (content (plist-get msg :content)))
    (if (and id (harmless-ui--fold-header-pos id))
        (harmless-ui--set-fold-body id content)
      (goto-char (point-max))
      (harmless-ui--insert-tool-fold
       (list :id id :name (plist-get msg :name) :args nil)
       (list (cons id content))
       "" nil))))

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
       (harmless-ui--insert-assistant-body msg)
       (setq harmless--stream-start
             (harmless-ui--drop-marker harmless--stream-start)
             harmless--stream-marker
             (harmless-ui--drop-marker harmless--stream-marker))
       (harmless-ui--rebuild-tool-block (plist-get msg :tool-calls))))))

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
        (harmless-ui-ensure-session-buffer session)
        (harmless-ui--with-session-buffer
         session
         (lambda ()
           (harmless-ui--attach-tool-result msg))))
       (_ nil)))
    (`(:text ,s)
     (harmless-ui--stream-text session s))
    (`(:reasoning ,s)
     (harmless-ui--stream-text session (propertize s 'face 'harmless-tool-face)))
    (`(:tool-call ,id ,name ,_delta)
     (harmless-ui--note-streaming-tool session id name))
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
