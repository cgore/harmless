;;; harmless-ask-region.el --- Ask from a file buffer -*- lexical-binding: t; -*-

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
;; `harmless-ask-region' sends the active region, or the defun at
;; point, to the live session for this project.  `harmless-ask-mode'
;; is off until enabled.  Its prefix is `C-c . a'.  It does not bind
;; `C-c C-c'.  A prefix argument replaces that text with the reply
;; as one undo entry and does not save the buffer.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-editor)
(require 'harmless-session)
(require 'harmless-turn)

(declare-function harmless-current "harmless")

(defvar harmless-ask--pending (make-hash-table :test 'equal)
  "Pending replacements, keyed by session id.
Each value is (BUFFER START END).  START and END are markers.")

(defvar harmless-ask--keep nil
  "When non-nil, `harmless-turn-run' keeps a pending replacement.
The ask command binds this around its own turn.  Any other turn
drops a replacement that has not been applied.")

(defun harmless-ask--check-buffer (buf)
  "Signal unless BUF is a writable file buffer.
The transcript, the prompt, the minibuffer, and any buffer that
does not visit a file signal \"buffer is not a file\".  A
read-only file buffer signals \"buffer is read-only\"."
  (when (or (minibufferp buf)
            (with-current-buffer buf
              (derived-mode-p 'harmless-session-mode 'harmless-prompt-mode))
            (not (harmless-editor--file-p buf)))
    (error "buffer is not a file"))
  (when (buffer-local-value 'buffer-read-only buf)
    (error "buffer is read-only")))

(defun harmless-ask--bounds ()
  "Return (BEG . END) for the active region or the defun at point.
An empty active region, or a point with no defun, signals
\"region is empty\".  An empty active region does not fall
through to the defun."
  (if (region-active-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (when (>= beg end)
          (error "region is empty"))
        (cons beg end))
    (save-excursion
      (unless (beginning-of-defun)
        (error "region is empty"))
      (let ((beg (point)))
        (end-of-defun)
        (when (<= (point) beg)
          (error "region is empty"))
        (cons beg (point))))))

(defun harmless-ask--session ()
  "Return the live session for this project.
An existing session is reused.  None opens one the way
`harmless-current' does."
  (let ((live (car (harmless-session-for-cwd (harmless-current-cwd)))))
    (cond
     (live live)
     ((fboundp 'harmless-current) (funcall 'harmless-current))
     (t (error "No Harmless session")))))

(defun harmless-ask--release (job)
  "Drop the markers in JOB."
  (when job
    (when (markerp (nth 1 job))
      (set-marker (nth 1 job) nil))
    (when (markerp (nth 2 job))
      (set-marker (nth 2 job) nil))))

(defun harmless-ask--replace (buf start end text)
  "Replace START to END in BUF with TEXT as one undo entry.
Do not save BUF.  A dead buffer inserts nothing.  A read-only
buffer signals \"buffer is read-only\" and leaves the region."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when buffer-read-only
        (error "buffer is read-only"))
      (let ((beg (and (markerp start) (marker-position start)))
            (endpos (and (markerp end) (marker-position end)))
            (modified (buffer-modified-p)))
        (when (and beg endpos)
          (undo-boundary)
          (let ((handle (prepare-change-group buf))
                (settled nil))
            (activate-change-group handle)
            (unwind-protect
                (condition-case err
                    (progn
                      (delete-region beg endpos)
                      (goto-char beg)
                      (insert text)
                      (accept-change-group handle)
                      (setq settled t)
                      (undo-amalgamate-change-group handle))
                  (error
                   (ignore-errors (cancel-change-group handle))
                   (setq settled t)
                   (set-buffer-modified-p modified)
                   (if (or buffer-read-only
                           (eq (car err) 'text-read-only)
                           (eq (car err) 'buffer-read-only))
                       (error "buffer is read-only")
                     (signal (car err) (cdr err)))))
              (unless settled
                (ignore-errors (cancel-change-group handle))
                (set-buffer-modified-p modified)))))))))

(defun harmless-ask-finish (session text)
  "Apply SESSION's pending replacement with TEXT, or drop it.
Empty and whitespace-only TEXT insert nothing.  The inserted
text is TEXT, not a trimmed copy.  A dead buffer inserts
nothing.  A read-only buffer signals \"buffer is read-only\"
and leaves the region."
  (when (harmless-session-p session)
    (let ((job (gethash (harmless-session-id session) harmless-ask--pending)))
      (remhash (harmless-session-id session) harmless-ask--pending)
      (unwind-protect
          (when (and job (stringp text) (not (string-blank-p text)))
            (harmless-ask--replace (car job) (nth 1 job) (nth 2 job) text))
        (harmless-ask--release job)))))

;;;###autoload
(defun harmless-ask-region (&optional replace)
  "Send the active region, or the defun at point, to the project session.
REPLACE, the prefix argument, replaces that text with the reply as
one undo entry and does not save the buffer.  An empty reply leaves
the region.  An existing session is reused.  With no session, open
one the way `harmless-current' does.  An empty region and a point
with no defun signal \"region is empty\".  A read-only buffer
signals \"buffer is read-only\".  The transcript, the prompt, and
the minibuffer signal \"buffer is not a file\"."
  (interactive "P")
  (let ((buf (current-buffer))
        (session nil)
        (bounds nil)
        (beg nil)
        (end nil)
        (text nil))
    (harmless-ask--check-buffer buf)
    (setq bounds (harmless-ask--bounds))
    (setq beg (copy-marker (car bounds)))
    (setq end (copy-marker (cdr bounds) t))
    (setq text (buffer-substring-no-properties (car bounds) (cdr bounds)))
    (condition-case err
        (progn
          (setq session (harmless-ask--session))
          (if replace
              (puthash (harmless-session-id session)
                       (list buf beg end)
                       harmless-ask--pending)
            (remhash (harmless-session-id session) harmless-ask--pending)
            (set-marker beg nil)
            (set-marker end nil))
          (let ((harmless-ask--keep t)
                (harmless-editor-note nil))
            (harmless-turn-run session text)))
      (error
       (when (harmless-session-p session)
         (remhash (harmless-session-id session) harmless-ask--pending))
       (set-marker beg nil)
       (set-marker end nil)
       (signal (car err) (cdr err))))))

(defvar harmless-ask-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c . a") #'harmless-ask-region)
    map)
  "Keymap for `harmless-ask-mode'.
`C-c . a' runs `harmless-ask-region'.  `C-c C-c' is left alone.")

;;;###autoload
(define-minor-mode harmless-ask-mode
  "Ask the live Harmless session from this file buffer.
The mode starts off.  It binds `C-c . a' to `harmless-ask-region'
and does not bind `C-c C-c'."
  :lighter " Ask"
  :keymap harmless-ask-mode-map
  :group 'harmless)

(provide 'harmless-ask-region)

;;; harmless-ask-region.el ends here
