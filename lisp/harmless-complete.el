;;; harmless-complete.el --- Complete at point with an overlay -*- lexical-binding: t; -*-

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
;; `harmless-complete' is off until enabled.  Typing a character, or
;; `harmless-complete-at-point', asks for a completion at point.  The
;; request is the text around point, not the session transcript, and
;; it does not call a tool.  The reply is an overlay.  TAB accepts
;; it.  Another key dismisses it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-provider)
(require 'harmless-session)
(require 'harmless-tools-fs)

(defvar harmless-complete nil
  "When non-nil, offer a completion overlay at point.
The minor mode of the same name sets this.  Nil shows no overlay.")

(defvar harmless-complete--overlay nil
  "The completion overlay, or nil.")

(defvar harmless-complete--exit nil
  "Exit function for the transient map, or nil.")

(defvar harmless-complete--token 0
  "Increments for each completion request.")

(defvar harmless-complete--acc ""
  "Reply text for the current request.")

(defvar harmless-complete--closing nil
  "Non-nil while the transient map is being removed.")

(defvar harmless-complete-map (make-sparse-keymap)
  "Keymap for `harmless-complete'.
This map is empty.  TAB accepts an overlay only while that
overlay is showing.")

(defvar harmless-complete-accept-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'harmless-complete-accept)
    (define-key map (kbd "C-i") #'harmless-complete-accept)
    map)
  "Keymap while a completion overlay is showing.
TAB accepts it.  Any other key dismisses it and runs as usual.")

(defun harmless-complete--hidden-name-p ()
  "Return non-nil when this buffer's name starts with a space."
  (let ((name (buffer-name)))
    (and (stringp name)
         (> (length name) 0)
         (eq (aref name 0) ?\s))))

(defun harmless-complete--eligible-p ()
  "Return non-nil when this buffer may show a completion overlay.
The mode is on, the buffer is writable, it is not the minibuffer,
and its name does not start with a space."
  (and harmless-complete
       (not buffer-read-only)
       (not (minibufferp))
       (not (harmless-complete--hidden-name-p))))

(defun harmless-complete--cap ()
  "Return the character cap for one completion request."
  (if (and (integerp harmless-read-file-max-bytes)
           (> harmless-read-file-max-bytes 0))
      harmless-read-file-max-bytes
    0))

(defun harmless-complete--excerpt ()
  "Return the accessible text around point, with a cursor marker.
The marker is \"<point>\".  Text on each side is cut so the buffer
text, not the marker, fits in `harmless-read-file-max-bytes'.
Point stays where it was."
  (save-excursion
    (let* ((cap (harmless-complete--cap))
           (before (buffer-substring-no-properties (point-min) (point)))
           (after (buffer-substring-no-properties (point) (point-max))))
      (when (> (+ (length before) (length after)) cap)
        (let* ((keep-before (min (length before) (/ cap 2)))
               (keep-after (min (length after) (- cap keep-before))))
          (setq keep-before (min (length before) (- cap keep-after)))
          (setq before (substring before (- (length before) keep-before)))
          (setq after (substring after 0 keep-after))))
      (concat before "<point>" after))))

(defun harmless-complete--provider ()
  "Return the provider to ask, or nil.
A live session for this project supplies its provider.  Otherwise
this is the default provider.  No session is started."
  (let ((session (ignore-errors
                   (car (harmless-session-for-cwd
                         (harmless-current-cwd))))))
    (or (and (harmless-session-p session)
             (harmless-session-provider session))
        (harmless-default-provider))))

(defun harmless-complete--delete-overlay ()
  "Remove the completion overlay."
  (when (overlayp harmless-complete--overlay)
    (delete-overlay harmless-complete--overlay))
  (setq harmless-complete--overlay nil))

(defun harmless-complete--on-exit ()
  "Drop the transient map.  Another key also drops the overlay."
  (setq harmless-complete--exit nil)
  (unless (or harmless-complete--closing
              (eq this-command 'harmless-complete-accept))
    (harmless-complete--delete-overlay)))

(defun harmless-complete--disarm ()
  "Deactivate the transient map, if one is armed."
  (when (functionp harmless-complete--exit)
    (let ((exit harmless-complete--exit)
          (harmless-complete--closing t))
      (setq harmless-complete--exit nil)
      (funcall exit))))

(defun harmless-complete--cancel ()
  "Remove the overlay and the transient map."
  (harmless-complete--delete-overlay)
  (harmless-complete--disarm))

(defun harmless-complete--show (text)
  "Show TEXT as an overlay at point.  Empty TEXT removes it."
  (if (or (null text) (string-empty-p text))
      (harmless-complete--cancel)
    (let ((pos (point)))
      (harmless-complete--delete-overlay)
      (setq harmless-complete--overlay (make-overlay pos pos))
      (overlay-put harmless-complete--overlay 'harmless-complete-text text)
      (overlay-put harmless-complete--overlay
                   'after-string (propertize text 'face 'shadow))
      (unless (functionp harmless-complete--exit)
        (setq harmless-complete--exit
              (set-transient-map harmless-complete-accept-map nil
                                 #'harmless-complete--on-exit))))))

(defun harmless-complete--event (token buf event)
  "Handle EVENT for TOKEN when BUF is still the requesting buffer."
  (when (and (eq token harmless-complete--token)
             harmless-complete
             (buffer-live-p buf))
    (with-current-buffer buf
      (pcase event
        (`(:text ,s)
         (when (stringp s)
           (setq harmless-complete--acc
                 (concat harmless-complete--acc s))
           (harmless-complete--show harmless-complete--acc)))
        (`(:error ,_)
         (setq harmless-complete--acc "")
         (harmless-complete--cancel))
        (`(:stop ,_)
         (if (or (not (stringp harmless-complete--acc))
                 (string-empty-p harmless-complete--acc))
             (harmless-complete--cancel)
           (harmless-complete--show harmless-complete--acc)))))))

;;;###autoload
(defun harmless-complete-accept ()
  "Insert the completion overlay at point.
Nil and \"\" insert nothing.  A read-only buffer signals
\"buffer is read-only\" and inserts nothing.  The buffer is not
saved."
  (interactive)
  (let* ((ov harmless-complete--overlay)
         (text (and (overlayp ov)
                    (overlay-buffer ov)
                    (eq (overlay-buffer ov) (current-buffer))
                    (overlay-get ov 'harmless-complete-text))))
    (harmless-complete--delete-overlay)
    (unwind-protect
        (cond
         ((or (null text) (string-empty-p text)) nil)
         (buffer-read-only (error "buffer is read-only"))
         (t
          (undo-boundary)
          (insert text)))
      (harmless-complete--disarm))))

;;;###autoload
(defun harmless-complete-at-point ()
  "Request a completion overlay for the text around point.
The mode stays quiet until `harmless-complete' is enabled.  A
read-only buffer, the minibuffer, and a buffer whose name starts
with a space show no overlay.  The request does not send the
session transcript and does not call a tool.  Nil and \"\" from
the model insert nothing."
  (interactive)
  (when (harmless-complete--eligible-p)
    (let* ((token (setq harmless-complete--token
                        (1+ harmless-complete--token)))
           (buf (current-buffer))
           (excerpt (harmless-complete--excerpt))
           (provider (harmless-complete--provider)))
      (setq harmless-complete--acc "")
      (if (not provider)
          (harmless-complete--cancel)
        (harmless-provider-complete
         provider
         (list (list :role :user :content excerpt))
         nil
         (lambda (event)
           (harmless-complete--event token buf event)))))))

(defun harmless-complete--post-command ()
  "Request a completion after a character is typed."
  (when (and harmless-complete
             (eq this-command 'self-insert-command))
    (harmless-complete-at-point)))

;;;###autoload
(define-minor-mode harmless-complete
  "Show a completion overlay at point.
The mode is off until you enable it.  Typing a character, or
`harmless-complete-at-point', sends the text around point.  The
request does not include the session transcript and does not call
a tool.  TAB accepts the overlay.  Another key dismisses it.  A
read-only buffer, the minibuffer, and a buffer whose name starts
with a space show no overlay."
  :global t
  :lighter " Complete"
  :group 'harmless
  :keymap harmless-complete-map
  (if harmless-complete
      (add-hook 'post-command-hook #'harmless-complete--post-command)
    (remove-hook 'post-command-hook #'harmless-complete--post-command)
    (harmless-complete--cancel)))

(provide 'harmless-complete)

;;; harmless-complete.el ends here
