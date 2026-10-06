;;; harmless-rewind.el --- Rewind a Harmless transcript -*- lexical-binding: t; -*-

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
;; `harmless-rewind' drops every turn after a chosen user turn and
;; saves the shorter transcript.  That turn stays, including its
;; assistant reply and tool results.  The dropped turns are not saved.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-ui)

(declare-function harmless--context-session "harmless")

(defun harmless-rewind--count-users (messages)
  "Return how many user turns MESSAGES contains."
  (cl-count-if #'harmless-message-user-p messages))

(defun harmless-rewind--dropped (messages keep)
  "Return how many user turns KEEP would drop from MESSAGES."
  (- (harmless-rewind--count-users messages) keep))

(defun harmless-rewind-split (messages keep)
  "Return the prefix of MESSAGES through KEEP user turns.
A turn starts at a user message and includes the assistant and tool
messages that follow it.  Messages before the first user message stay.
Signal when KEEP would drop nothing."
  (unless (and (integerp keep) (> keep 0))
    (error "Keep at least one turn"))
  (unless (listp messages)
    (error "Nothing to rewind"))
  (let ((starts nil)
        (i 0))
    (dolist (msg messages)
      (when (harmless-message-user-p msg)
        (push i starts))
      (setq i (1+ i)))
    (setq starts (nreverse starts))
    (let ((cut (nth keep starts)))
      (unless (and (integerp cut) (> cut 0) (< cut (length messages)))
        (error "Nothing to rewind"))
      (cl-subseq messages 0 cut))))

(defun harmless-rewind--prompt (session keep)
  "Return the confirmation question for keeping KEEP turns of SESSION."
  (let ((dropped (harmless-rewind--dropped
                  (harmless-session-messages session) keep)))
    (format "Rewind to turn %d and drop %d %s? "
            keep dropped (if (= dropped 1) "turn" "turns"))))

(defun harmless-rewind--turn-at-point ()
  "Return the 1-based user turn at point in the transcript.
The end of the buffer belongs to the turn that ends there."
  (unless (and (eq major-mode 'harmless-session-mode)
               (local-variable-p 'harmless--session))
    (error "Point is not on a turn"))
  (let* ((pos (if (and (> (point) (point-min)) (eobp))
                  (1- (point))
                (point)))
         (turn (get-text-property pos 'harmless-turn)))
    (unless (and (integerp turn) (> turn 0))
      (error "Point is not on a turn"))
    turn))

(defun harmless-rewind--label (index msg)
  "Return the completion label for user turn INDEX and MSG."
  (format "%d. %s"
          index
          (harmless-truncate
           (replace-regexp-in-string
            "\n" " " (or (plist-get msg :content) ""))
           60)))

(defun harmless-rewind--read (session)
  "Ask which user turn of SESSION to keep, and return its 1-based index."
  (unless (listp (harmless-session-messages session))
    (error "Nothing to rewind"))
  (let ((choices nil)
        (index 0))
    (dolist (msg (harmless-session-messages session))
      (when (harmless-message-user-p msg)
        (setq index (1+ index))
        (push (cons (harmless-rewind--label index msg) index) choices)))
    (setq choices (nreverse choices))
    (unless choices
      (error "Nothing to rewind"))
    (let ((keep (cdr (assoc (completing-read "Rewind to turn: " choices nil t)
                            choices))))
      (unless (and (integerp keep) (> keep 0))
        (error "Nothing to rewind"))
      keep)))

(defun harmless-rewind--choose (session)
  "Return the user turn to keep for SESSION.
Point in SESSION's transcript selects the turn.  Anywhere else, ask."
  (if (and (eq major-mode 'harmless-session-mode)
           (eq harmless--session session))
      (harmless-rewind--turn-at-point)
    (harmless-rewind--read session)))

(defun harmless-rewind-session (session keep)
  "Drop SESSION's turns after the first KEEP user turns.
Return the number of dropped user turns.  A failed save restores the
transcript, the last prompt size, and the previous update time.  A
successful rewind ends idle."
  (unless session
    (error "No Harmless session"))
  (when (memq (harmless-session-status session) '(streaming waiting-permission))
    (error "A turn is in progress"))
  (let* ((previous (harmless-session-messages session))
         (kept (harmless-rewind-split previous keep))
         (dropped (harmless-rewind--dropped previous keep))
         (previous-prompt (harmless-session-last-prompt-tokens session))
         (previous-completion (harmless-session-last-completion-tokens session))
         (previous-updated (harmless-session-updated-at session)))
    (condition-case err
        (progn
          (setf (harmless-session-messages session) kept)
          (setf (harmless-session-last-prompt-tokens session) 0)
          (setf (harmless-session-last-completion-tokens session) 0)
          (setf (harmless-session-updated-at session) (harmless-now-iso))
          (harmless-session-save session)
          (when (buffer-live-p (harmless-session-buffer session))
            (harmless-ui-render-session session))
          (harmless-session-set-status session 'idle)
          (message "Dropped %d %s" dropped (if (= dropped 1) "turn" "turns"))
          dropped)
      (error
       (setf (harmless-session-messages session) previous)
       (setf (harmless-session-last-prompt-tokens session) previous-prompt)
       (setf (harmless-session-last-completion-tokens session) previous-completion)
       (harmless-session-set-status session 'error)
       (setf (harmless-session-updated-at session) previous-updated)
       (harmless-emit session (list :error (error-message-string err)))
       nil))))

;;;###autoload
(defun harmless-rewind (&optional keep)
  "Drop turns after a chosen user turn in the current session.
KEEP nil uses the turn at point in the transcript, or asks which turn
to keep.  A numeric prefix argument sets KEEP, counting from the start
of the transcript.  The command asks before it drops anything."
  (interactive
   (list (when current-prefix-arg
           (prefix-numeric-value current-prefix-arg))))
  (let ((session (harmless--context-session)))
    (unless session
      (error "No Harmless session"))
    (let ((keep (or keep (harmless-rewind--choose session))))
      (harmless-rewind-split (harmless-session-messages session) keep)
      (when (y-or-n-p (harmless-rewind--prompt session keep))
        (harmless-rewind-session session keep)))))

(define-key harmless-session-mode-map (kbd "w") #'harmless-rewind)

(provide 'harmless-rewind)

;;; harmless-rewind.el ends here
