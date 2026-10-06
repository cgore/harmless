;;; harmless-fork.el --- Fork a Harmless session -*- lexical-binding: t; -*-

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
;; `harmless-fork' copies the transcript through a chosen user turn
;; into a new session.  That turn stays in the copy, including its
;; assistant reply and tool results.  The original session is left
;; unchanged.  Nothing is sent.

;;; Code:

(require 'cl-lib)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-ui)

(declare-function harmless--context-session "harmless")

(defun harmless-fork-split (messages turn)
  "Return the prefix of MESSAGES through user turn TURN.
TURN is 1-based.  A turn starts at a user message and includes the
assistant and tool messages that follow it.  Messages before the
first user message stay.  The chosen turn stays.  Signal when TURN
is not a user turn."
  (unless (and (integerp turn) (> turn 0))
    (error "Choose a turn"))
  (unless (listp messages)
    (error "Nothing to fork"))
  (let ((starts nil)
        (i 0))
    (dolist (msg messages)
      (when (harmless-message-user-p msg)
        (push i starts))
      (setq i (1+ i)))
    (setq starts (nreverse starts))
    (let* ((this (nth (1- turn) starts))
           (next (nth turn starts))
           (cut (if next next (length messages))))
      (unless (integerp this)
        (error "Nothing to fork"))
      (cl-subseq messages 0 cut))))

(defun harmless-fork--prompt (turn)
  "Return the confirmation question for forking through TURN."
  (format "Fork through turn %d? " turn))

(defun harmless-fork--turn-at-point ()
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

(defun harmless-fork--label (index msg)
  "Return the completion label for user turn INDEX and MSG."
  (format "%d. %s"
          index
          (harmless-truncate
           (replace-regexp-in-string
            "\n" " " (or (plist-get msg :content) ""))
           60)))

(defun harmless-fork--read (session)
  "Ask which user turn of SESSION to copy through, and return its index."
  (unless (listp (harmless-session-messages session))
    (error "Nothing to fork"))
  (let ((choices nil)
        (index 0))
    (dolist (msg (harmless-session-messages session))
      (when (harmless-message-user-p msg)
        (setq index (1+ index))
        (push (cons (harmless-fork--label index msg) index) choices)))
    (setq choices (nreverse choices))
    (unless choices
      (error "Nothing to fork"))
    (let ((turn (cdr (assoc (completing-read "Fork through turn: " choices nil t)
                            choices))))
      (unless (and (integerp turn) (> turn 0))
        (error "Nothing to fork"))
      turn)))

(defun harmless-fork--choose (session)
  "Return the user turn to copy through for SESSION.
Point in SESSION's transcript selects the turn.  Anywhere else, ask."
  (if (and (eq major-mode 'harmless-session-mode)
           (eq harmless--session session))
      (harmless-fork--turn-at-point)
    (harmless-fork--read session)))

(defun harmless-fork--create (session kept)
  "Return a new session holding a copy of KEPT from SESSION.
Register it and save the copy.  A failed save drops the registration
and signals.  SESSION is not modified."
  (let* ((cwd (harmless-session-cwd session))
         (title (harmless-session-title session))
         (child
          (apply
           #'harmless-session-new
           (append
            (list :provider (harmless-session-provider session)
                  :model (harmless-session-model session)
                  :reasoning-effort
                  (harmless-session-reasoning-effort session)
                  :parent-id (harmless-session-id session)
                  :source 'fork
                  :permission-mode
                  (harmless-session-permission-mode session))
            (and title (list :title title))
            (if (null cwd)
                (list :detached t)
              (list :cwd cwd))))))
    (condition-case err
        (progn
          (setf (harmless-session-messages child) (copy-tree kept))
          (setf (harmless-session-plan-mode child)
                (and (harmless-session-plan-mode session) t))
          (harmless-session-save child)
          child)
      (error
       (harmless-session-unregister child)
       (signal (car err) (cdr err))))))

(defun harmless-fork-session (session turn)
  "Copy SESSION through user turn TURN into a new session.
The chosen turn stays in the copy, including its assistant reply and
tool results.  SESSION is left unchanged.  Nothing is sent.  Return
the new session."
  (unless session
    (error "No Harmless session"))
  (when (memq (harmless-session-status session) '(streaming waiting-permission))
    (error "A turn is in progress"))
  (unless (harmless-session-provider session)
    (error "No Harmless provider configured"))
  (let* ((kept (harmless-fork-split (harmless-session-messages session) turn))
         (child (harmless-fork--create session kept)))
    (harmless-ui-open-session child)
    (message "Forked through turn %d" turn)
    child))

;;;###autoload
(defun harmless-fork (&optional turn)
  "Copy the current session through a chosen user turn.
TURN nil uses the turn at point in the transcript, or asks which turn
to copy through.  A numeric prefix argument sets TURN, counting from
the start of the transcript.  The command asks before it creates the
new session.  The original session is left unchanged."
  (interactive
   (list (when current-prefix-arg
           (prefix-numeric-value current-prefix-arg))))
  (let ((session (harmless--context-session)))
    (unless session
      (error "No Harmless session"))
    (when (memq (harmless-session-status session) '(streaming waiting-permission))
      (error "A turn is in progress"))
    (unless (harmless-session-provider session)
      (error "No Harmless provider configured"))
    (let ((turn (or turn (harmless-fork--choose session))))
      (harmless-fork-split (harmless-session-messages session) turn)
      (when (y-or-n-p (harmless-fork--prompt turn))
        (harmless-fork-session session turn)))))

(define-key harmless-session-mode-map (kbd "f") #'harmless-fork)

(provide 'harmless-fork)

;;; harmless-fork.el ends here
