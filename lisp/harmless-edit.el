;;; harmless-edit.el --- Edit an earlier Harmless prompt -*- lexical-binding: t; -*-

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
;; `harmless-edit-prompt' replaces a chosen user prompt, drops that
;; turn's reply and every later turn, and sends the new text.  The
;; dropped turns are not saved.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-ui)
(require 'harmless-turn)

(declare-function harmless--context-session "harmless")
(declare-function harmless-compact--validate-threshold "harmless-compact" ())

(defun harmless-edit--count-users (messages)
  "Return how many user turns MESSAGES contains."
  (cl-count-if #'harmless-message-user-p messages))

(defun harmless-edit--dropped (messages turn)
  "Return how many user turns editing TURN would drop from MESSAGES.
TURN is 1-based.  The chosen turn counts, because its reply is dropped."
  (- (harmless-edit--count-users messages) (1- turn)))

(defun harmless-edit-split (messages turn)
  "Return the prefix of MESSAGES before user turn TURN.
TURN is 1-based.  A turn starts at a user message and includes the
assistant and tool messages that follow it.  Messages before the
first user message stay.  Signal when TURN is not a user turn."
  (unless (and (integerp turn) (> turn 0))
    (error "Choose a turn"))
  (unless (listp messages)
    (error "Nothing to edit"))
  (let ((starts nil)
        (i 0))
    (dolist (msg messages)
      (when (harmless-message-user-p msg)
        (push i starts))
      (setq i (1+ i)))
    (setq starts (nreverse starts))
    (let ((cut (nth (1- turn) starts)))
      (unless (and (integerp cut) (>= cut 0) (< cut (length messages)))
        (error "Nothing to edit"))
      (cl-subseq messages 0 cut))))

(defun harmless-edit--content (messages turn)
  "Return the text of user turn TURN in MESSAGES, or nil."
  (let ((index 0)
        (content nil))
    (dolist (msg messages)
      (when (harmless-message-user-p msg)
        (setq index (1+ index))
        (when (= index turn)
          (setq content (or (plist-get msg :content) "")))))
    content))

(defun harmless-edit--prompt (session turn)
  "Return the confirmation question for editing TURN of SESSION."
  (let ((dropped (harmless-edit--dropped
                  (harmless-session-messages session) turn)))
    (format "Edit turn %d and drop %d %s? "
            turn dropped (if (= dropped 1) "turn" "turns"))))

(defun harmless-edit--turn-at-point ()
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

(defun harmless-edit--label (index msg)
  "Return the completion label for user turn INDEX and MSG."
  (format "%d. %s"
          index
          (harmless-truncate
           (replace-regexp-in-string
            "\n" " " (or (plist-get msg :content) ""))
           60)))

(defun harmless-edit--read (session)
  "Ask which user turn of SESSION to edit, and return its 1-based index."
  (unless (listp (harmless-session-messages session))
    (error "Nothing to edit"))
  (let ((choices nil)
        (index 0))
    (dolist (msg (harmless-session-messages session))
      (when (harmless-message-user-p msg)
        (setq index (1+ index))
        (push (cons (harmless-edit--label index msg) index) choices)))
    (setq choices (nreverse choices))
    (unless choices
      (error "Nothing to edit"))
    (let ((turn (cdr (assoc (completing-read "Edit turn: " choices nil t)
                            choices))))
      (unless (and (integerp turn) (> turn 0))
        (error "Nothing to edit"))
      turn)))

(defun harmless-edit--choose (session)
  "Return the user turn to edit in SESSION.
Point in SESSION's transcript selects the turn.  Anywhere else, ask."
  (if (and (eq major-mode 'harmless-session-mode)
           (eq harmless--session session))
      (harmless-edit--turn-at-point)
    (harmless-edit--read session)))

(defun harmless-edit--read-text (old)
  "Read a replacement for OLD, starting with that text."
  (string-trim
   (minibuffer-with-setup-hook
       (lambda ()
         (delete-region (minibuffer-prompt-end) (point-max))
         (insert (or old "")))
     (read-string "Edit prompt: "))))

(defun harmless-edit--prepare (session turn text)
  "Check that SESSION can replace user turn TURN with TEXT.
Return TEXT trimmed.  Signal before changing SESSION."
  (unless session
    (error "No Harmless session"))
  (when (memq (harmless-session-status session) '(streaming waiting-permission))
    (error "A turn is in progress"))
  (unless (harmless-session-provider session)
    (error "No Harmless provider configured"))
  (harmless-edit-split (harmless-session-messages session) turn)
  (unless (stringp text)
    (error "Prompt is empty"))
  (setq text (string-trim text))
  (when (string-empty-p text)
    (error "Prompt is empty"))
  (when (fboundp 'harmless-compact--validate-threshold)
    (harmless-compact--validate-threshold))
  text)

(defun harmless-edit-session (session turn text)
  "Replace user turn TURN of SESSION with TEXT and send that prompt.
The reply to that turn, and every later turn, is dropped and is not
saved.  Return how many user turns were dropped.  A failed save
restores the transcript, the last prompt size, and the previous
update time.  The new prompt is then sent."
  (let* ((text (harmless-edit--prepare session turn text))
         (previous (harmless-session-messages session))
         (kept (harmless-edit-split previous turn))
         (dropped (harmless-edit--dropped previous turn))
         (previous-prompt (harmless-session-last-prompt-tokens session))
         (previous-completion (harmless-session-last-completion-tokens session))
         (previous-updated (harmless-session-updated-at session))
         (failed nil))
    (condition-case err
        (progn
          (setf (harmless-session-messages session) kept)
          (setf (harmless-session-last-prompt-tokens session) 0)
          (setf (harmless-session-last-completion-tokens session) 0)
          (setf (harmless-session-updated-at session) (harmless-now-iso))
          (harmless-session-save session))
      (error
       (setf (harmless-session-messages session) previous)
       (setf (harmless-session-last-prompt-tokens session) previous-prompt)
       (setf (harmless-session-last-completion-tokens session) previous-completion)
       (harmless-session-set-status session 'error)
       (setf (harmless-session-updated-at session) previous-updated)
       (harmless-emit session (list :error (error-message-string err)))
       (setq failed t)))
    (if failed
        nil
      (harmless-ui-render-session session)
      (harmless-turn-run session text)
      (message "Edited turn %d" turn)
      dropped)))

;;;###autoload
(defun harmless-edit-prompt (&optional turn text)
  "Replace an earlier prompt and send the new text.
TURN nil uses the turn at point in the transcript, or asks which turn
to edit.  A numeric prefix argument sets TURN, counting from the start
of the transcript.  TEXT nil reads the replacement, starting from the
old prompt.  The command asks before it changes anything."
  (interactive
   (list (when current-prefix-arg
           (prefix-numeric-value current-prefix-arg))
         nil))
  (let ((session (harmless--context-session)))
    (unless session
      (error "No Harmless session"))
    (when (memq (harmless-session-status session) '(streaming waiting-permission))
      (error "A turn is in progress"))
    (unless (harmless-session-provider session)
      (error "No Harmless provider configured"))
    (let ((turn (or turn (harmless-edit--choose session))))
      (harmless-edit-split (harmless-session-messages session) turn)
      (when (fboundp 'harmless-compact--validate-threshold)
        (harmless-compact--validate-threshold))
      (let ((text (if (null text)
                      (harmless-edit--read-text
                       (or (harmless-edit--content
                            (harmless-session-messages session) turn)
                           ""))
                    text)))
        (unless (stringp text)
          (error "Prompt is empty"))
        (when (string-empty-p (string-trim text))
          (error "Prompt is empty"))
        (when (y-or-n-p (harmless-edit--prompt session turn))
          (harmless-edit-session session turn text))))))

(define-key harmless-session-mode-map (kbd "E") #'harmless-edit-prompt)

(provide 'harmless-edit)

;;; harmless-edit.el ends here
