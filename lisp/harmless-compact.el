;;; harmless-compact.el --- Compact a Harmless transcript -*- lexical-binding: t; -*-

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
;; `harmless-compact' asks the session model to summarize older turns,
;; replaces those turns with one summary, and leaves the recent turns
;; in place.  A tool call stays with its results.  An empty summary
;; leaves the transcript unchanged.  Before a new prompt,
;; `harmless-compact-threshold' may run that same summary.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-provider)
(require 'harmless-usage)
(require 'harmless-ui)

(declare-function harmless--context-session "harmless")

(defcustom harmless-compact-keep-turns 2
  "Number of recent user turns `harmless-compact' leaves verbatim.
A turn starts at a user message and includes the assistant and tool
messages that follow it."
  :type 'integer
  :group 'harmless)

(defcustom harmless-compact-threshold 80
  "Percent of the model context window that triggers automatic compact.
Before a new user turn, if the last prompt used at least this percent
of the model's window, Harmless summarizes older turns.  Nil disables
automatic compact.  The same percent applies to every model.  The window
comes from `harmless-usage-context-window', so a larger window
compacts at the same fullness.  A model with no known window is left
alone."
  :type '(choice (const :tag "Off" nil)
                 (integer :tag "Percent of the context window"))
  :group 'harmless)

(defconst harmless-compact-instructions
  "Summarize the conversation below for a later turn of the same coding session. Include the goal, decisions, files changed, commands run, and errors still open. Omit greetings and unchanged file dumps. Write only the summary."
  "System text for a compact request.")

(defconst harmless-compact-summary-prefix
  "Summary of the earlier conversation:\n\n"
  "Text placed before the model's summary in the transcript.")

(defvar harmless-compact--runs (make-hash-table :test 'eq)
  "Active compact runs, keyed by the session object.")

(cl-defstruct (harmless-compact-run
               (:constructor harmless-compact-run-create)
               (:copier nil))
  (text "")
  prompt-tokens
  completion-tokens
  (noted nil)
  (finished nil)
  result
  (then nil)
  (gate nil))

(defun harmless-compact--user-p (msg)
  "Return non-nil if MSG starts a user turn."
  (or (memq (plist-get msg :role) '(:user user))
      (equal (plist-get msg :role) "user")))

(defun harmless-compact--count-users (messages)
  "Return how many user turns MESSAGES contains."
  (cl-count-if #'harmless-compact--user-p messages))

(defun harmless-compact--keep (keep)
  "Return KEEP, or the default when KEEP is nil.
Signal when the result is not a positive integer."
  (let ((keep (if (null keep) harmless-compact-keep-turns keep)))
    (unless (and (integerp keep) (> keep 0))
      (error "Keep at least one turn"))
    keep))

(defun harmless-compact-split (messages keep)
  "Return (OLDER . KEPT) for MESSAGES, retaining KEEP user turns.
A turn starts at a user message.  Messages before the first kept
user message, including an earlier summary, are the older part.
Signal when there is no user turn to drop."
  (setq keep (harmless-compact--keep keep))
  (unless (listp messages)
    (error "Nothing to compact"))
  (let ((starts nil)
        (i 0))
    (dolist (msg messages)
      (when (harmless-compact--user-p msg)
        (push i starts))
      (setq i (1+ i)))
    (setq starts (nreverse starts))
    (let ((n (length starts)))
      (unless (> n keep)
        (error "Nothing to compact"))
      (let ((cut (nth (- n keep) starts)))
        (unless (and (integerp cut) (> cut 0))
          (error "Nothing to compact"))
        (cons (cl-subseq messages 0 cut)
              (cl-subseq messages cut))))))

(defun harmless-compact--arg-text (args)
  "Return tool-call ARGS as text."
  (cond
   ((null args) "")
   ((stringp args) args)
   (t (harmless-json-text args))))

(defun harmless-compact--render-message (msg)
  "Return one MSG as plain text for the summary request."
  (let* ((role (plist-get msg :role))
         (content (or (plist-get msg :content) ""))
         (reasoning (plist-get msg :reasoning))
         (label (cond
                 ((harmless-compact--user-p msg) "User")
                 ((or (memq role '(:assistant assistant))
                      (equal role "assistant"))
                  "Assistant")
                 ((or (memq role '(:tool tool))
                      (equal role "tool"))
                  (format "Tool %s" (or (plist-get msg :name) "tool")))
                 ((or (memq role '(:summary summary))
                      (equal role "summary"))
                  "Summary")
                 (t (format "%s" (or role "message")))))
         (calls (plist-get msg :tool-calls)))
    (concat label ":\n"
            (if (and (stringp reasoning) (not (string-empty-p reasoning)))
                (concat "Reasoning:\n" reasoning "\n")
              "")
            content
            (when calls
              (concat "\n"
                      (mapconcat
                       (lambda (tc)
                         (format "Tool call %s: %s"
                                 (or (plist-get tc :name) "tool")
                                 (harmless-compact--arg-text
                                  (plist-get tc :args))))
                       calls "\n"))))))

(defun harmless-compact--render (messages)
  "Return MESSAGES as the user text of a compact request."
  (mapconcat #'harmless-compact--render-message messages "\n\n"))

(defun harmless-compact--request (older)
  "Return provider messages that ask for a summary of OLDER."
  (list (list :role :system :content harmless-compact-instructions)
        (list :role :user :content (harmless-compact--render older))))

(defun harmless-compact-cancel (session)
  "Drop SESSION's compact so a late reply cannot change the transcript.
A continuation waiting on that compact is dropped with it."
  (when session
    (let ((run (gethash session harmless-compact--runs)))
      (when run
        (setf (harmless-compact-run-then run) nil
              (harmless-compact-run-finished run) t))
      (remhash session harmless-compact--runs))))

(defun harmless-compact--continue (run)
  "Call RUN's continuation once, after the attempt is ready and finished."
  (when (and run
             (harmless-compact-run-gate run)
             (harmless-compact-run-finished run))
    (let ((then (harmless-compact-run-then run)))
      (when (functionp then)
        (setf (harmless-compact-run-then run) nil)
        (funcall then)))))

(defun harmless-compact--validate-threshold ()
  "Return `harmless-compact-threshold', or signal when it is unusable.
Nil disables automatic compact.  Any other value must be an integer
from 1 to 100."
  (let ((value harmless-compact-threshold))
    (unless (or (null value)
                (and (integerp value) (>= value 1) (<= value 100)))
      (error "Compact threshold must be an integer from 1 to 100, or nil, not %S"
             value))
    value))

(defun harmless-compact-needed-p (session)
  "Return non-nil if SESSION is full enough to compact before a turn.
The new user message is already stored, so it counts as a kept turn."
  (let ((percent (harmless-compact--validate-threshold))
        (window (and session
                     (harmless-usage-context-window
                      (harmless-session-model session))))
        (used (and session
                   (or (harmless-session-last-prompt-tokens session) 0)))
        (keep harmless-compact-keep-turns)
        (messages (and session (harmless-session-messages session))))
    (and percent
         (numberp window)
         (> window 0)
         (numberp used)
         (>= (* used 100) (* window percent))
         (integerp keep)
         (> keep 0)
         (listp messages)
         (> (harmless-compact--count-users messages) keep))))

(defun harmless-compact--live-p (session run)
  "Return non-nil if RUN is still the compact running on SESSION."
  (and (eq run (gethash session harmless-compact--runs))
       (not (harmless-compact-run-finished run))))

(defun harmless-compact--note-usage (session run)
  "Record RUN's token counts on SESSION once."
  (unless (harmless-compact-run-noted run)
    (when (or (harmless-compact-run-prompt-tokens run)
              (harmless-compact-run-completion-tokens run))
      (setf (harmless-compact-run-noted run) t)
      (harmless-usage-note-turn
       session
       (or (harmless-compact-run-prompt-tokens run) 0)
       (or (harmless-compact-run-completion-tokens run) 0)))))

(defun harmless-compact--close (session run)
  "Mark RUN finished and clear SESSION's process slot."
  (setf (harmless-compact-run-finished run) t)
  (remhash session harmless-compact--runs)
  (harmless-compact--note-usage session run)
  (setf (harmless-session-process session) nil))

(defun harmless-compact--reject (session text)
  "Leave SESSION's messages in place and report TEXT."
  (harmless-session-set-status session 'error)
  (harmless-emit session (list :error text)))

(defun harmless-compact--fail (session run err)
  "Finish RUN and report ERR without changing SESSION's messages."
  (harmless-compact--close session run)
  (harmless-compact--reject session (format "%s" (or err "compact failed")))
  (harmless-compact--continue run))

(defun harmless-compact--succeed (session run older kept)
  "Replace OLDER with the summary in RUN and retain KEPT."
  (harmless-compact--close session run)
  (let ((text (string-trim (or (harmless-compact-run-text run) ""))))
    (if (string-empty-p text)
        (harmless-compact--reject
         session "The model returned an empty summary.")
      (let ((previous (harmless-session-messages session))
            (summary (concat harmless-compact-summary-prefix text))
            (n (harmless-compact--count-users older)))
        (condition-case err
            (progn
              (setf (harmless-session-messages session)
                    (cons (list :role :summary :content summary) kept))
              (harmless-session-save session)
              (when (buffer-live-p (harmless-session-buffer session))
                (harmless-ui-render-session session))
              (harmless-session-set-status session 'idle)
              (setf (harmless-compact-run-result run) summary)
              (message "Compacted %d %s" n (if (= n 1) "turn" "turns")))
          (error
           (setf (harmless-session-messages session) previous)
           (harmless-compact--reject session (error-message-string err))))))
    (harmless-compact--continue run)))

(defun harmless-compact--on-event (session run older kept event)
  "Handle one compact EVENT for RUN on SESSION."
  (when (harmless-compact--live-p session run)
    (pcase event
      (`(:text ,s)
       (when (stringp s)
         (setf (harmless-compact-run-text run)
               (concat (harmless-compact-run-text run) s))))
      (`(:usage ,prompt ,completion)
       (setf (harmless-compact-run-prompt-tokens run) prompt
             (harmless-compact-run-completion-tokens run) completion))
      (`(:error ,err)
       (harmless-compact--fail session run err))
      (`(:stop ,_reason)
       (harmless-compact--succeed session run older kept)))))

(defun harmless-compact--start (session run older kept)
  "Ask SESSION's provider to summarize OLDER."
  (puthash session run harmless-compact--runs)
  (harmless-session-set-status session 'streaming)
  (condition-case err
      (let ((harmless-current-model (harmless-session-model session))
            (harmless-current-reasoning-effort
             (harmless-session-effective-reasoning-effort session)))
        (setf (harmless-session-process session)
              (harmless-provider-complete
               (harmless-session-provider session)
               (harmless-compact--request older)
               nil
               (lambda (event)
                 (harmless-compact--on-event session run older kept event)))))
    (error
     (harmless-compact-cancel session)
     (setf (harmless-session-process session) nil)
     (harmless-session-set-status session 'error)
     (signal (car err) (cdr err)))))

(defun harmless-compact-session (session &optional keep then)
  "Summarize SESSION's older turns and keep KEEP recent user turns.
KEEP nil uses `harmless-compact-keep-turns'.  Return the stored
summary when the provider answers before this call returns.
THEN, if a function, runs after the attempt finishes, including when
the summary is empty or the provider fails.  The call waits until
this function's provider request has returned, so a synchronous
reply cannot lose the process slot of a following turn."
  (unless session
    (error "No Harmless session"))
  (when (memq (harmless-session-status session) '(streaming waiting-permission))
    (error "A turn is in progress"))
  (unless (harmless-session-provider session)
    (error "No Harmless provider configured"))
  (let* ((parts (harmless-compact-split (harmless-session-messages session) keep))
         (run (harmless-compact-run-create :then then)))
    (condition-case err
        (harmless-compact--start session run (car parts) (cdr parts))
      (error
       ;; Cancel cleared the continuation so a late reply cannot start
       ;; the turn.  This signal is that failed attempt, so put the
       ;; continuation back and let the gate run it.
       (setf (harmless-compact-run-finished run) t
             (harmless-compact-run-then run) then)
       (if (functionp then)
           (harmless-emit session (list :error (error-message-string err)))
         (signal (car err) (cdr err)))))
    (setf (harmless-compact-run-gate run) t)
    (harmless-compact--continue run)
    (harmless-compact-run-result run)))

(defun harmless-compact-before-turn (session then)
  "Compact SESSION when its last prompt fills the window, then call THEN.
THEN is called immediately when SESSION does not need a summary."
  (if (not (harmless-compact-needed-p session))
      (funcall then)
    (harmless-compact-session session nil then)))

;;;###autoload
(defun harmless-compact (&optional keep)
  "Summarize older turns in the current session.
KEEP user turns stay verbatim.  Nil uses `harmless-compact-keep-turns'.
A numeric prefix argument sets KEEP."
  (interactive
   (list (when current-prefix-arg
           (prefix-numeric-value current-prefix-arg))))
  (let ((session (harmless--context-session)))
    (unless session
      (error "No Harmless session"))
    (harmless-compact-session session keep)))

(define-key harmless-session-mode-map (kbd "k") #'harmless-compact)

(provide 'harmless-compact)

;;; harmless-compact.el ends here
