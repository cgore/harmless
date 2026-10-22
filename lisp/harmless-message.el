;;; harmless-message.el --- Findings between live Harmless sessions -*- lexical-binding: t; -*-

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
;; `harmless-tool-send-message' delivers a finding to another session that is live in
;; this Emacs.  An idle session receives it in the transcript immediately.
;; A session that is streaming or waiting on permission keeps it in memory
;; until the transcript can take a line, then it is written there.  The
;; finding does not start a turn.  A session that exists only on disk is
;; not a recipient.  Nothing here is a task for a later seat.

;;; Code:

(require 'subr-x)
(require 'harmless-session)
(require 'harmless-tools)

(defun harmless-message--one-line (value)
  "Return VALUE as one trimmed line, or nil when it is blank."
  (when (stringp value)
    (setq value
          (string-trim
           (replace-regexp-in-string "[\n\r\t]+" " " value)))
    (unless (string-empty-p value)
      value)))

(defun harmless-message--id (args)
  "Return the trimmed session id in ARGS, or signal."
  (let ((id (harmless-tool-arg args :session)))
    (cond
     ((null id) (error "session id is empty"))
     ((not (stringp id)) (error "session id is not an id"))
     (t
      (setq id (string-trim id))
      (when (string-empty-p id)
        (error "session id is empty"))
      id))))

(defun harmless-message--body (args)
  "Return the trimmed finding in ARGS, or signal."
  (let ((text (harmless-tool-arg args :text)))
    (unless (stringp text)
      (error "message must be a string"))
    (setq text (string-trim text))
    (when (string-empty-p text)
      (error "message is empty"))
    text))

(defun harmless-message--content (from text)
  "Return the transcript text for TEXT sent by session FROM."
  (format "Finding from %s (%s):\n%s"
          (harmless-session-id from)
          (or (harmless-message--one-line (harmless-session-title from))
              "untitled")
          text))

(defun harmless-message--busy-p (session)
  "Return non-nil when SESSION cannot take a transcript line yet."
  (memq (harmless-session-status session) '(streaming waiting-permission)))

(defun harmless-message-flush (session)
  "Append SESSION's queued findings to its transcript.
A finding that is still queued is only in memory.  This writes it
into the transcript and clears the queue.  It does not start a turn."
  (when (harmless-session-p session)
    (let ((pending (harmless-session-findings session)))
      (when pending
        (setf (harmless-session-findings session) nil)
        (dolist (msg pending)
          (harmless-session-append session msg))))))

(defun harmless-message-send (session args)
  "Deliver ARGS' finding from SESSION to another live session.
Return \"Delivered to ID\" when the transcript was updated, or
\"Queued for ID\" when that session is in a turn.  Signal when the
id or the text is refused."
  (let* ((id (harmless-message--id args))
         (text (harmless-message--body args))
         (target (harmless-session-get id)))
    (when (equal id (harmless-session-id session))
      (error "a session cannot message itself"))
    (unless (harmless-session-p target)
      (error "no live session %s" id))
    (let ((msg (list :role :finding
                     :content (harmless-message--content session text)
                     :from (harmless-session-id session))))
      (if (harmless-message--busy-p target)
          (progn
            (setf (harmless-session-findings target)
                  (append (harmless-session-findings target)
                          (list msg)))
            (format "Queued for %s" id))
        (harmless-session-append target msg)
        (format "Delivered to %s" id)))))

(defun harmless-message--line (session)
  "Return one list line for live SESSION."
  (let ((title (harmless-message--one-line (harmless-session-title session))))
    (concat (harmless-session-id session)
            " "
            (symbol-name (or (harmless-session-status session) 'idle))
            (if title (concat " " title) ""))))

(defun harmless-message-list-text ()
  "Return the live-session list, one session per line.
A session that is only on disk is omitted."
  (let ((lines (mapcar #'harmless-message--line (harmless-session-list))))
    (if lines
        (concat "Live sessions:\n" (mapconcat #'identity lines "\n"))
      "Live sessions:")))

(defun harmless-message--list-tool (session args)
  "harmless-tool-list-sessions implementation."
  (ignore session args)
  (harmless-message-list-text))

(defun harmless-message-register ()
  "Register the live-session messaging tools."
  (harmless-register-tool
   (harmless-tool-create
    :name "harmless-tool-list-sessions"
    :description "List live sessions on this machine. Each line is an id, a status, and a title. A session that is only on disk is omitted."
    :class 'read
    :schema '(:type "object")
    :fn #'harmless-message--list-tool))
  (harmless-register-tool
   (harmless-tool-create
    :name "harmless-tool-send-message"
    :description "Send TEXT to a live session on this machine. SESSION is that session's id, from harmless-tool-list-sessions. The finding is added to its transcript and does not start a turn."
    :class 'edit
    :schema '(:type "object"
              :properties (:session (:type "string"
                                     :description "Id of a live session")
                           :text (:type "string"
                                  :description "The finding to deliver"))
              :required ["session" "text"])
    :fn #'harmless-message-send)))

(harmless-message-register)

(provide 'harmless-message)

;;; harmless-message.el ends here
