;;; harmless-schedule.el --- One-shot prompts for Harmless -*- lexical-binding: t; -*-

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
;; `schedule' stores one prompt and a time.  When that time arrives,
;; the prompt is sent, or queued if a turn is already running.
;; `schedule_cancel' removes it before then.  An unexpired schedule
;; is restored when the session is loaded.  An expired one is dropped
;; and is not sent.  This is not a repeating job, and it does not
;; watch a queue or a context wall.

;;; Code:

(require 'cl-lib)
(require 'iso8601)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-tools)
(require 'harmless-turn)

(defvar harmless-schedule--timers (make-hash-table :test 'eq)
  "Live timers keyed by session.
Each value is an alist of id to timer.  Timers are not saved.")

(defvar harmless-schedule-arm-function #'harmless-schedule-arm-default
  "Function (SESSION ID AT) that arms one schedule.
AT is an Emacs time.  Tests replace this so no timer starts.")

(defun harmless-schedule--present-p (value)
  "Return non-nil when VALUE was passed.
Nil and JSON null are absent.  \"\" and :false are present."
  (not (null value)))

(defun harmless-schedule--parse (text)
  "Return an Emacs time for ISO-8601 TEXT, or nil."
  (condition-case nil
      (encode-time (iso8601-parse text))
    (error nil)))

(defun harmless-schedule--iso (time)
  "Return TIME as a UTC ISO-8601 string."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" time t))

(defun harmless-schedule--prompt (raw)
  "Return RAW trimmed, or signal when it is not prompt text."
  (unless (stringp raw)
    (error "prompt must be a string"))
  (setq raw (string-trim raw))
  (when (string-empty-p raw)
    (error "prompt is empty"))
  raw)

(defun harmless-schedule--from-delay (delay)
  "Return (TIME . ISO) for a positive DELAY in seconds."
  (unless (and (numberp delay) (> delay 0))
    (error "delay must be a positive number"))
  (let ((time (time-add (current-time) (seconds-to-time delay))))
    (cons time (harmless-schedule--iso time))))

(defun harmless-schedule--from-at (raw)
  "Return (TIME . ISO) for an absolute time RAW."
  (unless (stringp raw)
    (error "at must be a string"))
  (setq raw (string-trim raw))
  (when (string-empty-p raw)
    (error "at is empty"))
  (let ((time (harmless-schedule--parse raw)))
    (unless time
      (error "at is not a time"))
    (unless (time-less-p (current-time) time)
      (error "schedule time has passed"))
    (cons time (harmless-schedule--iso time))))

(defun harmless-schedule--time (args)
  "Return (TIME . ISO) from ARGS, or signal.
Exactly one of delay or at is required.  That check happens before
either value is validated."
  (let* ((delay (harmless-tool-arg args :delay))
         (at (harmless-tool-arg args :at))
         (has-delay (harmless-schedule--present-p delay))
         (has-at (harmless-schedule--present-p at)))
    (cond
     ((and has-delay has-at)
      (error "schedule time must be one"))
     ((and (not has-delay) (not has-at))
      (error "schedule time is missing"))
     (has-delay
      (harmless-schedule--from-delay delay))
     (t
      (harmless-schedule--from-at at)))))

(defun harmless-schedule--id (raw)
  "Return RAW as a schedule id string, or signal."
  (cond
   ((and (integerp raw) (>= raw 0))
    (number-to-string raw))
   ((and (integerp raw) (< raw 0))
    (error "schedule id is not an id"))
   ((stringp raw)
    (setq raw (string-trim raw))
    (cond
     ((string-empty-p raw)
      (error "schedule id is empty"))
     ((string-match-p "\\`-[0-9]+\\'" raw)
      (error "schedule id is not an id"))
     (t raw)))
   ((null raw)
    (error "schedule id is empty"))
   (t
    (error "schedule id must be a string"))))

(defun harmless-schedule--next-id (session)
  "Return the next decimal id for SESSION.
A failed schedule does not consume an id.  Cancelled ids are not
reused."
  (let ((best 0))
    (dolist (item (harmless-session-schedules session))
      (let ((id (plist-get item :id)))
        (when (and (stringp id)
                   (string-match-p "\\`[0-9]+\\'" id))
          (setq best (max best (string-to-number id))))))
    (number-to-string (1+ best))))

(defun harmless-schedule--find (session id)
  "Return SESSION's schedule with ID, or nil."
  (cl-find id (harmless-session-schedules session)
           :test (lambda (wanted item)
                   (equal wanted (plist-get item :id)))))

(defun harmless-schedule--remember (session id timer)
  "Remember TIMER as the arm for ID on SESSION."
  (let* ((bucket (gethash session harmless-schedule--timers))
         (old (assoc id bucket)))
    (when (and old (timerp (cdr old)) (not (eq (cdr old) timer)))
      (cancel-timer (cdr old)))
    (puthash session
             (cons (cons id timer)
                   (assoc-delete-all id bucket))
             harmless-schedule--timers)))

(defun harmless-schedule--forget (session id)
  "Cancel and drop the timer for ID on SESSION, if one exists."
  (let* ((bucket (gethash session harmless-schedule--timers))
         (cell (assoc id bucket)))
    (when (and cell (timerp (cdr cell)))
      (cancel-timer (cdr cell)))
    (when cell
      (puthash session
               (assoc-delete-all id bucket)
               harmless-schedule--timers))))

(defun harmless-schedule-arm-default (session id at)
  "Arm ID on SESSION to fire once at AT."
  (harmless-schedule--remember
   session id
   (run-at-time at nil #'harmless-schedule-fire session id)))

(defun harmless-schedule--store (session id prompt iso)
  "Append one schedule on SESSION and save it."
  (setf (harmless-session-schedules session)
        (append (harmless-session-schedules session)
                (list (list :id id :prompt prompt :at iso))))
  (harmless-session-save session))

(defun harmless-schedule--remove (session id)
  "Drop ID from SESSION and save."
  (harmless-schedule--forget session id)
  (setf (harmless-session-schedules session)
        (cl-remove-if (lambda (item)
                        (equal (plist-get item :id) id))
                      (harmless-session-schedules session)))
  (harmless-session-save session))

(defun harmless-schedule-fire (session id)
  "Send the prompt stored as ID on SESSION, if it is still waiting.
A session that is already in a turn queues the prompt.  A missing
id does nothing."
  (when (harmless-schedule--find session id)
    (let ((prompt (plist-get (harmless-schedule--find session id) :prompt)))
      (harmless-schedule--remove session id)
      (condition-case nil
          (harmless-turn-steer session prompt)
        (error nil)))))

(defun harmless-schedule--clean (raw now)
  "Return a stored plist for RAW when it is still due after NOW."
  (let ((id (plist-get raw :id))
        (prompt (plist-get raw :prompt))
        (at (plist-get raw :at)))
    (when (and (stringp id)
               (not (string-empty-p (string-trim id)))
               (stringp prompt)
               (not (string-empty-p (string-trim prompt)))
               (stringp at))
      (let ((time (harmless-schedule--parse (string-trim at))))
        (when (and time (time-less-p now time))
          (list :id (string-trim id)
                :prompt prompt
                :at (harmless-schedule--iso time)))))))

(defun harmless-schedule--same-p (raw kept)
  "Return non-nil when RAW and KEPT describe the same schedules."
  (and (listp raw)
       (= (length raw) (length kept))
       (cl-every
        (lambda (left right)
          (and (equal (plist-get left :id) (plist-get right :id))
               (equal (plist-get left :prompt) (plist-get right :prompt))
               (equal (plist-get left :at) (plist-get right :at))))
        raw kept)))

(defun harmless-schedule-restore (session summary)
  "Keep unexpired schedules from SUMMARY and arm them.
An expired schedule is dropped and is not sent.  Save when the
kept list differs from what SUMMARY stored."
  (let* ((raw (plist-get summary :schedules))
         (now (current-time))
         (kept nil))
    (when (listp raw)
      (dolist (item raw)
        (when (harmless-plist-p item)
          (let ((clean (harmless-schedule--clean item now)))
            (when clean
              (push clean kept))))))
    (setq kept (nreverse kept))
    (setf (harmless-session-schedules session) kept)
    (unless (harmless-schedule--same-p raw kept)
      (condition-case nil
          (harmless-session-save session)
        (error nil)))
    (dolist (item kept)
      (let ((time (harmless-schedule--parse (plist-get item :at))))
        (when time
          (condition-case nil
              (funcall harmless-schedule-arm-function
                       session
                       (plist-get item :id)
                       time)
            (error nil)))))
    kept))

(defun harmless-schedule-tool (session args)
  "schedule implementation."
  (let* ((prompt (harmless-schedule--prompt
                  (harmless-tool-arg args :prompt)))
         (due (harmless-schedule--time args))
         (id (harmless-schedule--next-id session))
         (iso (cdr due)))
    (harmless-schedule--store session id prompt iso)
    (funcall harmless-schedule-arm-function session id (car due))
    (format "Scheduled %s at %s" id iso)))

(defun harmless-schedule-cancel-tool (session args)
  "schedule_cancel implementation."
  (let* ((id (harmless-schedule--id (harmless-tool-arg args :id)))
         (found (harmless-schedule--find session id)))
    (unless found
      (error "no schedule %s" id))
    (harmless-schedule--remove session id)
    (format "Cancelled schedule %s" id)))

(defun harmless-schedule-register ()
  "Register the schedule tools."
  (harmless-register-tool
   (harmless-tool-create
    :name "schedule"
    :description "Send PROMPT once, after DELAY seconds or at an absolute UTC time AT. Pass one of delay or at. The prompt and the time are saved with the session. An unexpired schedule is restored when the session is loaded. An expired one is dropped and is not sent. This is not a repeating job."
    :class 'edit
    :schema '(:type "object"
              :properties (:prompt (:type "string"
                                    :description "Prompt to send")
                           :delay (:type "number"
                                   :description "Seconds from now")
                           :at (:type "string"
                                :description "Absolute UTC time"))
              :required ["prompt"])
    :fn #'harmless-schedule-tool))
  (harmless-register-tool
   (harmless-tool-create
    :name "schedule_cancel"
    :description "Cancel a scheduled prompt by id. The prompt is not sent."
    :class 'edit
    :schema '(:type "object"
              :properties (:id (:type "string"
                                 :description "Schedule id"))
              :required ["id"])
    :fn #'harmless-schedule-cancel-tool)))

(harmless-schedule-register)

(provide 'harmless-schedule)

;;; harmless-schedule.el ends here
