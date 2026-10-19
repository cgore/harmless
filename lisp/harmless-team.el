;;; harmless-team.el --- A rig of pods and named seats -*- lexical-binding: t; -*-

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
;; A rig is a file of pods and named seats.  A seat has a role and
;; an address `pod-member@rig`.  The session id can be replaced.
;; The name, the role, and the inbound work stay.  Edges are
;; recorded on the rig.  `delegates_to` and `spawned_by` decide
;; the launch order.  A spec, the built-in starter, or a bundle
;; can boot a rig.  Preview does not write.  A seat can adopt a
;; session that is already running.  Release clears that binding
;; and does not stop the session.  Send appends text to a seat's
;; screen and keeps a transcript across occupants.  The rig has
;; a chatroom: history, topics, and a wait for the next line.
;; A pace walks context into one seat, one piece at a time.
;; A queue row is owned work on the rig.  Replacing the seat
;; does not drop it.  A claim holds a row.  A note appends
;; history.  Closing records delivery.  Acceptance stays the
;; next seat's verdict.  A hand closes the source row and opens
;; the destination row in one write.  A park has one wake.
;; Resolving it records the decision and returns the row to
;; held.  Intake holds observations nobody owns.  The board
;; shows held rows, rows stalled after a claim, and rows
;; waiting on a person.  A declared workflow opens its first
;; step.  Closing that step opens the next step in the same
;; write.  The owner decides that the step is done.  A watchdog
;; is a reminder, a pool of artifacts, a nudge, an idle queue,
;; or a seat near its context wall.  The job stays in the rig
;; file.  It does not arm a timer or a schedule.  A snapshot
;; is a separate file.  The report says, per seat, whether it
;; resumed, was rebuilt, started fresh, is waiting, or failed.
;; A fresh start is a choice.  The check does not write.  A
;; restore that would fail does not write.  Handing a seat to
;; a successor keeps the seat's name, its edges, and its
;; inbound work.  The recap of decisions is written at that
;; boundary.  The successor is already running.  An image
;; captures a productive seat so another seat can start from
;; it.  A packet carries what that seat knows when its runtime
;; changes.  Neither one starts a session.  A compact plan
;; lists seats near the context wall and writes nothing.
;; Compacting deliberately records why and tells the room.
;; The transcript summary is M-x harmless-compact.  This file
;; does not register another machine.

;;; Code:

(require 'subr-x)
(require 'harmless-util)
(require 'harmless-tools-fs)

(defconst harmless-team--session-re
  (concat "\\`[0-9a-fA-F]\\{8\\}"
          "-[0-9a-fA-F]\\{4\\}"
          "-[0-9a-fA-F]\\{4\\}"
          "-[0-9a-fA-F]\\{4\\}"
          "-[0-9a-fA-F]\\{12\\}\\'")
  "Match one Harmless session id.")

(defun harmless-team--root-text-p (text)
  "Return non-nil if TEXT is \"/\" or a drive root.
TEXT is not expanded.  A relative path and \"~\" are not roots."
  (and (stringp text)
       (or (harmless-extra-root-text-p text)
           (harmless-filesystem-root-p text))))

(defun harmless-team--member-name-p (name)
  "Return non-nil if NAME is a pod or seat name.
A hyphen belongs to the address, so NAME has none."
  (and (stringp name)
       (string-match-p "\\`[A-Za-z0-9]+\\'" name)))

(defun harmless-team--rig-name-p (name)
  "Return non-nil if NAME can be a rig name."
  (and (stringp name)
       (string-match-p
        "\\`[A-Za-z0-9]+\\(?:-[A-Za-z0-9]+\\)*\\'" name)))

(defun harmless-team--token (text empty type-err name-p name-err)
  "Return TEXT trimmed, or signal.
EMPTY is the blank error.  TYPE-ERR is the non-string error.
NAME-P accepts the trimmed text.  NAME-ERR is its refusal."
  (cond
   ((null text) (error empty))
   ((not (stringp text)) (error type-err)))
  (setq text (string-trim text))
  (cond
   ((string-empty-p text) (error empty))
   ((not (funcall name-p text)) (error name-err))
   (t text)))

(defun harmless-team--rig-name (rig)
  "Return RIG trimmed, or signal."
  (harmless-team--token
   rig "team rig is empty" "team rig must be a string"
   #'harmless-team--rig-name-p "team rig is not a name"))

(defun harmless-team--pod-name (pod)
  "Return POD trimmed, or signal."
  (harmless-team--token
   pod "team pod is empty" "team pod must be a string"
   #'harmless-team--member-name-p "team pod is not a name"))

(defun harmless-team--seat-name (seat)
  "Return SEAT trimmed, or signal."
  (harmless-team--token
   seat "team seat is empty" "team seat must be a string"
   #'harmless-team--member-name-p "team seat is not a name"))

(defun harmless-team--role (role)
  "Return ROLE trimmed, or signal."
  (cond
   ((null role) (error "team role is empty"))
   ((not (stringp role))
    (error "team role must be a string")))
  (setq role (string-trim role))
  (if (string-empty-p role)
      (error "team role is empty")
    role))

(defun harmless-team--session (session)
  "Return SESSION trimmed, or signal.
A blank id signals \"team session is empty\".  Anything that is
not one id signals \"team session is not an id\"."
  (cond
   ((null session) (error "team session is empty"))
   ((not (stringp session))
    (error "team session must be a string")))
  (setq session (string-trim session))
  (cond
   ((string-empty-p session) (error "team session is empty"))
   ((not (string-match-p harmless-team--session-re session))
    (error "team session is not an id"))
   (t session)))

(defun harmless-team--work-text (text)
  "Return TEXT trimmed, or signal.
Internal newlines stay.  Blank text signals \"team work is
empty\"."
  (cond
   ((null text) (error "team work is empty"))
   ((not (stringp text))
    (error "team work must be a string")))
  (setq text (string-trim text))
  (if (string-empty-p text)
      (error "team work is empty")
    text))

(defun harmless-team-address (rig pod seat)
  "Return the address POD-SEAT@RIG.
Names are checked.  Nothing is written."
  (setq rig (harmless-team--rig-name rig))
  (setq pod (harmless-team--pod-name pod))
  (setq seat (harmless-team--seat-name seat))
  (format "%s-%s@%s" pod seat rig))

(defun harmless-team--same-p (a b)
  "Return non-nil if names A and B are the same seat name.
Windows compares them without case."
  (and (stringp a) (stringp b)
       (if (eq system-type 'windows-nt)
           (string= (downcase a) (downcase b))
         (string= a b))))

(defun harmless-team--refuse-root (place)
  "Signal if PLACE is the filesystem root or a drive root.
The error is \"Refusing to store a team at \" followed by
PLACE.  PLACE is not created."
  (let ((text (if (stringp place) (string-trim place) "")))
    (when (or (string-empty-p text)
              (harmless-team--root-text-p text)
              (and (file-name-absolute-p text)
                   (not (string-prefix-p "~" text))
                   (equal (directory-file-name (file-truename text))
                          (directory-file-name (file-truename "/")))))
      (error "Refusing to store a team at %s" text))))

(defun harmless-team--data-directory ()
  "Return the Harmless data directory, or signal.
A filesystem root and a drive root are refused before a team
file is created."
  (let ((raw harmless-directory))
    (when (and (stringp raw) (not (string-empty-p raw)))
      (harmless-team--refuse-root raw))
    (let ((data (harmless-data-directory)))
      (harmless-team--refuse-root data)
      data)))

(defun harmless-team--teams-root (create)
  "Return the teams directory under the Harmless data directory.
When CREATE is non-nil, create it.  A filesystem root is refused."
  (let ((root (file-name-as-directory
               (expand-file-name
                "teams" (harmless-team--data-directory)))))
    (harmless-team--refuse-root root)
    (when create
      (harmless-ensure-directory root))
    root))

(defun harmless-team--existing (root rig)
  "Return the rig file for RIG under ROOT, or nil.
RIG is already a valid name.  The directory is not created."
  (let ((want (concat rig ".json"))
        (found nil))
    (when (file-directory-p root)
      (dolist (file (directory-files root t "\\.json\\'"))
        (when (harmless-team--same-p
               (file-name-nondirectory file) want)
          (setq found file))))
    found))

(defun harmless-team--new-path (root rig)
  "Return a new rig file for RIG under ROOT, or signal."
  (let ((path (expand-file-name (concat rig ".json") root)))
    (unless (and (file-name-absolute-p path)
                 (harmless-directory-strictly-under-p path root)
                 (not (harmless-team--root-text-p path))
                 (not (equal (file-truename path) (file-truename "/"))))
      (error "Refusing to store a team at %s" path))
    path))

(defun harmless-team--string-list-p (value)
  "Return non-nil if VALUE is a proper list of strings.
Nil is an empty list."
  (while (and (consp value) (stringp (car value)))
    (setq value (cdr value)))
  (null value))

(defun harmless-team--seen-p (name seen)
  "Return non-nil if NAME is already in SEEN."
  (let ((found nil))
    (dolist (item seen)
      (when (harmless-team--same-p item name)
        (setq found t)))
    found))

(defconst harmless-team--runtimes
  '("harmless" "claude" "codex" "pi" "terminal")
  "Runtimes a seat can adopt.  Release does not stop one.")

(defun harmless-team--stored-session-p (seat)
  "Return non-nil if SEAT's session and runtime agree.
A missing runtime is an empty seat or an occupied session id.
`harmless' keeps a session id.  The other runtimes keep a
non-empty reference."
  (let ((session (plist-get seat :session))
        (runtime (plist-get seat :runtime)))
    (and (stringp session)
         (cond
          ((null runtime)
           (or (string-empty-p session)
               (string-match-p harmless-team--session-re session)))
          ((equal runtime "harmless")
           (string-match-p harmless-team--session-re session))
          ((and (stringp runtime)
                (member runtime harmless-team--runtimes))
           (not (string-empty-p session)))))))

(defun harmless-team--seat-ok-p (seat pod rig seen)
  "Return non-nil if SEAT belongs to POD in RIG.
SEEN holds seat names already accepted in POD."
  (and (harmless-plist-p seat)
       (harmless-team--member-name-p (plist-get seat :name))
       (not (harmless-team--seen-p (plist-get seat :name) seen))
       (stringp (plist-get seat :role))
       (not (string-empty-p (plist-get seat :role)))
       (harmless-team--stored-session-p seat)
       (harmless-team--string-list-p (plist-get seat :work))
       (equal (plist-get seat :address)
              (format "%s-%s@%s"
                      pod (plist-get seat :name) rig))))

(defun harmless-team--pod-ok-p (pod rig seen-pods)
  "Return non-nil if POD is one pod in RIG.
SEEN-PODS holds pod names already accepted."
  (and (harmless-plist-p pod)
       (harmless-team--member-name-p (plist-get pod :name))
       (not (harmless-team--seen-p (plist-get pod :name) seen-pods))
       (let ((seats (plist-get pod :seats))
             (seen nil)
             (ok t))
         (while (and ok (consp seats))
           (if (harmless-team--seat-ok-p
                (car seats) (plist-get pod :name) rig seen)
               (setq seen (cons (plist-get (car seats) :name) seen)
                     seats (cdr seats))
             (setq ok nil)))
         (and ok (null seats)))))

(defun harmless-team--transcript-ok-p (rig)
  "Return non-nil if RIG's transcript is readable.
Nil means the rig has no transcript yet."
  (let ((items (plist-get rig :transcript)))
    (if (null items)
        t
      (let ((ok t))
        (while (and ok (consp items))
          (let ((item (car items)))
            (if (and (harmless-plist-p item)
                     (stringp (plist-get item :seat))
                     (not (string-empty-p (plist-get item :seat)))
                     (or (null (plist-get item :runtime))
                         (member (plist-get item :runtime)
                                 harmless-team--runtimes))
                     (stringp (plist-get item :session))
                     (stringp (plist-get item :text))
                     (not (string-empty-p (plist-get item :text))))
                (setq items (cdr items))
              (setq ok nil))))
        (and ok (null items))))))

(defun harmless-team--room-ok-p (rig)
  "Return non-nil if RIG's chatroom is readable.
Nil means the rig has no room yet."
  (let ((items (plist-get rig :room)))
    (if (null items)
        t
      (let ((ok t))
        (while (and ok (consp items))
          (let ((item (car items)))
            (if (and (harmless-plist-p item)
                     (harmless-team--member-name-p
                      (plist-get item :topic))
                     (stringp (plist-get item :speaker))
                     (not (string-empty-p (plist-get item :speaker)))
                     (stringp (plist-get item :text))
                     (not (string-empty-p (plist-get item :text)))
                     (stringp (plist-get item :at))
                     (not (string-empty-p (plist-get item :at))))
                (setq items (cdr items))
              (setq ok nil))))
        (and ok (null items))))))

(defun harmless-team--paces-ok-p (rig)
  "Return non-nil if RIG's paces are readable.
Nil means the rig has no pace yet."
  (let ((items (plist-get rig :paces)))
    (if (null items)
        t
      (let ((ok t)
            (seen nil))
        (while (and ok (consp items))
          (let* ((item (car items))
                 (pieces (and (harmless-plist-p item)
                              (plist-get item :pieces)))
                 (cursor (and (harmless-plist-p item)
                              (plist-get item :cursor))))
            (if (and (harmless-plist-p item)
                     (harmless-team--member-name-p
                      (plist-get item :name))
                     (not (harmless-team--seen-p
                           (plist-get item :name) seen))
                     (stringp (plist-get item :seat))
                     (not (string-empty-p (plist-get item :seat)))
                     (harmless-team--string-list-p pieces)
                     (integerp cursor)
                     (>= cursor 0)
                     (<= cursor (length pieces)))
                (setq seen (cons (plist-get item :name) seen)
                      items (cdr items))
              (setq ok nil))))
        (and ok (null items))))))

(defconst harmless-team--row-reasons
  '("handed_off_to" "blocked_on" "denied" "canceled"
    "no-follow-on" "escalation")
  "Reasons that can close a queue row.
Closing records delivery.")

(defun harmless-team--row-plain-p (text)
  "Return non-nil if TEXT is a non-empty string."
  (and (stringp text) (not (string-empty-p text))))

(defun harmless-team--row-state-p (item)
  "Return non-nil if ITEM's state and fields agree.
Open and held rows carry no reason or wake.  A closed row has
one reason.  A parked row has one wake."
  (let ((state (plist-get item :state))
        (reason (plist-get item :reason))
        (wake (plist-get item :wake))
        (summary (plist-get item :summary))
        (evidence (plist-get item :evidence)))
    (cond
     ((member state '("open" "held"))
      (and (null reason) (null wake)
           (null summary) (null evidence)))
     ((equal state "closed")
      (and (stringp reason)
           (member reason harmless-team--row-reasons)
           (null wake) (null summary) (null evidence)))
     ((equal state "parked")
      (and (null reason)
           (harmless-team--row-plain-p wake)
           (or (and (null summary) (null evidence))
               (and (harmless-team--row-plain-p summary)
                    (harmless-team--row-plain-p evidence)))))
     (t nil))))

(defun harmless-team--row-id-p (id)
  "Return non-nil if ID is a row id."
  (and (stringp id) (string-match-p "\\`[1-9][0-9]*\\'" id)))

(defun harmless-team--rows-ok-p (rig)
  "Return non-nil if RIG's queue rows are readable.
Nil means the rig has no row yet."
  (let ((items (plist-get rig :rows)))
    (if (null items)
        t
      (let ((ok t)
            (seen nil))
        (while (and ok (consp items))
          (let ((item (car items)))
            (if (and (harmless-plist-p item)
                     (harmless-team--row-id-p (plist-get item :id))
                     (not (member (plist-get item :id) seen))
                     (stringp (plist-get item :seat))
                     (not (string-empty-p (plist-get item :seat)))
                     (stringp (plist-get item :body))
                     (not (string-empty-p (plist-get item :body)))
                     (harmless-team--row-state-p item)
                     (harmless-team--row-history-p
                      (plist-get item :history)))
                (setq seen (cons (plist-get item :id) seen)
                      items (cdr items))
              (setq ok nil))))
        (and ok (null items))))))

(defun harmless-team--row-history-p (items)
  "Return non-nil if ITEMS is a row's history.
Nil is an empty history."
  (if (null items)
      t
    (let ((ok t))
      (while (and ok (consp items))
        (let ((item (car items)))
          (if (and (harmless-plist-p item)
                   (stringp (plist-get item :at))
                   (not (string-empty-p (plist-get item :at)))
                   (stringp (plist-get item :text))
                   (not (string-empty-p (plist-get item :text))))
              (setq items (cdr items))
            (setq ok nil))))
      (and ok (null items)))))

(defun harmless-team--intake-ok-p (rig)
  "Return non-nil if RIG's intake is readable.
Nil means the rig has no intake yet."
  (let ((items (plist-get rig :intake)))
    (if (null items)
        t
      (let ((ok t)
            (seen nil))
        (while (and ok (consp items))
          (let ((item (car items)))
            (if (and (harmless-plist-p item)
                     (harmless-team--row-id-p (plist-get item :id))
                     (not (member (plist-get item :id) seen))
                     (stringp (plist-get item :body))
                     (not (string-empty-p (plist-get item :body)))
                     (stringp (plist-get item :at))
                     (not (string-empty-p (plist-get item :at))))
                (setq seen (cons (plist-get item :id) seen)
                      items (cdr items))
              (setq ok nil))))
        (and ok (null items))))))

(defun harmless-team--workflow-steps-ok-p (steps)
  "Return non-nil if STEPS is a list of seat and body."
  (and (consp steps)
       (let ((ok t))
         (while (and ok (consp steps))
           (let ((step (car steps)))
             (if (and (harmless-plist-p step)
                      (stringp (plist-get step :seat))
                      (not (string-empty-p (plist-get step :seat)))
                      (stringp (plist-get step :body))
                      (not (string-empty-p (plist-get step :body))))
                 (setq steps (cdr steps))
               (setq ok nil))))
         (and ok (null steps)))))

(defun harmless-team--workflows-ok-p (rig)
  "Return non-nil if RIG's workflows are readable.
Nil means the rig has no workflow yet."
  (let ((items (plist-get rig :workflows)))
    (if (null items)
        t
      (let ((ok t)
            (seen nil)
            (ids nil))
        (while (and ok (consp items))
          (let* ((item (car items))
                 (steps (and (harmless-plist-p item)
                             (plist-get item :steps)))
                 (index (and (harmless-plist-p item)
                             (plist-get item :index)))
                 (row (and (harmless-plist-p item)
                           (plist-get item :row))))
            (if (and (harmless-plist-p item)
                     (harmless-team--row-id-p (plist-get item :id))
                     (not (member (plist-get item :id) ids))
                     (harmless-team--member-name-p
                      (plist-get item :name))
                     (not (harmless-team--seen-p
                           (plist-get item :name) seen))
                     (harmless-team--workflow-steps-ok-p steps)
                     (integerp index)
                     (>= index 0)
                     (<= index (length steps))
                     (if (< index (length steps))
                         (harmless-team--row-id-p row)
                       (null row)))
                (setq ids (cons (plist-get item :id) ids)
                      seen (cons (plist-get item :name) seen)
                      items (cdr items))
              (setq ok nil))))
        (and ok (null items))))))

(defconst harmless-team--watchdog-kinds
  '("reminder" "artifacts" "nudge" "idle" "wall")
  "Watchdog kinds a rig can store.
The job stays in the rig file.  It is not a session schedule.")

(defun harmless-team--watchdog-kind-p (kind)
  "Return non-nil if KIND is one watchdog kind."
  (and (stringp kind) (member kind harmless-team--watchdog-kinds)))

(defun harmless-team--watchdog-due-p (text)
  "Return non-nil if TEXT is one UTC time stamp."
  (and (stringp text)
       (string-match-p
        (concat "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
                "T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}Z\\'")
        text)))

(defun harmless-team--watchdog-pool-p (pool)
  "Return non-nil if POOL is a list of artifact text."
  (and (consp pool)
       (let ((ok t))
         (while (and ok (consp pool))
           (if (and (stringp (car pool))
                    (not (string-empty-p (car pool))))
               (setq pool (cdr pool))
             (setq ok nil)))
         (and ok (null pool)))))

(defun harmless-team--watchdog-job-p (item)
  "Return non-nil if ITEM is one stored watchdog."
  (and (harmless-plist-p item)
       (harmless-team--row-id-p (plist-get item :id))
       (harmless-team--watchdog-kind-p (plist-get item :kind))
       (let ((kind (plist-get item :kind))
             (seat (plist-get item :seat))
             (target (plist-get item :target))
             (text (plist-get item :text))
             (due (plist-get item :due))
             (pool (plist-get item :pool))
             (percent (plist-get item :percent)))
         (cond
          ((equal kind "reminder")
           (and (stringp seat) (not (string-empty-p seat))
                (stringp text) (not (string-empty-p text))
                (harmless-team--watchdog-due-p due)
                (null target) (null pool) (null percent)))
          ((equal kind "artifacts")
           (and (null seat) (null target) (null text)
                (null due) (null percent)
                (harmless-team--watchdog-pool-p pool)))
          ((equal kind "nudge")
           (and (null seat) (null text) (null due)
                (null pool) (null percent)
                (harmless-team--member-name-p target)))
          ((equal kind "idle")
           (and (stringp seat) (not (string-empty-p seat))
                (null target) (null text) (null due)
                (null pool) (null percent)))
          ((equal kind "wall")
           (and (stringp seat) (not (string-empty-p seat))
                (null target) (null text) (null due)
                (null pool)
                (integerp percent)
                (>= percent 1) (<= percent 100)))))))

(defun harmless-team--watchdogs-ok-p (rig)
  "Return non-nil if RIG's watchdogs are readable.
Nil means the rig has no watchdog yet."
  (let ((items (plist-get rig :watchdogs)))
    (if (null items)
        t
      (let ((ok t)
            (ids nil))
        (while (and ok (consp items))
          (let ((item (car items)))
            (if (and (harmless-team--watchdog-job-p item)
                     (not (member (plist-get item :id) ids)))
                (setq ids (cons (plist-get item :id) ids)
                      items (cdr items))
              (setq ok nil))))
        (and ok (null items))))))

(defun harmless-team--boundary-p (item)
  "Return non-nil if ITEM is one stored handover boundary."
  (and (harmless-plist-p item)
       (harmless-team--row-id-p (plist-get item :id))
       (stringp (plist-get item :seat))
       (not (string-empty-p (plist-get item :seat)))
       (stringp (plist-get item :text))
       (not (string-empty-p (plist-get item :text)))
       (harmless-team--watchdog-due-p (plist-get item :at))
       (stringp (plist-get item :from))
       (not (string-empty-p (plist-get item :from)))
       (or (null (plist-get item :from-runtime))
           (member (plist-get item :from-runtime)
                   harmless-team--runtimes))
       (member (plist-get item :runtime) harmless-team--runtimes)
       (stringp (plist-get item :session))
       (not (string-empty-p (plist-get item :session)))))

(defun harmless-team--boundaries-ok-p (rig)
  "Return non-nil if RIG's handover boundaries are readable.
Nil means the rig has no boundary yet."
  (let ((items (plist-get rig :boundaries)))
    (if (null items)
        t
      (let ((ok t)
            (ids nil))
        (while (and ok (consp items))
          (let ((item (car items)))
            (if (and (harmless-team--boundary-p item)
                     (not (member (plist-get item :id) ids)))
                (setq ids (cons (plist-get item :id) ids)
                      items (cdr items))
              (setq ok nil))))
        (and ok (null items))))))

(defun harmless-team--compact-p (item)
  "Return non-nil if ITEM is one stored compact notice."
  (and (harmless-plist-p item)
       (harmless-team--row-id-p (plist-get item :id))
       (stringp (plist-get item :seat))
       (not (string-empty-p (plist-get item :seat)))
       (stringp (plist-get item :session))
       (not (string-empty-p (plist-get item :session)))
       (stringp (plist-get item :why))
       (not (string-empty-p (plist-get item :why)))
       (harmless-team--watchdog-due-p (plist-get item :at))
       (numberp (plist-get item :used))
       (>= (plist-get item :used) 0)
       (numberp (plist-get item :window))
       (> (plist-get item :window) 0)
       (integerp (plist-get item :percent))
       (>= (plist-get item :percent) 1)
       (<= (plist-get item :percent) 100)))

(defun harmless-team--compacts-ok-p (rig)
  "Return non-nil if RIG's compact notices are readable.
Nil means the rig has no compact notice yet."
  (let ((items (plist-get rig :compacts)))
    (if (null items)
        t
      (let ((ok t)
            (ids nil))
        (while (and ok (consp items))
          (let ((item (car items)))
            (if (and (harmless-team--compact-p item)
                     (not (member (plist-get item :id) ids)))
                (setq ids (cons (plist-get item :id) ids)
                      items (cdr items))
              (setq ok nil))))
        (and ok (null items))))))

(defun harmless-team--structure-p (rig)
  "Return non-nil if RIG is a rig plist this file can reread."
  (and (harmless-plist-p rig)
       (harmless-team--rig-name-p (plist-get rig :name))
       (let ((pods (plist-get rig :pods))
             (seen nil)
             (ok t))
         (while (and ok (consp pods))
           (if (harmless-team--pod-ok-p (car pods) (plist-get rig :name) seen)
               (setq seen (cons (plist-get (car pods) :name) seen)
                     pods (cdr pods))
             (setq ok nil)))
         (and ok (null pods)))
       (harmless-team--edges-ok-p rig)
       (harmless-team--transcript-ok-p rig)
       (harmless-team--room-ok-p rig)
       (harmless-team--paces-ok-p rig)
       (harmless-team--rows-ok-p rig)
       (harmless-team--intake-ok-p rig)
       (harmless-team--workflows-ok-p rig)
       (harmless-team--watchdogs-ok-p rig)
       (harmless-team--boundaries-ok-p rig)
       (harmless-team--compacts-ok-p rig)))

(defconst harmless-team--edge-kinds
  '("delegates_to" "spawned_by" "can_observe"
    "collaborates_with" "escalates_to")
  "Edge kinds a rig can record.")

(defun harmless-team--edge-kind-p (kind)
  "Return non-nil if KIND is one recorded edge."
  (and (stringp kind) (member kind harmless-team--edge-kinds)))

(defun harmless-team--key (name)
  "Return NAME as a hash key.  Windows ignores case."
  (if (and (eq system-type 'windows-nt) (stringp name))
      (downcase name)
    name))

(defun harmless-team--addresses (rig)
  "Return seat addresses in RIG, in file order."
  (let ((out nil))
    (dolist (pod (plist-get rig :pods))
      (dolist (seat (plist-get pod :seats))
        (setq out (append out (list (plist-get seat :address))))))
    out))

(defun harmless-team--stored (key addresses)
  "Return the address in ADDRESSES whose key is KEY."
  (let ((found nil))
    (dolist (addr addresses)
      (when (and (not found)
                 (equal (harmless-team--key addr) key))
        (setq found addr)))
    found))

(defun harmless-team--before (edge)
  "Return (EARLIER . LATER) when EDGE decides launch order.
`delegates_to' runs the source first.  `spawned_by' runs the
target first.  The other kinds do not decide order."
  (cond
   ((equal (plist-get edge :kind) "delegates_to")
    (cons (plist-get edge :from) (plist-get edge :to)))
   ((equal (plist-get edge :kind) "spawned_by")
    (cons (plist-get edge :to) (plist-get edge :from)))))

(defun harmless-team--launch-list (addresses edges)
  "Return ADDRESSES in launch order, or the symbol `cycle'."
  (let ((pos (make-hash-table :test 'equal))
        (indeg (make-hash-table :test 'equal))
        (succ (make-hash-table :test 'equal))
        (n 0))
    (dolist (addr addresses)
      (let ((key (harmless-team--key addr)))
        (puthash key n pos)
        (puthash key 0 indeg)
        (setq n (1+ n))))
    (dolist (edge edges)
      (let ((pair (harmless-team--before edge)))
        (when pair
          (let ((earlier (harmless-team--key (car pair)))
                (later (harmless-team--key (cdr pair))))
            (puthash later (1+ (gethash later indeg 0)) indeg)
            (puthash earlier
                     (cons later (gethash earlier succ))
                     succ)))))
    (let ((ready nil)
          (out nil)
          (left n))
      (dolist (addr addresses)
        (when (zerop (gethash (harmless-team--key addr) indeg 0))
          (push addr ready)))
      (setq ready (nreverse ready))
      (while ready
        (setq ready
              (sort ready
                    (lambda (a b)
                      (< (gethash (harmless-team--key a) pos)
                         (gethash (harmless-team--key b) pos)))))
        (let ((addr (car ready)))
          (setq ready (cdr ready))
          (push addr out)
          (setq left (1- left))
          (dolist (later (gethash (harmless-team--key addr) succ))
            (let ((left-deg (1- (gethash later indeg 0))))
              (puthash later left-deg indeg)
              (when (zerop left-deg)
                (push (harmless-team--stored later addresses)
                      ready))))))
      (if (zerop left) (nreverse out) 'cycle))))

(defun harmless-team--edge-seen-p (edge seen)
  "Return non-nil if EDGE is already in SEEN."
  (let ((found nil))
    (dolist (item seen)
      (when (and (equal (plist-get item :kind) (plist-get edge :kind))
                 (harmless-team--same-p (plist-get item :from)
                                        (plist-get edge :from))
                 (harmless-team--same-p (plist-get item :to)
                                        (plist-get edge :to)))
        (setq found t)))
    found))

(defun harmless-team--edges-ok-p (rig)
  "Return non-nil if RIG's edges are readable.
Nil edges are a rig with no edges.  A cycle is not readable."
  (let ((edges (plist-get rig :edges)))
    (if (null edges)
        t
      (let ((addresses (harmless-team--addresses rig))
            (seen nil)
            (ok t))
        (while (and ok (consp edges))
          (let ((edge (car edges)))
            (if (and (harmless-plist-p edge)
                     (harmless-team--edge-kind-p (plist-get edge :kind))
                     (harmless-team--seen-p
                      (plist-get edge :from) addresses)
                     (harmless-team--seen-p
                      (plist-get edge :to) addresses)
                     (not (harmless-team--same-p
                           (plist-get edge :from)
                           (plist-get edge :to)))
                     (not (harmless-team--edge-seen-p edge seen)))
                (setq seen (cons edge seen)
                      edges (cdr edges))
              (setq ok nil))))
        (and ok (null edges)
             (not (eq (harmless-team--launch-list
                       addresses (plist-get rig :edges))
                      'cycle)))))))

(defun harmless-team--read (path rig)
  "Return the rig plist at PATH, or signal.
RIG is the requested name.  A broken file signals \"team rig is
not valid\" and is not rewritten."
  (unless (and (stringp path)
               (file-name-absolute-p path)
               (file-regular-p path)
               (not (harmless-team--root-text-p path))
               (not (equal (file-truename path) (file-truename "/"))))
    (error "team rig is not valid"))
  (let* ((text (with-temp-buffer
                 (let ((coding-system-for-read 'utf-8-unix))
                   (insert-file-contents path))
                 (buffer-string)))
         (obj (harmless-json-decode-safe text))
         (base (file-name-sans-extension
                (file-name-nondirectory path))))
    (unless (and (harmless-team--structure-p obj)
                 (harmless-team--same-p (plist-get obj :name) rig)
                 (harmless-team--same-p (plist-get obj :name) base))
      (error "team rig is not valid"))
    obj))

(defun harmless-team--write (path rig)
  "Write RIG as JSON at PATH."
  (unless (and (stringp path)
               (file-name-absolute-p path)
               (not (harmless-team--root-text-p path))
               (not (equal (file-truename path) (file-truename "/"))))
    (error "Refusing to store a team at %s" path))
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region (harmless-json-text rig) nil path nil 'silent))
  path)

(defun harmless-team--find-named (items name)
  "Return the plist in ITEMS whose name matches NAME, or nil."
  (let ((found nil))
    (dolist (item items)
      (when (and (not found)
                 (harmless-plist-p item)
                 (harmless-team--same-p (plist-get item :name) name))
        (setq found item)))
    found))

(defun harmless-team--find (rig pod seat)
  "Return (POD . SEAT) inside RIG, or nil."
  (let* ((pod-item (harmless-team--find-named (plist-get rig :pods) pod))
         (seat-item (and pod-item
                         (harmless-team--find-named
                          (plist-get pod-item :seats) seat))))
    (when seat-item
      (cons pod-item seat-item))))

(defun harmless-team--public (rig pod seat)
  "Return the seat plist a caller sees."
  (let ((public (list :rig (plist-get rig :name)
                      :pod (plist-get pod :name)
                      :name (plist-get seat :name)
                      :role (plist-get seat :role)
                      :address (plist-get seat :address)
                      :session (plist-get seat :session)
                      :work (plist-get seat :work)))
        (runtime (plist-get seat :runtime)))
    (if (and (stringp runtime) (not (string-empty-p runtime)))
        (append public (list :runtime runtime))
      public)))

(defun harmless-team--load (rig)
  "Return (PATH . RIG) for RIG, or signal.
A missing file signals \"team rig is missing\"."
  (let* ((root (harmless-team--teams-root nil))
         (path (harmless-team--existing root rig)))
    (unless path
      (error "team rig is missing"))
    (cons path (harmless-team--read path rig))))

(defun harmless-team--add (rig pod seat role)
  "Add SEAT and return its public plist."
  (setq rig (harmless-team--rig-name rig))
  (setq pod (harmless-team--pod-name pod))
  (setq seat (harmless-team--seat-name seat))
  (setq role (harmless-team--role role))
  (let* ((root (harmless-team--teams-root nil))
         (path (harmless-team--existing root rig))
         (stored (if path
                     (harmless-team--read path rig)
                   (list :name rig :pods nil))))
    (unless path
      (setq path (harmless-team--new-path root rig)))
    (let* ((pod-item (harmless-team--find-named
                      (plist-get stored :pods) pod))
           (pod-name (if pod-item (plist-get pod-item :name) pod))
           (rig-name (plist-get stored :name)))
      (when (and pod-item
                 (harmless-team--find-named
                  (plist-get pod-item :seats) seat))
        (error "team seat already exists"))
      (let ((new (list :name seat
                       :role role
                       :session ""
                       :work nil
                       :address
                       (format "%s-%s@%s" pod-name seat rig-name))))
        (if pod-item
            (plist-put pod-item :seats
                       (append (plist-get pod-item :seats) (list new)))
          (setq stored
                (plist-put stored :pods
                           (append (plist-get stored :pods)
                                   (list (list :name pod-name
                                               :seats (list new)))))))
        (harmless-team--teams-root t)
        (harmless-team--write path stored)
        (harmless-team--public
         stored
         (if pod-item
             pod-item
           (harmless-team--find-named (plist-get stored :pods) pod))
         new)))))

(defun harmless-team--lookup (rig pod seat)
  "Return the public plist for SEAT, or signal."
  (setq rig (harmless-team--rig-name rig))
  (setq pod (harmless-team--pod-name pod))
  (setq seat (harmless-team--seat-name seat))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (found (harmless-team--find stored pod seat)))
    (unless found
      (error "team seat is missing"))
    (harmless-team--public stored (car found) (cdr found))))

(defun harmless-team--occupy (rig pod seat session)
  "Set SEAT's session id and return the public plist."
  (setq rig (harmless-team--rig-name rig))
  (setq pod (harmless-team--pod-name pod))
  (setq seat (harmless-team--seat-name seat))
  (setq session (harmless-team--session session))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (found (harmless-team--find stored pod seat)))
    (unless found
      (error "team seat is missing"))
    (plist-put (cdr found) :session session)
    (plist-put (cdr found) :runtime nil)
    (harmless-team--write (car loaded) stored)
    (harmless-team--public stored (car found) (cdr found))))

(defun harmless-team--assign-work (rig pod seat text)
  "Append TEXT to SEAT's work and return the public plist."
  (setq rig (harmless-team--rig-name rig))
  (setq pod (harmless-team--pod-name pod))
  (setq seat (harmless-team--seat-name seat))
  (setq text (harmless-team--work-text text))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (found (harmless-team--find stored pod seat)))
    (unless found
      (error "team seat is missing"))
    (plist-put (cdr found) :work
               (append (plist-get (cdr found) :work) (list text)))
    (harmless-team--write (car loaded) stored)
    (harmless-team--public stored (car found) (cdr found))))

;;;###autoload
(defun harmless-team-add-seat (rig pod seat role)
  "Add SEAT in POD of RIG with ROLE.
The address is POD-SEAT@RIG.  The seat starts with no session
and no inbound work.  Return the seat plist."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Pod: ")
         (read-string "Seat: ")
         (read-string "Role: ")))
  (let ((saved (harmless-team--add rig pod seat role)))
    (message "Seat %s" (plist-get saved :address))
    saved))

;;;###autoload
(defun harmless-team-occupy (rig pod seat session)
  "Put SESSION in SEAT of POD in RIG.
Calling this again replaces the session.  The name, the role,
and the inbound work stay.  Return the seat plist."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Pod: ")
         (read-string "Seat: ")
         (read-string "Session: ")))
  (let ((saved (harmless-team--occupy rig pod seat session)))
    (message "Seat %s is %s"
             (plist-get saved :address)
             (plist-get saved :session))
    saved))

;;;###autoload
(defun harmless-team-add-work (rig pod seat text)
  "Append TEXT to the inbound work of SEAT.
The text stays when the session is replaced.  Return the seat
plist."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Pod: ")
         (read-string "Seat: ")
         (read-string "Work: ")))
  (let ((saved (harmless-team--assign-work rig pod seat text)))
    (message "Work kept for %s" (plist-get saved :address))
    saved))

;;;###autoload
(defun harmless-team-seat (rig pod seat)
  "Return the seat SEAT in POD of RIG.
Read the rig file.  Do not change it."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Pod: ")
         (read-string "Seat: ")))
  (let ((saved (harmless-team--lookup rig pod seat)))
    (message "%s %s"
             (plist-get saved :address)
             (plist-get saved :session))
    saved))

(defun harmless-team--edge-kind (kind)
  "Return KIND trimmed, or signal."
  (cond
   ((null kind) (error "team edge is empty"))
   ((not (stringp kind))
    (error "team edge must be a string")))
  (setq kind (string-trim kind))
  (cond
   ((string-empty-p kind) (error "team edge is empty"))
   ((not (harmless-team--edge-kind-p kind))
    (error "team edge is not a kind"))
   (t kind)))

(defun harmless-team--address-of (rig pod seat)
  "Return the stored address of POD's SEAT in RIG, or nil."
  (let ((found (harmless-team--find rig pod seat)))
    (when found
      (plist-get (cdr found) :address))))

(defun harmless-team--endpoint (rig text)
  "Return the stored address for TEXT in RIG, or signal.
TEXT is `pod.member' or `pod-member@rig'."
  (cond
   ((null text) (error "team edge is empty"))
   ((not (stringp text))
    (error "team edge must be a string")))
  (setq text (string-trim text))
  (cond
   ((string-empty-p text) (error "team edge is empty"))
   ((string-match
     "\\`\\([A-Za-z0-9]+\\)\\.\\([A-Za-z0-9]+\\)\\'" text)
    (let ((pod (match-string 1 text))
          (seat (match-string 2 text)))
      (or (harmless-team--address-of rig pod seat)
          (error "team seat is missing"))))
   ((string-match
     "\\`\\([A-Za-z0-9]+\\)-\\([A-Za-z0-9]+\\)@\\(.+\\)\\'" text)
    (let ((pod (match-string 1 text))
          (seat (match-string 2 text))
          (name (match-string 3 text)))
      (unless (harmless-team--same-p name (plist-get rig :name))
        (error "team edge is not a seat"))
      (or (harmless-team--address-of rig pod seat)
          (error "team seat is missing"))))
   (t (error "team edge is not a seat"))))

(defun harmless-team--public-edge (edge)
  "Return EDGE with keys in a fixed order."
  (list :kind (plist-get edge :kind)
        :from (plist-get edge :from)
        :to (plist-get edge :to)))

(defun harmless-team--connect (rig kind from to)
  "Record an edge and return it.  A cycle is not written."
  (setq rig (harmless-team--rig-name rig))
  (setq kind (harmless-team--edge-kind kind))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (source (harmless-team--endpoint stored from))
         (target (harmless-team--endpoint stored to)))
    (when (harmless-team--same-p source target)
      (error "team edge is the same seat"))
    (let ((edges (plist-get stored :edges)))
      (dolist (edge edges)
        (when (and (equal (plist-get edge :kind) kind)
                   (harmless-team--same-p (plist-get edge :from) source)
                   (harmless-team--same-p (plist-get edge :to) target))
          (error "team edge already exists")))
      (let* ((new (list :kind kind :from source :to target))
             (next (append edges (list new))))
        (when (eq (harmless-team--launch-list
                   (harmless-team--addresses stored) next)
                  'cycle)
          (error "team edge is a cycle"))
        (setq stored (plist-put stored :edges next))
        (harmless-team--write (car loaded) stored)
        new))))

(defun harmless-team--edge-list (rig)
  "Return the edges of RIG in the order they were recorded."
  (setq rig (harmless-team--rig-name rig))
  (mapcar #'harmless-team--public-edge
          (plist-get (cdr (harmless-team--load rig)) :edges)))

(defun harmless-team--order (rig)
  "Return the launch order of RIG."
  (setq rig (harmless-team--rig-name rig))
  (let* ((stored (cdr (harmless-team--load rig)))
         (order (harmless-team--launch-list
                 (harmless-team--addresses stored)
                 (plist-get stored :edges))))
    (if (eq order 'cycle)
        (error "team rig is not valid")
      order)))

;;;###autoload
(defun harmless-team-edge (rig kind from to)
  "Record an edge of KIND from FROM to TO in RIG.
FROM and TO are `pod.member' or `pod-member@rig'.
`delegates_to' starts FROM before TO.  `spawned_by' starts TO
before FROM.  The other kinds are recorded and do not change
the order.  Return the edge."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Kind: ")
         (read-string "From: ")
         (read-string "To: ")))
  (let ((edge (harmless-team--connect rig kind from to)))
    (message "%s %s -> %s"
             (plist-get edge :kind)
             (plist-get edge :from)
             (plist-get edge :to))
    edge))

;;;###autoload
(defun harmless-team-edges (rig)
  "Return the edges recorded on RIG, in that order."
  (interactive (list (read-string "Rig: ")))
  (let ((edges (harmless-team--edge-list rig)))
    (message "%d edges" (length edges))
    edges))

;;;###autoload
(defun harmless-team-launch-order (rig)
  "Return the seat addresses of RIG in launch order.
`delegates_to' and `spawned_by' decide the order.  The other
edges do not.  This does not start a seat."
  (interactive (list (read-string "Rig: ")))
  (let ((order (harmless-team--order rig)))
    (message "%s" (mapconcat #'identity order ", "))
    order))

(defconst harmless-team--starter
  (list :name "starter"
        :pods (list
               (list :name "dev"
                     :seats (list
                             (list :name "build"
                                   :role "implementer")
                             (list :name "review"
                                   :role "checker"))))
        :edges (list
                (list :kind "delegates_to"
                      :from "dev.build"
                      :to "dev.review")))
  "The built-in starter rig.  Preview and boot copy it.")

(defun harmless-team--find-logical (members text)
  "Return the member in MEMBERS named by TEXT, or nil.
TEXT is `pod.member'."
  (when (and (stringp text)
             (string-match
              "\\`\\([A-Za-z0-9]+\\)\\.\\([A-Za-z0-9]+\\)\\'" text))
    (let ((pod (match-string 1 text))
          (seat (match-string 2 text))
          (found nil))
      (dolist (item members)
        (when (and (not found)
                   (harmless-team--same-p (plist-get item :pod) pod)
                   (harmless-team--same-p (plist-get item :seat) seat))
          (setq found item)))
      found)))

(defun harmless-team--normalize (spec)
  "Return SPEC in the form boot writes, or signal.
A broken spec signals \"team spec is not valid\"."
  (unless (and (harmless-plist-p spec)
               (harmless-team--rig-name-p (plist-get spec :name))
               (consp (plist-get spec :pods)))
    (error "team spec is not valid"))
  (let ((name (plist-get spec :name))
        (pods nil)
        (pod-seen nil)
        (members nil))
    (dolist (pod (plist-get spec :pods))
      (unless (and (harmless-plist-p pod)
                   (harmless-team--member-name-p (plist-get pod :name))
                   (not (harmless-team--seen-p
                         (plist-get pod :name) pod-seen))
                   (consp (plist-get pod :seats)))
        (error "team spec is not valid"))
      (setq pod-seen (cons (plist-get pod :name) pod-seen))
      (let ((seats nil)
            (seat-seen nil))
        (dolist (seat (plist-get pod :seats))
          (unless (and (harmless-plist-p seat)
                       (harmless-team--member-name-p
                        (plist-get seat :name))
                       (not (harmless-team--seen-p
                             (plist-get seat :name) seat-seen))
                       (stringp (plist-get seat :role))
                       (not (string-empty-p
                             (string-trim (plist-get seat :role)))))
            (error "team spec is not valid"))
          (setq seat-seen (cons (plist-get seat :name) seat-seen))
          (let* ((role (string-trim (plist-get seat :role)))
                 (logical (format "%s.%s"
                                  (plist-get pod :name)
                                  (plist-get seat :name)))
                 (address (format "%s-%s@%s"
                                  (plist-get pod :name)
                                  (plist-get seat :name)
                                  name)))
            (setq members
                  (append members
                          (list (list :logical logical
                                      :role role
                                      :pod (plist-get pod :name)
                                      :seat (plist-get seat :name)
                                      :address address))))
            (setq seats (append seats
                                (list (list :name (plist-get seat :name)
                                            :role role))))))
        (setq pods (append pods
                           (list (list :name (plist-get pod :name)
                                       :seats seats))))))
    (let ((edges nil)
          (edge-seen nil)
          (raw (plist-get spec :edges)))
      (when (and raw (not (consp raw)))
        (error "team spec is not valid"))
      (dolist (edge raw)
        (let ((from (and (harmless-plist-p edge)
                         (harmless-team--find-logical
                          members (plist-get edge :from))))
              (to (and (harmless-plist-p edge)
                       (harmless-team--find-logical
                        members (plist-get edge :to)))))
          (unless (and from to
                       (harmless-team--edge-kind-p
                        (plist-get edge :kind))
                       (not (harmless-team--same-p
                             (plist-get from :address)
                             (plist-get to :address))))
            (error "team spec is not valid"))
          (let ((new (list :kind (plist-get edge :kind)
                           :from (plist-get from :logical)
                           :to (plist-get to :logical)
                           :from-address (plist-get from :address)
                           :to-address (plist-get to :address))))
            (when (harmless-team--edge-seen-p
                   (list :kind (plist-get new :kind)
                         :from (plist-get new :from-address)
                         :to (plist-get new :to-address))
                   edge-seen)
              (error "team spec is not valid"))
            (setq edge-seen
                  (cons (list :kind (plist-get new :kind)
                              :from (plist-get new :from-address)
                              :to (plist-get new :to-address))
                        edge-seen))
            (setq edges (append edges (list new))))))
      (let* ((addresses (mapcar (lambda (member)
                                  (plist-get member :address))
                                members))
             (order-edges
              (mapcar (lambda (edge)
                        (list :kind (plist-get edge :kind)
                              :from (plist-get edge :from-address)
                              :to (plist-get edge :to-address)))
                      edges))
             (order (harmless-team--launch-list addresses order-edges)))
        (when (eq order 'cycle)
          (error "team spec is not valid"))
        (list :name name
              :pods pods
              :members members
              :edges edges
              :order order)))))

(defun harmless-team--plan (spec)
  "Return the preview plist for a normalized SPEC."
  (list :name (plist-get spec :name)
        :seats (mapcar (lambda (member)
                         (list (plist-get member :address)
                               (plist-get member :role)))
                       (plist-get spec :members))
        :edges (mapcar (lambda (edge)
                         (list :kind (plist-get edge :kind)
                               :from (plist-get edge :from-address)
                               :to (plist-get edge :to-address)))
                       (plist-get spec :edges))
        :order (plist-get spec :order)))

(defun harmless-team--read-spec-file (path)
  "Return the spec plist in PATH, or signal."
  (let ((obj (with-temp-buffer
               (let ((coding-system-for-read 'utf-8-unix))
                 (insert-file-contents path))
               (harmless-json-decode-safe (buffer-string)))))
    (unless (harmless-plist-p obj)
      (error "team spec is not valid"))
    obj))

(defun harmless-team--read-bundle (dir)
  "Return the spec in DIR's rig.json, or signal."
  (let ((path (expand-file-name "rig.json" dir)))
    (unless (and (file-regular-p path)
                 (harmless-directory-strictly-under-p path dir)
                 (not (equal (file-truename path)
                             (file-truename "/"))))
      (error "team spec is missing"))
    (harmless-team--read-spec-file path)))

(defun harmless-team--spec (text)
  "Return the spec TEXT names, or signal.
TEXT is \"starter\", an absolute spec file, or a bundle
directory.  \"~\" is not expanded.  Nothing is written."
  (cond
   ((null text) (error "team spec is empty"))
   ((not (stringp text))
    (error "team spec must be a string")))
  (setq text (string-trim text))
  (cond
   ((string-empty-p text) (error "team spec is empty"))
   ((harmless-team--root-text-p text)
    (error "team spec is the filesystem root"))
   ((equal text "starter")
    harmless-team--starter)
   ((or (string-prefix-p "~" text)
        (not (file-name-absolute-p text)))
    (error "team spec must be absolute")))
  (when (equal text "starter")
    (setq text nil))
  (if (null text)
      harmless-team--starter
    (progn
      (setq text (directory-file-name (expand-file-name text)))
      (when (or (harmless-team--root-text-p text)
                (equal (file-truename text) (file-truename "/")))
        (error "team spec is the filesystem root"))
      (cond
       ((not (file-exists-p text))
        (error "team spec is missing"))
       ((file-directory-p text)
        (harmless-team--read-bundle text))
       ((file-regular-p text)
        (harmless-team--read-spec-file text))
       (t (error "team spec is not valid"))))))

(defun harmless-team--preview (text)
  "Return the boot preview for TEXT.  Write nothing."
  (harmless-team--plan (harmless-team--normalize
                        (harmless-team--spec text))))

(defun harmless-team--boot (text)
  "Create the rig TEXT names and return the preview plist."
  (let* ((spec (harmless-team--normalize (harmless-team--spec text)))
         (name (plist-get spec :name))
         (root (harmless-team--teams-root nil)))
    (when (harmless-team--existing root name)
      (error "team rig already exists"))
    (dolist (pod (plist-get spec :pods))
      (dolist (seat (plist-get pod :seats))
        (harmless-team--add name
                            (plist-get pod :name)
                            (plist-get seat :name)
                            (plist-get seat :role))))
    (dolist (edge (plist-get spec :edges))
      (harmless-team--connect name
                              (plist-get edge :kind)
                              (plist-get edge :from)
                              (plist-get edge :to)))
    (harmless-team--plan spec)))

(defun harmless-team--find-address (rig address)
  "Return the seat plist in RIG with ADDRESS, or nil."
  (let ((found nil))
    (dolist (pod (plist-get rig :pods))
      (dolist (seat (plist-get pod :seats))
        (when (and (not found)
                   (harmless-team--same-p
                    (plist-get seat :address) address))
          (setq found seat))))
    found))

(defun harmless-team--excise (rig pod seat address)
  "Remove SEAT and edges that touch ADDRESS from RIG."
  (let ((pods nil))
    (dolist (item (plist-get rig :pods))
      (if (harmless-team--same-p (plist-get item :name) pod)
          (let ((seats nil))
            (dolist (one (plist-get item :seats))
              (unless (harmless-team--same-p
                       (plist-get one :name) seat)
                (setq seats (append seats (list one)))))
            (when seats
              (setq pods (append pods
                                 (list (plist-put item :seats seats))))))
        (setq pods (append pods (list item)))))
    (plist-put rig :pods pods))
  (let ((edges nil))
    (dolist (edge (plist-get rig :edges))
      (unless (or (harmless-team--same-p (plist-get edge :from) address)
                  (harmless-team--same-p (plist-get edge :to) address))
        (setq edges (append edges (list edge)))))
    (plist-put rig :edges edges))
  rig)

(defun harmless-team--drop (rig pod seat fallback)
  "Remove SEAT from RIG.  Work moves to FALLBACK when it exists."
  (setq rig (harmless-team--rig-name rig))
  (setq pod (harmless-team--pod-name pod))
  (setq seat (harmless-team--seat-name seat))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (found (harmless-team--find stored pod seat)))
    (unless found
      (error "team seat is missing"))
    (let* ((seat-item (cdr found))
           (address (plist-get seat-item :address))
           (work (plist-get seat-item :work))
           (target nil))
      (when (and work (null fallback))
        (error "team seat holds work"))
      (when fallback
        (setq target (harmless-team--endpoint stored fallback))
        (when (harmless-team--same-p target address)
          (error "team fallback is the same seat"))
        (when work
          (let ((other (harmless-team--find-address stored target)))
            (unless other
              (error "team seat is missing"))
            (plist-put other :work
                       (append (plist-get other :work) work)))))
      (harmless-team--excise stored pod seat address)
      (harmless-team--write (car loaded) stored)
      (list :removed address :fallback target :work work))))

;;;###autoload
(defun harmless-team-preview (spec)
  "Return what booting SPEC would record.
SPEC is \"starter\", an absolute JSON file, or a directory
with rig.json.  This does not write."
  (interactive (list (read-string "Spec: ")))
  (let ((plan (harmless-team--preview spec)))
    (message "Preview %s" (plist-get plan :name))
    plan))

;;;###autoload
(defun harmless-team-boot (spec)
  "Create the rig described by SPEC.
A second boot of the same rig is refused.  Return the preview
plist."
  (interactive (list (read-string "Spec: ")))
  (let ((plan (harmless-team--boot spec)))
    (message "Booted %s" (plist-get plan :name))
    plan))

;;;###autoload
(defun harmless-team-grow (rig pod seat role)
  "Add SEAT to the running rig RIG.
The pod is created when it is new.  Return the seat plist."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Pod: ")
         (read-string "Seat: ")
         (read-string "Role: ")))
  (let ((saved (harmless-team--add rig pod seat role)))
    (message "Grew %s" (plist-get saved :address))
    saved))

;;;###autoload
(defun harmless-team-remove (rig pod seat &optional fallback)
  "Remove SEAT from the running rig RIG.
When the seat holds work, FALLBACK receives that work.  Without
a fallback the removal is refused.  Edges that touch the seat
are dropped.  Return a plist of what was removed."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Pod: ")
         (read-string "Seat: ")
         (let ((answer (read-string "Fallback seat: ")))
           (if (string-empty-p (string-trim answer))
               nil
             answer))))
  (let ((removed (harmless-team--drop rig pod seat fallback)))
    (message "Removed %s" (plist-get removed :removed))
    removed))

(defun harmless-team--runtime (runtime)
  "Return RUNTIME trimmed, or signal."
  (cond
   ((null runtime) (error "team runtime is empty"))
   ((not (stringp runtime))
    (error "team runtime must be a string")))
  (setq runtime (string-trim runtime))
  (cond
   ((string-empty-p runtime) (error "team runtime is empty"))
   ((not (member runtime harmless-team--runtimes))
    (error "team runtime is not a runtime"))
   (t runtime)))

(defun harmless-team--existing-path (text)
  "Return TEXT as an absolute file or directory that exists.
\"~\" is not expanded.  TEXT is not created."
  (cond
   ((null text) (error "team session is empty"))
   ((not (stringp text))
    (error "team session must be a string")))
  (setq text (string-trim text))
  (cond
   ((string-empty-p text) (error "team session is empty"))
   ((or (harmless-team--root-text-p text)
        (and (file-name-absolute-p text)
             (not (string-prefix-p "~" text))
             (let ((expanded (directory-file-name
                              (expand-file-name text))))
               (or (harmless-team--root-text-p expanded)
                   (equal (file-truename expanded)
                          (file-truename "/"))))))
    (error "team session is the filesystem root"))
   ((or (string-prefix-p "~" text)
        (not (file-name-absolute-p text)))
    (error "team session must be absolute")))
  (setq text (directory-file-name (expand-file-name text)))
  (unless (or (file-regular-p text) (file-directory-p text))
    (error "team session is missing"))
  text)

(defun harmless-team--harmless-session (id)
  "Return the session directory for ID, or signal.
ID is a session id.  The directory must already exist.  This
does not create it."
  (setq id (harmless-team--session id))
  (let* ((root (file-name-as-directory
                (expand-file-name
                 "sessions" (harmless-team--data-directory))))
         (found nil))
    (when (and (file-directory-p root)
               (not (equal (file-truename root) (file-truename "/"))))
      (dolist (dir (directory-files-recursively
                    root (concat "\\`" (regexp-quote id) "\\'") t))
        (when (and (file-directory-p dir)
                   (file-regular-p
                    (expand-file-name "summary.json" dir))
                   (harmless-directory-strictly-under-p dir root)
                   (not (equal (file-truename dir)
                               (file-truename "/"))))
          (setq found (append found (list dir))))))
    (cond
     ((null found) (error "team session is missing"))
     ((cdr found) (error "team session is not one session"))
     (t (car found)))))

(defun harmless-team--terminal-ref (text)
  "Return a live process id or an existing path for TEXT.
A process id is a decimal string.  This does not start it."
  (cond
   ((null text) (error "team session is empty"))
   ((not (stringp text))
    (error "team session must be a string")))
  (setq text (string-trim text))
  (cond
   ((string-empty-p text) (error "team session is empty"))
   ((string-match-p "\\`[0-9]+\\'" text)
    (let ((pid (string-to-number text)))
      (unless (and (> pid 0) (process-attributes pid))
        (error "team session is not running"))
      (number-to-string pid)))
   (t (harmless-team--existing-path text))))

(defun harmless-team--occupant-ref (runtime ref)
  "Return the stored reference for RUNTIME and REF.
The session must already exist.  Nothing here is started."
  (cond
   ((equal runtime "harmless")
    (harmless-team--session ref)
    (harmless-team--harmless-session ref)
    (harmless-team--session ref))
   ((equal runtime "terminal")
    (harmless-team--terminal-ref ref))
   (t (harmless-team--existing-path ref))))

(defun harmless-team--adopt (rig pod seat runtime ref)
  "Bind REF to SEAT and return the public plist.
RUNTIME names the kind of session.  An earlier binding is
replaced.  The earlier session is not stopped."
  (setq rig (harmless-team--rig-name rig))
  (setq pod (harmless-team--pod-name pod))
  (setq seat (harmless-team--seat-name seat))
  (setq runtime (harmless-team--runtime runtime))
  (setq ref (harmless-team--occupant-ref runtime ref))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (found (harmless-team--find stored pod seat)))
    (unless found
      (error "team seat is missing"))
    (plist-put (cdr found) :session ref)
    (plist-put (cdr found) :runtime runtime)
    (harmless-team--write (car loaded) stored)
    (harmless-team--public stored (car found) (cdr found))))

(defun harmless-team--release (rig pod seat)
  "Clear SEAT's binding and return what was released.
The session, file, and process are left running."
  (setq rig (harmless-team--rig-name rig))
  (setq pod (harmless-team--pod-name pod))
  (setq seat (harmless-team--seat-name seat))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (found (harmless-team--find stored pod seat)))
    (unless found
      (error "team seat is missing"))
    (let* ((item (cdr found))
           (ref (plist-get item :session))
           (runtime (plist-get item :runtime)))
      (when (and (or (null ref) (string-empty-p ref))
                 (or (null runtime) (string-empty-p runtime)))
        (error "team seat is not occupied"))
      (plist-put item :session "")
      (plist-put item :runtime nil)
      (harmless-team--write (car loaded) stored)
      (list :released (plist-get item :address)
            :runtime runtime
            :session ref))))

;;;###autoload
(defun harmless-team-adopt (rig pod seat runtime ref)
  "Bind an already running session to SEAT in POD of RIG.
RUNTIME is `harmless', `claude', `codex', `pi', or `terminal'.
REF is that session.  This does not start it.  Return the seat
plist."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Pod: ")
         (read-string "Seat: ")
         (read-string "Runtime: ")
         (read-string "Session: ")))
  (let ((saved (harmless-team--adopt rig pod seat runtime ref)))
    (message "Adopted %s" (plist-get saved :address))
    saved))

;;;###autoload
(defun harmless-team-release (rig pod seat)
  "Clear the session bound to SEAT in POD of RIG.
The session keeps running.  The name, the role, and the inbound
work stay.  Return the binding that was cleared."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Pod: ")
         (read-string "Seat: ")))
  (let ((released (harmless-team--release rig pod seat)))
    (message "Released %s" (plist-get released :released))
    released))

(declare-function harmless-session-get "harmless-session" (id))
(declare-function harmless-session-p "harmless-session" (object))
(declare-function harmless-session-status "harmless-session" (session))
(declare-function harmless-usage-context-window "harmless-usage" (model))
(declare-function harmless-session-append-user "harmless-session"
                  (session text))

(defun harmless-team--send-text (text)
  "Return TEXT trimmed, or signal.
Internal newlines stay."
  (cond
   ((null text) (error "team send is empty"))
   ((not (stringp text))
    (error "team send must be a string")))
  (setq text (string-trim text))
  (if (string-empty-p text)
      (error "team send is empty")
    text))

(defun harmless-team--screen-block (text)
  "Signal when TEXT's last line is a prompt or a permission.
A last line of \">\", or a last line that ends in \">\", is a
prompt.  \"Allow this tool?\" is a permission question."
  (let ((line nil))
    (dolist (one (split-string (or text "") "\n"))
      (setq one (string-trim one))
      (unless (string-empty-p one)
        (setq line one)))
    (cond
     ((null line) nil)
     ((or (equal line ">") (string-suffix-p ">" line))
      (error "team seat is at a prompt"))
     ((equal line "Allow this tool?")
      (error "team seat is at a permission question")))))

(defun harmless-team--read-text (path)
  "Return PATH as text."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (buffer-string)))

(defun harmless-team--harmless-block (dir id)
  "Signal when the Harmless session ID in DIR cannot be typed on.
A live session is checked first.  The summary on disk is checked
next.  This does not start a turn."
  (when (and (fboundp 'harmless-session-get)
             (fboundp 'harmless-session-p)
             (fboundp 'harmless-session-status))
    (let ((live (harmless-session-get id)))
      (when (and live (harmless-session-p live))
        (cond
         ((eq (harmless-session-status live) 'waiting-permission)
          (error "team seat is at a permission question"))
         ((eq (harmless-session-status live) 'streaming)
          (error "team seat is at a prompt"))))))
  (let ((path (expand-file-name "summary.json" dir)))
    (unless (file-regular-p path)
      (error "team session is missing"))
    (let ((obj (harmless-json-decode-safe
                (harmless-team--read-text path))))
      (unless (harmless-plist-p obj)
        (error "team session is not valid"))
      (cond
       ((equal (plist-get obj :status) "waiting-permission")
        (error "team seat is at a permission question"))
       ((equal (plist-get obj :status) "streaming")
        (error "team seat is at a prompt"))))))

(defun harmless-team--screen-file (seat)
  "Return SEAT's screen file, or nil when it has no file.
An empty seat signals \"team seat is not occupied\"."
  (let ((runtime (plist-get seat :runtime))
        (ref (plist-get seat :session)))
    (cond
     ((or (null ref) (and (stringp ref) (string-empty-p ref)))
      (error "team seat is not occupied"))
     ((or (null runtime) (equal runtime "harmless"))
      (expand-file-name
       "messages.jsonl"
       (harmless-team--harmless-session ref)))
     ((and (equal runtime "terminal")
           (string-match-p "\\`[0-9]+\\'" ref))
      nil)
     ((and (stringp ref) (file-directory-p ref))
      (let ((screen (expand-file-name "screen.txt" ref)))
        (when (and (file-regular-p screen)
                   (not (harmless-team--root-text-p screen))
                   (not (equal (file-truename screen)
                               (file-truename "/"))))
          screen)))
     ((and (stringp ref)
           (file-regular-p ref)
           (not (harmless-team--root-text-p ref))
           (not (equal (file-truename ref) (file-truename "/"))))
      ref))))

(defun harmless-team--accept-send (seat)
  "Return SEAT's screen file when a send may type there."
  (let ((path (harmless-team--screen-file seat)))
    (unless path
      (error "team seat has no screen"))
    (if (or (null (plist-get seat :runtime))
            (equal (plist-get seat :runtime) "harmless"))
        (harmless-team--harmless-block
         (file-name-directory path)
         (plist-get seat :session))
      (harmless-team--screen-block (harmless-team--read-text path)))
    path))

(defun harmless-team--append-screen (path text json)
  "Append TEXT to PATH and check that it is there.
JSON non-nil writes one user message.  PATH is not a root."
  (let ((payload (if json
                     (concat (harmless-json-text
                              (list :role :user :content text))
                             "\n")
                   (concat text "\n")))
        (parent (directory-file-name
                 (file-name-directory path))))
    (unless (and (file-name-absolute-p path)
                 (not (harmless-team--root-text-p path))
                 (not (harmless-team--root-text-p parent))
                 (not (equal (file-truename parent)
                             (file-truename "/"))))
      (error "Refusing to store a team at %s" path))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region payload nil path t 'silent))
    (unless (string-suffix-p payload (harmless-team--read-text path))
      (error "team send did not appear"))))

(defun harmless-team--all-seats (rig)
  "Return RIG's seats in file order."
  (let ((out nil))
    (dolist (pod (plist-get rig :pods))
      (dolist (seat (plist-get pod :seats))
        (setq out (append out (list seat)))))
    out))

(defun harmless-team--seat-by-address (rig address)
  "Return the seat in RIG with ADDRESS, or nil."
  (let ((found nil))
    (dolist (seat (harmless-team--all-seats rig))
      (when (and (not found)
                 (harmless-team--same-p
                  (plist-get seat :address) address))
        (setq found seat)))
    found))

(defun harmless-team--send-targets (rig target)
  "Return the seats TARGET names, in file order.
TARGET is one seat, a comma-separated list, a pod, the rig
name, or \"*\"."
  (cond
   ((null target) (error "team send is empty"))
   ((not (stringp target))
    (error "team send must be a string")))
  (setq target (string-trim target))
  (cond
   ((string-empty-p target) (error "team send is empty"))
   ((equal target "*")
    (or (harmless-team--all-seats rig)
        (error "team send is not a target")))
   ((string-search "," target)
    (let ((seats nil))
      (dolist (one (split-string target "," t "[ \t\n\r]+"))
        (let ((seat (harmless-team--seat-by-address
                     rig (harmless-team--endpoint rig one))))
          (unless seat
            (error "team seat is missing"))
          (setq seats (append seats (list seat)))))
      (or seats (error "team send is not a target"))))
   ((or (string-match-p
         "\\`[A-Za-z0-9]+\\.[A-Za-z0-9]+\\'" target)
        (string-match-p
         "\\`[A-Za-z0-9]+-[A-Za-z0-9]+@.+" target))
    (let ((seat (harmless-team--seat-by-address
                 rig (harmless-team--endpoint rig target))))
      (unless seat
        (error "team seat is missing"))
      (list seat)))
   ((harmless-team--find-named (plist-get rig :pods) target)
    (or (plist-get (harmless-team--find-named
                    (plist-get rig :pods) target)
                   :seats)
        (error "team send is not a target")))
   ((harmless-team--same-p target (plist-get rig :name))
    (or (harmless-team--all-seats rig)
        (error "team send is not a target")))
   (t (error "team send is not a target"))))

(defun harmless-team--public-line (item)
  "Return one transcript ITEM with keys in a fixed order."
  (list :seat (plist-get item :seat)
        :runtime (plist-get item :runtime)
        :session (plist-get item :session)
        :text (plist-get item :text)))

(defun harmless-team--lines (rig seat)
  "Return RIG's transcript lines for SEAT's address."
  (let ((address (plist-get seat :address))
        (out nil))
    (dolist (item (plist-get rig :transcript))
      (when (harmless-team--same-p (plist-get item :seat) address)
        (setq out (append out (list (harmless-team--public-line item))))))
    out))

(defun harmless-team--send (rig target text)
  "Type TEXT onto TARGET in RIG and return the addresses.
A blocked seat refuses the whole send before any screen
changes.  This does not start a turn."
  (setq rig (harmless-team--rig-name rig))
  (setq text (harmless-team--send-text text))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (seats (harmless-team--send-targets stored target))
         (plans nil))
    (dolist (seat seats)
      (setq plans
            (append plans
                    (list (list seat
                                (harmless-team--accept-send seat))))))
    (dolist (plan plans)
      (let ((seat (car plan)))
        (harmless-team--append-screen
         (cadr plan) text
         (or (null (plist-get seat :runtime))
             (equal (plist-get seat :runtime) "harmless")))))
    (let ((lines (plist-get stored :transcript)))
      (dolist (plan plans)
        (let ((seat (car plan)))
          (setq lines
                (append lines
                        (list (list :seat (plist-get seat :address)
                                    :runtime (plist-get seat :runtime)
                                    :session (plist-get seat :session)
                                    :text text))))))
      (plist-put stored :transcript lines))
    (harmless-team--write (car loaded) stored)
    (mapcar (lambda (plan)
              (plist-get (car plan) :address))
            plans)))

(defun harmless-team--capture (rig pod seat)
  "Return the screen text of SEAT in POD of RIG."
  (setq rig (harmless-team--rig-name rig))
  (setq pod (harmless-team--pod-name pod))
  (setq seat (harmless-team--seat-name seat))
  (let* ((stored (cdr (harmless-team--load rig)))
         (found (harmless-team--find stored pod seat)))
    (unless found
      (error "team seat is missing"))
    (let ((path (harmless-team--screen-file (cdr found))))
      (if path
          (if (and (file-regular-p path)
                   (not (equal (file-truename path)
                               (file-truename "/"))))
              (harmless-team--read-text path)
            "")
        (mapconcat (lambda (line) (plist-get line :text))
                   (harmless-team--lines stored (cdr found))
                   "\n")))))

(defun harmless-team--transcript (rig pod seat)
  "Return every recorded line for SEAT, oldest first.
Lines from an earlier occupant stay."
  (setq rig (harmless-team--rig-name rig))
  (setq pod (harmless-team--pod-name pod))
  (setq seat (harmless-team--seat-name seat))
  (let* ((stored (cdr (harmless-team--load rig)))
         (found (harmless-team--find stored pod seat))
         (address (if found
                      (plist-get (cdr found) :address)
                    (harmless-team-address rig pod seat))))
    (let ((out nil))
      (dolist (item (plist-get stored :transcript))
        (when (harmless-team--same-p (plist-get item :seat) address)
          (setq out (append out
                            (list (harmless-team--public-line item))))))
      (when (and (null out) (null found))
        (error "team seat is missing"))
      out)))

;;;###autoload
(defun harmless-team-send (rig target text)
  "Type TEXT onto TARGET in RIG.
TARGET is one seat, a comma-separated list of seats, a pod, the
rig name, or \"*\".  A seat at a prompt or a permission question
refuses the whole send.  Return the addresses that received it."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Target: ")
         (read-string "Text: ")))
  (let ((sent (harmless-team--send rig target text)))
    (message "Sent to %s" (mapconcat #'identity sent ", "))
    sent))

;;;###autoload
(defun harmless-team-capture (rig pod seat)
  "Return the text on SEAT's screen in POD of RIG."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Pod: ")
         (read-string "Seat: ")))
  (let ((text (harmless-team--capture rig pod seat)))
    (message "Captured %s" (plist-get
                            (harmless-team--lookup rig pod seat)
                            :address))
    text))

;;;###autoload
(defun harmless-team-transcript (rig pod seat)
  "Return what SEAT has been sent, across every occupant.
Each line is :seat, :runtime, :session, and :text."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Pod: ")
         (read-string "Seat: ")))
  (let ((lines (harmless-team--transcript rig pod seat)))
    (message "%d lines" (length lines))
    lines))

(defvar harmless-team-room-wait-seconds 10
  "Seconds a room wait looks for a new line.
Zero checks once and does not wait.")

(defun harmless-team--room-topic (topic)
  "Return TOPIC trimmed, or signal."
  (harmless-team--token
   topic "team room topic is empty"
   "team room topic must be a string"
   #'harmless-team--member-name-p
   "team room topic is not a name"))

(defun harmless-team--room-speaker (speaker)
  "Return SPEAKER trimmed, or signal."
  (cond
   ((null speaker) (error "team room speaker is empty"))
   ((not (stringp speaker))
    (error "team room speaker must be a string")))
  (setq speaker (string-trim speaker))
  (if (string-empty-p speaker)
      (error "team room speaker is empty")
    speaker))

(defun harmless-team--room-text (text)
  "Return TEXT trimmed, or signal.
Internal newlines stay."
  (cond
   ((null text) (error "team room text is empty"))
   ((not (stringp text))
    (error "team room text must be a string")))
  (setq text (string-trim text))
  (if (string-empty-p text)
      (error "team room text is empty")
    text))

(defun harmless-team--public-room (item)
  "Return one room ITEM with keys in a fixed order."
  (list :topic (plist-get item :topic)
        :speaker (plist-get item :speaker)
        :text (plist-get item :text)
        :at (plist-get item :at)))

(defun harmless-team--room-lines (rig topic)
  "Return RIG's room lines.
TOPIC nil returns every topic.  Otherwise only that topic."
  (let ((out nil))
    (dolist (item (plist-get rig :room))
      (when (or (null topic)
                (harmless-team--same-p
                 (plist-get item :topic) topic))
        (setq out (append out
                          (list (harmless-team--public-room item))))))
    out))

(defun harmless-team--room-say (rig topic speaker text)
  "Append TEXT from SPEAKER to TOPIC in RIG and return the line."
  (setq rig (harmless-team--rig-name rig))
  (setq topic (harmless-team--room-topic topic))
  (setq speaker (harmless-team--room-speaker speaker))
  (setq text (harmless-team--room-text text))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (line (list :topic topic
                     :speaker speaker
                     :text text
                     :at (harmless-now-iso))))
    (plist-put stored :room
               (append (plist-get stored :room) (list line)))
    (harmless-team--write (car loaded) stored)
    (harmless-team--public-room line)))

(defun harmless-team--room-history (rig &optional topic)
  "Return RIG's room lines, oldest first.
TOPIC limits the lines.  Nil returns every topic."
  (setq rig (harmless-team--rig-name rig))
  (when topic
    (setq topic (harmless-team--room-topic topic)))
  (harmless-team--room-lines
   (cdr (harmless-team--load rig)) topic))

(defun harmless-team--room-wait (rig topic)
  "Return the next line said in TOPIC of RIG.
Lines already stored do not count.  This looks until someone
speaks, or until `harmless-team-room-wait-seconds' runs out."
  (setq rig (harmless-team--rig-name rig))
  (setq topic (harmless-team--room-topic topic))
  (let* ((limit (if (numberp harmless-team-room-wait-seconds)
                    harmless-team-room-wait-seconds
                  10))
         (seen (length (harmless-team--room-lines
                        (cdr (harmless-team--load rig)) topic)))
         (deadline (+ (float-time) (if (> limit 0) limit 0)))
         (found nil))
    (while (not found)
      (let ((lines (harmless-team--room-lines
                    (cdr (harmless-team--load rig)) topic)))
        (when (> (length lines) seen)
          (setq found (nth seen lines))))
      (unless found
        (when (or (<= limit 0) (>= (float-time) deadline))
          (error "team room is still quiet"))
        (accept-process-output nil 0.05)))
    found))

;;;###autoload
(defun harmless-team-room-say (rig topic speaker text)
  "Append TEXT from SPEAKER to TOPIC in RIG's room.
Return that line.  This does not start a turn."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Topic: ")
         (read-string "Speaker: ")
         (read-string "Text: ")))
  (let ((line (harmless-team--room-say rig topic speaker text)))
    (message "Said %s" (plist-get line :topic))
    line))

;;;###autoload
(defun harmless-team-room-history (rig &optional topic)
  "Return RIG's room lines, oldest first.
TOPIC limits the lines.  Nil returns every topic."
  (interactive
   (list (read-string "Rig: ")
         (let ((answer (read-string "Topic: ")))
           (if (string-empty-p (string-trim answer))
               nil
             answer))))
  (let ((lines (harmless-team--room-history rig topic)))
    (message "%d lines" (length lines))
    lines))

;;;###autoload
(defun harmless-team-room-wait (rig topic)
  "Return the next line said in TOPIC of RIG.
Lines already in the room do not count.  The wait stops if
nobody speaks within `harmless-team-room-wait-seconds'."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Topic: ")))
  (let ((line (harmless-team--room-wait rig topic)))
    (message "Heard %s" (plist-get line :topic))
    line))

(defun harmless-team--pace-name (name)
  "Return NAME trimmed, or signal."
  (harmless-team--token
   name "team pace is empty" "team pace must be a string"
   #'harmless-team--member-name-p "team pace is not a name"))

(defun harmless-team--pace-text (text)
  "Return TEXT trimmed, or signal.
Internal newlines stay."
  (cond
   ((null text) (error "team pace text is empty"))
   ((not (stringp text))
    (error "team pace text must be a string")))
  (setq text (string-trim text))
  (if (string-empty-p text)
      (error "team pace text is empty")
    text))

(defun harmless-team--public-pace (item)
  "Return one pace ITEM with keys in a fixed order."
  (let ((pieces (plist-get item :pieces))
        (cursor (plist-get item :cursor)))
    (list :name (plist-get item :name)
          :seat (plist-get item :seat)
          :pieces pieces
          :cursor cursor
          :left (nthcdr cursor pieces))))

(defun harmless-team--pace-open (rig name seat)
  "Create an empty pace NAME aimed at SEAT in RIG."
  (setq rig (harmless-team--rig-name rig))
  (setq name (harmless-team--pace-name name))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (address (harmless-team--endpoint stored seat)))
    (when (harmless-team--find-named (plist-get stored :paces) name)
      (error "team pace already exists"))
    (let ((pace (list :name name :seat address
                      :pieces nil :cursor 0)))
      (plist-put stored :paces
                 (append (plist-get stored :paces) (list pace)))
      (harmless-team--write (car loaded) stored)
      (harmless-team--public-pace pace))))

(defun harmless-team--pace-add (rig name text)
  "Append TEXT to pace NAME in RIG."
  (setq rig (harmless-team--rig-name rig))
  (setq name (harmless-team--pace-name name))
  (setq text (harmless-team--pace-text text))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (pace (harmless-team--find-named
                (plist-get stored :paces) name)))
    (unless pace
      (error "team pace is missing"))
    (plist-put pace :pieces
               (append (plist-get pace :pieces) (list text)))
    (harmless-team--write (car loaded) stored)
    (harmless-team--public-pace pace)))

(defun harmless-team--pace-show (rig name)
  "Return pace NAME in RIG without sending."
  (setq rig (harmless-team--rig-name rig))
  (setq name (harmless-team--pace-name name))
  (let ((pace (harmless-team--find-named
               (plist-get (cdr (harmless-team--load rig)) :paces)
               name)))
    (unless pace
      (error "team pace is missing"))
    (harmless-team--public-pace pace)))

(defun harmless-team--pace-step (rig name)
  "Send the next piece of pace NAME and return the pace.
A refusal leaves the pace where it was."
  (setq rig (harmless-team--rig-name rig))
  (setq name (harmless-team--pace-name name))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (pace (harmless-team--find-named
                (plist-get stored :paces) name)))
    (unless pace
      (error "team pace is missing"))
    (let ((cursor (plist-get pace :cursor))
          (pieces (plist-get pace :pieces)))
      (when (>= cursor (length pieces))
        (error "team pace is finished"))
      (harmless-team--send rig (plist-get pace :seat)
                           (nth cursor pieces))
      (let* ((again (harmless-team--load rig))
             (fresh (harmless-team--find-named
                     (plist-get (cdr again) :paces) name)))
        (unless fresh
          (error "team pace is missing"))
        (plist-put fresh :cursor (1+ (plist-get fresh :cursor)))
        (harmless-team--write (car again) (cdr again))
        (harmless-team--public-pace fresh)))))

;;;###autoload
(defun harmless-team-pace (rig name seat)
  "Start pace NAME in RIG, aimed at SEAT.
The pace has no pieces yet.  Return it."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Pace: ")
         (read-string "Seat: ")))
  (let ((pace (harmless-team--pace-open rig name seat)))
    (message "Paced %s" (plist-get pace :name))
    pace))

;;;###autoload
(defun harmless-team-pace-add (rig name text)
  "Append TEXT as the next piece of pace NAME in RIG.
Return the pace."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Pace: ")
         (read-string "Text: ")))
  (let ((pace (harmless-team--pace-add rig name text)))
    (message "Added to %s" (plist-get pace :name))
    pace))

;;;###autoload
(defun harmless-team-pace-show (rig name)
  "Return pace NAME in RIG without sending a piece."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Pace: ")))
  (harmless-team--pace-show rig name))

;;;###autoload
(defun harmless-team-pace-step (rig name)
  "Send the next piece of pace NAME in RIG to its seat.
One call sends one piece.  Return the pace."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Pace: ")))
  (let ((pace (harmless-team--pace-step rig name)))
    (message "Stepped %s" (plist-get pace :name))
    pace))

(defun harmless-team--row-body (text)
  "Return TEXT trimmed, or signal.
Internal newlines stay."
  (cond
   ((null text) (error "team row body is empty"))
   ((not (stringp text))
    (error "team row body must be a string")))
  (setq text (string-trim text))
  (if (string-empty-p text)
      (error "team row body is empty")
    text))

(defun harmless-team--row-id (id)
  "Return ID trimmed, or signal."
  (cond
   ((null id) (error "team row is empty"))
   ((not (stringp id)) (error "team row must be a string")))
  (setq id (string-trim id))
  (cond
   ((string-empty-p id) (error "team row is empty"))
   ((not (harmless-team--row-id-p id))
    (error "team row is not an id"))
   (t id)))

(defun harmless-team--public-history (item)
  "Return one history ITEM with keys in a fixed order."
  (list :at (plist-get item :at)
        :text (plist-get item :text)))

(defun harmless-team--public-row (item)
  "Return one queue row with keys in a fixed order."
  (list :id (plist-get item :id)
        :seat (plist-get item :seat)
        :body (plist-get item :body)
        :state (plist-get item :state)
        :history (mapcar #'harmless-team--public-history
                         (plist-get item :history))
        :reason (plist-get item :reason)
        :wake (plist-get item :wake)
        :summary (plist-get item :summary)
        :evidence (plist-get item :evidence)))

(defun harmless-team--next-row-id (rows)
  "Return the next row id after ROWS."
  (let ((n 0))
    (dolist (row rows)
      (let ((id (string-to-number (or (plist-get row :id) "0"))))
        (when (> id n)
          (setq n id))))
    (number-to-string (1+ n))))

(defun harmless-team--row-open (rig seat body)
  "Open a queue row for SEAT in RIG and return it.
The row stays on the rig.  Replacing the seat does not drop it."
  (setq rig (harmless-team--rig-name rig))
  (setq body (harmless-team--row-body body))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (address (harmless-team--endpoint stored seat))
         (row (list :id (harmless-team--next-row-id
                         (plist-get stored :rows))
                    :seat address
                    :body body
                    :state "open"
                    :history (list (list :at (harmless-now-iso)
                                         :text "opened")))))
    (plist-put stored :rows
               (append (plist-get stored :rows) (list row)))
    (harmless-team--write (car loaded) stored)
    (harmless-team--public-row row)))

(defun harmless-team--rows (rig &optional seat)
  "Return RIG's queue rows, oldest first.
SEAT limits the rows.  Nil returns every row."
  (setq rig (harmless-team--rig-name rig))
  (let* ((stored (cdr (harmless-team--load rig)))
         (address (when seat
                    (harmless-team--endpoint stored seat)))
         (out nil))
    (dolist (row (plist-get stored :rows))
      (when (or (null address)
                (harmless-team--same-p
                 (plist-get row :seat) address))
        (setq out (append out (list (harmless-team--public-row row))))))
    out))

(defun harmless-team--row-read (rig id)
  "Return the queue row ID in RIG."
  (setq rig (harmless-team--rig-name rig))
  (setq id (harmless-team--row-id id))
  (let ((found nil))
    (dolist (row (harmless-team--rows rig))
      (when (and (not found) (equal (plist-get row :id) id))
        (setq found row)))
    (unless found
      (error "team row is missing"))
    found))

;;;###autoload
(defun harmless-team-row (rig seat body)
  "Open a queue row aimed at SEAT in RIG.
BODY is the work.  The state is open.  Return the row."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Seat: ")
         (read-string "Body: ")))
  (let ((row (harmless-team--row-open rig seat body)))
    (message "Opened %s" (plist-get row :id))
    row))

;;;###autoload
(defun harmless-team-rows (rig &optional seat)
  "Return RIG's queue rows, oldest first.
SEAT limits the rows.  Nil returns every row."
  (interactive
   (list (read-string "Rig: ")
         (let ((answer (read-string "Seat: ")))
           (if (string-empty-p (string-trim answer))
               nil
             answer))))
  (let ((rows (harmless-team--rows rig seat)))
    (message "%d rows" (length rows))
    rows))

;;;###autoload
(defun harmless-team-row-read (rig id)
  "Return queue row ID in RIG."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Row: ")))
  (let ((row (harmless-team--row-read rig id)))
    (message "Row %s" (plist-get row :id))
    row))

(defun harmless-team--row-note-text (text)
  "Return TEXT trimmed, or signal.
Internal newlines stay."
  (cond
   ((null text) (error "team row note is empty"))
   ((not (stringp text))
    (error "team row note must be a string")))
  (setq text (string-trim text))
  (if (string-empty-p text)
      (error "team row note is empty")
    text))

(defun harmless-team--row-reason (reason)
  "Return REASON trimmed, or signal."
  (cond
   ((null reason) (error "team row reason is empty"))
   ((not (stringp reason))
    (error "team row reason must be a string")))
  (setq reason (string-trim reason))
  (cond
   ((string-empty-p reason) (error "team row reason is empty"))
   ((not (member reason harmless-team--row-reasons))
    (error "team row reason is not a reason"))
   (t reason)))

(defun harmless-team--row-stored (stored id)
  "Return the row ID inside STORED, or signal."
  (let ((found nil))
    (dolist (row (plist-get stored :rows))
      (when (and (not found)
                 (equal (plist-get row :id) id))
        (setq found row)))
    (unless found
      (error "team row is missing"))
    found))

(defun harmless-team--row-append (row text)
  "Append TEXT to ROW's history and return ROW."
  (plist-put row :history
             (append (plist-get row :history)
                     (list (list :at (harmless-now-iso)
                                 :text text))))
  row)

(defun harmless-team--seat-text (text)
  "Return TEXT trimmed, or signal.
TEXT must be a seat reference."
  (cond
   ((null text) (error "team edge is empty"))
   ((not (stringp text))
    (error "team edge must be a string")))
  (setq text (string-trim text))
  (cond
   ((string-empty-p text) (error "team edge is empty"))
   ((or (string-match-p
         "\\`[A-Za-z0-9]+\\.[A-Za-z0-9]+\\'" text)
        (string-match-p
         "\\`[A-Za-z0-9]+-[A-Za-z0-9]+@.+" text))
    text)
   (t (error "team edge is not a seat"))))

(defun harmless-team--row-claim (rig id)
  "Hold open row ID in RIG and return it."
  (setq rig (harmless-team--rig-name rig))
  (setq id (harmless-team--row-id id))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (row (harmless-team--row-stored stored id))
         (state (plist-get row :state)))
    (cond
     ((equal state "held") (error "team row is held"))
     ((equal state "closed") (error "team row is closed"))
     ((equal state "parked") (error "team row is parked")))
    (plist-put row :state "held")
    (harmless-team--row-append row "claimed")
    (harmless-team--write (car loaded) stored)
    (harmless-team--public-row row)))

(defun harmless-team--row-note (rig id text)
  "Append TEXT to row ID in RIG and return it.
A closed row is refused."
  (setq text (harmless-team--row-note-text text))
  (setq rig (harmless-team--rig-name rig))
  (setq id (harmless-team--row-id id))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (row (harmless-team--row-stored stored id)))
    (when (equal (plist-get row :state) "closed")
      (error "team row is closed"))
    (harmless-team--row-append row text)
    (harmless-team--write (car loaded) stored)
    (harmless-team--public-row row)))

(defun harmless-team--workflow-owns-p (stored id)
  "Return non-nil if a workflow's current row is ID."
  (let ((found nil))
    (dolist (workflow (plist-get stored :workflows))
      (when (equal (plist-get workflow :row) id)
        (setq found t)))
    found))

(defun harmless-team--workflow-prepare (stored id)
  "Resolve the next seat of each workflow that owns row ID.
A missing seat signals before the row changes."
  (dolist (workflow (plist-get stored :workflows))
    (when (equal (plist-get workflow :row) id)
      (let* ((steps (plist-get workflow :steps))
             (next (1+ (plist-get workflow :index))))
        (when (< next (length steps))
          (harmless-team--endpoint
           stored (plist-get (nth next steps) :seat)))))))

(defun harmless-team--workflow-advance (stored workflow)
  "Open WORKFLOW's next step in STORED, or finish it.
Return the opened row, or nil when WORKFLOW is done."
  (let* ((steps (plist-get workflow :steps))
         (next (1+ (plist-get workflow :index))))
    (if (>= next (length steps))
        (progn
          (plist-put workflow :index next)
          (plist-put workflow :row nil)
          nil)
      (let* ((step (nth next steps))
             (address (harmless-team--endpoint
                       stored (plist-get step :seat)))
             (row (list :id (harmless-team--next-row-id
                             (plist-get stored :rows))
                        :seat address
                        :body (plist-get step :body)
                        :state "open"
                        :history
                        (list (list :at (harmless-now-iso)
                                    :text "opened")))))
        (plist-put stored :rows
                   (append (plist-get stored :rows) (list row)))
        (plist-put workflow :index next)
        (plist-put workflow :row (plist-get row :id))
        row))))

(defun harmless-team--row-close (rig id reason)
  "Close held row ID in RIG for REASON and return it.
REASON records delivery.  When the row is a workflow's current
step, the next step's row opens in the same write.  Acceptance
stays the next seat's verdict."
  (setq reason (harmless-team--row-reason reason))
  (setq rig (harmless-team--rig-name rig))
  (setq id (harmless-team--row-id id))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (row (harmless-team--row-stored stored id))
         (state (plist-get row :state)))
    (cond
     ((equal state "open") (error "team row is not held"))
     ((equal state "closed") (error "team row is closed"))
     ((equal state "parked") (error "team row is parked")))
    (harmless-team--workflow-prepare stored id)
    (plist-put row :state "closed")
    (plist-put row :reason reason)
    (harmless-team--row-append row "delivered")
    (dolist (workflow (plist-get stored :workflows))
      (when (equal (plist-get workflow :row) id)
        (harmless-team--workflow-advance stored workflow)))
    (harmless-team--write (car loaded) stored)
    (harmless-team--public-row row)))

;;;###autoload
(defun harmless-team-row-claim (rig id)
  "Hold open queue row ID in RIG.
The history records the claim.  Return the row."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Row: ")))
  (let ((row (harmless-team--row-claim rig id)))
    (message "Claimed %s" (plist-get row :id))
    row))

;;;###autoload
(defun harmless-team-row-note (rig id text)
  "Append TEXT to queue row ID in RIG.
A closed row is refused.  Return the row."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Row: ")
         (read-string "Note: ")))
  (let ((row (harmless-team--row-note rig id text)))
    (message "Noted %s" (plist-get row :id))
    row))

;;;###autoload
(defun harmless-team-row-close (rig id reason)
  "Close held queue row ID in RIG for REASON.
REASON records delivery.  A workflow's next step opens in the
same write.  Return the row."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Row: ")
         (read-string "Reason: ")))
  (let ((row (harmless-team--row-close rig id reason)))
    (message "Closed %s" (plist-get row :id))
    row))

(defun harmless-team--row-hand (rig id seat)
  "Hand held row ID in RIG to SEAT and return both rows.
The source closes and the destination opens in one write."
  (setq id (harmless-team--row-id id))
  (setq seat (harmless-team--seat-text seat))
  (setq rig (harmless-team--rig-name rig))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (address (harmless-team--endpoint stored seat))
         (row (harmless-team--row-stored stored id))
         (state (plist-get row :state))
         (opened nil))
    (cond
     ((equal state "open") (error "team row is not held"))
     ((equal state "closed") (error "team row is closed"))
     ((equal state "parked") (error "team row is parked")))
    (when (harmless-team--workflow-owns-p stored id)
      (error "team row is a workflow step"))
    (when (harmless-team--same-p address (plist-get row :seat))
      (error "team row hand is the same seat"))
    (plist-put row :state "closed")
    (plist-put row :reason "handed_off_to")
    (harmless-team--row-append row "delivered")
    (setq opened
          (list :id (harmless-team--next-row-id
                     (plist-get stored :rows))
                :seat address
                :body (plist-get row :body)
                :state "open"
                :history
                (list (list :at (harmless-now-iso)
                            :text "opened"))))
    (plist-put stored :rows
               (append (plist-get stored :rows) (list opened)))
    (harmless-team--write (car loaded) stored)
    (list :source (harmless-team--public-row row)
          :destination (harmless-team--public-row opened))))

;;;###autoload
(defun harmless-team-row-hand (rig id seat)
  "Hand held queue row ID in RIG to SEAT.
The source closes and the destination opens together.  A
workflow's current step is refused.  Return both rows."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Row: ")
         (read-string "Seat: ")))
  (let ((hand (harmless-team--row-hand rig id seat)))
    (message "Handed %s to %s"
             (plist-get (plist-get hand :source) :id)
             (plist-get (plist-get hand :destination) :id))
    hand))

(defun harmless-team--row-wake (text)
  "Return TEXT trimmed, or signal."
  (cond
   ((null text) (error "team row wake is empty"))
   ((not (stringp text))
    (error "team row wake must be a string")))
  (setq text (string-trim text))
  (if (string-empty-p text)
      (error "team row wake is empty")
    text))

(defun harmless-team--row-decision (text)
  "Return TEXT trimmed, or signal."
  (cond
   ((null text) (error "team row decision is empty"))
   ((not (stringp text))
    (error "team row decision must be a string")))
  (setq text (string-trim text))
  (if (string-empty-p text)
      (error "team row decision is empty")
    text))

(defun harmless-team--row-field (text noun)
  "Return TEXT trimmed, or nil when TEXT is blank.
NOUN names the field in an error.  A non-string signals."
  (cond
   ((null text) nil)
   ((not (stringp text))
    (error "team row %s must be a string" noun))
   (t
    (setq text (string-trim text))
    (if (string-empty-p text) nil text))))

(defun harmless-team--row-park (rig id wake summary evidence)
  "Park held row ID in RIG on one WAKE and return it.
SUMMARY and EVIDENCE together ask a person."
  (setq wake (harmless-team--row-wake wake))
  (setq summary (harmless-team--row-field summary "summary"))
  (setq evidence (harmless-team--row-field evidence "evidence"))
  (cond
   ((and summary (null evidence))
    (error "team row evidence is empty"))
   ((and evidence (null summary))
    (error "team row summary is empty")))
  (setq rig (harmless-team--rig-name rig))
  (setq id (harmless-team--row-id id))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (row (harmless-team--row-stored stored id))
         (state (plist-get row :state)))
    (cond
     ((equal state "open") (error "team row is not held"))
     ((equal state "closed") (error "team row is closed"))
     ((equal state "parked") (error "team row is parked")))
    (plist-put row :state "parked")
    (plist-put row :wake wake)
    (harmless-team--row-append row "parked")
    (harmless-team--row-append row wake)
    (when summary
      (plist-put row :summary summary)
      (plist-put row :evidence evidence)
      (harmless-team--row-append row summary)
      (harmless-team--row-append row evidence))
    (harmless-team--write (car loaded) stored)
    (harmless-team--public-row row)))

(defun harmless-team--row-resolve (rig id decision)
  "Record DECISION on parked row ID and return it to held.
The wake is cleared.  The decision stays in the history."
  (setq decision (harmless-team--row-decision decision))
  (setq rig (harmless-team--rig-name rig))
  (setq id (harmless-team--row-id id))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (row (harmless-team--row-stored stored id)))
    (unless (equal (plist-get row :state) "parked")
      (error "team row is not parked"))
    (plist-put row :state "held")
    (plist-put row :wake nil)
    (plist-put row :summary nil)
    (plist-put row :evidence nil)
    (harmless-team--row-append row decision)
    (harmless-team--write (car loaded) stored)
    (harmless-team--public-row row)))

;;;###autoload
(defun harmless-team-row-park (rig id wake &optional summary evidence)
  "Park held queue row ID in RIG on one WAKE.
SUMMARY and EVIDENCE together ask a person.  Return the row."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Row: ")
         (read-string "Wake: ")
         (let ((answer (read-string "Summary: ")))
           (if (string-empty-p (string-trim answer))
               nil
             answer))
         (let ((answer (read-string "Evidence: ")))
           (if (string-empty-p (string-trim answer))
               nil
             answer))))
  (let ((row (harmless-team--row-park rig id wake summary evidence)))
    (message "Parked %s" (plist-get row :id))
    row))

;;;###autoload
(defun harmless-team-row-resolve (rig id decision)
  "Record DECISION for parked queue row ID in RIG.
The row returns to held.  Return the row."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Row: ")
         (read-string "Decision: ")))
  (let ((row (harmless-team--row-resolve rig id decision)))
    (message "Resolved %s" (plist-get row :id))
    row))

(defun harmless-team--intake-body (text)
  "Return TEXT trimmed, or signal.
Internal newlines stay."
  (cond
   ((null text) (error "team intake is empty"))
   ((not (stringp text))
    (error "team intake must be a string")))
  (setq text (string-trim text))
  (if (string-empty-p text)
      (error "team intake is empty")
    text))

(defun harmless-team--public-intake (item)
  "Return one intake item with keys in a fixed order."
  (list :id (plist-get item :id)
        :body (plist-get item :body)
        :at (plist-get item :at)))

(defun harmless-team--intake-add (rig body)
  "Append BODY to RIG's intake and return it.
The observation has no seat."
  (setq rig (harmless-team--rig-name rig))
  (setq body (harmless-team--intake-body body))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (item (list :id (harmless-team--next-row-id
                          (plist-get stored :intake))
                     :body body
                     :at (harmless-now-iso))))
    (plist-put stored :intake
               (append (plist-get stored :intake) (list item)))
    (harmless-team--write (car loaded) stored)
    (harmless-team--public-intake item)))

(defun harmless-team--intakes (rig)
  "Return RIG's intake, oldest first."
  (setq rig (harmless-team--rig-name rig))
  (mapcar #'harmless-team--public-intake
          (plist-get (cdr (harmless-team--load rig)) :intake)))

(defun harmless-team--board-view (view)
  "Return VIEW trimmed, or signal."
  (cond
   ((null view) (error "team board is empty"))
   ((not (stringp view))
    (error "team board must be a string")))
  (setq view (string-trim view))
  (cond
   ((string-empty-p view) (error "team board is empty"))
   ((not (member view '("held" "stalled" "waiting")))
    (error "team board is not a view"))
   (t view)))

(defun harmless-team--row-stalled-p (row)
  "Return non-nil if ROW is held and nothing follows its claim."
  (and (equal (plist-get row :state) "held")
       (let ((claimed nil)
             (after nil))
         (dolist (item (plist-get row :history))
           (if claimed
               (setq after t)
             (when (equal (plist-get item :text) "claimed")
               (setq claimed t))))
         (not after))))

(defun harmless-team--row-waiting-p (row)
  "Return non-nil if ROW is parked and asks a person."
  (and (equal (plist-get row :state) "parked")
       (harmless-team--row-plain-p (plist-get row :summary))
       (harmless-team--row-plain-p (plist-get row :evidence))))

(defun harmless-team--board (rig view)
  "Return RIG's rows for VIEW, oldest first.
VIEW is `held', `stalled', or `waiting'."
  (setq view (harmless-team--board-view view))
  (setq rig (harmless-team--rig-name rig))
  (let ((out nil))
    (dolist (row (plist-get (cdr (harmless-team--load rig)) :rows))
      (when (cond
             ((equal view "held")
              (member (plist-get row :state) '("held" "parked")))
             ((equal view "stalled")
              (harmless-team--row-stalled-p row))
             (t (harmless-team--row-waiting-p row)))
        (setq out (append out (list (harmless-team--public-row row))))))
    out))

;;;###autoload
(defun harmless-team-intake (rig body)
  "Record BODY on RIG's intake and return it.
The observation has no owner and no seat."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Observation: ")))
  (let ((item (harmless-team--intake-add rig body)))
    (message "Intake %s" (plist-get item :id))
    item))

;;;###autoload
(defun harmless-team-intakes (rig)
  "Return RIG's intake, oldest first."
  (interactive (list (read-string "Rig: ")))
  (let ((items (harmless-team--intakes rig)))
    (message "%d intakes" (length items))
    items))

;;;###autoload
(defun harmless-team-board (rig view)
  "Return RIG's rows for VIEW.
VIEW is `held', `stalled', or `waiting'."
  (interactive
   (list (read-string "Rig: ")
         (read-string "View: ")))
  (let ((rows (harmless-team--board rig view)))
    (message "%d rows" (length rows))
    rows))

(defun harmless-team--workflow-name (name)
  "Return NAME trimmed, or signal."
  (harmless-team--token
   name "team workflow is empty" "team workflow must be a string"
   #'harmless-team--member-name-p "team workflow is not a name"))

(defun harmless-team--workflow-body (text)
  "Return TEXT trimmed, or signal.
Internal newlines stay."
  (cond
   ((null text) (error "team workflow body is empty"))
   ((not (stringp text))
    (error "team workflow body must be a string")))
  (setq text (string-trim text))
  (if (string-empty-p text)
      (error "team workflow body is empty")
    text))

(defun harmless-team--workflow-steps (steps)
  "Return STEPS checked, or signal.
Each step has a seat and a body.  Seats are not resolved yet."
  (cond
   ((null steps) (error "team workflow has no steps"))
   ((not (listp steps))
    (error "team workflow steps must be a list")))
  (let ((out nil)
        (items steps))
    (while (consp items)
      (let ((step (car items)))
        (unless (and (harmless-plist-p step)
                     (plist-member step :seat)
                     (plist-member step :body))
          (error "team workflow step is not a step"))
        (setq out
              (append out
                      (list (list :seat (harmless-team--seat-text
                                         (plist-get step :seat))
                                  :body (harmless-team--workflow-body
                                         (plist-get step :body))))))
        (setq items (cdr items))))
    (unless (null items)
      (error "team workflow steps must be a list"))
    out))

(defun harmless-team--public-step (step)
  "Return one workflow STEP with keys in a fixed order."
  (list :seat (plist-get step :seat)
        :body (plist-get step :body)))

(defun harmless-team--public-workflow (item)
  "Return one workflow ITEM with keys in a fixed order."
  (list :id (plist-get item :id)
        :name (plist-get item :name)
        :steps (mapcar #'harmless-team--public-step
                       (plist-get item :steps))
        :index (plist-get item :index)
        :row (plist-get item :row)))

(defun harmless-team--workflow-add (rig name steps)
  "Declare workflow NAME in RIG and open its first step.
STEPS is a list of plists with :seat and :body.  Return the
workflow and that row."
  (setq name (harmless-team--workflow-name name))
  (setq steps (harmless-team--workflow-steps steps))
  (setq rig (harmless-team--rig-name rig))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (resolved nil))
    (when (harmless-team--find-named
           (plist-get stored :workflows) name)
      (error "team workflow already exists"))
    (dolist (step steps)
      (setq resolved
            (append resolved
                    (list (list :seat (harmless-team--endpoint
                                       stored (plist-get step :seat))
                                :body (plist-get step :body))))))
    (let* ((row-id (harmless-team--next-row-id
                    (plist-get stored :rows)))
           (first (car resolved))
           (row (list :id row-id
                      :seat (plist-get first :seat)
                      :body (plist-get first :body)
                      :state "open"
                      :history (list (list :at (harmless-now-iso)
                                           :text "opened"))))
           (workflow (list :id (harmless-team--next-row-id
                                (plist-get stored :workflows))
                           :name name
                           :steps resolved
                           :index 0
                           :row row-id)))
      (plist-put stored :rows
                 (append (plist-get stored :rows) (list row)))
      (plist-put stored :workflows
                 (append (plist-get stored :workflows)
                         (list workflow)))
      (harmless-team--write (car loaded) stored)
      (list :workflow (harmless-team--public-workflow workflow)
            :row (harmless-team--public-row row)))))

(defun harmless-team--workflows (rig)
  "Return RIG's workflows, oldest first."
  (setq rig (harmless-team--rig-name rig))
  (mapcar #'harmless-team--public-workflow
          (plist-get (cdr (harmless-team--load rig)) :workflows)))

(defun harmless-team--workflow-read-steps ()
  "Read workflow steps until the seat is blank."
  (let ((steps nil)
        (seat (read-string "Seat: ")))
    (while (not (string-empty-p (string-trim seat)))
      (setq steps
            (append steps
                    (list (list :seat seat
                                :body (read-string "Body: "))))
            seat (read-string "Seat: ")))
    steps))

;;;###autoload
(defun harmless-team-workflow (rig name steps)
  "Declare workflow NAME in RIG and open its first step.
STEPS is a list of plists with :seat and :body.  Closing the
current step opens the next one.  Return both."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Name: ")
         (harmless-team--workflow-read-steps)))
  (let ((declared (harmless-team--workflow-add rig name steps)))
    (message "Declared %s"
             (plist-get (plist-get declared :workflow) :name))
    declared))

;;;###autoload
(defun harmless-team-workflows (rig)
  "Return RIG's workflows, oldest first."
  (interactive (list (read-string "Rig: ")))
  (let ((items (harmless-team--workflows rig)))
    (message "%d workflows" (length items))
    items))

(defun harmless-team--watchdog-kind (kind)
  "Return KIND trimmed, or signal."
  (cond
   ((null kind) (error "team watchdog is empty"))
   ((not (stringp kind))
    (error "team watchdog must be a string")))
  (setq kind (string-trim kind))
  (cond
   ((string-empty-p kind) (error "team watchdog is empty"))
   ((not (harmless-team--watchdog-kind-p kind))
    (error "team watchdog is not a kind"))
   (t kind)))

(defun harmless-team--watchdog-text (text)
  "Return TEXT trimmed, or signal.
Internal newlines stay."
  (cond
   ((null text) (error "team watchdog text is empty"))
   ((not (stringp text))
    (error "team watchdog text must be a string")))
  (setq text (string-trim text))
  (if (string-empty-p text)
      (error "team watchdog text is empty")
    text))

(defun harmless-team--watchdog-due (text)
  "Return TEXT trimmed, or signal."
  (cond
   ((null text) (error "team watchdog due is empty"))
   ((not (stringp text))
    (error "team watchdog due must be a string")))
  (setq text (string-trim text))
  (cond
   ((string-empty-p text) (error "team watchdog due is empty"))
   ((not (harmless-team--watchdog-due-p text))
    (error "team watchdog due is not a time"))
   (t text)))

(defun harmless-team--watchdog-target (text)
  "Return TEXT trimmed, or signal."
  (harmless-team--token
   text "team watchdog target is empty"
   "team watchdog target must be a string"
   #'harmless-team--member-name-p
   "team watchdog target is not a name"))

(defun harmless-team--watchdog-percent (value)
  "Return VALUE, or signal when it is not a percent."
  (cond
   ((null value) (error "team watchdog percent is empty"))
   ((not (and (integerp value) (>= value 1) (<= value 100)))
    (error "team watchdog percent is not a percent"))
   (t value)))

(defun harmless-team--watchdog-pool (pool)
  "Return POOL trimmed, or signal.
The text is stored.  A path is not opened."
  (cond
   ((null pool) (error "team watchdog pool is empty"))
   ((not (listp pool))
    (error "team watchdog pool must be a list")))
  (let ((out nil)
        (items pool))
    (while (consp items)
      (let ((item (car items)))
        (cond
         ((null item) (error "team watchdog artifact is empty"))
         ((not (stringp item))
          (error "team watchdog artifact must be a string")))
        (setq item (string-trim item))
        (when (string-empty-p item)
          (error "team watchdog artifact is empty"))
        (setq out (append out (list item))
              items (cdr items))))
    (unless (null items)
      (error "team watchdog pool must be a list"))
    (when (null out)
      (error "team watchdog pool is empty"))
    out))

(defun harmless-team--watchdog-spec (kind spec)
  "Return SPEC checked for KIND, or signal.
Seats and workflows are not resolved yet."
  (unless (harmless-plist-p spec)
    (error "team watchdog spec is not a spec"))
  (cond
   ((equal kind "reminder")
    (list :seat (harmless-team--seat-text (plist-get spec :seat))
          :text (harmless-team--watchdog-text (plist-get spec :text))
          :due (harmless-team--watchdog-due (plist-get spec :due))))
   ((equal kind "artifacts")
    (list :pool (harmless-team--watchdog-pool (plist-get spec :pool))))
   ((equal kind "nudge")
    (list :target (harmless-team--watchdog-target
                   (plist-get spec :target))))
   ((equal kind "idle")
    (list :seat (harmless-team--seat-text (plist-get spec :seat))))
   (t
    (list :seat (harmless-team--seat-text (plist-get spec :seat))
          :percent (harmless-team--watchdog-percent
                    (plist-get spec :percent))))))

(defun harmless-team--public-watchdog (item)
  "Return one watchdog ITEM with keys in a fixed order."
  (list :id (plist-get item :id)
        :kind (plist-get item :kind)
        :seat (plist-get item :seat)
        :target (plist-get item :target)
        :text (plist-get item :text)
        :due (plist-get item :due)
        :pool (plist-get item :pool)
        :percent (plist-get item :percent)))

(defun harmless-team--seat-named (stored address)
  "Return the seat in STORED whose address is ADDRESS, or nil."
  (let ((found nil))
    (dolist (pod (plist-get stored :pods))
      (dolist (seat (plist-get pod :seats))
        (when (and (not found)
                   (harmless-team--same-p
                    (plist-get seat :address) address))
          (setq found seat))))
    found))

(defun harmless-team--watchdog-build (kind spec address target)
  "Return a stored watchdog for KIND from SPEC.
ADDRESS and TARGET are already resolved.  The id is filled
by the caller."
  (list :id nil
        :kind kind
        :seat address
        :target target
        :text (plist-get spec :text)
        :due (plist-get spec :due)
        :pool (plist-get spec :pool)
        :percent (plist-get spec :percent)))

(defun harmless-team--watchdog-add (rig kind spec)
  "Record one watchdog of KIND in RIG and return it.
SPEC is a plist.  The job is the rig file.  Nothing is armed."
  (setq kind (harmless-team--watchdog-kind kind))
  (setq spec (harmless-team--watchdog-spec kind spec))
  (setq rig (harmless-team--rig-name rig))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (address nil)
         (target nil))
    (when (plist-get spec :seat)
      (setq address (harmless-team--endpoint
                     stored (plist-get spec :seat))))
    (when (equal kind "nudge")
      (setq target (plist-get spec :target))
      (unless (harmless-team--find-named
               (plist-get stored :workflows) target)
        (error "team workflow is missing")))
    (let ((job (harmless-team--watchdog-build
                kind spec address target)))
      (plist-put job :id (harmless-team--next-row-id
                          (plist-get stored :watchdogs)))
      (plist-put stored :watchdogs
                 (append (plist-get stored :watchdogs) (list job)))
      (harmless-team--write (car loaded) stored)
      (harmless-team--public-watchdog job))))

(defun harmless-team--watchdogs (rig)
  "Return RIG's watchdogs, oldest first."
  (setq rig (harmless-team--rig-name rig))
  (mapcar #'harmless-team--public-watchdog
          (plist-get (cdr (harmless-team--load rig)) :watchdogs)))

(defun harmless-team--context-used (seat)
  "Return (USED . WINDOW) for SEAT, or nil.
This reads a summary already on disk.  It does not start a turn.
Only a Harmless session has a measurement."
  (let ((session (plist-get seat :session))
        (runtime (plist-get seat :runtime)))
    (when (and (equal runtime "harmless")
               (stringp session)
               (string-match-p harmless-team--session-re session))
      (condition-case nil
          (let* ((dir (harmless-team--harmless-session session))
                 (path (expand-file-name "summary.json" dir))
                 (obj (harmless-json-decode-safe
                       (harmless-team--read-text path)))
                 (used (and (harmless-plist-p obj)
                            (plist-get obj :last-prompt-tokens)))
                 (window
                  (and (fboundp 'harmless-usage-context-window)
                       (harmless-usage-context-window
                        (plist-get obj :model)))))
            (when (and (numberp used)
                       (numberp window)
                       (> window 0))
              (cons used window)))
        (error nil)))))

(defun harmless-team--watchdog-wall-due-p (stored job)
  "Return non-nil if JOB's seat has reached its context wall.
This reads a summary already on disk.  It does not start a turn."
  (let* ((seat (harmless-team--seat-named stored (plist-get job :seat)))
         (used (and seat (harmless-team--context-used seat)))
         (percent (plist-get job :percent)))
    (and used
         (numberp percent)
         (>= (* (car used) 100) (* (cdr used) percent)))))

(defun harmless-team--watchdog-due-now-p (stored job)
  "Return non-nil if JOB needs attention now.
A reminder compares its time.  An artifact pool is waiting.
A nudge waits while its workflow has a row.  An idle job
waits while its seat has an open row."
  (let ((kind (plist-get job :kind)))
    (cond
     ((equal kind "reminder")
      (not (string-lessp (harmless-now-iso) (plist-get job :due))))
     ((equal kind "artifacts")
      (consp (plist-get job :pool)))
     ((equal kind "nudge")
      (let ((workflow (harmless-team--find-named
                       (plist-get stored :workflows)
                       (plist-get job :target))))
        (and workflow (stringp (plist-get workflow :row)))))
     ((equal kind "idle")
      (let ((found nil))
        (dolist (row (plist-get stored :rows))
          (when (and (equal (plist-get row :state) "open")
                     (harmless-team--same-p
                      (plist-get row :seat)
                      (plist-get job :seat)))
            (setq found t)))
        found))
     (t (harmless-team--watchdog-wall-due-p stored job)))))

(defun harmless-team--watchdog-check (rig)
  "Return RIG's watchdogs that are due, oldest first.
This reads the rig file.  It does not arm a timer, a schedule,
or a monitor, and it does not start a turn."
  (setq rig (harmless-team--rig-name rig))
  (let* ((stored (cdr (harmless-team--load rig)))
         (out nil))
    (dolist (job (plist-get stored :watchdogs))
      (when (harmless-team--watchdog-due-now-p stored job)
        (setq out (append out
                          (list (harmless-team--public-watchdog job))))))
    out))

(defun harmless-team--watchdog-read (kind)
  "Read a watchdog spec for KIND from the minibuffer."
  (cond
   ((equal kind "reminder")
    (list :seat (read-string "Seat: ")
          :text (read-string "Text: ")
          :due (read-string "Due: ")))
   ((equal kind "artifacts")
    (list :pool (list (read-string "Artifact: "))))
   ((equal kind "nudge")
    (list :target (read-string "Workflow: ")))
   ((equal kind "idle")
    (list :seat (read-string "Seat: ")))
   ((equal kind "wall")
    (list :seat (read-string "Seat: ")
          :percent (read-number "Percent: ")))
   (t nil)))

;;;###autoload
(defun harmless-team-watchdog (rig kind spec)
  "Record one watchdog of KIND on RIG and return it.
SPEC is a plist.  The job stays in the rig file."
  (interactive
   (let ((kind (read-string "Kind: ")))
     (list (read-string "Rig: ")
           kind
           (harmless-team--watchdog-read (string-trim kind)))))
  (let ((job (harmless-team--watchdog-add rig kind spec)))
    (message "Watchdog %s" (plist-get job :id))
    job))

;;;###autoload
(defun harmless-team-watchdogs (rig)
  "Return RIG's watchdogs, oldest first."
  (interactive (list (read-string "Rig: ")))
  (let ((items (harmless-team--watchdogs rig)))
    (message "%d watchdogs" (length items))
    items))

;;;###autoload
(defun harmless-team-watchdog-check (rig)
  "Return RIG's watchdogs that are due, oldest first.
The check reads the rig file and does not arm a job."
  (interactive (list (read-string "Rig: ")))
  (let ((items (harmless-team--watchdog-check rig)))
    (message "%d due" (length items))
    items))

(defun harmless-team--snapshot-name (name)
  "Return NAME trimmed, or signal."
  (harmless-team--token
   name "team snapshot is empty" "team snapshot must be a string"
   #'harmless-team--member-name-p "team snapshot is not a name"))

(defun harmless-team--snapshot-fresh-shape (fresh)
  "Return FRESH when it is nil, one seat, or a list of seats.
A dotted list is refused.  Seats are resolved later."
  (cond
   ((null fresh) nil)
   ((stringp fresh) fresh)
   ((listp fresh)
    (let ((items fresh))
      (while (consp items)
        (setq items (cdr items)))
      (unless (null items)
        (error "team snapshot fresh must be a list"))
      fresh))
   (t (error "team snapshot fresh must be a list"))))

(defun harmless-team--snapshot-directory (rig)
  "Return the snapshot directory for RIG.  It is not created."
  (file-name-as-directory
   (expand-file-name
    rig
    (file-name-as-directory
     (expand-file-name
      "snapshots" (harmless-team--teams-root nil))))))

(defun harmless-team--snapshot-find (rig name)
  "Return RIG's snapshot file NAME, or nil.
The directory is not created."
  (let ((dir (harmless-team--snapshot-directory rig))
        (want (concat name ".json"))
        (found nil))
    (when (file-directory-p dir)
      (dolist (file (directory-files dir t "\\.json\\'"))
        (when (harmless-team--same-p
               (file-name-nondirectory file) want)
          (setq found file))))
    found))

(defun harmless-team--snapshot-new (rig name)
  "Return a new snapshot file for NAME in RIG, and create its directory."
  (let* ((dir (harmless-team--snapshot-directory rig))
         (path (expand-file-name (concat name ".json") dir)))
    (unless (and (file-name-absolute-p path)
                 (harmless-directory-strictly-under-p
                  path (harmless-team--data-directory))
                 (not (harmless-team--root-text-p path))
                 (not (equal (file-truename path)
                             (file-truename "/"))))
      (error "Refusing to store a team at %s" path))
    (harmless-ensure-directory dir)
    path))

(defun harmless-team--snapshot-write (path obj)
  "Write OBJ as JSON at PATH.  PATH is a snapshot, not the live rig."
  (unless (and (stringp path)
               (file-name-absolute-p path)
               (not (harmless-team--root-text-p path))
               (not (equal (file-truename path)
                           (file-truename "/"))))
    (error "Refusing to store a team at %s" path))
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region (harmless-json-text obj) nil path nil 'silent))
  path)

(defun harmless-team--snapshot-read (path rig name)
  "Return the snapshot plist at PATH, or signal.
RIG and NAME are the requested names.  A broken file signals
\"team snapshot is not valid\" and is not rewritten."
  (unless (and (stringp path)
               (file-name-absolute-p path)
               (file-regular-p path)
               (not (harmless-team--root-text-p path))
               (not (equal (file-truename path)
                           (file-truename "/"))))
    (error "team snapshot is not valid"))
  (let* ((text (harmless-team--read-text path))
         (obj (harmless-json-decode-safe text))
         (base (file-name-sans-extension
                (file-name-nondirectory path)))
         (team (and (harmless-plist-p obj) (plist-get obj :team))))
    (unless (and (harmless-plist-p obj)
                 (harmless-team--member-name-p (plist-get obj :name))
                 (harmless-team--same-p (plist-get obj :name) name)
                 (harmless-team--same-p (plist-get obj :name) base)
                 (stringp (plist-get obj :rig))
                 (harmless-team--same-p (plist-get obj :rig) rig)
                 (harmless-team--watchdog-due-p (plist-get obj :at))
                 (harmless-team--structure-p team)
                 (harmless-team--same-p
                  (plist-get obj :rig) (plist-get team :name)))
      (error "team snapshot is not valid"))
    obj))

(defun harmless-team--snapshot-fresh (team fresh)
  "Return the stored addresses in FRESH, or signal.
FRESH is nil, one seat, or a list of seats.  Each seat is
resolved in TEAM, which is the snapshot."
  (setq fresh (harmless-team--snapshot-fresh-shape fresh))
  (cond
   ((null fresh) nil)
   ((stringp fresh)
    (list (harmless-team--endpoint team fresh)))
   (t
    (let ((out nil))
      (dolist (seat fresh)
        (setq out (append out
                          (list (harmless-team--endpoint
                                 team seat)))))
      out))))

(defun harmless-team--snapshot-live-p (seat)
  "Return non-nil if SEAT's recorded session is still present.
This does not start a process or a turn."
  (let ((session (plist-get seat :session))
        (runtime (plist-get seat :runtime)))
    (cond
     ((or (null runtime)
          (and (stringp runtime) (string-empty-p runtime))
          (equal runtime "harmless"))
      (and (stringp session)
           (string-match-p harmless-team--session-re session)
           (condition-case nil
               (progn (harmless-team--harmless-session session) t)
             (error nil))))
     ((equal runtime "terminal")
      (condition-case nil
          (progn (harmless-team--terminal-ref session) t)
        (error nil)))
     ((member runtime '("claude" "codex" "pi"))
      (condition-case nil
          (progn (harmless-team--existing-path session) t)
        (error nil)))
     (t nil))))

(defun harmless-team--snapshot-waiting-p (team seat)
  "Return non-nil if SEAT has a snapshot row that asks a person."
  (let ((found nil))
    (dolist (row (plist-get team :rows))
      (when (and (harmless-team--same-p
                  (plist-get row :seat)
                  (plist-get seat :address))
                 (harmless-team--row-waiting-p row))
        (setq found t)))
    found))

(defun harmless-team--snapshot-status (team seat fresh)
  "Return SEAT's restore status in TEAM.
FRESH is a list of stored addresses.  One seat has one status."
  (cond
   ((harmless-team--seen-p (plist-get seat :address) fresh)
    "fresh")
   ((harmless-team--snapshot-waiting-p team seat)
    "waiting")
   ((let ((session (plist-get seat :session)))
      (or (null session)
          (and (stringp session)
               (string-empty-p (string-trim session)))))
    "rebuilt")
   ((harmless-team--snapshot-live-p seat)
    "resumed")
   (t "failed")))

(defun harmless-team--snapshot-report (team fresh)
  "Return the per-seat report for TEAM and FRESH.
FRESH is nil, one seat, or a list of seats."
  (let ((addresses (harmless-team--snapshot-fresh team fresh))
        (seats nil)
        (ok t))
    (dolist (pod (plist-get team :pods))
      (dolist (seat (plist-get pod :seats))
        (let ((status (harmless-team--snapshot-status
                       team seat addresses)))
          (when (equal status "failed")
            (setq ok nil))
          (setq seats
                (append seats
                        (list (list :seat (plist-get seat :address)
                                    :status status)))))))
    (list :ok ok :seats seats)))

(defun harmless-team--snapshot-apply (team fresh)
  "Return a copy of TEAM with FRESH seats cleared.
The snapshot file is not changed.  Work stays."
  (let ((addresses (harmless-team--snapshot-fresh team fresh))
        (copy (copy-tree team)))
    (dolist (pod (plist-get copy :pods))
      (dolist (seat (plist-get pod :seats))
        (when (harmless-team--seen-p
               (plist-get seat :address) addresses)
          (plist-put seat :session "")
          (plist-put seat :runtime nil))))
    copy))

(defun harmless-team--snapshot-take (rig name)
  "Store a named snapshot of RIG and return NAME.
A second snapshot of the same name is refused."
  (setq name (harmless-team--snapshot-name name))
  (setq rig (harmless-team--rig-name rig))
  (let ((stored (cdr (harmless-team--load rig))))
    (when (harmless-team--snapshot-find rig name)
      (error "team snapshot already exists"))
    (harmless-team--snapshot-write
     (harmless-team--snapshot-new rig name)
     (list :name name
           :rig (plist-get stored :name)
           :at (harmless-now-iso)
           :team stored))
    name))

(defun harmless-team--snapshot-open (rig name fresh)
  "Return (TEAM . REPORT) for RIG's snapshot NAME.
FRESH is the seats that start over.  Nothing is written."
  (setq name (harmless-team--snapshot-name name))
  (setq fresh (harmless-team--snapshot-fresh-shape fresh))
  (setq rig (harmless-team--rig-name rig))
  (let ((path (harmless-team--snapshot-find rig name)))
    (unless path
      (error "team snapshot is missing"))
    (let ((team (plist-get
                 (harmless-team--snapshot-read path rig name)
                 :team)))
      (cons team (harmless-team--snapshot-report team fresh)))))

(defun harmless-team--snapshot-check (rig name &optional fresh)
  "Return the report for restoring RIG's snapshot NAME.
FRESH is nil, one seat, or a list of seats.  This does not
write, and it does not start a turn or a process."
  (cdr (harmless-team--snapshot-open rig name fresh)))

(defun harmless-team--snapshot-restore-to (rig team)
  "Write TEAM over RIG's live file and return the path.
A missing rig file is created.  A snapshot file is not."
  (let* ((root (harmless-team--teams-root nil))
         (path (harmless-team--existing root rig)))
    (unless path
      (setq path (harmless-team--new-path
                  (harmless-team--teams-root t)
                  (plist-get team :name))))
    (harmless-team--write path team)))

(defun harmless-team--snapshot-restore (rig name &optional fresh)
  "Write RIG's snapshot NAME back when every seat can return.
FRESH seats are cleared and labeled.  A failed seat writes
nothing."
  (let* ((opened (harmless-team--snapshot-open rig name fresh))
         (report (cdr opened)))
    (unless (plist-get report :ok)
      (error "team snapshot cannot restore"))
    (harmless-team--snapshot-restore-to
     rig (harmless-team--snapshot-apply (car opened) fresh))
    report))

;;;###autoload
(defun harmless-team-snapshot (rig name)
  "Store a named snapshot of RIG and return NAME.
The copy is a separate file.  The live rig is not changed."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Name: ")))
  (let ((saved (harmless-team--snapshot-take rig name)))
    (message "Snapshot %s" saved)
    saved))

;;;###autoload
(defun harmless-team-snapshot-check (rig name &optional fresh)
  "Report whether RIG's snapshot NAME can be restored.
FRESH is nil, one seat, or a list of seats that start over.
The check does not write."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Name: ")
         (let ((answer (read-string "Fresh seat: ")))
           (if (string-empty-p (string-trim answer))
               nil
             answer))))
  (let ((report (harmless-team--snapshot-check rig name fresh)))
    (message "Checked %s" name)
    report))

;;;###autoload
(defun harmless-team-snapshot-restore (rig name &optional fresh)
  "Restore RIG's snapshot NAME when the check can succeed.
FRESH is nil, one seat, or a list of seats that start over.
A failed seat leaves the live rig unchanged."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Name: ")
         (let ((answer (read-string "Fresh seat: ")))
           (if (string-empty-p (string-trim answer))
               nil
             answer))))
  (let ((report (harmless-team--snapshot-restore rig name fresh)))
    (message "Restored %s" name)
    report))

(defun harmless-team--recap-text (text)
  "Return TEXT trimmed, or signal.
Internal newlines stay.  This text is the decisions."
  (cond
   ((null text) (error "team recap is empty"))
   ((not (stringp text))
    (error "team recap must be a string")))
  (setq text (string-trim text))
  (if (string-empty-p text)
      (error "team recap is empty")
    text))

(defun harmless-team--public-boundary (item)
  "Return ITEM with keys in a fixed order."
  (list :id (plist-get item :id)
        :seat (plist-get item :seat)
        :text (plist-get item :text)
        :at (plist-get item :at)
        :from (plist-get item :from)
        :from-runtime (plist-get item :from-runtime)
        :runtime (plist-get item :runtime)
        :session (plist-get item :session)))

(defun harmless-team--successor (rig pod seat runtime ref recap)
  "Bind REF to SEAT and record RECAP.  Return seat and boundary.
The seat's name, edges, and inbound work stay.  REF is already
running.  A refusal writes nothing."
  (setq recap (harmless-team--recap-text recap))
  (setq runtime (harmless-team--runtime runtime))
  (setq rig (harmless-team--rig-name rig))
  (setq pod (harmless-team--pod-name pod))
  (setq seat (harmless-team--seat-name seat))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (found (harmless-team--find stored pod seat)))
    (unless found
      (error "team seat is missing"))
    (let* ((item (cdr found))
           (from (plist-get item :session))
           (from-runtime (plist-get item :runtime)))
      (unless (and (stringp from) (not (string-empty-p from)))
        (error "team seat is not occupied"))
      (setq ref (harmless-team--occupant-ref runtime ref))
      (when (harmless-team--same-p from ref)
        (error "team successor is the same session"))
      (let ((boundary
             (list :id (harmless-team--next-row-id
                        (plist-get stored :boundaries))
                   :seat (plist-get item :address)
                   :text recap
                   :at (harmless-now-iso)
                   :from from
                   :from-runtime from-runtime
                   :runtime runtime
                   :session ref)))
        (plist-put stored :boundaries
                   (append (plist-get stored :boundaries)
                           (list boundary)))
        (plist-put item :session ref)
        (plist-put item :runtime runtime)
        (harmless-team--write (car loaded) stored)
        (list :seat (harmless-team--public stored (car found) item)
              :boundary (harmless-team--public-boundary boundary))))))

(defun harmless-team--side-directory (kind rig)
  "Return KIND's directory for RIG.  It is not created.
KIND is \"images\" or \"packets\"."
  (file-name-as-directory
   (expand-file-name
    rig
    (file-name-as-directory
     (expand-file-name kind (harmless-team--teams-root nil))))))

(defun harmless-team--side-find (kind rig name)
  "Return KIND's file NAME for RIG, or nil.
The directory is not created."
  (let ((dir (harmless-team--side-directory kind rig))
        (want (concat name ".json"))
        (found nil))
    (when (file-directory-p dir)
      (dolist (file (directory-files dir t "\\.json\\'"))
        (when (harmless-team--same-p
               (file-name-nondirectory file) want)
          (setq found file))))
    found))

(defun harmless-team--side-new (kind rig name)
  "Return a new KIND file for NAME in RIG and create its directory."
  (let* ((dir (harmless-team--side-directory kind rig))
         (path (expand-file-name (concat name ".json") dir)))
    (unless (and (file-name-absolute-p path)
                 (harmless-directory-strictly-under-p
                  path (harmless-team--data-directory))
                 (not (harmless-team--root-text-p path))
                 (not (equal (file-truename path)
                             (file-truename "/"))))
      (error "Refusing to store a team at %s" path))
    (harmless-ensure-directory dir)
    path))

(defun harmless-team--side-write (path obj)
  "Write OBJ as JSON at PATH.  PATH is not the live rig."
  (unless (and (stringp path)
               (file-name-absolute-p path)
               (not (harmless-team--root-text-p path))
               (not (equal (file-truename path)
                           (file-truename "/"))))
    (error "Refusing to store a team at %s" path))
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region (harmless-json-text obj) nil path nil 'silent))
  path)

(defun harmless-team--seat-recap (stored address)
  "Return ADDRESS's boundary texts in STORED, oldest first.
Nil means that seat has no recap."
  (let ((parts nil))
    (dolist (item (plist-get stored :boundaries))
      (when (harmless-team--same-p (plist-get item :seat) address)
        (setq parts (append parts (list (plist-get item :text))))))
    (when parts
      (mapconcat #'identity parts "\n"))))

(defun harmless-team--image-read (path rig name)
  "Return the image plist at PATH, or signal.
A broken file signals \"team image is not valid\"."
  (unless (and (stringp path)
               (file-name-absolute-p path)
               (file-regular-p path)
               (not (harmless-team--root-text-p path))
               (not (equal (file-truename path)
                           (file-truename "/"))))
    (error "team image is not valid"))
  (let* ((obj (harmless-json-decode-safe
               (harmless-team--read-text path)))
         (base (file-name-sans-extension
                (file-name-nondirectory path)))
         (text (and (harmless-plist-p obj) (plist-get obj :text))))
    (unless (and (harmless-plist-p obj)
                 (harmless-team--member-name-p (plist-get obj :name))
                 (harmless-team--same-p (plist-get obj :name) name)
                 (harmless-team--same-p (plist-get obj :name) base)
                 (stringp (plist-get obj :rig))
                 (harmless-team--same-p (plist-get obj :rig) rig)
                 (harmless-team--watchdog-due-p (plist-get obj :at))
                 (stringp (plist-get obj :role))
                 (not (string-empty-p (plist-get obj :role)))
                 (harmless-team--string-list-p (plist-get obj :work))
                 (or (null text)
                     (and (stringp text) (not (string-empty-p text)))))
      (error "team image is not valid"))
    obj))

(defun harmless-team--image-name (name)
  "Return NAME trimmed, or signal."
  (harmless-team--token
   name "team image is empty" "team image must be a string"
   #'harmless-team--member-name-p "team image is not a name"))

(defun harmless-team--packet-name (name)
  "Return NAME trimmed, or signal."
  (harmless-team--token
   name "team packet is empty" "team packet must be a string"
   #'harmless-team--member-name-p "team packet is not a name"))

(defun harmless-team--image-take (rig pod seat name)
  "Capture SEAT into a named image and return NAME.
SEAT must already be occupied.  The live rig is not changed."
  (setq name (harmless-team--image-name name))
  (setq rig (harmless-team--rig-name rig))
  (setq pod (harmless-team--pod-name pod))
  (setq seat (harmless-team--seat-name seat))
  (let* ((stored (cdr (harmless-team--load rig)))
         (found (harmless-team--find stored pod seat)))
    (unless found
      (error "team seat is missing"))
    (let* ((item (cdr found))
           (session (plist-get item :session)))
      (unless (and (stringp session) (not (string-empty-p session)))
        (error "team seat is not occupied"))
      (when (harmless-team--side-find "images" rig name)
        (error "team image already exists"))
      (harmless-team--side-write
       (harmless-team--side-new "images" rig name)
       (list :name name
             :rig (plist-get stored :name)
             :at (harmless-now-iso)
             :role (plist-get item :role)
             :work (plist-get item :work)
             :text (harmless-team--seat-recap
                    stored (plist-get item :address))))
      name)))

(defun harmless-team--image-start (rig name pod seat)
  "Add SEAT in POD of RIG from image NAME and return it.
The new seat copies the role and the inbound work.  It starts
with no session.  Nothing is started."
  (setq name (harmless-team--image-name name))
  (setq rig (harmless-team--rig-name rig))
  (setq pod (harmless-team--pod-name pod))
  (setq seat (harmless-team--seat-name seat))
  (let ((path (harmless-team--side-find "images" rig name)))
    (unless path
      (error "team image is missing"))
    (let* ((image (harmless-team--image-read path rig name))
           (loaded (harmless-team--load rig))
           (stored (cdr loaded))
           (pod-item (harmless-team--find-named
                      (plist-get stored :pods) pod))
           (pod-name (if pod-item (plist-get pod-item :name) pod)))
      (when (and pod-item
                 (harmless-team--find-named
                  (plist-get pod-item :seats) seat))
        (error "team seat already exists"))
      (let ((new (list :name seat
                       :role (plist-get image :role)
                       :session ""
                       :work (copy-tree (plist-get image :work))
                       :address
                       (format "%s-%s@%s"
                               pod-name seat
                               (plist-get stored :name)))))
        (if pod-item
            (plist-put pod-item :seats
                       (append (plist-get pod-item :seats) (list new)))
          (setq stored
                (plist-put stored :pods
                           (append (plist-get stored :pods)
                                   (list (list :name pod-name
                                               :seats (list new)))))))
        (harmless-team--write (car loaded) stored)
        (harmless-team--public
         stored
         (if pod-item
             pod-item
           (harmless-team--find-named (plist-get stored :pods) pod))
         new)))))

(defun harmless-team--packet-read (path rig name)
  "Return the packet plist at PATH, or signal.
A broken file signals \"team packet is not valid\"."
  (unless (and (stringp path)
               (file-name-absolute-p path)
               (file-regular-p path)
               (not (harmless-team--root-text-p path))
               (not (equal (file-truename path)
                           (file-truename "/"))))
    (error "team packet is not valid"))
  (let* ((obj (harmless-json-decode-safe
               (harmless-team--read-text path)))
         (base (file-name-sans-extension
                (file-name-nondirectory path)))
         (runtime (and (harmless-plist-p obj)
                       (plist-get obj :from-runtime))))
    (unless (and (harmless-plist-p obj)
                 (harmless-team--member-name-p (plist-get obj :name))
                 (harmless-team--same-p (plist-get obj :name) name)
                 (harmless-team--same-p (plist-get obj :name) base)
                 (stringp (plist-get obj :rig))
                 (harmless-team--same-p (plist-get obj :rig) rig)
                 (harmless-team--watchdog-due-p (plist-get obj :at))
                 (or (null runtime)
                     (member runtime harmless-team--runtimes))
                 (stringp (plist-get obj :role))
                 (not (string-empty-p (plist-get obj :role)))
                 (harmless-team--string-list-p (plist-get obj :work))
                 (stringp (plist-get obj :text))
                 (not (string-empty-p (plist-get obj :text))))
      (error "team packet is not valid"))
    obj))

(defun harmless-team--packet-text (stored item)
  "Return the text ITEM knows, or signal when it knows nothing."
  (or (harmless-team--seat-recap stored (plist-get item :address))
      (let ((work (plist-get item :work)))
        (when work
          (mapconcat #'identity work "\n")))
      (error "team packet is empty")))

(defun harmless-team--packet-take (rig pod seat name)
  "Capture what SEAT knows and return NAME.
SEAT must already be occupied.  The live rig is not changed."
  (setq name (harmless-team--packet-name name))
  (setq rig (harmless-team--rig-name rig))
  (setq pod (harmless-team--pod-name pod))
  (setq seat (harmless-team--seat-name seat))
  (let* ((stored (cdr (harmless-team--load rig)))
         (found (harmless-team--find stored pod seat)))
    (unless found
      (error "team seat is missing"))
    (let* ((item (cdr found))
           (session (plist-get item :session)))
      (unless (and (stringp session) (not (string-empty-p session)))
        (error "team seat is not occupied"))
      (let ((text (harmless-team--packet-text stored item)))
        (when (harmless-team--side-find "packets" rig name)
          (error "team packet already exists"))
        (harmless-team--side-write
         (harmless-team--side-new "packets" rig name)
         (list :name name
               :rig (plist-get stored :name)
               :at (harmless-now-iso)
               :from-runtime (plist-get item :runtime)
               :role (plist-get item :role)
               :work (plist-get item :work)
               :text text))
        name))))

(defun harmless-team--packet-apply (rig pod seat name runtime ref)
  "Carry packet NAME onto SEAT and bind the running session REF.
RUNTIME must differ from the packet.  The seat keeps its name.
REF is not started.  A refusal writes nothing."
  (setq name (harmless-team--packet-name name))
  (setq runtime (harmless-team--runtime runtime))
  (setq rig (harmless-team--rig-name rig))
  (setq pod (harmless-team--pod-name pod))
  (setq seat (harmless-team--seat-name seat))
  (let ((path (harmless-team--side-find "packets" rig name)))
    (unless path
      (error "team packet is missing"))
    (let* ((packet (harmless-team--packet-read path rig name))
           (loaded (harmless-team--load rig))
           (stored (cdr loaded))
           (found (harmless-team--find stored pod seat)))
      (unless found
        (error "team seat is missing"))
      (when (and (plist-get packet :from-runtime)
                 (equal (plist-get packet :from-runtime) runtime))
        (error "team packet is the same runtime"))
      (let* ((item (cdr found))
             (from (plist-get item :session)))
        (unless (and (stringp from) (not (string-empty-p from)))
          (error "team seat is not occupied"))
        (setq ref (harmless-team--occupant-ref runtime ref))
        (let ((boundary
               (list :id (harmless-team--next-row-id
                          (plist-get stored :boundaries))
                     :seat (plist-get item :address)
                     :text (plist-get packet :text)
                     :at (harmless-now-iso)
                     :from from
                     :from-runtime (plist-get item :runtime)
                     :runtime runtime
                     :session ref)))
          (plist-put item :work (copy-tree (plist-get packet :work)))
          (plist-put item :session ref)
          (plist-put item :runtime runtime)
          (plist-put stored :boundaries
                     (append (plist-get stored :boundaries)
                             (list boundary)))
          (harmless-team--write (car loaded) stored)
          (list :seat (harmless-team--public stored (car found) item)
                :boundary (harmless-team--public-boundary boundary)))))))

(defun harmless-team--boundaries (rig)
  "Return RIG's handover boundaries, oldest first."
  (setq rig (harmless-team--rig-name rig))
  (mapcar #'harmless-team--public-boundary
          (plist-get (cdr (harmless-team--load rig)) :boundaries)))

;;;###autoload
(defun harmless-team-successor (rig pod seat runtime ref recap)
  "Hand SEAT in POD of RIG to the running session REF.
RUNTIME names that session.  RECAP is the decisions at this
boundary.  The seat keeps its name, its edges, and its inbound
work.  REF is not started.  Return the seat and the boundary."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Pod: ")
         (read-string "Seat: ")
         (read-string "Runtime: ")
         (read-string "Session: ")
         (read-string "Recap: ")))
  (let ((saved (harmless-team--successor
                rig pod seat runtime ref recap)))
    (message "Successor %s" (plist-get (plist-get saved :seat) :address))
    saved))

;;;###autoload
(defun harmless-team-boundaries (rig)
  "Return RIG's handover boundaries, oldest first."
  (interactive (list (read-string "Rig: ")))
  (let ((items (harmless-team--boundaries rig)))
    (message "%d boundaries" (length items))
    items))

;;;###autoload
(defun harmless-team-image (rig pod seat name)
  "Capture the occupied seat SEAT in POD of RIG as NAME.
The copy is a separate file.  The live rig is not changed."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Pod: ")
         (read-string "Seat: ")
         (read-string "Name: ")))
  (let ((saved (harmless-team--image-take rig pod seat name)))
    (message "Image %s" saved)
    saved))

;;;###autoload
(defun harmless-team-image-start (rig name pod seat)
  "Start SEAT in POD of RIG from the image NAME.
The new seat copies the role and the inbound work.  It has no
session.  Nothing is started.  Return the seat."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Image: ")
         (read-string "Pod: ")
         (read-string "Seat: ")))
  (let ((saved (harmless-team--image-start rig name pod seat)))
    (message "Started %s" (plist-get saved :address))
    saved))

;;;###autoload
(defun harmless-team-packet (rig pod seat name)
  "Capture what SEAT in POD of RIG knows and name it NAME.
The copy is a separate file.  The live rig is not changed."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Pod: ")
         (read-string "Seat: ")
         (read-string "Name: ")))
  (let ((saved (harmless-team--packet-take rig pod seat name)))
    (message "Packet %s" saved)
    saved))

;;;###autoload
(defun harmless-team-packet-apply (rig pod seat name runtime ref)
  "Carry packet NAME onto SEAT and bind the running session REF.
RUNTIME must differ from the packet.  The seat keeps its name.
REF is not started."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Pod: ")
         (read-string "Seat: ")
         (read-string "Packet: ")
         (read-string "Runtime: ")
         (read-string "Session: ")))
  (let ((saved (harmless-team--packet-apply
                rig pod seat name runtime ref)))
    (message "Carried %s" (plist-get (plist-get saved :seat) :address))
    saved))

(defun harmless-team--percent (percent)
  "Return PERCENT, an integer from 1 through 100, or signal."
  (cond
   ((null percent) (error "team compact percent is empty"))
   ((stringp percent)
    (setq percent (string-trim percent))
    (if (string-empty-p percent)
        (error "team compact percent is empty")
      (error "team compact percent is not a percent")))
   ((not (integerp percent))
    (error "team compact percent is not a percent"))
   ((or (< percent 1) (> percent 100))
    (error "team compact percent is not a percent"))
   (t percent)))

(defun harmless-team--compact-why (text)
  "Return TEXT trimmed, or signal.
Internal newlines stay.  This text is why the seat is compacted."
  (cond
   ((null text) (error "team compact why is empty"))
   ((not (stringp text))
    (error "team compact why must be a string")))
  (setq text (string-trim text))
  (if (string-empty-p text)
      (error "team compact why is empty")
    text))

(defun harmless-team--public-compact (item)
  "Return ITEM with keys in a fixed order."
  (list :id (plist-get item :id)
        :seat (plist-get item :seat)
        :session (plist-get item :session)
        :why (plist-get item :why)
        :at (plist-get item :at)
        :used (plist-get item :used)
        :window (plist-get item :window)
        :percent (plist-get item :percent)))

(defun harmless-team--near-p (used percent)
  "Return non-nil if USED is at or above PERCENT of its window."
  (and used
       (>= (* (car used) 100) (* (cdr used) percent))))

(defun harmless-team--compact-plan (rig percent)
  "Return seats in RIG at or above PERCENT.  Write nothing.
Each item names the seat, the session, and the measurement.
A seat that is not a Harmless session is left off the list."
  (setq percent (harmless-team--percent percent))
  (setq rig (harmless-team--rig-name rig))
  (let ((stored (cdr (harmless-team--load rig)))
        (out nil))
    (dolist (pod (plist-get stored :pods))
      (dolist (seat (plist-get pod :seats))
        (let ((used (harmless-team--context-used seat)))
          (when (harmless-team--near-p used percent)
            (setq out
                  (append
                   out
                   (list (list :seat (plist-get seat :address)
                               :session (plist-get seat :session)
                               :used (car used)
                               :window (cdr used)
                               :percent percent))))))))
    out))

(defun harmless-team--compact (rig pod seat percent why)
  "Record a deliberate compact of SEAT and tell the room.
WHY is the reason.  PERCENT is the wall.  SEAT must already
be there.  The transcript is not summarized.  One write."
  (setq why (harmless-team--compact-why why))
  (setq percent (harmless-team--percent percent))
  (setq rig (harmless-team--rig-name rig))
  (setq pod (harmless-team--pod-name pod))
  (setq seat (harmless-team--seat-name seat))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (found (harmless-team--find stored pod seat)))
    (unless found
      (error "team seat is missing"))
    (let* ((item (cdr found))
           (session (plist-get item :session))
           (used (harmless-team--context-used item)))
      (unless (and (stringp session) (not (string-empty-p session)))
        (error "team seat is not occupied"))
      (unless (harmless-team--near-p used percent)
        (error "team seat is not near the wall"))
      (let* ((at (harmless-now-iso))
             (address (plist-get item :address))
             (record (list :id (harmless-team--next-row-id
                                (plist-get stored :compacts))
                           :seat address
                           :session session
                           :why why
                           :at at
                           :used (car used)
                           :window (cdr used)
                           :percent percent))
             (line (list :topic "compact"
                         :speaker address
                         :text why
                         :at at)))
        (plist-put stored :compacts
                   (append (plist-get stored :compacts)
                           (list record)))
        (plist-put stored :room
                   (append (plist-get stored :room) (list line)))
        (harmless-team--write (car loaded) stored)
        (list :compact (harmless-team--public-compact record)
              :room (harmless-team--public-room line))))))

(defun harmless-team--compacts (rig)
  "Return RIG's compact notices, oldest first."
  (setq rig (harmless-team--rig-name rig))
  (mapcar #'harmless-team--public-compact
          (plist-get (cdr (harmless-team--load rig)) :compacts)))

;;;###autoload
(defun harmless-team-compact-plan (rig percent)
  "List seats in RIG at or above PERCENT of the context window.
This does not compact and does not write.  The transcript
summary is M-x harmless-compact."
  (interactive
   (list (read-string "Rig: ")
         (read-number "Percent: ")))
  (let ((items (harmless-team--compact-plan rig percent)))
    (message "%d seats" (length items))
    items))

;;;###autoload
(defun harmless-team-compact (rig pod seat percent why)
  "Record a deliberate compact of SEAT and tell the room.
PERCENT is the wall.  WHY is the reason.  SEAT must already
be at that wall.  This does not summarize the transcript."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Pod: ")
         (read-string "Seat: ")
         (read-number "Percent: ")
         (read-string "Why: ")))
  (let ((saved (harmless-team--compact rig pod seat percent why)))
    (message "Compacted %s"
             (plist-get (plist-get saved :compact) :seat))
    saved))

;;;###autoload
(defun harmless-team-compacts (rig)
  "Return RIG's compact notices, oldest first."
  (interactive (list (read-string "Rig: ")))
  (let ((items (harmless-team--compacts rig)))
    (message "%d compacts" (length items))
    items))

(provide 'harmless-team)

;;; harmless-team.el ends here
