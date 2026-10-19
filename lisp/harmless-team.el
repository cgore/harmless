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
;; the launch order.  This file does not boot a team, send to a
;; seat, or register another machine.

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

(defun harmless-team--seat-ok-p (seat pod rig seen)
  "Return non-nil if SEAT belongs to POD in RIG.
SEEN holds seat names already accepted in POD."
  (and (harmless-plist-p seat)
       (harmless-team--member-name-p (plist-get seat :name))
       (not (harmless-team--seen-p (plist-get seat :name) seen))
       (stringp (plist-get seat :role))
       (not (string-empty-p (plist-get seat :role)))
       (stringp (plist-get seat :session))
       (or (string-empty-p (plist-get seat :session))
           (string-match-p harmless-team--session-re
                           (plist-get seat :session)))
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
       (harmless-team--edges-ok-p rig)))

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
  (list :rig (plist-get rig :name)
        :pod (plist-get pod :name)
        :name (plist-get seat :name)
        :role (plist-get seat :role)
        :address (plist-get seat :address)
        :session (plist-get seat :session)
        :work (plist-get seat :work)))

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

(provide 'harmless-team)

;;; harmless-team.el ends here
