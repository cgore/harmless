;;; harmless-health.el --- What a rig's record says is true -*- lexical-binding: t; -*-

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
;; Health reads the rig and the files it names.  It does not
;; start a session, a process, or a service.  A service sits
;; beside the seats and is reported on its own.  Recovery
;; rewrites a seat only when the live session and the record
;; disagree, and it does not delete the live session.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-team)

(defvar harmless-directory)

(defun harmless-health--name (name)
  "Return NAME as a service id, or signal."
  (cond
   ((null name) (error "service name is empty"))
   ((not (stringp name)) (error "service name must be a string"))
   (t
    (setq name (string-trim name))
    (cond
     ((string-empty-p name) (error "service name is empty"))
     ((or (string-prefix-p "~" name)
          (string-prefix-p "/" name)
          (string-prefix-p "\\" name)
          (string-match-p "[/\\\\]" name)
          (string-match-p "\\`[A-Za-z]:[/\\]?\\'" name)
          (file-name-absolute-p name)
          (not (harmless-team--member-name-p name)))
      (error "service name is not a name"))
     (t name)))))

(defun harmless-health--ref (ref)
  "Return REF as a stored service reference, or signal.
A missing path is kept.  It is not created.  \"~\" is not
expanded."
  (cond
   ((null ref) (error "service ref is empty"))
   ((not (stringp ref)) (error "service ref must be a string"))
   (t
    (setq ref (string-trim ref))
    (cond
     ((string-empty-p ref) (error "service ref is empty"))
     ((let ((case-fold-search nil))
        (string-match-p "\\`[0-9]+\\'" ref))
      (let ((pid (string-to-number ref)))
        (if (> pid 0)
            (number-to-string pid)
          (error "service ref is not a ref"))))
     ((harmless-team--root-text-p ref)
      (error "service ref is the filesystem root"))
     ((or (string-prefix-p "~" ref)
          (not (file-name-absolute-p ref)))
      (error "service ref must be absolute"))
     (t
      (setq ref (directory-file-name (expand-file-name ref)))
      (if (or (harmless-team--root-text-p ref)
              (equal (file-truename ref) (file-truename "/")))
          (error "service ref is the filesystem root")
        ref))))))

(defun harmless-health--summary-id (dir)
  "Return the id in DIR's summary, or nil.
This does not load the session."
  (let ((file (expand-file-name "summary.json" dir)))
    (condition-case nil
        (let ((obj (harmless-json-decode
                    (with-temp-buffer
                      (let ((coding-system-for-read 'utf-8-unix))
                        (insert-file-contents file))
                      (buffer-string)))))
          (and (harmless-plist-p obj)
               (let ((id (plist-get obj :id)))
                 (and (stringp id) (string-trim id)))))
      (error nil))))

(defun harmless-health--harmless (id)
  "Return the health of a Harmless session id ID."
  (condition-case err
      (let ((dir (harmless-team--harmless-session id)))
        (if (equal (harmless-health--summary-id dir) id)
            "live"
          "disagree"))
    (error
     (let ((msg (error-message-string err)))
       (cond
        ((equal msg "team session is missing") "missing")
        ((equal msg "team session is not one session") "disagree")
        (t (error "%s" msg)))))))

(defun harmless-health--path (ref)
  "Return the health of an absolute path REF.
A relative path is not expanded."
  (cond
   ((or (not (stringp ref))
        (string-empty-p ref)
        (string-prefix-p "~" ref)
        (not (file-name-absolute-p ref))
        (harmless-team--root-text-p ref))
    "disagree")
   (t
    (let ((path (directory-file-name (expand-file-name ref))))
      (cond
       ((or (harmless-team--root-text-p path)
            (equal (file-truename path) (file-truename "/")))
        "disagree")
       ((or (file-regular-p path) (file-directory-p path))
        "live")
       (t "missing"))))))

(defun harmless-health--terminal (ref)
  "Return the health of a terminal REF.
A pid is not started.  A path is not created."
  (if (and (stringp ref)
           (let ((case-fold-search nil))
             (string-match-p "\\`[0-9]+\\'" ref)))
      (let ((pid (string-to-number ref)))
        (if (and (> pid 0) (process-attributes pid))
            "live"
          "missing"))
    (harmless-health--path ref)))

(defun harmless-health--seat-health (runtime session)
  "Return the health word for RUNTIME and SESSION."
  (cond
   ((or (null session) (and (stringp session) (string-empty-p session)))
    "empty")
   ((or (null runtime) (equal runtime "harmless"))
    (harmless-health--harmless session))
   ((equal runtime "terminal")
    (harmless-health--terminal session))
   ((member runtime harmless-team--runtimes)
    (harmless-health--path session))
   (t "disagree")))

(defun harmless-health--seat (seat)
  "Return the public health plist for SEAT."
  (let ((runtime (plist-get seat :runtime))
        (session (plist-get seat :session)))
    (list :address (plist-get seat :address)
          :runtime (and (stringp runtime)
                        (not (string-empty-p runtime))
                        runtime)
          :session (if (stringp session) session "")
          :health (harmless-health--seat-health runtime session))))

(defun harmless-health--service-health (ref)
  "Return \"up\" or \"down\" for service REF."
  (cond
   ((and (stringp ref)
         (let ((case-fold-search nil))
           (string-match-p "\\`[1-9][0-9]*\\'" ref)))
    (if (process-attributes (string-to-number ref))
        "up"
      "down"))
   ((and (stringp ref)
         (file-name-absolute-p ref)
         (not (string-prefix-p "~" ref))
         (not (harmless-team--root-text-p ref))
         (or (file-regular-p ref) (file-directory-p ref)))
    "up")
   (t "down")))

(defun harmless-health--service (item)
  "Return the public health plist for service ITEM."
  (list :name (plist-get item :name)
        :ref (plist-get item :ref)
        :health (harmless-health--service-health (plist-get item :ref))))

(defun harmless-health--report (rig)
  "Return the health plist for rig plist RIG."
  (let ((seats nil)
        (services nil))
    (dolist (pod (plist-get rig :pods))
      (dolist (seat (plist-get pod :seats))
        (setq seats (append seats (list (harmless-health--seat seat))))))
    (dolist (item (plist-get rig :services))
      (setq services
            (append services (list (harmless-health--service item)))))
    (list :seats seats :services services)))

(defun harmless-health--find-service (rig name)
  "Return the service NAME in RIG, or nil."
  (let ((found nil))
    (dolist (item (plist-get rig :services))
      (when (and (not found)
                 (harmless-plist-p item)
                 (harmless-team--same-p (plist-get item :name) name))
        (setq found item)))
    found))

;;;###autoload
(defun harmless-team-service (rig name ref)
  "Record service NAME on RIG at REF.
REF is a process id or an absolute path.  A missing path is
not created.  A process is not started.  Return the service."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Service: ")
         (read-string "Ref: ")))
  (setq rig (harmless-team--rig-name rig))
  (setq name (harmless-health--name name))
  (setq ref (harmless-health--ref ref))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded)))
    (when (harmless-health--find-service stored name)
      (error "service is already registered: %s" name))
    (plist-put stored :services
               (append (plist-get stored :services)
                       (list (list :name name :ref ref))))
    (harmless-team--write (car loaded) stored)
    (message "Service %s" name)
    (list :name name :ref ref)))

;;;###autoload
(defun harmless-team-service-remove (rig name)
  "Forget service NAME on RIG.
The path and the process are left in place.  Return NAME."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Service: ")))
  (setq rig (harmless-team--rig-name rig))
  (setq name (harmless-health--name name))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (left (cl-remove name (plist-get stored :services)
                          :key (lambda (item) (plist-get item :name))
                          :test #'harmless-team--same-p)))
    (when (equal (length left) (length (plist-get stored :services)))
      (error "no service named %s" name))
    (plist-put stored :services left)
    (harmless-team--write (car loaded) stored)
    (message "Service %s" name)
    name))

;;;###autoload
(defun harmless-team-health (rig)
  "Return what is true for RIG.
Seats and services are separate.  This writes nothing and
starts nothing."
  (interactive (list (read-string "Rig: ")))
  (setq rig (harmless-team--rig-name rig))
  (let ((report (harmless-health--report
                 (cdr (harmless-team--load rig)))))
    (message "Health %s" rig)
    report))

(defun harmless-health--retarget (item)
  "Point ITEM at the session its summary names, or signal.
ITEM is a seat plist.  The old directory is not deleted."
  (let* ((id (plist-get item :session))
         (dir (harmless-team--harmless-session id))
         (live (harmless-health--summary-id dir)))
    (unless (and (stringp live)
                 (let ((case-fold-search nil))
                   (string-match-p harmless-team--session-re live))
                 (not (equal live id)))
      (error "team health cannot recover"))
    (let ((other (condition-case nil
                     (harmless-team--harmless-session live)
                   (error nil))))
      (unless (and other
                   (equal (harmless-health--summary-id other) live))
        (error "team health cannot recover"))
      (plist-put item :session live))))

(defun harmless-health--clear (item)
  "Clear ITEM's session binding.
The work and the profile stay.  Nothing is deleted."
  (plist-put item :session "")
  (plist-put item :runtime nil)
  item)

;;;###autoload
(defun harmless-team-recover (rig pod seat)
  "Make SEAT's record match its live session.
An empty seat and a live seat are left unchanged.  A missing
session is cleared.  A Harmless summary that names another
real session is retargeted.  The live files stay.  Return
the seat's health after the write."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Pod: ")
         (read-string "Seat: ")))
  (setq rig (harmless-team--rig-name rig))
  (setq pod (harmless-team--pod-name pod))
  (setq seat (harmless-team--seat-name seat))
  (let* ((loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (found (harmless-team--find stored pod seat)))
    (unless found
      (error "team seat is missing"))
    (let* ((item (cdr found))
           (health (plist-get (harmless-health--seat item) :health)))
      (cond
       ((member health '("empty" "live"))
        (error "team health agrees"))
       ((equal health "missing")
        (harmless-health--clear item))
       ((and (equal health "disagree")
             (or (null (plist-get item :runtime))
                 (equal (plist-get item :runtime) "harmless")))
        (harmless-health--retarget item))
       (t (error "team health cannot recover")))
      (harmless-team--write (car loaded) stored)
      (let ((after (harmless-health--seat item)))
        (message "Recovered %s" (plist-get item :address))
        after))))

(provide 'harmless-health)

;;; harmless-health.el ends here
