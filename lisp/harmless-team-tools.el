;;; harmless-team-tools.el --- Team commands as tools -*- lexical-binding: t; -*-

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
;; Tools that boot, inspect, message, snapshot, and restore a
;; team.  Each one calls the command a person runs, so the
;; guards match.  A detached chat is not offered these tools.
;; Plan mode refuses the ones that write.  Nothing is started.

;;; Code:

(require 'subr-x)
(require 'harmless-util)
(require 'harmless-tools)
(require 'harmless-tools-fs)
(require 'harmless-session)
(require 'harmless-team)

(defun harmless-team-tools--session (session)
  "Return SESSION, or signal.
A detached chat is refused before a team file is touched."
  (unless (harmless-session-p session)
    (error "No Harmless session"))
  (when (harmless-session-detached-p session)
    (error "This chat is not attached to a project"))
  session)

(defun harmless-team-tools--write (session)
  "Return SESSION when it may write a team file."
  (harmless-team-tools--session session)
  (harmless-tools-fs--reject-plan-edit session)
  session)

(defun harmless-team-tools--text (value)
  "Return VALUE as JSON text."
  (harmless-json-text value))

(defun harmless-team-tools-boot (session args)
  "Boot the team ARGS names for SESSION.
Return the same preview as `harmless-team-boot'."
  (harmless-team-tools--write session)
  (harmless-team-tools--text
   (harmless-team-boot (harmless-tool-arg args :spec))))

(defun harmless-team-tools-inspect (session args)
  "Return the seats, edges, order, and rows of ARGS's rig.
This writes nothing."
  (harmless-team-tools--session session)
  (let* ((rig (harmless-team--rig-name
               (harmless-tool-arg args :rig)))
         (order (harmless-team-launch-order rig))
         (seats nil))
    (dolist (address order)
      (let ((case-fold-search nil))
        (unless (and (stringp address)
                     (string-match
                      "\\`\\([A-Za-z0-9]+\\)-\\([A-Za-z0-9]+\\)@"
                      address))
          (error "team seat is missing"))
        (setq seats
              (append seats
                      (list (harmless-team-seat
                             rig
                             (match-string 1 address)
                             (match-string 2 address)))))))
    (harmless-team-tools--text
     (list :name rig
           :seats seats
           :edges (harmless-team-edges rig)
           :order order
           :rows (harmless-team-rows rig)))))

(defun harmless-team-tools-message (session args)
  "Send ARGS's text to a seat for SESSION.
Return the addresses `harmless-team-send' wrote."
  (harmless-team-tools--write session)
  (harmless-team-tools--text
   (harmless-team-send (harmless-tool-arg args :rig)
                       (harmless-tool-arg args :target)
                       (harmless-tool-arg args :text))))

(defun harmless-team-tools-snapshot (session args)
  "Store ARGS's named snapshot for SESSION.
Return the snapshot name."
  (harmless-team-tools--write session)
  (harmless-team-tools--text
   (harmless-team-snapshot (harmless-tool-arg args :rig)
                           (harmless-tool-arg args :name))))

(defun harmless-team-tools-restore (session args)
  "Restore ARGS's snapshot for SESSION.
Return the same report as `harmless-team-snapshot-restore'."
  (harmless-team-tools--write session)
  (harmless-team-tools--text
   (harmless-team-snapshot-restore
    (harmless-tool-arg args :rig)
    (harmless-tool-arg args :name)
    (harmless-tool-arg args :fresh))))

(defun harmless-team-tools-register ()
  "Register the team tools."
  (harmless-register-tool
   (harmless-tool-create
    :name "team_boot"
    :description "Boot a team from spec. spec is starter, an absolute JSON file, or a directory with rig.json. The guards match M-x harmless-team-boot."
    :class 'edit
    :schema '(:type "object"
              :properties (:spec (:type "string"
                                  :description "starter, or an absolute spec."))
              :required ("spec"))
    :fn #'harmless-team-tools-boot))
  (harmless-register-tool
   (harmless-tool-create
    :name "team_inspect"
    :description "Read one team. Return its seats, edges, launch order, and queue rows. This writes nothing."
    :class 'read
    :schema '(:type "object"
              :properties (:rig (:type "string"
                                 :description "Rig name."))
              :required ("rig"))
    :fn #'harmless-team-tools-inspect))
  (harmless-register-tool
   (harmless-tool-create
    :name "team_message"
    :description "Send text to target in rig. target is one seat, a pod, the rig name, or *. The seat must already be occupied. The guards match M-x harmless-team-send."
    :class 'edit
    :schema '(:type "object"
              :properties (:rig (:type "string")
                           :target (:type "string")
                           :text (:type "string"))
              :required ("rig" "target" "text"))
    :fn #'harmless-team-tools-message))
  (harmless-register-tool
   (harmless-tool-create
    :name "team_snapshot"
    :description "Store a named snapshot of rig. The live rig is not changed. The guards match M-x harmless-team-snapshot."
    :class 'edit
    :schema '(:type "object"
              :properties (:rig (:type "string")
                           :name (:type "string"))
              :required ("rig" "name"))
    :fn #'harmless-team-tools-snapshot))
  (harmless-register-tool
   (harmless-tool-create
    :name "team_restore"
    :description "Restore snapshot name of rig when every seat can return. fresh is one seat, or a list of seats, that start over. The guards match M-x harmless-team-snapshot-restore."
    :class 'edit
    :schema '(:type "object"
              :properties (:rig (:type "string")
                           :name (:type "string")
                           :fresh (:description "One seat, or a list of seats."))
              :required ("rig" "name"))
    :fn #'harmless-team-tools-restore)))

(harmless-team-tools-register)

(provide 'harmless-team-tools)

;;; harmless-team-tools.el ends here
