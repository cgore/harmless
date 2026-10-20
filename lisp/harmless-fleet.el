;;; harmless-fleet.el --- One view of a rig -*- lexical-binding: t; -*-

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
;; One view of a rig: launch order, edges, health, and the
;; rows that need a person.  The session dashboard stays a
;; list of sessions by project.  This command writes nothing
;; and starts nothing.

;;; Code:

(require 'subr-x)
(require 'harmless-team)
(require 'harmless-health)

(defvar harmless-directory)

(defconst harmless-fleet-buffer "*harmless-fleet*"
  "Buffer that shows one rig.")

(defun harmless-fleet--join (lines)
  "Return LINES as text, or \"none\"."
  (if lines
      (mapconcat #'identity lines "\n")
    "none"))

(defun harmless-fleet--text (report)
  "Return REPORT as the fleet view."
  (let* ((health (plist-get report :health))
         (topology
          (append (plist-get report :order)
                  (mapcar
                   (lambda (edge)
                     (format "%s %s -> %s"
                             (plist-get edge :kind)
                             (plist-get edge :from)
                             (plist-get edge :to)))
                   (plist-get report :edges))))
         (rows
          (append
           (mapcar (lambda (seat)
                     (format "%s %s"
                             (plist-get seat :address)
                             (plist-get seat :health)))
                   (plist-get health :seats))
           (mapcar (lambda (item)
                     (format "service %s %s"
                             (plist-get item :name)
                             (plist-get item :health)))
                   (plist-get health :services))))
         (people
          (mapcar (lambda (row)
                    (format "%s %s %s"
                            (plist-get row :id)
                            (plist-get row :seat)
                            (plist-get row :wake)))
                  (plist-get report :people))))
    (concat "Rig " (plist-get report :name) "\n"
            "\n"
            "Topology\n"
            (harmless-fleet--join topology) "\n"
            "\n"
            "Health\n"
            (harmless-fleet--join rows) "\n"
            "\n"
            "People\n"
            (harmless-fleet--join people) "\n")))

(defun harmless-fleet--show (text)
  "Replace the fleet buffer with TEXT.
Pop it when called interactively."
  (let ((buf (get-buffer-create harmless-fleet-buffer)))
    (with-current-buffer buf
      ;; Undo would arm a timer.  This view starts nothing.
      (let ((inhibit-read-only t)
            (buffer-undo-list t))
        (erase-buffer)
        (insert text)
        (goto-char (point-min)))
      (special-mode)
      (setq buffer-undo-list t))
    (when (called-interactively-p 'any)
      (pop-to-buffer buf))
    buf))

;;;###autoload
(defun harmless-team-fleet (rig)
  "Show RIG's topology, health, and rows that need a person.
The session dashboard is unchanged.  This writes nothing and
starts nothing.  Return the plist the view was built from."
  (interactive (list (read-string "Rig: ")))
  (setq rig (harmless-team--rig-name rig))
  (let ((report (list :name rig
                      :order (harmless-team-launch-order rig)
                      :edges (harmless-team-edges rig)
                      :health (harmless-team-health rig)
                      :people (harmless-team-board rig "waiting"))))
    (harmless-fleet--show (harmless-fleet--text report))
    (message "Fleet %s" rig)
    report))

(provide 'harmless-fleet)

;;; harmless-fleet.el ends here
