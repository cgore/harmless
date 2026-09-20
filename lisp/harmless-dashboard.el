;;; harmless-dashboard.el --- Session list for Harmless -*- lexical-binding: t; -*-

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
;; Tabulated list of live and saved sessions, grouped by project.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-ui)

(declare-function harmless-new "harmless")

(defvar harmless-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'harmless-dashboard-open)
    (define-key map (kbd "n") #'harmless-new)
    (define-key map (kbd "g") #'harmless-dashboard-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `harmless-dashboard-mode'.")

(defun harmless-dashboard--entries ()
  "Return tabulated-list entries for live and saved sessions."
  (let ((seen (make-hash-table :test 'equal))
        entries)
    (dolist (s (harmless-session-list))
      (puthash (harmless-session-id s) t seen)
      (push (harmless-dashboard--entry-from-session s) entries))
    (dolist (pair (harmless-session-list-on-disk))
      (let* ((summary (cdr pair))
             (id (plist-get summary :id)))
        (unless (gethash id seen)
          (push (harmless-dashboard--entry-from-summary (car pair) summary)
                entries))))
    (nreverse entries)))

(defun harmless-dashboard--entry-from-session (s)
  "Tabulated entry for live session S."
  (list (harmless-session-id s)
        (vector (harmless-session-project-name s)
                (or (harmless-session-title s) "(untitled)")
                (harmless-session-model-label s)
                (format "%s" (harmless-session-status s))
                (or (harmless-session-updated-at s) ""))))

(defun harmless-dashboard--entry-from-summary (dir summary)
  "Tabulated entry for saved SUMMARY in DIR."
  (ignore dir)
  (let ((cwd (plist-get summary :cwd)))
    (list (plist-get summary :id)
          (vector (if cwd
                      (file-name-nondirectory (directory-file-name cwd))
                    "?")
                  (or (plist-get summary :title) "(untitled)")
                  (harmless-model-label (plist-get summary :model)
                                        (or (plist-get summary :reasoning-effort)
                                            harmless-default-reasoning-effort))
                  "saved"
                  (or (plist-get summary :updated-at) "")))))

(defun harmless-dashboard--groups ()
  "Return `tabulated-list-groups' from current entries."
  (let ((entries (harmless-dashboard--entries))
        (buckets (make-hash-table :test 'equal))
        names groups)
    (dolist (entry entries)
      (let ((proj (aref (cadr entry) 0)))
        (unless (gethash proj buckets)
          (push proj names)
          (puthash proj nil buckets))
        (push entry (gethash proj buckets))))
    (dolist (name (nreverse names))
      (push (cons name (nreverse (gethash name buckets))) groups))
    (nreverse groups)))

(define-derived-mode harmless-dashboard-mode tabulated-list-mode "Harmless-Dash"
  "Major mode for the Harmless session list."
  :interactive nil
  (setq tabulated-list-format
        [("Project" 18 t)
         ("Title" 36 t)
         ("Model" 16 t)
         ("Status" 12 t)
         ("Updated" 20 t)]
        tabulated-list-padding 2
        tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook #'harmless-dashboard--refresh-local nil t)
  (harmless-dashboard--refresh-local)
  (tabulated-list-init-header))

(defun harmless-dashboard--refresh-local ()
  "Recompute dashboard entries in the current buffer."
  (let ((groups (harmless-dashboard--groups)))
    (setq tabulated-list-groups groups
          tabulated-list-entries
          (apply #'append (mapcar #'cdr groups)))))

;;;###autoload
(defun harmless-dashboard ()
  "Show the Harmless session dashboard."
  (interactive)
  (let ((buf (get-buffer-create "*harmless*")))
    (with-current-buffer buf
      (harmless-dashboard-mode)
      (tabulated-list-print))
    (pop-to-buffer buf)))

(defun harmless-dashboard-refresh ()
  "Refresh the dashboard."
  (interactive)
  (harmless-dashboard--refresh-local)
  (tabulated-list-print t))

(defun harmless-dashboard-open ()
  "Open the session on this dashboard line."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id
      (user-error "No session here"))
    (harmless-ui-open-session (harmless-session-resume id))))

(provide 'harmless-dashboard)

;;; harmless-dashboard.el ends here
