;;; harmless-plan.el --- Plan mode for Harmless -*- lexical-binding: t; -*-

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
;; Plan mode lets the model explore the project and write `plan.md' in
;; the session directory.  `write_file' and `replace' fail until the
;; user approves the plan.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-session)
(require 'harmless-tools)

(declare-function harmless-ensure-configured "harmless")
(declare-function harmless--context-session "harmless")
(declare-function harmless-current "harmless")

(defvar harmless-plan-enter-function
  (lambda (_session)
    (and (not noninteractive)
         (y-or-n-p "Harmless: enter plan mode? ")))
  "Function (SESSION) that returns non-nil to allow entering plan mode.")

(defvar harmless-plan-decide-function #'harmless-plan-decide-default
  "Function (SESSION PLAN-TEXT) deciding how to leave plan mode.
Return `approve', `quit', or a string of revision notes.")

(defun harmless-plan-file (session)
  "Return the absolute path of SESSION's plan file."
  (expand-file-name "plan.md" (harmless-session-dir session)))

(defun harmless-plan-instructions ()
  "System-prompt text used while plan mode is on."
  "Plan mode is active. Explore the project with read-only tools. Write the plan only with write_plan. Include why the change is needed, the approach, the files to modify, existing code to reuse, and how to verify. When the plan is ready, call exit_plan_mode. Do not call write_file or replace.")

(defun harmless-plan-enter (session)
  "Turn plan mode on for SESSION and save it."
  (setf (harmless-session-plan-mode session) t)
  (harmless-session-save session)
  session)

(defun harmless-plan-exit (session)
  "Turn plan mode off for SESSION and save it."
  (setf (harmless-session-plan-mode session) nil)
  (harmless-session-save session)
  session)

(defun harmless-plan--read (session)
  "Return SESSION's plan text, or an empty string."
  (let ((file (harmless-plan-file session)))
    (if (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (buffer-string))
      "")))

(defun harmless-plan-decide-default (session plan)
  "Show PLAN for SESSION and return the user's decision."
  (ignore session)
  (if noninteractive
      'quit
    (let ((buf (get-buffer-create "*Harmless Plan*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (special-mode)
          (let ((inhibit-read-only t))
            (insert (if (string-empty-p (string-trim plan))
                        "(No plan written yet.)\n"
                      plan)))))
      (pop-to-buffer buf)
      (pcase (read-char-choice
              "Plan: a approve and build, s request changes, q quit "
              '(?a ?s ?q))
        (?a 'approve)
        (?q 'quit)
        (?s (read-string "Plan changes: "))))))

(defun harmless-plan--tool-write (session args)
  "write_plan implementation."
  (unless (harmless-session-plan-mode session)
    (error "Plan mode is off"))
  (let ((contents (or (harmless-tool-arg args :contents)
                      (harmless-tool-arg args :content)
                      "")))
    (when (string-empty-p (string-trim contents))
      (error "Plan is empty"))
    (let ((file (harmless-plan-file session)))
      (write-region contents nil file nil 'silent)
      (format "Wrote plan.md (%d bytes)" (string-bytes contents)))))

(defun harmless-plan--tool-enter (session _args)
  "enter_plan_mode implementation."
  (if (harmless-session-plan-mode session)
      "Plan mode is already on."
    (if (funcall harmless-plan-enter-function session)
        (progn
          (harmless-plan-enter session)
          "Plan mode is on. Explore the project and write the plan with write_plan.")
      "The user declined plan mode. Continue in normal mode.")))

(defun harmless-plan--tool-exit (session _args)
  "exit_plan_mode implementation."
  (let ((decision (funcall harmless-plan-decide-function
                           session
                           (harmless-plan--read session))))
    (pcase decision
      ('approve
       (harmless-plan-exit session)
       "Approved. Implement the plan now.")
      ('quit
       (harmless-plan-exit session)
       "The user quit the plan. Stop without changing project files.")
      (_
       (format "Revise the plan. Notes: %s" decision)))))

(defun harmless-plan-register ()
  "Register plan-mode tools."
  (harmless-register-tool
   (harmless-tool-create
    :name "write_plan"
    :description "Write the session plan. CONTENTS is the full Markdown plan. Only available in plan mode."
    :class 'read
    :schema '(:type "object"
              :properties (:contents (:type "string"))
              :required ["contents"])
    :fn #'harmless-plan--tool-write))
  (harmless-register-tool
   (harmless-tool-create
    :name "enter_plan_mode"
    :description "Ask to enter plan mode before implementing an ambiguous change. Do not use this for a task whose implementation is already clear."
    :class 'read
    :schema '(:type "object")
    :fn #'harmless-plan--tool-enter))
  (harmless-register-tool
   (harmless-tool-create
    :name "exit_plan_mode"
    :description "Present the written plan for approval. Call this after write_plan."
    :class 'read
    :schema '(:type "object")
    :fn #'harmless-plan--tool-exit)))

(harmless-plan-register)

;;;###autoload
(defun harmless-plan (&optional off)
  "Turn plan mode on for the current session.
With a prefix argument, or OFF non-nil, turn it off."
  (interactive "P")
  (harmless-ensure-configured)
  (unless (harmless--context-session)
    (harmless-current))
  (let ((session (harmless--context-session)))
    (unless session
      (error "No Harmless session"))
    (if off
        (harmless-plan-exit session)
      (harmless-plan-enter session))
    (message "Plan mode %s"
             (if (harmless-session-plan-mode session) "on" "off"))))

(provide 'harmless-plan)

;;; harmless-plan.el ends here
