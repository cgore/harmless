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
;; the session directory.  `harmless-tool-write-file' and `harmless-tool-replace' fail until the
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

(defvar harmless-plan-read-comment-function
  (lambda () (read-string "Plan comment: "))
  "Function that returns the text of one plan comment.")

(defvar harmless-plan-read-notes-function
  (lambda () (read-string "Plan changes: "))
  "Function that returns freeform revision notes.")

(defun harmless-plan-file (session)
  "Return the absolute path of SESSION's plan file."
  (expand-file-name "plan.md" (harmless-session-dir session)))

(defun harmless-plan-instructions ()
  "System-prompt text used while plan mode is on."
  "Plan mode is active. Explore the project with read-only tools. Write the plan only with harmless-tool-write-plan. Include why the change is needed, the approach, the files to modify, existing code to reuse, and how to verify. When the plan is ready, call harmless-tool-exit-plan-mode. Do not call harmless-tool-write-file, harmless-tool-replace, or harmless-tool-apply-patch. A shell command that can write is refused.")

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

(defun harmless-plan-line-count (plan)
  "Return how many lines PLAN has.
Nil and \"\" have none.  A trailing newline does not add a line."
  (cond
   ((null plan) 0)
   ((not (stringp plan)) (error "plan must be a string"))
   ((string-empty-p plan) 0)
   (t
    (let ((n (length (split-string plan "\n"))))
      (if (string-suffix-p "\n" plan)
          (1- n)
        n)))))

(defun harmless-plan--line (value count)
  "Return VALUE when it is a line in a plan of COUNT lines."
  (if (and (integerp value) (> value 0) (<= value count))
      value
    (error "plan comment line is not a line")))

(defun harmless-plan-comment (plan start end text)
  "Return a comment for lines START through END of PLAN.
Lines count from 1.  END nil comments only on START.  Nil, \"\",
and whitespace signal \"plan comment is empty\".  A non-string
signals \"plan comment must be a string\".  A missing line, a
line past the end, and a value that is not a positive integer
signal \"plan comment line is not a line\".  An end before the
start signals \"plan comment range is backwards\".  \"/\" and
\"c:/\" are comments.  Nothing is written."
  (when (and plan (not (stringp plan)))
    (error "plan must be a string"))
  (let* ((count (harmless-plan-line-count plan))
         (start (harmless-plan--line start count))
         (end (if (null end)
                  start
                (harmless-plan--line end count))))
    (when (< end start)
      (error "plan comment range is backwards"))
    (cond
     ((null text) (error "plan comment is empty"))
     ((not (stringp text))
      (error "plan comment must be a string")))
    (setq text (string-trim text))
    (when (string-empty-p text)
      (error "plan comment is empty"))
    (list :start start :end end :text text)))

(defun harmless-plan-format-comments (comments)
  "Return revision text for COMMENTS, or nil when there are none.
One line is \"Line N: TEXT\".  A range is \"Lines N-M: TEXT\"."
  (when comments
    (mapconcat
     (lambda (comment)
       (let ((start (plist-get comment :start))
             (end (plist-get comment :end))
             (text (plist-get comment :text)))
         (if (equal start end)
             (format "Line %d: %s" start text)
           (format "Lines %d-%d: %s" start end text))))
     comments
     "\n")))

(defun harmless-plan-review-notes (comments notes)
  "Return revision notes from COMMENTS and freeform NOTES.
NOTES nil is no freeform text.  A non-string signals
\"plan notes must be a string\".  \"/\" and \"c:/\" stay text."
  (let ((body (harmless-plan-format-comments comments))
        (extra (cond
                ((null notes) "")
                ((not (stringp notes))
                 (error "plan notes must be a string"))
                (t (string-trim notes)))))
    (cond
     ((and body (not (string-empty-p extra)))
      (concat body "\n" extra))
     (body body)
     (t extra))))

(defun harmless-plan--buffer-lines ()
  "Return (START . END) for the region, or the line at point."
  (if (use-region-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (last (if (> end beg) (1- end) end)))
        (cons (line-number-at-pos beg)
              (line-number-at-pos last)))
    (let ((line (line-number-at-pos (point))))
      (cons line line))))

(defun harmless-plan-comment-at-point (plan)
  "Comment on PLAN at point, or on the active region.
The comment text comes from `harmless-plan-read-comment-function'."
  (let ((range (harmless-plan--buffer-lines)))
    (harmless-plan-comment
     plan (car range) (cdr range)
     (funcall harmless-plan-read-comment-function))))

(defun harmless-plan-review (plan)
  "Return a decision for PLAN shown in the current buffer.
`a' approves.  `q' quits.  `c' comments on the line at point or
the active region.  `s' returns the comments and any freeform
notes.  A comment does not change the plan."
  (let ((comments nil)
        (done nil)
        (decision nil))
    (while (not done)
      (pcase (read-char-choice
              "Plan: a approve and build, c comment, s request changes, q quit "
              '(?a ?c ?s ?q))
        (?a (setq decision 'approve
                  done t))
        (?q (setq decision 'quit
                  done t))
        (?c (push (harmless-plan-comment-at-point plan) comments))
        (?s (setq decision
                  (harmless-plan-review-notes
                   (nreverse comments)
                   (funcall harmless-plan-read-notes-function))
                  done t))))
    decision))

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
            (insert (if (and (stringp plan)
                             (string-empty-p (string-trim plan)))
                        "(No plan written yet.)\n"
                      (or plan ""))))))
      (pop-to-buffer buf)
      (with-current-buffer buf
        (harmless-plan-review plan)))))

(defun harmless-plan--tool-write (session args)
  "harmless-tool-write-plan implementation."
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
  "harmless-tool-enter-plan-mode implementation."
  (if (harmless-session-plan-mode session)
      "Plan mode is already on."
    (if (funcall harmless-plan-enter-function session)
        (progn
          (harmless-plan-enter session)
          "Plan mode is on. Explore the project and write the plan with harmless-tool-write-plan.")
      "The user declined plan mode. Continue in normal mode.")))

(defun harmless-plan--tool-exit (session _args)
  "harmless-tool-exit-plan-mode implementation."
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
    :name "harmless-tool-write-plan"
    :description "Write the session plan. CONTENTS is the full Markdown plan. Only available in plan mode."
    :class 'read
    :schema '(:type "object"
              :properties (:contents (:type "string"))
              :required ["contents"])
    :fn #'harmless-plan--tool-write))
  (harmless-register-tool
   (harmless-tool-create
    :name "harmless-tool-enter-plan-mode"
    :description "Ask to enter plan mode before implementing an ambiguous change. Do not use this for a task whose implementation is already clear."
    :class 'read
    :schema '(:type "object")
    :fn #'harmless-plan--tool-enter))
  (harmless-register-tool
   (harmless-tool-create
    :name "harmless-tool-exit-plan-mode"
    :description "Present the written plan for approval. Call this after harmless-tool-write-plan."
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
