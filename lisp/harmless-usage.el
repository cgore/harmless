;;; harmless-usage.el --- Session and account usage for Harmless -*- lexical-binding: t; -*-

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
;; Two reports.  Session totals are prompt and completion tokens Harmless
;; has counted.  Account lines are the last rate-limit remainder a
;; provider sent.  Neither number is an account credit balance.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)

(declare-function harmless--context-session "harmless")

(defvar harmless-usage-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'harmless-usage-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `harmless-usage-mode'.")

(define-derived-mode harmless-usage-mode special-mode "Harmless-Usage"
  "Buffer showing session token totals and account rate limits."
  (setq-local revert-buffer-function #'harmless-usage-refresh))

(defun harmless-usage-note-turn (session prompt completion)
  "Add one turn's PROMPT and COMPLETION tokens to SESSION."
  (setq prompt (or prompt 0)
        completion (or completion 0))
  (unless (numberp (harmless-session-prompt-tokens session))
    (setf (harmless-session-prompt-tokens session) 0))
  (unless (numberp (harmless-session-completion-tokens session))
    (setf (harmless-session-completion-tokens session) 0))
  (setf (harmless-session-last-prompt-tokens session) prompt
        (harmless-session-last-completion-tokens session) completion)
  (cl-incf (harmless-session-prompt-tokens session) prompt)
  (cl-incf (harmless-session-completion-tokens session) completion))

(defun harmless-usage-limits-file ()
  "Return the path of the saved rate-limit report."
  (expand-file-name "limits.json" harmless-directory))

(defun harmless-usage-load-limits ()
  "Return saved rate-limit plists, one per provider."
  (let ((file (harmless-usage-limits-file)))
    (when (file-readable-p file)
      (harmless-json-decode
       (with-temp-buffer
         (insert-file-contents file)
         (buffer-string))))))

(defun harmless-usage-record-limits (provider-name plist)
  "Merge PLIST into the saved rate-limit report for PROVIDER-NAME."
  (when (and provider-name plist)
    (let* ((all (harmless-usage-load-limits))
           (old (cl-find provider-name all
                         :key (lambda (entry) (plist-get entry :provider))
                         :test #'equal))
           (merged (copy-sequence old)))
      (cl-loop for (key value) on plist by #'cddr
               do (setq merged (plist-put merged key value)))
      (setq merged (plist-put merged :provider provider-name))
      (setq merged (plist-put merged :updated (harmless-now-iso)))
      (harmless-ensure-directory harmless-directory)
      (with-temp-file (harmless-usage-limits-file)
        (setq buffer-file-coding-system 'utf-8-unix)
        (insert (harmless-json-text
                 (cons merged
                       (cl-remove provider-name all
                                  :key (lambda (entry) (plist-get entry :provider))
                                  :test #'equal)))
                "\n"))
      merged)))

(defun harmless-usage--rows ()
  "Return usage plists for live sessions and saved sessions not already live."
  (let ((seen (make-hash-table :test 'equal))
        rows)
    (dolist (session (harmless-session-list))
      (puthash (harmless-session-id session) t seen)
      (push (list :id (harmless-session-id session)
                  :title (or (harmless-session-title session)
                             (harmless-session-project-name session))
                  :provider (or (harmless-session-provider-name session) "?")
                  :model (or (harmless-session-model session) "?")
                  :prompt (or (harmless-session-prompt-tokens session) 0)
                  :completion (or (harmless-session-completion-tokens session) 0)
                  :last-prompt (or (harmless-session-last-prompt-tokens session) 0)
                  :last-completion (or (harmless-session-last-completion-tokens session) 0))
            rows))
    (dolist (pair (harmless-session-list-on-disk))
      (let* ((summary (cdr pair))
             (id (plist-get summary :id)))
        (unless (gethash id seen)
          (push (list :id id
                      :title (or (plist-get summary :title)
                                 (plist-get summary :cwd)
                                 id)
                      :provider (or (plist-get summary :provider) "?")
                      :model (or (plist-get summary :model) "?")
                      :prompt (or (plist-get summary :prompt-tokens) 0)
                      :completion (or (plist-get summary :completion-tokens) 0)
                      :last-prompt (or (plist-get summary :last-prompt-tokens) 0)
                      :last-completion (or (plist-get summary :last-completion-tokens) 0))
                rows))))
    (nreverse rows)))

(defun harmless-usage--limit-line (label remaining limit reset)
  "Return one rate-limit line, or an empty string when REMAINING is nil."
  (if (not remaining)
      ""
    (concat
     (format "    %s remaining %s" label remaining)
     (if limit (format " of %s" limit) "")
     (if reset (format "    reset %s" reset) "")
     "\n")))

(defun harmless-usage--account-block (name entry)
  "Return the account section for provider NAME and limit ENTRY."
  (concat
   (format "  %s\n" name)
   (if (not entry)
       "    no rate-limit report yet\n"
     (concat
      (harmless-usage--limit-line
       "requests"
       (plist-get entry :requests-remaining)
       (plist-get entry :requests-limit)
       (plist-get entry :requests-reset))
      (harmless-usage--limit-line
       "tokens"
       (plist-get entry :tokens-remaining)
       (plist-get entry :tokens-limit)
       (plist-get entry :tokens-reset))
      (harmless-usage--limit-line
       "input tokens"
       (plist-get entry :input-tokens-remaining)
       (plist-get entry :input-tokens-limit)
       (plist-get entry :input-tokens-reset))
      (harmless-usage--limit-line
       "output tokens"
       (plist-get entry :output-tokens-remaining)
       (plist-get entry :output-tokens-limit)
       (plist-get entry :output-tokens-reset))
      (format "    updated %s\n" (or (plist-get entry :updated) "?"))))))

(defun harmless-usage-report (&optional session)
  "Return the usage report, highlighting SESSION when given."
  (let* ((rows (harmless-usage--rows))
         (groups (make-hash-table :test 'equal))
         (prompt-total 0)
         (completion-total 0)
         (limits (harmless-usage-load-limits))
         (names nil)
         (text ""))
    (dolist (row rows)
      (let* ((provider (plist-get row :provider))
             (model (plist-get row :model))
             (key (concat provider "\0" model))
             (slot (or (gethash key groups)
                       (list :provider provider :model model
                             :sessions 0 :prompt 0 :completion 0))))
        (setq slot (plist-put slot :sessions (1+ (plist-get slot :sessions))))
        (setq slot (plist-put slot :prompt
                              (+ (plist-get slot :prompt) (plist-get row :prompt))))
        (setq slot (plist-put slot :completion
                              (+ (plist-get slot :completion)
                                 (plist-get row :completion))))
        (puthash key slot groups)
        (cl-incf prompt-total (plist-get row :prompt))
        (cl-incf completion-total (plist-get row :completion))))
    (setq text
          (concat
           "Session\n\n"
           (if (not session)
               "  No session selected.\n"
             (format "  %s\n  %s / %s\n  prompt %s   completion %s   total %s\n%s"
                     (or (harmless-session-title session)
                         (harmless-session-project-name session))
                     (or (harmless-session-provider-name session) "?")
                     (or (harmless-session-model session) "?")
                     (or (harmless-session-prompt-tokens session) 0)
                     (or (harmless-session-completion-tokens session) 0)
                     (+ (or (harmless-session-prompt-tokens session) 0)
                        (or (harmless-session-completion-tokens session) 0))
                     (if (> (or (harmless-session-last-prompt-tokens session) 0) 0)
                         (format "  last turn prompt %s   completion %s\n"
                                 (harmless-session-last-prompt-tokens session)
                                 (or (harmless-session-last-completion-tokens session) 0))
                       "")))
           "\nAll sessions\n\n"))
    (if (= (hash-table-count groups) 0)
        (setq text (concat text "  No saved sessions.\n"))
      (let (keys)
        (maphash (lambda (key _slot) (push key keys)) groups)
        (dolist (key (sort keys #'string<))
          (let ((slot (gethash key groups)))
            (setq text
                  (concat text
                          (format "  %s / %s    %d session%s    prompt %s   completion %s\n"
                                  (plist-get slot :provider)
                                  (plist-get slot :model)
                                  (plist-get slot :sessions)
                                  (if (= (plist-get slot :sessions) 1) "" "s")
                                  (plist-get slot :prompt)
                                  (plist-get slot :completion))))))
        (setq text
              (concat text
                      (format "\n  total prompt %s   completion %s\n"
                              prompt-total completion-total)))))
    (setq text (concat text "\nAccounts\n\n"))
    (dolist (provider harmless-providers)
      (let ((name (harmless-provider-name provider)))
        (push name names)
        (setq text
              (concat text
                      (harmless-usage--account-block
                       name
                       (cl-find name limits
                                :key (lambda (entry) (plist-get entry :provider))
                                :test #'equal))))))
    (dolist (entry limits)
      (let ((name (plist-get entry :provider)))
        (unless (member name names)
          (setq text (concat text (harmless-usage--account-block name entry))))))
    (unless (or harmless-providers limits)
      (setq text (concat text "  No connections configured.\n")))
    text))

(defun harmless-usage--fill (session)
  "Replace the current buffer with the usage report for SESSION."
  (let ((inhibit-read-only t)
        (point (point)))
    (erase-buffer)
    (insert (harmless-usage-report session))
    (goto-char (min point (point-max)))))

;;;###autoload
(defun harmless-usage ()
  "Show session token totals and the last rate-limit report for each account."
  (interactive)
  (let ((session (and (fboundp 'harmless--context-session)
                      (harmless--context-session))))
    (let ((buf (get-buffer-create "*Harmless Usage*")))
      (with-current-buffer buf
        (harmless-usage-mode)
        (setq-local harmless--usage-session session)
        (harmless-usage--fill session))
      (pop-to-buffer buf))))

(defun harmless-usage-refresh (&rest _)
  "Redraw the usage buffer."
  (interactive)
  (when (derived-mode-p 'harmless-usage-mode)
    (harmless-usage--fill (and (boundp 'harmless--usage-session)
                               harmless--usage-session))))

(provide 'harmless-usage)

;;; harmless-usage.el ends here
