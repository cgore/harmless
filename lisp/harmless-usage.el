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
(declare-function harmless-xai-allowance "harmless-xai")
(declare-function harmless-xai--parse-time "harmless-xai")

(defcustom harmless-usage-context-windows
  '(("grok-4.7" . 500000)
    ("grok-4.6" . 500000)
    ("grok-4.5" . 500000)
    ("grok-4.3" . 1000000))
  "Context window size in tokens, by model name.
The provider does not send this on a reply.  Remaining context is the
window minus the last request's prompt size."
  :type '(alist :key-type string :value-type integer)
  :group 'harmless)

(defun harmless-usage--commas (n)
  "Return N grouped with thousands separators."
  (let ((s (number-to-string n))
        (out ""))
    (while (> (length s) 3)
      (setq out (concat "," (substring s -3) out)
            s (substring s 0 -3)))
    (concat s out)))

(defun harmless-usage-context-window (model)
  "Return the context window for MODEL, or nil if it is unknown."
  (cdr (assoc model harmless-usage-context-windows)))

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

(defun harmless-usage--limit-line (label remaining limit reset)
  "Return one rate-limit line, or an empty string when REMAINING is nil."
  (if (not remaining)
      ""
    (concat
     (format "    %s remaining %s" label remaining)
     (if limit (format " of %s" limit) "")
     (if reset (format "    reset %s" reset) "")
     "\n")))

(defun harmless-usage--bar (percent width)
  "Return a WIDTH-character bar filled to PERCENT."
  (let* ((pct (max 0 (min 100 (or percent 0))))
         (filled (round (* width (/ pct 100.0))))
         (empty (- width filled)))
    (concat "["
            (make-string filled ?#)
            (make-string empty ?-)
            "]")))

(defun harmless-usage--reset-line (iso)
  "Return a reset description for ISO timestamp ISO."
  (let ((end (and (fboundp 'harmless-xai--parse-time)
                  (harmless-xai--parse-time iso))))
    (if (not end)
        (format "resets %s" iso)
      (let ((secs (float-time (time-subtract end (current-time)))))
        (if (<= secs 0)
            (format "resets now (%s)" iso)
          (format "resets in %dd %dh (%s)"
                  (floor (/ secs 86400))
                  (floor (/ (mod (floor secs) 86400) 3600))
                  iso))))))

(defun harmless-usage--allowance-lines (info)
  "Return the Grok allowance lines for INFO, or nil."
  (when info
    (let ((pct (plist-get info :used-percent))
          (end (plist-get info :period-end))
          (kind (downcase (or (plist-get info :period-type) "usage"))))
      (concat
       (when (plist-get info :tier)
         (format "    plan %s\n" (plist-get info :tier)))
       (when pct
         (format "    used %.0f%% of the %s limit\n    %s\n"
                 pct kind (harmless-usage--bar pct 24)))
       (when end
         (format "    %s\n" (harmless-usage--reset-line end)))
       (mapconcat
        (lambda (item)
          (format "    %s %.0f%%\n"
                  (or (plist-get item :product) "?")
                  (or (plist-get item :percent) 0)))
        (plist-get info :products)
        "")))))

(defun harmless-usage--account-block (name entry &optional allowance)
  "Return the account section for provider NAME and limit ENTRY.
ALLOWANCE is the Grok pool text, or nil."
  (concat
   (format "  %s\n" name)
   (or allowance "")
   (cond
    (entry
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
      (format "    updated %s\n" (or (plist-get entry :updated) "?"))))
    (allowance "")
    (t "    no allowance report yet\n"))))

(defun harmless-usage--session-block (session)
  "Return the context and this-chat section for SESSION."
  (if (not session)
      "No session selected.\n"
    (let* ((model (or (harmless-session-model session) "?"))
           (window (harmless-usage-context-window model))
           (last-prompt (or (harmless-session-last-prompt-tokens session) 0))
           (prompt (or (harmless-session-prompt-tokens session) 0))
           (completion (or (harmless-session-completion-tokens session) 0)))
      (format "%s\n%s    %s\n\nContext\n  window        %s\n  last prompt   %s\n  remaining     %s\n\nThis chat\n  prompt        %s\n  completion    %s\n"
              (or (harmless-session-title session)
                  (harmless-session-project-name session))
              (or (harmless-session-provider-name session) "?")
              model
              (if window (harmless-usage--commas window) "unknown")
              (if (> last-prompt 0) (harmless-usage--commas last-prompt) "—")
              (if (and window (> last-prompt 0))
                  (harmless-usage--commas (max 0 (- window last-prompt)))
                "—")
              (harmless-usage--commas prompt)
              (harmless-usage--commas completion)))))

(defun harmless-usage-report (&optional session)
  "Return the usage report for SESSION and the configured accounts."
  (let ((limits (harmless-usage-load-limits))
        (names nil)
        (text (concat (harmless-usage--session-block session) "\nAccounts\n\n")))
    (dolist (provider harmless-providers)
      (let* ((name (harmless-provider-name provider))
             (allowance (and (not noninteractive)
                             (equal name "xAI")
                             (fboundp 'harmless-xai-allowance)
                             (harmless-usage--allowance-lines
                              (harmless-xai-allowance)))))
        (push name names)
        (setq text
              (concat text
                      (harmless-usage--account-block
                       name
                       (cl-find name limits
                                :key (lambda (entry) (plist-get entry :provider))
                                :test #'equal)
                       allowance)))))
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
