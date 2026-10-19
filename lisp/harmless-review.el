;;; harmless-review.el --- Read-only review for Harmless -*- lexical-binding: t; -*-

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
;; `review' reads the working tree, a base branch, or one commit and
;; can record a list of findings.  Git is used only to read.  A
;; finding is not applied as an edit.  Nothing is written into the
;; project.  The latest review is saved with the session.  A fork
;; copies it.  A subagent starts without one.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-tools)

(declare-function harmless-ensure-configured "harmless")
(declare-function harmless--context-session "harmless")
(declare-function harmless-current "harmless")

(defconst harmless-review-severities '(high medium low)
  "Severity symbols a review finding can store.")

(defconst harmless-review--absent (make-symbol "absent")
  "Marker for a field the caller did not send.")

(defcustom harmless-review-max-bytes 100000
  "Maximum length of a review diff returned to the model.
A longer diff is cut and ends with \"[truncated]\".  Findings
are not cut.  Nil and a non-positive number use 100000."
  :type 'integer
  :group 'harmless)

(defvar harmless-review-function nil
  "Optional function that turns a review diff into findings.
Called as (SESSION ARGS DIFF).  Return a list of findings, or
nil to leave the stored review unchanged.  `M-x harmless-review'
records a list.  The review tool does not call this function.
Do not call `harmless-turn-run' from it, and do not edit the
project.")

(defun harmless-review--name (key)
  "Return the field name of KEY."
  (cond
   ((keywordp key) (substring (symbol-name key) 1))
   ((symbolp key) (symbol-name key))
   ((stringp key) key)
   (t "")))

(defun harmless-review--has (plist key)
  "Return non-nil when PLIST sends KEY.
A present null is a value.  A missing key is not."
  (when (and (listp plist) (proper-list-p plist))
    (let ((name (harmless-review--name key)))
      (cl-loop for (k) on plist by #'cddr
               thereis (and (or (symbolp k) (stringp k))
                            (string= (harmless-review--name k) name))))))

(defun harmless-review--get (plist key)
  "Return KEY from PLIST, accepting a keyword or a string."
  (when (harmless-review--has plist key)
    (let ((name (harmless-review--name key)))
      (cl-loop for (k v) on plist by #'cddr
               when (and (or (symbolp k) (stringp k))
                         (string= (harmless-review--name k) name))
               return v))))

(defun harmless-review--items-p (value)
  "Return non-nil when VALUE is a list of findings, including empty.
A plist is one finding, not a list of findings."
  (or (null value)
      (and (listp value)
           (proper-list-p value)
           (not (harmless-plist-p value)))))

(defun harmless-review--item-p (raw)
  "Return non-nil when RAW looks like one finding."
  (and (proper-list-p raw)
       (consp raw)
       (or (symbolp (car raw)) (stringp (car raw)))))

(defun harmless-review--root-text-p (text)
  "Return non-nil if TEXT is \"/\" or a drive root.
TEXT is not expanded.  A relative path and \"~\" are not roots."
  (and (stringp text)
       (or (equal text "/")
           (equal text "//")
           (equal text "/.")
           (string-match-p "\\`[A-Za-z]:[/\\]?\\'" text)
           (and (file-name-absolute-p text)
                (not (string-prefix-p "~" text))
                (harmless-filesystem-root-p text)))))

(defun harmless-review--name-p (text)
  "Return non-nil when TEXT may be given to git as one name.
An absolute path, \"..\", and a leading dash are refused here.
TEXT is not expanded."
  (and (stringp text)
       (not (string-empty-p text))
       (not (string-prefix-p "-" text))
       (not (string-prefix-p "~" text))
       (not (string-match-p "[\0\n\r]" text))
       (not (string-match-p "\\.\\." text))
       (not (string-search "\\" text))
       (not (string-search ":" text))
       (not (file-name-absolute-p text))
       (not (harmless-review--root-text-p text))))

(defun harmless-review--ref (value kind noun)
  "Return VALUE as a git name.
KIND is \"base\" or \"commit\".  NOUN is \"branch\" or \"commit\"."
  (cond
   ((null value) (error "review %s is empty" kind))
   ((not (stringp value))
    (error "review %s must be a string" kind))
   (t
    (setq value (string-trim value))
    (when (string-empty-p value)
      (error "review %s is empty" kind))
    (unless (harmless-review--name-p value)
      (error "review %s is not a %s" kind noun))
    value)))

(defun harmless-review--target (args)
  "Return (KIND BASE COMMIT) for ARGS, or signal.
KIND is `working-tree', `branch', or `commit'.  Git is not run.
Names are not expanded."
  (let ((base (if (harmless-review--has args :base)
                  (harmless-review--get args :base)
                harmless-review--absent))
        (commit (if (harmless-review--has args :commit)
                    (harmless-review--get args :commit)
                  harmless-review--absent)))
    (when (and (not (eq base harmless-review--absent))
               (not (eq commit harmless-review--absent)))
      (error "review has both a base and a commit"))
    (cond
     ((not (eq base harmless-review--absent))
      (list 'branch (harmless-review--ref base "base" "branch") nil))
     ((not (eq commit harmless-review--absent))
      (list 'commit nil
            (harmless-review--ref commit "commit" "commit")))
     (t (list 'working-tree nil nil)))))

(defun harmless-review--symbol (value)
  "Return a severity or kind symbol parsed from VALUE, or nil.
A value that is not a name returns nil.  The caller decides
whether that is an error."
  (cond
   ((or (keywordp value) (symbolp value))
    (setq value (symbol-name value))
    (when (string-prefix-p ":" value)
      (setq value (substring value 1))))
   ((stringp value) (setq value (string-trim value)))
   (t (setq value "")))
  (unless (string-empty-p value)
    (intern value)))

(defun harmless-review--severity (raw)
  "Return the severity symbol of RAW."
  (unless (harmless-review--has raw :severity)
    (error "review severity is not a severity"))
  (let ((value (harmless-review--get raw :severity)))
    (when (null value)
      (error "review severity is not a severity"))
    (let ((severity (harmless-review--symbol value)))
      (unless (memq severity harmless-review-severities)
        (error "review severity is not a severity"))
      severity)))

(defun harmless-review--one-line (value empty not-string)
  "Return VALUE as one trimmed line.
Signal EMPTY when it is blank and NOT-STRING when it is not text."
  (cond
   ((null value) (error "%s" empty))
   ((not (stringp value)) (error "%s" not-string))
   (t
    (setq value
          (string-trim
           (replace-regexp-in-string "[\n\r\t]+" " " value)))
    (when (string-empty-p value)
      (error "%s" empty))
    value)))

(defun harmless-review--text (raw)
  "Return the trimmed text of RAW."
  (unless (harmless-review--has raw :text)
    (error "review finding is empty"))
  (harmless-review--one-line
   (harmless-review--get raw :text)
   "review finding is empty"
   "review finding must be a string"))

(defun harmless-review--file (raw)
  "Return the file text of RAW, or nil when it was omitted.
A file is stored as text.  It is not resolved and not written."
  (when (harmless-review--has raw :file)
    (harmless-review--one-line
     (harmless-review--get raw :file)
     "review file is empty"
     "review file must be a string")))

(defun harmless-review--line-no (raw)
  "Return the line of RAW, or nil when it was omitted."
  (when (harmless-review--has raw :line)
    (let ((value (harmless-review--get raw :line)))
      (unless (and (integerp value) (> value 0))
        (error "review line is not a line"))
      value)))

(defun harmless-review--finding (raw)
  "Return one stored finding for RAW."
  (unless (harmless-review--item-p raw)
    (error "review finding must be an item"))
  (list :severity (harmless-review--severity raw)
        :file (harmless-review--file raw)
        :line (harmless-review--line-no raw)
        :text (harmless-review--text raw)))

(defun harmless-review--findings (raw)
  "Return the finding list RAW, or signal."
  (unless (harmless-review--items-p raw)
    (error "review findings must be a list"))
  (mapcar #'harmless-review--finding raw))

(defun harmless-review--project (session)
  "Return SESSION's project directory, or signal.
Nil, \"\", a relative path, \"~\", and a filesystem root are not
a project.  They are not expanded.  A missing directory is not
created and is not a git repository."
  (let ((cwd (harmless-session-cwd session)))
    (cond
     ((or (null cwd)
          (not (stringp cwd))
          (string-empty-p (if (stringp cwd) (string-trim cwd) ""))
          (not (file-name-absolute-p cwd))
          (string-prefix-p "~" cwd)
          (harmless-review--root-text-p (string-trim cwd)))
      (error "This chat is not attached to a project"))
     ((not (file-directory-p cwd))
      (error "this project is not a git repository"))
     (t cwd))))

(defun harmless-review--git (repo err &rest args)
  "Run git ARGS in REPO.  Signal ERR unless git exits 0.
Return the output without trailing newlines.  ARGS are read
commands.  This does not add, apply, checkout, or commit."
  (unless (and (stringp repo)
               (file-name-absolute-p repo)
               (not (string-prefix-p "~" repo))
               (not (harmless-review--root-text-p
                     (directory-file-name repo)))
               (not (harmless-filesystem-root-p repo)))
    (error "This chat is not attached to a project"))
  (let ((git (executable-find "git")))
    (unless git
      (error "git is not available"))
    (with-temp-buffer
      (let* ((process-environment
              (cons "GIT_TERMINAL_PROMPT=0" process-environment))
             (coding-system-for-read 'utf-8-unix)
             (exit (apply
                    #'call-process git nil
                    (current-buffer) nil
                    (append (list "-C" repo
                                  "--no-pager"
                                  "--no-optional-locks")
                            args))))
        (unless (eq exit 0)
          (error "%s" err))
        (replace-regexp-in-string
         "[\n\r]+\\'" ""
         (replace-regexp-in-string "\r\n" "\n" (buffer-string)))))))

(defun harmless-review--inside (repo)
  "Signal unless REPO is a git working tree.  Return REPO."
  (let ((out (harmless-review--git
              repo "this project is not a git repository"
              "rev-parse" "--is-inside-work-tree")))
    (unless (equal out "true")
      (error "this project is not a git repository"))
    repo))

(defun harmless-review--join (a b)
  "Return A and B as one diff, omitting an empty side."
  (setq a (or a "")
        b (or b ""))
  (cond
   ((and (string-empty-p a) (string-empty-p b)) "")
   ((string-empty-p a) b)
   ((string-empty-p b) a)
   (t (concat a "\n" b))))

(defun harmless-review--untracked (repo)
  "Return untracked paths in REPO.  Nothing is staged."
  (let ((out (harmless-review--git
              repo "review could not read the diff"
              "status" "--porcelain=v1"
              "--untracked-files=all"))
        (paths nil))
    (dolist (line (split-string out "[\n\r]+" t))
      (when (string-prefix-p "?? " line)
        (push (substring line 3) paths)))
    (nreverse paths)))

(defun harmless-review--body (label diff paths)
  "Return the review text for LABEL, DIFF, and untracked PATHS."
  (concat "Review target: " label "\n"
          (if (and (string-empty-p (or diff "")) (null paths))
              "No diff"
            (concat
             (if (string-empty-p (or diff "")) "No diff" diff)
             (if paths
                 (concat "\nUntracked:\n"
                         (mapconcat #'identity paths "\n"))
               "")))))

(defun harmless-review--working (repo)
  "Return the working tree review of REPO."
  (harmless-review--body
   "working tree"
   (harmless-review--join
    (harmless-review--git
     repo "review could not read the diff"
     "diff" "--no-ext-diff" "--no-color")
    (harmless-review--git
     repo "review could not read the diff"
     "diff" "--cached" "--no-ext-diff" "--no-color"))
   (harmless-review--untracked repo)))

(defun harmless-review--branch (repo base)
  "Return the review of REPO against branch BASE."
  (harmless-review--git
   repo "review base is not a branch"
   "show-ref" "--verify" "--quiet"
   (concat "refs/heads/" base))
  (harmless-review--body
   (concat "branch " base)
   (harmless-review--git
    repo "review could not read the diff"
    "diff" "--no-ext-diff" "--no-color"
    (concat base "...HEAD"))
   nil))

(defun harmless-review--commit (repo commit)
  "Return the review of one COMMIT in REPO."
  (harmless-review--git
   repo "review commit is not a commit"
   "rev-parse" "--verify" "--end-of-options"
   (concat commit "^{commit}"))
  (harmless-review--body
   (concat "commit " commit)
   (harmless-review--git
    repo "review commit is not a commit"
    "show" "--no-ext-diff" "--no-color"
    "--format=medium" "-p" "--end-of-options" commit)
   nil))

(defun harmless-review--read (session kind base commit)
  "Return the diff for SESSION's KIND, BASE, and COMMIT."
  (let ((repo (harmless-review--inside
               (harmless-review--project session))))
    (cond
     ((eq kind 'branch) (harmless-review--branch repo base))
     ((eq kind 'commit) (harmless-review--commit repo commit))
     (t (harmless-review--working repo)))))

(defun harmless-review--cut (text)
  "Cut TEXT to `harmless-review-max-bytes' characters."
  (let ((max harmless-review-max-bytes))
    (unless (and (integerp max) (> max 0))
      (setq max 100000))
    (if (<= (length text) max)
        text
      (concat (substring text 0 max) "\n[truncated]"))))

(defun harmless-review--format-line (item)
  "Return one display line for finding ITEM."
  (let ((file (plist-get item :file))
        (line (plist-get item :line))
        (where ""))
    (cond
     ((and file line)
      (setq where (format "%s:%d: " file line)))
     (file (setq where (format "%s: " file)))
     (line (setq where (format "%d: " line))))
    (format "[%s] %s%s"
            (symbol-name (plist-get item :severity))
            where
            (plist-get item :text))))

(defun harmless-review-format (findings)
  "Return the text of FINDINGS, or \"No findings\" when none."
  (if findings
      (mapconcat #'harmless-review--format-line findings "\n")
    "No findings"))

(defun harmless-review-header (session)
  "Return a header suffix for SESSION's findings, or \"\"."
  (let ((n (length (plist-get (harmless-session-review session)
                              :findings))))
    (if (> n 0)
        (format "  review %d" n)
      "")))

(defun harmless-review--store (session kind base commit findings)
  "Store FINDINGS for KIND on SESSION and return their text.
Save only when the stored review changes.  The project is not
edited."
  (let ((next (list :kind kind
                    :base base
                    :commit commit
                    :findings findings)))
    (unless (equal next (harmless-session-review session))
      (setf (harmless-session-review session) next)
      (harmless-session-save session))
    (harmless-review-format findings)))

(defun harmless-review-run (session args)
  "Review SESSION from ARGS.
Without findings, return the diff and leave the stored review
unchanged.  With findings, store that list and return it.  Git
only reads.  A finding is not applied as an edit."
  (unless (harmless-session-p session)
    (error "No Harmless session"))
  (let* ((target (harmless-review--target args))
         (kind (nth 0 target))
         (base (nth 1 target))
         (commit (nth 2 target))
         (raw (if (harmless-review--has args :findings)
                  (harmless-review--get args :findings)
                harmless-review--absent))
         (findings (unless (eq raw harmless-review--absent)
                     (harmless-review--findings raw)))
         (diff (harmless-review--cut
                (harmless-review--read
                 session kind base commit))))
    (if (eq raw harmless-review--absent)
        diff
      (harmless-review--store session kind base commit findings))))

(defun harmless-review-summary (session)
  "Return a :review entry, or nil when SESSION has none."
  (let ((review (harmless-session-review session)))
    (when review
      (list :review (copy-tree review)))))

(defun harmless-review--saved-target-p (kind base commit)
  "Return non-nil when KIND, BASE, and COMMIT can be kept."
  (cond
   ((eq kind 'working-tree) (and (null base) (null commit)))
   ((eq kind 'branch)
    (and (stringp base)
         (harmless-review--name-p base)
         (null commit)))
   ((eq kind 'commit)
    (and (stringp commit)
         (harmless-review--name-p commit)
         (null base)))
   (t nil)))

(defun harmless-review--clean-finding (raw)
  "Return a stored finding for RAW, or nil when RAW is not usable."
  (condition-case nil
      (when (harmless-review--item-p raw)
        (harmless-review--finding raw))
    (error nil)))

(defun harmless-review--kept (kind base commit findings)
  "Return one stored review plist."
  (list :kind kind
        :base (and (eq kind 'branch) base)
        :commit (and (eq kind 'commit) commit)
        :findings findings))

(defun harmless-review--from-summary (raw)
  "Return (REVIEW . DROPPED) for saved RAW.
DROPPED is non-nil when a bad target or finding was removed.
Nil drops nothing."
  (cond
   ((null raw) (cons nil nil))
   ((not (and (harmless-plist-p raw) (proper-list-p raw)))
    (cons nil t))
   (t
    (let* ((kind (harmless-review--symbol
                  (when (harmless-review--has raw :kind)
                    (harmless-review--get raw :kind))))
           (base (when (harmless-review--has raw :base)
                   (harmless-review--get raw :base)))
           (commit (when (harmless-review--has raw :commit)
                     (harmless-review--get raw :commit)))
           (findings (when (harmless-review--has raw :findings)
                       (harmless-review--get raw :findings))))
      (cond
       ((not (memq kind '(working-tree branch commit)))
        (cons nil t))
       ((not (harmless-review--saved-target-p kind base commit))
        (cons nil t))
       ((not (harmless-review--items-p findings))
        (cons (harmless-review--kept kind base commit nil) t))
       (t
        (let ((kept nil)
              (dropped nil))
          (dolist (item findings)
            (let ((clean (harmless-review--clean-finding item)))
              (if clean
                  (push clean kept)
                (setq dropped t))))
          (cons (harmless-review--kept
                 kind base commit (nreverse kept))
                dropped))))))))

(defun harmless-review-restore (session raw)
  "Keep a usable review from RAW on SESSION.
A bad target drops the review.  A bad finding is dropped.  Save
when something was dropped.  This does not edit the project."
  (let* ((parsed (harmless-review--from-summary raw))
         (review (car parsed)))
    (setf (harmless-session-review session) review)
    (when (cdr parsed)
      (condition-case nil
          (harmless-session-save session)
        (error nil)))
    review))

(defun harmless-review-tool (session args)
  "review implementation."
  (harmless-review-run session args))

(defun harmless-review-read-args ()
  "Read a working tree, a branch, or a commit from the minibuffer."
  (let ((choice (completing-read
                 "Review: "
                 '("working tree" "branch" "commit")
                 nil t nil nil "working tree")))
    (cond
     ((equal choice "branch")
      (list :base (read-string "Base branch: ")))
     ((equal choice "commit")
      (list :commit (read-string "Commit: ")))
     (t nil))))

;;;###autoload
(defun harmless-review (&optional session args)
  "Review SESSION's working tree, a base branch, or one commit.
ARGS is a plist :base, :commit, and optional :findings.  Show
the diff when findings are omitted.  Record findings when they
are passed, or when `harmless-review-function' returns a list.
The project is not edited.  Findings are not applied as edits."
  (interactive (list nil (harmless-review-read-args)))
  (cond
   ((harmless-session-p session) nil)
   ((and (called-interactively-p 'any) (null session))
    (harmless-ensure-configured)
    (unless (harmless--context-session)
      (harmless-current))
    (setq session (harmless--context-session)))
   (t (error "No Harmless session")))
  (unless (harmless-session-p session)
    (error "No Harmless session"))
  (let ((result
         (if (and (functionp harmless-review-function)
                  (not (harmless-review--has args :findings)))
             (let* ((target (harmless-review--target args))
                    (diff (harmless-review--cut
                           (harmless-review--read
                            session
                            (nth 0 target)
                            (nth 1 target)
                            (nth 2 target))))
                    (found (funcall harmless-review-function
                                    session args diff)))
               (if (null found)
                   diff
                 (harmless-review-run
                  session
                  (append (list :findings found)
                          (copy-tree args)))))
           (harmless-review-run session args))))
    (when (called-interactively-p 'any)
      (message "%s" result))
    result))

(defun harmless-review-register ()
  "Register the review tool."
  (harmless-register-tool
   (harmless-tool-create
    :name "review"
    :description "Read the working tree, a base branch, or one commit. Omit base and commit to read the working tree. base is a branch name. commit is one commit. findings records a list of severity, file, line, and text. The list is the result of the review. Nothing is applied as an edit and nothing is written into the project."
    :class 'read
    :schema '(:type "object"
              :properties
              (:base (:type "string"
                      :description "Branch to review against.")
               :commit (:type "string"
                        :description "One commit to review.")
               :findings
               (:type "array"
                :description "Findings to record. Omit to read the diff."
                :items (:type "object"
                        :properties
                        (:severity (:type "string")
                         :file (:type "string")
                         :line (:type "integer")
                         :text (:type "string"))))))
    :fn #'harmless-review-tool)))

(harmless-review-register)

(provide 'harmless-review)

;;; harmless-review.el ends here
