;;; harmless-refs.el --- Prompt context references for Harmless -*- lexical-binding: t; -*-

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
;; A prompt can name @file:, @folder:, @diff, @staged, @git:N, and
;; @url:.  The typed text stays.  The attachment follows under
;; "--- Attached Context ---".  A file or folder must stay inside
;; the project or an extra directory.  An attachment over half the
;; context window is left off.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-tools-fs)
(require 'harmless-web)

(defconst harmless-refs-header "--- Attached Context ---"
  "Heading placed before prompt attachments.")

(defvar harmless-refs-folder-max 200
  "Most entries one @folder: listing includes.")

(defvar harmless-refs-git-max 10
  "Most commits @git:N will include.")

(defvar harmless-refs-window-function #'harmless-refs-default-window
  "Function (SESSION) returning the context window in tokens.")

(defun harmless-refs-default-window (session)
  "Return SESSION's context window, or 128000 when it is unknown."
  (let ((model (and (harmless-session-p session)
                    (harmless-session-model session)))
        (reader (and (fboundp 'harmless-usage-context-window)
                     (symbol-function 'harmless-usage-context-window))))
    (or (and reader (funcall reader model))
        128000)))

(defun harmless-refs--tokens (text)
  "Return how many tokens TEXT is.
Four characters are one token.  Empty text is zero."
  (if (or (not (stringp text)) (string-empty-p text))
      0
    (ceiling (length text) 4)))

(defun harmless-refs--window (session)
  "Return a positive context window for SESSION."
  (let ((n (funcall harmless-refs-window-function session)))
    (if (and (integerp n) (> n 0))
        n
      128000)))

(defun harmless-refs--cap (text)
  "Cut TEXT at `harmless-read-file-max-bytes'."
  (if (and (stringp text) (> (length text) harmless-read-file-max-bytes))
      (concat (substring text 0 harmless-read-file-max-bytes) "\n[truncated]")
    (or text "")))

(defun harmless-refs--boundary-p (text pos)
  "Return non-nil if POS in TEXT can start an @ reference."
  (or (= pos 0)
      (memq (aref text (1- pos)) '(?\s ?\t ?\n ?\r))))

(defun harmless-refs--dot-path-p (value)
  "Return non-nil if VALUE ends in a . or .. path component.
A sentence period is not one of those components."
  (or (member value '("." ".."))
      (string-suffix-p "/." value)
      (string-suffix-p "/.." value)
      (string-suffix-p "\\." value)
      (string-suffix-p "\\.." value)))

(defun harmless-refs--strip (value)
  "Drop trailing punctuation from VALUE.
A final . or .. path component stays.  A sentence period does not."
  (when (stringp value)
    (while (and (> (length value) 0)
                (memq (aref value (1- (length value))) '(?, ?\; ?! ??)))
      (setq value (substring value 0 -1)))
    (while (and (> (length value) 0)
                (eq (aref value (1- (length value))) ?.)
                (not (harmless-refs--dot-path-p value)))
      (setq value (substring value 0 -1))))
  value)

(defun harmless-refs--take-value (text pos)
  "Read a reference value from TEXT at POS.
Return (VALUE . END).  END is the index after the value."
  (let ((end pos))
    (while (and (< end (length text))
                (not (memq (aref text end) '(?\s ?\t ?\n ?\r))))
      (setq end (1+ end)))
    (cons (harmless-refs--strip (substring text pos end)) end)))

(defun harmless-refs--prefix-p (text pos keyword)
  "Return non-nil if TEXT at POS starts with KEYWORD."
  (and (<= (+ pos (length keyword)) (length text))
       (string= (substring text pos (+ pos (length keyword))) keyword)))

(defun harmless-refs--scan (text)
  "Return the @ references in TEXT, in order."
  (let ((i 0)
        (refs nil))
    (while (setq i (string-search "@" text i))
      (if (not (harmless-refs--boundary-p text i))
          (setq i (1+ i))
        (cond
         ((harmless-refs--prefix-p text i "@file:")
          (let ((got (harmless-refs--take-value text (+ i 6))))
            (push (list :kind 'file
                        :label (concat "@file:" (car got))
                        :value (car got))
                  refs)
            (setq i (cdr got))))
         ((harmless-refs--prefix-p text i "@folder:")
          (let ((got (harmless-refs--take-value text (+ i 8))))
            (push (list :kind 'folder
                        :label (concat "@folder:" (car got))
                        :value (car got))
                  refs)
            (setq i (cdr got))))
         ((harmless-refs--prefix-p text i "@url:")
          (let ((got (harmless-refs--take-value text (+ i 5))))
            (push (list :kind 'url
                        :label (concat "@url:" (car got))
                        :value (car got))
                  refs)
            (setq i (cdr got))))
         ((harmless-refs--prefix-p text i "@git:")
          (let ((got (harmless-refs--take-value text (+ i 5))))
            (push (list :kind 'git
                        :label (concat "@git:" (car got))
                        :value (car got))
                  refs)
            (setq i (cdr got))))
         ((and (harmless-refs--prefix-p text i "@diff")
               (not (harmless-refs--prefix-p text i "@diff:"))
               (or (= (+ i 5) (length text))
                   (not (string-match-p "[[:alnum:]_-]"
                                        (string (aref text (+ i 5)))))))
          (push (list :kind 'diff :label "@diff" :value nil) refs)
          (setq i (+ i 5)))
         ((and (harmless-refs--prefix-p text i "@staged")
               (not (harmless-refs--prefix-p text i "@staged:"))
               (or (= (+ i 7) (length text))
                   (not (string-match-p "[[:alnum:]_-]"
                                        (string (aref text (+ i 7)))))))
          (push (list :kind 'staged :label "@staged" :value nil) refs)
          (setq i (+ i 7)))
         (t (setq i (1+ i))))))
    (nreverse refs)))

(defun harmless-refs--inside-p (session full)
  "Return non-nil if FULL is inside SESSION's project or an extra directory."
  (or (harmless-tools-fs-inside-p
       (harmless-session-require-project session) full)
      (cl-some
       (lambda (root)
         (and (stringp root)
              (not (string-empty-p root))
              (not (harmless-extra-root-text-p root))
              (file-directory-p root)
              (harmless-tools-fs-inside-p root full)))
       (harmless-session-extra-dirs session))))

(defun harmless-refs--resolve (session raw)
  "Return RAW as a path SESSION may read.
A relative path is expanded against the project.  Nil, \"\",
a path outside the project, and the filesystem root are refused."
  (unless (and (stringp raw) (not (string-empty-p (string-trim raw))))
    (error "context reference is empty"))
  (setq raw (string-trim raw))
  (when (or (harmless-extra-root-text-p raw)
            (harmless-filesystem-root-p raw))
    (error "context reference is the filesystem root"))
  (let* ((cwd (harmless-session-require-project session))
         (full (directory-file-name (harmless-expand-file-name raw cwd))))
    (when (or (harmless-filesystem-root-p full)
              (harmless-extra-root-text-p full))
      (error "context reference is the filesystem root"))
    (unless (harmless-refs--inside-p session full)
      (error "context reference is outside the project"))
    full))

(defun harmless-refs--file-spec (value)
  "Split a @file: VALUE into (PATH START END).
START and END are nil when VALUE has no line range."
  (if (and (stringp value)
           (string-match "\\`\\(.*\\):\\([0-9]+\\)-\\([0-9]+\\)\\'" value))
      (list (match-string 1 value)
            (string-to-number (match-string 2 value))
            (string-to-number (match-string 3 value)))
    (list value nil nil)))

(defun harmless-refs--read (path start end)
  "Return PATH as text, or lines START through END inclusive.
START and END are 1-based.  A binary file is refused."
  (unless (file-readable-p path)
    (error "context reference is missing"))
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path nil nil nil t))
    (when (save-excursion (goto-char (point-min)) (search-forward "\0" nil t))
      (error "context reference is not text"))
    (if (not (and start end))
        (progn
          (when (> (buffer-size) harmless-read-file-max-bytes)
            (goto-char (1+ harmless-read-file-max-bytes))
            (delete-region (point) (point-max))
            (insert "\n[truncated]"))
          (buffer-string))
      (when (or (< start 1) (< end start))
        (error "context reference line range is invalid"))
      (let ((lines (count-lines (point-min) (point-max))))
        (when (or (= lines 0) (> start lines))
          (error "context reference line range is past the end"))
        (when (> end lines)
          (setq end lines))
        (goto-char (point-min))
        (forward-line (1- start))
        (let ((from (point)))
          (forward-line (1+ (- end start)))
          (harmless-refs--cap
           (buffer-substring-no-properties from (point))))))))

(defun harmless-refs--file (session value)
  "Return the @file: VALUE for SESSION."
  (pcase-let ((`(,path ,start ,end) (harmless-refs--file-spec value)))
    (let ((full (harmless-refs--resolve session path)))
      (cond
       ((not (file-exists-p full))
        (error "context reference is missing"))
       ((file-directory-p full)
        (error "context reference is a directory"))
       (t (harmless-refs--read full start end))))))

(defun harmless-refs--folder-lines (root)
  "Return listing lines for ROOT, capped at `harmless-refs-folder-max'."
  (let ((count 0)
        (lines nil)
        (overflow nil)
        (base (file-name-as-directory root)))
    (cl-labels
        ((add (line)
           (if (>= count harmless-refs-folder-max)
               (setq overflow t)
             (setq count (1+ count))
             (setq lines (nconc lines (list line)))))
         (walk (current)
           (dolist (name (sort (directory-files current nil nil t) #'string<))
             (when overflow
               (throw 'harmless-refs-folder nil))
             (unless (member name '("." ".." ".git"))
               (let ((full (expand-file-name name current)))
                 (when (harmless-tools-fs-inside-p root full)
                   (let ((rel (subst-char-in-string
                               ?\\ ?/
                               (file-relative-name full base))))
                     (if (file-directory-p full)
                         (progn
                           (add (concat rel "/"))
                           (unless overflow
                             (walk full)))
                       (add (format "%s %d"
                                    rel
                                    (or (file-attribute-size
                                         (file-attributes full))
                                        0)))))))))))
      (catch 'harmless-refs-folder
        (walk root)))
    (when overflow
      (setq lines (nconc lines (list "- ..."))))
    lines))

(defun harmless-refs--folder (session value)
  "Return the @folder: VALUE listing for SESSION."
  (let ((full (harmless-refs--resolve session value)))
    (cond
     ((not (file-exists-p full))
      (error "context reference is missing"))
     ((not (file-directory-p full))
      (error "context reference is not a directory"))
     (t (let ((lines (harmless-refs--folder-lines full)))
          (if lines
              (mapconcat #'identity lines "\n")
            "(empty)"))))))

(defun harmless-refs--git-ok (dir)
  "Signal unless DIR is a git checkout and git can be run."
  (unless (executable-find "git")
    (error "context reference needs git"))
  (with-temp-buffer
    (unless (and (eq 0 (call-process "git" nil (current-buffer) nil
                                     "-C" dir "rev-parse"
                                     "--is-inside-work-tree"))
                 (string-prefix-p "true" (string-trim (buffer-string))))
      (error "context reference is not a git checkout"))))

(defun harmless-refs--git-out (dir &rest args)
  "Return the output of git ARGS in DIR."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (unless (eq 0 (apply #'call-process
                           "git" nil (current-buffer) nil
                           (append (list "-C" dir "--no-pager") args)))
        (error "context reference git failed")))
    (harmless-refs--cap (buffer-string))))

(defun harmless-refs--diff (session staged)
  "Return SESSION's unstaged diff, or the index when STAGED is non-nil."
  (let* ((dir (harmless-session-require-project session))
         (args (list "diff" "--no-ext-diff")))
    (harmless-refs--git-ok dir)
    (when staged
      (setq args (append args '("--cached"))))
    (setq args (append args (list "--" dir)))
    (let ((text (string-trim (apply #'harmless-refs--git-out dir args))))
      (if (string-empty-p text)
          (if staged "(no staged changes)" "(no unstaged changes)")
        text))))

(defun harmless-refs--git-n (session value)
  "Return the last N commits in SESSION.  VALUE is the decimal count."
  (unless (and (stringp value) (string-match-p "\\`[0-9]+\\'" value))
    (error "git history must be from 1 to 10"))
  (let ((n (string-to-number value))
        (dir (harmless-session-require-project session)))
    (unless (and (>= n 1) (<= n harmless-refs-git-max))
      (error "git history must be from 1 to 10"))
    (harmless-refs--git-ok dir)
    (let ((text (string-trim
                 (harmless-refs--git-out
                  dir "log" "-n" (number-to-string n) "-p" "--" dir))))
      (if (string-empty-p text)
          "(no commits)"
        text))))

(defun harmless-refs--url (value)
  "Return the body of the http or https URL VALUE."
  (unless (and (stringp value) (not (string-empty-p value)))
    (error "context reference is empty"))
  (harmless-refs--cap (harmless-web-get value)))

(defun harmless-refs--one (session ref)
  "Return one rendered REF for SESSION."
  (concat (plist-get ref :label) "\n"
          (pcase (plist-get ref :kind)
            ('file (harmless-refs--file session (plist-get ref :value)))
            ('folder (harmless-refs--folder session (plist-get ref :value)))
            ('diff (harmless-refs--diff session nil))
            ('staged (harmless-refs--diff session t))
            ('git (harmless-refs--git-n session (plist-get ref :value)))
            ('url (harmless-refs--url (plist-get ref :value))))))

(defun harmless-refs-expand (session text)
  "Return TEXT with @ references expanded for SESSION.
Nil and a non-string are returned unchanged.  A prompt with no
reference is returned unchanged.  When the attachment would take
more than half the context window, TEXT is returned unchanged."
  (cond
   ((not (stringp text)) text)
   (t
    (let ((refs (harmless-refs--scan text)))
      (if (null refs)
          text
        (unless (harmless-session-p session)
          (error "No Harmless session"))
        (let* ((body (mapconcat (lambda (ref)
                                  (harmless-refs--one session ref))
                                refs "\n\n"))
               (attached (concat harmless-refs-header "\n\n" body))
               (tokens (harmless-refs--tokens attached))
               (window (harmless-refs--window session)))
          (if (> tokens (/ window 2))
              text
            (concat text "\n\n" attached
                    (when (> tokens (/ window 4))
                      "\n\nAttached context is 25% of the context window or more.")))))))))

(provide 'harmless-refs)

;;; harmless-refs.el ends here
