;;; harmless-context.el --- Context packs addressed by ref -*- lexical-binding: t; -*-

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
;; A project holds context packs under packs/NAME/PACK.md.
;; A ref names a pack, one file in that pack, or one heading
;; in that file.  A profile composes the pieces the pack
;; declares for a fresh seat, a handover, or a return from
;; compaction.  The token total is an estimate of four
;; characters each.  The text is not truncated.  Nothing is
;; started, and nothing is written.

;;; Code:

(require 'subr-x)
(require 'harmless-util)
(require 'harmless-tools-fs)

(defconst harmless-context--situations
  '("fresh" "handover" "post-compaction")
  "Profiles a pack can compose.")

(defconst harmless-context--name-re
  "\\`[A-Za-z0-9]+\\(?:-[A-Za-z0-9]+\\)*\\'"
  "Match one pack name.")

(defun harmless-context--root-text-p (text)
  "Return non-nil if TEXT is \"/\" or a drive root.
TEXT is not expanded.  A relative path and \"~\" are not roots."
  (and (stringp text)
       (or (harmless-extra-root-text-p text)
           (harmless-filesystem-root-p text))))

(defun harmless-context--text (text empty type-err)
  "Return TEXT trimmed, or signal.
EMPTY is the blank error.  TYPE-ERR is the non-string error."
  (cond
   ((null text) (error empty))
   ((not (stringp text)) (error type-err)))
  (setq text (string-trim text))
  (if (string-empty-p text)
      (error empty)
    text))

(defun harmless-context--name (name)
  "Return pack NAME trimmed, or signal."
  (setq name (harmless-context--text
              name "context pack is empty"
              "context pack must be a string"))
  (unless (string-match-p harmless-context--name-re name)
    (error "context pack is not a name"))
  name)

(defun harmless-context--project (directory)
  "Return DIRECTORY as an absolute project, or signal.
This does not create it."
  (cond
   ((null directory) (error "context project is empty"))
   ((not (stringp directory))
    (error "context project must be a string")))
  (setq directory (string-trim directory))
  (cond
   ((string-empty-p directory)
    (error "context project is empty"))
   ((harmless-context--root-text-p directory)
    (error "context project is the filesystem root"))
   ((or (string-prefix-p "~" directory)
        (not (file-name-absolute-p directory)))
    (error "context project must be absolute")))
  (setq directory (directory-file-name (expand-file-name directory)))
  (cond
   ((or (harmless-context--root-text-p directory)
        (equal (file-truename directory) (file-truename "/")))
    (error "context project is the filesystem root"))
   ((not (file-exists-p directory))
    (error "context project is missing"))
   ((not (file-directory-p directory))
    (error "context project is not a directory"))
   (t directory)))

(defun harmless-context--place (path parent)
  "Return PATH when it is strictly inside PARENT, or signal."
  (unless (and (stringp path)
               (file-name-absolute-p path)
               (harmless-directory-strictly-under-p path parent)
               (not (harmless-context--root-text-p path))
               (not (equal (file-truename path)
                           (file-truename "/"))))
    (error "Refusing to read context at %s" path))
  path)

(defun harmless-context--relative-p (text)
  "Return non-nil if TEXT is one file inside a pack."
  (and (stringp text)
       (not (string-empty-p text))
       (not (file-name-absolute-p text))
       (not (string-prefix-p "~" text))
       (not (string-prefix-p "/" text))
       (not (string-match-p "\\.\\." text))
       (not (string-match-p "[\\\\]" text))
       (not (string-search "#" text))))

(defun harmless-context--escapes-p (text)
  "Return non-nil if TEXT would leave a pack directory."
  (or (not (stringp text))
      (string-empty-p text)
      (file-name-absolute-p text)
      (string-prefix-p "~" text)
      (string-prefix-p "/" text)
      (string-match-p "\\.\\." text)
      (string-match-p "[\\\\]" text)))

(defun harmless-context--tokens (text)
  "Return how many tokens TEXT is.
Four characters are one token.  Empty text is zero."
  (if (or (not (stringp text)) (string-empty-p text))
      0
    (ceiling (length text) 4)))

(defun harmless-context--read (path)
  "Return PATH as text."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (buffer-string)))

(defun harmless-context--slug (text)
  "Return TEXT as a heading slug."
  (save-match-data
    (setq text (downcase (string-trim text)))
    (setq text (replace-regexp-in-string "[^a-z0-9]+" "-" text))
    (string-trim text "[-]+" "[-]+")))

(defun harmless-context--heading (line)
  "Return (LEVEL . TITLE) when LINE is a heading, else nil."
  (save-match-data
    (when (string-match "\\`\\(#+\\)[ \t]+\\(.+\\)\\'" line)
      (cons (length (match-string 1 line))
            (string-trim (match-string 2 line))))))

(defun harmless-context--section (text name)
  "Return the heading NAME in TEXT, including its subsections.
NAME matches the heading text or its slug."
  (let ((want (harmless-context--slug name))
        (lines (split-string text "\n"))
        (level nil)
        (out nil)
        (taking nil)
        (done nil))
    (when (string-empty-p want)
      (error "context section is missing"))
    (dolist (line lines)
      (let ((heading (harmless-context--heading line)))
        (cond
         (done nil)
         (taking
          (if (and heading (<= (car heading) level))
              (setq taking nil
                    done t)
            (push line out)))
         ((and heading
               (equal (harmless-context--slug (cdr heading)) want))
          (setq taking t
                level (car heading))
          (push line out)))))
    (unless level
      (error "context section is missing"))
    (string-trim-right (mapconcat #'identity (nreverse out) "\n"))))

(defun harmless-context--situation-line (line)
  "Return the situation LINE names, or nil."
  (save-match-data
    (when (string-match
           "\\`# \\(fresh\\|handover\\|post-compaction\\)\\'"
           line)
      (match-string 1 line))))

(defun harmless-context--piece (text files)
  "Return TEXT when it is a piece of FILES, or signal."
  (let* ((hash (string-search "#" text))
         (file (if hash (substring text 0 hash) text))
         (section (and hash (substring text (1+ hash)))))
    (when (harmless-context--escapes-p file)
      (error "context file escapes the pack"))
    (unless (and (harmless-context--relative-p file)
                 (member file files)
                 (or (null hash)
                     (and (stringp section)
                          (not (string-empty-p
                                (string-trim section))))))
      (error "context pack is not valid"))
    text))

(defun harmless-context--manifest (text)
  "Return the pieces declared by TEXT, or signal."
  (unless (and (stringp text) (string-prefix-p "# Files\n" text))
    (error "context pack is not valid"))
  (let ((lines (split-string
                (substring text (length "# Files\n")) "\n"))
        (spec (list :files nil :fresh nil :handover nil
                    :post-compaction nil))
        (which :files)
        (count 0))
    (dolist (line lines)
      (setq line (string-trim line))
      (unless (string-empty-p line)
        (let ((sit (harmless-context--situation-line line)))
          (cond
           (sit
            (when (and (not (eq which :files)) (= count 0))
              (error "context pack is not valid"))
            (when (null (plist-get spec :files))
              (error "context pack is not valid"))
            (when (plist-get spec (intern (concat ":" sit)))
              (error "context pack is not valid"))
            (setq which (intern (concat ":" sit))
                  count 0))
           ((string-prefix-p "#" line)
            (error "context pack is not valid"))
           ((eq which :files)
            (when (harmless-context--escapes-p line)
              (error "context file escapes the pack"))
            (unless (harmless-context--relative-p line)
              (error "context pack is not valid"))
            (when (member line (plist-get spec :files))
              (error "context pack is not valid"))
            (setq spec (plist-put
                        spec :files
                        (append (plist-get spec :files)
                                (list line)))))
           (t
            (harmless-context--piece line (plist-get spec :files))
            (setq spec (plist-put
                        spec which
                        (append (plist-get spec which)
                                (list line))))
            (setq count (1+ count)))))))
    (when (null (plist-get spec :files))
      (error "context pack is not valid"))
    (when (and (not (eq which :files)) (= count 0))
      (error "context pack is not valid"))
    spec))

(defun harmless-context--pack-dir (directory pack)
  "Return PACK's directory in DIRECTORY, or signal.
PACK.md must already exist.  This does not create it."
  (setq pack (harmless-context--name pack))
  (setq directory (harmless-context--project directory))
  (let ((dir (harmless-context--place
              (expand-file-name
               pack
               (file-name-as-directory
                (expand-file-name "packs" directory)))
              directory)))
    (unless (file-regular-p (expand-file-name "PACK.md" dir))
      (error "context pack is missing"))
    dir))

(defun harmless-context--file (pack-dir relative)
  "Return RELATIVE inside PACK-DIR, or signal."
  (when (harmless-context--escapes-p relative)
    (error "context file escapes the pack"))
  (unless (harmless-context--relative-p relative)
    (error "context pack is not valid"))
  (let ((path (harmless-context--place
               (expand-file-name relative pack-dir)
               pack-dir)))
    (cond
     ((not (file-exists-p path))
      (error "context file is missing"))
     ((not (file-regular-p path))
      (error "context file is not a file"))
     (t path))))

(defun harmless-context--resolve (pack-dir piece)
  "Return the text of PIECE inside PACK-DIR."
  (let* ((hash (string-search "#" piece))
         (file (if hash (substring piece 0 hash) piece))
         (section (and hash (substring piece (1+ hash))))
         (text (harmless-context--read
                (harmless-context--file pack-dir file))))
    (if section
        (harmless-context--section text section)
      text)))

(defun harmless-context--join (parts)
  "Return PARTS separated by a blank line."
  (mapconcat #'identity parts "\n\n"))

(defun harmless-context--ref (text)
  "Return the pack, file, and section in TEXT, or signal."
  (setq text (harmless-context--text
              text "context ref is empty"
              "context ref must be a string"))
  (save-match-data
    (unless (string-match
             (concat "\\`\\([A-Za-z0-9]+\\(?:-[A-Za-z0-9]+\\)*\\)"
                     "\\(?:/\\([^#]+\\)\\(?:#\\(.+\\)\\)?\\)?\\'")
             text)
      (error "context ref is not a ref"))
    (let ((pack (match-string 1 text))
          (file (match-string 2 text))
          (section (match-string 3 text)))
      (when (and file (or (harmless-context--escapes-p file)
                          (not (harmless-context--relative-p file))))
        (error "context ref is not a ref"))
      (when (and section (string-empty-p (string-trim section)))
        (error "context ref is not a ref"))
      (list :ref text :pack pack :file file :section section))))

(defun harmless-context--list (directory)
  "Return the pack names in DIRECTORY, sorted.
A missing packs directory is an empty list.  This writes
nothing."
  (setq directory (harmless-context--project directory))
  (let ((root (expand-file-name "packs" directory))
        (out nil))
    (when (file-directory-p root)
      (dolist (name (directory-files root))
        (when (and (string-match-p harmless-context--name-re name)
                   (file-regular-p
                    (expand-file-name
                     "PACK.md" (expand-file-name name root))))
          (push name out))))
    (sort out #'string<)))

(defun harmless-context--show (directory pack)
  "Return PACK's manifest.  This writes nothing."
  (setq pack (harmless-context--name pack))
  (let* ((dir (harmless-context--pack-dir directory pack))
         (spec (harmless-context--manifest
                (harmless-context--read
                 (expand-file-name "PACK.md" dir)))))
    (append (list :pack pack) spec)))

(defun harmless-context--get (directory ref)
  "Return the text REF names.  This writes nothing.
The token total counts the whole text."
  (let* ((parsed (harmless-context--ref ref))
         (dir (harmless-context--pack-dir
               directory (plist-get parsed :pack)))
         (spec (harmless-context--manifest
                (harmless-context--read
                 (expand-file-name "PACK.md" dir))))
         (file (plist-get parsed :file))
         (section (plist-get parsed :section))
         (text nil))
    (setq text
          (cond
           ((null file)
            (harmless-context--join
             (mapcar (lambda (piece)
                       (harmless-context--resolve dir piece))
                     (plist-get spec :files))))
           ((not (member file (plist-get spec :files)))
            (error "context file is not in the pack"))
           (section
            (harmless-context--section
             (harmless-context--read
              (harmless-context--file dir file))
             section))
           (t
            (harmless-context--read
             (harmless-context--file dir file)))))
    (list :ref (plist-get parsed :ref)
          :text text
          :tokens (harmless-context--tokens text))))

(defun harmless-context--situation (text)
  "Return situation TEXT, or signal."
  (setq text (harmless-context--text
              text "context situation is empty"
              "context situation must be a string"))
  (unless (member text harmless-context--situations)
    (error "context situation is not a situation"))
  text)

(defun harmless-context--budget (budget)
  "Return BUDGET, or nil when it was omitted.
Any other value signals."
  (cond
   ((null budget) nil)
   ((and (integerp budget) (> budget 0)) budget)
   (t (error "context budget is not a budget"))))

(defun harmless-context--profile (directory pack situation budget)
  "Return the profile PACK declares for SITUATION.
BUDGET nil reports zero overage.  The text is complete."
  (setq situation (harmless-context--situation situation))
  (setq budget (harmless-context--budget budget))
  (setq pack (harmless-context--name pack))
  (let* ((dir (harmless-context--pack-dir directory pack))
         (spec (harmless-context--manifest
                (harmless-context--read
                 (expand-file-name "PACK.md" dir))))
         (key (intern (concat ":" situation)))
         (pieces (plist-get spec key)))
    (unless pieces
      (error "context situation is missing"))
    (let* ((text (harmless-context--join
                  (mapcar (lambda (piece)
                            (harmless-context--resolve dir piece))
                          pieces)))
           (tokens (harmless-context--tokens text))
           (over (if (and budget (> tokens budget))
                     (- tokens budget)
                   0)))
      (list :situation situation
            :pieces pieces
            :text text
            :tokens tokens
            :budget budget
            :over over))))

(defun harmless-context--report (plist)
  "Tell the user the token total in PLIST and return PLIST."
  (let ((over (plist-get plist :over)))
    (if (and (integerp over) (> over 0))
        (message "Tokens %s, over %s"
                 (plist-get plist :tokens) over)
      (message "Tokens %s" (plist-get plist :tokens))))
  plist)

;;;###autoload
(defun harmless-context-list (directory)
  "List context packs in DIRECTORY.
This writes nothing."
  (interactive (list (read-string "Project: ")))
  (harmless-context--list directory))

;;;###autoload
(defun harmless-context-show (directory pack)
  "Show the manifest of PACK in DIRECTORY.
This writes nothing."
  (interactive
   (list (read-string "Project: ")
         (read-string "Pack: ")))
  (harmless-context--show directory pack))

;;;###autoload
(defun harmless-context-get (directory ref)
  "Read the pack, file, or section REF names.
This writes nothing.  The token total covers the whole text."
  (interactive
   (list (read-string "Project: ")
         (read-string "Ref: ")))
  (harmless-context--report
   (harmless-context--get directory ref)))

;;;###autoload
(defun harmless-context-profile (directory pack situation
                                 &optional budget)
  "Compose PACK for SITUATION and report the token total.
SITUATION is fresh, handover, or post-compaction.  BUDGET is
a positive integer or nil.  The text is not truncated."
  (interactive
   (list (read-string "Project: ")
         (read-string "Pack: ")
         (read-string "Situation: ")
         (let ((raw (string-trim (read-string "Budget: "))))
           (cond
            ((string-empty-p raw) nil)
            ((string-match-p "\\`[0-9]+\\'" raw)
             (string-to-number raw))
            (t raw)))))
  (harmless-context--report
   (harmless-context--profile directory pack situation budget)))

(provide 'harmless-context)

;;; harmless-context.el ends here
