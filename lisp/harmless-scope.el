;;; harmless-scope.el --- Work on disk as missions and slices -*- lexical-binding: t; -*-

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
;; A project holds missions.  A mission holds slices.  Each node
;; stores its intent in SPEC.md.  Reading a slice composes the
;; project intent, the mission intent, and the slice intent.  Only
;; a slice has a specification.  Proof is a file under that slice.
;; A judgment names who made it.  Progress is derived from those
;; judgments and is not stored.

;;; Code:

(require 'subr-x)
(require 'harmless-util)
(require 'harmless-tools-fs)

(defconst harmless-scope--verdicts
  '("accept" "reject" "withdraw")
  "Judgments a slice item can record.")

(defconst harmless-scope--iso
  (concat "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
          "T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}Z\\'")
  "Match one UTC timestamp.")

(defun harmless-scope--root-text-p (text)
  "Return non-nil if TEXT is \"/\" or a drive root.
TEXT is not expanded.  A relative path and \"~\" are not roots."
  (and (stringp text)
       (or (harmless-extra-root-text-p text)
           (harmless-filesystem-root-p text))))

(defun harmless-scope--text (text empty type-err)
  "Return TEXT trimmed, or signal.
EMPTY is the blank error.  TYPE-ERR is the non-string error.
Internal newlines stay."
  (cond
   ((null text) (error empty))
   ((not (stringp text)) (error type-err)))
  (setq text (string-trim text))
  (if (string-empty-p text)
      (error empty)
    text))

(defun harmless-scope--name (name empty type-err bad)
  "Return NAME trimmed, or signal.
EMPTY, TYPE-ERR, and BAD are the refusal texts."
  (setq name (harmless-scope--text name empty type-err))
  (unless (string-match-p "\\`[A-Za-z0-9]+\\'" name)
    (error bad))
  name)

(defun harmless-scope--project (directory)
  "Return DIRECTORY as an absolute project, or signal.
This does not create it."
  (cond
   ((null directory) (error "scope project is empty"))
   ((not (stringp directory))
    (error "scope project must be a string")))
  (setq directory (string-trim directory))
  (cond
   ((string-empty-p directory)
    (error "scope project is empty"))
   ((harmless-scope--root-text-p directory)
    (error "scope project is the filesystem root"))
   ((or (string-prefix-p "~" directory)
        (not (file-name-absolute-p directory)))
    (error "scope project must be absolute")))
  (setq directory (directory-file-name (expand-file-name directory)))
  (cond
   ((or (harmless-scope--root-text-p directory)
        (equal (file-truename directory) (file-truename "/")))
    (error "scope project is the filesystem root"))
   ((not (file-exists-p directory))
    (error "scope project is missing"))
   ((not (file-directory-p directory))
    (error "scope project is not a directory"))
   (t directory)))

(defun harmless-scope--place (path parent)
  "Return PATH when it is strictly inside PARENT, or signal."
  (unless (and (stringp path)
               (file-name-absolute-p path)
               (harmless-directory-strictly-under-p path parent)
               (not (harmless-scope--root-text-p path))
               (not (equal (file-truename path)
                           (file-truename "/"))))
    (error "Refusing to store scope at %s" path))
  path)

(defun harmless-scope--write (path text)
  "Write TEXT to PATH.  PATH is not the filesystem root."
  (harmless-scope--place path (file-name-directory path))
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil path nil 'silent))
  path)

(defun harmless-scope--read (path)
  "Return PATH as text."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (buffer-string)))

(defun harmless-scope--intent-file (directory)
  "Return DIRECTORY's SPEC.md.  It is not created."
  (harmless-scope--place
   (expand-file-name "SPEC.md" directory)
   directory))

(defun harmless-scope--mission-dir (project mission)
  "Return MISSION's directory in PROJECT.  It is not created."
  (harmless-scope--place
   (expand-file-name mission
                     (file-name-as-directory
                      (expand-file-name "missions" project)))
   project))

(defun harmless-scope--slice-dir (project mission slice)
  "Return SLICE's directory.  It is not created."
  (harmless-scope--place
   (expand-file-name
    slice
    (file-name-as-directory
     (expand-file-name
      "slices"
      (harmless-scope--mission-dir project mission))))
   project))

(defun harmless-scope--parts (text)
  "Return (INTENT . SPEC) from TEXT, or signal.
SPEC nil means the file has no specification."
  (unless (and (stringp text) (string-prefix-p "# Intent\n" text))
    (error "scope file is not valid"))
  (setq text (substring text (length "# Intent\n")))
  (let ((mark (string-search "\n# Specification\n" text)))
    (if (not mark)
        (let ((intent (string-trim text)))
          (when (string-empty-p intent)
            (error "scope file is not valid"))
          (cons intent nil))
      (let ((intent (string-trim (substring text 0 mark)))
            (spec (string-trim
                   (substring text
                              (+ mark (length "\n# Specification\n"))))))
        (when (or (string-empty-p intent) (string-empty-p spec))
          (error "scope file is not valid"))
        (cons intent spec)))))

(defun harmless-scope--intent-text (intent)
  "Return INTENT as the body of SPEC.md."
  (concat "# Intent\n\n" intent "\n"))

(defun harmless-scope--slice-text (intent spec)
  "Return INTENT and SPEC as the body of a slice SPEC.md."
  (concat "# Intent\n\n" intent "\n\n# Specification\n\n" spec "\n"))

(defun harmless-scope--items (spec)
  "Return SPEC's contract items, in order.
A blank line is not an item."
  (let ((out nil))
    (dolist (line (split-string spec "\n"))
      (unless (string-empty-p (string-trim line))
        (setq out (append out (list (string-trim line))))))
    out))

(defun harmless-scope--read-intent (path)
  "Return the intent in PATH."
  (car (harmless-scope--parts (harmless-scope--read path))))

(defun harmless-scope--project-intent (project)
  "Return PROJECT's intent, or signal when SPEC.md is missing."
  (let ((path (harmless-scope--intent-file project)))
    (unless (file-regular-p path)
      (error "scope project has no intent"))
    (harmless-scope--read-intent path)))

(defun harmless-scope--ensure (dir parent)
  "Create DIR inside PARENT and return DIR."
  (harmless-scope--place dir parent)
  (harmless-ensure-directory dir)
  dir)

(defun harmless-scope--project-write (directory intent)
  "Write DIRECTORY's intent and return the file."
  (setq intent (harmless-scope--text
                intent "scope intent is empty"
                "scope intent must be a string"))
  (setq directory (harmless-scope--project directory))
  (let ((path (harmless-scope--intent-file directory)))
    (when (file-exists-p path)
      (error "scope project already exists"))
    (harmless-scope--write path (harmless-scope--intent-text intent))
    path))

(defun harmless-scope--mission (directory mission intent)
  "Add MISSION under DIRECTORY and return its SPEC.md."
  (setq intent (harmless-scope--text
                intent "scope intent is empty"
                "scope intent must be a string"))
  (setq mission (harmless-scope--name
                 mission "scope mission is empty"
                 "scope mission must be a string"
                 "scope mission is not a name"))
  (setq directory (harmless-scope--project directory))
  (harmless-scope--project-intent directory)
  (let* ((dir (harmless-scope--ensure
               (harmless-scope--mission-dir directory mission)
               directory))
         (path (harmless-scope--intent-file dir)))
    (when (file-exists-p path)
      (error "scope mission already exists"))
    (harmless-scope--write path (harmless-scope--intent-text intent))
    path))

(defun harmless-scope--slice (directory mission slice intent spec)
  "Add SLICE under MISSION and return its SPEC.md.
SPEC is the specification.  A mission does not get one."
  (setq intent (harmless-scope--text
                intent "scope intent is empty"
                "scope intent must be a string"))
  (setq spec (harmless-scope--text
              spec "scope spec is empty" "scope spec must be a string"))
  (setq mission (harmless-scope--name
                 mission "scope mission is empty"
                 "scope mission must be a string"
                 "scope mission is not a name"))
  (setq slice (harmless-scope--name
               slice "scope slice is empty"
               "scope slice must be a string"
               "scope slice is not a name"))
  (setq directory (harmless-scope--project directory))
  (let ((mission-dir (harmless-scope--mission-dir directory mission)))
    (unless (file-regular-p (harmless-scope--intent-file mission-dir))
      (error "scope mission is missing"))
    (let* ((dir (harmless-scope--ensure
                 (harmless-scope--slice-dir directory mission slice)
                 directory))
           (path (harmless-scope--intent-file dir)))
      (when (file-exists-p path)
        (error "scope slice already exists"))
      (harmless-scope--write
       path (harmless-scope--slice-text intent spec))
      path)))

(defun harmless-scope--proof-file (directory mission slice name)
  "Return the proof file NAME in SLICE.  It is not created."
  (setq name (harmless-scope--name
              name "scope proof is empty"
              "scope proof must be a string"
              "scope proof is not a name"))
  (harmless-scope--place
   (expand-file-name
    (concat name ".md")
    (file-name-as-directory
     (expand-file-name
      "proof"
      (harmless-scope--slice-dir directory mission slice))))
   directory))

(defun harmless-scope--proof (directory mission slice name text)
  "Write TEXT as proof NAME in SLICE and return the file."
  (setq text (harmless-scope--text
              text "scope proof is empty"
              "scope proof must be a string"))
  (setq mission (harmless-scope--name
                 mission "scope mission is empty"
                 "scope mission must be a string"
                 "scope mission is not a name"))
  (setq slice (harmless-scope--name
               slice "scope slice is empty"
               "scope slice must be a string"
               "scope slice is not a name"))
  (setq directory (harmless-scope--project directory))
  (let ((slice-dir (harmless-scope--slice-dir directory mission slice)))
    (unless (file-regular-p (harmless-scope--intent-file slice-dir))
      (error "scope slice is missing"))
    (let ((path (harmless-scope--proof-file
                 directory mission slice name)))
      (when (file-exists-p path)
        (error "scope proof already exists"))
      (harmless-scope--ensure (file-name-directory path) directory)
      (harmless-scope--write path (concat text "\n"))
      path)))

(defun harmless-scope--judgment-p (item)
  "Return non-nil if ITEM is one stored judgment."
  (and (harmless-plist-p item)
       (stringp (plist-get item :item))
       (string-match-p "\\`[1-9][0-9]*\\'" (plist-get item :item))
       (stringp (plist-get item :judge))
       (not (string-empty-p (plist-get item :judge)))
       (member (plist-get item :verdict) harmless-scope--verdicts)
       (stringp (plist-get item :reason))
       (not (string-empty-p (plist-get item :reason)))
       (stringp (plist-get item :evidence))
       (not (string-empty-p (plist-get item :evidence)))
       (stringp (plist-get item :at))
       (string-match-p harmless-scope--iso (plist-get item :at))))

(defun harmless-scope--judgments-file (slice-dir)
  "Return SLICE-DIR's judgment file.  It is not created."
  (harmless-scope--place
   (expand-file-name "judgments.json" slice-dir)
   slice-dir))

(defun harmless-scope--judgments (slice-dir)
  "Return SLICE-DIR's judgments, oldest first.
A missing file is an empty list.  A broken file signals."
  (let ((path (harmless-scope--judgments-file slice-dir)))
    (if (not (file-regular-p path))
        nil
      (let ((obj (condition-case nil
                     (harmless-json-decode
                      (harmless-scope--read path))
                   (error :bad))))
        (when (or (eq obj :bad) (not (listp obj)))
          (error "scope judgments are not valid"))
        (dolist (item obj)
          (unless (harmless-scope--judgment-p item)
            (error "scope judgments are not valid")))
        obj))))

(defun harmless-scope--item (item)
  "Return ITEM as a contract number, or signal."
  (cond
   ((null item) (error "scope item is empty"))
   ((integerp item)
    (when (<= item 0)
      (error "scope item is not an item"))
    (number-to-string item))
   ((not (stringp item))
    (error "scope item is not an item"))
   (t
    (setq item (string-trim item))
    (when (string-empty-p item)
      (error "scope item is empty"))
    (unless (string-match-p "\\`[1-9][0-9]*\\'" item)
      (error "scope item is not an item"))
    item)))

(defun harmless-scope--verdict (verdict)
  "Return VERDICT, or signal."
  (cond
   ((null verdict) (error "scope verdict is empty"))
   ((not (stringp verdict))
    (error "scope verdict must be a string")))
  (setq verdict (string-trim verdict))
  (cond
   ((string-empty-p verdict) (error "scope verdict is empty"))
   ((not (member verdict harmless-scope--verdicts))
    (error "scope verdict is not a verdict"))
   (t verdict)))

(defun harmless-scope--judge (directory mission slice item judge
                              verdict reason evidence)
  "Record one judgment on ITEM and return it.
EVIDENCE is a proof name already in the slice.  The slice
specification is not changed."
  (setq reason (harmless-scope--text
                reason "scope reason is empty"
                "scope reason must be a string"))
  (setq judge (harmless-scope--text
               judge "scope judge is empty"
               "scope judge must be a string"))
  (setq verdict (harmless-scope--verdict verdict))
  (setq item (harmless-scope--item item))
  (setq evidence (harmless-scope--name
                  evidence "scope evidence is empty"
                  "scope evidence must be a string"
                  "scope evidence is not a name"))
  (setq mission (harmless-scope--name
                 mission "scope mission is empty"
                 "scope mission must be a string"
                 "scope mission is not a name"))
  (setq slice (harmless-scope--name
               slice "scope slice is empty"
               "scope slice must be a string"
               "scope slice is not a name"))
  (setq directory (harmless-scope--project directory))
  (let ((slice-dir (harmless-scope--slice-dir directory mission slice)))
    (unless (file-regular-p (harmless-scope--intent-file slice-dir))
      (error "scope slice is missing"))
    (let* ((parts (harmless-scope--parts
                   (harmless-scope--read
                    (harmless-scope--intent-file slice-dir))))
           (items (harmless-scope--items (or (cdr parts) "")))
           (proof (harmless-scope--proof-file
                   directory mission slice evidence))
           (path (harmless-scope--judgments-file slice-dir))
           (stored (harmless-scope--judgments slice-dir)))
      (unless (cdr parts)
        (error "scope slice is not specified"))
      (unless (<= (string-to-number item) (length items))
        (error "scope item is not an item"))
      (unless (file-regular-p proof)
        (error "scope evidence is missing"))
      (let ((record (list :item item
                          :judge judge
                          :verdict verdict
                          :reason reason
                          :evidence evidence
                          :at (harmless-now-iso))))
        (harmless-scope--write
         path (harmless-json-text (append stored (list record))))
        record))))

(defun harmless-scope--latest (judgments item proof-names)
  "Return the latest verdict for ITEM, or nil.
PROOF-NAMES are the proof files still in the slice.  A
judgment whose evidence is gone does not count."
  (let ((found nil))
    (dolist (record judgments)
      (when (and (equal (plist-get record :item) item)
                 (member (plist-get record :evidence) proof-names))
        (setq found (plist-get record :verdict))))
    found))

(defun harmless-scope--proof-names (slice-dir)
  "Return proof names in SLICE-DIR, without the suffix."
  (let ((dir (expand-file-name "proof" slice-dir))
        (out nil))
    (when (file-directory-p dir)
      (dolist (file (directory-files dir t "\\.md\\'"))
        (setq out (append
                   out
                   (list (file-name-sans-extension
                          (file-name-nondirectory file)))))))
    out))

(defun harmless-scope--progress (directory mission slice)
  "Return SLICE's progress.  This writes nothing.
Status is \"proved\", \"rejected\", or \"open\"."
  (setq mission (harmless-scope--name
                 mission "scope mission is empty"
                 "scope mission must be a string"
                 "scope mission is not a name"))
  (setq slice (harmless-scope--name
               slice "scope slice is empty"
               "scope slice must be a string"
               "scope slice is not a name"))
  (setq directory (harmless-scope--project directory))
  (let ((slice-dir (harmless-scope--slice-dir directory mission slice)))
    (unless (file-regular-p (harmless-scope--intent-file slice-dir))
      (error "scope slice is missing"))
    (let* ((parts (harmless-scope--parts
                   (harmless-scope--read
                    (harmless-scope--intent-file slice-dir))))
           (items (harmless-scope--items (or (cdr parts) "")))
           (judgments (harmless-scope--judgments slice-dir))
           (names (harmless-scope--proof-names slice-dir))
           (accepted 0)
           (rejected 0)
           (open 0)
           (n 0))
      (unless (cdr parts)
        (error "scope slice is not specified"))
      (dolist (_item items)
        (setq n (1+ n))
        (let ((verdict (harmless-scope--latest
                        judgments (number-to-string n) names)))
          (cond
           ((equal verdict "accept") (setq accepted (1+ accepted)))
           ((equal verdict "reject") (setq rejected (1+ rejected)))
           (t (setq open (1+ open))))))
      (list :items (length items)
            :accepted accepted
            :rejected rejected
            :open open
            :status (cond
                     ((and (> (length items) 0)
                           (= accepted (length items)))
                      "proved")
                     ((> rejected 0) "rejected")
                     (t "open"))))))

(defun harmless-scope--show (directory &optional mission slice)
  "Return the composed intent at DIRECTORY, MISSION, or SLICE.
A slice includes its specification and derived progress.
This writes nothing."
  (setq directory (harmless-scope--project directory))
  (let ((project (harmless-scope--project-intent directory)))
    (if (null mission)
        (progn
          (when slice
            (error "scope mission is empty"))
          (list :intent project))
      (setq mission (harmless-scope--name
                     mission "scope mission is empty"
                     "scope mission must be a string"
                     "scope mission is not a name"))
      (let ((mission-dir (harmless-scope--mission-dir
                          directory mission)))
        (unless (file-regular-p
                 (harmless-scope--intent-file mission-dir))
          (error "scope mission is missing"))
        (let ((mission-intent
               (harmless-scope--read-intent
                (harmless-scope--intent-file mission-dir))))
          (if (null slice)
              (list :project project
                    :mission mission
                    :intent mission-intent)
            (setq slice (harmless-scope--name
                         slice "scope slice is empty"
                         "scope slice must be a string"
                         "scope slice is not a name"))
            (let ((slice-dir (harmless-scope--slice-dir
                              directory mission slice)))
              (unless (file-regular-p
                       (harmless-scope--intent-file slice-dir))
                (error "scope slice is missing"))
              (let ((parts (harmless-scope--parts
                            (harmless-scope--read
                             (harmless-scope--intent-file slice-dir)))))
                (unless (cdr parts)
                  (error "scope slice is not specified"))
                (list :project project
                      :mission mission-intent
                      :slice (car parts)
                      :spec (cdr parts)
                      :items (harmless-scope--items (cdr parts))
                      :progress (harmless-scope--progress
                                 directory mission slice))))))))))

;;;###autoload
(defun harmless-scope-project (directory intent)
  "Write the intent of project DIRECTORY.
The file is SPEC.md.  A second write is refused."
  (interactive
   (list (read-string "Project: ")
         (read-string "Intent: ")))
  (let ((path (harmless-scope--project-write directory intent)))
    (message "Project %s" path)
    path))

;;;###autoload
(defun harmless-scope-mission (directory mission intent)
  "Add MISSION under DIRECTORY with INTENT.
A mission is not a specification."
  (interactive
   (list (read-string "Project: ")
         (read-string "Mission: ")
         (read-string "Intent: ")))
  (let ((path (harmless-scope--mission directory mission intent)))
    (message "Mission %s" path)
    path))

;;;###autoload
(defun harmless-scope-slice (directory mission slice intent spec)
  "Add SLICE under MISSION with INTENT and SPEC.
SPEC is the specification.  Only a slice has one."
  (interactive
   (list (read-string "Project: ")
         (read-string "Mission: ")
         (read-string "Slice: ")
         (read-string "Intent: ")
         (read-string "Specification: ")))
  (let ((path (harmless-scope--slice
               directory mission slice intent spec)))
    (message "Slice %s" path)
    path))

;;;###autoload
(defun harmless-scope-show (directory &optional mission slice)
  "Show intent composed down to MISSION or SLICE.
Nil MISSION is the project.  This writes nothing."
  (interactive
   (list (read-string "Project: ")
         (read-string "Mission: ")
         (read-string "Slice: ")))
  (let ((mission (and (stringp mission)
                      (not (string-empty-p (string-trim mission)))
                      mission))
        (slice (and (stringp slice)
                    (not (string-empty-p (string-trim slice)))
                    slice)))
    (harmless-scope--show directory mission slice)))

;;;###autoload
(defun harmless-scope-proof (directory mission slice name text)
  "Write TEXT as proof NAME on SLICE and return the file."
  (interactive
   (list (read-string "Project: ")
         (read-string "Mission: ")
         (read-string "Slice: ")
         (read-string "Name: ")
         (read-string "Evidence: ")))
  (let ((path (harmless-scope--proof
               directory mission slice name text)))
    (message "Proof %s" path)
    path))

;;;###autoload
(defun harmless-scope-judge (directory mission slice item judge
                             verdict reason evidence)
  "Judge ITEM of SLICE as VERDICT and return the judgment.
JUDGE is who decided.  REASON is why.  EVIDENCE is a proof
name already stored on the slice."
  (interactive
   (list (read-string "Project: ")
         (read-string "Mission: ")
         (read-string "Slice: ")
         (read-string "Item: ")
         (read-string "Judge: ")
         (read-string "Verdict: ")
         (read-string "Reason: ")
         (read-string "Evidence: ")))
  (let ((saved (harmless-scope--judge
                directory mission slice item judge
                verdict reason evidence)))
    (message "Judged %s" (plist-get saved :item))
    saved))

;;;###autoload
(defun harmless-scope-progress (directory mission slice)
  "Return SLICE's progress derived from its evidence.
This writes nothing.  Status is proved, rejected, or open."
  (interactive
   (list (read-string "Project: ")
         (read-string "Mission: ")
         (read-string "Slice: ")))
  (let ((saved (harmless-scope--progress directory mission slice)))
    (message "%s" (plist-get saved :status))
    saved))

(provide 'harmless-scope)

;;; harmless-scope.el ends here
