;;; harmless-machine.el --- A registry of other machines -*- lexical-binding: t; -*-

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
;; A machine is a name and a directory.  The directory is that
;; host's Harmless data.  Registering it does not open a
;; connection and does not store a secret.  The same team
;; commands run with that directory as `harmless-directory'.
;; One relative file can be copied from one host to another.
;; Nothing is started.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-tools-fs)

(defvar harmless-directory)

(defun harmless-machine-name-p (name)
  "Return non-nil if NAME is a machine id, not a path."
  (and (stringp name)
       (string-match-p
        "\\`[A-Za-z0-9]+\\(?:-[A-Za-z0-9]+\\)*\\'" name)))

(defun harmless-machine--name (name)
  "Return NAME as a machine id, or signal.
Nil and \"\" are empty.  A path, \"~\", and the filesystem root
are not names and are not expanded."
  (cond
   ((null name) (error "machine name is empty"))
   ((not (stringp name)) (error "machine name must be a string"))
   (t
    (setq name (string-trim name))
    (cond
     ((string-empty-p name) (error "machine name is empty"))
     ((or (string-prefix-p "~" name)
          (string-prefix-p "/" name)
          (string-prefix-p "\\" name)
          (string-match-p "[/\\\\]" name)
          (string-match-p "\\`[A-Za-z]:[/\\]?\\'" name)
          (file-name-absolute-p name)
          (not (harmless-machine-name-p name)))
      (error "machine name is not a name"))
     (t name)))))

(defun harmless-machine--root-p (text)
  "Return non-nil if TEXT is \"/\" or a drive root."
  (or (harmless-filesystem-root-p text)
      (harmless-extra-root-text-p text)))

(defun harmless-machine--check-directory (directory exists)
  "Return DIRECTORY as a host directory, or signal.
EXISTS non-nil requires a directory that is already there.
A relative path and \"~\" are not expanded."
  (cond
   ((null directory) (error "machine directory is empty"))
   ((not (stringp directory))
    (error "machine directory must be a string"))
   (t
    (setq directory (string-trim directory))
    (cond
     ((string-empty-p directory)
      (error "machine directory is empty"))
     ((harmless-machine--root-p directory)
      (error "machine directory is the filesystem root"))
     ((or (string-prefix-p "~" directory)
          (not (file-name-absolute-p directory)))
      (error "machine directory must be absolute"))
     (t
      (setq directory
            (directory-file-name (expand-file-name directory)))
      (cond
       ((or (harmless-machine--root-p directory)
            (equal (file-truename directory) (file-truename "/")))
        (error "machine directory is the filesystem root"))
       ((and exists (not (file-exists-p directory)))
        (error "machine directory is missing"))
       ((and exists (not (file-directory-p directory)))
        (error "machine directory is not a directory"))
       ((and (file-exists-p directory)
             (not (file-directory-p directory)))
        (error "machine directory is not a directory"))
       (t directory)))))))

(defun harmless-machine--data ()
  "Return the local data directory, or signal.
A filesystem root is refused before the registry is written."
  (let ((raw harmless-directory))
    (when (and (stringp raw)
               (not (string-empty-p (string-trim raw)))
               (harmless-machine--root-p raw))
      (error "Refusing to store a machine at %s" (string-trim raw)))
    (let ((data (harmless-data-directory)))
      (when (harmless-machine--root-p data)
        (error "Refusing to store a machine at %s" data))
      data)))

(defun harmless-machine--file ()
  "Return the local machine registry, or signal.
The file is not created."
  (let* ((data (harmless-machine--data))
         (file (expand-file-name "machines.json" data)))
    (unless (and (file-name-absolute-p file)
                 (harmless-directory-strictly-under-p file data)
                 (not (harmless-machine--root-p file))
                 (not (equal (file-truename file)
                             (file-truename "/"))))
      (error "Refusing to store a machine at %s" file))
    file))

(defun harmless-machine--text (entries)
  "Return ENTRIES as JSON with stable key order."
  (concat
   "["
   (mapconcat
    (lambda (entry)
      (concat "{\"name\":"
              (harmless-json-text (plist-get entry :name))
              ",\"directory\":"
              (harmless-json-text (plist-get entry :directory))
              "}"))
    entries
    ",")
   "]"))

(defun harmless-machine--read-text (file)
  "Return the text of FILE."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents file))
    (buffer-string)))

(defun harmless-machine--entries (parsed)
  "Return machine plists from PARSED, or signal."
  (cond
   ((null parsed) nil)
   ((not (and (listp parsed) (proper-list-p parsed)))
    (error "machines are not valid"))
   (t
    (let ((out nil)
          (seen (make-hash-table :test 'equal)))
      (dolist (item parsed)
        (unless (harmless-plist-p item)
          (error "machines are not valid"))
        (let ((name (condition-case nil
                        (harmless-machine--name (plist-get item :name))
                      (error (error "machines are not valid"))))
              (dir (condition-case nil
                       (harmless-machine--check-directory
                        (plist-get item :directory) nil)
                     (error (error "machines are not valid")))))
          (when (gethash name seen)
            (error "machines are not valid"))
          (puthash name t seen)
          (push (list :name name :directory dir) out)))
      (sort out (lambda (a b)
                  (string< (plist-get a :name)
                           (plist-get b :name))))))))

(defun harmless-machine--read ()
  "Return registered machines, sorted by name.
A missing registry is an empty list."
  (let ((file (harmless-machine--file)))
    (cond
     ((not (file-exists-p file)) nil)
     ((not (file-regular-p file))
      (error "machines are not valid"))
     (t
      (harmless-machine--entries
       (condition-case nil
           (harmless-json-decode (harmless-machine--read-text file))
         (error (error "machines are not valid"))))))))

(defun harmless-machine--write (entries)
  "Write ENTRIES to the local registry and return that path."
  (let ((file (harmless-machine--file))
        (text (concat (harmless-machine--text entries) "\n")))
    (harmless-ensure-directory (file-name-directory file))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region text nil file nil 'silent))
    file))

(defun harmless-machine--get (name)
  "Return the machine named NAME, or signal."
  (setq name (harmless-machine--name name))
  (or (cl-find name (harmless-machine--read)
               :key (lambda (entry) (plist-get entry :name))
               :test #'equal)
      (error "no machine named %s" name)))

(defun harmless-machine--live (entry)
  "Return ENTRY's directory when it is a directory, or signal."
  (let ((directory (plist-get entry :directory)))
    (cond
     ((harmless-machine--root-p directory)
      (error "machine directory is the filesystem root"))
     ((not (file-exists-p directory))
      (error "machine directory is missing"))
     ((not (file-directory-p directory))
      (error "machine directory is not a directory"))
     (t directory))))

(defun harmless-machine--verbs ()
  "Return public team command names, sorted."
  (let (names)
    (mapatoms
     (lambda (sym)
       (when (and (commandp sym)
                  (string-prefix-p "harmless-team-" (symbol-name sym))
                  (not (string-prefix-p "harmless-team--"
                                        (symbol-name sym))))
         (push (symbol-name sym) names))))
    (sort names #'string<)))

(defun harmless-machine--verb (verb)
  "Return VERB when it is a public team command, or signal."
  (cond
   ((or (null verb) (not (symbolp verb)) (not (commandp verb)))
    (error "machine verb is not a team verb"))
   (t
    (let ((name (symbol-name verb)))
      (if (and (string-prefix-p "harmless-team-" name)
               (not (string-prefix-p "harmless-team--" name)))
          verb
        (error "machine verb is not a team verb"))))))

(defun harmless-machine--relative (file)
  "Return FILE as a relative file under one host, or signal.
A path, \"~\", \"..\", and a trailing slash are refused and
are not opened."
  (cond
   ((null file) (error "machine file is empty"))
   ((not (stringp file)) (error "machine file must be a string"))
   (t
    (setq file (string-trim file))
    (cond
     ((string-empty-p file) (error "machine file is empty"))
     ((or (string-prefix-p "~" file)
          (string-prefix-p "/" file)
          (string-prefix-p "\\" file)
          (string-match-p "\\`[A-Za-z]:" file)
          (file-name-absolute-p file)
          (string-match-p "\\.\\." file)
          (string-match-p "[/\\\\]\\'" file)
          (member file '("." "..")))
      (error "machine file escapes the machine"))
     (t
      (let ((parts (split-string file "[/\\\\]" t)))
        (when (or (null parts)
                  (cl-find-if-not
                   #'harmless-safe-path-component-p parts))
          (error "machine file escapes the machine"))
        (string-join parts "/")))))))

(defun harmless-machine--child (directory relative)
  "Return RELATIVE inside DIRECTORY, or signal when it leaves."
  (let ((full (expand-file-name relative directory)))
    (unless (and (file-name-absolute-p full)
                 (harmless-directory-strictly-under-p full directory)
                 (not (harmless-machine--root-p full))
                 (not (equal (file-truename full) (file-truename "/"))))
      (error "machine file escapes the machine"))
    full))

;;;###autoload
(defun harmless-machine-add (name directory)
  "Register machine NAME at DIRECTORY.
DIRECTORY is that host's Harmless data.  It must already exist.
This does not open a connection and does not store a secret.
Return (:name :directory)."
  (interactive
   (list (read-string "Machine: ")
         (read-string "Directory: ")))
  (setq name (harmless-machine--name name))
  (setq directory (harmless-machine--check-directory directory t))
  (let ((entries (harmless-machine--read)))
    (when (cl-find name entries
                   :key (lambda (entry) (plist-get entry :name))
                   :test #'equal)
      (error "machine is already registered: %s" name))
    (setq entries
          (sort (append entries
                        (list (list :name name :directory directory)))
                (lambda (a b)
                  (string< (plist-get a :name) (plist-get b :name)))))
    (harmless-machine--write entries))
  (message "Machine %s" name)
  (list :name name :directory directory))

;;;###autoload
(defun harmless-machine-list ()
  "Return registered machine names, sorted.
A missing registry is nil.  This writes nothing."
  (interactive)
  (mapcar (lambda (entry) (plist-get entry :name))
          (harmless-machine--read)))

;;;###autoload
(defun harmless-machine-show (name)
  "Return the machine named NAME.
The plist has :name and :directory.  This writes nothing."
  (interactive (list (read-string "Machine: ")))
  (harmless-machine--get name))

;;;###autoload
(defun harmless-machine-remove (name)
  "Forget machine NAME.
The host directory is not deleted.  Return NAME."
  (interactive (list (read-string "Machine: ")))
  (setq name (harmless-machine--name name))
  (let* ((entries (harmless-machine--read))
         (left (cl-remove name entries
                          :key (lambda (entry) (plist-get entry :name))
                          :test #'equal)))
    (when (equal (length left) (length entries))
      (error "no machine named %s" name))
    (harmless-machine--write left))
  (message "Machine %s" name)
  name)

;;;###autoload
(defun harmless-machine-call (name verb &rest args)
  "Run team command VERB on machine NAME with ARGS.
VERB is a public `harmless-team-' command.  For that call,
`harmless-directory' is the host directory.  An interactive
call prompts for VERB's own arguments.  This does not start
a process."
  (interactive
   (list (read-string "Machine: ")
         (intern (completing-read
                  "Verb: " (harmless-machine--verbs) nil t))))
  (setq name (harmless-machine--name name))
  (setq verb (harmless-machine--verb verb))
  (let* ((directory (harmless-machine--live
                     (harmless-machine--get name)))
         (harmless-directory directory))
    (if (called-interactively-p 'any)
        (call-interactively verb)
      (apply verb args))))

;;;###autoload
(defun harmless-machine-copy (from to file)
  "Copy one FILE from machine FROM to machine TO.
FILE is relative to each host directory.  A directory is
refused.  An existing destination is refused.  Return the
destination path."
  (interactive
   (list (read-string "From: ")
         (read-string "To: ")
         (read-string "File: ")))
  (setq from (harmless-machine--name from))
  (setq to (harmless-machine--name to))
  (setq file (harmless-machine--relative file))
  (let* ((src-dir (harmless-machine--live (harmless-machine--get from)))
         (dst-dir (harmless-machine--live (harmless-machine--get to)))
         (src (harmless-machine--child src-dir file))
         (dst (harmless-machine--child dst-dir file)))
    (cond
     ((not (file-exists-p src))
      (error "machine file is missing"))
     ((not (file-regular-p src))
      (error "machine file is not a file"))
     ((file-exists-p dst)
      (error "machine file is already there"))
     (t
      (let ((parent (file-name-directory dst)))
        (unless (or (harmless-same-directory-p parent dst-dir)
                    (harmless-directory-strictly-under-p parent dst-dir))
          (error "machine file escapes the machine"))
        (harmless-ensure-directory parent))
      (copy-file src dst nil t)
      (message "Copied %s" file)
      dst))))

(provide 'harmless-machine)

;;; harmless-machine.el ends here
