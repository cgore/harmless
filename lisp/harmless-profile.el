;;; harmless-profile.el --- Named auth profiles for a seat -*- lexical-binding: t; -*-

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
;; A profile is a name and a runtime.  The env field is the
;; name of an environment variable.  The token is that
;; variable's value.  Switching a seat stores the profile name
;; and keeps the session and the inbound work.  The rig and
;; the profile file do not store the token.  This is not
;; `harmless-login'.  Nothing is started.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-team)

(defvar harmless-directory)

(defun harmless-profile--root-p (text)
  "Return non-nil if TEXT is \"/\" or a drive root."
  (or (harmless-filesystem-root-p text)
      (harmless-extra-root-text-p text)))

(defun harmless-profile--name (name)
  "Return NAME as a profile id, or signal.
Nil and \"\" are empty.  A path, \"~\", and the filesystem
root are not names and are not expanded."
  (cond
   ((null name) (error "profile name is empty"))
   ((not (stringp name)) (error "profile name must be a string"))
   (t
    (setq name (string-trim name))
    (cond
     ((string-empty-p name) (error "profile name is empty"))
     ((or (string-prefix-p "~" name)
          (string-prefix-p "/" name)
          (string-prefix-p "\\" name)
          (string-match-p "[/\\\\]" name)
          (string-match-p "\\`[A-Za-z]:[/\\]?\\'" name)
          (file-name-absolute-p name)
          (not (harmless-team--profile-name-p name)))
      (error "profile name is not a name"))
     (t name)))))

(defun harmless-profile--runtime (runtime)
  "Return RUNTIME as a seat runtime, or signal."
  (cond
   ((null runtime) (error "profile runtime is empty"))
   ((not (stringp runtime))
    (error "profile runtime must be a string")))
  (setq runtime (string-trim runtime))
  (cond
   ((string-empty-p runtime) (error "profile runtime is empty"))
   ((not (member runtime harmless-team--runtimes))
    (error "profile runtime is not a runtime"))
   (t runtime)))

(defun harmless-profile--env-p (env)
  "Return non-nil if ENV is an environment variable name.
Matching does not ignore case."
  (and (stringp env)
       (let ((case-fold-search nil))
         (string-match-p "\\`[A-Z][A-Z0-9_]*\\'" env))))

(defun harmless-profile--env (env)
  "Return ENV as a variable name, or signal.
The value of that variable is the token.  It is not stored."
  (cond
   ((null env) (error "profile env is empty"))
   ((not (stringp env)) (error "profile env must be a string"))
   (t
    (setq env (string-trim env))
    (cond
     ((string-empty-p env) (error "profile env is empty"))
     ((or (string-prefix-p "~" env)
          (string-prefix-p "/" env)
          (string-prefix-p "\\" env)
          (string-match-p "[/\\\\[:space:]=]" env)
          (string-match-p "xox[abprs]-" env)
          (string-match-p "token=" env)
          (not (harmless-profile--env-p env)))
      (error "profile env is not a name"))
     (t env)))))

(defun harmless-profile--data ()
  "Return the local data directory, or signal.
A filesystem root is refused before the file is written."
  (let ((raw harmless-directory))
    (when (stringp raw)
      (setq raw (string-trim raw)))
    (when (and (stringp raw)
               (not (string-empty-p raw))
               (harmless-profile--root-p raw))
      (error "Refusing to store a profile at %s" raw))
    (let ((data (harmless-data-directory)))
      (when (harmless-profile--root-p data)
        (error "Refusing to store a profile at %s" data))
      data)))

(defun harmless-profile--file ()
  "Return the profile file, or signal.
The file is not created."
  (let* ((data (harmless-profile--data))
         (file (expand-file-name "profiles.json" data)))
    (unless (and (file-name-absolute-p file)
                 (harmless-directory-strictly-under-p file data)
                 (not (harmless-profile--root-p file))
                 (not (equal (file-truename file)
                             (file-truename "/"))))
      (error "Refusing to store a profile at %s" file))
    file))

(defun harmless-profile--text (entries)
  "Return ENTRIES as JSON with stable key order."
  (concat
   "["
   (mapconcat
    (lambda (entry)
      (concat "{\"name\":"
              (harmless-json-text (plist-get entry :name))
              ",\"runtime\":"
              (harmless-json-text (plist-get entry :runtime))
              ",\"env\":"
              (harmless-json-text (plist-get entry :env))
              "}"))
    entries
    ",")
   "]"))

(defun harmless-profile--read-text (file)
  "Return the text of FILE."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents file))
    (buffer-string)))

(defun harmless-profile--entries (parsed)
  "Return profile plists from PARSED, or signal.
A token, a secret, or a password is not valid."
  (cond
   ((null parsed) nil)
   ((not (and (listp parsed) (proper-list-p parsed)))
    (error "profiles are not valid"))
   (t
    (let ((out nil)
          (seen (make-hash-table :test 'equal)))
      (dolist (item parsed)
        (unless (and (harmless-plist-p item)
                     (not (plist-member item :token))
                     (not (plist-member item :secret))
                     (not (plist-member item :password)))
          (error "profiles are not valid"))
        (let ((name (condition-case nil
                        (harmless-profile--name (plist-get item :name))
                      (error (error "profiles are not valid"))))
              (runtime (condition-case nil
                           (harmless-profile--runtime
                            (plist-get item :runtime))
                         (error (error "profiles are not valid"))))
              (env (condition-case nil
                       (harmless-profile--env (plist-get item :env))
                     (error (error "profiles are not valid")))))
          (when (gethash name seen)
            (error "profiles are not valid"))
          (puthash name t seen)
          (push (list :name name :runtime runtime :env env) out)))
      (sort out (lambda (a b)
                  (string< (plist-get a :name)
                           (plist-get b :name))))))))

(defun harmless-profile--read ()
  "Return the profiles, sorted by name.
A missing file is an empty list."
  (let ((file (harmless-profile--file)))
    (cond
     ((not (file-exists-p file)) nil)
     ((not (file-regular-p file))
      (error "profiles are not valid"))
     (t
      (harmless-profile--entries
       (condition-case nil
           (harmless-json-decode (harmless-profile--read-text file))
         (error (error "profiles are not valid"))))))))

(defun harmless-profile--write (entries)
  "Write ENTRIES and return that path."
  (let ((file (harmless-profile--file))
        (text (concat (harmless-profile--text entries) "\n")))
    (harmless-ensure-directory (file-name-directory file))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region text nil file nil 'silent))
    file))

(defun harmless-profile--get (name)
  "Return the profile NAME, or signal."
  (setq name (harmless-profile--name name))
  (or (cl-find name (harmless-profile--read)
               :key (lambda (entry) (plist-get entry :name))
               :test #'equal)
      (error "no profile named %s" name)))

(defun harmless-profile--public (entry)
  "Return ENTRY with keys in a fixed order."
  (list :name (plist-get entry :name)
        :runtime (plist-get entry :runtime)
        :env (plist-get entry :env)))

(defun harmless-profile--set-p (env)
  "Return non-nil if ENV's value is a non-empty string.
The value itself is not returned."
  (let ((value (getenv env)))
    (and (stringp value)
         (not (string-empty-p (string-trim value))))))

;;;###autoload
(defun harmless-profile-add (name runtime env)
  "Register profile NAME for RUNTIME.
ENV is the environment variable that holds the token.  The
token is not stored.  Return the profile plist."
  (interactive
   (list (read-string "Profile: ")
         (read-string "Runtime: ")
         (read-string "Env: ")))
  (setq name (harmless-profile--name name))
  (setq runtime (harmless-profile--runtime runtime))
  (setq env (harmless-profile--env env))
  (let ((entries (harmless-profile--read)))
    (when (cl-find name entries
                   :key (lambda (entry) (plist-get entry :name))
                   :test #'equal)
      (error "profile is already registered: %s" name))
    (setq entries
          (sort (append entries
                        (list (list :name name
                                    :runtime runtime
                                    :env env)))
                (lambda (a b)
                  (string< (plist-get a :name)
                           (plist-get b :name)))))
    (harmless-profile--write entries))
  (message "Profile %s" name)
  (harmless-profile--public (harmless-profile--get name)))

;;;###autoload
(defun harmless-profile-list ()
  "Return profile names, sorted.
A missing file is nil.  This writes nothing."
  (interactive)
  (mapcar (lambda (entry) (plist-get entry :name))
          (harmless-profile--read)))

;;;###autoload
(defun harmless-profile-show (name)
  "Return profile NAME.
:set says whether its variable holds a token.  The token is
not returned.  This writes nothing."
  (interactive (list (read-string "Profile: ")))
  (let ((entry (harmless-profile--get name)))
    (append (harmless-profile--public entry)
            (list :set (and (harmless-profile--set-p
                             (plist-get entry :env))
                            t)))))

;;;###autoload
(defun harmless-profile-remove (name)
  "Forget profile NAME.
A seat that already names it keeps that name and its
conversation.  Return NAME."
  (interactive (list (read-string "Profile: ")))
  (setq name (harmless-profile--name name))
  (let* ((entries (harmless-profile--read))
         (left (cl-remove name entries
                          :key (lambda (entry) (plist-get entry :name))
                          :test #'equal)))
    (when (equal (length left) (length entries))
      (error "no profile named %s" name))
    (harmless-profile--write left))
  (message "Profile %s" name)
  name)

(defun harmless-profile-token (name)
  "Return the token for profile NAME from the environment.
This writes nothing.  A missing or blank value signals
\"profile token is missing\"."
  (let* ((entry (harmless-profile--get name))
         (value (getenv (plist-get entry :env))))
    (cond
     ((not (stringp value)) (error "profile token is missing"))
     (t
      (setq value (string-trim value))
      (if (string-empty-p value)
          (error "profile token is missing")
        value)))))

;;;###autoload
(defun harmless-profile-switch (rig pod seat profile)
  "Point SEAT at PROFILE and keep its conversation.
The profile's runtime must be the seat's runtime.  The session
and the inbound work stay.  The rig stores the profile name
and never the token.  Return the seat plist."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Pod: ")
         (read-string "Seat: ")
         (read-string "Profile: ")))
  (setq rig (harmless-team--rig-name rig))
  (setq pod (harmless-team--pod-name pod))
  (setq seat (harmless-team--seat-name seat))
  (setq profile (harmless-profile--name profile))
  (let* ((entry (harmless-profile--get profile))
         (loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (found (harmless-team--find stored pod seat)))
    (unless found
      (error "team seat is missing"))
    (let* ((item (cdr found))
           (runtime (plist-get item :runtime))
           (session (plist-get item :session)))
      (unless (and (stringp runtime)
                   (not (string-empty-p runtime))
                   (stringp session)
                   (not (string-empty-p session)))
        (error "team seat is not occupied"))
      (unless (equal runtime (plist-get entry :runtime))
        (error "profile runtime does not match"))
      (plist-put item :profile profile)
      (harmless-team--write (car loaded) stored)
      (message "Switched %s" (plist-get item :address))
      (harmless-team--public stored (car found) item))))

(provide 'harmless-profile)

;;; harmless-profile.el ends here
