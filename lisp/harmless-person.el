;;; harmless-person.el --- A person is an address -*- lexical-binding: t; -*-

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
;; A person is an address, `name@slack`.  Asking that person
;; opens a queue row on the rig.  The row's history is the
;; receipt.  Slack is the connector.  The token is read from
;; the environment when a sender is set.  It is not written
;; into the rig or the people file.  Nothing is started.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-team)

(defvar harmless-directory)

(defvar harmless-person-slack-function nil
  "Function (TOKEN CHANNEL TEXT) that posts one Slack message.
Nil records the ask and does not post.  TOKEN is the
environment value of `SLACK_BOT_TOKEN'.  The function must
not write TOKEN into the rig or the people file.")

(defun harmless-person-name-p (name)
  "Return non-nil if NAME is a person id, not a path.
A hyphen would look like a seat address, so NAME has none.
Letters are lowercase.  Matching does not ignore case."
  (and (stringp name)
       (let ((case-fold-search nil))
         (string-match-p "\\`[a-z0-9]+\\'" name))))

(defun harmless-person--name (name)
  "Return NAME as a person id, or signal.
Nil and \"\" are empty.  A path, \"~\", and the filesystem
root are not names and are not expanded."
  (cond
   ((null name) (error "person name is empty"))
   ((not (stringp name)) (error "person name must be a string"))
   (t
    (setq name (string-trim name))
    (cond
     ((string-empty-p name) (error "person name is empty"))
     ((or (string-prefix-p "~" name)
          (string-prefix-p "/" name)
          (string-prefix-p "\\" name)
          (string-match-p "[/\\\\]" name)
          (string-match-p "\\`[A-Za-z]:[/\\]?\\'" name)
          (file-name-absolute-p name)
          (not (harmless-person-name-p name)))
      (error "person name is not a name"))
     (t name)))))

(defun harmless-person--channel (channel)
  "Return CHANNEL as a Slack channel, or signal.
A leading \"#\" is removed.  A path, \"~\", and a token are
refused and are not stored."
  (cond
   ((null channel) (error "person channel is empty"))
   ((not (stringp channel))
    (error "person channel must be a string"))
   (t
    (setq channel (string-trim channel))
    (when (and (not (string-empty-p channel))
               (eq (aref channel 0) ?#))
      (setq channel (substring channel 1)))
    (cond
     ((string-empty-p channel) (error "person channel is empty"))
     ((or (string-prefix-p "~" channel)
          (string-prefix-p "/" channel)
          (string-prefix-p "\\" channel)
          (string-match-p "[/\\\\[:space:]]" channel)
          (string-match-p "\\`[A-Za-z]:" channel)
          (file-name-absolute-p channel)
          (string-match-p "xox[abprs]-" channel)
          (string-match-p "token=" channel)
          (not (string-match-p
                "\\`[A-Za-z0-9][A-Za-z0-9_-]*\\'" channel)))
      (error "person channel is not a channel"))
     (t channel)))))

(defun harmless-person--question (text)
  "Return TEXT as the question, or signal."
  (cond
   ((null text) (error "person question is empty"))
   ((not (stringp text))
    (error "person question must be a string"))
   (t
    (setq text (string-trim text))
    (if (string-empty-p text)
        (error "person question is empty")
      text))))

(defun harmless-person--address (name)
  "Return the address of person NAME."
  (format "%s@slack" name))

(defun harmless-person--root-p (text)
  "Return non-nil if TEXT is \"/\" or a drive root."
  (or (harmless-filesystem-root-p text)
      (harmless-extra-root-text-p text)))

(defun harmless-person--file (rig)
  "Return RIG's people file, or signal.
The file is not created.  RIG is already a rig name."
  (let ((raw harmless-directory))
    (when (stringp raw)
      (setq raw (string-trim raw)))
    (when (and (stringp raw)
               (not (string-empty-p raw))
               (harmless-person--root-p raw))
      (error "Refusing to store a person at %s" raw)))
  (let* ((root (harmless-team--teams-root nil))
         (file (expand-file-name (concat rig ".people.json") root)))
    (unless (and (file-name-absolute-p file)
                 (harmless-directory-strictly-under-p file root)
                 (not (harmless-person--root-p file))
                 (not (equal (file-truename file)
                             (file-truename "/"))))
      (error "Refusing to store a person at %s" file))
    file))

(defun harmless-person--text (entries)
  "Return ENTRIES as JSON with stable key order."
  (concat
   "["
   (mapconcat
    (lambda (entry)
      (concat "{\"name\":"
              (harmless-json-text (plist-get entry :name))
              ",\"connector\":\"slack\",\"channel\":"
              (harmless-json-text (plist-get entry :channel))
              "}"))
    entries
    ",")
   "]"))

(defun harmless-person--read-text (file)
  "Return the text of FILE."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents file))
    (buffer-string)))

(defun harmless-person--entries (parsed)
  "Return person plists from PARSED, or signal.
A token, a secret, or a password is not valid."
  (cond
   ((null parsed) nil)
   ((not (and (listp parsed) (proper-list-p parsed)))
    (error "people are not valid"))
   (t
    (let ((out nil)
          (seen (make-hash-table :test 'equal)))
      (dolist (item parsed)
        (unless (and (harmless-plist-p item)
                     (not (plist-member item :token))
                     (not (plist-member item :secret))
                     (not (plist-member item :password))
                     (equal (plist-get item :connector) "slack"))
          (error "people are not valid"))
        (let ((name (condition-case nil
                        (harmless-person--name (plist-get item :name))
                      (error (error "people are not valid"))))
              (channel (condition-case nil
                           (harmless-person--channel
                            (plist-get item :channel))
                         (error (error "people are not valid")))))
          (when (gethash name seen)
            (error "people are not valid"))
          (puthash name t seen)
          (push (list :name name
                      :address (harmless-person--address name)
                      :connector "slack"
                      :channel channel)
                out)))
      (sort out (lambda (a b)
                  (string< (plist-get a :name)
                           (plist-get b :name))))))))

(defun harmless-person--read (rig)
  "Return RIG's people, sorted by name.
A missing file is an empty list."
  (setq rig (harmless-team--rig-name rig))
  (let ((file (harmless-person--file rig)))
    (cond
     ((not (file-exists-p file)) nil)
     ((not (file-regular-p file))
      (error "people are not valid"))
     (t
      (harmless-person--entries
       (condition-case nil
           (harmless-json-decode (harmless-person--read-text file))
         (error (error "people are not valid"))))))))

(defun harmless-person--write (rig entries)
  "Write ENTRIES for RIG and return that path."
  (let ((file (harmless-person--file rig))
        (text (concat (harmless-person--text entries) "\n")))
    (harmless-ensure-directory (file-name-directory file))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region text nil file nil 'silent))
    file))

(defun harmless-person--get (rig name)
  "Return the person NAME in RIG, or signal."
  (setq name (harmless-person--name name))
  (or (cl-find name (harmless-person--read rig)
               :key (lambda (entry) (plist-get entry :name))
               :test #'equal)
      (error "no person named %s" name)))

(defun harmless-person--token ()
  "Return the Slack token from the environment, or nil.
This does not read the rig or the people file."
  (let ((token (getenv "SLACK_BOT_TOKEN")))
    (cond
     ((not (stringp token)) nil)
     (t
      (setq token (string-trim token))
      (if (string-empty-p token) nil token)))))

(defun harmless-person--deliver (channel text)
  "Post TEXT to CHANNEL, or record that Slack was not posted.
Return the receipt text.  The token is not part of that text."
  (let ((token (harmless-person--token))
        (sender harmless-person-slack-function))
    (cond
     ((null sender) "slack was not posted")
     ((null token) "slack token is missing")
     (t
      (condition-case nil
          (progn
            (funcall sender token channel text)
            "delivered to slack")
        (error "slack was not posted"))))))

(defun harmless-person--public (entry)
  "Return ENTRY with keys in a fixed order."
  (list :name (plist-get entry :name)
        :address (plist-get entry :address)
        :connector (plist-get entry :connector)
        :channel (plist-get entry :channel)))

;;;###autoload
(defun harmless-person-add (rig name channel)
  "Register person NAME on RIG at Slack CHANNEL.
The address is NAME@slack.  CHANNEL is not a secret.  The
Slack token is not stored.  Return the person plist."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Person: ")
         (read-string "Channel: ")))
  (setq rig (harmless-team--rig-name rig))
  (setq name (harmless-person--name name))
  (setq channel (harmless-person--channel channel))
  (harmless-team--load rig)
  (let ((entries (harmless-person--read rig)))
    (when (cl-find name entries
                   :key (lambda (entry) (plist-get entry :name))
                   :test #'equal)
      (error "person is already registered: %s" name))
    (setq entries
          (sort (append entries
                        (list (list :name name
                                    :address (harmless-person--address name)
                                    :connector "slack"
                                    :channel channel)))
                (lambda (a b)
                  (string< (plist-get a :name)
                           (plist-get b :name)))))
    (harmless-person--write rig entries))
  (message "Person %s" (harmless-person--address name))
  (harmless-person--public (harmless-person--get rig name)))

;;;###autoload
(defun harmless-person-list (rig)
  "Return RIG's person names, sorted.
A missing file is nil.  This writes nothing."
  (interactive (list (read-string "Rig: ")))
  (mapcar (lambda (entry) (plist-get entry :name))
          (harmless-person--read rig)))

;;;###autoload
(defun harmless-person-show (rig name)
  "Return person NAME on RIG.
This writes nothing."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Person: ")))
  (harmless-person--public (harmless-person--get rig name)))

;;;###autoload
(defun harmless-person-remove (rig name)
  "Forget person NAME on RIG.
Rows already opened for that address stay.  Return NAME."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Person: ")))
  (setq rig (harmless-team--rig-name rig))
  (setq name (harmless-person--name name))
  (let* ((entries (harmless-person--read rig))
         (left (cl-remove name entries
                          :key (lambda (entry) (plist-get entry :name))
                          :test #'equal)))
    (when (equal (length left) (length entries))
      (error "no person named %s" name))
    (harmless-person--write rig left))
  (message "Person %s" name)
  name)

;;;###autoload
(defun harmless-person-ask (rig name question)
  "Open a queue row asking person NAME on RIG.
QUESTION is the row body.  The history is the receipt: the
ask, then whether Slack was posted.  The token is not written.
Return the row."
  (interactive
   (list (read-string "Rig: ")
         (read-string "Person: ")
         (read-string "Question: ")))
  (setq rig (harmless-team--rig-name rig))
  (setq name (harmless-person--name name))
  (setq question (harmless-person--question question))
  (let* ((person (harmless-person--get rig name))
         (address (plist-get person :address))
         (channel (plist-get person :channel))
         (loaded (harmless-team--load rig))
         (stored (cdr loaded))
         (row (list :id (harmless-team--next-row-id
                         (plist-get stored :rows))
                    :seat address
                    :body question
                    :state "open"
                    :history
                    (list (list :at (harmless-now-iso) :text "opened")
                          (list :at (harmless-now-iso)
                                :text (format
                                       "asked %s on slack channel %s"
                                       address channel))))))
    (plist-put stored :rows
               (append (plist-get stored :rows) (list row)))
    (harmless-team--write (car loaded) stored)
    (let* ((receipt (harmless-person--deliver channel question))
           (again (harmless-team--load rig))
           (found (harmless-team--row-stored
                   (cdr again) (plist-get row :id))))
      (harmless-team--row-append found receipt)
      (harmless-team--write (car again) (cdr again))
      (message "Asked %s" address)
      (harmless-team--public-row found))))

(provide 'harmless-person)

;;; harmless-person.el ends here
