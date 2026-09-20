;;; harmless-util.el --- Shared helpers for Harmless -*- lexical-binding: t; -*-

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
;; JSON conversion, UUIDs, and small predicates used throughout Harmless.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'browse-url)

(defgroup harmless nil
  "AI coding harness that lives inside Emacs."
  :group 'tools
  :prefix "harmless-")

(defvar harmless-browse-url-function nil
  "Function used to open login and similar URLs.
Nil means `browse-url-default-browser'.")

(defcustom harmless-browse-url-function nil
  "Function used to open login and similar URLs.
Nil means `browse-url-default-browser' (the desktop browser), not
`browse-url-browser-function', which is often an in-Emacs browser
such as w3m or eww."
  :type '(choice (const :tag "System default browser" nil)
                 function)
  :group 'harmless)

(defun harmless-browse-url (url)
  "Open URL in an external browser.
Ignores `browse-url-browser-function' unless
`harmless-browse-url-function' is set."
  (funcall (or (symbol-value 'harmless-browse-url-function)
               #'browse-url-default-browser)
           url))

(defconst harmless-version "0.1.0"
  "Harmless version string.")

(defun harmless-uuid ()
  "Return a 36-character hex id shaped like a UUID."
  (let ((s (secure-hash 'md5 (format "%s|%s|%s"
                                     (float-time)
                                     (random most-positive-fixnum)
                                     (emacs-pid)))))
    (format "%s-%s-%s-%s-%s"
            (substring s 0 8)
            (substring s 8 12)
            (substring s 12 16)
            (substring s 16 20)
            (substring s 20 32))))

(defun harmless-now-iso ()
  "Return the current UTC time as an ISO-8601 string."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t))

(defun harmless-url-encode (string)
  "Percent-encode STRING for use in a filename."
  (url-hexify-string string))

(defun harmless-plist-p (obj)
  "Return non-nil if OBJ looks like a keyword plist."
  (and (consp obj) (keywordp (car obj))))

(defun harmless-json-prepare (obj)
  "Convert OBJ into a value `json-serialize' accepts."
  (cond
   ((eq obj :false) :false)
   ((eq obj :null) :null)
   ((eq obj t) t)
   ((null obj) :null)
   ((keywordp obj) (substring (symbol-name obj) 1))
   ((symbolp obj) (symbol-name obj))
   ((stringp obj) (harmless-ensure-utf8 obj))
   ((numberp obj) obj)
   ((hash-table-p obj)
    (let ((out (make-hash-table :test 'equal)))
      (maphash (lambda (k v)
                 (puthash (if (stringp k) k (harmless-json-prepare k))
                          (harmless-json-prepare v)
                          out))
               obj)
      out))
   ((vectorp obj)
    (cl-map 'vector #'harmless-json-prepare obj))
   ((harmless-plist-p obj)
    (let ((out (make-hash-table :test 'equal)))
      (while obj
        (puthash (substring (symbol-name (car obj)) 1)
                 (harmless-json-prepare (cadr obj))
                 out)
        (setq obj (cddr obj)))
      out))
   ((listp obj)
    (cl-map 'vector #'harmless-json-prepare obj))
   (t (error "Cannot JSON-encode %S" obj))))

(defun harmless-json-encode (obj)
  "Encode OBJ as a unibyte UTF-8 JSON string."
  (json-serialize (harmless-json-prepare obj)))

(defun harmless-json-text (obj)
  "Encode OBJ as multibyte Unicode JSON, safe to insert into a buffer."
  (decode-coding-string (harmless-json-encode obj) 'utf-8-unix))

(defun harmless-json-decode (string)
  "Decode JSON STRING into plists and lists."
  (json-parse-string string
                     :object-type 'plist
                     :array-type 'list
                     :null-object nil
                     :false-object :false))

(defun harmless-json-decode-safe (string)
  "Decode JSON STRING, or return nil if it is not valid JSON."
  (condition-case nil
      (harmless-json-decode string)
    (json-parse-error nil)
    (error nil)))

(defun harmless-plist-get (obj key &rest more)
  "Get KEY from plist or hash OBJ, then each of MORE."
  (let ((v (cond
            ((hash-table-p obj)
             (or (gethash key obj)
                 (and (keywordp key)
                      (gethash (substring (symbol-name key) 1) obj))))
            ((listp obj)
             (or (plist-get obj key)
                 (and (keywordp key)
                      (plist-get obj (intern (substring (symbol-name key) 1))))))
            (t nil))))
    (if more
        (apply #'harmless-plist-get v more)
      v)))

(defun harmless-json-true-p (value)
  "Return non-nil if VALUE is a JSON true, not false or null."
  (and value (not (eq value :false))))

(defun harmless-ensure-utf8 (string)
  "Return STRING as Unicode text.
If STRING contains Emacs eight-bit characters (raw bytes from an
undecoded process), interpret those bytes as UTF-8."
  (cond
   ((not (stringp string)) string)
   ((not (multibyte-string-p string))
    (decode-coding-string string 'utf-8-unix t))
   (t
    (let ((i 0)
          (n (length string))
          raw)
      (while (and (< i n) (not raw))
        (when (>= (aref string i) #x3FFF80)
          (setq raw t))
        (setq i (1+ i)))
      (if (not raw)
          string
        (decode-coding-string
         (encode-coding-string string 'raw-text-unix)
         'utf-8-unix t))))))

(defun harmless-truncate (string n)
  "Return STRING truncated to at most N characters."
  (setq string (harmless-ensure-utf8 string))
  (if (<= (length string) n)
      string
    (concat (substring string 0 (max 0 (- n 1))) "…")))

(defun harmless-ensure-directory (dir)
  "Create DIR and parents if they do not exist.  Return DIR."
  (make-directory dir t)
  dir)

(provide 'harmless-util)

;;; harmless-util.el ends here
