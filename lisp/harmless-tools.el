;;; harmless-tools.el --- Tool registry for Harmless -*- lexical-binding: t; -*-

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
;; Named tools the model can call.  Each has a JSON schema, an Elisp
;; function, and a permission class (`read', `edit', or `shell').

;;; Code:

(require 'cl-lib)
(require 'harmless-util)

(cl-defstruct (harmless-tool
               (:constructor harmless-tool-create)
               (:copier nil))
  name
  description
  schema
  fn
  class)

(defvar harmless-tools nil
  "List of registered `harmless-tool' objects.")

(defun harmless-register-tool (tool)
  "Register TOOL, replacing any previous tool of the same name."
  (setq harmless-tools
        (cons tool
              (cl-remove (harmless-tool-name tool)
                         harmless-tools
                         :key #'harmless-tool-name
                         :test #'string=)))
  tool)

(defun harmless-tool-by-name (name)
  "Return the registered tool named NAME, or nil."
  (cl-find name harmless-tools :key #'harmless-tool-name :test #'string=))

(defun harmless-tools-enabled ()
  "Return the tools offered to the model."
  harmless-tools)

(defun harmless-tool-call (tool session args)
  "Run TOOL's function with SESSION and ARGS plist.
Return a string result, or a string starting with `Error:' on failure."
  (condition-case err
      (funcall (harmless-tool-fn tool) session args)
    (error (format "Error: %s" (error-message-string err)))))

(defun harmless-tool-parse-args (raw)
  "Parse RAW tool arguments (string or plist) into a plist."
  (cond
   ((null raw) nil)
   ((harmless-plist-p raw) raw)
   ((stringp raw)
    (or (harmless-json-decode-safe raw) nil))
   ((listp raw) raw)
   (t nil)))

(defun harmless-tool-arg (args key)
  "Get KEY from ARGS, accepting keyword or string names."
  (or (plist-get args key)
      (let ((name (if (keywordp key)
                      (substring (symbol-name key) 1)
                    key)))
        (or (plist-get args (intern (concat ":" name)))
            (cl-loop for (k v) on args by #'cddr
                     when (and (stringp k) (string= k name))
                     return v)))))

(provide 'harmless-tools)

;;; harmless-tools.el ends here
