;;; harmless-lsp.el --- Language server tool for Harmless -*- lexical-binding: t; -*-

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
;; One read-only tool, `lsp'.  It asks Eglot for a definition, for
;; references, or for diagnostics.  Harmless does not start a server
;; and does not open a file to find one.  Line and column arguments
;; start at 1.  Xref columns start at 0 and are shown starting at 1.
;; Nothing is written.  `write_file' is unchanged.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'xref)
(require 'flymake)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-tools)
(require 'harmless-tools-fs)

(declare-function harmless-ensure-configured "harmless")
(declare-function harmless--context-session "harmless")
(declare-function harmless-current "harmless")
(declare-function eglot-current-server "eglot" ())

(defconst harmless-lsp-operations
  '(definition references diagnostics)
  "Operations the lsp tool accepts.")

(defvar harmless-lsp-query-function nil
  "Function that answers `lsp' instead of Eglot, or nil.
Called as (SESSION OP PATH LINE COLUMN).  OP is `definition',
`references', or `diagnostics'.  PATH is absolute.  LINE and
COLUMN are 1-based, or nil for diagnostics.  Return a string.
Do not call `harmless-turn-run' from it, and do not edit the
project.  The tool does not start a language server.")

(defun harmless-lsp--name (key)
  "Return the field name of KEY."
  (cond
   ((keywordp key) (substring (symbol-name key) 1))
   ((symbolp key) (symbol-name key))
   ((stringp key) key)
   (t "")))

(defun harmless-lsp--has (plist key)
  "Return non-nil when PLIST sends KEY.
A present null is a value.  A missing key is not."
  (when (and (listp plist) (proper-list-p plist))
    (let ((name (harmless-lsp--name key)))
      (cl-loop for (k) on plist by #'cddr
               thereis (and (or (symbolp k) (stringp k))
                            (string= (harmless-lsp--name k) name))))))

(defun harmless-lsp--get (plist key)
  "Return KEY from PLIST, accepting a keyword or a string."
  (when (harmless-lsp--has plist key)
    (let ((name (harmless-lsp--name key)))
      (cl-loop for (k v) on plist by #'cddr
               when (and (or (symbolp k) (stringp k))
                         (string= (harmless-lsp--name k) name))
               return v))))

(defun harmless-lsp--operation (args)
  "Return the operation symbol in ARGS."
  (unless (harmless-lsp--has args :operation)
    (error "lsp operation is empty"))
  (let ((value (harmless-lsp--get args :operation)))
    (cond
     ((null value) (error "lsp operation is empty"))
     ((eq value :false) (error "lsp operation must be a string"))
     ((stringp value)
      (setq value (string-trim value))
      (when (string-empty-p value)
        (error "lsp operation is empty")))
     ((symbolp value)
      (setq value (symbol-name value))
      (when (string-prefix-p ":" value)
        (setq value (substring value 1)))
      (when (string-empty-p value)
        (error "lsp operation is not an operation")))
     (t (error "lsp operation must be a string")))
    (let ((op (intern value)))
      (unless (memq op harmless-lsp-operations)
        (error "lsp operation is not an operation"))
      op)))

(defun harmless-lsp--path-text (args)
  "Return the path text in ARGS.
A filesystem root is refused here and is not expanded."
  (unless (harmless-lsp--has args :path)
    (error "lsp path is empty"))
  (let ((value (harmless-lsp--get args :path)))
    (cond
     ((null value) (error "lsp path is empty"))
     ((eq value :false) (error "lsp path must be a string"))
     ((not (stringp value)) (error "lsp path must be a string")))
    (setq value (string-trim value))
    (when (string-empty-p value)
      (error "lsp path is empty"))
    (when (harmless-extra-root-text-p value)
      (error "lsp path is the filesystem root"))
    value))

(defun harmless-lsp--pos (value empty bad)
  "Return VALUE as a positive integer.
Signal EMPTY when it is missing and BAD when it is unusable."
  (cond
   ((null value) (error "%s" empty))
   ((eq value :false) (error "%s" bad))
   ((not (integerp value)) (error "%s" bad))
   ((<= value 0) (error "%s" bad))
   (t value)))

(defun harmless-lsp--position (op args)
  "Return (LINE COLUMN) for operation OP.
Diagnostics return (nil nil).  A position on diagnostics is
refused."
  (let ((has-line (harmless-lsp--has args :line))
        (has-column (harmless-lsp--has args :column)))
    (if (eq op 'diagnostics)
        (progn
          (when (or has-line has-column)
            (error "lsp diagnostics takes no position"))
          (list nil nil))
      (list
       (harmless-lsp--pos
        (if has-line (harmless-lsp--get args :line) nil)
        "lsp line is empty" "lsp line is not a line")
       (harmless-lsp--pos
        (if has-column (harmless-lsp--get args :column) nil)
        "lsp column is empty" "lsp column is not a column")))))

(defun harmless-lsp--file (session text)
  "Return TEXT as an absolute file SESSION may read."
  (let ((full (harmless-tools-fs-resolve session text)))
    (when (or (harmless-filesystem-root-p full)
              (harmless-extra-root-text-p full))
      (error "lsp path is the filesystem root"))
    (unless (and (file-exists-p full)
                 (file-regular-p full)
                 (not (file-directory-p full)))
      (error "lsp path is not a file"))
    full))

(defun harmless-lsp--result (value)
  "Return VALUE when it is text the model can read."
  (cond
   ((null value) (error "lsp query returned nothing"))
   ((not (stringp value)) (error "lsp query must be a string"))
   (t value)))

(defun harmless-lsp--display (session file)
  "Return FILE relative to SESSION when it stays in the project."
  (cond
   ((not (stringp file)) "?")
   ((not (file-name-absolute-p file)) file)
   (t
    (let ((root (ignore-errors
                  (harmless-session-require-project session))))
      (if (and root
               (ignore-errors
                 (harmless-tools-fs-inside-p root file)))
          (file-relative-name file (directory-file-name root))
        file)))))

(defun harmless-lsp--column (location)
  "Return a 1-based column for an xref LOCATION.
Xref file columns start at 0."
  (max 1
       (1+ (or (and (xref-file-location-p location)
                    (xref-file-location-column location))
               0))))

(defun harmless-lsp--xref< (a b)
  "Return non-nil when xref item A sorts before B."
  (let* ((la (xref-item-location a))
         (lb (xref-item-location b))
         (fa (or (xref-location-group la) ""))
         (fb (or (xref-location-group lb) ""))
         (na (or (xref-location-line la) 1))
         (nb (or (xref-location-line lb) 1)))
    (or (string< fa fb)
        (and (string= fa fb)
             (or (< na nb)
                 (and (= na nb)
                      (< (harmless-lsp--column la)
                         (harmless-lsp--column lb))))))))

(defun harmless-lsp--format-xrefs (session items empty)
  "Format xref ITEMS for SESSION, or EMPTY when there are none."
  (unless (and (listp items)
               (proper-list-p items)
               (cl-every #'xref-item-p items))
    (error "lsp query failed"))
  (if (null items)
      empty
    (mapconcat
     (lambda (item)
       (let ((loc (xref-item-location item)))
         (format "%s:%d:%d"
                 (harmless-lsp--display
                  session (xref-location-group loc))
                 (or (xref-location-line loc) 1)
                 (harmless-lsp--column loc))))
     (sort (copy-sequence items) #'harmless-lsp--xref<)
     "\n")))

(defun harmless-lsp--one-line (text)
  "Return TEXT as one trimmed line."
  (substring-no-properties
   (string-trim
    (replace-regexp-in-string
     "[\n\r\t]+" " "
     (if (stringp text) text "")))))

(defun harmless-lsp--severity (type)
  "Return a severity symbol for a Flymake TYPE."
  (let ((name
         (cond
          ((symbolp type)
           (let ((raw (symbol-name type)))
             (if (string-prefix-p ":" raw)
                 (substring raw 1)
               raw)))
          ((stringp type) (string-trim type))
          (t ""))))
    (cond
     ((member name '("error" "warning")) (intern name))
     ((member name '("note" "info" "information" "hint")) 'note)
     ((string-empty-p name) 'error)
     (t (intern name)))))

(defun harmless-lsp--diagnostic-message (diag)
  "Return DIAG's message.
Emacs 31 can select the message part.  Emacs 30 returns the text
slot, and its byte compiler inlines a one-argument call, so the
two-argument call has to go through `apply'."
  (apply #'flymake-diagnostic-text
         (if (>= emacs-major-version 31)
             (list diag '(message))
           (list diag))))

(defun harmless-lsp--diagnostic-rows (diags)
  "Return (LINE COLUMN SEVERITY TEXT) rows for Flymake DIAGS.
LINE and COLUMN start at 1."
  (unless (and (listp diags) (proper-list-p diags))
    (error "lsp query failed"))
  (mapcar
   (lambda (diag)
     (let ((buf (flymake-diagnostic-buffer diag))
           (beg (flymake-diagnostic-beg diag))
           (line 1)
           (column 1))
       (when (and (bufferp buf) (number-or-marker-p beg))
         (with-current-buffer buf
           (save-excursion
             (goto-char beg)
             (setq line (line-number-at-pos (point) t))
             (setq column (1+ (current-column))))))
       (list line column
             (harmless-lsp--severity (flymake-diagnostic-type diag))
             (harmless-lsp--one-line
              (harmless-lsp--diagnostic-message diag)))))
   diags))

(defun harmless-lsp--diag< (a b)
  "Return non-nil when diagnostic row A sorts before B."
  (or (< (nth 0 a) (nth 0 b))
      (and (= (nth 0 a) (nth 0 b))
           (or (< (nth 1 a) (nth 1 b))
               (and (= (nth 1 a) (nth 1 b))
                    (or (string< (format "%s" (nth 2 a))
                                 (format "%s" (nth 2 b)))
                        (and (equal (nth 2 a) (nth 2 b))
                             (string< (nth 3 a) (nth 3 b)))))))))

(defun harmless-lsp--format-diagnostics (rows)
  "Format diagnostic ROWS, or \"No diagnostics\" when empty."
  (if (null rows)
      "No diagnostics"
    (mapconcat
     (lambda (row)
       (format "%d:%d: %s: %s"
               (nth 0 row) (nth 1 row) (nth 2 row) (nth 3 row)))
     (sort (copy-sequence rows) #'harmless-lsp--diag<)
     "\n")))

(defun harmless-lsp--goto (line column)
  "Move to 1-based LINE and COLUMN in the current buffer.
A column past the end of the line stops at the end of the line."
  (goto-char (point-min))
  (forward-line (1- (max 1 line)))
  (let ((at (1- (max 1 column))))
    (goto-char (min (+ (point) at) (line-end-position)))))

(defun harmless-lsp--ask-eglot (session op path line column)
  "Ask the Eglot server visiting PATH.
OP is `definition', `references', or `diagnostics'.  LINE and
COLUMN start at 1.  This does not start a server and does not
open PATH."
  (let ((buf (find-buffer-visiting path)))
    (unless (buffer-live-p buf)
      (error "lsp server is not running"))
    (unless (require 'eglot nil t)
      (error "lsp server is not running"))
    (with-current-buffer buf
      (unless (eglot-current-server)
        (error "lsp server is not running"))
      (save-restriction
        (widen)
        (save-excursion
          (when (and line column)
            (harmless-lsp--goto line column))
          (let ((raw
                 (condition-case err
                     (cond
                      ((eq op 'definition)
                       (xref-backend-definitions 'eglot "identifier"))
                      ((eq op 'references)
                       (xref-backend-references 'eglot "identifier"))
                      (t (flymake-diagnostics)))
                   (error
                    (if (string-match-p
                         "No current JSON-RPC"
                         (error-message-string err))
                        (error "lsp server is not running")
                      (error "lsp query failed"))))))
            (if (eq op 'diagnostics)
                (harmless-lsp--format-diagnostics
                 (harmless-lsp--diagnostic-rows raw))
              (harmless-lsp--format-xrefs
               session raw
               (if (eq op 'definition)
                   "No definition"
                 "No references")))))))))

(defun harmless-lsp-run (session args)
  "Run an lsp query for SESSION and ARGS.
ARGS is a plist :operation, :path, and, except for diagnostics,
:line and :column.  Nothing is written."
  (unless (harmless-session-p session)
    (error "No Harmless session"))
  (let* ((op (harmless-lsp--operation args))
         (text (harmless-lsp--path-text args))
         (pos (harmless-lsp--position op args))
         (full (harmless-lsp--file session text)))
    (harmless-lsp--result
     (if (functionp harmless-lsp-query-function)
         (funcall harmless-lsp-query-function
                  session op full (nth 0 pos) (nth 1 pos))
       (harmless-lsp--ask-eglot
        session op full (nth 0 pos) (nth 1 pos))))))

(defun harmless-lsp-tool (session args)
  "lsp implementation."
  (harmless-lsp-run session args))

(defun harmless-lsp-read-args ()
  "Read an operation, a file, and a position from the minibuffer."
  (let ((op (completing-read
             "LSP: "
             '("definition" "references" "diagnostics")
             nil t nil nil "definition")))
    (if (equal op "diagnostics")
        (list :operation op :path (read-file-name "File: "))
      (list :operation op
            :path (read-file-name "File: ")
            :line (read-number "Line: " 1)
            :column (read-number "Column: " 1)))))

;;;###autoload
(defun harmless-lsp (&optional session args)
  "Ask a language server about SESSION's project.
ARGS is a plist :operation, :path, :line, and :column.  The file
must already be open in a buffer whose server is running.
Harmless does not start a server.  Nothing is written."
  (interactive (list nil (harmless-lsp-read-args)))
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
  (let ((result (harmless-lsp-run session args)))
    (when (called-interactively-p 'any)
      (message "%s" result))
    result))

(defun harmless-lsp-register ()
  "Register the lsp tool."
  (harmless-register-tool
   (harmless-tool-create
    :name "lsp"
    :description "Ask a language server that is already running. operation is definition, references, or diagnostics. path is a project file. line and column start at 1 and are required for definition and references. diagnostics reads the whole file and takes no position. Harmless does not start a server and does not edit the file."
    :class 'read
    :schema '(:type "object"
              :properties
              (:operation
               (:type "string"
                :description "definition, references, or diagnostics.")
               :path (:type "string" :description "Project file.")
               :line (:type "integer"
                      :description "1-based line. Not for diagnostics.")
               :column (:type "integer"
                        :description "1-based column. Not for diagnostics."))
              :required ("operation" "path"))
    :fn #'harmless-lsp-tool)))

(harmless-lsp-register)

(provide 'harmless-lsp)

;;; harmless-lsp.el ends here
