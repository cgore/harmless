;;; harmless-execute-child.el --- Interpreter for execute_code -*- lexical-binding: t; -*-

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
;; Loaded by `emacs -Q --batch -l' in a process of its own.  Harmless does
;; not load this file into the user's Emacs.  When HARMLESS_EXECUTE_PORT is
;; unset, loading the file does nothing.  The process connects to 127.0.0.1
;; on that port, reads one JSON line per script, and evaluates it.  Tool
;; calls go back to the parent.  Only printed output is returned.

;;; Code:

(defvar harmless-execute-child--proc nil
  "Network connection to the Harmless Emacs, or nil.")

(defvar harmless-execute-child--output nil
  "Buffer that collects printed output for one script.")

(defun harmless-execute-child--filter (proc text)
  "Append TEXT to PROC's inbox."
  (process-put proc 'inbox
               (concat (or (process-get proc 'inbox) "") text)))

(defun harmless-execute-child--take-line (proc)
  "Return PROC's next complete line, or nil."
  (let* ((inbox (or (process-get proc 'inbox) ""))
         (pos (string-search "\n" inbox)))
    (when pos
      (process-put proc 'inbox (substring inbox (1+ pos)))
      (let ((line (substring inbox 0 pos)))
        (if (string-suffix-p "\r" line)
            (substring line 0 -1)
          line)))))

(defun harmless-execute-child--read-line (proc)
  "Return PROC's next line.  A closed connection signals."
  (let ((line (harmless-execute-child--take-line proc)))
    (while (not line)
      (unless (process-live-p proc)
        (error "execute code failed: interpreter exited"))
      (accept-process-output proc 0.1)
      (setq line (harmless-execute-child--take-line proc)))
    line))

(defun harmless-execute-child--value (value)
  "Return VALUE in a form `json-serialize' accepts."
  (cond
   ((eq value t) t)
   ((eq value :false) :false)
   ((eq value :null) :null)
   ((null value) :null)
   ((or (stringp value) (numberp value)) value)
   ((keywordp value) (substring (symbol-name value) 1))
   ((symbolp value) (symbol-name value))
   ((and (listp value) (keywordp (car value)))
    (harmless-execute-child--args value))
   ((listp value)
    (vconcat (mapcar #'harmless-execute-child--value value)))
   ((hash-table-p value) value)
   ((vectorp value) value)
   (t (format "%s" value))))

(defun harmless-execute-child--args (args)
  "Return keyword plist ARGS as a JSON object."
  (let ((table (make-hash-table :test 'equal)))
    (while (and (consp args) (keywordp (car args)) (consp (cdr args)))
      (puthash (substring (symbol-name (car args)) 1)
               (harmless-execute-child--value (cadr args))
               table)
      (setq args (cddr args)))
    table))

(defun harmless-execute-child--send (proc plist)
  "Send PLIST to PROC as one JSON line."
  (let ((table (make-hash-table :test 'equal)))
    (while plist
      (puthash (substring (symbol-name (car plist)) 1)
               (harmless-execute-child--value (cadr plist))
               table)
      (setq plist (cddr plist)))
    (process-send-string proc (concat (json-serialize table) "\n"))))

(defun harmless-execute-child--call (name &rest args)
  "Ask the parent to call tool NAME with keyword ARGS."
  (harmless-execute-child--send
   harmless-execute-child--proc
   (list :op "tool" :name name :args (harmless-execute-child--args args)))
  (let* ((line (harmless-execute-child--read-line harmless-execute-child--proc))
         (msg (json-parse-string line
                                 :object-type 'hash-table
                                 :array-type 'list
                                 :null-object nil
                                 :false-object :false))
         (ok (gethash "ok" msg)))
    (if (and ok (not (eq ok :false)))
        (or (gethash "text" msg) "")
      (error "%s" (or (gethash "error" msg) "execute code failed")))))

(defun harmless-execute-child--install (names)
  "Define each tool in NAMES that is not already defined.
`tool' is defined again on every script, so a script cannot replace it."
  (when (vectorp names)
    (setq names (append names nil)))
  (dolist (name names)
    (when (and (stringp name) (not (string-empty-p name)))
      (let ((tool-name name)
            (sym (intern name)))
        (unless (fboundp sym)
          (defalias sym
            (lambda (&rest args)
              (apply #'harmless-execute-child--call tool-name args)))))))
  (defalias 'tool
    (lambda (name &rest args)
      (apply #'harmless-execute-child--call
             (cond
              ((symbolp name) (symbol-name name))
              ((stringp name) name)
              (t (format "%s" name)))
             args))))

(defun harmless-execute-child--message (format-string &rest args)
  "Print FORMAT-STRING the way a script's `message' should.
The text goes to the print buffer, followed by a newline."
  (let ((text (cond
               ((null format-string) "")
               ((stringp format-string)
                (apply #'format format-string args))
               (t (format "%s" format-string)))))
    (princ text)
    (princ "\n")
    text))

(defun harmless-execute-child--output-buffer ()
  "Return the buffer that collects one script's printed output."
  (unless (buffer-live-p harmless-execute-child--output)
    (setq harmless-execute-child--output
          (get-buffer-create " *harmless-execute-output*")))
  harmless-execute-child--output)

(defun harmless-execute-child--eval (code names)
  "Evaluate CODE after installing NAMES.
Return (done TEXT) or (error MESSAGE TEXT)."
  (harmless-execute-child--install names)
  (with-current-buffer (harmless-execute-child--output-buffer)
    (erase-buffer)
    (condition-case err
        (let ((standard-output (current-buffer))
              (form (read (concat "(progn " code ")")))
              (orig (symbol-function 'message)))
          (unwind-protect
              (progn
                (fset 'message #'harmless-execute-child--message)
                (eval form nil))
            (fset 'message orig))
          (list 'done (buffer-string)))
      (error (list 'error (error-message-string err) (buffer-string))))))

(defun harmless-execute-child--handle (proc line)
  "Answer one request LINE on PROC."
  (let* ((msg (json-parse-string line
                                 :object-type 'hash-table
                                 :array-type 'list
                                 :null-object nil
                                 :false-object :false))
         (op (gethash "op" msg)))
    (when (equal op "exec")
      (let* ((names (gethash "names" msg))
             (result (harmless-execute-child--eval
                      (or (gethash "code" msg) "")
                      names)))
        (if (eq (car result) 'done)
            (harmless-execute-child--send
             proc (list :op "done" :output (nth 1 result)))
          (harmless-execute-child--send
           proc (list :op "error"
                      :message (or (nth 1 result) "")
                      :output (or (nth 2 result) ""))))))))

(defun harmless-execute-child--start ()
  "Connect to the parent and evaluate scripts until the socket closes."
  (let ((port (getenv "HARMLESS_EXECUTE_PORT")))
    (when (and (stringp port) (string-match-p "\\`[0-9]+\\'" port))
      (let ((proc (make-network-process
                   :name "harmless-execute"
                   :host "127.0.0.1"
                   :service (string-to-number port)
                   :family 'ipv4
                   :coding '(utf-8-unix . utf-8-unix)
                   :noquery t
                   :filter #'harmless-execute-child--filter)))
        (setq harmless-execute-child--proc proc)
        (set-process-query-on-exit-flag proc nil)
        (set-process-coding-system proc 'utf-8-unix 'utf-8-unix)
        (while (process-live-p proc)
          (condition-case err
              (harmless-execute-child--handle
               proc (harmless-execute-child--read-line proc))
            (error
             (condition-case nil
                 (harmless-execute-child--send
                  proc (list :op "error"
                             :message (error-message-string err)
                             :output ""))
               (error (setq proc nil))))))
        (kill-emacs 0)))))

(provide 'harmless-execute-child)

(harmless-execute-child--start)

;;; harmless-execute-child.el ends here
