;;; harmless-tools-shell.el --- Shell tool for Harmless -*- lexical-binding: t; -*-

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
;; run_shell: async `make-process' in the session cwd.  The tool function
;; takes a callback so the turn loop can keep Emacs responsive.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-tools)

(defcustom harmless-shell-output-max-bytes 20000
  "Maximum bytes of shell output returned to the model.
The UI can still show the full buffer contents."
  :type 'integer
  :group 'harmless)

(defcustom harmless-shell-timeout-seconds 120
  "Seconds to wait for a shell command before killing it."
  :type 'number
  :group 'harmless)

(defun harmless-tools-shell-kill (proc)
  "SIGTERM PROC, then SIGKILL shortly after if it is still alive."
  (when (and proc (process-live-p proc))
    (ignore-errors (signal-process proc 'SIGTERM))
    (run-at-time 1.5 nil
                 (lambda (p)
                   (when (process-live-p p)
                     (ignore-errors (delete-process p))))
                 proc)))

(defun harmless-tools-shell--run (session args callback)
  "Run ARGS command in SESSION cwd and call CALLBACK with the output string."
  (let ((command (harmless-tool-arg args :command)))
    (if (not (and command (not (string-empty-p command))))
        (funcall callback "Error: command is empty")
    (let* ((buf (generate-new-buffer " *harmless-shell*"))
           (cwd (harmless-session-cwd session))
           (timed-out nil)
           (timer nil)
           (proc nil))
      (setq proc
            (let ((default-directory cwd))
            (make-process
             :name "harmless-shell"
             :buffer buf
             :command (list shell-file-name
                            (or shell-command-switch "-c")
                            command)
             :coding '(utf-8-unix . utf-8-unix)
             :connection-type 'pipe
             :sentinel
             (lambda (p _change)
               (unless (process-live-p p)
                 (when timer (cancel-timer timer))
                 (let* ((raw (if (buffer-live-p buf)
                                 (with-current-buffer buf (buffer-string))
                               ""))
                        (exit (process-exit-status p))
                        (full (format "exit %s\n%s" exit raw))
                        (for-model (if (> (string-bytes full)
                                          harmless-shell-output-max-bytes)
                                       (concat
                                        (substring full 0
                                                   (min (length full)
                                                        harmless-shell-output-max-bytes))
                                        "\n[truncated]")
                                     full)))
                   (when (buffer-live-p buf)
                     (kill-buffer buf))
                   (when (eq (harmless-session-process session) p)
                     (setf (harmless-session-process session) nil))
                   (funcall callback
                            (if timed-out
                                (concat "Error: timed out\n" for-model)
                              for-model))))))))
      (setf (harmless-session-process session) proc)
      (setq timer
            (run-at-time
             harmless-shell-timeout-seconds nil
             (lambda ()
               (setq timed-out t)
               (harmless-tools-shell-kill proc))))
      proc))))

(defun harmless-tools-shell-register ()
  "Register the run_shell tool."
  (harmless-register-tool
   (harmless-tool-create
    :name "run_shell"
    :description "Run a shell command in the project root. Output is truncated for the model."
    :class 'shell
    :schema '(:type "object"
              :properties (:command (:type "string" :description "Shell command"))
              :required ["command"])
    :fn #'harmless-tools-shell--run)))

(harmless-tools-shell-register)

(provide 'harmless-tools-shell)

;;; harmless-tools-shell.el ends here
