;;; harmless-harness.el --- Standing prompt for every Harmless turn -*- lexical-binding: t; -*-

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
;; Every turn starts with this policy: which tool to use, when to
;; verify, and when to delegate.  Project instructions, path-scoped
;; rules, and plan mode are added after it.  When they disagree with
;; this policy, they win.  The text is not stored in the transcript
;; and it does not read the filesystem.

;;; Code:

(require 'subr-x)
(require 'harmless-instructions)

(defconst harmless-harness-prompt
  (concat
   "Harmless harness policy.\n\n"
   "Tool policy. When this chat is attached to a project, inspect "
   "the project with a tool before you edit it. Do not invent file "
   "contents, command output, or test results. read_file, list_dir, "
   "glob, and grep read the project. read_file also reads an image, "
   "a PDF, or a notebook. write_file writes a whole file. replace "
   "changes one unique string. apply_patch applies several hunks and "
   "writes nothing if any hunk fails. notebook_edit replaces one "
   "notebook cell and does not create the notebook. lsp asks a "
   "language server that is already running. It does not start a "
   "server and it does not edit the file. review reports findings "
   "and does not apply them. run_shell runs a command. A command "
   "that can write may ask the user. Report a failed command as the "
   "tool returned it. ask_user only when the user must choose. Do "
   "not ask for a fact a tool can read. todo_write tracks work in "
   "progress. memory_remember records a durable fact. Do not store "
   "secrets, credentials, or one-off task state.\n\n"
   "When to verify. After an edit, read the changed part or run the "
   "project's test when the project has one. Claim a test passed "
   "only when a tool result says so. When a tool returns an error, "
   "report that error and fix the cause.\n\n"
   "When to delegate. Make a small change yourself. spawn_agent when "
   "a subtask needs its own transcript. Give it a self-contained "
   "prompt. A subagent cannot start another subagent. A read-only "
   "subagent cannot write. run_workflow when a named workflow, or a "
   "script written for this task, should start the child sessions "
   "and return one result.\n\n"
   "Project instructions, path-scoped rules, and plan mode follow "
   "this policy. When they disagree with it, follow them.")
  "Standing system text sent on every turn.
It states tool policy, when to verify, and when to delegate.
Later project text wins when it disagrees.")

(defun harmless-harness-prepend (messages)
  "Return MESSAGES with `harmless-harness-prompt' first.
MESSAGES is not modified.  Nil is an empty transcript.  A value
that is not a list is not walked.  This does not read the filesystem."
  (let ((block harmless-harness-prompt))
    (cond
     ((or (null block) (not (stringp block)) (string-empty-p block))
      (if (listp messages) messages nil))
     ((and (consp messages)
           (harmless-message-system-p (car messages)))
      (let ((msg (copy-sequence (car messages))))
        (plist-put msg :content
                   (concat block
                           "\n\n"
                           (or (and (stringp (plist-get msg :content))
                                    (plist-get msg :content))
                               "")))
        (cons msg (cdr messages))))
     ((listp messages)
      (cons (list :role :system :content block) messages))
     (t (list (list :role :system :content block))))))

(provide 'harmless-harness)

;;; harmless-harness.el ends here
