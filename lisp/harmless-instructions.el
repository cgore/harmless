;;; harmless-instructions.el --- Project instructions for Harmless -*- lexical-binding: t; -*-

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
;; Project instructions live in `CLAUDE.md', `AGENTS.md', `HARMLESS.md',
;; and `.harmless/HARMLESS.md'.  `harmless-claude-md' decides when
;; `CLAUDE.md' is read.  Each file applies to that directory and
;; everything under it.  Files are read from the home directory down to
;; the session directory.  A later file wins when two of them disagree.
;; An @path import is expanded relative to the file that contains it.
;; The imported file must stay inside the session directory.

;;; Code:

(require 'subr-x)
(require 'harmless-util)

(defcustom harmless-claude-md 'anthropic
  "When to read `CLAUDE.md' as project instructions.
nil never reads it.  t always reads it.  `anthropic' reads it only
when the session model id starts with \"claude-\"."
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Always" t)
                 (const :tag "Anthropic models only" anthropic))
  :group 'harmless)

(defvar harmless-current-model)

(defun harmless-instruction-model (model)
  "Return MODEL, or `harmless-current-model' when MODEL is nil."
  (or model
      (and (boundp 'harmless-current-model)
           harmless-current-model)))

(defun harmless-claude-md-p (model)
  "Return non-nil if `CLAUDE.md' should be read for MODEL.
nil and any value other than t or `anthropic' mean no.  `anthropic'
is true only for a model id that starts with \"claude-\"."
  (cond
   ((eq harmless-claude-md t) t)
   ((eq harmless-claude-md 'anthropic)
    (let ((name (harmless-instruction-model model)))
      (and (stringp name)
           (not (string-empty-p name))
           (string-prefix-p "claude-" name))))
   (t nil)))

(defun harmless-message-system-p (msg)
  "Return non-nil if MSG is a system message."
  (or (memq (plist-get msg :role) '(:system system))
      (equal (plist-get msg :role) "system")))

(defun harmless-messages-system-text (messages)
  "Return the combined text of system MESSAGES, or nil."
  (let ((parts (delq nil
                     (mapcar (lambda (msg)
                               (when (harmless-message-system-p msg)
                                 (let ((text (string-trim
                                              (or (plist-get msg :content) ""))))
                                   (unless (string-empty-p text)
                                     text))))
                             messages))))
    (and parts (mapconcat #'identity parts "\n\n"))))

(defun harmless-instruction-directories (dir &optional stop)
  "Return directories from the outermost ancestor of DIR through DIR.
The walk includes STOP, which defaults to the user's home directory,
and does not continue above it.  It also stops at the filesystem root."
  (let ((dir (directory-file-name (expand-file-name (or dir default-directory))))
        (stop (directory-file-name (expand-file-name (or stop "~/"))))
        (root (directory-file-name (expand-file-name "/")))
        (acc nil))
    (while dir
      (push dir acc)
      (if (or (string= dir stop) (string= dir root))
          (setq dir nil)
        (let ((parent (file-name-directory dir)))
          (setq dir (and parent
                         (directory-file-name parent)
                         (unless (string= (directory-file-name parent) dir)
                           (directory-file-name parent)))))))
    acc))

(defun harmless-instruction-candidates (dir &optional model)
  "Return instruction paths for DIR, in the order they should be read.
`CLAUDE.md' comes first when `harmless-claude-md-p' is non-nil for
MODEL.  Then `AGENTS.md', `HARMLESS.md', and `.harmless/HARMLESS.md'."
  (let ((paths (list (expand-file-name "AGENTS.md" dir)
                     (expand-file-name "HARMLESS.md" dir)
                     (expand-file-name "HARMLESS.md"
                                       (expand-file-name ".harmless" dir)))))
    (if (harmless-claude-md-p model)
        (cons (expand-file-name "CLAUDE.md" dir) paths)
      paths)))

(defun harmless-instruction-files (dir &optional stop model)
  "Return instruction files that apply to DIR, outermost first.
At each directory, `CLAUDE.md' comes first when it applies to MODEL,
then `AGENTS.md', then `HARMLESS.md', then `.harmless/HARMLESS.md'.
STOP is passed to `harmless-instruction-directories'."
  (let (files)
    (dolist (ancestor (harmless-instruction-directories dir stop))
      (dolist (path (harmless-instruction-candidates ancestor model))
        (when (file-readable-p path)
          (push path files))))
    (nreverse files)))

(defun harmless-instruction--read-raw (path)
  "Return the trimmed contents of PATH."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (string-trim (buffer-string))))

(defun harmless-instruction--inside-p (project path)
  "Return non-nil if PATH is inside PROJECT after resolving `..'."
  (let* ((root (file-name-as-directory
                (file-truename (directory-file-name project))))
         (full (file-truename path)))
    (or (string-prefix-p root full)
        (string-prefix-p root (file-name-as-directory full))
        (string= (directory-file-name root) full))))

(defun harmless-instruction-resolve-import (spelled file project)
  "Return the absolute path SPELLED names, or signal.
FILE is the instruction file that wrote the import.  PROJECT is the
session directory.  SPELL is relative to FILE, or absolute.  An empty
SPELL, a directory, a missing file, the filesystem root, and a path
outside PROJECT are refused."
  (when (or (null spelled)
            (not (stringp spelled))
            (string-blank-p spelled))
    (error "Instruction import is empty"))
  (when (or (null project)
            (not (stringp project))
            (string-empty-p project)
            (harmless-filesystem-root-p project))
    (error "Instruction import escapes the project: %s" spelled))
  (let* ((base (or (and (stringp file) (file-name-directory file))
                   default-directory))
         (full (expand-file-name spelled base)))
    (when (or (harmless-filesystem-root-p full)
              (not (harmless-instruction--inside-p project full)))
      (error "Instruction import escapes the project: %s" spelled))
    (cond
     ((file-directory-p full)
      (error "Instruction import is a directory: %s" spelled))
     ((not (file-regular-p full))
      (error "Instruction import is missing: %s" spelled))
     (t full))))

(defun harmless-instruction--line-end (text start)
  "Return the index of the line ending in TEXT at or after START."
  (let ((n (length text))
        (i start))
    (while (and (< i n) (not (eq (aref text i) ?\n)))
      (setq i (1+ i)))
    i))

(defun harmless-instruction--fence-marker (text start end)
  "Return ?` or ?~ when the line in TEXT from START to END is a fence."
  (let ((line (substring text start end)))
    (cond
     ((string-match-p "\\`[ \t]*```" line) ?`)
     ((string-match-p "\\`[ \t]*~~~" line) ?~)
     (t nil))))

(defun harmless-instruction--backtick-run (text start end)
  "Return the number of backticks in TEXT from START before END."
  (let ((i start))
    (while (and (< i end) (eq (aref text i) ?`))
      (setq i (1+ i)))
    (- i start)))

(defun harmless-instruction--code-span-end (text start end n)
  "Return the index of the last backtick that closes a span, or nil.
START is just after an opening run of N backticks.  The search stops
at END."
  (let ((i start)
        (found nil))
    (while (and (< i end) (not found))
      (if (eq (aref text i) ?`)
          (let ((run (harmless-instruction--backtick-run text i end)))
            (if (= run n)
                (setq found (+ i run -1))
              (setq i (+ i run))))
        (setq i (1+ i))))
    found))

(defun harmless-instruction--import-boundary-p (text index)
  "Return non-nil if INDEX in TEXT can start an @path import."
  (or (zerop index)
      (memq (aref text (1- index)) '(?\s ?\t ?\n ?\r))))

(defun harmless-instruction--copy-line (text start)
  "Insert the line of TEXT that begins at START, including its newline."
  (let ((end (harmless-instruction--line-end text start)))
    (insert (substring text start (min (length text) (1+ end))))
    (min (length text) (1+ end))))

(defun harmless-instruction--insert-import (text start end file project stack)
  "Insert the import at START in TEXT and return the index after it.
END is the end of the current line.  FILE, PROJECT, and STACK are
passed to the expander.  A quoted path is not an import."
  (let ((next (1+ start)))
    (if (and (< next end) (memq (aref text next) '(?\" ?')))
        (progn
          (insert ?@)
          next)
      (let ((raw "")
            (i next)
            (stopped nil))
        (while (and (< i end) (not stopped))
          (let ((char (aref text i)))
            (cond
             ((and (eq char ?\\)
                   (< (1+ i) end)
                   (eq (aref text (1+ i)) ?\s))
              (setq raw (concat raw " ")
                    i (+ i 2)))
             ((memq char '(?\s ?\t))
              (setq stopped t))
             (t
              (setq raw (concat raw (char-to-string char))
                    i (1+ i))))))
        (when (string-blank-p raw)
          (error "Instruction import is empty"))
        (let* ((full (harmless-instruction-resolve-import raw file project))
               (true (file-truename full)))
          (when (member true stack)
            (error "Instruction import is a cycle: %s" raw))
          (insert (harmless-instruction--expand
                   (harmless-instruction--read-raw true)
                   true project (cons true stack))))
        i))))

(defun harmless-instruction--expand-line (text start end file project stack)
  "Expand imports on one line of TEXT and return the next index.
START and END bound the line, excluding its newline.  FILE, PROJECT,
and STACK are passed through to each import."
  (let ((i start))
    (while (< i end)
      (cond
       ((eq (aref text i) ?`)
        (let* ((run (harmless-instruction--backtick-run text i end))
               (close (harmless-instruction--code-span-end
                       text (+ i run) end run)))
          (if close
              (progn
                (insert (substring text i (1+ close)))
                (setq i (1+ close)))
            (insert (substring text i end))
            (setq i end))))
       ((and (eq (aref text i) ?@)
             (harmless-instruction--import-boundary-p text i))
        (setq i (harmless-instruction--insert-import
                 text i end file project stack)))
       (t
        (insert (aref text i))
        (setq i (1+ i)))))
    (when (and (< end (length text)) (eq (aref text end) ?\n))
      (insert ?\n)
      (setq end (1+ end)))
    end))

(defun harmless-instruction--expand (text file project stack)
  "Return TEXT with @path imports expanded.
FILE is the file that contains TEXT.  PROJECT is the session
directory.  STACK lists truenames already being expanded.  An import
is relative to FILE.  Inline code and fenced code blocks are left
unchanged."
  (with-temp-buffer
    (let ((i 0)
          (n (length text))
          (fence nil))
      (while (< i n)
        (let* ((end (harmless-instruction--line-end text i))
               (marker (harmless-instruction--fence-marker text i end)))
          (cond
           ((and (not fence) marker)
            (setq fence marker)
            (setq i (harmless-instruction--copy-line text i)))
           ((and fence (eq marker fence))
            (setq fence nil)
            (setq i (harmless-instruction--copy-line text i)))
           (fence
            (setq i (harmless-instruction--copy-line text i)))
           (t
            (setq i (harmless-instruction--expand-line
                     text i end file project stack)))))))
    (buffer-string)))

(defun harmless-instruction--read (path project)
  "Return PATH with @path imports expanded for the session at PROJECT."
  (let ((body (harmless-instruction--read-raw path)))
    (if (string-empty-p body)
        body
      (harmless-instruction--expand
       body path project (list (file-truename path))))))

(defun harmless-instructions-text (dir &optional stop model)
  "Return the project-instruction prompt for DIR, or nil if none exist.
STOP and MODEL are passed to `harmless-instruction-files'.  @path
imports are expanded inside the session directory."
  (let* ((project (directory-file-name
                   (expand-file-name (or dir default-directory))))
         (chunks
          (delq nil
                (mapcar (lambda (path)
                          (let ((body (harmless-instruction--read path project)))
                            (unless (string-empty-p body)
                              (format "## %s\n%s" path body))))
                        (harmless-instruction-files dir stop model)))))
    (when chunks
      (concat
       "Project instructions for this session.  Files are listed from the outermost directory to the innermost.  When they disagree, prefer the later file.\n\n"
       (mapconcat #'identity chunks "\n\n")))))

(defun harmless-context-append (messages block)
  "Return MESSAGES with BLOCK added to the leading system message.
MESSAGES is unchanged when BLOCK is empty."
  (if (or (null block) (string-empty-p block))
      messages
    (if (and messages (harmless-message-system-p (car messages)))
        (cons (list :role :system
                    :content (concat (plist-get (car messages) :content)
                                     "\n\n"
                                     block))
              (cdr messages))
      (cons (list :role :system :content block) messages))))

(defun harmless-instructions-apply (dir messages &optional stop model)
  "Return MESSAGES with project instructions for DIR prepended.
MESSAGES is unchanged when DIR has no instruction files.  STOP and
MODEL are passed to `harmless-instructions-text'."
  (if-let* ((text (harmless-instructions-text dir stop model)))
      (cons (list :role :system :content text) messages)
    messages))

(provide 'harmless-instructions)

;;; harmless-instructions.el ends here
