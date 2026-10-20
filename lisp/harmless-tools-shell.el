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
;; takes a callback so the turn loop can keep Emacs responsive.  A command
;; that is still running at its timeout stays running in the background.
;; shell_output reads that job.  shell_kill stops it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-tools)
(require 'harmless-tools-fs)

(defcustom harmless-shell-output-max-bytes 20000
  "Maximum bytes of shell output returned to the model.
The UI can still show the full buffer contents."
  :type 'integer
  :group 'harmless)

(defcustom harmless-shell-timeout-seconds 120
  "Seconds to wait before a shell command moves to the background.
The process is not killed.  The model can pass a larger `timeout'."
  :type 'number
  :group 'harmless)

(cl-defstruct (harmless-shell-job
               (:constructor harmless-shell-job--create)
               (:copier nil))
  id
  command
  proc
  buffer
  output
  status
  exit
  notified
  waiters
  timer
  delivered
  background)

(defconst harmless-shell-cwd-marker "__HARMLESS_CWD__"
  "Line prefix a shell prints so Harmless can see its directory.")

(defun harmless-shell-kind (&optional file)
  "Return the shell family for FILE, or `shell-file-name'."
  (let ((name (downcase (file-name-nondirectory
                         (or file shell-file-name "")))))
    (cond
     ((string-match-p "pwsh\\|powershell" name) 'powershell)
     ((string-match-p "cmd" name) 'cmd)
     (t 'posix))))

(defun harmless-shell-directory (session)
  "Return the directory SESSION's next command starts in.
A remembered directory is used only when it is an existing directory
inside the project.  Nil, \"\", a relative path, the filesystem root,
and a missing directory all mean the project root."
  (let ((root (harmless-session-require-project session))
        (cwd (harmless-session-shell-cwd session)))
    (if (and (stringp cwd)
             (not (string-empty-p cwd))
             (file-name-absolute-p cwd)
             (not (harmless-filesystem-root-p cwd))
             (file-directory-p cwd)
             (harmless-tools-fs-inside-p root cwd))
        (directory-file-name (file-truename cwd))
      (directory-file-name (file-truename root)))))

(defun harmless-shell--wrap (kind directory command)
  "Return COMMAND as run by KIND, starting in DIRECTORY.
The shell prints `harmless-shell-cwd-marker' and its directory last."
  (let ((dir (shell-quote-argument directory))
        (marker harmless-shell-cwd-marker))
    (pcase kind
      ('cmd
       (format "cd /d %s && (%s) & set __EC=!ERRORLEVEL! & echo %s!CD! & exit /b !__EC!"
               dir command marker))
      ('powershell
       (format "Set-Location -LiteralPath %s; %s; if ($?) { $__ec = 0 } else { $__ec = 1 }; Write-Output ('%s' + (Get-Location).Path); exit $__ec"
               dir command marker))
      (_
       (format "cd -- %s && { %s\n}; __harmless_ec=$?; printf '\\n%s%%s\\n' \"$PWD\"; exit $__harmless_ec"
               dir command marker)))))

(defun harmless-shell--argv (kind command)
  "Return the process argv that runs COMMAND for shell KIND."
  (if (eq kind 'cmd)
      (list shell-file-name "/v:on" "/c" command)
    (list shell-file-name (or shell-command-switch "-c") command)))

(defconst harmless-shell-plan-refusal
  "Error: Plan mode is on. A shell command that can write is refused"
  "Result when plan mode refuses a shell command that can write.")

(defcustom harmless-dangerous-commands
  '("diskpart" "halt" "mkfs" "poweroff" "reboot" "shutdown")
  "Program names that are refused before a permission question.
The match is the lower-case file name, without a directory and
without a trailing `.exe'.  A name that starts with `mkfs.' is
refused too.  `sudo' and `doas' are unwrapped once.  `rm' of the
filesystem root is refused even though `rm' is not on this list."
  :type '(repeat string)
  :group 'harmless)

(defconst harmless-shell-root-commands
  '("del" "erase" "rd" "rm" "rmdir")
  "Programs refused when an argument is the filesystem root.")

(defun harmless-shell--base (word)
  "Return the lower-case file name of WORD, without `.exe'."
  (let ((name (downcase (file-name-nondirectory word))))
    (if (and (string-suffix-p ".exe" name) (> (length name) 4))
        (substring name 0 -4)
      name)))

(defun harmless-shell--command-words (words)
  "Return WORDS after assignments and one sudo or doas wrapper."
  (setq words (harmless-shell--drop-assignments words))
  (when (and words
             (member (harmless-shell--base (car words))
                     '("sudo" "doas")))
    (setq words (cdr words))
    (while (and words (string-prefix-p "-" (car words)))
      (setq words (cdr words)))
    (setq words (harmless-shell--drop-assignments words)))
  words)

(defun harmless-shell--dangerous-base-p (name)
  "Return non-nil if NAME is on the dangerous-command denylist."
  (or (member name harmless-dangerous-commands)
      (and (string-prefix-p "mkfs." name)
           (> (length name) (length "mkfs.")))))

(defun harmless-shell--root-target-p (word)
  "Return non-nil if WORD names the filesystem root or a drive root.
`~', `~/', and `/*' count.  A longer path does not."
  (and (stringp word)
       (not (string-empty-p word))
       (or (member word '("~" "~/" "~\\" "/*"))
           (harmless-filesystem-root-p word)
           (string-match-p "\\`[A-Za-z]:[/\\]?\\'" word))))

(defun harmless-shell--dangerous-in-words (words)
  "Return the denied program in WORDS, or nil."
  (let ((words (harmless-shell--command-words words)))
    (when words
      (let ((name (harmless-shell--base (car words))))
        (cond
         ((harmless-shell--dangerous-base-p name) name)
         ((and (member name harmless-shell-root-commands)
               (cl-some #'harmless-shell--root-target-p (cdr words)))
          name))))))

(defun harmless-shell-dangerous-name (command)
  "Return the denied program in COMMAND, or nil.
Nil, \"\", and a blank command are not denied.  A pipeline is
denied when any simple command is denied."
  (when (and (stringp command)
             (string-match-p "[^ \t\n\r]" command))
    (let ((segments (harmless-shell--segments command))
          (found nil))
      (if segments
          (dolist (words segments)
            (unless found
              (setq found
                    (harmless-shell--dangerous-in-words words))))
        (setq found
              (harmless-shell--dangerous-in-words
               (split-string (string-trim command)
                             "[ \t\n\r]+" t))))
      found)))

(defun harmless-shell-subject-command (tool args)
  "Return the shell command TOOL would run, or nil.
A file monitor and a shell monitor are not commands."
  (let ((name (harmless-tool-name tool)))
    (cond
     ((string= name "run_shell")
      (let ((command (harmless-tool-arg args :command)))
        (and (stringp command) command)))
     ((string= name "monitor")
      (when (and (null (harmless-tool-arg args :path))
                 (null (harmless-tool-arg args :shell))
                 (stringp (harmless-tool-arg args :command)))
        (harmless-tool-arg args :command))))))

(defun harmless-shell-dangerous-refusal (tool args)
  "Return a refusal when TOOL runs a dangerous command.
The string includes \"Error: \".  Nil means the call may ask."
  (let ((name (harmless-shell-dangerous-name
               (harmless-shell-subject-command tool args))))
    (when name
      (format "Error: dangerous command refused: %s" name))))

(defconst harmless-shell-read-commands
  '("[" "ag" "basename" "cat" "cd" "chdir" "cmp" "column" "comm" "cut"
    "date" "df" "diff" "dir" "dirname" "du" "echo" "egrep" "env" "false"
    "fc" "fgrep" "file" "find" "findstr" "fold" "git" "grep" "head"
    "hexdump" "id" "less" "ls" "more" "nl" "od" "printenv" "printf" "pwd"
    "readlink" "realpath" "rev" "rg" "sed" "sort" "stat" "strings" "tac"
    "tail" "test" "tr" "true" "type" "uname" "uniq" "wc" "where"
    "whereis" "which" "whoami" "xxd")
  "Command names that read unless a later check says they write.
The lookup is the downcased file name, with no directory.")

(defconst harmless-shell-git-read-subs
  '("blame" "cat-file" "check-attr" "check-ignore" "describe" "diff"
    "diff-files" "diff-index" "diff-tree" "for-each-ref" "grep" "help"
    "log" "ls-files" "ls-remote" "ls-tree" "merge-base" "name-rev"
    "rev-list" "rev-parse" "shortlog" "show" "show-ref" "status" "var"
    "version")
  "git subcommands that only read.")

(defconst harmless-shell-find-write-words
  '("-delete" "-exec" "-execdir" "-fls" "-fprint" "-fprint0" "-fprintf"
    "-ok" "-okdir")
  "find arguments that write or run another command.")

(defun harmless-shell-command-kind (command)
  "Return `read' when COMMAND cannot write a file, otherwise `write'.
Nil, \"\", a blank command, a relative program, and a program Harmless
does not recognize are `write'.  A redirect, a command substitution,
and a subshell are `write' too."
  (if (not (and (stringp command)
                (string-match-p "[^ \t\n\r]" command)))
      'write
    (let ((segments (harmless-shell--segments command)))
      (if (and segments
               (cl-every #'harmless-shell--segment-read-p segments))
          'read
        'write))))

(defun harmless-shell-plan-refuses-p (session command)
  "Return non-nil when plan mode must refuse COMMAND on SESSION.
Nil, \"\", and a blank command are not this refusal.  The caller
refuses before asking for permission."
  (and (harmless-session-plan-mode session)
       (stringp command)
       (string-match-p "[^ \t\n\r]" command)
       (eq (harmless-shell-command-kind command) 'write)))

(defun harmless-shell-run-class (_session args)
  "Return `read' or `shell' for a run_shell call in ARGS."
  (if (eq 'read (harmless-shell-command-kind
                 (harmless-tool-arg args :command)))
      'read
    'shell))

(defun harmless-shell--digits-p (chars)
  "Return non-nil when every character in CHARS is a digit.
Nil is all digits, so a bare `>&1' can be a file-descriptor copy."
  (cl-every (lambda (ch) (and (<= ?0 ch) (<= ch ?9))) chars))

(defun harmless-shell--segments (command)
  "Return simple-command word lists in COMMAND.
Return nil when COMMAND might write: an unquoted redirect, a command
substitution, a subshell, or a quote that does not close.  A copy
such as `2>&1' is not a redirect to a file."
  (let ((segments nil)
        (words nil)
        (word nil)
        (quoted nil)
        (i 0)
        (n (length command))
        (fail nil))
    (cl-labels
        ((emit-word ()
           (when (or word quoted)
             (push (apply #'string (nreverse word)) words)
             (setq word nil quoted nil)))
         (emit-segment ()
           (emit-word)
           (when words
             (push (nreverse words) segments)
             (setq words nil))))
      (while (and (not fail) (< i n))
        (let ((c (aref command i)))
          (cond
           ((eq c ?')
            (setq quoted t
                  i (1+ i))
            (while (and (< i n) (not (eq (aref command i) ?')))
              (push (aref command i) word)
              (setq i (1+ i)))
            (if (>= i n)
                (setq fail t)
              (setq i (1+ i))))
           ((eq c ?\")
            (setq quoted t
                  i (1+ i))
            (while (and (not fail) (< i n)
                        (not (eq (aref command i) ?\")))
              (let ((d (aref command i)))
                (cond
                 ((and (eq d ?\\) (< (1+ i) n))
                  (push (aref command (1+ i)) word)
                  (setq i (+ i 2)))
                 ((eq d ?`)
                  (setq fail t))
                 ((and (eq d ?$)
                       (< (1+ i) n)
                       (eq (aref command (1+ i)) ?\())
                  (setq fail t))
                 (t
                  (push d word)
                  (setq i (1+ i))))))
            (if (or fail (>= i n))
                (setq fail t)
              (setq i (1+ i))))
           ((or (eq c ?`) (eq c ?\() (eq c ?\)))
            (setq fail t))
           ((and (eq c ?$)
                 (< (1+ i) n)
                 (eq (aref command (1+ i)) ?\())
            (setq fail t))
           ((eq c ?>)
            (if (and (harmless-shell--digits-p word)
                     (< (1+ i) n)
                     (eq (aref command (1+ i)) ?&)
                     (< (+ i 2) n)
                     (or (eq (aref command (+ i 2)) ?-)
                         (<= ?0 (aref command (+ i 2)) ?9)))
                (progn
                  (setq word nil
                        i (+ i 3))
                  (while (and (< i n) (<= ?0 (aref command i) ?9))
                    (setq i (1+ i))))
              (setq fail t)))
           ((memq c '(?\s ?\t ?\r ?<))
            (emit-word)
            (setq i (1+ i)))
           ((memq c '(?| ?& ?\; ?\n))
            (emit-segment)
            (setq i (1+ i))
            (when (and (< i n) (eq (aref command i) c) (memq c '(?| ?&)))
              (setq i (1+ i))))
           (t
            (push c word)
            (setq i (1+ i))))))
      (unless fail
        (emit-segment)
        (nreverse segments)))))

(defun harmless-shell--drop-assignments (words)
  "Drop leading NAME=value words from WORDS."
  (while (and words
              (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*=" (car words)))
    (setq words (cdr words)))
  words)

(defun harmless-shell--segment-read-p (words)
  "Return non-nil when the simple command WORDS only reads."
  (let ((words (harmless-shell--drop-assignments words)))
    (or (null words)
        (let ((argv0 (car words)))
          (and (not (string-match-p "[\\\\/]" argv0))
               (not (string-empty-p argv0))
               (harmless-shell--argv-read-p
                (downcase argv0) (cdr words)))))))

(defun harmless-shell--argv-read-p (name args)
  "Return non-nil when NAME with ARGS is a known read-only command.
NAME is already downcased and has no directory."
  (and (member name harmless-shell-read-commands)
       (pcase name
         ("git" (harmless-shell--git-read-p args))
         ("find" (harmless-shell--find-read-p args))
         ("sed" (harmless-shell--sed-read-p args))
         ("sort" (harmless-shell--sort-read-p args))
         ("rg" (harmless-shell--rg-read-p args))
         (_ t))))

(defun harmless-shell--git-skip-globals (args)
  "Drop read-only git global options from ARGS.
Return nil when an option is missing its argument or is unknown."
  (let (fail)
    (while (and args (not fail) (string-prefix-p "-" (car args)))
      (let ((opt (car args)))
        (cond
         ((member opt '("-C" "-c" "--git-dir" "--work-tree"
                        "--namespace" "--exec-path"))
          (if (cdr args)
              (setq args (cddr args))
            (setq fail t)))
         ((or (and (string-prefix-p "-C" opt) (> (length opt) 2))
              (and (string-prefix-p "-c" opt) (> (length opt) 2))
              (string-prefix-p "--git-dir=" opt)
              (string-prefix-p "--work-tree=" opt)
              (string-prefix-p "--namespace=" opt)
              (string-prefix-p "--exec-path=" opt))
          (setq args (cdr args)))
         ((member opt '("--no-pager" "--paginate" "-p" "--bare"
                        "--literal-pathspecs" "--glob-pathspecs"
                        "--noglob-pathspecs" "--icase-pathspecs"
                        "--no-optional-locks" "--no-replace-objects"))
          (setq args (cdr args)))
         (t (setq fail t)))))
    (if fail nil args)))

(defun harmless-shell--git-output-p (args)
  "Return non-nil when git ARGS name an `--output' file."
  (cl-some (lambda (arg)
             (let ((down (downcase arg)))
               (or (string= down "--output")
                   (string-prefix-p "--output=" down))))
           args))

(defun harmless-shell--git-branch-read-p (args)
  "Return non-nil when git branch ARGS only list branches."
  (and (not (cl-some #'harmless-shell--git-branch-writes-p args))
       (or (null args)
           (member "--list" args)
           (member "-l" args)
           (cl-every (lambda (arg) (string-prefix-p "-" arg)) args))))

(defun harmless-shell--git-branch-writes-p (arg)
  "Return non-nil when git branch ARG creates, moves, or deletes."
  (let ((down (downcase arg)))
    (or (member down '("-d" "-m" "-c" "--delete" "--move" "--copy"
                       "--edit-description" "-u" "--unset-upstream"
                       "--set-upstream"))
        (string-prefix-p "--set-upstream-to" down)
        (string-prefix-p "--edit-description" down))))

(defun harmless-shell--git-tag-read-p (args)
  "Return non-nil when git tag ARGS only list tags."
  (and (not (cl-some #'harmless-shell--git-tag-writes-p args))
       (or (null args)
           (member "--list" args)
           (member "-l" args)
           (cl-every (lambda (arg) (string-prefix-p "-" arg)) args))))

(defun harmless-shell--git-tag-writes-p (arg)
  "Return non-nil when git tag ARG creates or deletes a tag."
  (let ((down (downcase arg)))
    (or (member down '("-d" "-a" "-s" "-f" "-u" "--delete" "--annotate"
                       "--sign" "--force" "--local-user" "--cleanup"))
        (string-prefix-p "-m" down)
        (string-prefix-p "--file" down)
        (string-prefix-p "-f" down))))

(defun harmless-shell--git-remote-read-p (args)
  "Return non-nil when git remote ARGS only read."
  (cond
   ((null args) t)
   ((string-prefix-p "-" (car args))
    (cl-every (lambda (arg) (member arg '("-v" "--verbose"))) args))
   ((member (downcase (car args)) '("show" "get-url"))
    (not (cl-some (lambda (arg) (member arg '("--prune" "-p")))
                  (cdr args))))
   (t nil)))

(defun harmless-shell--git-config-read-p (args)
  "Return non-nil when git config ARGS only read a value."
  (let ((positionals 0)
        (write nil))
    (while (and args (not write))
      (let ((arg (car args))
            (down (downcase (car args))))
        (cond
         ((member down '("--default" "--file"))
          (if (cdr args)
              (setq args (cddr args))
            (setq write t)))
         ((member down '("--get" "--get-all" "--get-regexp" "--list" "-l"
                         "--show-origin" "--show-scope" "--null" "-z"
                         "--local" "--global" "--system" "--worktree"
                         "--name-only" "--includes" "--no-includes"))
          (setq args (cdr args)))
         ((or (member down '("--unset" "--unset-all" "--add" "--replace-all"
                             "--rename-section" "--remove-section"
                             "--edit" "-e"))
              (string-prefix-p "--unset" down)
              (string-prefix-p "--replace" down))
          (setq write t))
         ((string-prefix-p "-" arg)
          (setq write t))
         (t
          (setq positionals (1+ positionals)
                args (cdr args))))))
    (and (not write) (<= positionals 1))))

(defun harmless-shell--git-read-p (args)
  "Return non-nil when git ARGS only read."
  (let ((args (harmless-shell--git-skip-globals args)))
    (when args
      (let ((sub (downcase (car args)))
            (rest (cdr args)))
        (and (not (harmless-shell--git-output-p rest))
             (cond
              ((member sub harmless-shell-git-read-subs) t)
              ((string= sub "branch")
               (harmless-shell--git-branch-read-p rest))
              ((string= sub "tag")
               (harmless-shell--git-tag-read-p rest))
              ((string= sub "remote")
               (harmless-shell--git-remote-read-p rest))
              ((string= sub "config")
               (harmless-shell--git-config-read-p rest))
              ((string= sub "stash")
               (and rest
                    (member (downcase (car rest)) '("list" "show"))))
              ((string= sub "reflog")
               (and (not (cl-some
                          (lambda (arg)
                            (member (downcase arg) '("delete" "expire")))
                          rest))
                    (or (null rest)
                        (member (downcase (car rest)) '("show" "exists"))
                        (string-prefix-p "-" (car rest)))))
              (t nil)))))))

(defun harmless-shell--find-read-p (args)
  "Return non-nil when find ARGS do not write or run a command."
  (not (cl-some (lambda (arg)
                  (member (downcase arg) harmless-shell-find-write-words))
                args)))

(defun harmless-shell--sort-read-p (args)
  "Return non-nil when sort ARGS do not name an output file."
  (not (cl-some (lambda (arg)
                  (let ((down (downcase arg)))
                    (or (string= down "-o")
                        (string= down "--output")
                        (string-prefix-p "--output=" down)
                        (and (string-prefix-p "-o" down)
                             (> (length down) 2)))))
                args)))

(defun harmless-shell--rg-read-p (args)
  "Return non-nil when rg ARGS do not run a preprocessor."
  (not (cl-some (lambda (arg)
                  (let ((down (downcase arg)))
                    (or (string= down "--pre")
                        (string= down "--pre-glob")
                        (string-prefix-p "--pre=" down)
                        (string-prefix-p "--pre-glob=" down))))
                args)))

(defun harmless-shell--scan-to (script j delim)
  "Scan SCRIPT from J until DELIM.  Return the index after it, or nil."
  (let ((n (length script)))
    (while (and (< j n) (not (eq (aref script j) delim)))
      (setq j (if (and (eq (aref script j) ?\\) (< (1+ j) n))
                  (+ j 2)
                (1+ j))))
    (and (< j n) (1+ j))))

(defun harmless-shell--sed-skip-s (script i)
  "Return the index after the sed s command at I, or the symbol `write'."
  (let ((n (length script)))
    (if (>= (+ i 2) n)
        n
      (let ((delim (aref script (1+ i))))
        (if (memq delim '(?\s ?\t ?\n ?\;))
            (1+ i)
          (let ((j (harmless-shell--scan-to script (+ i 2) delim)))
            (setq j (and j (harmless-shell--scan-to script j delim)))
            (if (null j)
                n
              (let (write)
                (while (and (< j n)
                            (not (memq (aref script j)
                                       '(?\s ?\t ?\n ?\;))))
                  (when (memq (aref script j) '(?w ?W))
                    (setq write t))
                  (setq j (1+ j)))
                (if write 'write j)))))))))

(defun harmless-shell--sed-skip-y (script i)
  "Return the index after the sed y command at I."
  (let ((n (length script)))
    (if (>= (+ i 2) n)
        n
      (let ((delim (aref script (1+ i))))
        (if (memq delim '(?\s ?\t ?\n ?\;))
            (1+ i)
          (let ((j (harmless-shell--scan-to script (+ i 2) delim)))
            (or (and j (harmless-shell--scan-to script j delim)) n)))))))

(defun harmless-shell--sed-script-writes-p (script)
  "Return non-nil if sed SCRIPT writes a file."
  (let ((i 0)
        (n (length script))
        (write nil))
    (while (and (not write) (< i n))
      (let ((c (aref script i)))
        (cond
         ((memq c '(?\s ?\t ?\n ?\; ?\{ ?\}))
          (setq i (1+ i)))
         ((memq c '(?w ?W))
          (setq write t))
         ((eq c ?/)
          (setq i (or (harmless-shell--scan-to script (1+ i) ?/) n)))
         ((eq c ?s)
          (let ((next (harmless-shell--sed-skip-s script i)))
            (if (eq next 'write)
                (setq write t)
              (setq i next))))
         ((eq c ?y)
          (setq i (harmless-shell--sed-skip-y script i)))
         (t
          (setq i (1+ i))))))
    write))

(defun harmless-shell--sed-inplace-p (arg)
  "Return non-nil when sed ARG edits files in place."
  (or (string-prefix-p "-i" arg)
      (string-prefix-p "--in-place" arg)))

(defun harmless-shell--sed-read-p (args)
  "Return non-nil when sed ARGS do not write a file."
  (let ((scripts nil)
        (write nil)
        (seen nil)
        (case-fold-search nil))
    (save-match-data
    (while (and args (not write))
      (let ((arg (car args)))
        (cond
         ((or (string= arg "-e") (string= arg "--expression"))
          (if (cdr args)
              (progn
                (push (cadr args) scripts)
                (setq seen t
                      args (cddr args)))
            (setq write t)))
         ((string-prefix-p "--expression=" arg)
          (push (substring arg (length "--expression=")) scripts)
          (setq seen t
                args (cdr args)))
         ((or (string= arg "-f")
              (string= arg "--file")
              (string-prefix-p "--file=" arg)
              (harmless-shell--sed-inplace-p arg))
          (setq write t))
         ((string-prefix-p "-" arg)
          (cond
           ((string-match "\\`-e\\(.+\\)\\'" arg)
            (push (match-string 1 arg) scripts)
            (setq seen t))
           ((string-match "\\`-[nErRuUl]+e\\(.+\\)\\'" arg)
            (push (match-string 1 arg) scripts)
            (setq seen t)))
          (setq args (cdr args)))
         ((not seen)
          (push arg scripts)
          (setq seen t
                args (cdr args)))
         (t
          (setq args (cdr args))))))
    (and (not write)
         (cl-notany #'harmless-shell--sed-script-writes-p scripts)))))

(defun harmless-shell--split-cwd (text)
  "Return (OUTPUT . DIR) with the last cwd marker removed from TEXT.
DIR is nil when the marker is absent."
  (let ((case-fold-search nil)
        (start 0)
        (last nil)
        (re (concat "^" (regexp-quote harmless-shell-cwd-marker) "\\(.*\\)$")))
    (while (string-match re text start)
      (setq last (list (match-beginning 0)
                       (match-end 0)
                       (string-trim (match-string 1 text)))
            start (match-end 0)))
    (if (null last)
        (cons text nil)
      (cons (concat (substring text 0 (nth 0 last))
                    (substring text (nth 1 last)))
            (nth 2 last)))))

(defun harmless-shell--accept-cwd (session reported)
  "Remember REPORTED when it stays inside SESSION's project.
Return a sentence when REPORTED leaves the project, or nil."
  (let ((root (directory-file-name
               (file-truename (harmless-session-require-project session)))))
    (cond
     ((or (not (stringp reported))
          (string-empty-p reported)
          (not (file-name-absolute-p reported))
          (not (file-directory-p reported)))
      nil)
     ((or (harmless-filesystem-root-p reported)
          (not (harmless-tools-fs-inside-p root reported)))
      (let ((next (harmless-shell-directory session))
            (kept (harmless-session-shell-cwd session)))
        (unless (and (stringp kept)
                     (harmless-same-directory-p kept next))
          (setf (harmless-session-shell-cwd session) nil))
        (format "The shell left the project. The next command starts in %s."
                next)))
     ((harmless-same-directory-p reported root)
      (setf (harmless-session-shell-cwd session) nil)
      nil)
     (t
      (setf (harmless-session-shell-cwd session)
            (directory-file-name (file-truename reported)))
      nil))))

(defun harmless-shell--settle-cwd (session job)
  "Strip JOB's cwd marker and remember a directory inside the project.
Store the text the model should see."
  (let* ((split (harmless-shell--split-cwd (harmless-shell--text job)))
         (note (harmless-shell--accept-cwd session (cdr split)))
         (text (car split)))
    (when (and note (not (string-empty-p note)))
      (setq text (concat text
                         (if (or (string-empty-p text)
                                 (string-suffix-p "\n" text))
                             ""
                           "\n")
                         note "\n")))
    (setf (harmless-shell-job-output job) text)))

(defun harmless-tools-shell-kill (proc)
  "SIGTERM PROC, then SIGKILL shortly after if it is still alive."
  (when (and proc (process-live-p proc))
    (ignore-errors (signal-process proc 'SIGTERM))
    (run-at-time 1.5 nil
                 (lambda (p)
                   (when (process-live-p p)
                     (ignore-errors (delete-process p))))
                 proc)))

(defun harmless-shell--for-model (text)
  "Return TEXT cut at `harmless-shell-output-max-bytes' for the model."
  (setq text (harmless-ensure-utf8 (or text "")))
  (if (> (string-bytes text) harmless-shell-output-max-bytes)
      (concat (substring text 0
                         (min (length text) harmless-shell-output-max-bytes))
              "\n[truncated]")
    text))

(defun harmless-shell--run-timeout (args)
  "Return how long `run_shell' should wait, or an error string.
A missing timeout uses `harmless-shell-timeout-seconds'.  Nil, zero,
a negative number, and a non-number are refused."
  (if (not (harmless-shell--arg-present-p args :timeout))
      harmless-shell-timeout-seconds
    (let ((raw (harmless-tool-arg args :timeout)))
      (if (and (numberp raw) (> raw 0))
          raw
        "Error: timeout must be a positive number"))))

(defun harmless-shell--arg-present-p (args key)
  "Return non-nil if KEY is present in plist ARGS."
  (or (and (listp args) (plist-member args key))
      (let ((name (if (keywordp key) (substring (symbol-name key) 1) key)))
        (and (listp args)
             (cl-loop for k in args by #'cddr
                      thereis (and (stringp k) (string= k name)))))))

(defun harmless-shell--output-timeout (args)
  "Return how long `shell_output' should wait, or an error string.
Nil means return the output already available.  A present timeout that
is not a positive number is refused."
  (if (not (harmless-shell--arg-present-p args :timeout))
      nil
    (let ((raw (harmless-tool-arg args :timeout)))
      (if (and (numberp raw) (> raw 0))
          raw
        "Error: timeout must be a positive number"))))

(defun harmless-shell--id (raw)
  "Return RAW as a shell id string, or nil when it is empty."
  (cond
   ((and (integerp raw) (>= raw 0)) (number-to-string raw))
   ((stringp raw)
    (let ((id (string-trim raw)))
      (and (not (string-empty-p id)) id)))
   (t nil)))

(defun harmless-shell-find (session id)
  "Return SESSION's background job ID, or nil."
  (cl-find id (harmless-session-shell-jobs session)
           :key #'harmless-shell-job-id :test #'equal))

(defun harmless-shell--append-output (buf text)
  "Insert TEXT into BUF, keeping at most `harmless-shell-output-max-bytes'."
  (when (and (buffer-live-p buf) (stringp text) (not (string-empty-p text)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert text)
      (when (> (1- (position-bytes (point-max)))
               harmless-shell-output-max-bytes)
        (let ((note "[earlier output discarded]\n"))
          (goto-char (point-min))
          (when (looking-at (regexp-quote note))
            (delete-region (point) (match-end 0)))
          (let* ((keep (max 0 (- harmless-shell-output-max-bytes
                                 (string-bytes note))))
                 (excess (- (1- (position-bytes (point-max))) keep)))
            (when (> excess 0)
              (goto-char (byte-to-position (1+ excess)))
              (delete-region (point-min) (point))))
          (goto-char (point-min))
          (insert note))))))

(defun harmless-shell--text (job)
  "Return JOB's output so far."
  (if (buffer-live-p (harmless-shell-job-buffer job))
      (with-current-buffer (harmless-shell-job-buffer job)
        (buffer-string))
    (or (harmless-shell-job-output job) "")))

(defun harmless-shell--close-buffer (job)
  "Drop JOB's process buffer.  The caller stores the output text first."
  (let ((proc (harmless-shell-job-proc job))
        (buf (harmless-shell-job-buffer job)))
    (when (processp proc)
      (ignore-errors (set-process-buffer proc nil)))
    (when (buffer-live-p buf)
      (kill-buffer buf))
    (setf (harmless-shell-job-buffer job) nil)))

(defun harmless-shell--foreground-text (job)
  "Return the tool result for a foreground JOB that has exited."
  (harmless-shell--for-model
   (format "exit %s\n%s"
           (or (harmless-shell-job-exit job) "?")
           (harmless-shell--text job))))

(defun harmless-shell--background-text (job)
  "Return the tool result that hands JOB to the model as a background id."
  (let ((raw (harmless-shell--text job))
        (id (harmless-shell-job-id job)))
    (harmless-shell--for-model
     (if (eq (harmless-shell-job-status job) 'running)
         (format "background %s\n%s" id raw)
       (format "background %s\nexit %s\n%s"
               id (or (harmless-shell-job-exit job) "?") raw)))))

(defun harmless-shell--snapshot (job)
  "Return the `shell_output' text for JOB."
  (let ((raw (harmless-shell--text job))
        (id (harmless-shell-job-id job)))
    (harmless-shell--for-model
     (pcase (harmless-shell-job-status job)
       ('running (format "shell %s running\n%s" id raw))
       ('killed (format "shell %s killed\n%s" id raw))
       (_ (format "shell %s exited %s\n%s"
                  id (or (harmless-shell-job-exit job) "?") raw))))))

(defun harmless-shell--fire-waiter (waiter text)
  "Call WAITER once with TEXT."
  (unless (nth 2 waiter)
    (setf (nth 2 waiter) t)
    (when (timerp (nth 1 waiter))
      (cancel-timer (nth 1 waiter)))
    (funcall (car waiter) text)))

(defun harmless-shell--deliver-waiters (job)
  "Give every waiter on JOB the current snapshot."
  (let ((waiters (harmless-shell-job-waiters job))
        (text (harmless-shell--snapshot job)))
    (setf (harmless-shell-job-waiters job) nil)
    (dolist (waiter waiters)
      (harmless-shell--fire-waiter waiter text))))

(defun harmless-shell--notify (session job)
  "Tell the user JOB finished, once.  Do not append a model message."
  (unless (harmless-shell-job-notified job)
    (setf (harmless-shell-job-notified job) t)
    (let* ((id (harmless-shell-job-id job))
           (status (if (eq (harmless-shell-job-status job) 'killed)
                       "killed"
                     (format "exited %s"
                             (or (harmless-shell-job-exit job) "?")))))
      (message "Harmless: background shell %s %s" id status)
      (harmless-emit session
                     (list :background id status
                           (harmless-shell-job-command job))))))

(defun harmless-shell--finish (session job)
  "Record that JOB's process has exited and wake its waiters."
  (when (eq (harmless-shell-job-status job) 'running)
    (setf (harmless-shell-job-status job)
          (if (eq (harmless-shell-job-exit job) 'killed) 'killed 'exited)))
  (let ((proc (harmless-shell-job-proc job)))
    (when (and proc (not (eq (harmless-shell-job-exit job) 'killed)))
      (setf (harmless-shell-job-exit job) (process-exit-status proc)))
    (when (timerp (harmless-shell-job-timer job))
      (cancel-timer (harmless-shell-job-timer job))
      (setf (harmless-shell-job-timer job) nil))
    (harmless-session-untrack-process session proc))
  (harmless-shell--settle-cwd session job)
  (harmless-shell--close-buffer job)
  (harmless-shell--deliver-waiters job))

(defun harmless-shell--adopt (session command proc buf background)
  "Register PROC as a shell job of SESSION and return the job."
  (let* ((n (1+ (or (harmless-session-shell-seq session) 0)))
         (job (harmless-shell-job--create
               :id (number-to-string n)
               :command command
               :proc proc
               :buffer buf
               :output ""
               :status 'running
               :background background)))
    (setf (harmless-session-shell-seq session) n)
    (push job (harmless-session-shell-jobs session))
    (set-process-filter
     proc
     (lambda (_proc text)
       (harmless-shell--append-output buf text)))
    job))

(defun harmless-tools-shell--run (session args callback)
  "Run ARGS command in SESSION and call CALLBACK with the output string.
A positive `timeout' waits that many seconds, then leaves the process
running and returns its id.  `background' true returns the id at once.
Nil and \"\" commands, and a timeout that is not a positive number,
are refused."
  (let ((command (harmless-tool-arg args :command)))
    (if (not (and (stringp command) (not (string-empty-p command))))
        (funcall callback "Error: command is empty")
      (let ((cwd (harmless-session-require-project session))
            (timeout (harmless-shell--run-timeout args)))
        (cond
         ((stringp timeout)
          (funcall callback timeout))
         ((harmless-shell-plan-refuses-p session command)
          (funcall callback harmless-shell-plan-refusal))
         (t
          (let* ((background (harmless-json-true-p
                              (harmless-tool-arg args :background)))
                 (buf (generate-new-buffer " *harmless-shell*"))
                 (kind (harmless-shell-kind))
                 (proc (let ((default-directory cwd))
                         (make-process
                          :name "harmless-shell"
                          :buffer buf
                          :command (harmless-shell--argv
                                    kind
                                    (harmless-shell--wrap
                                     kind
                                     (harmless-shell-directory session)
                                     command))
                          :coding '(utf-8-unix . utf-8-unix)
                          :connection-type 'pipe
                          :sentinel #'ignore)))
                 (job (harmless-shell--adopt session command proc buf background)))
            (unless background
              (harmless-session-track-process session proc)
              (setf (harmless-shell-job-timer job)
                    (run-at-time
                     timeout nil
                     (lambda ()
                       (when (and (eq (harmless-shell-job-status job) 'running)
                                  (not (harmless-shell-job-delivered job))
                                  (process-live-p proc))
                         (setf (harmless-shell-job-background job) t
                               (harmless-shell-job-delivered job) t)
                         (harmless-session-untrack-process session proc)
                         (funcall callback
                                  (harmless-shell--background-text job)))))))
            (set-process-sentinel
             proc
             (lambda (p _change)
               (unless (or (process-live-p p)
                           (not (eq (harmless-shell-job-status job) 'running)))
                 (harmless-shell--finish session job)
                 (cond
                  ((harmless-shell-job-delivered job)
                   (harmless-shell--notify session job))
                  (t
                   (setf (harmless-shell-job-delivered job) t)
                   (funcall callback
                            (if (harmless-shell-job-background job)
                                (progn
                                  (setf (harmless-shell-job-notified job) t)
                                  (harmless-shell--background-text job))
                              (harmless-shell--foreground-text job))))))))
            (unless (process-live-p proc)
              (funcall (process-sentinel proc) proc "exited"))
            (when (and background
                       (not (harmless-shell-job-delivered job)))
              (setf (harmless-shell-job-delivered job) t)
              (funcall callback (harmless-shell--background-text job)))
            proc)))))))

(defun harmless-tools-shell--output (session args callback)
  "Call CALLBACK with the output of the background shell in ARGS.
`id' is required.  `timeout' waits that many seconds for the process
to exit.  An unknown id, an empty id, and a bad timeout are refused."
  (let ((id (harmless-shell--id (harmless-tool-arg args :id)))
        (timeout (harmless-shell--output-timeout args)))
    (cond
     ((null id)
      (funcall callback "Error: shell id is empty"))
     ((stringp timeout)
      (funcall callback timeout))
     (t
      (let ((job (harmless-shell-find session id)))
        (cond
         ((null job)
          (funcall callback (format "Error: no background shell %s" id)))
         ((or (not (eq (harmless-shell-job-status job) 'running))
              (null timeout))
          (funcall callback (harmless-shell--snapshot job)))
         (t
          (let ((waiter (list callback nil nil)))
            (setf (nth 1 waiter)
                  (run-at-time
                   timeout nil
                   (lambda ()
                     (when (memq waiter (harmless-shell-job-waiters job))
                       (setf (harmless-shell-job-waiters job)
                             (delq waiter (harmless-shell-job-waiters job)))
                       (harmless-shell--fire-waiter
                        waiter (harmless-shell--snapshot job))))))
            (push waiter (harmless-shell-job-waiters job))))))))))

(defun harmless-tools-shell--kill (session args)
  "Stop the background shell named in ARGS.  Return a result string.
An unknown id, an empty id, and a shell that is not running are refused."
  (let ((id (harmless-shell--id (harmless-tool-arg args :id))))
    (cond
     ((null id) "Error: shell id is empty")
     (t
      (let ((job (harmless-shell-find session id)))
        (cond
         ((null job) (format "Error: no background shell %s" id))
         ((not (eq (harmless-shell-job-status job) 'running))
          (format "Error: shell %s is not running" id))
         (t
          (setf (harmless-shell-job-exit job) 'killed
                (harmless-shell-job-status job) 'killed)
          (harmless-tools-shell-kill (harmless-shell-job-proc job))
          (harmless-shell--finish session job)
          (harmless-shell--notify session job)
          (format "killed shell %s" id))))))))

(defun harmless-tools-shell-register ()
  "Register the shell tools."
  (harmless-register-tool
   (harmless-tool-create
    :name "run_shell"
    :description "Run a shell command in the project. A cd that stays inside the project is remembered for the next command. A read-only command does not ask for permission. In plan mode a command that can write is refused. timeout is seconds to wait before the command is left running in the background. background true returns an id immediately. Read it with shell_output and stop it with shell_kill."
    :class 'shell
    :classify #'harmless-shell-run-class
    :async t
    :schema '(:type "object"
              :properties (:command (:type "string"
                                    :description "Shell command")
                           :timeout (:type "number"
                                    :description "Seconds to wait before leaving the command in the background. A positive number. Omit it for the default.")
                           :background (:type "boolean"
                                       :description "Return a background id immediately."))
              :required ["command"])
    :fn #'harmless-tools-shell--run))
  (harmless-register-tool
   (harmless-tool-create
    :name "shell_output"
    :description "Read a background shell by id. timeout waits that many seconds for it to exit. Omit timeout to return the output already available."
    :class 'read
    :async t
    :schema '(:type "object"
              :properties (:id (:type "string"
                                 :description "Background shell id")
                           :timeout (:type "number"
                                    :description "Seconds to wait for the command to exit."))
              :required ["id"])
    :fn #'harmless-tools-shell--output))
  (harmless-register-tool
   (harmless-tool-create
    :name "shell_kill"
    :description "Stop a background shell by id."
    :class 'shell
    :schema '(:type "object"
              :properties (:id (:type "string"
                                 :description "Background shell id"))
              :required ["id"])
    :fn #'harmless-tools-shell--kill)))

(harmless-tools-shell-register)

(provide 'harmless-tools-shell)

;;; harmless-tools-shell.el ends here
