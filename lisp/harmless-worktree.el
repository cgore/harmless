;;; harmless-worktree.el --- Isolated git checkouts for Harmless -*- lexical-binding: t; -*-

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
;; `enter_worktree' points a session at a new git checkout of the same
;; repository.  `spawn_agent' and a workflow agent can start a child
;; there.  The checkout lives under the Harmless data directory, not
;; inside the project and not at the filesystem root.  The same name
;; is the same checkout.  Leaving a session's worktree does not delete
;; it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-tools)
(require 'harmless-agent)

(defvar harmless--session)

(defconst harmless-worktree-plan-refusal
  "Error: Plan mode is on. A worktree is refused"
  "Result when plan mode blocks a new checkout.")

(defun harmless-worktree-directory-p (dir)
  "Return non-nil if DIR can be a git checkout.
Nil, \"\", a relative path, and the filesystem root cannot.  A
trailing slash is the same directory.  A drive root is not /."
  (and (stringp dir)
       (not (string-empty-p dir))
       (harmless-path-absolute-p dir)
       (not (harmless-filesystem-root-p dir))))

(defun harmless-worktree-name (value)
  "Return a worktree name, or nil when VALUE was omitted.
\"\" and whitespace signal \"worktree name is empty\".  A path signals
\"worktree name is not a name\"."
  (cond
   ((null value) nil)
   ((and (symbolp value) (not (keywordp value)))
    (harmless-worktree-name (symbol-name value)))
   ((not (stringp value))
    (error "worktree name is not a name"))
   (t
    (setq value (string-trim value))
    (cond
     ((string-empty-p value) (error "worktree name is empty"))
     ((harmless-agent-name-p value) value)
     (t (error "worktree name is not a name"))))))

(defun harmless-worktree-name-required (value)
  "Return VALUE as a worktree name.  An omitted name is empty."
  (or (harmless-worktree-name value)
      (error "worktree name is empty")))

(defun harmless-worktree-requested-p (args)
  "Return non-nil when ARGS includes a worktree value.
An omitted argument is nil.  An empty string still counts."
  (not (null (harmless-tool-arg args :worktree))))

(defun harmless-worktree-root ()
  "Return the absolute directory that holds checkouts.
Never returns the filesystem root."
  (let ((dir (file-name-as-directory
              (expand-file-name "worktrees" (harmless-data-directory)))))
    (unless (and (not (harmless-filesystem-root-p dir))
                 (harmless-directory-strictly-under-p
                  dir (harmless-data-directory)))
      (error "Refusing to store a Harmless worktree at %s" dir))
    dir))

(defun harmless-worktree--key (repo)
  "Return a short directory name for REPO.
The full path is not encoded again.  A child session stores its
project by that path, and a second encoding exceeds a Windows path."
  (substring (md5 (directory-file-name
                   (harmless-collapse-path (expand-file-name repo))))
             0 16))

(defun harmless-worktree-path (repo name)
  "Return the absolute checkout for NAME of main tree REPO.
The path is under the data directory, outside REPO, and it is not
the filesystem root."
  (unless (and (harmless-worktree-directory-p repo)
               (harmless-agent-name-p name))
    (error "Refusing to store a Harmless worktree at %s"
           (or repo "/")))
  (let* ((root (harmless-worktree-root))
         (key (harmless-worktree--key repo))
         (dir (harmless-collapse-path
               (expand-file-name name (expand-file-name key root)))))
    (unless (and (harmless-safe-path-component-p key)
                 (harmless-directory-strictly-under-p dir root)
                 (not (harmless-filesystem-root-p dir))
                 (not (harmless-same-directory-p dir repo))
                 (not (harmless-directory-strictly-under-p dir repo)))
      (error "Refusing to store a Harmless worktree at %s" dir))
    dir))

(defun harmless-worktree--first-line (text)
  "Return the first non-empty line of TEXT, or \"unknown\"."
  (or (car (split-string (or text "") "[\n\r]+" t))
      "unknown"))

(defun harmless-worktree--normalize (path)
  "Return absolute PATH with slashes and no trailing slash."
  (setq path (subst-char-in-string ?\\ ?/ (string-trim (or path ""))))
  (unless (and (not (string-empty-p path))
               (file-name-absolute-p path))
    (error "git worktree failed: %s"
           (harmless-worktree--first-line path)))
  (directory-file-name (harmless-collapse-path path)))

(defun harmless-worktree--abs (repo path)
  "Return PATH absolute against REPO."
  (setq path (subst-char-in-string ?\\ ?/ (string-trim (or path ""))))
  (when (string-empty-p path)
    (error "git worktree failed: unknown"))
  (unless (file-name-absolute-p path)
    (setq path (expand-file-name path repo)))
  (harmless-worktree--normalize path))

(defun harmless-worktree--git (repo &rest args)
  "Run git ARGS in REPO.  Return (EXIT . OUTPUT)."
  (unless (harmless-worktree-directory-p repo)
    (error "this project is not a git repository"))
  (let ((git (executable-find "git")))
    (unless git
      (error "git is not available"))
    (with-temp-buffer
      (let* ((process-environment
              (cons "GIT_TERMINAL_PROMPT=0" process-environment))
             (exit (apply #'call-process git nil
                          (list (current-buffer) t) nil
                          (append (list "-C" repo) args))))
        (cons exit (string-trim (buffer-string)))))))

(defun harmless-worktree--entries (repo)
  "Return (PATH . BRANCH) rows for REPO's worktrees."
  (let ((result (harmless-worktree--git
                 repo "worktree" "list" "--porcelain"))
        (path nil)
        (branch nil)
        (rows nil))
    (unless (eq (car result) 0)
      (error "git worktree failed: %s"
             (harmless-worktree--first-line (cdr result))))
    (dolist (line (split-string (cdr result) "[\n\r]+" t))
      (cond
       ((string-prefix-p "worktree " line)
        (when path
          (push (cons path branch) rows))
        (setq path (harmless-worktree--abs repo (substring line 9))
              branch nil))
       ((string-prefix-p "branch " line)
        (setq branch (substring line 7)))))
    (when path
      (push (cons path branch) rows))
    (nreverse rows)))

(defun harmless-worktree--branch-p (repo branch)
  "Return non-nil if REPO has branch BRANCH."
  (eq (car (harmless-worktree--git
            repo "show-ref" "--verify" "--quiet"
            (concat "refs/heads/" branch)))
      0))

(defun harmless-worktree--assert-repo (dir)
  "Signal unless DIR is inside a git working tree.  Return DIR."
  (let ((result (harmless-worktree--git
                 dir "rev-parse" "--is-inside-work-tree")))
    (unless (and (eq (car result) 0) (equal (cdr result) "true"))
      (error "this project is not a git repository"))
    dir))

(defun harmless-worktree--repo (session)
  "Return SESSION's project directory, or signal.
Nil, \"\", a relative path, and / are not a project.  They are not
expanded, so a missing directory is not /."
  (let ((cwd (harmless-session-cwd session)))
    (if (or (harmless-session-detached-p session)
            (not (stringp cwd))
            (string-empty-p cwd)
            (not (file-name-absolute-p cwd))
            (harmless-filesystem-root-p cwd))
        (error "This chat is not attached to a project")
      (let ((dir (harmless-session-require-project session)))
        (unless (and (harmless-worktree-directory-p dir)
                     (file-directory-p dir))
          (error "this project is not a git repository"))
        dir))))

(defun harmless-worktree--main-root (repo)
  "Return the main working tree that owns REPO."
  (let ((result (harmless-worktree--git
                 repo "rev-parse" "--git-common-dir")))
    (unless (eq (car result) 0)
      (error "this project is not a git repository"))
    (let ((common (harmless-worktree--abs repo (cdr result))))
      (unless (equal (file-name-nondirectory common) ".git")
        (error "this project is not a git repository"))
      (let ((main (directory-file-name (file-name-directory common))))
        (unless (harmless-worktree-directory-p main)
          (error "this project is not a git repository"))
        main))))

(defun harmless-worktree--find (repo path)
  "Return the worktree row for PATH in REPO, or nil."
  (let ((want (harmless-worktree--normalize path)))
    (cl-find want (harmless-worktree--entries repo)
             :key #'car :test #'equal)))

(defun harmless-worktree--create (repo path branch name)
  "Create BRANCH at PATH from REPO.  Return the worktree plist."
  (when (or (file-exists-p path) (file-symlink-p path))
    (error "worktree path already exists"))
  (when (harmless-worktree--branch-p repo branch)
    (error "worktree branch already exists: %s" branch))
  (let ((parent (file-name-directory (file-name-as-directory path))))
    (unless (and parent
                 (harmless-directory-strictly-under-p
                  parent (harmless-worktree-root)))
      (error "Refusing to store a Harmless worktree at %s" path))
    (harmless-ensure-directory parent))
  (let ((result (harmless-worktree--git
                 repo "worktree" "add" "-b" branch path)))
    (unless (eq (car result) 0)
      (error "git worktree failed: %s"
             (harmless-worktree--first-line (cdr result)))))
  (unless (file-directory-p path)
    (error "git worktree failed: missing checkout"))
  (list :name name
        :path (harmless-worktree--normalize path)
        :branch branch))

(defun harmless-worktree-ensure (session name)
  "Return the worktree plist for NAME in SESSION's repository.
Create it from the current HEAD when it is missing.  The same name
is the same checkout.  Uncommitted files are not copied."
  (let* ((name (harmless-worktree-name-required name))
         (start (harmless-worktree--assert-repo
                 (harmless-worktree--repo session)))
         (main (harmless-worktree--main-root start))
         (branch (concat "harmless/" name))
         (path (harmless-worktree-path main name))
         (existing (harmless-worktree--find start path)))
    (if existing
        (list :name name
              :path (car existing)
              :branch (or (cdr existing) branch))
      (harmless-worktree--create start path branch name))))

(defun harmless-worktree-for-child (session args)
  "Return a checkout path for ARGS, or nil when none was requested.
The parent's project is not changed."
  (let ((name (harmless-worktree-name
               (harmless-tool-arg args :worktree))))
    (when name
      (plist-get (harmless-worktree-ensure session name) :path))))

(defun harmless-worktree-enter (session name)
  "Point SESSION at the worktree NAME and return its path.
The previous project is kept so the session can return to it.  A
remembered shell directory is cleared."
  (unless (harmless-session-p session)
    (error "No Harmless session"))
  (when (harmless-session-plan-mode session)
    (error "Plan mode is on. A worktree is refused"))
  (let* ((made (harmless-worktree-ensure session name))
         (slot (harmless-session-worktree session))
         (original (or (and slot (plist-get slot :original))
                       (directory-file-name
                        (harmless-session-require-project session))))
         (path (plist-get made :path)))
    (setf (harmless-session-cwd session) path)
    (setf (harmless-session-shell-cwd session) nil)
    (setf (harmless-session-worktree session)
          (list :name (plist-get made :name)
                :path path
                :branch (plist-get made :branch)
                :original original))
    (harmless-session-save session)
    path))

(defun harmless-worktree--exit (session)
  "Point SESSION back at the checkout it left.  Return that path.
The worktree directory stays on disk."
  (unless (harmless-session-p session)
    (error "No Harmless session"))
  (let* ((wt (harmless-session-worktree session))
         (original (and wt (plist-get wt :original))))
    (unless wt
      (error "this session is not in a worktree"))
    (unless (and (stringp original)
                 (not (string-empty-p original))
                 (file-name-absolute-p original)
                 (not (harmless-filesystem-root-p original))
                 (file-directory-p original))
      (error "worktree original is not a directory"))
    (setf (harmless-session-cwd session) (directory-file-name original))
    (setf (harmless-session-shell-cwd session) nil)
    (setf (harmless-session-worktree session) nil)
    (harmless-session-save session)
    (harmless-session-cwd session)))

(defun harmless-worktree-plan-refuses-p (session tool _args)
  "Return a refusal when plan mode blocks TOOL, or nil."
  (when (and (string= (harmless-tool-name tool) "enter_worktree")
             (harmless-session-plan-mode session))
    harmless-worktree-plan-refusal))

(defun harmless-worktree--tool-enter (session args)
  "enter_worktree implementation."
  (harmless-worktree-enter
   session
   (harmless-worktree-name-required (harmless-tool-arg args :name))))

(defun harmless-worktree--tool-exit (session _args)
  "exit_worktree implementation."
  (harmless-worktree--exit session))

(defun harmless-worktree--current ()
  "Return the session for an interactive command, or nil."
  (cond
   ((and (boundp 'harmless--session)
         (harmless-session-p harmless--session))
    harmless--session)
   (t (car (harmless-session-for-cwd
            (condition-case nil
                (harmless-current-cwd)
              (error nil)))))))

(defun harmless-worktree--command (fn)
  "Run FN on the current session and show the path it returns."
  (let ((session (harmless-worktree--current)))
    (unless session
      (user-error "No Harmless session"))
    (when (memq (harmless-session-status session)
                '(streaming waiting-permission))
      (user-error "A turn is in progress"))
    (condition-case err
        (message "Harmless: %s" (funcall fn session))
      (error (user-error "%s" (error-message-string err))))))

;;;###autoload
(defun harmless-worktree (name)
  "Create or reuse a worktree named NAME and enter it.
NAME is read from the minibuffer.  A running turn is refused."
  (interactive "sWorktree name: ")
  (harmless-worktree--command
   (lambda (session) (harmless-worktree-enter session name))))

;;;###autoload
(defun harmless-worktree-exit ()
  "Return the current session to the checkout it left.
A running turn is refused.  The worktree stays on disk."
  (interactive)
  (harmless-worktree--command #'harmless-worktree--exit))

(defun harmless-worktree-register ()
  "Register the worktree tools."
  (harmless-register-tool
   (harmless-tool-create
    :name "enter_worktree"
    :description
    (concat
     "Point this session at an isolated git checkout named NAME. "
     "The checkout branches from the current HEAD. "
     "The same name is the same checkout. "
     "Uncommitted files are not copied. "
     "The original project stays on disk.")
    :class 'edit
    :schema '(:type "object"
              :properties (:name (:type "string"
                                 :description "Checkout name, not a path"))
              :required ["name"])
    :fn #'harmless-worktree--tool-enter))
  (harmless-register-tool
   (harmless-tool-create
    :name "exit_worktree"
    :description
    (concat
     "Point this session back at the checkout it left. "
     "The worktree stays on disk.")
    :class 'edit
    :schema '(:type "object")
    :fn #'harmless-worktree--tool-exit)))

(harmless-worktree-register)

(provide 'harmless-worktree)

;;; harmless-worktree.el ends here
