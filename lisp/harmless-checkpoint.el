;;; harmless-checkpoint.el --- Shadow git snapshots for Harmless -*- lexical-binding: t; -*-

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
;; A checkpoint is a commit in a git repository stored under the Harmless
;; data directory.  The project's .git is not the git directory.  Snapshots
;; are off until `harmless-checkpoints' is set.  A snapshot that fails does
;; not block the tool that asked for it.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-tools)
(require 'harmless-tools-fs)

(defgroup harmless-checkpoint nil
  "Shadow git snapshots."
  :group 'harmless)

(defcustom harmless-checkpoints nil
  "When non-nil, snapshot a tree before an edit or a writing shell.
The snapshot is stored outside the project's git directory.  Nil
takes no snapshot."
  :type 'boolean
  :group 'harmless-checkpoint)

(defconst harmless-checkpoint-max-bytes 1000000
  "Files larger than this many bytes are left out of a snapshot.")

(defconst harmless-checkpoint-max-files 5000
  "A tree with more regular files than this is not snapshotted.")

(defvar harmless-checkpoint--seen (make-hash-table :test 'equal)
  "Session id to directories already snapshotted during this turn.")

(defun harmless-checkpoint-begin-turn (session)
  "Forget which directories SESSION snapshotted during the previous turn."
  (when (harmless-session-p session)
    (remhash (harmless-session-id session) harmless-checkpoint--seen)))

(defun harmless-checkpoint--slash (path)
  "Return PATH with backslashes written as forward slashes."
  (subst-char-in-string ?\\ ?/ path))

(defun harmless-checkpoint--key (dir)
  "Return the identity used to snapshot DIR once per turn."
  (harmless-checkpoint--slash
   (directory-file-name
    (if (harmless-tools-fs--textual-p dir)
        dir
      (file-truename dir)))))

(defun harmless-checkpoint--root-text-p (text)
  "Return non-nil if TEXT is \"/\" or a drive root."
  (or (harmless-filesystem-root-p text)
      (harmless-extra-root-text-p text)))

(defun harmless-checkpoint--tree (session file)
  "Return the project or extra directory that contains FILE, or nil."
  (let ((cwd (condition-case nil
                 (harmless-session-require-project session)
               (error nil))))
    (cond
     ((or (null cwd) (not (stringp file))) nil)
     ((harmless-tools-fs-inside-p cwd file) cwd)
     (t (cl-find-if (lambda (root)
                      (harmless-tools-fs-inside-p root file))
                    (harmless-extra--usable session))))))

(defun harmless-checkpoint--skippable-p (dir)
  "Return non-nil if DIR must not be snapshotted."
  (or (not (stringp dir))
      (string-empty-p (string-trim dir))
      (not (file-name-absolute-p dir))
      (harmless-checkpoint--root-text-p dir)
      (not (file-directory-p dir))
      (condition-case nil
          (harmless-checkpoint--root-text-p (file-truename dir))
        (error t))))

(defun harmless-checkpoint--data-dir ()
  "Return the data directory, or signal a checkpoint path error.
Nil uses the Harmless data directory."
  (when (and (stringp harmless-directory)
             (string-empty-p (string-trim harmless-directory)))
    (error "checkpoint path is empty"))
  (when (and (stringp harmless-directory)
             (harmless-checkpoint--root-text-p harmless-directory))
    (error "checkpoint path is the filesystem root"))
  (let ((dir (condition-case err
                 (harmless-data-directory)
               (error
                (if (string-prefix-p "Refusing to store Harmless data"
                                     (error-message-string err))
                    (error "checkpoint path is the filesystem root")
                  (signal (car err) (cdr err)))))))
    (when (or (not (stringp dir))
              (harmless-checkpoint--root-text-p dir))
      (error "checkpoint path is the filesystem root"))
    dir))

(defun harmless-checkpoint--store (work)
  "Return the shadow repository for WORK.
The directory is not created.  A bad data directory signals.
Each snapshotted directory has its own repository, so a second
directory does not reuse the first directory's index."
  (let ((data (harmless-checkpoint--data-dir)))
    (expand-file-name
     (harmless-session-encode-cwd work)
     (expand-file-name "checkpoints" data))))

(defun harmless-checkpoint--env (git-dir work-tree config)
  "Return a process environment that uses GIT-DIR and WORK-TREE."
  (append
   (list (concat "GIT_DIR=" (harmless-checkpoint--slash git-dir))
         (concat "GIT_WORK_TREE=" (harmless-checkpoint--slash work-tree))
         (concat "GIT_CONFIG_GLOBAL=" (harmless-checkpoint--slash config))
         "GIT_CONFIG_NOSYSTEM=1"
         "GIT_AUTHOR_NAME=Harmless"
         "GIT_AUTHOR_EMAIL=harmless@example.com"
         "GIT_COMMITTER_NAME=Harmless"
         "GIT_COMMITTER_EMAIL=harmless@example.com")
   (cl-remove-if (lambda (kv) (string-match-p "\\`GIT_" kv))
                 process-environment)))

(defun harmless-checkpoint--git (git-dir work-tree &rest args)
  "Run git ARGS with GIT-DIR and WORK-TREE.  Return the output.
Signal when git is missing or the command fails."
  (unless (executable-find "git")
    (error "checkpoint needs git"))
  (let ((config (expand-file-name "harmless-gitconfig" (file-name-directory git-dir))))
    (unless (file-exists-p config)
      (write-region "" nil config nil 'silent))
    (with-temp-buffer
      (let* ((process-environment
              (harmless-checkpoint--env git-dir work-tree config))
             (coding-system-for-read 'utf-8-unix)
             (coding-system-for-write 'utf-8-unix)
             (default-directory work-tree)
             (status (apply #'call-process
                            "git" nil (current-buffer) nil
                            (append
                             (list "-c" "user.email=harmless@example.com"
                                   "-c" "user.name=Harmless"
                                   "-c" "commit.gpgsign=false"
                                   "-c" "core.autocrlf=false"
                                   "-c" "safe.directory=*"
                                   "-c" (concat "core.hooksPath="
                                                (harmless-checkpoint--slash
                                                 (expand-file-name
                                                  "hooks"
                                                  (file-name-directory git-dir)))))
                             args))))
        (unless (eq 0 status)
          (error "checkpoint git failed"))
        (buffer-string)))))

(defun harmless-checkpoint--init (store)
  "Create STORE as a git repository when it has none.  Return its git dir."
  (unless (executable-find "git")
    (error "checkpoint needs git"))
  (let ((git-dir (expand-file-name ".git" store))
        (config (expand-file-name "harmless-gitconfig" store)))
    (make-directory store t)
    (make-directory (expand-file-name "hooks" store) t)
    (unless (file-exists-p config)
      (write-region "" nil config nil 'silent))
    (unless (file-exists-p (expand-file-name "HEAD" git-dir))
      (let ((process-environment
             (append
              (list (concat "GIT_CONFIG_GLOBAL="
                            (harmless-checkpoint--slash config))
                    "GIT_CONFIG_NOSYSTEM=1")
              (cl-remove-if (lambda (kv) (string-match-p "\\`GIT_" kv))
                            process-environment)))
            (default-directory store))
        (unless (eq 0 (call-process "git" nil nil nil
                                    "-c" "user.email=harmless@example.com"
                                    "-c" "user.name=Harmless"
                                    "-c" "init.defaultBranch=harmless"
                                    "init" "--quiet" store))
          (error "checkpoint git failed"))))
    (let ((exclude (expand-file-name "info/exclude" git-dir)))
      (make-directory (file-name-directory exclude) t)
      (let ((text (if (file-exists-p exclude)
                      (with-temp-buffer
                        (insert-file-contents exclude)
                        (buffer-string))
                    "")))
        (unless (string-match-p "\\(?:\\`\\|\n\\)\\.git\\(?:\\'\\|\n\\)" text)
          (write-region ".git\n" nil exclude 'append 'silent))))
    git-dir))

(defun harmless-checkpoint--files (root)
  "Return relative files under ROOT, or the symbol `too-many'.
A file over the size cap is omitted.  `.git' is omitted.  A symlink
that leaves ROOT is omitted."
  (let ((count 0)
        (files nil)
        (overflow nil)
        (base (file-name-as-directory
               (if (harmless-tools-fs--textual-p root)
                   root
                 (file-truename root)))))
    (cl-labels ((walk (dir)
                  (unless overflow
                    (dolist (name (directory-files
                                   dir nil directory-files-no-dot-files-regexp))
                      (unless overflow
                        (let ((full (expand-file-name name dir)))
                          (cond
                           ((string= name ".git") nil)
                           ((and (file-symlink-p full)
                                 (or (file-directory-p full)
                                     (condition-case nil
                                         (not (harmless-tools-fs-inside-p
                                               root (file-truename full)))
                                       (error t))))
                            nil)
                           ((file-directory-p full)
                            (walk full))
                           ((file-regular-p full)
                            (setq count (1+ count))
                            (when (> count harmless-checkpoint-max-files)
                              (setq overflow t))
                            (when (and (not overflow)
                                       (<= (or (file-attribute-size
                                                (file-attributes full))
                                               0)
                                           harmless-checkpoint-max-bytes))
                              (push (harmless-checkpoint--slash
                                     (file-relative-name full base))
                                    files))))))))))
      (walk (directory-file-name root)))
    (if overflow 'too-many (nreverse files))))

(defun harmless-checkpoint--next-id (git-dir work)
  "Return the next checkpoint id for GIT-DIR."
  (let ((n (condition-case nil
               (string-to-number
                (string-trim
                 (harmless-checkpoint--git
                  git-dir work "rev-list" "--count" "HEAD")))
             (error 0))))
    (format "cp-%d" (1+ n))))

(defun harmless-checkpoint--take (_session work)
  "Snapshot WORK and return the checkpoint id."
  (let* ((store (harmless-checkpoint--store work))
         (files (harmless-checkpoint--files work)))
    (when (eq files 'too-many)
      (error "checkpoint tree is too large"))
    (let ((git-dir (harmless-checkpoint--init store)))
      (dolist (batch (seq-partition files 40))
        (apply #'harmless-checkpoint--git git-dir work "add" "--" batch))
      (let ((id (harmless-checkpoint--next-id git-dir work)))
        (harmless-checkpoint--git
         git-dir work "commit" "--allow-empty" "--no-verify" "--quiet"
         "-m" id)
        id))))

(defun harmless-checkpoint-before (session file)
  "Snapshot the tree that contains FILE when checkpoints are on.
A disabled checkpoint, a skipped tree, a second request in this
turn, and a failed snapshot return nil and do not signal."
  (when (and harmless-checkpoints
             (harmless-session-p session)
             (stringp (harmless-session-id session)))
    (let ((work (harmless-checkpoint--tree session file)))
      (when (and work (not (harmless-checkpoint--skippable-p work)))
        (let* ((id (harmless-session-id session))
               (key (condition-case nil
                        (harmless-checkpoint--key work)
                      (error nil)))
               (seen (and key (gethash id harmless-checkpoint--seen))))
          (when (and key (not (member key seen)))
            (puthash id (cons key seen) harmless-checkpoint--seen)
            (condition-case nil
                (harmless-checkpoint--take session work)
              (error nil)))))))
  nil)

(defun harmless-checkpoint--id (raw)
  "Return RAW as a checkpoint id, or signal."
  (cond
   ((null raw) (error "checkpoint id is empty"))
   ((not (stringp raw)) (error "checkpoint id must be a string"))
   ((string-empty-p (string-trim raw)) (error "checkpoint id is empty"))
   ((not (string-match-p "\\`cp-[1-9][0-9]*\\'" (string-trim raw)))
    (error "no such checkpoint"))
   (t (string-trim raw))))

(defun harmless-checkpoint--hash (work id)
  "Return the commit hash for ID in WORK's store."
  (let* ((store (harmless-checkpoint--store work))
         (git-dir (expand-file-name ".git" store)))
    (unless (file-exists-p (expand-file-name "HEAD" git-dir))
      (error "no such checkpoint"))
    (let ((found nil))
      (dolist (line (split-string
                     (harmless-checkpoint--git
                      git-dir work "log" "--format=%H%x09%s")
                     "\n" t))
        (when (and (not found)
                   (string-match "\\`\\([0-9a-f]+\\)\t\\(.*\\)\\'" line)
                   (string= (match-string 2 line) id))
          (setq found (match-string 1 line))))
      (unless (and found (string-match-p "\\`[0-9a-f]\\{7,40\\}\\'" found))
        (error "no such checkpoint"))
      (cons git-dir found))))

(defun harmless-checkpoint--located (session file)
  "Return (DIRECTORY . RELATIVE) for FILE in SESSION.
Nil FILE uses the project and has no relative path."
  (cond
   ((null file)
    (cons (harmless-session-require-project session) nil))
   ((not (stringp file)) (error "checkpoint path must be a string"))
   ((string-empty-p (string-trim file)) (error "checkpoint path is empty"))
   ((harmless-checkpoint--root-text-p file)
    (error "checkpoint path is the filesystem root"))
   (t
    (let* ((full (condition-case err
                     (harmless-tools-fs-resolve session (string-trim file))
                   (error
                    (if (string-prefix-p "Path escapes project:"
                                         (error-message-string err))
                        (error "checkpoint path escapes the project")
                      (signal (car err) (cdr err))))))
           (work (or (harmless-checkpoint--tree session full)
                     (error "checkpoint path escapes the project")))
           (rel (harmless-checkpoint--slash (file-relative-name full work))))
      (when (or (string= rel "..")
                (string-prefix-p "../" rel)
                (harmless-checkpoint--root-text-p full))
        (error "checkpoint path escapes the project"))
      (cons work rel)))))

(defun harmless-checkpoint-diff (session id &optional file)
  "Return the work tree diff from checkpoint ID.
FILE limits the diff to one path.  Nil compares the whole project."
  (unless (harmless-session-p session)
    (error "No Harmless session"))
  (let* ((name (harmless-checkpoint--id id))
         (located (harmless-checkpoint--located session file))
         (work (car located))
         (rel (cdr located))
         (found (harmless-checkpoint--hash work name))
         (text (string-trim
                (apply #'harmless-checkpoint--git
                       (car found) work
                       "diff" "--no-ext-diff" (cdr found) "--"
                       (when rel (list rel))))))
    (if (> (length text) 100000)
        (concat (substring text 0 100000) "\n[truncated]")
      text)))

(defun harmless-checkpoint--present-p (git-dir work hash rel)
  "Return non-nil if REL exists in HASH."
  (condition-case nil
      (progn
        (harmless-checkpoint--git
         git-dir work "cat-file" "-e" (concat hash ":" rel))
        t)
    (error nil)))

(defun harmless-checkpoint-restore (session id &optional file)
  "Restore checkpoint ID into SESSION's project.
Nil FILE restores every file the checkpoint recorded and removes
files created after it.  A hand-edit of another file is kept when
FILE names one path.  The project's git directory is not the
repository this command uses."
  (unless (harmless-session-p session)
    (error "No Harmless session"))
  (when (harmless-session-plan-mode session)
    (harmless-tools-fs--reject-plan-edit session))
  (let* ((name (harmless-checkpoint--id id))
         (located (harmless-checkpoint--located session file))
         (work (car located))
         (rel (cdr located))
         (found (harmless-checkpoint--hash work name))
         (git-dir (car found))
         (hash (cdr found)))
    (if (null rel)
        (progn
          (harmless-checkpoint--git git-dir work "checkout" hash "--" ".")
          (let ((store (harmless-checkpoint--store work))
                (clean (list "clean" "-fd" "-e" ".git")))
            (when (harmless-tools-fs-inside-p work store)
              (setq clean
                    (append clean
                            (list "-e"
                                  (file-name-as-directory
                                   (harmless-checkpoint--slash
                                    (file-relative-name store work)))))))
            (apply #'harmless-checkpoint--git git-dir work clean))
          (format "Restored %s" name))
      (unless (harmless-checkpoint--present-p git-dir work hash rel)
        (error "checkpoint file is missing"))
      (harmless-checkpoint--git git-dir work "checkout" hash "--" rel)
      (format "Restored %s in %s" rel name))))

(defun harmless-checkpoint--tool-diff (session args)
  "Tool wrapper: diff ARGS' checkpoint."
  (harmless-checkpoint-diff session
                            (harmless-tool-arg args :id)
                            (harmless-tool-arg args :path)))

(defun harmless-checkpoint--tool-restore (session args)
  "Tool wrapper: restore ARGS' checkpoint."
  (harmless-checkpoint-restore session
                               (harmless-tool-arg args :id)
                               (harmless-tool-arg args :path)))

(harmless-register-tool
 (harmless-tool-create
  :name "harmless-tool-checkpoint-diff"
  :description "Show the diff from a shadow checkpoint to the current tree. ID is the checkpoint name. PATH limits the diff to one file. Nil and an empty id are refused. A checkpoint that is not in the store is refused. The project's git directory is not read."
  :class 'read
  :schema '(:type "object"
            :properties (:id (:type "string"
                              :description "Checkpoint id, such as cp-1")
                         :path (:type "string"
                                :description "One file to compare. Omit for the whole tree."))
            :required ["id"])
  :fn #'harmless-checkpoint--tool-diff))

(harmless-register-tool
 (harmless-tool-create
  :name "harmless-tool-checkpoint-restore"
  :description "Restore a shadow checkpoint. ID is the checkpoint name. PATH restores that one file and keeps later edits of other files. Omit PATH to restore every recorded file. Nil and an empty id are refused. A checkpoint that is not in the store is refused. The project's git directory is not updated."
  :class 'edit
  :schema '(:type "object"
            :properties (:id (:type "string"
                              :description "Checkpoint id, such as cp-1")
                         :path (:type "string"
                                :description "One file to restore. Omit to restore every file."))
            :required ["id"])
  :fn #'harmless-checkpoint--tool-restore))

(provide 'harmless-checkpoint)

;;; harmless-checkpoint.el ends here
