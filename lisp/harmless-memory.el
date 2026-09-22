;;; harmless-memory.el --- Cross-session memory for Harmless -*- lexical-binding: t; -*-

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
;; Memory is Markdown under `harmless-directory'.  Global notes apply
;; to every project.  Workspace notes belong to one project directory.
;; New facts land in `observations/_inbox/'.  `harmless-dream' folds
;; them into `topics/' and moves the inbox files to `archive/'.
;; `MEMORY.md' is a generated index.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-tools)

(defun harmless-memory-root ()
  "Return the root directory for Harmless memory."
  (expand-file-name "memory" harmless-directory))

(defun harmless-memory--global-p (scope)
  "Return non-nil if SCOPE names the global memory."
  (member scope '("global" "g")))

(defun harmless-memory-scope-dir (scope cwd)
  "Return the directory for SCOPE, using CWD for workspace memory."
  (if (harmless-memory--global-p scope)
      (expand-file-name "global" (harmless-memory-root))
    (expand-file-name
     (harmless-session-encode-cwd (or cwd default-directory))
     (expand-file-name "workspaces" (harmless-memory-root)))))

(defun harmless-memory--ensure (scope cwd)
  "Create the directory layout for SCOPE and return it."
  (let ((dir (harmless-memory-scope-dir scope cwd)))
    (dolist (sub '("topics" "observations/_inbox" "archive"))
      (harmless-ensure-directory (expand-file-name sub dir)))
    dir))

(defun harmless-memory--slug (name)
  "Return a filename slug for NAME."
  (let ((slug (downcase (string-trim (or name "")))))
    (setq slug (replace-regexp-in-string "[^a-z0-9]+" "-" slug))
    (setq slug (replace-regexp-in-string "\\`-+\\|-+\\'" "" slug))
    (if (string-empty-p slug) "note" slug)))

(defun harmless-memory--resolve (scope cwd rel)
  "Return REL resolved inside SCOPE, or signal if it escapes."
  (when (or (null rel)
            (string-empty-p rel)
            (file-name-absolute-p rel)
            (string-prefix-p ".." rel)
            (string-match-p "/\\.\\." rel))
    (error "Path escapes memory"))
  (let* ((root (file-name-as-directory
                (file-truename (harmless-memory--ensure scope cwd))))
         (path (expand-file-name rel root)))
    (unless (string-prefix-p root path)
      (error "Path escapes memory"))
    (when (file-exists-p path)
      (unless (string-prefix-p root (file-truename path))
        (error "Path escapes memory")))
    path))

(defun harmless-memory--md-files (dir)
  "Return Markdown files directly in DIR, or nil."
  (when (file-directory-p dir)
    (sort (directory-files dir t "\\.md\\'" t) #'string<)))

(defun harmless-memory--read-note (path)
  "Return a plist (:topic :title :body) for the note at PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (let ((topic nil)
          (title nil))
      (when (looking-at "---[ \t]*\n")
        (forward-line 1)
        (let ((start (point)))
          (when (re-search-forward "^---[ \t]*\n" nil t)
            (dolist (line (split-string
                           (buffer-substring-no-properties start (match-beginning 0))
                           "\n" t))
              (when (string-match "\\`topic:[ \t]*\\(.+\\)\\'" line)
                (setq topic (string-trim (match-string 1 line))))))))
      (when (re-search-forward "^# \\(.+\\)[ \t]*$" nil t)
        (setq title (string-trim (match-string 1)))
        (forward-line 1))
      (list :topic (and topic (not (string-empty-p topic)) topic)
            :title (or title (file-name-base path))
            :body (string-trim (buffer-substring-no-properties (point) (point-max)))))))

(defun harmless-memory--summary (path)
  "Return a one-line summary of the note at PATH."
  (let ((body (plist-get (harmless-memory--read-note path) :body)))
    (harmless-truncate
     (or (car (split-string (or body "") "\n" t "[ \t]+"))
         "(empty)")
     160)))

(defun harmless-memory--title (path)
  "Return the title of the note at PATH."
  (plist-get (harmless-memory--read-note path) :title))

(defun harmless-memory--rebuild (scope cwd)
  "Regenerate MEMORY.md for SCOPE."
  (let* ((dir (harmless-memory--ensure scope cwd))
         (topics (harmless-memory--md-files (expand-file-name "topics" dir)))
         (inbox (harmless-memory--md-files
                 (expand-file-name "observations/_inbox" dir)))
         (label (if (harmless-memory--global-p scope) "Global" "Workspace")))
    (with-temp-file (expand-file-name "MEMORY.md" dir)
      (insert (format "# %s memory index\n\n" label)
              "> Generated by Harmless.  Do not edit this file directly.\n\n")
      (insert "## Topics\n\n")
      (if topics
          (dolist (file topics)
            (insert (format "- **%s** — %s (`topics/%s`)\n"
                            (harmless-memory--title file)
                            (harmless-memory--summary file)
                            (file-name-nondirectory file))))
        (insert "(none)\n"))
      (insert "\n## Pending observations\n\n")
      (if inbox
          (dolist (file inbox)
            (insert (format "- **%s** (`observations/_inbox/%s`)\n"
                            (harmless-memory--title file)
                            (file-name-nondirectory file))))
        (insert "(none)\n")))
    dir))

(defun harmless-memory-remember (scope cwd title body &optional topic)
  "Store TITLE and BODY as an inbox observation in SCOPE.
TOPIC is an optional slug hint.  Return the path relative to the scope."
  (when (or (string-empty-p (string-trim (or title "")))
            (string-empty-p (string-trim (or body ""))))
    (error "Title and body are required"))
  (harmless-memory--ensure scope cwd)
  (let* ((slug (and topic (not (string-empty-p topic))
                    (harmless-memory--slug topic)))
         (name (format "%s-%s.md"
                       (format-time-string "%Y%m%dT%H%M%SZ" nil t)
                       (substring (harmless-uuid) 0 8)))
         (rel (concat "observations/_inbox/" name))
         (path (harmless-memory--resolve scope cwd rel)))
    (with-temp-file path
      (insert "---\n")
      (when slug (insert (format "topic: %s\n" slug)))
      (insert (format "scope: %s\ncreated: %s\n---\n\n# %s\n\n%s\n"
                      (if (harmless-memory--global-p scope) "global" "workspace")
                      (harmless-now-iso)
                      (string-trim title)
                      (string-trim body))))
    (harmless-memory--rebuild scope cwd)
    rel))

(defun harmless-memory-read (scope cwd rel)
  "Return the contents of REL inside SCOPE."
  (let ((path (harmless-memory--resolve scope cwd rel)))
    (unless (file-readable-p path)
      (error "No such memory file: %s" rel))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun harmless-memory-write-topic (scope cwd topic contents)
  "Replace the topic named TOPIC in SCOPE with CONTENTS."
  (when (string-empty-p (string-trim (or contents "")))
    (error "Topic contents are required"))
  (let* ((slug (harmless-memory--slug topic))
         (rel (format "topics/%s.md" slug))
         (path (harmless-memory--resolve scope cwd rel)))
    (harmless-memory--ensure scope cwd)
    (with-temp-file path
      (insert contents)
      (unless (string-suffix-p "\n" contents)
        (insert "\n")))
    (harmless-memory--rebuild scope cwd)
    (format "Wrote %s" rel)))

(defun harmless-memory--fold (scope cwd file)
  "Append the observation FILE into its topic in SCOPE."
  (let* ((note (harmless-memory--read-note file))
         (title (plist-get note :title))
         (slug (harmless-memory--slug (or (plist-get note :topic) title)))
         (rel (format "topics/%s.md" slug))
         (path (harmless-memory--resolve scope cwd rel))
         (body (or (plist-get note :body) "")))
    (harmless-memory--ensure scope cwd)
    (if (file-exists-p path)
        (let ((existing (with-temp-buffer
                          (insert-file-contents path)
                          (buffer-string))))
          (with-temp-file path
            (insert existing)
            (unless (string-suffix-p "\n" existing) (insert "\n"))
            (insert (format "\n## %s\n\n%s\n" title body))))
      (with-temp-file path
        (insert (format "# %s\n\n%s\n" title body))))))

(defun harmless-memory-dream (scope cwd)
  "Fold SCOPE's inbox into topics.  Return the number moved."
  (let* ((dir (harmless-memory-scope-dir scope cwd))
         (inbox (harmless-memory--md-files
                 (expand-file-name "observations/_inbox" dir)))
         (dest (expand-file-name
                (format "archive/dream-%s"
                        (format-time-string "%Y%m%dT%H%M%SZ" nil t))
                dir))
         (n 0))
    (when inbox
      (harmless-ensure-directory dest)
      (dolist (file inbox)
        (harmless-memory--fold scope cwd file)
        (rename-file file (expand-file-name (file-name-nondirectory file) dest) t)
        (setq n (1+ n)))
      (harmless-memory--rebuild scope cwd))
    n))

(defun harmless-memory--scope-index (scope cwd)
  "Return index lines for SCOPE, or nil when it has no notes."
  (let* ((dir (harmless-memory-scope-dir scope cwd))
         (topics (harmless-memory--md-files (expand-file-name "topics" dir)))
         (inbox (harmless-memory--md-files
                 (expand-file-name "observations/_inbox" dir)))
         (label (if (harmless-memory--global-p scope) "Global" "Workspace")))
    (when (or topics inbox)
      (concat
       (format "### %s\n" label)
       (when topics
         (concat
          (mapconcat
           (lambda (file)
             (format "- **%s** — %s (`topics/%s`)"
                     (harmless-memory--title file)
                     (harmless-memory--summary file)
                     (file-name-nondirectory file)))
           topics
           "\n")
          "\n"))
       (when inbox
         (concat
          (if topics "\n" "")
          "Pending:\n"
          (mapconcat
           (lambda (file)
             (format "- **%s** (`observations/_inbox/%s`)"
                     (harmless-memory--title file)
                     (file-name-nondirectory file)))
           inbox
           "\n")
          "\n"))))))

(defun harmless-memory-catalog (cwd)
  "Return the memory index for CWD, or nil when memory is empty."
  (let ((parts (delq nil
                     (list (harmless-memory--scope-index "global" cwd)
                           (harmless-memory--scope-index "workspace" cwd)))))
    (when parts
      (concat
       "Memory from earlier sessions.  This index is a map.  Read a note with memory_read before relying on it.  Record a durable fact with memory_remember.  Replace a curated topic with memory_write_topic.  Do not edit MEMORY.md.  Do not store secrets, credentials, or one-off task state.  Use workspace for this project and global for a preference that applies everywhere.\n\n"
       (mapconcat #'identity parts "\n")))))

(defun harmless-memory--tool-remember (session args)
  "Tool wrapper: remember ARGS on SESSION's project."
  (harmless-memory-remember
   (or (harmless-tool-arg args :scope) "workspace")
   (harmless-session-cwd session)
   (or (harmless-tool-arg args :title) "")
   (or (harmless-tool-arg args :body) "")
   (harmless-tool-arg args :topic)))

(defun harmless-memory--tool-read (session args)
  "Tool wrapper: read a memory file for SESSION."
  (harmless-memory-read
   (or (harmless-tool-arg args :scope) "workspace")
   (harmless-session-cwd session)
   (harmless-tool-arg args :path)))

(defun harmless-memory--tool-write-topic (session args)
  "Tool wrapper: write a topic for SESSION."
  (harmless-memory-write-topic
   (or (harmless-tool-arg args :scope) "workspace")
   (harmless-session-cwd session)
   (or (harmless-tool-arg args :topic) "")
   (or (harmless-tool-arg args :contents) "")))

(defun harmless-memory-register ()
  "Register memory tools."
  (harmless-register-tool
   (harmless-tool-create
    :name "memory_read"
    :description "Read a memory note. PATH is relative to the scope, such as topics/emacs.md or observations/_inbox/NAME.md. SCOPE is workspace or global."
    :class 'read
    :schema '(:type "object"
              :properties (:path (:type "string")
                           :scope (:type "string"
                                   :description "workspace or global"))
              :required ["path"])
    :fn #'harmless-memory--tool-read))
  (harmless-register-tool
   (harmless-tool-create
    :name "memory_remember"
    :description "Save a durable fact to the memory inbox. Use workspace for this project and global for a preference that applies everywhere. TOPIC is an optional slug, such as emacs."
    :class 'read
    :schema '(:type "object"
              :properties (:title (:type "string")
                           :body (:type "string")
                           :topic (:type "string")
                           :scope (:type "string"
                                   :description "workspace or global"))
              :required ["title" "body"])
    :fn #'harmless-memory--tool-remember))
  (harmless-register-tool
   (harmless-tool-create
    :name "memory_write_topic"
    :description "Replace a curated memory topic with CONTENTS, which is the full Markdown file. TOPIC is a slug such as emacs."
    :class 'read
    :schema '(:type "object"
              :properties (:topic (:type "string")
                           :contents (:type "string")
                           :scope (:type "string"
                                   :description "workspace or global"))
              :required ["topic" "contents"])
    :fn #'harmless-memory--tool-write-topic)))

(harmless-memory-register)

;;;###autoload
(defun harmless-remember (title body &optional global)
  "Save TITLE and BODY in workspace memory.
With GLOBAL, or a prefix argument, save it in global memory."
  (interactive
   (list (read-string "Remember: ")
         (read-string "Note: ")
         current-prefix-arg))
  (let ((rel (harmless-memory-remember
              (if global "global" "workspace")
              (harmless-current-cwd)
              title body)))
    (message "Remembered %s" rel)
    rel))

;;;###autoload
(defun harmless-dream ()
  "Fold inbox observations into topics for this project and for global memory."
  (interactive)
  (let* ((cwd (harmless-current-cwd))
         (n (+ (harmless-memory-dream "workspace" cwd)
               (harmless-memory-dream "global" cwd))))
    (message "Folded %d observation%s into topics" n (if (= n 1) "" "s"))
    n))

(provide 'harmless-memory)

;;; harmless-memory.el ends here
