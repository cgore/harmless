;;; harmless-plugin.el --- Install a plugin as one bundle -*- lexical-binding: t; -*-

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
;; A plugin is one directory: skills, agents, hooks.json, and mcp.json.
;; Installing it copies that directory.  It does not merge the files
;; into the project's other skill, agent, or MCP locations.  A
;; marketplace is a directory of plugins, or a marketplace.json that
;; names them.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-tools-fs)
(require 'harmless-hook)
(require 'harmless-mcp)

(defun harmless-plugin-name-p (name)
  "Return non-nil if NAME is a plugin id, not a path."
  (and (stringp name)
       (string-match-p "\\`[a-z0-9]+\\(?:-[a-z0-9]+\\)*\\'" name)))

(defun harmless-plugin-name (name)
  "Return NAME as a plugin id, or signal.
Nil and \"\" are empty.  A path, \"~\", and the filesystem root are
not names and are not expanded."
  (cond
   ((null name) (error "plugin name is empty"))
   ((not (stringp name)) (error "plugin name must be a string"))
   (t
    (let ((raw (string-trim name)))
      (cond
       ((string-empty-p raw) (error "plugin name is empty"))
       ((or (string-prefix-p "~" raw)
            (string-prefix-p "/" raw)
            (string-prefix-p "\\" raw)
            (string-match-p "[/\\\\]" raw)
            (string-match-p "\\`[A-Za-z]:[/\\]?\\'" raw)
            (file-name-absolute-p raw))
        (error "plugin name is not a name"))
       ((harmless-plugin-name-p raw) raw)
       (t (error "plugin name is not a name")))))))

(defun harmless-plugin-scope (scope)
  "Return `project' or `user' for SCOPE.
Nil is `project'."
  (cond
   ((null scope) 'project)
   ((memq scope '(project user)) scope)
   ((and (stringp scope)
         (member (string-trim scope) '("project" "user")))
    (intern (string-trim scope)))
   (t (error "plugin scope is not a scope"))))

(defun harmless-plugin--root-text-p (text)
  "Return non-nil if TEXT is \"/\" or a drive root."
  (or (harmless-filesystem-root-p text)
      (and (stringp text)
           (or (string-match-p "\\`[A-Za-z]:[/\\]?\\'" (string-trim text))
               (harmless-extra-root-text-p text)))))

(defun harmless-plugin-directory (dir)
  "Return DIR as an absolute directory, or signal.
A relative path and \"~\" are not expanded.  \"/\", \"c:/\", and a
drive root are refused."
  (cond
   ((or (not (stringp dir))
        (string-empty-p (string-trim dir)))
    (error "plugin directory is empty"))
   ((harmless-plugin--root-text-p dir)
    (error "plugin directory is the filesystem root"))
   ((or (string-prefix-p "~" (string-trim dir))
        (not (file-name-absolute-p dir)))
    (error "plugin directory must be absolute"))
   (t (harmless-absolute-directory dir))))

(defun harmless-plugin--read (file)
  "Return the text of FILE."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents file))
    (buffer-string)))

(defun harmless-plugin--manifest (dir)
  "Return the plugin.json plist in DIR, or signal."
  (let* ((file (expand-file-name "plugin.json" dir))
         (parsed (condition-case nil
                     (harmless-json-decode (harmless-plugin--read file))
                   (error (error "plugin.json is not valid")))))
    (unless (harmless-plist-p parsed)
      (error "plugin.json is not valid"))
    (list :name (harmless-plugin-name (plist-get parsed :name))
          :description (let ((text (plist-get parsed :description)))
                         (when (and (stringp text)
                                    (not (string-empty-p (string-trim text))))
                           (string-trim text)))
          :path dir)))

(defun harmless-plugin--source (marketplace source)
  "Return the plugin directory SOURCE names inside MARKETPLACE.
SOURCE must be relative and stay inside MARKETPLACE."
  (cond
   ((or (not (stringp source))
        (string-empty-p (string-trim source)))
    (error "plugin source is empty"))
   (t
    (setq source (string-trim source))
    (when (or (string-prefix-p "~" source)
              (string-prefix-p "/" source)
              (string-prefix-p "\\" source)
              (string-match-p "\\`[A-Za-z]:" source)
              (file-name-absolute-p source))
      (error "plugin source must be relative"))
    (let ((full (expand-file-name source marketplace)))
      (cond
       ((or (harmless-plugin--root-text-p full)
            (equal (file-truename full) (file-truename "/")))
        (error "plugin source is the filesystem root"))
       ((not (harmless-directory-strictly-under-p full marketplace))
        (error "plugin source escapes the marketplace: %s" source))
       ((not (file-directory-p full))
        (error "plugin source is missing: %s" source))
       (t full))))))

(defun harmless-plugin-marketplace-entries (dir)
  "Return plugin plists in marketplace DIR.
Each plist has :name and :path.  A marketplace.json, when present,
is the whole list.  Otherwise each child with a matching
plugin.json is a plugin."
  (setq dir (harmless-plugin-directory dir))
  (cond
   ((not (file-exists-p dir))
    (error "marketplace is missing"))
   ((not (file-directory-p dir))
    (error "marketplace is not a directory"))
   (t
    (let ((file (expand-file-name "marketplace.json" dir)))
      (if (file-regular-p file)
          (harmless-plugin--entries-from-file dir file)
        (harmless-plugin--entries-from-tree dir))))))

(defun harmless-plugin--entries-from-file (dir file)
  "Return plugins named by FILE under marketplace DIR."
  (let ((parsed (condition-case nil
                    (harmless-json-decode (harmless-plugin--read file))
                  (error (error "marketplace.json is not valid")))))
    (unless (harmless-plist-p parsed)
      (error "marketplace.json is not valid"))
    (unless (plist-member parsed :plugins)
      (error "marketplace has no plugins"))
    (let ((items (plist-get parsed :plugins))
          (out nil))
      (unless (and (listp items) (proper-list-p items))
        (error "marketplace has no plugins"))
      (dolist (item items)
        (unless (harmless-plist-p item)
          (error "marketplace plugin is not a plugin"))
        (let* ((name (harmless-plugin-name (plist-get item :name)))
               (source (or (plist-get item :source) name))
               (path (harmless-plugin--source dir source))
               (manifest (harmless-plugin--manifest path)))
          (unless (equal (plist-get manifest :name) name)
            (error "plugin name does not match"))
          (setq out (append out (list (list :name name :path path))))))
      out)))

(defun harmless-plugin--entries-from-tree (dir)
  "Return child plugins of marketplace DIR."
  (let (out)
    (dolist (name (sort (directory-files dir) #'string<))
      (unless (or (member name '("." ".."))
                  (eq (aref name 0) ?.))
        (let ((path (expand-file-name name dir)))
          (when (and (file-directory-p path)
                     (file-regular-p (expand-file-name "plugin.json" path))
                     (harmless-plugin-name-p name))
            (let ((manifest (harmless-plugin--manifest path)))
              (when (equal (plist-get manifest :name) name)
                (setq out (append out (list (list :name name
                                                  :path path))))))))))
    out))

(defun harmless-plugin-marketplace-find (dir name)
  "Return the plugin named NAME in marketplace DIR."
  (setq name (harmless-plugin-name name))
  (or (cl-find name (harmless-plugin-marketplace-entries dir)
               :key (lambda (entry) (plist-get entry :name))
               :test #'equal)
      (error "no plugin named %s" name)))

(defun harmless-plugin--symbol (value)
  "Return VALUE as a symbol when it is text."
  (cond
   ((and (stringp value) (not (string-empty-p (string-trim value))))
    (intern (string-trim value)))
   (t value)))

(defun harmless-plugin--hooks-in (dir)
  "Return the hooks.json hooks in plugin DIR.
A missing file is nil."
  (let ((file (expand-file-name "hooks.json" dir)))
    (when (file-regular-p file)
      (let ((parsed (condition-case nil
                        (harmless-json-decode (harmless-plugin--read file))
                      (error (error "plugin hooks are not valid")))))
        (unless (and (listp parsed) (proper-list-p parsed))
          (error "plugin hooks are not valid"))
        (mapcar
         (lambda (raw)
           (unless (harmless-plist-p raw)
             (error "hook must be a hook"))
           (let ((hook (list :event (harmless-plugin--symbol
                                     (plist-get raw :event))
                             :type (harmless-plugin--symbol
                                    (plist-get raw :type)))))
             (dolist (key '(:tool :command :url :server :method :prompt))
               (let ((value (plist-get raw key)))
                 (when (stringp value)
                   (setq hook (plist-put hook key value)))))
             (harmless-hook-validate-one hook)))
         parsed)))))

(defun harmless-plugin--servers-in (dir)
  "Return MCP servers from plugin DIR's mcp.json.
A missing file is nil."
  (let ((file (expand-file-name "mcp.json" dir)))
    (when (file-regular-p file)
      (harmless-mcp--parse (harmless-plugin--read file)))))

(defun harmless-plugin--check-bundle (dir)
  "Signal unless the hooks and MCP files in DIR are valid."
  (harmless-plugin--hooks-in dir)
  (harmless-plugin--servers-in dir)
  nil)

(defun harmless-plugin-user-root ()
  "Return the user plugin directory, or nil when it is not safe.
This does not create the directory."
  (condition-case nil
      (let ((dir (file-name-as-directory
                  (expand-file-name "plugins" (harmless-data-directory)))))
        (unless (harmless-plugin--root-text-p dir)
          dir))
    (error nil)))

(defun harmless-plugin-project-root (session)
  "Return SESSION's project plugin directory, or nil.
A detached chat, a relative path, and the filesystem root have
none.  This does not create the directory and does not consult
`default-directory'."
  (when (and (harmless-session-p session)
             (not (harmless-session-detached-p session)))
    (let ((cwd (harmless-session-cwd session)))
      (when (and (stringp cwd)
                 (not (string-empty-p (string-trim cwd)))
                 (file-name-absolute-p cwd)
                 (not (string-prefix-p "~" (string-trim cwd)))
                 (not (harmless-plugin--root-text-p cwd)))
        (let ((dir (file-name-as-directory
                    (expand-file-name
                     ".harmless/plugins"
                     (file-name-as-directory
                      (directory-file-name cwd))))))
          (unless (harmless-plugin--root-text-p dir)
            dir))))))

(defun harmless-plugin--project-dir (session)
  "Return SESSION's project plugin directory, or signal.
A detached chat signals \"This chat is not attached to a project\".
A relative path signals \"plugin directory must be absolute\".
The filesystem root signals \"plugin directory is the filesystem
root\".  This does not create the directory."
  (when (or (not (harmless-session-p session))
            (harmless-session-detached-p session))
    (error "This chat is not attached to a project"))
  (let ((cwd (harmless-session-cwd session)))
    (cond
     ((or (not (stringp cwd))
          (string-empty-p (string-trim cwd)))
      (error "This chat is not attached to a project"))
     ((harmless-plugin--root-text-p cwd)
      (error "plugin directory is the filesystem root"))
     ((or (string-prefix-p "~" (string-trim cwd))
          (not (file-name-absolute-p cwd)))
      (error "plugin directory must be absolute"))
     (t
      (let ((dir (file-name-as-directory
                  (expand-file-name
                   ".harmless/plugins"
                   (file-name-as-directory
                    (directory-file-name cwd))))))
        (when (harmless-plugin--root-text-p dir)
          (error "plugin directory is the filesystem root"))
        dir)))))

(defun harmless-plugin--install-root (session scope)
  "Return the directory that receives a SCOPE install for SESSION."
  (if (eq (harmless-plugin-scope scope) 'user)
      (or (harmless-plugin-user-root)
          (error "plugin directory is the filesystem root"))
    (harmless-plugin--project-dir session)))

(defun harmless-plugin--install (session marketplace plugin &optional scope)
  "Copy PLUGIN from MARKETPLACE into SESSION's SCOPE.
The copy is one directory.  Nothing else in the project is
written.  Return the installed directory."
  (unless (harmless-session-p session)
    (error "No Harmless session"))
  (setq plugin (harmless-plugin-name plugin))
  (setq scope (harmless-plugin-scope scope))
  (let* ((root (harmless-plugin--install-root session scope))
         (entry (harmless-plugin-marketplace-find marketplace plugin))
         (source (plist-get entry :path))
         (dest (directory-file-name (expand-file-name plugin root))))
    (harmless-plugin--check-bundle source)
    (unless (harmless-directory-strictly-under-p dest root)
      (error "plugin directory is the filesystem root"))
    (when (or (harmless-plugin--root-text-p dest)
              (equal (file-truename dest) (file-truename "/")))
      (error "plugin directory is the filesystem root"))
    (when (file-exists-p dest)
      (error "plugin is already installed: %s" plugin))
    (make-directory root t)
    (copy-directory source dest t t)
    (unless (file-regular-p (expand-file-name "plugin.json" dest))
      (error "plugin.json is not valid"))
    dest))

(defun harmless-plugin--names (root)
  "Return installed plugin ids in ROOT, sorted by name.
A missing directory is nil.  The filesystem root is not listed."
  (when (and (stringp root)
             (file-directory-p root)
             (not (harmless-plugin--root-text-p root)))
    (let (names)
      (dolist (name (directory-files root))
        (when (and (harmless-plugin-name-p name)
                   (file-directory-p (expand-file-name name root))
                   (file-regular-p
                    (expand-file-name
                     "plugin.json" (expand-file-name name root))))
          (let ((manifest (condition-case nil
                              (harmless-plugin--manifest
                               (expand-file-name name root))
                            (error nil))))
            (when (and manifest
                       (equal (plist-get manifest :name) name))
              (push name names)))))
      (sort names #'string<))))

(defun harmless-plugin-installed (session)
  "Return plugins installed for SESSION.
User plugins come first.  Each plist has :name, :path, and
:scope."
  (let (out)
    (dolist (pair (list (cons 'user (harmless-plugin-user-root))
                        (cons 'project (harmless-plugin-project-root session))))
      (dolist (name (harmless-plugin--names (cdr pair)))
        (setq out (append out
                          (list (list :name name
                                      :scope (car pair)
                                      :path (expand-file-name
                                             name (cdr pair))))))))
    out))

(defun harmless-plugin--skill-dirs (root)
  "Return skill directories of plugins in ROOT, lowest name first."
  (let (dirs)
    (dolist (name (harmless-plugin--names root))
      (let ((dir (expand-file-name
                  "skills" (expand-file-name name root))))
        (when (and (file-directory-p dir)
                   (not (harmless-plugin--root-text-p dir)))
          (push dir dirs))))
    (nreverse dirs)))

(defun harmless-plugin-skill-directories (ancestor)
  "Return plugin skill directories installed under ANCESTOR."
  (when (and (stringp ancestor)
             (file-name-absolute-p ancestor)
             (not (harmless-plugin--root-text-p ancestor)))
    (harmless-plugin--skill-dirs
     (expand-file-name ".harmless/plugins" ancestor))))

(defun harmless-plugin-user-skill-directories ()
  "Return skill directories of plugins installed for the user."
  (harmless-plugin--skill-dirs (harmless-plugin-user-root)))

(defun harmless-plugin--agent-files (root)
  "Return agent files of plugins in ROOT, lowest name first."
  (let (files)
    (dolist (name (harmless-plugin--names root))
      (let ((dir (expand-file-name
                  "agents" (expand-file-name name root))))
        (when (and (file-directory-p dir)
                   (not (harmless-plugin--root-text-p dir)))
          (dolist (file (sort (directory-files dir nil "\\.md\\'")
                              #'string<))
            (unless (eq (aref file 0) ?.)
              (let ((path (expand-file-name file dir)))
                (when (file-regular-p path)
                  (push path files))))))))
    (nreverse files)))

(defun harmless-plugin-agent-files (ancestor)
  "Return agent files of plugins installed under ANCESTOR."
  (when (and (stringp ancestor)
             (file-name-absolute-p ancestor)
             (not (harmless-plugin--root-text-p ancestor)))
    (harmless-plugin--agent-files
     (expand-file-name ".harmless/plugins" ancestor))))

(defun harmless-plugin-user-agent-files ()
  "Return agent files of plugins installed for the user."
  (harmless-plugin--agent-files (harmless-plugin-user-root)))

(defun harmless-plugin--servers (root)
  "Return MCP servers from plugins in ROOT.
A later plugin name overrides the same server name."
  (let (servers)
    (dolist (name (harmless-plugin--names root))
      (condition-case nil
          (setq servers
                (harmless-mcp--merge
                 servers
                 (harmless-plugin--servers-in
                  (expand-file-name name root))))
        (error nil)))
    servers))

(defun harmless-plugin-mcp-servers (session)
  "Return MCP servers from plugins installed for SESSION.
Project plugins override a user plugin of the same server name."
  (harmless-mcp--merge
   (harmless-plugin--servers (harmless-plugin-user-root))
   (harmless-plugin--servers (harmless-plugin-project-root session))))

(defun harmless-plugin--hooks (root)
  "Return hooks from plugins in ROOT, lowest name first."
  (let (hooks)
    (dolist (name (harmless-plugin--names root))
      (setq hooks
            (append hooks
                    (harmless-plugin--hooks-in
                     (expand-file-name name root)))))
    hooks))

(defun harmless-plugin-hooks (session)
  "Return hooks from plugins installed for SESSION.
User plugins come first.  `harmless-hooks' is not included."
  (append (harmless-plugin--hooks (harmless-plugin-user-root))
          (harmless-plugin--hooks (harmless-plugin-project-root session))))

(provide 'harmless-plugin)

;;; harmless-plugin.el ends here
