;;; harmless-mcp.el --- MCP client for Harmless -*- lexical-binding: t; -*-

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
;; Configured MCP servers show up next to the built-in tools.  A
;; tool's schema is loaded when it is about to be called.  Resource
;; reads are built in.  The class is `network', so accept-edits still
;; asks.  Nothing is written into the project.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-tools)
(require 'harmless-agent)

(declare-function harmless-plugin-mcp-servers "harmless-plugin" (session))

(defvar harmless-mcp-call-function #'harmless-mcp-call-default
  "Function (SERVER METHOD PARAMS) that returns one MCP result.
SERVER is a plist with :name, :command, and :args.  The return
value is the JSON-RPC result object, not the envelope.")

(defvar harmless-mcp-exchange-function #'harmless-mcp-exchange-default
  "Function (COMMAND ARGS MESSAGES) that speaks one MCP session.
MESSAGES is a list of JSON-RPC plists.  Return the server's
stdout as a string.  Refuse a command that is not a program
before starting it.")

(defvar harmless-mcp--tools (make-hash-table :test 'eq)
  "Dynamic MCP tools last offered to each session.
This is not saved.  The global tool registry stays free of them.")

(defconst harmless-mcp-protocol-version "2024-11-05"
  "MCP protocol version sent in initialize.")

(defun harmless-mcp-global-file ()
  "Return the global mcp.json under the Harmless data directory."
  (expand-file-name "mcp.json" (harmless-data-directory)))

(defun harmless-mcp-project-file (session)
  "Return SESSION's project mcp.json, or nil when it has none.
A detached chat, nil, \"\", a relative path, and the filesystem
root do not select a file.  A drive root is not the filesystem
root."
  (when (and session (not (harmless-session-detached-p session)))
    (let ((cwd (harmless-session-cwd session)))
      (when (and (stringp cwd)
                 (not (string-empty-p cwd))
                 (harmless-path-absolute-p cwd)
                 (not (equal (directory-file-name cwd) "/"))
                 (not (harmless-filesystem-root-p cwd)))
        (let ((file (if (harmless-foreign-absolute-p cwd)
                        (concat (file-name-as-directory
                                 (harmless-collapse-path cwd))
                                ".harmless/mcp.json")
                      (expand-file-name
                       ".harmless/mcp.json"
                       (file-name-as-directory
                        (directory-file-name cwd))))))
          (when (and (stringp file)
                     (harmless-path-absolute-p file)
                     (not (equal file "/.harmless/mcp.json"))
                     (not (equal file "/mcp.json"))
                     (not (harmless-filesystem-root-p file)))
            file))))))

(defun harmless-mcp--server-name (name)
  "Return NAME as an MCP server name, or signal.
Nil is refused before a symbol is accepted.  Surrounding
whitespace is trimmed, so \"sub \" is \"sub\"."
  (cond
   ((null name)
    (error "MCP server name is not a name"))
   ((stringp name)
    (setq name (string-trim name)))
   ((keywordp name)
    (setq name (string-trim (substring (symbol-name name) 1))))
   ((symbolp name)
    (setq name (string-trim (symbol-name name))))
   (t
    (error "MCP server name is not a name")))
  (unless (harmless-agent-name-p name)
    (error "MCP server name is not a name"))
  name)

(defun harmless-mcp--command (command)
  "Return COMMAND trimmed, or signal when it is not a command.
This does not check that the command is a program."
  (cond
   ((null command) (error "MCP command is empty"))
   ((not (stringp command)) (error "MCP command must be a string"))
   (t
    (setq command (string-trim command))
    (when (string-empty-p command)
      (error "MCP command is empty"))
    command)))

(defun harmless-mcp--args (args)
  "Return ARGS when every element is a string.
Nil means the server has no arguments."
  (cond
   ((null args) nil)
   ((not (listp args)) (error "MCP argument must be a string"))
   (t
    (dolist (arg args)
      (unless (stringp arg)
        (error "MCP argument must be a string")))
    args)))

(defun harmless-mcp--entry (name spec)
  "Return a server plist for NAME and SPEC."
  (unless (harmless-plist-p spec)
    (error "MCP command is empty"))
  (list :name (harmless-mcp--server-name name)
        :command (harmless-mcp--command (plist-get spec :command))
        :args (harmless-mcp--args (plist-get spec :args))))

(defun harmless-mcp--merge (base extra)
  "Return BASE servers with EXTRA applied.
A name that is already present keeps its place and takes the new
command.  A new name is appended."
  (let ((out (mapcar #'copy-sequence base)))
    (dolist (item extra)
      (let ((at (cl-position (plist-get item :name) out
                             :key (lambda (server)
                                    (plist-get server :name))
                             :test #'equal)))
        (if at
            (setcar (nthcdr at out) (copy-sequence item))
          (setq out (append out (list (copy-sequence item)))))))
    out))

(defun harmless-mcp--servers-from (value)
  "Return servers from a decoded mcpServers VALUE.
Nil, from an empty object, means no servers."
  (cond
   ((null value) nil)
   ((not (harmless-plist-p value))
    (error "mcp.json has no servers"))
   (t
    (let (out)
      (while value
        (setq out (harmless-mcp--merge
                   out
                   (list (harmless-mcp--entry (car value) (cadr value)))))
        (setq value (cddr value)))
      out))))

(defun harmless-mcp--parse (text)
  "Return servers described by JSON TEXT."
  (let ((parsed (condition-case nil
                    (harmless-json-decode text)
                  (error (error "mcp.json is not valid")))))
    (unless (harmless-plist-p parsed)
      (error "mcp.json is not valid"))
    (unless (plist-member parsed :mcpServers)
      (error "mcp.json has no servers"))
    (harmless-mcp--servers-from (plist-get parsed :mcpServers))))

(defun harmless-mcp--read-file (file)
  "Return servers from FILE, or nil when FILE is missing.
FILE is not created.  The filesystem root is not a config file."
  (when (and (stringp file)
             (not (string-empty-p file))
             (file-name-absolute-p file)
             (not (equal file "/"))
             (not (equal file "/mcp.json"))
             (not (equal file "/.harmless/mcp.json"))
             (not (harmless-filesystem-root-p file))
             (file-regular-p file))
    (harmless-mcp--parse
     (with-temp-buffer
       (insert-file-contents file)
       (buffer-string)))))

(defun harmless-mcp-servers (session)
  "Return the MCP servers offered to SESSION.
The global file is first.  The project file overrides the same
name.  A detached chat has no project file."
  (harmless-mcp--merge
   (harmless-mcp--merge
    (harmless-mcp--read-file (harmless-mcp-global-file))
    (harmless-mcp--read-file (harmless-mcp-project-file session)))
   (when (fboundp 'harmless-plugin-mcp-servers)
     (harmless-plugin-mcp-servers session))))

(defun harmless-mcp--find (session name)
  "Return SESSION's server named NAME, or signal."
  (or (cl-find name (harmless-mcp-servers session)
               :key (lambda (server) (plist-get server :name))
               :test #'equal)
      (error "no MCP server %s" name)))

(defun harmless-mcp--server-arg (session args)
  "Return the server ARGS names on SESSION."
  (let ((name (harmless-tool-arg args :server)))
    (cond
     ((null name) (error "server name is empty"))
     ((not (stringp name)) (error "server name must be a string"))
     (t
      (setq name (string-trim name))
      (when (string-empty-p name)
        (error "server name is empty"))
      (unless (harmless-agent-name-p name)
        (error "server name is not a name"))
      (harmless-mcp--find session name)))))

(defun harmless-mcp--uri-arg (args)
  "Return the resource URI in ARGS.
\"/\" and \"c:/\" are URI text, not paths."
  (let ((uri (harmless-tool-arg args :uri)))
    (cond
     ((null uri) (error "resource uri is empty"))
     ((not (stringp uri)) (error "resource uri must be a string"))
     (t
      (setq uri (string-trim uri))
      (when (string-empty-p uri)
        (error "resource uri is empty"))
      uri))))

(defun harmless-mcp--drive-root-p (path)
  "Return non-nil if PATH is a Windows drive root."
  (and (stringp path)
       (string-match-p "\\`[A-Za-z]:[/\\]?\\'" path)))

(defun harmless-mcp--program (command)
  "Return a program to run for COMMAND, or signal.
\"/\", the filesystem root, a drive root, and a directory are
not programs.  A name that `executable-find' and
`file-executable-p' both miss is not a program."
  (setq command (harmless-mcp--command command))
  (when (or (equal command "/")
            (harmless-filesystem-root-p command)
            (harmless-mcp--drive-root-p command)
            (and (file-name-absolute-p command)
                 (file-directory-p command)))
    (error "MCP command is not a program"))
  (let ((found (executable-find command)))
    (cond
     ((and (stringp found)
           (not (file-directory-p found)))
      found)
     ((and (file-name-absolute-p command)
           (file-executable-p command)
           (not (file-directory-p command)))
      command)
     (t (error "MCP command is not a program")))))

(defun harmless-mcp-exchange-default (command args messages)
  "Speak MESSAGES to COMMAND with ARGS.  Return stdout.
A command that is not a program is refused before a process
starts."
  (setq command (harmless-mcp--program command))
  (when args
    (setq args (harmless-mcp--args args)))
  (unless (listp messages)
    (error "MCP call failed"))
  (let* ((payload (concat (mapconcat #'harmless-json-text messages "\n")
                          "\n"))
         (buf (generate-new-buffer " *harmless-mcp*"))
         (proc nil))
    (unwind-protect
        (let ((coding-system-for-write 'utf-8-unix)
              (coding-system-for-read 'utf-8-unix))
          (condition-case err
              (setq proc (make-process
                          :name "harmless-mcp"
                          :buffer buf
                          :command (cons command (or args nil))
                          :connection-type 'pipe
                          :noquery t
                          :stderr buf))
            (error (error "MCP call failed: %s"
                          (error-message-string err))))
          (process-send-string proc payload)
          (process-send-eof proc)
          (let ((deadline (+ (float-time) 20)))
            (while (and (process-live-p proc)
                        (< (float-time) deadline))
              (accept-process-output proc 0.2)))
          (when (process-live-p proc)
            (delete-process proc)
            (setq proc nil)
            (error "MCP call failed: timed out"))
          (with-current-buffer buf
            (buffer-string)))
      (when (and proc (process-live-p proc))
        (delete-process proc))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(defun harmless-mcp--id-match (got want)
  "Return non-nil if JSON-RPC id GOT is WANT."
  (or (equal got want)
      (and (numberp got) (numberp want) (= got want))
      (and (stringp got) (equal got (format "%s" want)))))

(defun harmless-mcp--rpc-result (raw id)
  "Return the JSON-RPC result in RAW for ID, or signal."
  (unless (stringp raw)
    (error "MCP call failed"))
  (let (found fallback)
    (dolist (line (split-string raw "\n" t))
      (let ((obj (harmless-json-decode-safe (string-trim line))))
        (when (harmless-plist-p obj)
          (when (harmless-mcp--id-match (plist-get obj :id) id)
            (setq found obj))
          (when (plist-get obj :error)
            (setq fallback obj)))))
    (unless found
      (let ((obj (harmless-json-decode-safe (string-trim raw))))
        (when (harmless-plist-p obj)
          (setq found obj))))
    (unless found
      (setq found fallback))
    (unless found
      (error "MCP call failed"))
    (when (plist-get found :error)
      (let* ((err (plist-get found :error))
             (msg (and (harmless-plist-p err)
                       (plist-get err :message))))
        (if (and (stringp msg) (not (string-empty-p (string-trim msg))))
            (error "MCP call failed: %s" (string-trim msg))
          (error "MCP call failed"))))
    (plist-get found :result)))

(defun harmless-mcp--request (id method params)
  "Return a JSON-RPC request plist for ID, METHOD, and PARAMS."
  (let ((msg (list :jsonrpc "2.0" :id id :method method)))
    (if params
        (append msg (list :params params))
      msg)))

(defun harmless-mcp-call-default (server method params)
  "Call METHOD on SERVER with PARAMS and return the result."
  (let ((messages
         (list (harmless-mcp--request
                1 "initialize"
                (list :protocolVersion harmless-mcp-protocol-version
                      :capabilities (make-hash-table :test 'equal)
                      :clientInfo (list :name "harmless"
                                        :version "0.1.0")))
               (list :jsonrpc "2.0"
                     :method "notifications/initialized")
               (harmless-mcp--request 2 method params))))
    (harmless-mcp--rpc-result
     (funcall harmless-mcp-exchange-function
              (plist-get server :command)
              (plist-get server :args)
              messages)
     2)))

(defun harmless-mcp--listed (server)
  "Return tool spec plists from SERVER's tools/list."
  (let* ((result (funcall harmless-mcp-call-function
                          server "tools/list" nil))
         (tools (and (harmless-plist-p result)
                     (plist-get result :tools)))
         (out nil))
    (dolist (spec tools)
      (when (harmless-plist-p spec)
        (let ((name (plist-get spec :name)))
          (when (and (stringp name) (not (string-empty-p name)))
            (push spec out)))))
    (nreverse out)))

(defun harmless-mcp--arguments (args)
  "Return ARGS as the object passed to tools/call."
  (cond
   ((null args) (make-hash-table :test 'equal))
   ((harmless-plist-p args) args)
   ((listp args)
    (let (out)
      (while args
        (when (stringp (car args))
          (setq out (plist-put out (intern (concat ":" (car args)))
                               (cadr args))))
        (setq args (cddr args)))
      (or out (make-hash-table :test 'equal))))
   (t (make-hash-table :test 'equal))))

(defun harmless-mcp--require-args (schema args)
  "Signal when ARGS omits a name in SCHEMA's required list."
  (let ((required (and (harmless-plist-p schema)
                       (plist-get schema :required))))
    (dolist (name required)
      (when (and (stringp name)
                 (null (harmless-tool-arg args (intern (concat ":" name)))))
        (error "MCP argument %s is required" name)))))

(defun harmless-mcp--text-blocks (content)
  "Return the text of CONTENT blocks, in order."
  (let (texts)
    (dolist (block content)
      (when (and (harmless-plist-p block)
                 (equal (plist-get block :type) "text"))
        (let ((text (plist-get block :text)))
          (when (and (stringp text) (not (string-empty-p text)))
            (push text texts)))))
    (nreverse texts)))

(defun harmless-mcp--format-call (result)
  "Return the text of a tools/call RESULT.
An isError result is prefixed with \"Error: \"."
  (unless (harmless-plist-p result)
    (error "MCP call returned no text"))
  (let ((texts (harmless-mcp--text-blocks (plist-get result :content))))
    (unless texts
      (error "MCP call returned no text"))
    (let ((text (string-join texts "\n")))
      (if (harmless-json-true-p (plist-get result :isError))
          (concat "Error: " text)
        text))))

(defun harmless-mcp-invoke (tool session server remote args)
  "Call REMOTE on SERVER for TOOL, after loading its schema."
  (ignore session)
  (let* ((spec (cl-find remote (harmless-mcp--listed server)
                        :key (lambda (item) (plist-get item :name))
                        :test #'equal))
         (schema (and spec (plist-get spec :inputSchema))))
    (unless spec
      (error "MCP call failed"))
    (setf (harmless-tool-schema tool)
          (if (harmless-plist-p schema)
              schema
            '(:type "object")))
    (harmless-mcp--require-args (harmless-tool-schema tool) args)
    (harmless-mcp--format-call
     (funcall harmless-mcp-call-function
              server "tools/call"
              (list :name remote
                    :arguments (harmless-mcp--arguments args))))))

(defun harmless-mcp--make-tool (server spec)
  "Return a network tool for SPEC on SERVER.
The offered schema is an empty object.  The server schema is
loaded when the tool runs."
  (let* ((remote (plist-get spec :name))
         (desc (plist-get spec :description))
         (tool (harmless-tool-create
                :name (format "harmless-tool-mcp-%s--%s"
                              (plist-get server :name) remote)
                :description (if (and (stringp desc)
                                      (not (string-empty-p desc)))
                                 desc
                               remote)
                :class 'network
                :schema '(:type "object")
                :fn #'ignore)))
    (setf (harmless-tool-fn tool)
          (lambda (session args)
            (harmless-mcp-invoke tool session server remote args)))
    tool))

(defun harmless-mcp-tools (session)
  "Return dynamic MCP tools for SESSION.
A server whose tools/list fails is skipped.  These tools are not
registered globally."
  (let (tools)
    (dolist (server (harmless-mcp-servers session))
      (condition-case nil
          (dolist (spec (harmless-mcp--listed server))
            (push (harmless-mcp--make-tool server spec) tools))
        (error nil)))
    (setq tools (nreverse tools))
    (puthash session tools harmless-mcp--tools)
    tools))

(defun harmless-mcp-tool-by-name (session name)
  "Return SESSION's dynamic MCP tool named NAME, or nil."
  (cl-find name (gethash session harmless-mcp--tools)
           :key #'harmless-tool-name
           :test #'equal))

(defun harmless-mcp--format-resources (resources)
  "Return a text list of RESOURCES, or \"No resources\"."
  (let (blocks)
    (dolist (item resources)
      (when (harmless-plist-p item)
        (let ((uri (plist-get item :uri))
              (name (plist-get item :name)))
          (when (and (stringp uri)
                     (not (string-empty-p (string-trim uri))))
            (setq uri (string-trim uri))
            (push (if (and (stringp name)
                           (not (string-empty-p (string-trim name))))
                      (concat uri "\n" (string-trim name))
                    uri)
                  blocks)))))
    (if blocks
        (string-join (nreverse blocks) "\n\n")
      "No resources")))

(defun harmless-mcp--resource-text (result)
  "Return the text of a resources/read RESULT."
  (unless (harmless-plist-p result)
    (error "resource has no text"))
  (let (texts)
    (dolist (item (plist-get result :contents))
      (when (harmless-plist-p item)
        (let ((text (plist-get item :text)))
          (when (and (stringp text) (not (string-empty-p text)))
            (push text texts)))))
    (if texts
        (string-join (nreverse texts) "\n")
      (error "resource has no text"))))

(defun harmless-mcp-list-resources (session args)
  "harmless-tool-mcp-list-resources implementation."
  (let ((server (harmless-mcp--server-arg session args)))
    (harmless-mcp--format-resources
     (plist-get (funcall harmless-mcp-call-function
                         server "resources/list" nil)
                :resources))))

(defun harmless-mcp-read-resource (session args)
  "harmless-tool-mcp-read-resource implementation."
  (let ((server (harmless-mcp--server-arg session args))
        (uri (harmless-mcp--uri-arg args)))
    (harmless-mcp--resource-text
     (funcall harmless-mcp-call-function
              server "resources/read"
              (list :uri uri)))))

(defun harmless-mcp-register ()
  "Register the built-in MCP resource tools."
  (harmless-register-tool
   (harmless-tool-create
    :name "harmless-tool-mcp-list-resources"
    :description "List resources on one configured MCP server. SERVER is the name from mcp.json. Nothing is written into the project."
    :class 'network
    :schema '(:type "object"
              :properties (:server (:type "string"
                                    :description "MCP server name"))
              :required ["server"])
    :fn #'harmless-mcp-list-resources))
  (harmless-register-tool
   (harmless-tool-create
    :name "harmless-tool-mcp-read-resource"
    :description "Read one MCP resource as text. SERVER is the name from mcp.json. URI is the resource URI. Nothing is written into the project."
    :class 'network
    :schema '(:type "object"
              :properties (:server (:type "string"
                                    :description "MCP server name")
                           :uri (:type "string"
                                 :description "Resource URI"))
              :required ["server" "uri"])
    :fn #'harmless-mcp-read-resource)))

(harmless-mcp-register)

(provide 'harmless-mcp)

;;; harmless-mcp.el ends here
