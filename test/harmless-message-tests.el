;;; harmless-message-tests.el --- Tests for live session findings -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless-provider)
(require 'harmless-session)
(require 'harmless-tools)
(require 'harmless-perm)
(require 'harmless-turn)
(require 'harmless-agent)
(require 'harmless-message)
(require 'harmless-ui)
(require 'harmless-fork)
(require 'harmless-openai)

(cl-defstruct (harmless-message-fake
               (:include harmless-provider)
               (:constructor harmless-message-make-fake))
  script)

(cl-defmethod harmless-provider-complete ((provider harmless-message-fake)
                                          _messages _tools callback)
  (when (harmless-message-fake-script provider)
    (funcall (harmless-message-fake-script provider) _messages callback))
  nil)

(defmacro harmless-message-test-with (data &rest body)
  "Bind a temp data directory DATA, then run BODY."
  (declare (indent 1))
  `(let* ((,data (make-temp-file "harmless-msg-data-" t))
          (harmless-directory ,data)
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-perm-ask-function
          (lambda (_session _class _item callback)
            (funcall callback 'allow))))
     (unwind-protect
         (progn ,@body)
       (maphash
        (lambda (_id session)
          (dolist (buf (list (harmless-session-buffer session)
                             (harmless-session-prompt-buffer session)))
            (when (buffer-live-p buf)
              (kill-buffer buf))))
        harmless--sessions))))

(defun harmless-message-test-session (&rest args)
  "Return a session.  ARGS are passed to `harmless-session-new'."
  (apply #'harmless-session-new
         :provider (harmless-message-make-fake :name "fake" :host "none")
         :model "m"
         args))

(defun harmless-message-test-send (session id text)
  "Return the tool result of sending TEXT from SESSION to ID."
  (harmless-tool-call (harmless-tool-by-name "harmless-tool-send-message")
                      session
                      (list :session id :text text)))

(defun harmless-message-test-file (session name)
  "Return NAME inside SESSION's directory."
  (expand-file-name name (harmless-session-directory session)))

(defun harmless-message-test-raw (session name)
  "Return the contents of NAME in SESSION's directory."
  (with-temp-buffer
    (insert-file-contents (harmless-message-test-file session name))
    (buffer-string)))

(defun harmless-message-test-stored (session)
  "Return SESSION's transcript as stored in messages.jsonl."
  (let ((raw (harmless-message-test-raw session "messages.jsonl"))
        (acc nil))
    (dolist (line (split-string raw "\n" t))
      (push (harmless-session--normalize-message
             (harmless-json-decode (string-trim line)))
            acc))
    (nreverse acc)))

(defun harmless-message-test-not-root (session)
  "Require SESSION's files to stay off the filesystem root."
  (let* ((dir (harmless-session-directory session))
         (file (harmless-message-test-file session "messages.jsonl")))
    (should (file-name-absolute-p dir))
    (should (file-name-absolute-p file))
    (should-not (harmless-filesystem-root-p dir))
    (should-not (harmless-filesystem-root-p (file-name-directory file)))
    (should (harmless-directory-strictly-under-p dir harmless-directory))
    (should (file-exists-p file))
    (should-not (equal (file-truename file)
                       (file-truename
                        (expand-file-name "messages.jsonl" "/"))))))

(ert-deftest harmless-message-refuses-a-bad-request ()
  (harmless-message-test-with data
    (let* ((repo (make-temp-file "harmless-msg-repo-" t))
           (sender (harmless-message-test-session
                    :cwd repo :id "sender" :title "Keep"))
           (root-before (file-exists-p "/messages.jsonl")))
      (dolist (id (list nil "" "   " "\n"))
        (should (equal (harmless-message-test-send sender id "hello")
                       "Error: session id is empty")))
      (dolist (id (list 42 :false))
        (should (equal (harmless-message-test-send sender id "hello")
                       "Error: session id is not an id")))
      (dolist (text (list nil :false 42))
        (should (equal (harmless-message-test-send sender "sender" text)
                       "Error: message must be a string")))
      (dolist (text (list "" "  " "\n\t"))
        (should (equal (harmless-message-test-send sender "other" text)
                       "Error: message is empty")))
      (should (equal (harmless-message-test-send sender "sender" "hello")
                     "Error: a session cannot message itself"))
      (dolist (id (list "sub" "sub/" "/" "c:/" "../x" "a\\b" "a/b"
                        "." ".."))
        (should (equal (harmless-message-test-send sender id "hello")
                       (format "Error: no live session %s" id))))
      (should (equal (harmless-message-test-raw sender "messages.jsonl") ""))
      (harmless-message-test-not-root sender)
      (should (eq root-before (file-exists-p "/messages.jsonl")))
      (should-not (file-exists-p (expand-file-name "sub" data)))
      (should-not (file-exists-p (expand-file-name "sub" repo)))
      (let ((dir (harmless-session-directory sender)))
        (harmless-session-unregister sender)
        (should (equal (harmless-message-test-send
                        (harmless-message-test-session
                         :cwd repo :id "other" :title "Other")
                        "sender" "hello")
                       "Error: no live session sender"))
        (should (equal (with-temp-buffer
                         (insert-file-contents
                          (expand-file-name "messages.jsonl" dir))
                         (buffer-string))
                       ""))))))

(ert-deftest harmless-message-delivers-to-an-idle-session ()
  (harmless-message-test-with _data
    (let* ((repo (make-temp-file "harmless-msg-repo-" t))
           (calls 0)
           (sender (harmless-message-test-session
                    :cwd repo :id "sender" :title "A\nB"))
           (target (harmless-message-test-session
                    :cwd repo :id "target" :title "Keep me"))
           (plain (harmless-message-test-session :cwd repo :id "plain"))
           (expected "Finding from sender (A B):\nshipped")
           (second "Finding from sender (A B):\nnext"))
      (setf (harmless-message-fake-script
             (harmless-session-provider sender))
            (lambda (&rest _) (setq calls (1+ calls))))
      (should (equal (harmless-message-test-send sender "  target  " " shipped ")
                     "Delivered to target"))
      (should (equal (harmless-message-test-send sender "target" "next")
                     "Delivered to target"))
      (should (= calls 0))
      (should (eq (harmless-session-status target) 'idle))
      (should (equal (harmless-session-title target) "Keep me"))
      (should-not (harmless-session-title plain))
      (should (equal (harmless-message-test-send sender "plain" "note")
                     "Delivered to plain"))
      (should-not (harmless-session-title plain))
      (should-not (harmless-message-user-p
                   (car (harmless-session-messages plain))))
      (let ((stored (harmless-message-test-stored target)))
        (should (equal (mapcar (lambda (msg) (plist-get msg :content)) stored)
                       (list expected second)))
        (should (equal (mapcar (lambda (msg) (plist-get msg :role)) stored)
                       '(:finding :finding)))
        (should (equal (mapcar (lambda (msg) (plist-get msg :from)) stored)
                       '("sender" "sender"))))
      (should (equal (plist-get (car (harmless-session-messages plain)) :content)
                     "Finding from sender (A B):\nnote"))
      (should (string-search "Finding from sender (A B):\\nshipped"
                             (harmless-message-test-raw
                              target "messages.jsonl")))
      (harmless-message-test-not-root target)
      (harmless-message-test-not-root sender)
      (let ((summary (harmless-json-decode
                      (harmless-message-test-raw target "summary.json"))))
        (should-not (plist-member summary :findings))
        (should-not (plist-member summary :queue)))
      (let ((loaded (harmless-session-load
                     (harmless-session-directory target))))
        (should (equal (plist-get (car (harmless-session-messages loaded))
                                  :content)
                       expected))
        (should (eq (plist-get (car (harmless-session-messages loaded)) :role)
                    :finding)))
      (should (equal (harmless-openai--format-message
                      (car (harmless-session-messages target)))
                     (list :role "user" :content expected)))
      (with-current-buffer (harmless-session-buffer target)
        (should (string-search "Finding" (buffer-string)))
        (should (string-search expected (buffer-string))))
      (should (equal (harmless-ui--tool-verb "harmless-tool-send-message") "message"))
      (should (equal (harmless-ui--tool-verb "harmless-tool-list-sessions") "sessions"))
      (should (equal (harmless-ui--tool-summary "harmless-tool-send-message" '(:session "target"))
                     "target"))
      (should (equal (harmless-message-test-send plain "target" "from plain")
                     "Delivered to target"))
      (should (equal (plist-get
                      (car (last (harmless-session-messages
                                  (harmless-session-get "target"))))
                      :content)
                     "Finding from plain (untitled):\nfrom plain")))))

(ert-deftest harmless-message-queues-until-the-turn-is-safe ()
  (harmless-message-test-with _data
    (let* ((repo (make-temp-file "harmless-msg-repo-" t))
           (sender (harmless-message-test-session
                    :cwd repo :id "sender" :title "Keep"))
           (target (harmless-message-test-session
                    :cwd repo :id "target" :title "Peer"))
           (first "Finding from sender (Keep):\none")
           (second "Finding from sender (Keep):\ntwo"))
      (setf (harmless-session-status target) 'streaming)
      (should (equal (harmless-message-test-send sender "target" "one")
                     "Queued for target"))
      (setf (harmless-session-status target) 'waiting-permission)
      (should (equal (harmless-message-test-send sender "target" "two")
                     "Queued for target"))
      (should (eq (harmless-session-status target) 'waiting-permission))
      (should-not (harmless-session-messages target))
      (should (equal (mapcar (lambda (msg) (plist-get msg :content))
                             (harmless-session-findings target))
                     (list first second)))
      (should-not (string-search "one"
                                 (harmless-message-test-raw
                                  target "messages.jsonl")))
      (should-not (string-search "one"
                                 (harmless-message-test-raw
                                  target "summary.json")))
      (harmless-message-flush target)
      (should-not (harmless-session-findings target))
      (should (eq (harmless-session-status target) 'waiting-permission))
      (should (equal (mapcar (lambda (msg) (plist-get msg :content))
                             (harmless-session-messages target))
                     (list first second)))
      (setf (harmless-session-status target) 'streaming)
      (setf (harmless-session-steers target) '("later"))
      (should (equal (harmless-message-test-send sender "target" "three")
                     "Queued for target"))
      (harmless-turn-abort target)
      (should-not (harmless-session-steers target))
      (should-not (harmless-session-findings target))
      (should (eq (harmless-session-status target) 'idle))
      (should (equal (plist-get (car (last (harmless-session-messages target)))
                                :content)
                     "Finding from sender (Keep):\nthree"))
      (harmless-session-append-user sender "hello")
      (setf (harmless-session-findings sender)
            (list (list :role :finding :content "pending" :from "x")))
      (let* ((kept (harmless-fork-split
                    (harmless-session-messages sender) 1))
             (child (harmless-fork--create sender kept)))
          (should-not (harmless-session-findings child))
          (should (equal (harmless-session-findings sender)
                         (list (list :role :finding
                                     :content "pending"
                                     :from "x"))))
          (harmless-message-test-not-root child)))))

(ert-deftest harmless-message-lists-live-sessions ()
  (harmless-message-test-with _data
    (should (equal (harmless-message-list-text) "Live sessions:"))
    (should (equal (harmless-tool-call (harmless-tool-by-name "harmless-tool-list-sessions")
                                      nil nil)
                   "Live sessions:"))
    (let ((older (harmless-message-test-session
                  :detached t :id "older"))
          (newer (harmless-message-test-session
                  :detached t :id "newer" :title "A\nB")))
      (setf (harmless-session-updated-at older) "2020-01-01T00:00:00Z")
      (setf (harmless-session-updated-at newer) "2020-01-02T00:00:00Z")
      (setf (harmless-session-status newer) 'streaming)
      (should (equal (harmless-message-list-text)
                     "Live sessions:\nnewer streaming A B\nolder idle"))
      (should (harmless-session-detached-p older))
      (should (equal (harmless-message-test-send older "newer" "ping")
                     "Queued for newer"))
      (harmless-message-test-not-root older)
      (harmless-message-test-not-root newer)
      (should (string-search "/_detached/"
                             (harmless-session-directory older)))
      (should-not (file-exists-p "/_detached")))))

(ert-deftest harmless-message-permission-and-plan-mode ()
  (harmless-message-test-with _data
    (let* ((repo (make-temp-file "harmless-msg-repo-" t))
           (calls 0)
           (asked nil)
           (peer (harmless-message-test-session
                  :cwd repo :id "peer" :title "Peer"))
           (session
            (harmless-session-new
             :cwd repo
             :id "sender"
             :title "Keep"
             :model "m"
             :provider
             (harmless-message-make-fake
              :name "fake" :host "none"
              :script
              (lambda (_messages callback)
                (setq calls (1+ calls))
                (if (= calls 1)
                    (progn
                      (funcall callback
                               '(:tool-call "1" "harmless-tool-send-message"
                                 "{\"session\":\"peer\",\"text\":\"found it\"}"))
                      (funcall callback '(:stop "tool_calls")))
                  (funcall callback '(:text "done"))
                  (funcall callback '(:stop "stop"))))))))
      (setq harmless-perm-ask-function
            (lambda (_session class item callback)
              (push (list class item) asked)
              (funcall callback 'deny)))
      (harmless-turn-run session "go")
      (should (equal asked '((edit "harmless-tool-send-message"))))
      (should (equal (cl-loop for msg in (harmless-session-messages session)
                              when (eq (plist-get msg :role) :tool)
                              collect (plist-get msg :content))
                     '("Error: user denied this tool call")))
      (should-not (harmless-session-messages peer))
      (should (equal (harmless-message-test-raw peer "messages.jsonl") ""))
      (should (= calls 2))
      (setq calls 0)
      (setq asked nil)
      (setq session
            (harmless-session-new
             :cwd repo
             :id "sender-2"
             :title "Keep"
             :model "m"
             :permission-mode 'accept-edits
             :provider
             (harmless-message-make-fake
              :name "fake" :host "none"
              :script
              (lambda (_messages callback)
                (setq calls (1+ calls))
                (if (= calls 1)
                    (progn
                      (funcall callback
                               '(:tool-call "1" "harmless-tool-send-message"
                                 "{\"session\":\"peer\",\"text\":\"found it\"}"))
                      (funcall callback '(:stop "tool_calls")))
                  (funcall callback '(:text "done"))
                  (funcall callback '(:stop "stop")))))))
      (setf (harmless-session-plan-mode session) t)
      (setq harmless-perm-ask-function
            (lambda (&rest _) (error "permission asked")))
      (harmless-turn-run session "go")
      (should (= calls 2))
      (should-not asked)
      (should (equal (cl-loop for msg in (harmless-session-messages session)
                              when (eq (plist-get msg :role) :tool)
                              collect (plist-get msg :content))
                     '("Delivered to peer")))
      (should (equal (plist-get (car (harmless-session-messages peer)) :content)
                     "Finding from sender-2 (Keep):\nfound it"))
      (should (eq (harmless-session-status peer) 'idle))
      (should (equal (harmless-session-title peer) "Peer"))
      (harmless-message-test-not-root peer))))

(ert-deftest harmless-message-read-only-may-send ()
  (harmless-message-test-with _data
    (let* ((repo (make-temp-file "harmless-msg-repo-" t))
           (peer (harmless-message-test-session
                  :cwd repo :id "peer" :title "Peer"))
           (child (harmless-message-test-session
                   :cwd repo :id "child" :title "Reader" :source 'agent
                   :parent-id "parent")))
      (setf (harmless-session-agent child)
            (list :allow 'all :sandbox 'read-only :callback nil
                  :turns 0 :max 8 :error nil :prompt nil :task nil))
      (let ((names (mapcar #'harmless-tool-name
                           (harmless-agent-filter-tools
                            child (harmless-tools-enabled)))))
        (should (member "harmless-tool-send-message" names))
        (should (member "harmless-tool-list-sessions" names))
        (should-not (member "harmless-tool-write-file" names))
        (should-not (member "harmless-tool-spawn-agent" names)))
      (should-not (harmless-agent-refuse-call
                   child (harmless-tool-by-name "harmless-tool-send-message")
                   '(:session "peer" :text "look")))
      (should (equal (harmless-agent-refuse-call
                      child (harmless-tool-by-name "harmless-tool-write-file")
                      '(:path "x" :contents "y"))
                     "Error: This subagent is read-only. A tool that can write is refused"))
      (setf (plist-get (harmless-session-agent child) :allow)
            '("harmless-tool-read-file"))
      (should (equal (harmless-agent-refuse-call
                      child (harmless-tool-by-name "harmless-tool-send-message")
                      '(:session "peer" :text "look"))
                     "Error: this subagent cannot use harmless-tool-send-message"))
      (setf (plist-get (harmless-session-agent child) :allow) 'all)
      (should (equal (harmless-message-test-send child "peer" "look")
                     "Delivered to peer"))
      (should (equal (plist-get (car (harmless-session-messages peer)) :content)
                     "Finding from child (Reader):\nlook"))
      (should (eq (harmless-session-status peer) 'idle))
      (harmless-message-test-not-root peer)
      (should-not (file-exists-p (expand-file-name "x" repo))))))
