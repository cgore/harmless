;;; harmless-compact-tests.el --- Tests for transcript compact -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(cl-defstruct (harmless-compact-fake
               (:include harmless-provider)
               (:constructor harmless-compact-make-fake))
  script)

(cl-defmethod harmless-provider-complete ((provider harmless-compact-fake)
                                          messages tools callback)
  ;; Return the script value.  A synchronous compact can name one
  ;; process and the following turn another.  The session must keep
  ;; the turn's name.
  (funcall (harmless-compact-fake-script provider) messages tools callback))

(defvar harmless-compact-test-requests nil)
(defvar harmless-compact-test-live nil)
(defvar harmless-compact-test-reply nil)
(defvar harmless-compact-test-script nil)
(defvar harmless-compact-test-callback nil)

(defun harmless-compact-test-error (text thunk)
  "Assert that THUNK signals an error whose message is TEXT."
  (let ((err (should-error (funcall thunk) :type 'error)))
    (should (equal (error-message-string err) text))))

(defun harmless-compact-test-prepare ()
  "Reset the fake provider and install a summary reply."
  (setq harmless-compact-test-requests nil
        harmless-compact-test-callback nil
        harmless-compact-test-reply nil
        harmless-compact-test-script
        (lambda (_messages _tools callback)
          (funcall callback '(:usage 9 3))
          (funcall callback
                   (list :text (or harmless-compact-test-reply "  SUM-ONE\n")))
          (funcall callback '(:stop "stop")))))

(defun harmless-compact-test-provider ()
  "Return a fake provider that records the compact request."
  (harmless-compact-make-fake
   :name "fake"
   :host "none"
   :script
   (lambda (messages tools callback)
     (push (list messages tools
                 harmless-current-model
                 harmless-current-reasoning-effort
                 (and harmless-compact-test-live
                      (harmless-session-status harmless-compact-test-live)))
           harmless-compact-test-requests)
     (funcall harmless-compact-test-script messages tools callback))))

(defun harmless-compact-test-make-session (&rest args)
  "Create a session on the compact fake provider.
ARGS are passed to `harmless-session-new'."
  (let ((provider (harmless-compact-test-provider)))
    (setq harmless-providers (list provider))
    (setq harmless-compact-test-live
          (apply #'harmless-session-new
                 :provider provider
                 :model "grok-4.6"
                 :reasoning-effort "high"
                 args))
    harmless-compact-test-live))

(defun harmless-compact-test-turns (texts)
  "Return a user message and an assistant reply for each string in TEXTS."
  (cl-mapcan (lambda (text)
               (list (list :role :user :content text)
                     (list :role :assistant
                           :content (concat text "-reply"))))
             texts))

(defun harmless-compact-test-roles (session)
  "Return the role of each message in SESSION."
  (mapcar (lambda (msg) (plist-get msg :role))
          (harmless-session-messages session)))

(defun harmless-compact-test-absent (session text)
  "Assert TEXT appears in no message stored on SESSION."
  (dolist (msg (harmless-session-messages session))
    (should-not (string-search text (or (plist-get msg :content) "")))
    (let ((reasoning (plist-get msg :reasoning))
          (calls (plist-get msg :tool-calls)))
      (when reasoning
        (should-not (string-search text reasoning)))
      (when calls
        (should-not (string-search text (format "%S" calls)))))))

(defun harmless-compact-test-transcript (session)
  "Return the saved messages file for SESSION."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "messages.jsonl" (harmless-session-dir session)))
    (buffer-string)))

(defun harmless-compact-test-assert-stored (session data-dir root-messages root-summary)
  "Assert SESSION is stored under DATA-DIR and not at the filesystem root.
ROOT-MESSAGES and ROOT-SUMMARY are the prior existence of the root files."
  (let* ((dir (harmless-session-dir session))
         (file (expand-file-name "messages.jsonl" dir)))
    (should (string-prefix-p (file-name-as-directory data-dir) dir))
    (should (file-exists-p file))
    (should-not (equal file "/messages.jsonl"))
    (should-not (harmless-filesystem-root-p dir))
    (should (eq root-messages (file-exists-p "/messages.jsonl")))
    (should (eq root-summary (file-exists-p "/summary.json")))))

(defmacro harmless-compact-test-env (&rest body)
  "Run BODY with a temp Harmless directory and a fresh session table."
  (declare (indent 0))
  `(let* ((root (make-temp-file "harmless-compact-" t))
          (harmless-directory (expand-file-name ".harmless" root))
          (harmless--sessions (make-hash-table :test 'equal))
          (harmless-providers nil)
          (harmless-compact-test-requests nil)
          (harmless-compact-test-live nil)
          (harmless-compact-test-reply nil)
          (harmless-compact-test-script nil)
          (harmless-compact-test-callback nil))
     (harmless-compact-test-prepare)
     ,@body))

(defun harmless-compact-test-request-text (record)
  "Return the user text sent in recorded provider call RECORD."
  (plist-get (cadr (car record)) :content))

(defun harmless-compact-test-record-text (record)
  "Return every message content in recorded provider call RECORD."
  (mapconcat (lambda (msg) (or (plist-get msg :content) ""))
             (car record) "\n"))

(defun harmless-compact-test-auto-session (cwd tokens texts)
  "Return a saved session at CWD whose last prompt used TOKENS.
TEXTS is a list of user-turn strings."
  (let ((session (harmless-compact-test-make-session :cwd cwd)))
    (setf (harmless-session-messages session)
          (harmless-compact-test-turns texts)
          (harmless-session-last-prompt-tokens session) tokens)
    (harmless-session-save session)
    session))

(defun harmless-compact-test-auto-script ()
  "Return a script that summarizes a tools-nil call and answers a turn.
The compact reply reports a full window, so a second compact in the
same turn shows up as another tools-nil request.  The return value is
the process name for that call."
  (lambda (_messages tools callback)
    (funcall callback (list :usage (if (null tools) 400000 11) 3))
    (funcall callback (list :text (if (null tools) "SUM-AUTO" "TURN-OK")))
    (funcall callback '(:stop "stop"))
    (if (null tools) "compact-proc" "turn-proc")))

(defmacro harmless-compact-test-at-window (&rest body)
  "Run BODY with fixed context windows and the default threshold."
  (declare (indent 0))
  `(let ((harmless-usage-context-windows
          '(("grok-4.6" . 500000)
            ("grok-4.3" . 1000000)))
         (harmless-openai--context-windows nil)
         (harmless-compact-threshold 80)
         (harmless-compact-keep-turns 2))
     ,@body))

(ert-deftest harmless-compact-split-keeps-a-tool-result-with-its-turn ()
  (let* ((old (list :role "user" :content "OLD"))
         (call (list :role :assistant :content "looking"
                     :tool-calls (list (list :id "1" :name "read_file"
                                             :args '(:path "old.txt")))))
         (tool (list :role :tool :id "1" :name "read_file" :content "old body"))
         (new (list :role 'user :content "NEW"))
         (result (list :role 'tool :id "2" :name "read_file" :content "new body"))
         (parts (harmless-compact-split (list old call tool new result) 1)))
    (should (eq old (nth 0 (car parts))))
    (should (eq call (nth 1 (car parts))))
    (should (eq tool (nth 2 (car parts))))
    (should (eq new (nth 0 (cdr parts))))
    (should (eq result (nth 1 (cdr parts))))
    (harmless-compact-test-error
     "Nothing to compact"
     (lambda ()
       (harmless-compact-split
        (list (list :role :summary :content "S") new)
        1)))
    (harmless-compact-test-error
     "Keep at least one turn"
     (lambda () (harmless-compact-split (list old new) 0)))
    (harmless-compact-test-error
     "Nothing to compact"
     (lambda () (harmless-compact-split nil nil)))
    (harmless-compact-test-error
     "Nothing to compact"
     (lambda () (harmless-compact-split '() 1)))
    (harmless-compact-test-error
     "Nothing to compact"
     (lambda () (harmless-compact-split "" 1)))
    (harmless-compact-test-error
     "Nothing to compact"
     (lambda () (harmless-compact-split (vector old) 1)))))

(ert-deftest harmless-compact-summary-is-sent-as-a-user-message ()
  (should (equal (harmless-openai--format-message
                  '(:role :summary :content "SUM"))
                 '(:role "user" :content "SUM")))
  (should (equal (harmless-anthropic--format-one
                  '(:role :summary :content "SUM"))
                 '(:role "user" :content "SUM")))
  (should (equal (harmless-openai--responses-input
                  '((:role :summary :content "SUM")))
                 '((:role "user" :content "SUM"))))
  (should (equal (harmless-anthropic--format-messages
                  '((:role :summary :content "SUM")
                    (:role :user :content "LATEST")))
                 '((:role "user" :content "SUM\n\nLATEST")))))

(ert-deftest harmless-compact-key-and-menu ()
  (should (eq (lookup-key harmless-session-mode-map (kbd "k"))
              #'harmless-compact))
  (should-not (eq (lookup-key harmless-prompt-mode-map (kbd "k"))
                  #'harmless-compact))
  (should (equal (transient--suffix-key
                  (transient-get-suffix 'harmless-menu 'harmless-compact))
                 "k"))
  (should (= 2 harmless-compact-keep-turns))
  (should (= 80 harmless-compact-threshold))
  (should (equal harmless-compact-instructions
                 "Summarize the conversation below for a later turn of the same coding session. Include the goal, decisions, files changed, commands run, and errors still open. Omit greetings and unchanged file dumps. Write only the summary."))
  (should (equal harmless-compact-summary-prefix
                 "Summary of the earlier conversation:\n\n")))

(ert-deftest harmless-compact-replaces-older-turns ()
  (harmless-compact-test-env
    (let* ((root-messages (file-exists-p "/messages.jsonl"))
           (root-summary (file-exists-p "/summary.json"))
           (session (harmless-compact-test-make-session :cwd root))
           (old-user (list :role :user :content "OLD-GOAL"))
           (old-asst (list :role :assistant :content "looking"
                           :reasoning "because old"
                           :tool-calls
                           (list (list :id "c1" :name "read_file"
                                       :args '(:path "old.txt"))
                                 (list :id "c2" :name "echo"
                                       :args "STRING-ARGS")
                                 (list :id "c3" :name "bare_tool"
                                       :args nil))))
           (old-tool (list :role :tool :id "c1" :name "read_file"
                           :content "old body"))
           (mid-user (list :role :user :content "MID"))
           (mid-asst (list :role :assistant :content "mid reply"))
           (late-user (list :role :user :content "LATEST"))
           (late-asst (list :role :assistant :content "late reply"
                            :tool-calls
                            (list (list :id "k1" :name "read_file"
                                        :args '(:path "kept.txt")))))
           (late-tool (list :role :tool :id "k1" :name "read_file"
                            :content "kept body"))
           (notes nil)
           (result nil)
           (first nil)
           (loaded nil))
      (with-temp-file (expand-file-name "HARMLESS.md" root)
        (insert "DO-NOT-SEND-INSTRUCTIONS\n"))
      (setf (harmless-session-plan-mode session) t)
      (setf (harmless-session-prompt-tokens session) 100)
      (setf (harmless-session-messages session)
            (list old-user old-asst old-tool mid-user mid-asst
                  late-user late-asst late-tool))
      (harmless-ui-ensure-session-buffer session)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) notes))))
        (setq result (harmless-compact-session session))
        (should (= 109 (harmless-session-prompt-tokens session)))
        (should (= 9 (harmless-session-last-prompt-tokens session)))
        (should (= 3 (harmless-session-completion-tokens session)))
        (should (= 3 (harmless-session-last-completion-tokens session)))
        (setq first (car harmless-compact-test-requests))
        (setq loaded (harmless-session-load (harmless-session-dir session)))
        (setq harmless-compact-test-reply "  SUM-TWO\n")
        (harmless-session-append session (list :role :user :content "NEWER"))
        (harmless-session-append
         session (list :role :assistant :content "newer reply"))
        (harmless-compact-session session))
      (let ((request (harmless-compact-test-request-text first))
            (msgs (harmless-session-messages session))
            (file (harmless-compact-test-transcript session)))
        (should (equal result
                       "Summary of the earlier conversation:\n\nSUM-ONE"))
        (should (null (nth 1 first)))
        (should (equal "grok-4.6" (nth 2 first)))
        (should (equal "high" (nth 3 first)))
        (should (eq 'streaming (nth 4 first)))
        (should (eq :system (plist-get (car (car first)) :role)))
        (should (equal harmless-compact-instructions
                       (plist-get (car (car first)) :content)))
        (should (null (nthcdr 2 (car first))))
        (should (string-search "OLD-GOAL" request))
        (should (string-search "old.txt" request))
        (should (string-search "STRING-ARGS" request))
        (should (string-search "Tool call bare_tool: " request))
        (should (string-search "Reasoning:\nbecause old\nlooking" request))
        (should (string-search "old body" request))
        (should-not (string-search "MID" request))
        (should-not (string-search "LATEST" request))
        (should-not (string-search "kept.txt" request))
        (should-not (string-search "kept body" request))
        (should-not (string-search "DO-NOT-SEND-INSTRUCTIONS" request))
        (should-not (string-search "Plan mode is active" request))
        (should (eq 'idle (harmless-session-status session)))
        (should (eq t (harmless-session-plan-mode session)))
        (should (= 118 (harmless-session-prompt-tokens session)))
        (should (= 9 (harmless-session-last-prompt-tokens session)))
        (should (= 6 (harmless-session-completion-tokens session)))
        (should (= 3 (harmless-session-last-completion-tokens session)))
        (should (equal '("Compacted 1 turn" "Compacted 1 turn")
                       (nreverse notes)))
        (should (equal '(:summary :user :assistant :tool :user :assistant)
                       (harmless-compact-test-roles session)))
        (should (equal "Summary of the earlier conversation:\n\nSUM-TWO"
                       (plist-get (car msgs) :content)))
        (should (eq late-user (nth 1 msgs)))
        (should (eq late-asst (nth 2 msgs)))
        (should (eq late-tool (nth 3 msgs)))
        (should (equal "NEWER" (plist-get (nth 4 msgs) :content)))
        (should (equal "newer reply" (plist-get (nth 5 msgs) :content)))
        (should (= 1 (cl-count :summary msgs
                               :key (lambda (msg) (plist-get msg :role)))))
        (harmless-compact-test-absent session "OLD-GOAL")
        (harmless-compact-test-absent session "MID")
        (harmless-compact-test-absent session "SUM-ONE")
        (harmless-compact-test-absent session "because old")
        (harmless-compact-test-absent session "old.txt")
        (let ((second (car harmless-compact-test-requests))
              (again (harmless-compact-test-request-text
                      (car harmless-compact-test-requests))))
          (should (null (nth 1 second)))
          (should (string-search "Summary of the earlier conversation:" again))
          (should (string-search "SUM-ONE" again))
          (should (string-search "MID" again))
          (should-not (string-search "NEWER" again))
          (should-not (string-search "LATEST" again))
          (should-not (string-search "DO-NOT-SEND-INSTRUCTIONS" again)))
        (should (equal '(:summary :user :assistant :user :assistant :tool)
                       (harmless-compact-test-roles loaded)))
        (should (equal "Summary of the earlier conversation:\n\nSUM-ONE"
                       (plist-get (car (harmless-session-messages loaded))
                                  :content)))
        (should (equal "MID"
                       (plist-get (nth 1 (harmless-session-messages loaded))
                                  :content)))
        (should (equal "LATEST"
                       (plist-get (nth 3 (harmless-session-messages loaded))
                                  :content)))
        (should (equal "kept body"
                       (plist-get (nth 5 (harmless-session-messages loaded))
                                  :content)))
        (should (eq t (harmless-session-plan-mode loaded)))
        (should (string-search "Summary of the earlier conversation:" file))
        (should (string-search "SUM-TWO" file))
        (should (string-search "LATEST" file))
        (should (string-search "NEWER" file))
        (should-not (string-search "OLD-GOAL" file))
        (should-not (string-search "MID" file))
        (harmless-compact-test-assert-stored
         session harmless-directory root-messages root-summary)
        (should-not (file-exists-p
                     (expand-file-name "messages.jsonl" root)))
        (let ((text (with-current-buffer (harmless-session-buffer session)
                      (buffer-string))))
          (should (string-search "Summary" text))
          (should (string-search "SUM-TWO" text))
          (should (string-search "LATEST" text))
          (should (string-search "NEWER" text))
          (should-not (string-search "OLD-GOAL" text))
          (should-not (string-search "MID" text))
          (should-not (string-search "Error:" text)))))))

(ert-deftest harmless-compact-keep-one-turn ()
  (harmless-compact-test-env
    (let* ((session (harmless-compact-test-make-session :cwd root))
           (messages (harmless-compact-test-turns '("A" "B" "C")))
           (notes nil)
           (last-user (nth 4 messages)))
      (setf (harmless-session-messages session) messages)
      (setf (harmless-session-status session) 'error)
      (should (null (harmless-session-buffer session)))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) notes))))
        (should (equal "Summary of the earlier conversation:\n\nSUM-ONE"
                       (harmless-compact-session session 1))))
      (should (eq 'idle (harmless-session-status session)))
      (should (null (harmless-session-buffer session)))
      (should (equal '("Compacted 2 turns") notes))
      (should (eq last-user (nth 1 (harmless-session-messages session))))
      (should (equal '("Summary of the earlier conversation:\n\nSUM-ONE"
                       "C" "C-reply")
                     (mapcar (lambda (msg) (plist-get msg :content))
                             (harmless-session-messages session))))
      (harmless-compact-test-absent session "A")
      (harmless-compact-test-absent session "B"))))

(ert-deftest harmless-compact-refuses-an-empty-transcript ()
  (harmless-compact-test-env
    (let ((session (harmless-compact-test-make-session :cwd root)))
      (dolist (messages (list nil '() "" (vector (list :role :user :content "A"))
                              (harmless-compact-test-turns '("A" "B"))
                              (cons (list :role :summary :content "S")
                                    (harmless-compact-test-turns '("A" "B")))
                              (list (list :role :summary :content "ONLY"))
                              (list (list :role :tool :id "1" :name "read_file"
                                          :content "x")
                                    (list :role :user :content "ONLY"))))
        (setf (harmless-session-messages session) messages)
        (setf (harmless-session-status session) 'idle)
        (harmless-compact-test-error
         "Nothing to compact"
         (lambda () (harmless-compact-session session)))
        (should (eq 'idle (harmless-session-status session)))
        (should (eq messages (harmless-session-messages session))))
      (should (null harmless-compact-test-requests))
      (setf (harmless-session-messages session)
            (harmless-compact-test-turns '("A" "B" "C")))
      (harmless-compact-test-error
       "Nothing to compact"
       (lambda () (harmless-compact-session session 3)))
      (harmless-compact-test-error
       "Nothing to compact"
       (lambda () (harmless-compact-session session 9)))
      (should (null harmless-compact-test-requests)))))

(ert-deftest harmless-compact-requires-a-positive-keep ()
  (harmless-compact-test-env
    (let ((session (harmless-compact-test-make-session :cwd root)))
      (setf (harmless-session-messages session) "")
      (harmless-compact-test-error
       "Keep at least one turn"
       (lambda () (harmless-compact-session session 0)))
      (setf (harmless-session-messages session)
            (harmless-compact-test-turns '("A" "B" "C")))
      (dolist (keep '("" 0 -1 t 1.5))
        (harmless-compact-test-error
         "Keep at least one turn"
         (lambda () (harmless-compact-session session keep))))
      (should (null harmless-compact-test-requests))
      (should (eq 'idle (harmless-session-status session))))))

(ert-deftest harmless-compact-refuses-a-busy-session ()
  (harmless-compact-test-env
    (let ((session (harmless-compact-test-make-session :cwd root)))
      (dolist (status '(streaming waiting-permission))
        (setf (harmless-session-messages session) nil)
        (setf (harmless-session-status session) status)
        (harmless-compact-test-error
         "A turn is in progress"
         (lambda () (harmless-compact-session session)))
        (should (eq status (harmless-session-status session)))
        (should (null (harmless-session-messages session)))
        (setf (harmless-session-messages session)
              (harmless-compact-test-turns '("A" "B" "C")))
        (harmless-compact-test-error
         "A turn is in progress"
         (lambda () (harmless-compact-session session)))
        (should (eq status (harmless-session-status session)))
        (should (equal '("A" "A-reply" "B" "B-reply" "C" "C-reply")
                       (mapcar (lambda (msg) (plist-get msg :content))
                               (harmless-session-messages session)))))
      (should (null harmless-compact-test-requests)))))

(ert-deftest harmless-compact-empty-summary-keeps-the-transcript ()
  (harmless-compact-test-env
    (let* ((session (harmless-compact-test-make-session :cwd root))
           (messages (harmless-compact-test-turns '("OLD-GOAL" "LATEST")))
           (events nil))
      (setf (harmless-session-messages session) messages)
      (harmless-session-save session)
      (harmless-ui-ensure-session-buffer session)
      (harmless-ui-render-session session)
      (let ((harmless-event-functions
             (cons (lambda (_session event) (push event events))
                   harmless-event-functions)))
        (dolist (payload '(nil "  " " \n "))
          (setq harmless-compact-test-script
                (lambda (_messages _tools callback)
                  (unless (null payload)
                    (funcall callback '(:usage 9 3))
                    (funcall callback (list :text payload)))
                  (funcall callback '(:stop "stop"))))
          (setf (harmless-session-prompt-tokens session) 100)
          (setf (harmless-session-completion-tokens session) 50)
          (should (null (harmless-compact-session session 1)))
          (should (eq messages (harmless-session-messages session)))
          (should (eq 'error (harmless-session-status session)))
          (should (member '(:error "The model returned an empty summary.")
                          events))
          (if (null payload)
              (progn
                (should (= 100 (harmless-session-prompt-tokens session)))
                (should (= 50 (harmless-session-completion-tokens session)))
                (should (= 0 (harmless-session-last-prompt-tokens session))))
            (should (= 109 (harmless-session-prompt-tokens session)))
            (should (= 53 (harmless-session-completion-tokens session)))
            (should (= 9 (harmless-session-last-prompt-tokens session)))
            (should (= 3 (harmless-session-last-completion-tokens session))))))
      (should (string-search "OLD-GOAL" (harmless-compact-test-transcript session)))
      (should-not (string-search "Summary of the earlier conversation:"
                                 (harmless-compact-test-transcript session)))
      (with-current-buffer (harmless-session-buffer session)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "MARKER-KEEP\n")))
      (setq harmless-compact-test-script
            (lambda (_messages _tools callback)
              (funcall callback '(:text " \n "))
              (funcall callback '(:stop "stop"))))
      (harmless-compact-session session 1)
      (let ((text (with-current-buffer (harmless-session-buffer session)
                    (buffer-string))))
        (should (string-search "OLD-GOAL" text))
        (should (string-search "MARKER-KEEP" text))
        (should (string-search "Error: The model returned an empty summary."
                               text))))))

(ert-deftest harmless-compact-provider-error-keeps-the-transcript ()
  (harmless-compact-test-env
    (let* ((session (harmless-compact-test-make-session :cwd root))
           (messages (harmless-compact-test-turns '("OLD-GOAL" "LATEST")))
           (events nil))
      (setf (harmless-session-messages session) messages)
      (harmless-session-save session)
      (setq harmless-compact-test-script
            (lambda (_messages _tools callback)
              (funcall callback '(:error "HTTP 500"))))
      (let ((harmless-event-functions
             (cons (lambda (_session event) (push event events))
                   harmless-event-functions)))
        (should (null (harmless-compact-session session 1))))
      (should (eq messages (harmless-session-messages session)))
      (should (eq 'error (harmless-session-status session)))
      (should (member '(:error "HTTP 500") events))
      (should (string-search "OLD-GOAL" (harmless-compact-test-transcript session)))
      (should-not (string-search "SUM-ONE"
                                 (harmless-compact-test-transcript session))))))

(ert-deftest harmless-compact-abort-ignores-a-late-summary ()
  (harmless-compact-test-env
    (let* ((session (harmless-compact-test-make-session :cwd root))
           (messages (harmless-compact-test-turns '("A" "B" "C"))))
      (setf (harmless-session-messages session) messages)
      (harmless-session-save session)
      (setq harmless-compact-test-script
            (lambda (_messages _tools callback)
              (setq harmless-compact-test-callback callback)
              nil))
      (should (null (harmless-compact-session session)))
      (should (eq 'streaming (harmless-session-status session)))
      (harmless-turn-abort session)
      (should (eq 'idle (harmless-session-status session)))
      (funcall harmless-compact-test-callback '(:text "SECRET-SUMMARY"))
      (funcall harmless-compact-test-callback '(:stop "stop"))
      (should (eq messages (harmless-session-messages session)))
      (should (eq 'idle (harmless-session-status session)))
      (harmless-compact-test-absent session "SECRET-SUMMARY")
      (should (string-search "\"A\"" (harmless-compact-test-transcript session)))
      (should-not (string-search "SECRET-SUMMARY"
                                 (harmless-compact-test-transcript session))))))

(ert-deftest harmless-compact-provider-signal-leaves-the-transcript ()
  (harmless-compact-test-env
    (let* ((session (harmless-compact-test-make-session :cwd root))
           (messages (harmless-compact-test-turns '("A" "B" "C"))))
      (setf (harmless-session-messages session) messages)
      (setq harmless-compact-test-script
            (lambda (_messages _tools callback)
              (setq harmless-compact-test-callback callback)
              (error "provider exploded")))
      (harmless-compact-test-error
       "provider exploded"
       (lambda () (harmless-compact-session session)))
      (should (eq 'error (harmless-session-status session)))
      (should (eq messages (harmless-session-messages session)))
      (should (null (harmless-session-process session)))
      (funcall harmless-compact-test-callback '(:text "SECRET-SUMMARY"))
      (funcall harmless-compact-test-callback '(:stop "stop"))
      (should (eq messages (harmless-session-messages session)))
      (harmless-compact-test-absent session "SECRET-SUMMARY"))))

(ert-deftest harmless-compact-replaces-an-earlier-summary ()
  (harmless-compact-test-env
    (let* ((session (harmless-compact-test-make-session :cwd root))
           (previous (list :role :summary :content "OLD-PREAMBLE"))
           (turns (harmless-compact-test-turns '("A" "B" "C")))
           (last-user (nth 4 turns)))
      (setf (harmless-session-messages session) (cons previous turns))
      (should (equal "Summary of the earlier conversation:\n\nSUM-ONE"
                     (harmless-compact-session session 1)))
      (let ((request (harmless-compact-test-request-text
                      (car harmless-compact-test-requests)))
            (msgs (harmless-session-messages session)))
        (should (string-search "OLD-PREAMBLE" request))
        (should (string-search "A" request))
        (should-not (string-search "C" request))
        (should (eq last-user (nth 1 msgs)))
        (should-not (memq previous msgs))
        (should (= 1 (cl-count :summary msgs
                               :key (lambda (msg) (plist-get msg :role)))))
        (should (equal "Summary of the earlier conversation:\n\nSUM-ONE"
                       (plist-get (car msgs) :content)))
        (harmless-compact-test-absent session "OLD-PREAMBLE")
        (harmless-compact-test-absent session "A")
        (harmless-compact-test-absent session "B")))))

(ert-deftest harmless-compact-root-cwd-stays-under-the-data-directory ()
  (harmless-compact-test-env
    (let ((root-messages (file-exists-p "/messages.jsonl"))
          (root-summary (file-exists-p "/summary.json"))
          (session (harmless-compact-test-make-session :cwd "/")))
      (setf (harmless-session-messages session)
            (harmless-compact-test-turns '("A" "B" "C")))
      (harmless-compact-session session 1)
      (harmless-compact-test-assert-stored
       session harmless-directory root-messages root-summary)
      (should (equal "/" (harmless-session-cwd session)))
      (should (string-search "SUM-ONE" (harmless-compact-test-transcript session)))
      (should-not (file-exists-p "/SUM-ONE")))))

(ert-deftest harmless-compact-trailing-slash-stays-under-the-data-directory ()
  (harmless-compact-test-env
    (let* ((slash (file-name-as-directory root))
           (root-messages (file-exists-p "/messages.jsonl"))
           (root-summary (file-exists-p "/summary.json"))
           (session (harmless-compact-test-make-session :cwd slash)))
      (should (string-suffix-p "/" slash))
      (setf (harmless-session-messages session)
            (harmless-compact-test-turns '("A" "B" "C")))
      (harmless-compact-session session 1)
      (harmless-compact-test-assert-stored
       session harmless-directory root-messages root-summary)
      (should (string-search "SUM-ONE" (harmless-compact-test-transcript session)))
      (should-not (file-exists-p (expand-file-name "messages.jsonl" slash))))))

(ert-deftest harmless-compact-missing-directory-is-not-the-filesystem-root ()
  (harmless-compact-test-env
    (let* ((missing (expand-file-name "no-such-project" root))
           (root-messages (file-exists-p "/messages.jsonl"))
           (root-summary (file-exists-p "/summary.json"))
           (session (harmless-compact-test-make-session :cwd missing)))
      (should-not (file-exists-p missing))
      (setf (harmless-session-messages session)
            (harmless-compact-test-turns '("A" "B" "C")))
      (harmless-compact-session session 1)
      (should-not (file-exists-p missing))
      (should-not (file-directory-p missing))
      (harmless-compact-test-assert-stored
       session harmless-directory root-messages root-summary)
      (should (string-search "C" (harmless-compact-test-transcript session))))))

(ert-deftest harmless-compact-detached-chat-saves-under-the-data-directory ()
  (harmless-compact-test-env
    (let* ((scratch (make-temp-file "harmless-compact-scratch-" t))
           (default-directory (file-name-as-directory scratch))
           (before (directory-files scratch nil directory-files-no-dot-files-regexp))
           (root-messages (file-exists-p "/messages.jsonl"))
           (root-summary (file-exists-p "/summary.json"))
           (session (harmless-compact-test-make-session :detached t)))
      (should (null (harmless-session-cwd session)))
      (setf (harmless-session-messages session)
            (harmless-compact-test-turns '("A" "B" "C")))
      (harmless-compact-session session 1)
      (harmless-compact-test-assert-stored
       session harmless-directory root-messages root-summary)
      (should (string-search "/_detached/"
                             (file-name-as-directory (harmless-session-dir session))))
      (should (string-search "SUM-ONE" (harmless-compact-test-transcript session)))
      (should (equal before
                     (directory-files scratch nil
                                      directory-files-no-dot-files-regexp)))
      (should-not (file-exists-p
                   (expand-file-name "messages.jsonl" scratch))))))

(ert-deftest harmless-compact-relative-cwd-restores-the-transcript ()
  (harmless-compact-test-env
    (let ((root-messages (file-exists-p "/messages.jsonl"))
          (default-directory (file-name-as-directory root)))
      (dolist (case '(("proj" . "Directory must be absolute, not \"proj\"")
                      ("" . "Directory must be absolute, not \"\"")
                      ("relative/" . "Directory must be absolute, not \"relative/\"")))
        (let* ((cwd (car case))
               (provider (harmless-compact-test-provider))
               (messages (harmless-compact-test-turns '("A" "B" "C")))
               (session (harmless-session--create
                         :id "compact-rel"
                         :cwd cwd
                         :provider provider
                         :model "grok-4.6"
                         :reasoning-effort "high"
                         :messages messages
                         :status 'idle))
               (events nil))
          (setq harmless-providers (list provider)
                harmless-compact-test-live session)
          (let ((harmless-event-functions
                 (cons (lambda (_session event) (push event events))
                       harmless-event-functions)))
            (should (null (harmless-compact-session session 1))))
          (should (eq messages (harmless-session-messages session)))
          (should (eq 'error (harmless-session-status session)))
          (should (member (list :error (cdr case)) events))
          (should (eq root-messages (file-exists-p "/messages.jsonl")))
          (should-not (file-exists-p
                       (expand-file-name "messages.jsonl" root))))))))

(ert-deftest harmless-compact-requires-a-session ()
  (harmless-compact-test-error
   "No Harmless session"
   (lambda () (harmless-compact-session nil))))

(ert-deftest harmless-compact-requires-a-provider ()
  (harmless-compact-test-env
    (let ((session (harmless-session--create
                    :id "compact-none"
                    :cwd root
                    :provider nil
                    :model "grok-4.6"
                    :messages (harmless-compact-test-turns '("A" "B" "C"))
                    :status 'idle)))
      (harmless-compact-test-error
       "No Harmless provider configured"
       (lambda () (harmless-compact-session session)))
      (should (eq 'idle (harmless-session-status session)))
      (should (null harmless-compact-test-requests))
      (should (equal '("A" "A-reply" "B" "B-reply" "C" "C-reply")
                     (mapcar (lambda (msg) (plist-get msg :content))
                             (harmless-session-messages session)))))))

(ert-deftest harmless-compact-command-uses-the-current-session ()
  (harmless-compact-test-env
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
      (with-temp-buffer
        (setq default-directory (file-name-as-directory root))
        (harmless-compact-test-error
         "No Harmless session"
         (lambda () (harmless-compact)))))
    (let ((session (harmless-compact-test-make-session :cwd root)))
      (setf (harmless-session-messages session)
            (harmless-compact-test-turns '("A" "B" "C")))
      (with-temp-buffer
        (setq default-directory "relative")
        (setq-local harmless--session session)
        (should (equal "Summary of the earlier conversation:\n\nSUM-ONE"
                       (harmless-compact))))
      (should (equal '("Summary of the earlier conversation:\n\nSUM-ONE"
                       "B" "B-reply" "C" "C-reply")
                     (mapcar (lambda (msg) (plist-get msg :content))
                             (harmless-session-messages session)))))
    (let ((session (harmless-compact-test-make-session :cwd root))
          (notes nil))
      (setf (harmless-session-messages session)
            (harmless-compact-test-turns '("U1" "U2" "U3" "U4" "U5")))
      (with-temp-buffer
        (setq-local harmless--session session)
        (let ((current-prefix-arg '(4)))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) notes))))
            (should (equal "Summary of the earlier conversation:\n\nSUM-ONE"
                           (call-interactively #'harmless-compact))))))
      (should (equal '("Compacted 1 turn") notes))
      (should (equal '("Summary of the earlier conversation:\n\nSUM-ONE"
                       "U2" "U2-reply" "U3" "U3-reply"
                       "U4" "U4-reply" "U5" "U5-reply")
                     (mapcar (lambda (msg) (plist-get msg :content))
                             (harmless-session-messages session))))
      (harmless-compact-test-absent session "U1"))
    (let ((session (harmless-compact-test-make-session :cwd root)))
      (setf (harmless-session-messages session)
            (harmless-compact-test-turns '("A" "B" "C")))
      (with-temp-buffer
        (setq-local harmless--session session)
        (let ((current-prefix-arg 0))
          (harmless-compact-test-error
           "Keep at least one turn"
           (lambda () (call-interactively #'harmless-compact)))))
      (should (eq 'idle (harmless-session-status session)))
      (should (equal '("A" "A-reply" "B" "B-reply" "C" "C-reply")
                     (mapcar (lambda (msg) (plist-get msg :content))
                             (harmless-session-messages session)))))))

(ert-deftest harmless-compact-needed-at-the-window-boundary ()
  (harmless-compact-test-at-window
    (let ((probe (lambda (model used texts)
                   (harmless-compact-needed-p
                    (harmless-session--create
                     :model model
                     :last-prompt-tokens used
                     :messages (harmless-compact-test-turns texts))))))
      (should-not (harmless-compact-needed-p nil))
      (should-not
       (harmless-compact-needed-p
        (harmless-session--create
         :model "grok-4.6"
         :messages nil
         :last-prompt-tokens 500000)))
      (should-not
       (harmless-compact-needed-p
        (harmless-session--create
         :model "grok-4.6"
         :messages ()
         :last-prompt-tokens 500000)))
      (should-not
       (harmless-compact-needed-p
        (harmless-session--create
         :model "grok-4.6"
         :messages "not-a-list"
         :last-prompt-tokens 500000)))
      (should-not
       (harmless-compact-needed-p
        (harmless-session--create
         :model "grok-4.6"
         :messages (vector (list :role :user :content "OLD-GOAL"))
         :last-prompt-tokens 500000)))
      (should-not
       (harmless-compact-needed-p
        (harmless-session--create
         :model "grok-4.6"
         :messages (harmless-compact-test-turns
                    '("OLD-GOAL" "MID-TURN" "LATE-TURN")))))
      (should-not (funcall probe "grok-4.6" 399999
                           '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
      (should-not (funcall probe "grok-4.6" 399999.0
                           '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
      (should (funcall probe "grok-4.6" 400000
                       '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
      (should (funcall probe "grok-4.6" 400000.0
                       '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
      (should-not (funcall probe "grok-4.3" 799999
                           '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
      (should (funcall probe "grok-4.3" 800000
                       '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
      (should-not (funcall probe "nope" 800000
                           '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
      (should-not (funcall probe "grok-4.6" 500000
                           '("OLD-GOAL" "LATE-TURN")))
      (should-not (funcall probe "grok-4.6" 500000 '("ONLY-TURN")))
      (should (funcall probe "grok-4.6" 500000
                       '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
      (let ((harmless-compact-threshold nil))
        (should-not (funcall probe "grok-4.6" 500000
                             '("OLD-GOAL" "MID-TURN" "LATE-TURN"))))
      (let ((harmless-compact-threshold 100))
        (should-not (funcall probe "grok-4.6" 499999
                             '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
        (should (funcall probe "grok-4.6" 500000
                         '("OLD-GOAL" "MID-TURN" "LATE-TURN"))))
      (let ((harmless-compact-threshold 1))
        (should-not (funcall probe "grok-4.6" 4999
                             '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
        (should (funcall probe "grok-4.6" 5000
                         '("OLD-GOAL" "MID-TURN" "LATE-TURN"))))
      (let ((harmless-compact-keep-turns 0))
        (should-not (funcall probe "grok-4.6" 500000
                             '("OLD-GOAL" "MID-TURN" "LATE-TURN"))))
      (let ((harmless-compact-keep-turns nil))
        (should-not (funcall probe "grok-4.6" 500000
                             '("OLD-GOAL" "MID-TURN" "LATE-TURN"))))
      (let ((harmless-usage-context-windows '(("grok-4.6" . 0))))
        (should-not (funcall probe "grok-4.6" 500000
                             '("OLD-GOAL" "MID-TURN" "LATE-TURN"))))
      (let ((harmless-usage-context-windows '(("grok-4.6" . -1))))
        (should-not (funcall probe "grok-4.6" 500000
                             '("OLD-GOAL" "MID-TURN" "LATE-TURN"))))
      (let ((harmless-openai--context-windows '(("grok-4.6" . 1000)))
            (harmless-usage-context-windows '(("grok-4.6" . 500000))))
        (should-not (funcall probe "grok-4.6" 799
                             '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
        (should (funcall probe "grok-4.6" 800
                         '("OLD-GOAL" "MID-TURN" "LATE-TURN")))))))

(ert-deftest harmless-compact-threshold-must-be-a-percent ()
  (should (= 1 (let ((harmless-compact-threshold 1))
                 (harmless-compact--validate-threshold))))
  (should (= 80 (let ((harmless-compact-threshold 80))
                  (harmless-compact--validate-threshold))))
  (should (= 100 (let ((harmless-compact-threshold 100))
                   (harmless-compact--validate-threshold))))
  (should (null (let ((harmless-compact-threshold nil))
                  (harmless-compact--validate-threshold))))
  (harmless-compact-test-env
    (let* ((root-messages (file-exists-p "/messages.jsonl"))
           (session (harmless-compact-test-auto-session
                     root 500000
                     '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
           (messages (harmless-session-messages session)))
      (dolist (case
               '((0 . "Compact threshold must be an integer from 1 to 100, or nil, not 0")
                 (101 . "Compact threshold must be an integer from 1 to 100, or nil, not 101")
                 (t . "Compact threshold must be an integer from 1 to 100, or nil, not t")
                 ("" . "Compact threshold must be an integer from 1 to 100, or nil, not \"\"")
                 (0.8 . "Compact threshold must be an integer from 1 to 100, or nil, not 0.8")
                 (80.0 . "Compact threshold must be an integer from 1 to 100, or nil, not 80.0")))
        (let ((harmless-compact-threshold (car case)))
          (harmless-compact-test-error
           (cdr case)
           (lambda () (harmless-turn-run session "NEW-PROMPT")))))
      (should (eq messages (harmless-session-messages session)))
      (should (null harmless-compact-test-requests))
      (should (eq 'idle (harmless-session-status session)))
      (should (string-search "OLD-GOAL"
                             (harmless-compact-test-transcript session)))
      (should-not (string-search "NEW-PROMPT"
                                 (harmless-compact-test-transcript session)))
      (should (eq root-messages (file-exists-p "/messages.jsonl"))))))

(ert-deftest harmless-compact-full-window-summarizes-before-the-prompt ()
  (harmless-compact-test-env
    (harmless-compact-test-at-window
      (setq harmless-compact-test-script (harmless-compact-test-auto-script))
      (let* ((root-messages (file-exists-p "/messages.jsonl"))
             (root-summary (file-exists-p "/summary.json"))
             (prior (harmless-compact-test-turns
                     '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
             (late-user (nth 4 prior))
             (late-reply (nth 5 prior))
             (session (harmless-compact-test-make-session :cwd root)))
        (setf (harmless-session-messages session) prior
              (harmless-session-last-prompt-tokens session) 400000)
        (harmless-session-save session)
        (harmless-turn-run session "NEW-PROMPT")
        (let ((turn (car harmless-compact-test-requests))
              (compact (cadr harmless-compact-test-requests))
              (msgs (harmless-session-messages session)))
          (should (= 2 (length harmless-compact-test-requests)))
          (should (= 1 (cl-count-if (lambda (record) (null (nth 1 record)))
                                    harmless-compact-test-requests)))
          (should (null (nth 1 compact)))
          (should (consp (nth 1 turn)))
          (should (equal "grok-4.6" (nth 2 compact)))
          (should (equal "high" (nth 3 compact)))
          (should (eq 'streaming (nth 4 compact)))
          (should (eq 'streaming (nth 4 turn)))
          (let ((older (harmless-compact-test-request-text compact))
                (sent (harmless-compact-test-record-text turn)))
            (should (string-search "OLD-GOAL" older))
            (should (string-search "MID-TURN" older))
            (should-not (string-search "LATE-TURN" older))
            (should-not (string-search "NEW-PROMPT" older))
            (should (string-search "SUM-AUTO" sent))
            (should (string-search "LATE-TURN" sent))
            (should (string-search "NEW-PROMPT" sent))
            (should-not (string-search "OLD-GOAL" sent)))
          (should (eq late-user (nth 1 msgs)))
          (should (eq late-reply (nth 2 msgs)))
          (should (equal :summary (plist-get (car msgs) :role)))
          (should (equal "Summary of the earlier conversation:\n\nSUM-AUTO"
                         (plist-get (car msgs) :content)))
          (should (equal "NEW-PROMPT" (plist-get (nth 3 msgs) :content)))
          (should (equal "TURN-OK" (plist-get (nth 4 msgs) :content)))
          (harmless-compact-test-absent session "OLD-GOAL")
          (harmless-compact-test-absent session "MID-TURN"))
        (should (eq 'idle (harmless-session-status session)))
        (should (equal "turn-proc" (harmless-session-process session)))
        (should (= 400011 (harmless-session-prompt-tokens session)))
        (should (= 6 (harmless-session-completion-tokens session)))
        (should (= 11 (harmless-session-last-prompt-tokens session)))
        (should (= 3 (harmless-session-last-completion-tokens session)))
        (harmless-compact-test-assert-stored
         session harmless-directory root-messages root-summary)
        (let ((file (harmless-compact-test-transcript session)))
          (should (string-search "SUM-AUTO" file))
          (should (string-search "LATE-TURN" file))
          (should (string-search "NEW-PROMPT" file))
          (should (string-search "TURN-OK" file))
          (should-not (string-search "OLD-GOAL" file))
          (should-not (string-search "MID-TURN" file)))))))

(defun harmless-compact-test-assert-sent (session)
  "Assert SESSION's prompt was sent with no compact."
  (should (= 1 (length harmless-compact-test-requests)))
  (should (consp (nth 1 (car harmless-compact-test-requests))))
  (should (eq 'idle (harmless-session-status session)))
  (should (string-search "NEW-PROMPT"
                         (harmless-compact-test-transcript session)))
  (should (string-search "TURN-OK"
                         (harmless-compact-test-transcript session)))
  (should-not (string-search "SUM-AUTO"
                             (harmless-compact-test-transcript session)))
  (should-not (string-search "Summary of the earlier conversation:"
                             (harmless-compact-test-transcript session))))

(ert-deftest harmless-compact-room-in-the-window-sends-the-prompt ()
  (harmless-compact-test-env
    (harmless-compact-test-at-window
      (setq harmless-compact-test-script (harmless-compact-test-auto-script))
      (let ((session (harmless-compact-test-auto-session
                      root 399999
                      '("OLD-GOAL" "MID-TURN" "LATE-TURN"))))
        (harmless-turn-run session "NEW-PROMPT")
        (harmless-compact-test-assert-sent session)
        (should (string-search "OLD-GOAL"
                               (harmless-compact-test-transcript session)))
        (should (equal '(:user :assistant :user :assistant
                         :user :assistant :user :assistant)
                       (harmless-compact-test-roles session)))))))

(ert-deftest harmless-compact-nil-threshold-sends-the-prompt ()
  (harmless-compact-test-env
    (harmless-compact-test-at-window
      (setq harmless-compact-test-script (harmless-compact-test-auto-script))
      (let ((harmless-compact-threshold nil)
            (session (harmless-compact-test-auto-session
                      root 500000
                      '("OLD-GOAL" "MID-TURN" "LATE-TURN"))))
        (harmless-turn-run session "NEW-PROMPT")
        (harmless-compact-test-assert-sent session)
        (should (string-search "OLD-GOAL"
                               (harmless-compact-test-transcript session)))))))

(ert-deftest harmless-compact-unknown-window-sends-the-prompt ()
  (harmless-compact-test-env
    (harmless-compact-test-at-window
      (setq harmless-compact-test-script (harmless-compact-test-auto-script))
      (let ((session (harmless-compact-test-auto-session
                      root 800000
                      '("OLD-GOAL" "MID-TURN" "LATE-TURN"))))
        (setf (harmless-session-model session) "nope")
        (harmless-turn-run session "NEW-PROMPT")
        (harmless-compact-test-assert-sent session)
        (should (string-search "OLD-GOAL"
                               (harmless-compact-test-transcript session)))))))

(ert-deftest harmless-compact-keeps-a-short-transcript ()
  (harmless-compact-test-env
    (harmless-compact-test-at-window
      (setq harmless-compact-test-script (harmless-compact-test-auto-script))
      (let ((session (harmless-compact-test-auto-session
                      root 500000 '("ONLY-TURN"))))
        (harmless-turn-run session "NEW-PROMPT")
        (harmless-compact-test-assert-sent session)
        (should (string-search "ONLY-TURN"
                               (harmless-compact-test-transcript session)))
        (should (equal '(:user :assistant :user :assistant)
                       (harmless-compact-test-roles session)))))))

(ert-deftest harmless-compact-bad-keep-still-sends-the-prompt ()
  (harmless-compact-test-env
    (harmless-compact-test-at-window
      (setq harmless-compact-test-script (harmless-compact-test-auto-script))
      (dolist (keep '(0 nil "2" -1))
        (setq harmless-compact-test-requests nil)
        (let ((harmless-compact-keep-turns keep)
              (session (harmless-compact-test-auto-session
                        root 500000
                        '("OLD-GOAL" "MID-TURN" "LATE-TURN"))))
          (harmless-turn-run session "NEW-PROMPT")
          (harmless-compact-test-assert-sent session)
          (should (string-search "OLD-GOAL"
                                 (harmless-compact-test-transcript session))))))))

(ert-deftest harmless-compact-empty-summary-still-sends-the-prompt ()
  (harmless-compact-test-env
    (harmless-compact-test-at-window
      (let ((session (harmless-compact-test-auto-session
                      root 400000
                      '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
            (events nil))
        (setq harmless-compact-test-script
              (lambda (_messages tools callback)
                (if (null tools)
                    (progn
                      (funcall callback '(:usage 9 3))
                      (funcall callback '(:text "  "))
                      (funcall callback '(:stop "stop")))
                  (funcall callback '(:text "TURN-OK"))
                  (funcall callback '(:stop "stop")))
                nil))
        (let ((harmless-event-functions
               (cons (lambda (_session event) (push event events))
                     harmless-event-functions)))
          (harmless-turn-run session "NEW-PROMPT"))
        (should (= 2 (length harmless-compact-test-requests)))
        (should (null (nth 1 (cadr harmless-compact-test-requests))))
        (should (consp (nth 1 (car harmless-compact-test-requests))))
        (should (eq 'idle (harmless-session-status session)))
        (should (member '(:error "The model returned an empty summary.") events))
        (should (string-search "OLD-GOAL"
                               (harmless-compact-test-transcript session)))
        (should (string-search "NEW-PROMPT"
                               (harmless-compact-test-transcript session)))
        (should (string-search "TURN-OK"
                               (harmless-compact-test-transcript session)))
        (should-not (string-search "Summary of the earlier conversation:"
                                   (harmless-compact-test-transcript session)))
        (should-not (memq :summary (harmless-compact-test-roles session)))))))

(ert-deftest harmless-compact-provider-error-still-sends-the-prompt ()
  (harmless-compact-test-env
    (harmless-compact-test-at-window
      (let ((session (harmless-compact-test-auto-session
                      root 400000
                      '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
            (events nil))
        (setq harmless-compact-test-script
              (lambda (_messages tools callback)
                (if (null tools)
                    (funcall callback '(:error "HTTP 500"))
                  (funcall callback '(:text "TURN-OK"))
                  (funcall callback '(:stop "stop")))
                nil))
        (let ((harmless-event-functions
               (cons (lambda (_session event) (push event events))
                     harmless-event-functions)))
          (harmless-turn-run session "NEW-PROMPT"))
        (should (= 2 (length harmless-compact-test-requests)))
        (should (eq 'idle (harmless-session-status session)))
        (should (member '(:error "HTTP 500") events))
        (should (string-search "OLD-GOAL"
                               (harmless-compact-test-transcript session)))
        (should (string-search "NEW-PROMPT"
                               (harmless-compact-test-transcript session)))
        (should (string-search "TURN-OK"
                               (harmless-compact-test-transcript session)))
        (should-not (memq :summary (harmless-compact-test-roles session)))))))

(ert-deftest harmless-compact-provider-signal-still-sends-the-prompt ()
  (harmless-compact-test-env
    (harmless-compact-test-at-window
      (let ((session (harmless-compact-test-auto-session
                      root 400000
                      '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
            (events nil))
        (setq harmless-compact-test-script
              (lambda (_messages tools callback)
                (if (null tools)
                    (progn
                      (setq harmless-compact-test-callback callback)
                      (error "provider exploded"))
                  (funcall callback '(:text "TURN-OK"))
                  (funcall callback '(:stop "stop"))
                  "turn-proc")))
        (let ((harmless-event-functions
               (cons (lambda (_session event) (push event events))
                     harmless-event-functions)))
          (harmless-turn-run session "NEW-PROMPT"))
        (should (= 2 (length harmless-compact-test-requests)))
        (should (null (nth 1 (cadr harmless-compact-test-requests))))
        (should (consp (nth 1 (car harmless-compact-test-requests))))
        (should (eq 'idle (harmless-session-status session)))
        (should (equal "turn-proc" (harmless-session-process session)))
        (should (member '(:error "provider exploded") events))
        (should (string-search "OLD-GOAL"
                               (harmless-compact-test-transcript session)))
        (should (string-search "NEW-PROMPT"
                               (harmless-compact-test-transcript session)))
        (should (string-search "TURN-OK"
                               (harmless-compact-test-transcript session)))
        (should-not (memq :summary (harmless-compact-test-roles session)))
        (funcall harmless-compact-test-callback '(:text "SECRET-SUMMARY"))
        (funcall harmless-compact-test-callback '(:stop "stop"))
        (should (= 2 (length harmless-compact-test-requests)))
        (harmless-compact-test-absent session "SECRET-SUMMARY")))))

(ert-deftest harmless-compact-waits-for-the-summary ()
  (harmless-compact-test-env
    (harmless-compact-test-at-window
      (let ((session (harmless-compact-test-auto-session
                      root 400000
                      '("OLD-GOAL" "MID-TURN" "LATE-TURN"))))
        (setq harmless-compact-test-script
              (lambda (_messages tools callback)
                (if (null tools)
                    (progn
                      (setq harmless-compact-test-callback callback)
                      nil)
                  (funcall callback '(:text "TURN-OK"))
                  (funcall callback '(:stop "stop"))
                  "turn-proc")))
        (harmless-turn-run session "NEW-PROMPT")
        (should (= 1 (length harmless-compact-test-requests)))
        (should (null (nth 1 (car harmless-compact-test-requests))))
        (should-not (string-search "NEW-PROMPT"
                                   (harmless-compact-test-request-text
                                    (car harmless-compact-test-requests))))
        (should (string-search "OLD-GOAL"
                               (harmless-compact-test-request-text
                                (car harmless-compact-test-requests))))
        (should (eq 'streaming (harmless-session-status session)))
        (should (null (harmless-session-process session)))
        (should-not (memq :summary (harmless-compact-test-roles session)))
        (should (string-search "NEW-PROMPT"
                               (harmless-compact-test-transcript session)))
        (funcall harmless-compact-test-callback '(:text "SUM-AUTO"))
        (funcall harmless-compact-test-callback '(:stop "stop"))
        (should (= 2 (length harmless-compact-test-requests)))
        (should (consp (nth 1 (car harmless-compact-test-requests))))
        (should (equal "turn-proc" (harmless-session-process session)))
        (should (eq 'idle (harmless-session-status session)))
        (should (equal :summary
                       (plist-get (car (harmless-session-messages session))
                                  :role)))
        (should (equal "Summary of the earlier conversation:\n\nSUM-AUTO"
                       (plist-get (car (harmless-session-messages session))
                                  :content)))
        (harmless-compact-test-absent session "OLD-GOAL")
        (harmless-compact-test-absent session "MID-TURN")
        (should (string-search "LATE-TURN"
                               (harmless-compact-test-transcript session)))
        (should (string-search "NEW-PROMPT"
                               (harmless-compact-test-transcript session)))
        (should (string-search "TURN-OK"
                               (harmless-compact-test-transcript session)))))))

(ert-deftest harmless-compact-abort-drops-the-waiting-turn ()
  (harmless-compact-test-env
    (harmless-compact-test-at-window
      (let ((session (harmless-compact-test-auto-session
                      root 400000
                      '("OLD-GOAL" "MID-TURN" "LATE-TURN"))))
        (setq harmless-compact-test-script
              (lambda (_messages tools callback)
                (when (null tools)
                  (setq harmless-compact-test-callback callback))
                nil))
        (harmless-turn-run session "NEW-PROMPT")
        (should (= 1 (length harmless-compact-test-requests)))
        (should (eq 'streaming (harmless-session-status session)))
        (harmless-turn-abort session)
        (should (eq 'idle (harmless-session-status session)))
        (funcall harmless-compact-test-callback '(:text "SECRET-SUMMARY"))
        (funcall harmless-compact-test-callback '(:stop "stop"))
        (should (= 1 (length harmless-compact-test-requests)))
        (should (eq 'idle (harmless-session-status session)))
        (should (string-search "OLD-GOAL"
                               (harmless-compact-test-transcript session)))
        (should (string-search "NEW-PROMPT"
                               (harmless-compact-test-transcript session)))
        (harmless-compact-test-absent session "SECRET-SUMMARY")
        (harmless-compact-test-absent session "TURN-OK")
        (should-not (memq :summary (harmless-compact-test-roles session)))
        (should-not (string-search "SECRET-SUMMARY"
                                   (harmless-compact-test-transcript session)))))))

(provide 'harmless-compact-tests)
