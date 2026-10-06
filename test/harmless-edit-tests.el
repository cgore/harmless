;;; harmless-edit-tests.el --- Tests for editing an earlier prompt -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(cl-defstruct (harmless-edit-fake
               (:include harmless-provider)
               (:constructor harmless-edit-make-fake))
  script)

(cl-defmethod harmless-provider-complete ((provider harmless-edit-fake)
                                          messages tools callback)
  (ignore tools)
  (when (functionp (harmless-edit-fake-script provider))
    (funcall (harmless-edit-fake-script provider) messages callback))
  nil)

(defun harmless-edit-test-error (text thunk)
  "Assert that THUNK signals an error whose message is TEXT."
  (let ((err (should-error (funcall thunk) :type 'error)))
    (should (equal (error-message-string err) text))))

(defun harmless-edit-test-turns (texts)
  "Return a user message and an assistant reply for each string in TEXTS."
  (cl-mapcan (lambda (text)
               (list (list :role :user :content text)
                     (list :role :assistant
                           :content (concat text "-reply"))))
             texts))

(defun harmless-edit-test-rich ()
  "Return a summary, a tool call, and three user turns."
  (list (list :role :summary :content "EARLIER")
        (list :role :user :content "OLD-GOAL")
        (list :role :assistant :content "looking"
              :tool-calls (list (list :id "1" :name "read_file"
                                      :args '(:path "old.txt"))))
        (list :role :tool :id "1" :name "read_file" :content "old body")
        (list :role :user :content "MID-TURN")
        (list :role :assistant :content "MID-TURN-reply")
        (list :role :user :content "LATE-TURN")
        (list :role :assistant :content "LATE-TURN-reply")))

(defun harmless-edit-test-roles (session)
  "Return the role of each message in SESSION."
  (mapcar (lambda (msg) (plist-get msg :role))
          (harmless-session-messages session)))

(defun harmless-edit-test-contents (session)
  "Return the content of each message in SESSION."
  (mapcar (lambda (msg) (plist-get msg :content))
          (harmless-session-messages session)))

(defun harmless-edit-test-transcript (session)
  "Return the saved messages file for SESSION."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "messages.jsonl" (harmless-session-dir session)))
    (buffer-string)))

(defun harmless-edit-test-assert-stored (session data-dir root-messages root-summary)
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

(defun harmless-edit-test-make-session (&rest args)
  "Create a session on a dummy provider that answers NEW-REPLY.
ARGS are passed to `harmless-session-new'."
  (let ((provider (harmless-edit-make-fake
                   :name "fake"
                   :host "none"
                   :script
                   (lambda (_messages callback)
                     (funcall callback '(:text "NEW-REPLY"))
                     (funcall callback '(:stop "stop"))))))
    (setq harmless-providers (list provider))
    (apply #'harmless-session-new
           :provider provider
           :model "grok-4.6"
           :reasoning-effort "high"
           args)))

(defmacro harmless-edit-test-env (&rest body)
  "Run BODY with a temp Harmless directory and a fresh session table."
  (declare (indent 0))
  `(let* ((root (make-temp-file "harmless-edit-" t))
          (harmless-directory (expand-file-name ".harmless" root))
          (harmless--sessions (make-hash-table :test 'equal))
          (harmless-providers nil))
     ,@body))

(defmacro harmless-edit-test-answers (answer text &rest body)
  "Run BODY with `y-or-n-p' returning ANSWER and `read-string' returning TEXT.
`harmless-edit-test-asked' collects the questions.
`harmless-edit-test-notes' collects `message' text.
`harmless-edit-test-reads' collects each read."
  (declare (indent 2))
  `(let (harmless-edit-test-asked
         harmless-edit-test-notes
         harmless-edit-test-reads)
     (cl-letf (((symbol-function 'y-or-n-p)
                (lambda (prompt)
                  (push prompt harmless-edit-test-asked)
                  ,answer))
               ((symbol-function 'message)
                (lambda (fmt &rest args)
                  (push (apply #'format fmt args)
                        harmless-edit-test-notes)))
               ((symbol-function 'read-string)
                (lambda (&rest _)
                  (push t harmless-edit-test-reads)
                  ,text)))
       ,@body)))

(ert-deftest harmless-edit-split-drops-the-chosen-turn ()
  (let* ((summary (list :role :summary :content "EARLIER"))
         (tool (list :role :tool :id "1" :name "read_file" :content "old body"))
         (call (list :role :assistant :content "looking"
                     :tool-calls (list (list :id "1" :name "read_file"
                                             :args '(:path "old.txt")))))
         (first (list :role :user :content "OLD-GOAL"))
         (second (list :role 'user :content "MID-TURN"))
         (third (list :role "user" :content "LATE-TURN"))
         (reply (list :role :assistant :content "LATE-TURN-reply"))
         (messages (list summary first call tool second third reply)))
    (should (eq summary (car (harmless-edit-split messages 1))))
    (should (null (cdr (harmless-edit-split messages 1))))
    (let ((kept (harmless-edit-split messages 2)))
      (should (eq summary (nth 0 kept)))
      (should (eq first (nth 1 kept)))
      (should (eq call (nth 2 kept)))
      (should (eq tool (nth 3 kept)))
      (should (null (nth 4 kept))))
    (should (eq second (car (last (harmless-edit-split messages 3)))))
    (should (eq tool (car (harmless-edit-split (list tool first second) 1))))
    (should (null (harmless-edit-split (list first second) 1)))
    (should (equal (list summary)
                   (harmless-edit-split (list summary first reply) 1)))
    (harmless-edit-test-error
     "Nothing to edit"
     (lambda () (harmless-edit-split messages 4)))
    (harmless-edit-test-error
     "Nothing to edit"
     (lambda () (harmless-edit-split messages 9)))
    (harmless-edit-test-error
     "Nothing to edit"
     (lambda () (harmless-edit-split nil 1)))
    (harmless-edit-test-error
     "Nothing to edit"
     (lambda () (harmless-edit-split '() 1)))
    (harmless-edit-test-error
     "Nothing to edit"
     (lambda () (harmless-edit-split "" 1)))
    (harmless-edit-test-error
     "Nothing to edit"
     (lambda () (harmless-edit-split (vector first) 1)))
    (harmless-edit-test-error
     "Nothing to edit"
     (lambda () (harmless-edit-split (list summary) 1)))
    (dolist (turn '(0 -1 t "1" 1.0))
      (harmless-edit-test-error
       "Choose a turn"
       (lambda () (harmless-edit-split messages turn))))
    (harmless-edit-test-error
     "Choose a turn"
     (lambda () (harmless-edit-split "" 0)))))

(ert-deftest harmless-edit-key-and-menu ()
  (should (eq (lookup-key harmless-session-mode-map (kbd "E"))
              #'harmless-edit-prompt))
  (should-not (eq (lookup-key harmless-session-mode-map (kbd "e"))
                  #'harmless-edit-prompt))
  (should-not (eq (lookup-key harmless-prompt-mode-map (kbd "E"))
                  #'harmless-edit-prompt))
  (should (equal (transient--suffix-key
                  (transient-get-suffix 'harmless-menu 'harmless-edit-prompt))
                 "E")))

(ert-deftest harmless-edit-starts-from-the-old-prompt ()
  (let (prompt)
    (cl-letf (((symbol-function 'read-string)
               (lambda (arg &rest _)
                 (setq prompt arg)
                 (with-temp-buffer
                   (run-hooks 'minibuffer-setup-hook)
                   (buffer-string)))))
      (should (equal "OLD-GOAL" (harmless-edit--read-text "  OLD-GOAL  ")))
      (should (equal "" (harmless-edit--read-text nil))))
    (should (equal "Edit prompt: " prompt))))

(ert-deftest harmless-edit-replaces-the-prompt-and-sends ()
  (harmless-edit-test-env
    (let* ((seen nil)
           (provider (harmless-edit-make-fake
                      :name "fake"
                      :host "none"
                      :script
                      (lambda (messages callback)
                        (setq seen messages)
                        (funcall callback '(:text "NEW-REPLY"))
                        (funcall callback '(:stop "stop")))))
           (root-messages (file-exists-p "/messages.jsonl"))
           (root-summary (file-exists-p "/summary.json"))
           (harmless-usage-context-windows '(("grok-4.6" . 500000)))
           (harmless-openai--context-windows nil)
           (harmless-compact-threshold 80)
           (harmless-compact-keep-turns 1)
           (messages (harmless-edit-test-rich))
           (session (progn
                      (setq harmless-providers (list provider))
                      (harmless-session-new
                       :cwd root
                       :provider provider
                       :model "grok-4.6"
                       :reasoning-effort "high"))))
      (setf (harmless-session-messages session) messages)
      (setf (harmless-session-last-prompt-tokens session) 400000)
      (setf (harmless-session-last-completion-tokens session) 7)
      (setf (harmless-session-prompt-tokens session) 100)
      (setf (harmless-session-completion-tokens session) 9)
      (setf (harmless-session-plan-mode session) t)
      (setf (harmless-session-status session) 'error)
      (should (harmless-compact-needed-p session))
      (should (null (harmless-session-buffer session)))
      (harmless-edit-test-answers t "unused"
        (should (= 2 (harmless-edit-session session 2 "  NEW-GOAL  ")))
        (should (equal '("Edited turn 2") harmless-edit-test-notes))
        (should (null harmless-edit-test-asked))
        (should (null harmless-edit-test-reads)))
      (let ((kept (harmless-session-messages session))
            (blob (mapconcat (lambda (msg) (or (plist-get msg :content) ""))
                             seen "\n")))
        (should (eq (nth 0 messages) (nth 0 kept)))
        (should (eq (nth 1 messages) (nth 1 kept)))
        (should (eq (nth 2 messages) (nth 2 kept)))
        (should (eq (nth 3 messages) (nth 3 kept)))
        (should (equal "NEW-GOAL" (plist-get (nth 4 kept) :content)))
        (should (equal "NEW-REPLY" (plist-get (nth 5 kept) :content)))
        (should (equal '(:summary :user :assistant :tool :user :assistant)
                       (harmless-edit-test-roles session)))
        (should (string-search "NEW-GOAL" blob))
        (should (string-search "OLD-GOAL" blob))
        (should (string-search "old body" blob))
        (should-not (string-search "MID-TURN" blob))
        (should-not (string-search "LATE-TURN" blob)))
      (should (eq 'idle (harmless-session-status session)))
      (should (harmless-session-plan-mode session))
      (should (= 0 (harmless-session-last-prompt-tokens session)))
      (should (= 0 (harmless-session-last-completion-tokens session)))
      (should (= 100 (harmless-session-prompt-tokens session)))
      (should (= 9 (harmless-session-completion-tokens session)))
      (should-not (harmless-compact-needed-p session))
      (setf (harmless-session-last-prompt-tokens session) 400000)
      (should (harmless-compact-needed-p session))
      (setf (harmless-session-last-prompt-tokens session) 0)
      (harmless-edit-test-assert-stored
       session harmless-directory root-messages root-summary)
      (let ((file (harmless-edit-test-transcript session)))
        (should (string-search "EARLIER" file))
        (should (string-search "OLD-GOAL" file))
        (should (string-search "old body" file))
        (should (string-search "old.txt" file))
        (should (string-search "NEW-GOAL" file))
        (should (string-search "NEW-REPLY" file))
        (should-not (string-search "MID-TURN" file))
        (should-not (string-search "LATE-TURN" file)))
      (with-current-buffer (harmless-session-buffer session)
        (should (string-search "OLD-GOAL" (buffer-string)))
        (should (string-search "old body" (buffer-string)))
        (should (string-search "NEW-GOAL" (buffer-string)))
        (should-not (string-search "LATE-TURN" (buffer-string)))))))

(ert-deftest harmless-edit-reruns-the-same-prompt ()
  (harmless-edit-test-env
    (let* ((messages (harmless-edit-test-turns '("OLD-GOAL")))
           (session (harmless-edit-test-make-session :cwd root))
           (old-user (nth 0 messages))
           (old-reply (nth 1 messages)))
      (setf (harmless-session-messages session) messages)
      (should (= 1 (harmless-edit-session session 1 "OLD-GOAL")))
      (should-not (eq old-user (nth 0 (harmless-session-messages session))))
      (should-not (member old-reply (harmless-session-messages session)))
      (should (equal '("OLD-GOAL" "NEW-REPLY")
                     (harmless-edit-test-contents session)))
      (let ((file (harmless-edit-test-transcript session)))
        (should (string-search "OLD-GOAL" file))
        (should (string-search "NEW-REPLY" file))
        (should-not (string-search "OLD-GOAL-reply" file))))))

(ert-deftest harmless-edit-command-uses-the-turn-at-point ()
  (harmless-edit-test-env
    (let ((session (harmless-edit-test-make-session :cwd root))
          (messages (harmless-edit-test-rich)))
      (setf (harmless-session-messages session) messages)
      (setf (harmless-session-last-prompt-tokens session) 400000)
      (harmless-ui-render-session session)
      (with-current-buffer (harmless-session-buffer session)
        (goto-char (point-min))
        (should (search-forward "EARLIER" nil t))
        (goto-char (match-beginning 0))
        (harmless-edit-test-answers t "NEW-GOAL"
          (harmless-edit-test-error
           "Point is not on a turn"
           (lambda () (harmless-edit-prompt)))
          (should (null harmless-edit-test-asked))
          (should (null harmless-edit-test-reads)))
        (should (eq messages (harmless-session-messages session)))
        (goto-char (point-max))
        (should (= 3 (harmless-edit--turn-at-point)))
        (goto-char (point-min))
        (should (search-forward "old body" nil t))
        (goto-char (match-beginning 0))
        (should (= 1 (get-text-property (point) 'harmless-turn)))
        (harmless-edit-test-answers t "NEW-GOAL"
          (should (= 3 (harmless-edit-prompt)))
          (should (equal '("Edit turn 1 and drop 3 turns? ")
                         harmless-edit-test-asked))
          (should (equal '("Edited turn 1") harmless-edit-test-notes))
          (should harmless-edit-test-reads)))
      (should (equal '("EARLIER" "NEW-GOAL" "NEW-REPLY")
                     (harmless-edit-test-contents session)))
      (should (= 0 (harmless-session-last-prompt-tokens session)))
      (with-current-buffer (harmless-session-buffer session)
        (should (string-search "EARLIER" (buffer-string)))
        (should (string-search "NEW-GOAL" (buffer-string)))
        (should-not (string-search "old body" (buffer-string)))
        (should-not (string-search "LATE-TURN" (buffer-string)))))))

(ert-deftest harmless-edit-at-end-edits-the-last-turn ()
  (harmless-edit-test-env
    (let ((session (harmless-edit-test-make-session :cwd root))
          (messages (harmless-edit-test-rich)))
      (setf (harmless-session-messages session) messages)
      (harmless-ui-render-session session)
      (with-current-buffer (harmless-session-buffer session)
        (goto-char (point-max))
        (harmless-edit-test-answers t "END-GOAL"
          (should (= 1 (harmless-edit-prompt)))
          (should (equal '("Edit turn 3 and drop 1 turn? ")
                         harmless-edit-test-asked))
          (should (equal '("Edited turn 3") harmless-edit-test-notes))))
      (should (equal '("EARLIER" "OLD-GOAL" "looking" "old body"
                       "MID-TURN" "MID-TURN-reply" "END-GOAL" "NEW-REPLY")
                     (harmless-edit-test-contents session)))
      (should-not (member "LATE-TURN" (harmless-edit-test-contents session))))))

(ert-deftest harmless-edit-prefix-wins-over-point ()
  (harmless-edit-test-env
    (let ((session (harmless-edit-test-make-session :cwd root))
          (messages (harmless-edit-test-turns
                     '("TURN-ONE" "TURN-TWO" "TURN-THREE"
                       "TURN-FOUR" "TURN-FIVE"))))
      (setf (harmless-session-messages session) messages)
      (harmless-ui-render-session session)
      (with-current-buffer (harmless-session-buffer session)
        (goto-char (point-min))
        (should (search-forward "TURN-ONE" nil t))
        (harmless-edit-test-answers nil "ELSEWHERE"
          (should (null (harmless-edit-prompt 2 "ELSEWHERE")))
          (should (equal '("Edit turn 2 and drop 4 turns? ")
                         harmless-edit-test-asked))
          (should (null harmless-edit-test-notes))
          (should (null harmless-edit-test-reads)))
        (should (eq messages (harmless-session-messages session)))
        (harmless-edit-test-answers t "EDITED-FOUR"
          (let ((current-prefix-arg '(4)))
            (should (= 2 (call-interactively #'harmless-edit-prompt))))
          (should (equal '("Edit turn 4 and drop 2 turns? ")
                         harmless-edit-test-asked))
          (should (equal '("Edited turn 4") harmless-edit-test-notes))
          (should harmless-edit-test-reads)))
      (should (equal '("TURN-ONE" "TURN-ONE-reply"
                       "TURN-TWO" "TURN-TWO-reply"
                       "TURN-THREE" "TURN-THREE-reply"
                       "EDITED-FOUR" "NEW-REPLY")
                     (harmless-edit-test-contents session)))
      (should-not (member "TURN-FOUR" (harmless-edit-test-contents session)))
      (should-not (member "TURN-FIVE" (harmless-edit-test-contents session))))))

(ert-deftest harmless-edit-asks-outside-the-transcript ()
  (harmless-edit-test-env
    (let ((session (harmless-edit-test-make-session :cwd root))
          (messages (harmless-edit-test-rich))
          (prompt nil))
      (setf (harmless-session-messages session) messages)
      (with-temp-buffer
        (setq default-directory "relative")
        (setq-local harmless--session session)
        (harmless-edit-test-answers t "NEW-GOAL"
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) "not a choice")))
            (harmless-edit-test-error
             "Nothing to edit"
             (lambda () (harmless-edit-prompt)))
            (should (null harmless-edit-test-asked))
            (should (null harmless-edit-test-reads))))
        (should (eq messages (harmless-session-messages session)))
        (harmless-edit-test-answers t "NEW-GOAL"
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (arg choices &rest _)
                       (setq prompt arg)
                       (car (nth 1 choices)))))
            (should (= 2 (harmless-edit-prompt)))
            (should harmless-edit-test-reads))
          (should (equal '("Edit turn 2 and drop 2 turns? ")
                         harmless-edit-test-asked))))
      (should (equal "Edit turn: " prompt))
      (should (equal '("EARLIER" "OLD-GOAL" "looking" "old body"
                       "NEW-GOAL" "NEW-REPLY")
                     (harmless-edit-test-contents session)))
      (should-not (member "MID-TURN" (harmless-edit-test-contents session)))
      (should-not (member "LATE-TURN" (harmless-edit-test-contents session)))
      (setf (harmless-session-messages session) messages)
      (harmless-ui-ensure-prompt-buffer session)
      (with-current-buffer (harmless-ui-ensure-prompt-buffer session)
        (harmless-edit-test-answers t "FRESH"
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt choices &rest _)
                       (car (car choices)))))
            (should (= 3 (harmless-edit-prompt)))
            (should harmless-edit-test-reads))))
      (should (equal '("EARLIER" "FRESH" "NEW-REPLY")
                     (harmless-edit-test-contents session)))
      (should-not (member "OLD-GOAL" (harmless-edit-test-contents session))))))

(ert-deftest harmless-edit-refuses-a-bad-turn-before-asking ()
  (harmless-edit-test-env
    (let ((session (harmless-edit-test-make-session :cwd root))
          (messages (harmless-edit-test-turns
                     '("OLD-GOAL" "MID-TURN" "LATE-TURN"))))
      (setf (harmless-session-messages session) messages)
      (with-temp-buffer
        (setq-local harmless--session session)
        (setq default-directory "relative")
        (harmless-edit-test-answers t "NEW-GOAL"
          (let ((current-prefix-arg 0))
            (harmless-edit-test-error
             "Choose a turn"
             (lambda () (call-interactively #'harmless-edit-prompt))))
          (should (null harmless-edit-test-asked))
          (should (null harmless-edit-test-reads))
          (should (eq messages (harmless-session-messages session))))))))

(ert-deftest harmless-edit-refuses-an-empty-prompt-before-asking ()
  (harmless-edit-test-env
    (let ((session (harmless-edit-test-make-session :cwd root))
          (messages (harmless-edit-test-turns '("OLD-GOAL" "LATE-TURN"))))
      (setf (harmless-session-messages session) messages)
      (setf (harmless-session-last-prompt-tokens session) 400000)
      (dolist (text (list nil "" "   " "\n" t))
        (harmless-edit-test-error
         "Prompt is empty"
         (lambda () (harmless-edit-session session 1 text)))
        (should (eq messages (harmless-session-messages session)))
        (should (= 400000 (harmless-session-last-prompt-tokens session)))
        (should (eq 'idle (harmless-session-status session))))
      (with-temp-buffer
        (setq-local harmless--session session)
        (setq default-directory "relative")
        (harmless-edit-test-answers t "   "
          (harmless-edit-test-error
           "Prompt is empty"
           (lambda () (harmless-edit-prompt 1)))
          (should harmless-edit-test-reads)
          (should (null harmless-edit-test-asked))))
      (should (eq messages (harmless-session-messages session))))))

(ert-deftest harmless-edit-refuses-an-empty-transcript ()
  (harmless-edit-test-env
    (let ((session (harmless-edit-test-make-session :cwd root)))
      (setf (harmless-session-last-prompt-tokens session) 400000)
      (dolist (messages (list nil
                              '()
                              ""
                              (vector (list :role :user :content "OLD-GOAL"))
                              (list (list :role :summary :content "EARLIER"))))
        (setf (harmless-session-messages session) messages)
        (harmless-edit-test-error
         "Nothing to edit"
         (lambda () (harmless-edit-session session 1 "NEW-GOAL")))
        (should (eq messages (harmless-session-messages session)))
        (should (= 400000 (harmless-session-last-prompt-tokens session)))
        (should (eq 'idle (harmless-session-status session))))
      (setf (harmless-session-messages session) nil)
      (with-temp-buffer
        (setq-local harmless--session session)
        (setq default-directory "relative")
        (let ((asked-choice nil))
          (harmless-edit-test-answers t "NEW-GOAL"
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _)
                         (setq asked-choice t)
                         "1. x")))
              (harmless-edit-test-error
               "Nothing to edit"
               (lambda () (harmless-edit-prompt)))
              (should (null harmless-edit-test-reads))
              (should (null harmless-edit-test-asked))))
          (should (null asked-choice)))))))

(ert-deftest harmless-edit-refuses-a-busy-session ()
  (harmless-edit-test-env
    (dolist (status '(streaming waiting-permission))
      (let ((session (harmless-edit-test-make-session :cwd root))
            (messages (harmless-edit-test-turns
                       '("OLD-GOAL" "MID-TURN" "LATE-TURN"))))
        (setf (harmless-session-status session) status)
        (harmless-edit-test-error
         "A turn is in progress"
         (lambda () (harmless-edit-session session 1 "")))
        (should (null (harmless-session-messages session)))
        (should (eq status (harmless-session-status session)))
        (setf (harmless-session-messages session) messages)
        (harmless-edit-test-error
         "A turn is in progress"
         (lambda () (harmless-edit-session session 1 "NEW-GOAL")))
        (should (eq messages (harmless-session-messages session)))
        (should (eq status (harmless-session-status session)))
        (with-temp-buffer
          (setq-local harmless--session session)
          (harmless-edit-test-answers t "NEW-GOAL"
            (harmless-edit-test-error
             "A turn is in progress"
             (lambda () (harmless-edit-prompt)))
            (should (null harmless-edit-test-reads))
            (should (null harmless-edit-test-asked))))))))

(ert-deftest harmless-edit-requires-a-session ()
  (harmless-edit-test-error
   "No Harmless session"
   (lambda () (harmless-edit-session nil 1 "NEW-GOAL")))
  (harmless-edit-test-env
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
      (with-temp-buffer
        (setq default-directory (file-name-as-directory root))
        (harmless-edit-test-error
         "No Harmless session"
         (lambda () (harmless-edit-prompt)))))))

(ert-deftest harmless-edit-requires-a-provider ()
  (harmless-edit-test-env
    (let* ((messages (harmless-edit-test-turns
                      '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
           (session (harmless-session--create
                     :id "edit-none"
                     :cwd root
                     :provider nil
                     :model "grok-4.6"
                     :messages nil
                     :status 'idle
                     :prompt-tokens 100
                     :last-prompt-tokens 400000
                     :last-completion-tokens 7)))
      (harmless-edit-test-error
       "No Harmless provider configured"
       (lambda () (harmless-edit-session session 1 "NEW-GOAL")))
      (should (null (harmless-session-messages session)))
      (should (= 400000 (harmless-session-last-prompt-tokens session)))
      (should (eq 'idle (harmless-session-status session)))
      (setf (harmless-session-messages session) messages)
      (harmless-edit-test-error
       "No Harmless provider configured"
       (lambda () (harmless-edit-session session 1 "NEW-GOAL")))
      (should (eq messages (harmless-session-messages session)))
      (should (= 400000 (harmless-session-last-prompt-tokens session)))
      (should (eq 'idle (harmless-session-status session)))
      (with-temp-buffer
        (setq-local harmless--session session)
        (let ((asked-choice nil))
          (harmless-edit-test-answers t "NEW-GOAL"
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _)
                         (setq asked-choice t)
                         "1. x")))
              (harmless-edit-test-error
               "No Harmless provider configured"
               (lambda () (harmless-edit-prompt)))
              (should (null harmless-edit-test-reads))
              (should (null harmless-edit-test-asked))))
          (should (null asked-choice)))))))

(ert-deftest harmless-edit-bad-threshold-leaves-the-transcript ()
  (harmless-edit-test-env
    (let ((session (harmless-edit-test-make-session :cwd root))
          (messages (harmless-edit-test-turns '("OLD-GOAL" "LATE-TURN")))
          (harmless-compact-threshold 80.0)
          (called nil))
      (setf (harmless-session-messages session) messages)
      (setf (harmless-edit-fake-script (harmless-session-provider session))
            (lambda (&rest _) (setq called t)))
      (harmless-edit-test-error
       "Compact threshold must be an integer from 1 to 100, or nil, not 80.0"
       (lambda () (harmless-edit-session session 1 "NEW-GOAL")))
      (harmless-edit-test-answers t "NEW-GOAL"
        (harmless-edit-test-error
         "Compact threshold must be an integer from 1 to 100, or nil, not 80.0"
         (lambda ()
           (with-temp-buffer
             (setq-local harmless--session session)
             (setq default-directory "relative")
             (harmless-edit-prompt 1))))
        (should (null harmless-edit-test-asked))
        (should (null harmless-edit-test-reads)))
      (should (null called))
      (should (eq messages (harmless-session-messages session)))
      (should (eq 'idle (harmless-session-status session))))))

(ert-deftest harmless-edit-provider-error-keeps-the-new-prompt ()
  (harmless-edit-test-env
    (let* ((root-messages (file-exists-p "/messages.jsonl"))
           (root-summary (file-exists-p "/summary.json"))
           (provider (harmless-edit-make-fake
                      :name "fake"
                      :host "none"
                      :script (lambda (&rest _) (error "provider broke"))))
           (messages (harmless-edit-test-turns
                      '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
           (session (progn
                      (setq harmless-providers (list provider))
                      (harmless-session-new
                       :cwd root
                       :provider provider
                       :model "grok-4.6"
                       :reasoning-effort "high")))
           (notes nil))
      (setf (harmless-session-messages session) messages)
      (setf (harmless-session-prompt-tokens session) 100)
      (setf (harmless-session-completion-tokens session) 9)
      (setf (harmless-session-last-prompt-tokens session) 400000)
      (setf (harmless-session-last-completion-tokens session) 7)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) notes))))
        (harmless-edit-test-error
         "provider broke"
         (lambda () (harmless-edit-session session 2 "NEW-GOAL"))))
      (should (null notes))
      (should (eq (nth 0 messages) (nth 0 (harmless-session-messages session))))
      (should (eq (nth 1 messages) (nth 1 (harmless-session-messages session))))
      (should (equal "NEW-GOAL"
                     (plist-get (nth 2 (harmless-session-messages session))
                                :content)))
      (should (equal '(:user :assistant :user)
                     (harmless-edit-test-roles session)))
      (should (eq 'streaming (harmless-session-status session)))
      (should (= 0 (harmless-session-last-prompt-tokens session)))
      (should (= 0 (harmless-session-last-completion-tokens session)))
      (should (= 100 (harmless-session-prompt-tokens session)))
      (should (= 9 (harmless-session-completion-tokens session)))
      (harmless-edit-test-assert-stored
       session harmless-directory root-messages root-summary)
      (let ((file (harmless-edit-test-transcript session)))
        (should (string-search "OLD-GOAL" file))
        (should (string-search "NEW-GOAL" file))
        (should-not (string-search "MID-TURN" file))
        (should-not (string-search "LATE-TURN" file))
        (should-not (string-search "NEW-REPLY" file))))))

(ert-deftest harmless-edit-relative-cwd-restores-the-transcript ()
  (harmless-edit-test-env
    (let* ((root-messages (file-exists-p "/messages.jsonl"))
           (default-directory (file-name-as-directory root))
           (called nil)
           (provider (harmless-edit-make-fake
                      :name "fake"
                      :host "none"
                      :script (lambda (&rest _) (setq called t)))))
      (dolist (case '(("proj" . "Directory must be absolute, not \"proj\"")
                      ("" . "Directory must be absolute, not \"\"")
                      ("relative/" . "Directory must be absolute, not \"relative/\"")))
        (let* ((cwd (car case))
               (messages (harmless-edit-test-turns
                          '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
               (session (harmless-session--create
                         :id "edit-rel"
                         :cwd cwd
                         :provider provider
                         :model "grok-4.6"
                         :messages messages
                         :status 'idle
                         :last-prompt-tokens 400000
                         :last-completion-tokens 7
                         :updated-at "2020-01-01T00:00:00Z"))
               (events nil))
          (let ((harmless-event-functions
                 (cons (lambda (_session event) (push event events))
                       harmless-event-functions)))
            (should (null (harmless-edit-session session 1 "NEW-GOAL"))))
          (should (eq messages (harmless-session-messages session)))
          (should (= 400000 (harmless-session-last-prompt-tokens session)))
          (should (= 7 (harmless-session-last-completion-tokens session)))
          (should (equal "2020-01-01T00:00:00Z"
                         (harmless-session-updated-at session)))
          (should (eq 'error (harmless-session-status session)))
          (should (member (list :error (cdr case)) events))
          (should (null called))
          (should (eq root-messages (file-exists-p "/messages.jsonl")))
          (should-not (file-exists-p
                       (expand-file-name "messages.jsonl" root))))))))

(ert-deftest harmless-edit-root-cwd-stays-under-the-data-directory ()
  (harmless-edit-test-env
    (let ((root-messages (file-exists-p "/messages.jsonl"))
          (root-summary (file-exists-p "/summary.json"))
          (session (harmless-edit-test-make-session :cwd "/")))
      (setf (harmless-session-messages session)
            (harmless-edit-test-turns '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
      (harmless-edit-session session 1 "NEW-GOAL")
      (harmless-edit-test-assert-stored
       session harmless-directory root-messages root-summary)
      (should (equal "/" (harmless-session-cwd session)))
      (let ((file (harmless-edit-test-transcript session)))
        (should (string-search "NEW-GOAL" file))
        (should (string-search "NEW-REPLY" file))
        (should-not (string-search "OLD-GOAL" file))
        (should-not (string-search "LATE-TURN" file))))))

(ert-deftest harmless-edit-trailing-slash-stays-under-the-data-directory ()
  (harmless-edit-test-env
    (let* ((slash (file-name-as-directory root))
           (root-messages (file-exists-p "/messages.jsonl"))
           (root-summary (file-exists-p "/summary.json"))
           (session (harmless-edit-test-make-session :cwd slash)))
      (should (string-suffix-p "/" slash))
      (setf (harmless-session-messages session)
            (harmless-edit-test-turns '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
      (harmless-edit-session session 2 "NEW-GOAL")
      (harmless-edit-test-assert-stored
       session harmless-directory root-messages root-summary)
      (let ((file (harmless-edit-test-transcript session)))
        (should (string-search "OLD-GOAL" file))
        (should (string-search "NEW-GOAL" file))
        (should-not (string-search "LATE-TURN" file)))
      (should-not (file-exists-p (expand-file-name "messages.jsonl" slash))))))

(ert-deftest harmless-edit-missing-directory-is-not-the-filesystem-root ()
  (harmless-edit-test-env
    (let* ((missing (expand-file-name "no-such-project" root))
           (root-messages (file-exists-p "/messages.jsonl"))
           (root-summary (file-exists-p "/summary.json"))
           (session (harmless-edit-test-make-session :cwd missing)))
      (should-not (file-exists-p missing))
      (setf (harmless-session-messages session)
            (harmless-edit-test-turns '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
      (harmless-edit-session session 1 "NEW-GOAL")
      (should-not (file-exists-p missing))
      (should-not (file-directory-p missing))
      (harmless-edit-test-assert-stored
       session harmless-directory root-messages root-summary)
      (let ((file (harmless-edit-test-transcript session)))
        (should (string-search "NEW-GOAL" file))
        (should-not (string-search "OLD-GOAL" file))
        (should-not (string-search "LATE-TURN" file))))))

(ert-deftest harmless-edit-detached-chat-saves-under-the-data-directory ()
  (harmless-edit-test-env
    (let* ((scratch (make-temp-file "harmless-edit-scratch-" t))
           (default-directory (file-name-as-directory scratch))
           (before (directory-files scratch nil directory-files-no-dot-files-regexp))
           (root-messages (file-exists-p "/messages.jsonl"))
           (root-summary (file-exists-p "/summary.json"))
           (session (harmless-edit-test-make-session :detached t)))
      (should (null (harmless-session-cwd session)))
      (setf (harmless-session-messages session)
            (harmless-edit-test-turns '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
      (harmless-edit-session session 2 "NEW-GOAL")
      (harmless-edit-test-assert-stored
       session harmless-directory root-messages root-summary)
      (should (string-search "/_detached/"
                             (file-name-as-directory (harmless-session-dir session))))
      (let ((file (harmless-edit-test-transcript session)))
        (should (string-search "OLD-GOAL" file))
        (should (string-search "NEW-GOAL" file))
        (should-not (string-search "LATE-TURN" file)))
      (should (equal before
                     (directory-files scratch nil
                                      directory-files-no-dot-files-regexp)))
      (should-not (file-exists-p
                   (expand-file-name "messages.jsonl" scratch))))))

(provide 'harmless-edit-tests)
