;;; harmless-rewind-tests.el --- Tests for transcript rewind -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(cl-defstruct (harmless-rewind-fake
               (:include harmless-provider)
               (:constructor harmless-rewind-make-fake)))

(defun harmless-rewind-test-error (text thunk)
  "Assert that THUNK signals an error whose message is TEXT."
  (let ((err (should-error (funcall thunk) :type 'error)))
    (should (equal (error-message-string err) text))))

(defun harmless-rewind-test-turns (texts)
  "Return a user message and an assistant reply for each string in TEXTS."
  (cl-mapcan (lambda (text)
               (list (list :role :user :content text)
                     (list :role :assistant
                           :content (concat text "-reply"))))
             texts))

(defun harmless-rewind-test-rich ()
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

(defun harmless-rewind-test-roles (session)
  "Return the role of each message in SESSION."
  (mapcar (lambda (msg) (plist-get msg :role))
          (harmless-session-messages session)))

(defun harmless-rewind-test-transcript (session)
  "Return the saved messages file for SESSION."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "messages.jsonl" (harmless-session-dir session)))
    (buffer-string)))

(defun harmless-rewind-test-assert-stored (session data-dir root-messages root-summary)
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

(defun harmless-rewind-test-make-session (&rest args)
  "Create a session on a dummy provider.
ARGS are passed to `harmless-session-new'."
  (let ((provider (harmless-rewind-make-fake :name "fake" :host "none")))
    (setq harmless-providers (list provider))
    (apply #'harmless-session-new
           :provider provider
           :model "grok-4.6"
           :reasoning-effort "high"
           args)))

(defmacro harmless-rewind-test-env (&rest body)
  "Run BODY with a temp Harmless directory and a fresh session table."
  (declare (indent 0))
  `(let* ((root (make-temp-file "harmless-rewind-" t))
          (harmless-directory (expand-file-name ".harmless" root))
          (harmless--sessions (make-hash-table :test 'equal))
          (harmless-providers nil))
     ,@body))

(defmacro harmless-rewind-test-answers (answer &rest body)
  "Run BODY with `y-or-n-p' returning ANSWER.
`harmless-rewind-test-asked' collects the questions.
`harmless-rewind-test-notes' collects `message' text."
  (declare (indent 1))
  `(let (harmless-rewind-test-asked
         harmless-rewind-test-notes)
     (cl-letf (((symbol-function 'y-or-n-p)
                (lambda (prompt)
                  (push prompt harmless-rewind-test-asked)
                  ,answer))
               ((symbol-function 'message)
                (lambda (fmt &rest args)
                  (push (apply #'format fmt args)
                        harmless-rewind-test-notes))))
       ,@body)))

(ert-deftest harmless-rewind-split-keeps-the-chosen-turn ()
  (let* ((summary (list :role :summary :content "EARLIER"))
         (tool (list :role :tool :id "1" :name "read_file" :content "old body"))
         (call (list :role :assistant :content "looking"
                     :tool-calls (list (list :id "1" :name "read_file"
                                             :args '(:path "old.txt")))))
         (first (list :role :user :content "OLD-GOAL"))
         (second (list :role 'user :content "MID-TURN"))
         (third (list :role "user" :content "LATE-TURN"))
         (reply (list :role :assistant :content "LATE-TURN-reply"))
         (messages (list summary first call tool second third reply))
         (kept (harmless-rewind-split messages 1)))
    (should (eq summary (nth 0 kept)))
    (should (eq first (nth 1 kept)))
    (should (eq call (nth 2 kept)))
    (should (eq tool (nth 3 kept)))
    (should (equal '("EARLIER" "OLD-GOAL" "looking" "old body")
                   (mapcar (lambda (msg) (plist-get msg :content)) kept)))
    (should (eq tool (nth 0 (harmless-rewind-split
                             (list tool first second) 1))))
    (should (eq first (nth 1 (harmless-rewind-split
                              (list tool first second) 1))))
    (harmless-rewind-test-error
     "Nothing to rewind"
     (lambda () (harmless-rewind-split (list summary first call) 1)))
    (harmless-rewind-test-error
     "Nothing to rewind"
     (lambda () (harmless-rewind-split messages 3)))
    (harmless-rewind-test-error
     "Nothing to rewind"
     (lambda () (harmless-rewind-split messages 9)))
    (harmless-rewind-test-error
     "Nothing to rewind"
     (lambda () (harmless-rewind-split nil 1)))
    (harmless-rewind-test-error
     "Nothing to rewind"
     (lambda () (harmless-rewind-split '() 1)))
    (harmless-rewind-test-error
     "Nothing to rewind"
     (lambda () (harmless-rewind-split "" 1)))
    (harmless-rewind-test-error
     "Nothing to rewind"
     (lambda () (harmless-rewind-split (vector first) 1)))
    (dolist (keep '(0 -1 t "1" 1.0))
      (harmless-rewind-test-error
       "Keep at least one turn"
       (lambda () (harmless-rewind-split messages keep))))))

(ert-deftest harmless-rewind-key-and-menu ()
  (should (eq (lookup-key harmless-session-mode-map (kbd "w"))
              #'harmless-rewind))
  (should-not (eq (lookup-key harmless-prompt-mode-map (kbd "w"))
                  #'harmless-rewind))
  (should (equal (transient--suffix-key
                  (transient-get-suffix 'harmless-menu 'harmless-rewind))
                 "w")))

(ert-deftest harmless-rewind-drops-later-turns ()
  (harmless-rewind-test-env
    (let* ((root-messages (file-exists-p "/messages.jsonl"))
           (root-summary (file-exists-p "/summary.json"))
           (session (harmless-rewind-test-make-session :cwd root))
           (messages (harmless-rewind-test-rich))
           (summary (nth 0 messages))
           (user (nth 1 messages))
           (call (nth 2 messages))
           (tool (nth 3 messages))
           (harmless-usage-context-windows '(("grok-4.6" . 500000)))
           (harmless-openai--context-windows nil)
           (harmless-compact-threshold 80)
           (harmless-compact-keep-turns 1))
      (setf (harmless-session-messages session) messages)
      (setf (harmless-session-last-prompt-tokens session) 400000)
      (setf (harmless-session-last-completion-tokens session) 7)
      (setf (harmless-session-prompt-tokens session) 100)
      (setf (harmless-session-completion-tokens session) 9)
      (setf (harmless-session-plan-mode session) t)
      (setf (harmless-session-status session) 'error)
      (should (harmless-compact-needed-p session))
      (should (null (harmless-session-buffer session)))
      (harmless-rewind-test-answers t
        (should (= 1 (harmless-rewind-session session 2)))
        (should (equal '("Dropped 1 turn") harmless-rewind-test-notes)))
      (let ((kept (harmless-session-messages session)))
        (should (eq summary (nth 0 kept)))
        (should (eq user (nth 1 kept)))
        (should (eq call (nth 2 kept)))
        (should (eq tool (nth 3 kept)))
        (should (eq (nth 4 messages) (nth 4 kept)))
        (should (equal '(:summary :user :assistant :tool :user :assistant)
                       (harmless-rewind-test-roles session))))
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
      (should-not (harmless-compact-needed-p session))
      (should (null (harmless-session-buffer session)))
      (harmless-rewind-test-assert-stored
       session harmless-directory root-messages root-summary)
      (let ((file (harmless-rewind-test-transcript session)))
        (should (string-search "EARLIER" file))
        (should (string-search "OLD-GOAL" file))
        (should (string-search "old body" file))
        (should (string-search "old.txt" file))
        (should (string-search "MID-TURN" file))
        (should-not (string-search "LATE-TURN" file)))
      (let ((kept (harmless-session-messages session)))
        (harmless-rewind-test-error
         "Nothing to rewind"
         (lambda () (harmless-rewind-session session 2)))
        (should (eq kept (harmless-session-messages session)))
        (should (= 0 (harmless-session-last-prompt-tokens session)))))))

(ert-deftest harmless-rewind-marks-the-turn-at-point ()
  (harmless-rewind-test-env
    (let ((session (harmless-rewind-test-make-session :cwd root))
          (messages (list (list :role :summary :content "EARLIER")
                          (list :role :user :content "OLD-GOAL")
                          (list :role :assistant :content "looking"
                                :tool-calls (list (list :id "1" :name "read_file"
                                                        :args '(:path "old.txt"))))
                          (list :role :tool :id "1" :name "read_file"
                                :content "old body")
                          (list :role 'user :content "MID-TURN")
                          (list :role :assistant :content "MID-TURN-reply")
                          (list :role "user" :content "LATE-TURN")
                          (list :role :assistant :content "LATE-TURN-reply"))))
      (setf (harmless-session-messages session) messages)
      (harmless-ui-render-session session)
      (with-current-buffer (harmless-session-buffer session)
        (goto-char (point-min))
        (should (search-forward "EARLIER" nil t))
        (should (null (get-text-property (match-beginning 0) 'harmless-turn)))
        (goto-char (point-min))
        (should (search-forward "old body" nil t))
        (should (= 1 (get-text-property (match-beginning 0) 'harmless-turn)))
        (goto-char (point-min))
        (should (search-forward "MID-TURN" nil t))
        (should (= 2 (get-text-property (match-beginning 0) 'harmless-turn)))
        (goto-char (point-min))
        (should (search-forward "LATE-TURN-reply" nil t))
        (should (= 3 (get-text-property (match-beginning 0) 'harmless-turn)))
        (goto-char (point-max))
        (should (= 3 (harmless-rewind--turn-at-point)))))))

(ert-deftest harmless-rewind-command-uses-the-turn-at-point ()
  (harmless-rewind-test-env
    (let ((session (harmless-rewind-test-make-session :cwd root))
          (messages (harmless-rewind-test-rich)))
      (setf (harmless-session-messages session) messages)
      (setf (harmless-session-last-prompt-tokens session) 400000)
      (harmless-ui-render-session session)
      (with-current-buffer (harmless-session-buffer session)
        (goto-char (point-min))
        (should (search-forward "EARLIER" nil t))
        (goto-char (match-beginning 0))
        (harmless-rewind-test-answers t
          (harmless-rewind-test-error
           "Point is not on a turn"
           (lambda () (harmless-rewind)))
          (should (null harmless-rewind-test-asked)))
        (should (eq messages (harmless-session-messages session)))
        (goto-char (point-max))
        (harmless-rewind-test-answers t
          (harmless-rewind-test-error
           "Nothing to rewind"
           (lambda () (harmless-rewind)))
          (should (null harmless-rewind-test-asked)))
        (goto-char (point-min))
        (should (search-forward "old body" nil t))
        (goto-char (match-beginning 0))
        (harmless-rewind-test-answers t
          (should (= 2 (harmless-rewind)))
          (should (equal '("Rewind to turn 1 and drop 2 turns? ")
                         harmless-rewind-test-asked))
          (should (equal '("Dropped 2 turns") harmless-rewind-test-notes))))
      (should (equal '(:summary :user :assistant :tool)
                     (harmless-rewind-test-roles session)))
      (should (= 0 (harmless-session-last-prompt-tokens session)))
      (with-current-buffer (harmless-session-buffer session)
        (should (string-search "OLD-GOAL" (buffer-string)))
        (should (string-search "old body" (buffer-string)))
        (should-not (string-search "LATE-TURN" (buffer-string)))))))

(ert-deftest harmless-rewind-prefix-wins-over-point ()
  (harmless-rewind-test-env
    (let ((session (harmless-rewind-test-make-session :cwd root))
          (messages (harmless-rewind-test-turns
                     '("TURN-ONE" "TURN-TWO" "TURN-THREE" "TURN-FOUR" "TURN-FIVE"))))
      (setf (harmless-session-messages session) messages)
      (harmless-ui-render-session session)
      (with-current-buffer (harmless-session-buffer session)
        (goto-char (point-min))
        (should (search-forward "TURN-ONE" nil t))
        (harmless-rewind-test-answers nil
          (should (null (harmless-rewind 2)))
          (should (equal '("Rewind to turn 2 and drop 3 turns? ")
                         harmless-rewind-test-asked))
          (should (null harmless-rewind-test-notes)))
        (should (eq messages (harmless-session-messages session)))
        (harmless-rewind-test-answers t
          (let ((current-prefix-arg '(4)))
            (should (= 1 (call-interactively #'harmless-rewind))))
          (should (equal '("Rewind to turn 4 and drop 1 turn? ")
                         harmless-rewind-test-asked))
          (should (equal '("Dropped 1 turn") harmless-rewind-test-notes))))
      (should (equal '("TURN-ONE" "TURN-ONE-reply"
                       "TURN-TWO" "TURN-TWO-reply"
                       "TURN-THREE" "TURN-THREE-reply"
                       "TURN-FOUR" "TURN-FOUR-reply")
                     (mapcar (lambda (msg) (plist-get msg :content))
                             (harmless-session-messages session))))
      (should-not (member "TURN-FIVE"
                          (mapcar (lambda (msg) (plist-get msg :content))
                                  (harmless-session-messages session)))))))

(ert-deftest harmless-rewind-asks-outside-the-transcript ()
  (harmless-rewind-test-env
    (let ((session (harmless-rewind-test-make-session :cwd root))
          (messages (harmless-rewind-test-rich)))
      (setf (harmless-session-messages session) messages)
      (with-temp-buffer
        (setq default-directory "relative")
        (setq-local harmless--session session)
        (harmless-rewind-test-answers t
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt choices &rest _)
                       (car (nth 1 choices)))))
            (should (= 1 (harmless-rewind))))
          (should (equal '("Rewind to turn 2 and drop 1 turn? ")
                         harmless-rewind-test-asked))))
      (should (equal '("EARLIER" "OLD-GOAL" "looking" "old body"
                       "MID-TURN" "MID-TURN-reply")
                     (mapcar (lambda (msg) (plist-get msg :content))
                             (harmless-session-messages session))))
      (should-not (member "LATE-TURN"
                          (mapcar (lambda (msg) (plist-get msg :content))
                                  (harmless-session-messages session))))
      (harmless-ui-ensure-prompt-buffer session)
      (setf (harmless-session-messages session) messages)
      (with-current-buffer (harmless-ui-ensure-prompt-buffer session)
        (harmless-rewind-test-answers t
          (let ((harmless-rewind-test-read nil))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt choices &rest _)
                         (setq harmless-rewind-test-read t)
                         (car (car choices)))))
              (should (= 2 (harmless-rewind)))
              (should harmless-rewind-test-read)))))
      (should (equal '("EARLIER" "OLD-GOAL" "looking" "old body")
                     (mapcar (lambda (msg) (plist-get msg :content))
                             (harmless-session-messages session)))))))

(ert-deftest harmless-rewind-refuses-a-bad-keep-before-asking ()
  (harmless-rewind-test-env
    (let ((session (harmless-rewind-test-make-session :cwd root))
          (messages (harmless-rewind-test-turns
                     '("OLD-GOAL" "MID-TURN" "LATE-TURN"))))
      (setf (harmless-session-messages session) messages)
      (with-temp-buffer
        (setq-local harmless--session session)
        (setq default-directory "relative")
        (let ((asked nil))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (prompt)
                       (push prompt asked)
                       t)))
            (let ((current-prefix-arg 0))
              (harmless-rewind-test-error
               "Keep at least one turn"
               (lambda () (call-interactively #'harmless-rewind)))))
            (should (null asked))
            (should (eq messages (harmless-session-messages session))))))))

(ert-deftest harmless-rewind-refuses-an-empty-transcript ()
  (harmless-rewind-test-env
    (let ((session (harmless-rewind-test-make-session :cwd root)))
      (setf (harmless-session-last-prompt-tokens session) 400000)
      (dolist (messages (list nil
                              '()
                              ""
                              (vector (list :role :user :content "OLD-GOAL"))
                              (harmless-rewind-test-turns '("ONLY-TURN"))
                              (list (list :role :summary :content "EARLIER")
                                    (list :role :user :content "ONLY-TURN"))))
        (setf (harmless-session-messages session) messages)
        (harmless-rewind-test-error
         "Nothing to rewind"
         (lambda () (harmless-rewind-session session 1)))
        (should (eq messages (harmless-session-messages session)))
        (should (= 400000 (harmless-session-last-prompt-tokens session)))
        (should (eq 'idle (harmless-session-status session)))))))

(ert-deftest harmless-rewind-refuses-a-busy-session ()
  (harmless-rewind-test-env
    (dolist (status '(streaming waiting-permission))
      (let ((session (harmless-rewind-test-make-session :cwd root))
            (messages (harmless-rewind-test-turns
                       '("OLD-GOAL" "MID-TURN" "LATE-TURN"))))
        (setf (harmless-session-status session) status)
        (harmless-rewind-test-error
         "A turn is in progress"
         (lambda () (harmless-rewind-session session 1)))
        (should (null (harmless-session-messages session)))
        (should (eq status (harmless-session-status session)))
        (setf (harmless-session-messages session) messages)
        (harmless-rewind-test-error
         "A turn is in progress"
         (lambda () (harmless-rewind-session session 1)))
        (should (eq messages (harmless-session-messages session)))
        (should (eq status (harmless-session-status session)))))))

(ert-deftest harmless-rewind-requires-a-session ()
  (harmless-rewind-test-error
   "No Harmless session"
   (lambda () (harmless-rewind-session nil 1)))
  (harmless-rewind-test-env
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
      (with-temp-buffer
        (setq default-directory (file-name-as-directory root))
        (harmless-rewind-test-error
         "No Harmless session"
         (lambda () (harmless-rewind)))))))

(ert-deftest harmless-rewind-works-without-a-provider ()
  (harmless-rewind-test-env
    (let* ((root-messages (file-exists-p "/messages.jsonl"))
           (root-summary (file-exists-p "/summary.json"))
           (messages (harmless-rewind-test-turns
                      '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
           (session (harmless-session--create
                     :id "rewind-none"
                     :cwd root
                     :provider nil
                     :model "grok-4.6"
                     :messages messages
                     :status 'idle
                     :prompt-tokens 100
                     :completion-tokens 9
                     :last-prompt-tokens 400000
                     :last-completion-tokens 7)))
      (should (= 2 (harmless-rewind-session session 1)))
      (should (eq (nth 0 messages) (nth 0 (harmless-session-messages session))))
      (should (eq (nth 1 messages) (nth 1 (harmless-session-messages session))))
      (should (eq 'idle (harmless-session-status session)))
      (should (= 0 (harmless-session-last-prompt-tokens session)))
      (should (= 100 (harmless-session-prompt-tokens session)))
      (harmless-rewind-test-assert-stored
       session harmless-directory root-messages root-summary)
      (let ((file (harmless-rewind-test-transcript session)))
        (should (string-search "OLD-GOAL" file))
        (should-not (string-search "MID-TURN" file))
        (should-not (string-search "LATE-TURN" file))))))

(ert-deftest harmless-rewind-relative-cwd-restores-the-transcript ()
  (harmless-rewind-test-env
    (let ((root-messages (file-exists-p "/messages.jsonl"))
          (default-directory (file-name-as-directory root)))
      (dolist (case '(("proj" . "Directory must be absolute, not \"proj\"")
                      ("" . "Directory must be absolute, not \"\"")
                      ("relative/" . "Directory must be absolute, not \"relative/\"")))
        (let* ((cwd (car case))
               (messages (harmless-rewind-test-turns
                          '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
               (session (harmless-session--create
                         :id "rewind-rel"
                         :cwd cwd
                         :provider nil
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
            (should (null (harmless-rewind-session session 1))))
          (should (eq messages (harmless-session-messages session)))
          (should (= 400000 (harmless-session-last-prompt-tokens session)))
          (should (= 7 (harmless-session-last-completion-tokens session)))
          (should (equal "2020-01-01T00:00:00Z"
                         (harmless-session-updated-at session)))
          (should (eq 'error (harmless-session-status session)))
          (should (member (list :error (cdr case)) events))
          (should (eq root-messages (file-exists-p "/messages.jsonl")))
          (should-not (file-exists-p
                       (expand-file-name "messages.jsonl" root))))))))

(ert-deftest harmless-rewind-root-cwd-stays-under-the-data-directory ()
  (harmless-rewind-test-env
    (let ((root-messages (file-exists-p "/messages.jsonl"))
          (root-summary (file-exists-p "/summary.json"))
          (session (harmless-rewind-test-make-session :cwd "/")))
      (setf (harmless-session-messages session)
            (harmless-rewind-test-turns '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
      (harmless-rewind-session session 1)
      (harmless-rewind-test-assert-stored
       session harmless-directory root-messages root-summary)
      (should (equal "/" (harmless-session-cwd session)))
      (let ((file (harmless-rewind-test-transcript session)))
        (should (string-search "OLD-GOAL" file))
        (should-not (string-search "LATE-TURN" file))))))

(ert-deftest harmless-rewind-trailing-slash-stays-under-the-data-directory ()
  (harmless-rewind-test-env
    (let* ((slash (file-name-as-directory root))
           (root-messages (file-exists-p "/messages.jsonl"))
           (root-summary (file-exists-p "/summary.json"))
           (session (harmless-rewind-test-make-session :cwd slash)))
      (should (string-suffix-p "/" slash))
      (setf (harmless-session-messages session)
            (harmless-rewind-test-turns '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
      (harmless-rewind-session session 1)
      (harmless-rewind-test-assert-stored
       session harmless-directory root-messages root-summary)
      (let ((file (harmless-rewind-test-transcript session)))
        (should (string-search "OLD-GOAL" file))
        (should-not (string-search "LATE-TURN" file)))
      (should-not (file-exists-p (expand-file-name "messages.jsonl" slash))))))

(ert-deftest harmless-rewind-missing-directory-is-not-the-filesystem-root ()
  (harmless-rewind-test-env
    (let* ((missing (expand-file-name "no-such-project" root))
           (root-messages (file-exists-p "/messages.jsonl"))
           (root-summary (file-exists-p "/summary.json"))
           (session (harmless-rewind-test-make-session :cwd missing)))
      (should-not (file-exists-p missing))
      (setf (harmless-session-messages session)
            (harmless-rewind-test-turns '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
      (harmless-rewind-session session 1)
      (should-not (file-exists-p missing))
      (should-not (file-directory-p missing))
      (harmless-rewind-test-assert-stored
       session harmless-directory root-messages root-summary)
      (should (string-search "OLD-GOAL"
                             (harmless-rewind-test-transcript session)))
      (should-not (string-search "LATE-TURN"
                                 (harmless-rewind-test-transcript session))))))

(ert-deftest harmless-rewind-detached-chat-saves-under-the-data-directory ()
  (harmless-rewind-test-env
    (let* ((scratch (make-temp-file "harmless-rewind-scratch-" t))
           (default-directory (file-name-as-directory scratch))
           (before (directory-files scratch nil directory-files-no-dot-files-regexp))
           (root-messages (file-exists-p "/messages.jsonl"))
           (root-summary (file-exists-p "/summary.json"))
           (session (harmless-rewind-test-make-session :detached t)))
      (should (null (harmless-session-cwd session)))
      (setf (harmless-session-messages session)
            (harmless-rewind-test-turns '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
      (harmless-rewind-session session 1)
      (harmless-rewind-test-assert-stored
       session harmless-directory root-messages root-summary)
      (should (string-search "/_detached/"
                             (file-name-as-directory (harmless-session-dir session))))
      (let ((file (harmless-rewind-test-transcript session)))
        (should (string-search "OLD-GOAL" file))
        (should-not (string-search "LATE-TURN" file)))
      (should (equal before
                     (directory-files scratch nil
                                      directory-files-no-dot-files-regexp)))
      (should-not (file-exists-p
                   (expand-file-name "messages.jsonl" scratch))))))

(provide 'harmless-rewind-tests)
