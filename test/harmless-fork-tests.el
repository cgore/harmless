;;; harmless-fork-tests.el --- Tests for forking a session -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(cl-defstruct (harmless-fork-fake
               (:include harmless-provider)
               (:constructor harmless-fork-make-fake)))

(defun harmless-fork-test-error (text thunk)
  "Assert that THUNK signals an error whose message is TEXT."
  (let ((err (should-error (funcall thunk) :type 'error)))
    (should (equal (error-message-string err) text))))

(defun harmless-fork-test-turns (texts)
  "Return a user message and an assistant reply for each string in TEXTS."
  (cl-mapcan (lambda (text)
               (list (list :role :user :content text)
                     (list :role :assistant
                           :content (concat text "-reply"))))
             texts))

(defun harmless-fork-test-rich ()
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

(defun harmless-fork-test-roles (session)
  "Return the role of each message in SESSION."
  (mapcar (lambda (msg) (plist-get msg :role))
          (harmless-session-messages session)))

(defun harmless-fork-test-contents (session)
  "Return the content of each message in SESSION."
  (mapcar (lambda (msg) (plist-get msg :content))
          (harmless-session-messages session)))

(defun harmless-fork-test-file (session name)
  "Return the saved NAME file for SESSION."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name name (harmless-session-dir session)))
    (buffer-string)))

(defun harmless-fork-test-transcript (session)
  "Return the saved messages file for SESSION."
  (harmless-fork-test-file session "messages.jsonl"))

(defun harmless-fork-test-dirs ()
  "Return the saved session directories."
  (let ((root (harmless-sessions-root))
        dirs)
    (when (file-directory-p root)
      (dolist (cwd-dir (directory-files root t directory-files-no-dot-files-regexp))
        (when (file-directory-p cwd-dir)
          (dolist (sid (directory-files cwd-dir t directory-files-no-dot-files-regexp))
            (when (file-exists-p (expand-file-name "summary.json" sid))
              (push sid dirs))))))
    dirs))

(defun harmless-fork-test-assert-stored (session data-dir root-messages root-summary)
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

(defun harmless-fork-test-make-session (&rest args)
  "Create a session on a dummy provider.
ARGS are passed to `harmless-session-new'."
  (let ((provider (harmless-fork-make-fake :name "fake" :host "none")))
    (setq harmless-providers (list provider))
    (apply #'harmless-session-new
           :provider provider
           :model "grok-4.6"
           :reasoning-effort "high"
           args)))

(defmacro harmless-fork-test-env (&rest body)
  "Run BODY with a temp Harmless directory and a fresh session table."
  (declare (indent 0))
  `(let* ((root (make-temp-file "harmless-fork-" t))
          (harmless-directory (expand-file-name ".harmless" root))
          (harmless--sessions (make-hash-table :test 'equal))
          (harmless-providers nil)
          (harmless-event-functions nil))
     ,@body))

(defmacro harmless-fork-test-answers (answer &rest body)
  "Run BODY with `y-or-n-p' returning ANSWER.
`harmless-fork-test-asked' collects the questions.
`harmless-fork-test-notes' collects `message' text."
  (declare (indent 1))
  `(let (harmless-fork-test-asked
         harmless-fork-test-notes)
     (cl-letf (((symbol-function 'y-or-n-p)
                (lambda (prompt)
                  (push prompt harmless-fork-test-asked)
                  ,answer))
               ((symbol-function 'message)
                (lambda (fmt &rest args)
                  (push (apply #'format fmt args)
                        harmless-fork-test-notes))))
       ,@body)))

(ert-deftest harmless-fork-split-keeps-the-chosen-turn ()
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
         (only (list tool first reply)))
    (let ((kept (harmless-fork-split messages 1)))
      (should (eq summary (nth 0 kept)))
      (should (eq first (nth 1 kept)))
      (should (eq call (nth 2 kept)))
      (should (eq tool (nth 3 kept)))
      (should (null (nth 4 kept))))
    (let ((kept (harmless-fork-split messages 2)))
      (should (eq second (car (last kept))))
      (should (eq tool (nth 3 kept)))
      (should-not (member third kept)))
    (should (equal messages (harmless-fork-split messages 3)))
    (should (eq reply (car (last (harmless-fork-split messages 3)))))
    (should (equal only (harmless-fork-split only 1)))
    (should (eq tool (car (harmless-fork-split only 1))))
    (should (eq reply (car (last (harmless-fork-split only 1)))))
    (should (equal (list summary first reply)
                   (harmless-fork-split (list summary first reply) 1)))
    (harmless-fork-test-error
     "Nothing to fork"
     (lambda () (harmless-fork-split messages 4)))
    (harmless-fork-test-error
     "Nothing to fork"
     (lambda () (harmless-fork-split messages 9)))
    (harmless-fork-test-error
     "Nothing to fork"
     (lambda () (harmless-fork-split nil 1)))
    (harmless-fork-test-error
     "Nothing to fork"
     (lambda () (harmless-fork-split '() 1)))
    (harmless-fork-test-error
     "Nothing to fork"
     (lambda () (harmless-fork-split "" 1)))
    (harmless-fork-test-error
     "Nothing to fork"
     (lambda () (harmless-fork-split (vector first) 1)))
    (harmless-fork-test-error
     "Nothing to fork"
     (lambda () (harmless-fork-split (list summary) 1)))
    (dolist (turn '(0 -1 t "1" 1.0))
      (harmless-fork-test-error
       "Choose a turn"
       (lambda () (harmless-fork-split messages turn))))
    (harmless-fork-test-error
     "Choose a turn"
     (lambda () (harmless-fork-split "" 0)))
    (harmless-fork-test-error
     "Choose a turn"
     (lambda () (harmless-fork-split (vector first) 0)))))

(ert-deftest harmless-fork-key-and-menu ()
  (should (eq (lookup-key harmless-session-mode-map (kbd "f"))
              #'harmless-fork))
  (should-not (eq (lookup-key harmless-prompt-mode-map (kbd "f"))
                  #'harmless-fork))
  (should (equal (transient--suffix-key
                  (transient-get-suffix 'harmless-menu 'harmless-fork))
                 "f")))

(ert-deftest harmless-fork-copies-through-the-chosen-turn ()
  (harmless-fork-test-env
    (let* ((root-messages (file-exists-p "/messages.jsonl"))
           (root-summary (file-exists-p "/summary.json"))
           (session (harmless-fork-test-make-session :cwd root))
           (messages (harmless-fork-test-rich))
           (parent-file nil)
           (parent-summary nil)
           (child nil))
      (setf (harmless-session-messages session) messages)
      (setf (harmless-session-model session) "grok-4.5")
      (setf (harmless-session-reasoning-effort session) "low")
      (setf (harmless-session-permission-mode session) 'always-approve)
      (setf (harmless-session-title session) "Kept title")
      (setf (harmless-session-title-locked session) t)
      (setf (harmless-session-plan-mode session) t)
      (setf (harmless-session-allow-classes session) '(write))
      (setf (harmless-session-status session) 'error)
      (setf (harmless-session-last-prompt-tokens session) 400000)
      (setf (harmless-session-last-completion-tokens session) 7)
      (setf (harmless-session-prompt-tokens session) 100)
      (setf (harmless-session-completion-tokens session) 9)
      (setf (harmless-session-updated-at session) "2020-01-01T00:00:00Z")
      (harmless-session-save session)
      (setq parent-file (harmless-fork-test-transcript session))
      (setq parent-summary (harmless-fork-test-file session "summary.json"))
      (should (null (harmless-session-buffer session)))
      (should (= 1 (hash-table-count harmless--sessions)))
      (harmless-fork-test-answers t
        (setq child (harmless-fork-session session 2))
        (should (null harmless-fork-test-asked))
        (should (equal '("Forked through turn 2") harmless-fork-test-notes)))
      (should (= 2 (hash-table-count harmless--sessions)))
      (should (eq child (harmless-session-get (harmless-session-id child))))
      (should-not (equal (harmless-session-id child)
                         (harmless-session-id session)))
      (should (equal (harmless-session-id session)
                     (harmless-session-parent-id child)))
      (should (eq 'fork (harmless-session-source child)))
      (should (equal "Kept title" (harmless-session-title child)))
      (should (harmless-session-title-locked child))
      (should (equal "grok-4.5" (harmless-session-model child)))
      (should (equal "low" (harmless-session-reasoning-effort child)))
      (should (eq 'always-approve (harmless-session-permission-mode child)))
      (should (harmless-session-plan-mode child))
      (should (eq (harmless-session-provider session)
                  (harmless-session-provider child)))
      (should (eq 'idle (harmless-session-status child)))
      (should (null (harmless-session-allow-classes child)))
      (should (null (harmless-session-process child)))
      (should (= 0 (harmless-session-last-prompt-tokens child)))
      (should (= 0 (harmless-session-last-completion-tokens child)))
      (should (= 0 (harmless-session-prompt-tokens child)))
      (should (= 0 (harmless-session-completion-tokens child)))
      (should (equal '("EARLIER" "OLD-GOAL" "looking" "old body"
                       "MID-TURN" "MID-TURN-reply")
                     (harmless-fork-test-contents child)))
      (should (equal '(:summary :user :assistant :tool :user :assistant)
                     (harmless-fork-test-roles child)))
      (should-not (eq messages (harmless-session-messages child)))
      (should-not (eq (nth 1 messages)
                      (nth 1 (harmless-session-messages child))))
      (should-not (eq (plist-get (nth 2 messages) :tool-calls)
                      (plist-get (nth 2 (harmless-session-messages child))
                                 :tool-calls)))
      (let ((parent-args (plist-get (car (plist-get (nth 2 messages) :tool-calls))
                                    :args))
            (child-args (plist-get
                         (car (plist-get (nth 2 (harmless-session-messages child))
                                         :tool-calls))
                         :args)))
        (should (equal parent-args child-args))
        (should-not (eq parent-args child-args))
        (setf (car child-args) :other)
        (should (eq :path (car parent-args))))
      (setf (plist-get (nth 1 (harmless-session-messages child)) :content)
            "CHANGED")
      (should (equal "OLD-GOAL" (plist-get (nth 1 messages) :content)))
      (should (eq messages (harmless-session-messages session)))
      (should (eq 'error (harmless-session-status session)))
      (should (equal "2020-01-01T00:00:00Z"
                     (harmless-session-updated-at session)))
      (should (= 400000 (harmless-session-last-prompt-tokens session)))
      (should (= 7 (harmless-session-last-completion-tokens session)))
      (should (= 100 (harmless-session-prompt-tokens session)))
      (should (= 9 (harmless-session-completion-tokens session)))
      (should (equal '(write) (harmless-session-allow-classes session)))
      (should (null (harmless-session-buffer session)))
      (should (buffer-live-p (harmless-session-buffer child)))
      (should (equal parent-file (harmless-fork-test-transcript session)))
      (should (equal parent-summary
                     (harmless-fork-test-file session "summary.json")))
      (harmless-fork-test-assert-stored
       child harmless-directory root-messages root-summary)
      (let ((file (harmless-fork-test-transcript child)))
        (should (string-search "EARLIER" file))
        (should (string-search "OLD-GOAL" file))
        (should (string-search "old body" file))
        (should (string-search "old.txt" file))
        (should (string-search "MID-TURN" file))
        (should-not (string-search "LATE-TURN" file))
        (should-not (string-search "CHANGED" file)))
      (with-current-buffer (harmless-session-buffer child)
        (should (string-search "OLD-GOAL" (buffer-string)))
        (should (string-search "old body" (buffer-string)))
        (should (string-search "MID-TURN" (buffer-string)))
        (should-not (string-search "LATE-TURN" (buffer-string))))
      (let ((summary (harmless-json-decode
                      (harmless-fork-test-file child "summary.json"))))
        (should (equal (harmless-session-id session)
                       (plist-get summary :parent-id)))
        (should (equal "fork" (plist-get summary :source)))
        (should (harmless-json-true-p (plist-get summary :plan-mode)))
        (should (equal "Kept title" (plist-get summary :title)))
        (should (= 0 (plist-get summary :prompt-tokens)))
        (should (= 0 (plist-get summary :completion-tokens)))
        (should (= 0 (plist-get summary :last-prompt-tokens)))
        (should (= 0 (plist-get summary :last-completion-tokens))))
      (let ((loaded (harmless-session-load (harmless-session-dir child))))
        (should (eq 'fork (harmless-session-source loaded)))
        (should (equal (harmless-session-id session)
                       (harmless-session-parent-id loaded)))
        (should (harmless-session-plan-mode loaded))
        (should (harmless-session-title-locked loaded))
        (should (equal "Kept title" (harmless-session-title loaded)))
        (should (= 0 (harmless-session-prompt-tokens loaded)))
        (should (cl-find "MID-TURN" (harmless-session-messages loaded)
                         :key (lambda (msg) (plist-get msg :content))
                         :test #'equal))
        (should-not (cl-find "LATE-TURN" (harmless-session-messages loaded)
                             :key (lambda (msg) (plist-get msg :content))
                             :test #'equal)))
      (harmless-session-append-user child "LATER-TURN")
      (should (equal "Kept title" (harmless-session-title child))))))

(ert-deftest harmless-fork-copies-the-whole-transcript ()
  (harmless-fork-test-env
    (let* ((root-messages (file-exists-p "/messages.jsonl"))
           (root-summary (file-exists-p "/summary.json"))
           (session (harmless-fork-test-make-session :cwd root))
           (messages (harmless-fork-test-rich))
           (parent-file nil)
           (child nil))
      (setf (harmless-session-messages session) messages)
      (setf (harmless-session-last-prompt-tokens session) 400000)
      (setf (harmless-session-last-completion-tokens session) 7)
      (setf (harmless-session-prompt-tokens session) 100)
      (setf (harmless-session-completion-tokens session) 9)
      (setf (harmless-session-updated-at session) "2020-01-01T00:00:00Z")
      (harmless-session-save session)
      (setq parent-file (harmless-fork-test-transcript session))
      (harmless-fork-test-answers t
        (setq child (harmless-fork-session session 3))
        (should (equal '("Forked through turn 3") harmless-fork-test-notes)))
      (should (= (length messages) (length (harmless-session-messages child))))
      (should (equal (harmless-fork-test-contents session)
                     (harmless-fork-test-contents child)))
      (should-not (eq messages (harmless-session-messages child)))
      (should-not (eq (nth 6 messages)
                      (nth 6 (harmless-session-messages child))))
      (should (eq messages (harmless-session-messages session)))
      (should (equal "2020-01-01T00:00:00Z"
                     (harmless-session-updated-at session)))
      (should (= 400000 (harmless-session-last-prompt-tokens session)))
      (should (= 100 (harmless-session-prompt-tokens session)))
      (should (= 0 (harmless-session-prompt-tokens child)))
      (should (= 0 (harmless-session-last-prompt-tokens child)))
      (should (eq 'idle (harmless-session-status child)))
      (should (null (harmless-session-plan-mode child)))
      (should (equal parent-file (harmless-fork-test-transcript session)))
      (harmless-fork-test-assert-stored
       child harmless-directory root-messages root-summary)
      (should (string-search "LATE-TURN" (harmless-fork-test-transcript child)))
      (should (string-search "LATE-TURN" parent-file))
      (with-current-buffer (harmless-session-buffer child)
        (should (string-search "LATE-TURN" (buffer-string)))
        (should (string-search "OLD-GOAL" (buffer-string)))))))

(ert-deftest harmless-fork-marks-the-turn-at-point ()
  (harmless-fork-test-env
    (let ((session (harmless-fork-test-make-session :cwd root))
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
        (should (= 3 (harmless-fork--turn-at-point)))))))

(ert-deftest harmless-fork-command-uses-the-turn-at-point ()
  (harmless-fork-test-env
    (let ((session (harmless-fork-test-make-session :cwd root))
          (messages (harmless-fork-test-rich))
          (child nil))
      (setf (harmless-session-messages session) messages)
      (setf (harmless-session-last-prompt-tokens session) 400000)
      (harmless-ui-render-session session)
      (with-current-buffer (harmless-session-buffer session)
        (goto-char (point-min))
        (should (search-forward "EARLIER" nil t))
        (goto-char (match-beginning 0))
        (harmless-fork-test-answers t
          (let ((read nil))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _)
                         (setq read t)
                         "ELSEWHERE")))
              (harmless-fork-test-error
               "Point is not on a turn"
               (lambda () (harmless-fork)))
              (should (null read))))
          (should (null harmless-fork-test-asked)))
        (should (eq messages (harmless-session-messages session)))
        (should (= 1 (hash-table-count harmless--sessions)))
        (goto-char (point-max))
        (should (= 3 (harmless-fork--turn-at-point)))
        (goto-char (point-min))
        (should (search-forward "old body" nil t))
        (goto-char (match-beginning 0))
        (should (= 1 (get-text-property (point) 'harmless-turn)))
        (harmless-fork-test-answers t
          (setq child (harmless-fork))
          (should (equal '("Fork through turn 1? ")
                         harmless-fork-test-asked))
          (should (equal '("Forked through turn 1")
                         harmless-fork-test-notes))))
      (should (eq messages (harmless-session-messages session)))
      (should (= 400000 (harmless-session-last-prompt-tokens session)))
      (should (equal '("EARLIER" "OLD-GOAL" "looking" "old body")
                     (harmless-fork-test-contents child)))
      (should-not (member "LATE-TURN" (harmless-fork-test-contents child)))
      (should (equal (harmless-session-id session)
                     (harmless-session-parent-id child)))
      (with-current-buffer (harmless-session-buffer session)
        (should (string-search "LATE-TURN" (buffer-string)))
        (should (string-search "old body" (buffer-string))))
      (with-current-buffer (harmless-session-buffer child)
        (should (string-search "OLD-GOAL" (buffer-string)))
        (should (string-search "old body" (buffer-string)))
        (should-not (string-search "LATE-TURN" (buffer-string)))
        (should-not (string-search "MID-TURN" (buffer-string)))))))

(ert-deftest harmless-fork-at-end-copies-the-last-turn ()
  (harmless-fork-test-env
    (let ((session (harmless-fork-test-make-session :cwd root))
          (messages (harmless-fork-test-rich))
          (child nil))
      (setf (harmless-session-messages session) messages)
      (harmless-ui-render-session session)
      (with-current-buffer (harmless-session-buffer session)
        (goto-char (point-max))
        (harmless-fork-test-answers t
          (setq child (harmless-fork))
          (should (equal '("Fork through turn 3? ")
                         harmless-fork-test-asked))
          (should (equal '("Forked through turn 3")
                         harmless-fork-test-notes))))
      (should (eq messages (harmless-session-messages session)))
      (should (equal (harmless-fork-test-contents session)
                     (harmless-fork-test-contents child)))
      (should (member "LATE-TURN" (harmless-fork-test-contents child)))
      (with-current-buffer (harmless-session-buffer session)
        (should (string-search "LATE-TURN" (buffer-string))))
      (with-current-buffer (harmless-session-buffer child)
        (should (string-search "LATE-TURN" (buffer-string)))))))

(ert-deftest harmless-fork-prefix-wins-over-point ()
  (harmless-fork-test-env
    (let ((session (harmless-fork-test-make-session :cwd root))
          (messages (harmless-fork-test-turns
                     '("TURN-ONE" "TURN-TWO" "TURN-THREE"
                       "TURN-FOUR" "TURN-FIVE")))
          (child nil))
      (setf (harmless-session-messages session) messages)
      (harmless-ui-render-session session)
      (with-current-buffer (harmless-session-buffer session)
        (goto-char (point-min))
        (should (search-forward "TURN-ONE" nil t))
        (harmless-fork-test-answers nil
          (should (null (harmless-fork 2)))
          (should (equal '("Fork through turn 2? ")
                         harmless-fork-test-asked))
          (should (null harmless-fork-test-notes)))
        (should (eq messages (harmless-session-messages session)))
        (should (= 1 (hash-table-count harmless--sessions)))
        (harmless-fork-test-answers t
          (let ((current-prefix-arg '(4)))
            (setq child (call-interactively #'harmless-fork)))
          (should (equal '("Fork through turn 4? ")
                         harmless-fork-test-asked))
          (should (equal '("Forked through turn 4")
                         harmless-fork-test-notes))))
      (should (eq messages (harmless-session-messages session)))
      (should (member "TURN-FIVE" (harmless-fork-test-contents session)))
      (should (equal '("TURN-ONE" "TURN-ONE-reply"
                       "TURN-TWO" "TURN-TWO-reply"
                       "TURN-THREE" "TURN-THREE-reply"
                       "TURN-FOUR" "TURN-FOUR-reply")
                     (harmless-fork-test-contents child)))
      (should-not (member "TURN-FIVE" (harmless-fork-test-contents child))))))

(ert-deftest harmless-fork-asks-outside-the-transcript ()
  (harmless-fork-test-env
    (let ((session (harmless-fork-test-make-session :cwd root))
          (messages (harmless-fork-test-rich))
          (prompt nil)
          (child nil))
      (setf (harmless-session-messages session) messages)
      (with-temp-buffer
        (setq default-directory "relative")
        (setq-local harmless--session session)
        (harmless-fork-test-answers t
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) "ELSEWHERE")))
            (harmless-fork-test-error
             "Nothing to fork"
             (lambda () (harmless-fork)))
            (should (null harmless-fork-test-asked)))))
      (should (eq messages (harmless-session-messages session)))
      (should (= 1 (hash-table-count harmless--sessions)))
      (with-temp-buffer
        (setq default-directory "relative")
        (setq-local harmless--session session)
        (harmless-fork-test-answers t
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (arg choices &rest _)
                       (setq prompt arg)
                       (car (nth 1 choices)))))
            (setq child (harmless-fork)))
          (should (equal '("Fork through turn 2? ")
                         harmless-fork-test-asked))
          (should (equal '("Forked through turn 2")
                         harmless-fork-test-notes))))
      (should (equal "Fork through turn: " prompt))
      (should (eq messages (harmless-session-messages session)))
      (should (equal '("EARLIER" "OLD-GOAL" "looking" "old body"
                       "MID-TURN" "MID-TURN-reply")
                     (harmless-fork-test-contents child)))
      (should-not (member "LATE-TURN" (harmless-fork-test-contents child)))
      (with-current-buffer (harmless-session-buffer child)
        (should (string-search "MID-TURN" (buffer-string)))
        (should-not (string-search "LATE-TURN" (buffer-string))))
      (harmless-ui-ensure-prompt-buffer session)
      (with-current-buffer (harmless-ui-ensure-prompt-buffer session)
        (harmless-fork-test-answers t
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt choices &rest _)
                       (car (car choices)))))
            (setq child (harmless-fork)))))
      (should (eq messages (harmless-session-messages session)))
      (should (equal '("EARLIER" "OLD-GOAL" "looking" "old body")
                     (harmless-fork-test-contents child)))
      (should-not (member "MID-TURN" (harmless-fork-test-contents child))))))

(ert-deftest harmless-fork-refuses-a-bad-turn-before-asking ()
  (harmless-fork-test-env
    (let ((session (harmless-fork-test-make-session :cwd root))
          (messages (harmless-fork-test-turns
                     '("OLD-GOAL" "MID-TURN" "LATE-TURN"))))
      (setf (harmless-session-messages session) messages)
      (with-temp-buffer
        (setq-local harmless--session session)
        (setq default-directory "relative")
        (harmless-fork-test-answers t
          (let ((current-prefix-arg 0))
            (harmless-fork-test-error
             "Choose a turn"
             (lambda () (call-interactively #'harmless-fork))))
          (should (null harmless-fork-test-asked))
          (should (eq messages (harmless-session-messages session)))))
      (should (= 1 (hash-table-count harmless--sessions)))
      (setf (harmless-session-messages session) "")
      (harmless-fork-test-error
       "Choose a turn"
       (lambda () (harmless-fork-session session 0)))
      (should (equal "" (harmless-session-messages session)))
      (should (= 1 (hash-table-count harmless--sessions))))))

(ert-deftest harmless-fork-refuses-an-empty-transcript ()
  (harmless-fork-test-env
    (let ((session (harmless-fork-test-make-session :cwd root))
          (dirs nil))
      (setf (harmless-session-last-prompt-tokens session) 400000)
      (setq dirs (harmless-fork-test-dirs))
      (dolist (messages (list nil
                              '()
                              ""
                              (vector (list :role :user :content "OLD-GOAL"))
                              (list (list :role :summary :content "EARLIER"))))
        (setf (harmless-session-messages session) messages)
        (harmless-fork-test-error
         "Nothing to fork"
         (lambda () (harmless-fork-session session 1)))
        (should (eq messages (harmless-session-messages session)))
        (should (= 400000 (harmless-session-last-prompt-tokens session)))
        (should (eq 'idle (harmless-session-status session)))
        (should (= 1 (hash-table-count harmless--sessions))))
      (setf (harmless-session-messages session)
            (harmless-fork-test-turns '("OLD-GOAL" "LATE-TURN")))
      (harmless-fork-test-error
       "Nothing to fork"
       (lambda () (harmless-fork-session session 9)))
      (should (= 400000 (harmless-session-last-prompt-tokens session)))
      (should (equal dirs (harmless-fork-test-dirs)))
      (setf (harmless-session-messages session) nil)
      (with-temp-buffer
        (setq-local harmless--session session)
        (setq default-directory "relative")
        (let ((asked-choice nil))
          (harmless-fork-test-answers t
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _)
                         (setq asked-choice t)
                         "ELSEWHERE")))
              (harmless-fork-test-error
               "Nothing to fork"
               (lambda () (harmless-fork)))
              (should (null harmless-fork-test-asked))))
          (should (null asked-choice)))))))

(ert-deftest harmless-fork-refuses-a-busy-session ()
  (harmless-fork-test-env
    (dolist (status '(streaming waiting-permission))
      (let ((session (harmless-session--create
                      :id "fork-busy"
                      :cwd root
                      :provider nil
                      :messages nil
                      :status status
                      :updated-at "2020-01-01T00:00:00Z"))
            (count (hash-table-count harmless--sessions)))
        (harmless-fork-test-error
         "A turn is in progress"
         (lambda () (harmless-fork-session session 1)))
        (should (null (harmless-session-messages session)))
        (should (eq status (harmless-session-status session)))
        (should (equal "2020-01-01T00:00:00Z"
                       (harmless-session-updated-at session)))
        (should (= count (hash-table-count harmless--sessions)))
        (with-temp-buffer
          (setq-local harmless--session session)
          (let ((asked-choice nil))
            (harmless-fork-test-answers t
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (&rest _)
                           (setq asked-choice t)
                           "ELSEWHERE")))
                (harmless-fork-test-error
                 "A turn is in progress"
                 (lambda () (harmless-fork)))
                (should (null harmless-fork-test-asked))))
            (should (null asked-choice)))))
      (let ((session (harmless-fork-test-make-session :cwd root))
            (messages (harmless-fork-test-turns
                       '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
            (dirs nil))
        (setf (harmless-session-messages session) messages)
        (setf (harmless-session-status session) status)
        (setq dirs (harmless-fork-test-dirs))
        (harmless-fork-test-error
         "A turn is in progress"
         (lambda () (harmless-fork-session session 1)))
        (should (eq messages (harmless-session-messages session)))
        (should (eq status (harmless-session-status session)))
        (should (equal dirs (harmless-fork-test-dirs)))))))

(ert-deftest harmless-fork-requires-a-session ()
  (harmless-fork-test-error
   "No Harmless session"
   (lambda () (harmless-fork-session nil 1)))
  (harmless-fork-test-env
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
      (with-temp-buffer
        (setq default-directory (file-name-as-directory root))
        (harmless-fork-test-error
         "No Harmless session"
         (lambda () (harmless-fork)))))
    (should (= 0 (hash-table-count harmless--sessions)))))

(ert-deftest harmless-fork-requires-a-provider ()
  (harmless-fork-test-env
    (let* ((messages (harmless-fork-test-turns
                      '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
           (session (harmless-session--create
                     :id "fork-none"
                     :cwd root
                     :provider nil
                     :model "grok-4.6"
                     :messages nil
                     :status 'idle
                     :prompt-tokens 100
                     :last-prompt-tokens 400000
                     :last-completion-tokens 7
                     :updated-at "2020-01-01T00:00:00Z")))
      (harmless-fork-test-error
       "No Harmless provider configured"
       (lambda () (harmless-fork-session session 1)))
      (should (null (harmless-session-messages session)))
      (should (= 400000 (harmless-session-last-prompt-tokens session)))
      (should (eq 'idle (harmless-session-status session)))
      (should (equal "2020-01-01T00:00:00Z"
                     (harmless-session-updated-at session)))
      (should (= 0 (hash-table-count harmless--sessions)))
      (should (null (harmless-fork-test-dirs)))
      (setf (harmless-session-messages session) messages)
      (harmless-fork-test-error
       "No Harmless provider configured"
       (lambda () (harmless-fork-session session 1)))
      (should (eq messages (harmless-session-messages session)))
      (should (= 400000 (harmless-session-last-prompt-tokens session)))
      (should (eq 'idle (harmless-session-status session)))
      (should (= 0 (hash-table-count harmless--sessions)))
      (with-temp-buffer
        (setq-local harmless--session session)
        (let ((asked-choice nil))
          (harmless-fork-test-answers t
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _)
                         (setq asked-choice t)
                         "ELSEWHERE")))
              (harmless-fork-test-error
               "No Harmless provider configured"
               (lambda () (harmless-fork)))
              (should (null harmless-fork-test-asked))))
          (should (null asked-choice)))))))

(ert-deftest harmless-fork-relative-cwd-writes-nothing ()
  (harmless-fork-test-env
    (let ((root-messages (file-exists-p "/messages.jsonl"))
          (root-summary (file-exists-p "/summary.json"))
          (default-directory (file-name-as-directory root))
          (provider (harmless-fork-make-fake :name "fake" :host "none")))
      (dolist (case '(("proj" . "Directory must be absolute, not \"proj\"")
                      ("" . "Directory must be absolute, not \"\"")
                      ("relative/" . "Directory must be absolute, not \"relative/\"")))
        (let* ((cwd (car case))
               (messages (harmless-fork-test-turns
                          '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
               (session (harmless-session--create
                         :id "fork-rel"
                         :cwd cwd
                         :provider provider
                         :model "grok-4.6"
                         :messages messages
                         :status 'idle
                         :last-prompt-tokens 400000
                         :last-completion-tokens 7
                         :prompt-tokens 100
                         :completion-tokens 9
                         :updated-at "2020-01-01T00:00:00Z")))
          (harmless-fork-test-error
           (cdr case)
           (lambda () (harmless-fork-session session 1)))
          (should (eq messages (harmless-session-messages session)))
          (should (= 400000 (harmless-session-last-prompt-tokens session)))
          (should (= 7 (harmless-session-last-completion-tokens session)))
          (should (= 100 (harmless-session-prompt-tokens session)))
          (should (= 9 (harmless-session-completion-tokens session)))
          (should (equal "2020-01-01T00:00:00Z"
                         (harmless-session-updated-at session)))
          (should (eq 'idle (harmless-session-status session)))
          (should (= 0 (hash-table-count harmless--sessions)))
          (should (null (harmless-fork-test-dirs)))
          (should (eq root-messages (file-exists-p "/messages.jsonl")))
          (should (eq root-summary (file-exists-p "/summary.json")))
          (should-not (file-exists-p
                       (expand-file-name "messages.jsonl" root))))))))

(ert-deftest harmless-fork-root-cwd-stays-under-the-data-directory ()
  (harmless-fork-test-env
    (let* ((root-messages (file-exists-p "/messages.jsonl"))
           (root-summary (file-exists-p "/summary.json"))
           (session (harmless-fork-test-make-session :cwd "/"))
           (parent-file nil)
           (child nil))
      (setf (harmless-session-messages session)
            (harmless-fork-test-turns '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
      (setq parent-file (harmless-fork-test-transcript session))
      (setq child (harmless-fork-session session 1))
      (should (equal "/" (harmless-session-cwd session)))
      (should (equal "/" (harmless-session-cwd child)))
      (should (equal parent-file (harmless-fork-test-transcript session)))
      (should-not (string-search "OLD-GOAL" parent-file))
      (harmless-fork-test-assert-stored
       child harmless-directory root-messages root-summary)
      (let ((file (harmless-fork-test-transcript child)))
        (should (string-search "OLD-GOAL" file))
        (should-not (string-search "LATE-TURN" file)))
      (should (null (harmless-session-plan-mode child))))))

(ert-deftest harmless-fork-trailing-slash-stays-under-the-data-directory ()
  (harmless-fork-test-env
    (let* ((slash (file-name-as-directory root))
           (root-messages (file-exists-p "/messages.jsonl"))
           (root-summary (file-exists-p "/summary.json"))
           (session (harmless-fork-test-make-session :cwd slash))
           (parent-file nil)
           (child nil))
      (should (string-suffix-p "/" slash))
      (setf (harmless-session-messages session)
            (harmless-fork-test-turns '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
      (setq parent-file (harmless-fork-test-transcript session))
      (setq child (harmless-fork-session session 1))
      (should (equal parent-file (harmless-fork-test-transcript session)))
      (harmless-fork-test-assert-stored
       child harmless-directory root-messages root-summary)
      (let ((file (harmless-fork-test-transcript child)))
        (should (string-search "OLD-GOAL" file))
        (should-not (string-search "LATE-TURN" file)))
      (should-not (file-exists-p (expand-file-name "messages.jsonl" slash))))))

(ert-deftest harmless-fork-missing-directory-is-not-the-filesystem-root ()
  (harmless-fork-test-env
    (let* ((missing (expand-file-name "no-such-project" root))
           (root-messages (file-exists-p "/messages.jsonl"))
           (root-summary (file-exists-p "/summary.json"))
           (session (harmless-fork-test-make-session :cwd missing))
           (parent-file nil)
           (child nil))
      (should-not (file-exists-p missing))
      (setf (harmless-session-messages session)
            (harmless-fork-test-turns '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
      (setq parent-file (harmless-fork-test-transcript session))
      (setq child (harmless-fork-session session 1))
      (should-not (file-exists-p missing))
      (should-not (file-directory-p missing))
      (should (equal parent-file (harmless-fork-test-transcript session)))
      (should-not (string-search "OLD-GOAL" parent-file))
      (harmless-fork-test-assert-stored
       child harmless-directory root-messages root-summary)
      (let ((file (harmless-fork-test-transcript child)))
        (should (string-search "OLD-GOAL" file))
        (should-not (string-search "LATE-TURN" file))))))

(ert-deftest harmless-fork-detached-chat-saves-under-the-data-directory ()
  (harmless-fork-test-env
    (let* ((scratch (make-temp-file "harmless-fork-scratch-" t))
           (default-directory (file-name-as-directory scratch))
           (before (directory-files scratch nil directory-files-no-dot-files-regexp))
           (root-messages (file-exists-p "/messages.jsonl"))
           (root-summary (file-exists-p "/summary.json"))
           (session (harmless-fork-test-make-session :detached t))
           (parent-file nil)
           (child nil))
      (should (null (harmless-session-cwd session)))
      (setf (harmless-session-messages session)
            (harmless-fork-test-turns '("OLD-GOAL" "MID-TURN" "LATE-TURN")))
      (setq parent-file (harmless-fork-test-transcript session))
      (setq child (harmless-fork-session session 1))
      (should (null (harmless-session-cwd session)))
      (should (null (harmless-session-cwd child)))
      (should (equal parent-file (harmless-fork-test-transcript session)))
      (should-not (string-search "OLD-GOAL" parent-file))
      (harmless-fork-test-assert-stored
       child harmless-directory root-messages root-summary)
      (should (string-search "/_detached/"
                             (file-name-as-directory (harmless-session-dir child))))
      (let ((file (harmless-fork-test-transcript child)))
        (should (string-search "OLD-GOAL" file))
        (should-not (string-search "LATE-TURN" file)))
      (should (equal before
                     (directory-files scratch nil
                                      directory-files-no-dot-files-regexp)))
      (should-not (file-exists-p
                   (expand-file-name "messages.jsonl" scratch))))))

(provide 'harmless-fork-tests)
