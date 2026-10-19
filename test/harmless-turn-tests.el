;;; harmless-turn-tests.el --- Tests for the Harmless turn loop -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless-provider)
(require 'harmless-openai)
(require 'harmless-session)
(require 'harmless-tools)
(require 'harmless-tools-fs)
(require 'harmless-tools-shell)
(require 'harmless-perm)
(require 'harmless-turn)

(cl-defstruct (harmless-fake-provider
               (:include harmless-provider)
               (:constructor harmless-make-fake))
  script)

(cl-defmethod harmless-provider-complete ((provider harmless-fake-provider)
                                          messages tools callback)
  (ignore tools)
  (funcall (harmless-fake-provider-script provider) messages callback)
  nil)

(ert-deftest harmless-turn-tool-then-text ()
  (let* ((dir (make-temp-file "harmless-proj-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-perm-ask-function
          (lambda (_s _c _i cb) (funcall cb 'allow)))
         (calls 0)
         (provider
          (harmless-make-fake
           :name "fake"
           :host "none"
           :script
           (lambda (messages callback)
             (setq calls (1+ calls))
             (if (= calls 1)
                 (progn
                   (funcall callback '(:tool-call "1" "write_file"
                                       "{\"path\":\"x.txt\",\"contents\":\"hi\"}"))
                   (funcall callback '(:stop "tool_calls")))
               (should (cl-find-if
                        (lambda (m)
                          (memq (plist-get m :role) '(:tool tool)))
                        messages))
               (funcall callback '(:text "wrote it"))
               (funcall callback '(:stop "stop"))))))
         (harmless-providers (list provider))
         (session (harmless-session-new :cwd dir :provider provider :model "m")))
    (harmless-turn-run session "please write")
    (should (= calls 2))
    (should (eq 'idle (harmless-session-status session)))
    (should (file-exists-p (expand-file-name "x.txt" dir)))
    (should (string= "hi"
                     (with-temp-buffer
                       (insert-file-contents (expand-file-name "x.txt" dir))
                       (buffer-string))))
    (let ((roles (mapcar (lambda (m) (plist-get m :role))
                         (harmless-session-messages session))))
      (should (equal '(:user :assistant :tool :assistant) roles)))))

(ert-deftest harmless-turn-counts-latest-usage-once ()
  (let* ((dir (make-temp-file "harmless-usage-turn-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (provider
          (harmless-make-fake
           :name "xAI"
           :host "none"
           :script
           (lambda (_messages callback)
             (funcall callback '(:usage 41 1))
             (funcall callback '(:usage 41 12))
             (funcall callback '(:limits (:tokens-remaining "80"
                                           :tokens-limit "100")))
             (funcall callback '(:text "ok"))
             (funcall callback '(:stop "stop")))))
         (harmless-providers (list provider))
         (session (harmless-session-new :cwd dir :provider provider :model "grok-4.6")))
    (harmless-turn-run session "hi")
    (should (= 41 (harmless-session-prompt-tokens session)))
    (should (= 12 (harmless-session-completion-tokens session)))
    (should (= 41 (harmless-session-last-prompt-tokens session)))
    (let ((limits (harmless-usage-load-limits)))
      (should (equal "80" (plist-get (car limits) :tokens-remaining))))))

(ert-deftest harmless-turn-denied-tool ()
  (let* ((dir (make-temp-file "harmless-proj-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-perm-ask-function
          (lambda (_s _c _i cb) (funcall cb 'deny)))
         (calls 0)
         (provider
          (harmless-make-fake
           :name "fake"
           :host "none"
           :script
           (lambda (messages callback)
             (setq calls (1+ calls))
             (if (cl-find-if (lambda (m) (memq (plist-get m :role) '(:tool tool)))
                             messages)
                 (progn
                   (funcall callback '(:text "ok"))
                   (funcall callback '(:stop "stop")))
               (funcall callback '(:tool-call "1" "write_file"
                                   "{\"path\":\"nope.txt\",\"contents\":\"x\"}"))
               (funcall callback '(:stop "tool_calls"))))))
         (harmless-providers (list provider))
         (session (harmless-session-new :cwd dir :provider provider :model "m")))
    (harmless-turn-run session "write")
    (should-not (file-exists-p (expand-file-name "nope.txt" dir)))
    (should (string-match-p "denied"
                            (plist-get
                             (cl-find-if (lambda (m)
                                           (memq (plist-get m :role) '(:tool tool)))
                                         (harmless-session-messages session))
                             :content)))))

(ert-deftest harmless-turn-sends-project-instructions ()
  (let* ((dir (make-temp-file "harmless-proj-" t))
         (harmless-directory (expand-file-name ".harmless-state" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-perm-ask-function
          (lambda (_s _c _i cb) (funcall cb 'allow)))
         (seen nil)
         (provider
          (harmless-make-fake
           :name "fake"
           :host "none"
           :script
           (lambda (messages callback)
             (setq seen messages)
             (funcall callback '(:text "ok"))
             (funcall callback '(:stop "stop")))))
         (harmless-providers (list provider))
         (session (harmless-session-new :cwd dir :provider provider :model "m")))
    (make-directory (expand-file-name ".harmless" dir))
    (with-temp-file (expand-file-name "HARMLESS.md" dir)
      (insert "Use two spaces.\n"))
    (with-temp-file (expand-file-name ".harmless/HARMLESS.md" dir)
      (insert "Prefer the dotfile.\n"))
    (harmless-turn-run session "hello")
    (let ((text (plist-get (car seen) :content)))
      (should (eq :system (plist-get (car seen) :role)))
      (should (string-match-p "Use two spaces" text))
      (should (string-match-p "Prefer the dotfile" text))
      (should (< (string-match "Use two spaces" text)
                 (string-match "Prefer the dotfile" text))))
    (should-not (cl-find-if #'harmless-message-system-p
                            (harmless-session-messages session)))))

(ert-deftest harmless-turn-empty-stop-is-an-error ()
  (let* ((dir (make-temp-file "harmless-empty-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (events nil)
         (provider
          (harmless-make-fake
           :name "fake"
           :host "none"
           :script
           (lambda (_messages callback)
             (funcall callback '(:stop "end_turn")))))
         (harmless-providers (list provider))
         (session (harmless-session-new :cwd dir :provider provider :model "m")))
    (let ((harmless-event-functions
           (cons (lambda (_s event) (push event events))
                 harmless-event-functions)))
      (harmless-turn-run session "hello"))
    (should (eq 'error (harmless-session-status session)))
    (should (equal '(:user)
                   (mapcar (lambda (m) (plist-get m :role))
                           (harmless-session-messages session))))
    (should (cl-find '(:error "The model returned an empty reply.")
                     events :test #'equal))))

(defun harmless-turn-test--wait (session)
  "Wait until SESSION is idle or the wait gives up."
  (let ((n 0))
    (while (and (not (eq (harmless-session-status session) 'idle))
                (< n 50))
      (setq n (1+ n))
      (accept-process-output nil 0.1))))

(ert-deftest harmless-turn-runs-tools-together ()
  (let* ((dir (make-temp-file "harmless-parallel-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-tools harmless-tools)
         (harmless-perm-ask-function
          (lambda (&rest _) (error "Read tools must not ask for permission")))
         (started nil)
         (release nil)
         (seen nil)
         (calls 0)
         (provider
          (harmless-make-fake
           :name "fake"
           :host "none"
           :script
           (lambda (messages callback)
             (setq calls (1+ calls))
             (if (= calls 1)
                 (progn
                   (funcall callback
                            '(:tool-call "a" "hold"
                              "{\"label\":\"a\"}"))
                   (funcall callback
                            '(:tool-call "b" "hold"
                              "{\"label\":\"b\"}"))
                   (funcall callback '(:stop "tool_calls")))
               (setq seen messages)
               (funcall callback '(:text "both"))
               (funcall callback '(:stop "stop"))))))
         (harmless-providers (list provider))
         (session (harmless-session-new :cwd dir :provider provider :model "m")))
    (harmless-register-tool
     (harmless-tool-create
      :name "hold"
      :description "Hold the callback until the test releases it."
      :class 'read
      :async t
      :schema '(:type "object"
                :properties (:label (:type "string"))
                :required ["label"])
      :fn (lambda (_session args callback)
            (let ((label (harmless-tool-arg args :label)))
              (push label started)
              (push (cons label callback) release)))))
    (harmless-turn-run session "run both")
    (should (= calls 1))
    (should (equal '("b" "a") started))
    (should (= 2 (length release)))
    (should (equal '(:user :assistant)
                   (mapcar (lambda (m) (plist-get m :role))
                           (harmless-session-messages session))))
    (funcall (cdr (assoc "b" release)) "bee")
    (should (= calls 1))
    (should (equal '(:user :assistant)
                   (mapcar (lambda (m) (plist-get m :role))
                           (harmless-session-messages session))))
    (funcall (cdr (assoc "a" release)) "ay")
    (should (= calls 2))
    (should (eq 'idle (harmless-session-status session)))
    (should (equal '(:user :assistant :tool :tool :assistant)
                   (mapcar (lambda (m) (plist-get m :role))
                           (harmless-session-messages session))))
    (let ((tools (seq-filter (lambda (m) (eq (plist-get m :role) :tool))
                             (harmless-session-messages session))))
      (should (equal '("ay" "bee")
                     (mapcar (lambda (m) (plist-get m :content)) tools)))
      (should (equal '("a" "b")
                     (mapcar (lambda (m) (plist-get m :id)) tools))))
    (let ((sent (seq-filter (lambda (m) (eq (plist-get m :role) :tool))
                            seen)))
      (should (equal '("ay" "bee")
                     (mapcar (lambda (m) (plist-get m :content)) sent))))))

(ert-deftest harmless-turn-asks-for-the-next-tool-before-the-first-finishes ()
  (let* ((dir (make-temp-file "harmless-pipeline-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-tools harmless-tools)
         (asks nil)
         (harmless-perm-ask-function
          (lambda (_session _class item callback)
            (push (cons item callback) asks)))
         (started nil)
         (release nil)
         (calls 0)
         (provider
          (harmless-make-fake
           :name "fake"
           :host "none"
           :script
           (lambda (_messages callback)
             (setq calls (1+ calls))
             (if (= calls 1)
                 (progn
                   (funcall callback
                            '(:tool-call "1" "hold"
                              "{\"path\":\"one\"}"))
                   (funcall callback
                            '(:tool-call "2" "hold"
                              "{\"path\":\"two\"}"))
                   (funcall callback '(:stop "tool_calls")))
               (funcall callback '(:text "ok"))
               (funcall callback '(:stop "stop"))))))
         (harmless-providers (list provider))
         (session (harmless-session-new :cwd dir :provider provider :model "m")))
    (harmless-register-tool
     (harmless-tool-create
      :name "hold"
      :description "Hold the callback until the test releases it."
      :class 'edit
      :async t
      :schema '(:type "object"
                :properties (:path (:type "string"))
                :required ["path"])
      :fn (lambda (_session args callback)
            (push (harmless-tool-arg args :path) started)
            (push callback release))))
    (harmless-turn-run session "edit both")
    (should (= calls 1))
    (should (equal '("one") (mapcar #'car asks)))
    (should (null started))
    (funcall (cdr (car asks)) 'allow)
    (should (equal '("one") started))
    (should (equal '("two" "one") (mapcar #'car asks)))
    (should (= 1 (length release)))
    (funcall (cdr (car asks)) 'allow)
    (should (equal '("two" "one") started))
    (should (= calls 1))
    (funcall (nth 1 release) "first-done")
    (funcall (nth 0 release) "second-done")
    (should (= calls 2))
    (let ((tools (seq-filter (lambda (m) (eq (plist-get m :role) :tool))
                             (harmless-session-messages session))))
      (should (equal '("first-done" "second-done")
                     (mapcar (lambda (m) (plist-get m :content)) tools))))))

(ert-deftest harmless-turn-abort-skips-late-tool-results ()
  (let* ((dir (make-temp-file "harmless-abort-batch-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-tools harmless-tools)
         (release nil)
         (calls 0)
         (events nil)
         (provider
          (harmless-make-fake
           :name "fake"
           :host "none"
           :script
           (lambda (_messages callback)
             (setq calls (1+ calls))
             (funcall callback '(:tool-call "a" "hold" "{\"label\":\"a\"}"))
             (funcall callback '(:tool-call "b" "hold" "{\"label\":\"b\"}"))
             (funcall callback '(:stop "tool_calls")))))
         (harmless-providers (list provider))
         (session (harmless-session-new :cwd dir :provider provider :model "m")))
    (harmless-register-tool
     (harmless-tool-create
      :name "hold"
      :description "Hold the callback until the test releases it."
      :class 'read
      :async t
      :schema '(:type "object")
      :fn (lambda (_session _args callback)
            (push callback release))))
    (let ((harmless-event-functions
           (cons (lambda (_s event) (push event events))
                 harmless-event-functions)))
      (harmless-turn-run session "go")
      (should (= 2 (length release)))
      (harmless-turn-abort session)
      (dolist (callback release)
        (funcall callback "late"))
      (should (= calls 1))
      (should (eq 'idle (harmless-session-status session)))
      (should (equal '(:user :assistant)
                     (mapcar (lambda (m) (plist-get m :role))
                             (harmless-session-messages session))))
      (should (cl-find '(:error "aborted") events :test #'equal)))))

(ert-deftest harmless-turn-unknown-tool-does-not-block-the-rest ()
  (let* ((dir (make-temp-file "harmless-unknown-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-tools harmless-tools)
         (release nil)
         (calls 0)
         (provider
          (harmless-make-fake
           :name "fake"
           :host "none"
           :script
           (lambda (_messages callback)
             (setq calls (1+ calls))
             (if (= calls 1)
                 (progn
                   (funcall callback
                            '(:tool-call "a" "hold" "{\"label\":\"a\"}"))
                   (funcall callback
                            '(:tool-call "z" "missing" "{}"))
                   (funcall callback '(:stop "tool_calls")))
               (funcall callback '(:text "ok"))
               (funcall callback '(:stop "stop"))))))
         (harmless-providers (list provider))
         (session (harmless-session-new :cwd dir :provider provider :model "m")))
    (harmless-register-tool
     (harmless-tool-create
      :name "hold"
      :description "Hold the callback until the test releases it."
      :class 'read
      :async t
      :schema '(:type "object")
      :fn (lambda (_session _args callback)
            (push callback release))))
    (harmless-turn-run session "go")
    (should (= 1 (length release)))
    (should (= calls 1))
    (funcall (car release) "held")
    (should (= calls 2))
    (let ((tools (seq-filter (lambda (m) (eq (plist-get m :role) :tool))
                             (harmless-session-messages session))))
      (should (equal '("held" "Error: unknown tool missing")
                     (mapcar (lambda (m) (plist-get m :content)) tools))))))

(ert-deftest harmless-turn-runs-two-shells ()
  (let* ((dir (make-temp-file "harmless-two-shells-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-perm-ask-function
          (lambda (_s _c _i cb) (funcall cb 'allow)))
         (calls 0)
         (provider
          (harmless-make-fake
           :name "fake"
           :host "none"
           :script
           (lambda (_messages callback)
             (setq calls (1+ calls))
             (if (= calls 1)
                 (progn
                   (funcall callback
                            '(:tool-call "1" "run_shell"
                              "{\"command\":\"echo one\"}"))
                   (funcall callback
                            '(:tool-call "2" "run_shell"
                              "{\"command\":\"echo two\"}"))
                   (funcall callback '(:stop "tool_calls")))
               (funcall callback '(:text "done"))
               (funcall callback '(:stop "stop"))))))
         (harmless-providers (list provider))
         (session (harmless-session-new :cwd dir :provider provider :model "m")))
    (harmless-turn-run session "echo")
    (harmless-turn-test--wait session)
    (should (= calls 2))
    (should (eq 'idle (harmless-session-status session)))
    (let ((tools (seq-filter (lambda (m) (eq (plist-get m :role) :tool))
                             (harmless-session-messages session))))
      (should (= 2 (length tools)))
      (should (string-match-p "one" (plist-get (nth 0 tools) :content)))
      (should (string-match-p "two" (plist-get (nth 1 tools) :content)))
      (should (string-match-p "exit 0" (plist-get (nth 0 tools) :content))))))

(defun harmless-turn-test--shell-turn (session provider command)
  "Run one shell COMMAND on SESSION through PROVIDER, then stop."
  (let ((calls 0))
    (setf (harmless-fake-provider-script provider)
          (lambda (_messages callback)
            (setq calls (1+ calls))
            (if (= calls 1)
                (progn
                  (funcall callback
                           (list :tool-call "1" "run_shell"
                                 (format "{\"command\":%S}" command)))
                  (funcall callback '(:stop "tool_calls")))
              (funcall callback '(:text "done"))
              (funcall callback '(:stop "stop")))))
    (harmless-turn-run session "go")
    (harmless-turn-test--wait session)))

(defun harmless-turn-test--tool-contents (session)
  "Return tool-result strings recorded on SESSION, in order."
  (mapcar (lambda (message) (plist-get message :content))
          (seq-filter (lambda (message) (eq (plist-get message :role) :tool))
                      (harmless-session-messages session))))

(ert-deftest harmless-turn-read-only-shell-does-not-ask ()
  (let* ((dir (make-temp-file "harmless-ro-shell-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (asked nil)
         (harmless-perm-ask-function
          (lambda (&rest _)
            (setq asked t)
            (error "A read-only shell must not ask")))
         (provider (harmless-make-fake :name "fake" :host "none" :script #'ignore))
         (harmless-providers (list provider))
         (session (harmless-session-new :cwd dir :provider provider :model "m")))
    (unwind-protect
        (progn
          (harmless-turn-test--shell-turn session provider "echo hi")
          (should-not asked)
          (should (eq 'idle (harmless-session-status session)))
          (let ((text (car (harmless-turn-test--tool-contents session))))
            (should (string-match-p "hi" text))
            (should-not (string-search "__HARMLESS_CWD__" text))))
      (dolist (job (harmless-session-shell-jobs session))
        (let ((proc (harmless-shell-job-proc job)))
          (when (and (processp proc) (process-live-p proc))
            (delete-process proc)))))))

(ert-deftest harmless-turn-shell-write-asks ()
  (let* ((dir (make-temp-file "harmless-rw-shell-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (out (expand-file-name "out.txt" dir))
         (asked nil)
         (item nil)
         (harmless-perm-ask-function
          (lambda (_session class description callback)
            (setq asked class
                  item description)
            (funcall callback 'deny)))
         (provider (harmless-make-fake :name "fake" :host "none" :script #'ignore))
         (harmless-providers (list provider))
         (session (harmless-session-new :cwd dir :provider provider :model "m")))
    (unwind-protect
        (progn
          (harmless-turn-test--shell-turn session provider "echo hi > out.txt")
          (should (eq 'shell asked))
          (should (string= "echo hi > out.txt" item))
          (should-not (file-exists-p out))
          (should-not (file-exists-p "/out.txt"))
          (should (equal '("Error: user denied this tool call")
                         (harmless-turn-test--tool-contents session))))
      (dolist (job (harmless-session-shell-jobs session))
        (let ((proc (harmless-shell-job-proc job)))
          (when (and (processp proc) (process-live-p proc))
            (delete-process proc))))
      (when (file-exists-p out) (delete-file out)))))

(ert-deftest harmless-turn-plan-refuses-shell-write-before-permission ()
  (let* ((dir (make-temp-file "harmless-plan-shell-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (out (expand-file-name "out.txt" dir))
         (asked nil)
         (harmless-perm-ask-function
          (lambda (&rest _)
            (setq asked t)
            (error "Plan mode must refuse before the question")))
         (calls 0)
         (provider
          (harmless-make-fake
           :name "fake"
           :host "none"
           :script
           (lambda (_messages callback)
             (setq calls (1+ calls))
             (if (= calls 1)
                 (progn
                   (funcall callback
                            '(:tool-call "1" "run_shell"
                              "{\"command\":\"echo hi\"}"))
                   (funcall callback
                            '(:tool-call "2" "run_shell"
                              "{\"command\":\"echo hi > out.txt\"}"))
                   (funcall callback '(:stop "tool_calls")))
               (funcall callback '(:text "done"))
               (funcall callback '(:stop "stop"))))))
         (harmless-providers (list provider))
         (session (harmless-session-new :cwd dir :provider provider :model "m")))
    (unwind-protect
        (progn
          (setf (harmless-session-plan-mode session) t)
          (setf (harmless-session-permission-mode session) 'always-approve)
          (harmless-turn-run session "plan")
          (harmless-turn-test--wait session)
          (should-not asked)
          (should (eq 'idle (harmless-session-status session)))
          (should (= calls 2))
          (should-not (file-exists-p out))
          (should-not (file-exists-p "/out.txt"))
          (let ((tools (seq-filter (lambda (m) (eq (plist-get m :role) :tool))
                                   (harmless-session-messages session))))
            (should (= 2 (length tools)))
            (should (string-match-p "hi" (plist-get (nth 0 tools) :content)))
            (should (equal harmless-shell-plan-refusal
                           (plist-get (nth 1 tools) :content)))))
      (dolist (job (harmless-session-shell-jobs session))
        (let ((proc (harmless-shell-job-proc job)))
          (when (and (processp proc) (process-live-p proc))
            (delete-process proc))))
      (when (file-exists-p out) (delete-file out)))))

(ert-deftest harmless-turn-plan-shell-kill-still-asks ()
  (let* ((dir (make-temp-file "harmless-plan-kill-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (asked nil)
         (harmless-perm-ask-function
          (lambda (_session class _item callback)
            (setq asked class)
            (funcall callback 'allow)))
         (calls 0)
         (provider
          (harmless-make-fake
           :name "fake"
           :host "none"
           :script
           (lambda (_messages callback)
             (setq calls (1+ calls))
             (if (= calls 1)
                 (progn
                   (funcall callback
                            '(:tool-call "1" "shell_kill" "{\"id\":\"9\"}"))
                   (funcall callback '(:stop "tool_calls")))
               (funcall callback '(:text "done"))
               (funcall callback '(:stop "stop"))))))
         (harmless-providers (list provider))
         (session (harmless-session-new :cwd dir :provider provider :model "m")))
    (setf (harmless-session-plan-mode session) t)
    (harmless-turn-run session "kill")
    (should (eq 'shell asked))
    (should (equal '("Error: no background shell 9")
                   (harmless-turn-test--tool-contents session)))
    (should (= calls 2))))

(provide 'harmless-turn-tests)
