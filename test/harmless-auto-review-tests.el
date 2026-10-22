;;; harmless-auto-review-tests.el --- Tests for auto-review -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(cl-defstruct (harmless-auto-fake
               (:include harmless-provider)
               (:constructor harmless-auto-make-fake))
  script)

(cl-defmethod harmless-provider-complete ((provider harmless-auto-fake)
                                          messages tools callback)
  (when (functionp (harmless-auto-fake-script provider))
    (funcall (harmless-auto-fake-script provider)
             messages tools callback))
  nil)

(defvar harmless-auto-test-calls 0
  "Provider calls in the current auto-review turn.")

(defvar harmless-auto-test-asked nil
  "Permission questions recorded by the current turn.")

(defvar harmless-auto-test-reviews nil
  "Reviewer calls recorded by the current turn.")

(defun harmless-auto-test-read (file)
  "Return the contents of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun harmless-auto-test-summary (session)
  "Return SESSION's summary plist from disk."
  (harmless-json-decode
   (harmless-auto-test-read
    (expand-file-name "summary.json" (harmless-session-dir session)))))

(defun harmless-auto-test-assert-session (session)
  "Require SESSION's directory to stay off the filesystem root."
  (let ((dir (harmless-session-dir session)))
    (should (file-name-absolute-p dir))
    (should-not (harmless-filesystem-root-p dir))
    (should (harmless-directory-strictly-under-p dir harmless-directory))
    (should-not (equal (file-truename dir)
                       (file-truename (expand-file-name "/"))))))

(defun harmless-auto-test-assert-written (path root)
  "Require PATH to be a regular file strictly inside ROOT."
  (setq path (expand-file-name path))
  (should (file-name-absolute-p path))
  (should (file-regular-p path))
  (should-not (harmless-filesystem-root-p path))
  (should (string-prefix-p
           (file-name-as-directory (expand-file-name root))
           path))
  (should-not (equal
               (file-truename path)
               (file-truename
                (expand-file-name (file-name-nondirectory path) "/"))))
  path)

(defmacro harmless-auto-test-with (data repo &rest body)
  "Bind temp directories DATA and REPO, then run BODY."
  (declare (indent 2))
  `(let* ((,data (make-temp-file "harmless-auto-data-" t))
          (,repo (make-temp-file "harmless-auto-repo-" t))
          (harmless-directory ,data)
          (harmless--sessions (make-hash-table :test 'equal))
          (harmless-compact-threshold nil)
          (harmless-permission-rules nil)
          (harmless-permission-denies nil)
          (harmless-dangerous-commands
           '("diskpart" "halt" "mkfs" "poweroff" "reboot" "shutdown"))
          (harmless-auto-review-function nil)
          (harmless-auto-test-calls 0)
          (harmless-auto-test-asked nil)
          (harmless-auto-test-reviews nil)
          (harmless-auto-review-model-function
           (lambda (&rest args)
             (push args harmless-auto-test-reviews)
             (error "model must not be called")))
          (harmless-perm-ask-function
           (lambda (_session class item callback)
             (push (list class item) harmless-auto-test-asked)
             (funcall callback 'deny))))
     ,@body))

(defun harmless-auto-test-session (repo &rest args)
  "Return a session in REPO.  ARGS are passed to `harmless-session-new'."
  (apply #'harmless-session-new
         :cwd repo
         :provider (harmless-auto-make-fake
                    :name "fake" :host "none" :script #'ignore)
         :model "m"
         :title "Auto"
         args))

(defun harmless-auto-test-run (session name args)
  "Run tool NAME with ARGS on SESSION and wait until it stops."
  (setq harmless-auto-test-calls 0)
  (setq harmless-auto-test-asked nil)
  (setq harmless-auto-test-reviews nil)
  (setf (harmless-auto-fake-script (harmless-session-provider session))
        (lambda (_messages _tools callback)
          (setq harmless-auto-test-calls
                (1+ harmless-auto-test-calls))
          (if (= harmless-auto-test-calls 1)
              (progn
                (funcall callback
                         (list :tool-call "1" name
                               (harmless-json-text args)))
                (funcall callback '(:stop "tool_calls")))
            (funcall callback '(:text "done"))
            (funcall callback '(:stop "stop")))))
  (harmless-turn-run session "go")
  (let ((n 0))
    (while (and (not (memq (harmless-session-status session)
                           '(idle error)))
                (< n 80))
      (setq n (1+ n))
      (accept-process-output nil 0.05))))

(defun harmless-auto-test-last (session)
  "Return the latest tool result stored on SESSION."
  (car (last (cl-loop for msg in (harmless-session-messages session)
                      when (eq (plist-get msg :role) :tool)
                      collect (plist-get msg :content)))))

(defun harmless-auto-test-reply (provider text)
  "Make PROVIDER answer a review with TEXT, and return a request box.
The box's car is (MESSAGES TOOLS MODEL) after the provider runs."
  (let ((box (list nil)))
    (setf (harmless-auto-fake-script provider)
          (lambda (messages tools callback)
            (setcar box (list messages tools harmless-current-model))
            (when (stringp text)
              (funcall callback (list :text text)))
            (funcall callback '(:stop "stop"))))
    box))

(ert-deftest harmless-auto-review-parses-and-fails-closed ()
  (let ((slash (file-exists-p "/.harmless"))
        (cdrive (file-exists-p "c:/.harmless"))
        (tool (harmless-tool-by-name "harmless-tool-run-shell")))
    (should (eq 'allow (harmless-auto-review-parse "allow")))
    (should (eq 'allow (harmless-auto-review-parse "  ALLOW \n")))
    (should (eq 'escalate (harmless-auto-review-parse "escalate")))
    (should (eq 'escalate (harmless-auto-review-parse " ESCALATE")))
    (should (eq 'escalate (harmless-auto-review-parse nil)))
    (should (eq 'escalate (harmless-auto-review-parse "")))
    (should (eq 'escalate (harmless-auto-review-parse "   ")))
    (should (eq 'escalate (harmless-auto-review-parse "allow please")))
    (should (eq 'escalate (harmless-auto-review-parse "allow\nmore")))
    (should (eq 'escalate (harmless-auto-review-parse "yes")))
    (should (eq 'escalate (harmless-auto-review-parse "/")))
    (should (eq 'escalate (harmless-auto-review-parse "c:/")))
    (should (equal (harmless-auto-review-prompt nil nil)
                   "Tool: \nArguments: none"))
    (should (equal (harmless-auto-review-prompt "" "")
                   "Tool: \nArguments: "))
    (should (equal (harmless-auto-review-prompt
                    "harmless-tool-run-shell" '(:command nil))
                   "Tool: harmless-tool-run-shell\nArguments: command=nil"))
    (should (equal (harmless-auto-review-prompt
                    "harmless-tool-run-shell" '(:command :false))
                   "Tool: harmless-tool-run-shell\nArguments: command=false"))
    (should (equal (harmless-auto-review-prompt
                    "harmless-tool-run-shell" '(:command ""))
                   "Tool: harmless-tool-run-shell\nArguments: command=\"\""))
    (should (equal (harmless-auto-review-prompt
                    tool '(:path "note.txt/" :command "../note.txt"))
                   (concat "Tool: harmless-tool-run-shell\nArguments: "
                           "path=\"note.txt/\" command=\"../note.txt\"")))
    (let ((prompt (harmless-auto-review-prompt
                   tool '(:command "echo hi > /"))))
      (should (string-search "echo hi > /" prompt))
      (should (string-search "Tool: harmless-tool-run-shell" prompt))
      (should-not (file-exists-p "/echo")))
    (let* ((provider (harmless-auto-make-fake :name "fake" :host "none"))
           (session (harmless-session--create
                     :provider provider
                     :model "review-model"
                     :permission-mode 'auto))
           (request nil)
           (got nil)
           (n 0))
      (setq request
            (harmless-auto-test-reply provider "  ALLOW \n"))
      (harmless-auto-review-model
       session tool '(:command "echo hi > /")
       (lambda (decision)
         (setq n (1+ n))
         (setq got decision)))
      (should (eq 'allow got))
      (should (= n 1))
      (let ((got-request (car request)))
        (should (null (nth 1 got-request)))
        (should (equal "review-model" (nth 2 got-request)))
        (should (equal harmless-auto-review-instructions
                       (plist-get (car (car got-request)) :content)))
        (should (eq :user (plist-get (cadr (car got-request)) :role)))
        (should (string-search
                 "echo hi > /"
                 (plist-get (cadr (car got-request)) :content))))
      (setq n 0)
      (harmless-auto-test-reply provider "allow please")
      (harmless-auto-review-model
       session tool '(:command "echo")
       (lambda (decision) (setq n (1+ n) got decision)))
      (should (eq 'escalate got))
      (should (= n 1))
      (setf (harmless-auto-fake-script provider)
            (lambda (_messages tools callback)
              (should (null tools))
              (funcall callback '(:tool-call "9" "harmless-tool-run-shell" "{}"))
              (funcall callback '(:text "allow"))
              (funcall callback '(:stop "stop"))
              (funcall callback '(:stop "stop"))))
      (setq n 0)
      (harmless-auto-review-model
       session tool nil
       (lambda (decision) (setq n (1+ n) got decision)))
      (should (eq 'escalate got))
      (should (= n 1))
      (harmless-auto-test-reply provider nil)
      (setq n 0)
      (harmless-auto-review-model
       session tool nil
       (lambda (decision) (setq n (1+ n) got decision)))
      (should (eq 'escalate got))
      (setf (harmless-auto-fake-script provider)
            (lambda (&rest _) (error "review down")))
      (setq n 0)
      (harmless-auto-review-model
       session tool nil
       (lambda (decision) (setq n (1+ n) got decision)))
      (should (eq 'escalate got))
      (should (= n 1))
      (harmless-auto-test-reply provider "allow")
      (let ((err (should-error
                  (harmless-auto-review-model
                   session tool nil
                   (lambda (_decision) (error "callback failed")))
                  :type 'error)))
        (should (equal (error-message-string err) "callback failed")))
      (setq n 0)
      (harmless-auto-review-model
       nil tool nil
       (lambda (decision) (setq n (1+ n) got decision)))
      (should (eq 'escalate got))
      (should (= n 1))
      (let ((harmless-auto-review-function
             (lambda (_session got args)
             (should (eq got tool))
             (should (equal (harmless-tool-arg args :command) "/"))
             'allow))
          (harmless-auto-review-model-function
           (lambda (&rest _) (error "model must not be called")))
          (got nil))
      (harmless-auto-review session tool '(:command "/")
                            (lambda (decision) (setq got decision)))
      (should (eq 'allow got)))
    (dolist (bad (list "allow" t nil 'escalate))
      (let ((harmless-auto-review-function (lambda (&rest _) bad))
            (got nil))
        (harmless-auto-review session tool nil
                              (lambda (decision) (setq got decision)))
        (should (eq 'escalate got))))
    (let ((harmless-auto-review-function
           (lambda (&rest _) (error "classifier down")))
          (got nil))
      (harmless-auto-review nil nil nil
                            (lambda (decision) (setq got decision)))
      (should (eq 'escalate got)))
    (let ((harmless-auto-review-function "allow")
          (harmless-auto-review-model-function
           (lambda (_session _tool _args callback)
             (funcall callback 'allow)
             (funcall callback 'escalate)))
          (n 0)
          (got nil))
      (harmless-auto-review session tool '(:command "c:/")
                            (lambda (decision)
                              (setq n (1+ n))
                              (setq got decision)))
      (should (= n 1))
      (should (eq 'allow got)))
    (let ((harmless-auto-review-function nil)
          (harmless-auto-review-model-function nil)
          (got nil))
      (harmless-auto-review nil nil '(:command "")
                            (lambda (decision) (setq got decision)))
      (should (eq 'escalate got))))
    (should (eq slash (file-exists-p "/.harmless")))
    (should (eq cdrive (file-exists-p "c:/.harmless")))
    (should-not (file-exists-p "/echo"))
    (should-not (file-exists-p "c:/echo"))))

(ert-deftest harmless-auto-review-mode-is-saved ()
  (harmless-auto-test-with data repo
    (let ((slash (file-exists-p "/.harmless"))
          (cdrive (file-exists-p "c:/.harmless"))
          (root (harmless-sessions-root))
          (provider (harmless-auto-make-fake
                     :name "fake" :host "none" :script #'ignore)))
      (let ((before (and (file-directory-p root) (directory-files root))))
        (dolist (bad (list "" "auto" "/" "c:/" 'bogus))
          (let ((err (should-error
                      (harmless-session-new
                       :cwd repo :provider provider :model "m"
                       :permission-mode bad)
                      :type 'error)))
            (should (equal (error-message-string err)
                           "permission mode is not a mode"))))
        (let ((harmless-permission-mode 'bogus))
          (let ((err (should-error
                      (harmless-session-new
                       :cwd repo :provider provider :model "m")
                      :type 'error)))
            (should (equal (error-message-string err)
                           "permission mode is not a mode"))))
        (should (equal before
                       (and (file-directory-p root) (directory-files root)))))
      (let ((harmless-permission-mode 'auto))
        (let ((session (harmless-session-new
                        :cwd (file-name-as-directory repo)
                        :provider provider :model "m" :title "Auto")))
          (should (eq 'auto (harmless-session-permission-mode session)))
        (should (harmless-same-directory-p
                 (harmless-session-cwd session) repo))
        (harmless-auto-test-assert-session session)
        (let ((file (expand-file-name
                     "summary.json" (harmless-session-dir session))))
          (should (file-name-absolute-p file))
          (should (harmless-directory-strictly-under-p
                   (file-name-directory file) data))
          (should-not (harmless-filesystem-root-p file))
          (should (equal "auto"
                         (plist-get (harmless-auto-test-summary session)
                                    :permission-mode)))
          (should (string-search "\"auto\"" (harmless-auto-test-read file))))))
      (let ((session (harmless-session-new
                      :cwd repo :provider provider :model "m"
                      :permission-mode nil)))
        (should (eq 'ask (harmless-session-permission-mode session))))
      (let ((session (harmless-auto-test-session repo)))
        (with-temp-buffer
          (setq-local harmless--session session)
          (harmless-set-permission-mode 'auto)
          (should (eq 'auto (harmless-session-permission-mode session)))
          (should (equal "auto"
                         (plist-get (harmless-auto-test-summary session)
                                    :permission-mode)))
          (let ((err (should-error (harmless-set-permission-mode 'nope)
                                   :type 'error)))
            (should (equal (error-message-string err)
                           "permission mode is not a mode")))
          (let ((err (should-error (harmless-set-permission-mode "/")
                                   :type 'error)))
            (should (equal (error-message-string err)
                           "permission mode is not a mode")))
          (should (eq 'auto (harmless-session-permission-mode session)))
          (should (equal "auto"
                         (plist-get (harmless-auto-test-summary session)
                                    :permission-mode))))
        (dolist (bad (list "nope" "" nil))
          (let* ((file (expand-file-name
                        "summary.json" (harmless-session-dir session)))
                 (plist (harmless-auto-test-summary session)))
            (setq plist (plist-put (copy-sequence plist)
                                   :permission-mode bad))
            (let ((coding-system-for-write 'utf-8-unix))
              (with-temp-file file
                (insert (harmless-json-text plist) "\n")))
            (harmless-session-unregister session)
            (let ((loaded (harmless-session-load
                           (harmless-session-dir session))))
              (should (eq 'ask (harmless-session-permission-mode loaded)))
              (setq session loaded))))
        (let* ((file (expand-file-name
                      "summary.json" (harmless-session-dir session)))
               (plist (plist-put (copy-sequence
                                  (harmless-auto-test-summary session))
                                 :permission-mode "auto")))
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file file
              (insert (harmless-json-text plist) "\n")))
          (harmless-session-unregister session)
          (let ((loaded (harmless-session-load
                         (harmless-session-dir session))))
            (should (eq 'auto (harmless-session-permission-mode loaded)))
            (harmless-auto-test-assert-session loaded))))
      (let ((detached (harmless-session-new
                       :detached t :provider provider :model "m"
                       :permission-mode 'auto :title "Detached")))
        (should (eq 'auto (harmless-session-permission-mode detached)))
        (should-not (harmless-session-cwd detached))
        (harmless-auto-test-assert-session detached))
      (should (eq slash (file-exists-p "/.harmless")))
      (should (eq cdrive (file-exists-p "c:/.harmless"))))))

(ert-deftest harmless-auto-review-before-the-question ()
  (harmless-auto-test-with data repo
    (let ((slash (file-exists-p "/.harmless"))
          (cdrive (file-exists-p "c:/.harmless"))
          (root-note (file-exists-p "/note.txt"))
          (sessions nil))
      (unwind-protect
          (progn
            (let ((session (harmless-auto-test-session repo)))
              (push session sessions)
              (setf (harmless-session-permission-mode session) 'ask)
              (setq harmless-auto-review-function
                    (lambda (&rest _)
                      (push 'classifier harmless-auto-test-reviews)
                      'allow))
              (harmless-auto-test-run
               session "harmless-tool-run-shell"
               '(:command "echo allowed>harmless-auto-ok.txt"))
              (should-not harmless-auto-test-reviews)
              (should (equal harmless-auto-test-asked
                             '((shell "echo allowed>harmless-auto-ok.txt"))))
              (should (equal (harmless-auto-test-last session)
                             "Error: user denied this tool call"))
              (should-not (file-exists-p
                           (expand-file-name "harmless-auto-ok.txt" repo))))
            (let* ((path (expand-file-name "harmless-auto-ok.txt" repo))
                   (session (harmless-auto-test-session
                             (file-name-as-directory repo)
                             :permission-mode 'auto))
                   (early nil))
              (push session sessions)
              (setq harmless-auto-review-function nil)
              (setq harmless-auto-review-model-function
                    (lambda (_session _tool args callback)
                      (push (harmless-tool-arg args :command)
                            harmless-auto-test-reviews)
                      (run-with-timer
                       0.05 nil
                       (lambda ()
                         (setq early (file-exists-p path))
                         (funcall callback 'allow)))))
              (harmless-auto-test-run
               session "harmless-tool-run-shell"
               '(:command "echo allowed>harmless-auto-ok.txt"))
              (should-not early)
              (should (equal harmless-auto-test-reviews
                             '("echo allowed>harmless-auto-ok.txt")))
              (should-not harmless-auto-test-asked)
              (should (eq 'idle (harmless-session-status session)))
              (harmless-auto-test-assert-written path repo)
              (should (string-match-p "allowed" (harmless-auto-test-read path)))
              (harmless-auto-test-assert-session session))
            (let ((session (harmless-auto-test-session
                            repo :permission-mode 'auto))
                  (path (expand-file-name "skipped.txt" repo)))
              (push session sessions)
              (setq harmless-auto-review-function
                    (lambda (_session tool args)
                      (push (list (harmless-tool-name tool)
                                  (harmless-tool-arg args :command)
                                  (harmless-tool-arg args :path))
                            harmless-auto-test-reviews)
                      'escalate))
              (harmless-auto-test-run
               session "harmless-tool-run-shell"
               '(:command "echo allowed>skipped.txt"))
              (should (equal harmless-auto-test-reviews
                             '(("harmless-tool-run-shell" "echo allowed>skipped.txt" nil))))
              (should (equal harmless-auto-test-asked
                             '((shell "echo allowed>skipped.txt"))))
              (should (equal (harmless-auto-test-last session)
                             "Error: user denied this tool call"))
              (should-not (file-exists-p path)))
            (let ((session (harmless-auto-test-session
                            repo :permission-mode 'auto)))
              (push session sessions)
              (setq harmless-auto-review-function
                    (lambda (&rest _) (error "classifier down")))
              (harmless-auto-test-run
               session "harmless-tool-write-file"
               '(:path "../note.txt" :contents "nope\n"))
              (should (equal harmless-auto-test-asked
                             '((edit "../note.txt"))))
              (should (equal (harmless-auto-test-last session)
                             "Error: user denied this tool call"))
              (should-not (file-exists-p
                           (expand-file-name "../note.txt" repo)))
              (should-not (file-exists-p "/note.txt")))
            (let ((session (harmless-auto-test-session
                            repo :permission-mode 'auto))
                  (path (expand-file-name "note.txt" repo)))
              (push session sessions)
              (setq harmless-auto-review-function
                    (lambda (_session _tool args)
                      (let ((path (harmless-tool-arg args :path)))
                        (push path harmless-auto-test-reviews)
                        (if (equal path "note.txt") 'allow 'escalate))))
              (harmless-auto-test-run
               session "harmless-tool-write-file"
               '(:path "note.txt/" :contents "stay\n"))
              (should (equal harmless-auto-test-reviews '("note.txt/")))
              (should (equal harmless-auto-test-asked '((edit "note.txt/"))))
              (should (equal (harmless-auto-test-last session)
                             "Error: user denied this tool call"))
              (should-not (file-exists-p path))
              (harmless-auto-test-run
               session "harmless-tool-write-file"
               '(:path "/" :contents "stay\n"))
              (should (equal harmless-auto-test-reviews '("/")))
              (should (equal harmless-auto-test-asked '((edit "/"))))
              (should-not (file-exists-p "/note.txt"))
              (harmless-auto-test-run
               session "harmless-tool-write-file"
               '(:path "note.txt" :contents "stay\n"))
              (should (equal harmless-auto-test-reviews '("note.txt")))
              (harmless-auto-test-assert-written path repo)
              (should (equal "stay\n" (harmless-auto-test-read path))))
            (let ((session (harmless-auto-test-session
                            repo :permission-mode 'auto)))
              (push session sessions)
              (setq harmless-auto-review-function
                    (lambda (_session _tool args)
                      (push (harmless-tool-arg args :command)
                            harmless-auto-test-reviews)
                      'allow))
              (harmless-auto-test-run
               session "harmless-tool-run-shell" '(:command ""))
              (should (equal harmless-auto-test-reviews '("")))
              (should-not harmless-auto-test-asked)
              (should (equal (harmless-auto-test-last session)
                             "Error: command is empty"))
              (harmless-auto-test-run
               session "harmless-tool-run-shell" '(:command nil))
              (should (equal harmless-auto-test-reviews '(nil)))
              (should (equal (harmless-auto-test-last session)
                             "Error: command is empty")))
            (let ((session (harmless-auto-test-session
                            repo :permission-mode 'auto)))
              (push session sessions)
              (setq harmless-auto-review-function
                    (lambda (&rest _)
                      (push 'reviewed harmless-auto-test-reviews)
                      'allow))
              (harmless-auto-test-run
               session "harmless-tool-run-shell" '(:command "echo hi"))
              (should-not harmless-auto-test-reviews)
              (should-not harmless-auto-test-asked)
              (should (string-match-p
                       "hi" (harmless-auto-test-last session))))
            (let ((session (harmless-auto-test-session
                            repo :permission-mode 'auto))
                  (harmless-dangerous-commands '("echo")))
              (push session sessions)
              (setq harmless-auto-review-function
                    (lambda (&rest _)
                      (push 'reviewed harmless-auto-test-reviews)
                      'allow))
              (harmless-auto-test-run
               session "harmless-tool-run-shell"
               '(:command "echo allowed>harmless-auto-danger.txt"))
              (should-not harmless-auto-test-reviews)
              (should-not harmless-auto-test-asked)
              (should (equal (harmless-auto-test-last session)
                             "Error: dangerous command refused: echo"))
              (should-not
               (file-exists-p
                (expand-file-name "harmless-auto-danger.txt" repo))))
            (let ((session (harmless-auto-test-session
                            repo :permission-mode 'auto)))
              (push session sessions)
              (setf (harmless-session-plan-mode session) t)
              (setq harmless-auto-review-function
                    (lambda (&rest _)
                      (push 'reviewed harmless-auto-test-reviews)
                      'allow))
              (harmless-auto-test-run
               session "harmless-tool-run-shell"
               '(:command "echo allowed>harmless-auto-plan.txt"))
              (should-not harmless-auto-test-reviews)
              (should-not harmless-auto-test-asked)
              (should (equal (harmless-auto-test-last session)
                             harmless-shell-plan-refusal))
              (should-not
               (file-exists-p
                (expand-file-name "harmless-auto-plan.txt" repo))))
            (let ((session (harmless-auto-test-session
                            repo :permission-mode 'auto)))
              (push session sessions)
              (harmless-perm-set-rules
               session '("Bash(echo allowed*)") nil)
              (setq harmless-auto-review-function
                    (lambda (&rest _)
                      (push 'reviewed harmless-auto-test-reviews)
                      'escalate))
              (harmless-auto-test-run
               session "harmless-tool-run-shell"
               '(:command "echo allowed>harmless-auto-rule.txt"))
              (should-not harmless-auto-test-reviews)
              (should-not harmless-auto-test-asked)
              (let ((path (expand-file-name
                           "harmless-auto-rule.txt" repo)))
                (harmless-auto-test-assert-written path repo)
                (should (string-match-p
                         "allowed" (harmless-auto-test-read path))))
              (harmless-perm-set-rules
               session nil '("Bash(echo allowed*)"))
              (harmless-auto-test-run
               session "harmless-tool-run-shell"
               '(:command "echo allowed>harmless-auto-denied.txt"))
              (should-not harmless-auto-test-reviews)
              (should-not harmless-auto-test-asked)
              (should (equal (harmless-auto-test-last session)
                             (concat "Error: Permission rule denied "
                                     "Bash(echo allowed*)")))
              (should-not
               (file-exists-p
                (expand-file-name "harmless-auto-denied.txt" repo))))
            (let ((session (harmless-auto-test-session
                            repo :permission-mode 'always-approve))
                  (path (expand-file-name "always.txt" repo)))
              (push session sessions)
              (setq harmless-auto-review-function
                    (lambda (&rest _)
                      (push 'reviewed harmless-auto-test-reviews)
                      'escalate))
              (harmless-auto-test-run
               session "harmless-tool-write-file"
               '(:path "always.txt" :contents "yes\n"))
              (should-not harmless-auto-test-reviews)
              (should-not harmless-auto-test-asked)
              (harmless-auto-test-assert-written path repo)
              (should (equal "yes\n" (harmless-auto-test-read path))))
            (let ((session (harmless-auto-test-session
                            repo :permission-mode 'accept-edits))
                  (path (expand-file-name "edit.txt" repo)))
              (push session sessions)
              (setq harmless-auto-review-function
                    (lambda (&rest _)
                      (push 'reviewed harmless-auto-test-reviews)
                      'escalate))
              (harmless-auto-test-run
               session "harmless-tool-write-file"
               '(:path "edit.txt" :contents "edit\n"))
              (should-not harmless-auto-test-reviews)
              (should-not harmless-auto-test-asked)
              (harmless-auto-test-assert-written path repo)
              (should (equal "edit\n" (harmless-auto-test-read path)))
              (harmless-auto-test-run
               session "harmless-tool-run-shell"
               '(:command "echo allowed>harmless-auto-edit-shell.txt"))
              (should-not harmless-auto-test-reviews)
              (should (equal harmless-auto-test-asked
                             '((shell
                                "echo allowed>harmless-auto-edit-shell.txt"))))
              (should-not
               (file-exists-p
                (expand-file-name
                 "harmless-auto-edit-shell.txt" repo))))
            (let ((child (harmless-session-new
                          :cwd repo
                          :provider (harmless-auto-make-fake
                                     :name "fake" :host "none"
                                     :script #'ignore)
                          :model "m" :source 'agent
                          :permission-mode 'auto :title "Child")))
              (push child sessions)
              (setf (harmless-session-agent child)
                    (list :allow 'all :sandbox 'read-only))
              (setq harmless-auto-review-function
                    (lambda (&rest _)
                      (push 'reviewed harmless-auto-test-reviews)
                      'allow))
              (harmless-auto-test-run
               child "harmless-tool-run-shell"
               '(:command "echo allowed>harmless-auto-child.txt"))
              (should-not harmless-auto-test-reviews)
              (should-not harmless-auto-test-asked)
              (should (equal (harmless-auto-test-last child)
                             harmless-agent-read-only-refusal))
              (should-not
               (file-exists-p
                (expand-file-name "harmless-auto-child.txt" repo))))
            (let ((ask (harmless-auto-test-session repo))
                  (auto (harmless-auto-test-session
                         repo :permission-mode 'auto)))
              (push ask sessions)
              (push auto sessions)
              (should (eq 'ask (harmless-session-permission-mode ask)))
              (should (eq 'auto (harmless-session-permission-mode auto)))
              (should-not (eq ask auto)))
            (should (eq slash (file-exists-p "/.harmless")))
            (should (eq cdrive (file-exists-p "c:/.harmless")))
            (should (eq root-note (file-exists-p "/note.txt")))
            (should-not (file-exists-p "/harmless-auto-ok.txt"))
            (should-not (file-exists-p "c:/harmless-auto-ok.txt")))
        (dolist (session sessions)
          (dolist (job (harmless-session-shell-jobs session))
            (let ((proc (harmless-shell-job-proc job)))
              (when (and (processp proc) (process-live-p proc))
                (delete-process proc)))))))))
