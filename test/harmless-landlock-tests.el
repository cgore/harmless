;;; harmless-landlock-tests.el --- Tests for the Landlock shell sandbox -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(defun harmless-landlock-test--wait (predicate)
  "Return non-nil when PREDICATE becomes true within about eight seconds."
  (let ((n 0))
    (while (and (not (funcall predicate)) (< n 80))
      (setq n (1+ n))
      (accept-process-output nil 0.1))
    (funcall predicate)))

(defun harmless-landlock-test--call (session args)
  "Run the shell tool for ARGS and return the callback text."
  (let (result done)
    (harmless-tools-shell--run
     session args
     (lambda (text)
       (setq result text done t)))
    (should (harmless-landlock-test--wait (lambda () done)))
    result))

(defun harmless-landlock-test--provider ()
  "Return a local provider that is never contacted."
  (harmless-make-openai-compat
   "local" :host "127.0.0.1:9" :protocol "http"
   :key "none" :models '("m")))

(defun harmless-landlock-test--contents (file)
  "Return the full text of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun harmless-landlock-test--summary (session)
  "Return SESSION's summary plist."
  (harmless-json-decode
   (harmless-landlock-test--contents
    (expand-file-name "summary.json" (harmless-session-dir session)))))

(defun harmless-landlock-test--argv (session)
  "Return the process argv of SESSION's newest shell."
  (let ((job (car (harmless-session-shell-jobs session))))
    (should (harmless-shell-job-p job))
    (process-command (harmless-shell-job-proc job))))

(defun harmless-landlock-test--flag (argv flag)
  "Return the argument after FLAG in ARGV."
  (let ((tail (member flag argv)))
    (and tail (cadr tail))))

(defun harmless-landlock-test--flags (argv flag)
  "Return every argument that follows FLAG in ARGV."
  (let (out)
    (while argv
      (when (and (stringp (car argv)) (string= (car argv) flag))
        (push (cadr argv) out))
      (setq argv (cdr argv)))
    (nreverse out)))

(defun harmless-landlock-test--stop (session)
  "Kill shell processes SESSION still has."
  (dolist (job (copy-sequence (harmless-session-shell-jobs session)))
    (let ((proc (harmless-shell-job-proc job)))
      (when (and (processp proc) (process-live-p proc))
        (ignore-errors (delete-process proc)))))
  (harmless-landlock-test--wait
   (lambda ()
     (not (cl-some (lambda (job)
                     (let ((proc (harmless-shell-job-proc job)))
                       (and (processp proc) (process-live-p proc))))
                   (harmless-session-shell-jobs session))))))

(defun harmless-landlock-test--delete (dir parent)
  "Delete DIR when it is a directory strictly inside PARENT."
  (when (and (stringp dir)
             (stringp parent)
             (file-directory-p dir)
             (not (harmless-filesystem-root-p dir))
             (not (harmless-filesystem-root-p parent))
             (harmless-directory-strictly-under-p dir parent))
    (delete-directory dir t)))

(defun harmless-landlock-test--assert-not-root ()
  "Assert Harmless did not write session files at /."
  (dolist (name '("summary.json" "messages.jsonl" "config.el"))
    (should-not (file-exists-p (concat "/" name)))))

(defmacro harmless-landlock-test--with (session &rest body)
  "Run BODY with SESSION in a temporary project.
The data directory stays inside that project for BODY."
  (declare (indent 1))
  `(let* ((root (make-temp-file "harmless-ll-" t))
          (dir (expand-file-name "proj" root))
          (harmless-directory (expand-file-name ".harmless" dir))
          (harmless--sessions (make-hash-table :test 'equal))
          (provider (harmless-landlock-test--provider))
          (harmless-providers (list provider)))
     (make-directory dir t)
     (unwind-protect
         (let ((,session (harmless-session-new
                          :cwd dir :provider provider :model "m")))
           ,@body
           (harmless-landlock-test--assert-not-root))
       (maphash (lambda (_key live)
                  (harmless-landlock-test--stop live))
                harmless--sessions)
       (harmless-landlock-test--delete root temporary-file-directory))))

(ert-deftest harmless-landlock-sandbox-defaults-to-off ()
  (harmless-landlock-test--with session
    (should (eq 'off (harmless-session-shell-sandbox session)))
    (should (eq 'off (harmless-shell-sandbox-effective session)))
    (let ((file (expand-file-name
                 "summary.json" (harmless-session-dir session)))
          (text (harmless-landlock-test--call session '(:command "echo harmless-ll-marker"))))
      (should (file-name-absolute-p file))
      (should (harmless-directory-strictly-under-p
               (file-name-directory file) harmless-directory))
      (should-not (equal file "/summary.json"))
      (should-not (string-search "shell-sandbox"
                                 (harmless-landlock-test--contents file)))
      (should (string-search "harmless-ll-marker" text))
      (should (string-prefix-p "exit 0\n" text))
      (let ((argv (harmless-landlock-test--argv session)))
        (should-not (cl-some (lambda (arg)
                               (and (stringp arg)
                                    (string-search "harmless-landlock" arg)))
                             argv))))
    (let ((harmless-shell-sandbox 'read-only))
      (let ((other (harmless-session-new
                    :cwd (harmless-session-cwd session)
                    :provider (car harmless-providers)
                    :model "m")))
        (should (eq 'read-only (harmless-session-shell-sandbox other)))
        (should (equal "read-only"
                       (plist-get (harmless-landlock-test--summary other)
                                  :shell-sandbox)))))
    (dolist (bad (list nil "" 'nope :workspace-write 'workspace))
      (let ((harmless-shell-sandbox bad))
        (let ((err (should-error
                    (harmless-session-new
                     :cwd (harmless-session-cwd session)
                     :provider (car harmless-providers)
                     :model "m")
                    :type 'error)))
          (should (equal (error-message-string err)
                         "shell sandbox must be off, read-only, or workspace-write")))))
    (let ((err (should-error
                (harmless-session-new
                 :cwd (harmless-session-cwd session)
                 :provider (car harmless-providers)
                 :model "m"
                 :shell-sandbox nil)
                :type 'error)))
      (should (equal (error-message-string err)
                     "shell sandbox must be off, read-only, or workspace-write")))
    (should (eq 'off (harmless-shell-sandbox-from-summary nil)))
    (should (eq 'off (harmless-shell-sandbox-from-summary "")))
    (should (eq 'off (harmless-shell-sandbox-from-summary "nope")))
    (should (eq 'off (harmless-shell-sandbox-from-summary :nope)))
    (should (eq 'off (harmless-shell-sandbox-from-summary :false)))
    (should (eq 'off (harmless-shell-sandbox-from-summary t)))
    (should (eq 'off (harmless-shell-sandbox-from-summary "/")))
    (should (eq 'off (harmless-shell-sandbox-from-summary "c:/")))
    (should (eq 'off (harmless-shell-sandbox-from-summary " c:/")))
    (should (eq 'workspace-write
                (harmless-shell-sandbox-from-summary :workspace-write)))
    (should (eq 'workspace-write
                (harmless-shell-sandbox-from-summary 'workspace-write)))
    (should (eq 'read-only
                (harmless-shell-sandbox-from-summary "read-only")))))

(ert-deftest harmless-landlock-setter-refuses-a-bad-mode ()
  (let ((harmless--sessions (make-hash-table :test 'equal)))
    (with-temp-buffer
      (setq-local default-directory
                  (file-name-as-directory
                   (make-temp-file "harmless-ll-ctx-" t)))
      (unwind-protect
          (let ((err (should-error (harmless-set-shell-sandbox 'off)
                                   :type 'user-error)))
            (should (equal (error-message-string err) "No Harmless session"))
            (harmless-landlock-test--assert-not-root))
        (harmless-landlock-test--delete default-directory
                                        temporary-file-directory))))
  (harmless-landlock-test--with session
    (let ((file (expand-file-name
                 "summary.json" (harmless-session-dir session)))
          (before nil))
      (setq before (harmless-landlock-test--contents file))
      (with-temp-buffer
        (setq-local harmless--session session)
        (dolist (bad (list 'nope nil ""))
          (let ((err (should-error (harmless-set-shell-sandbox bad)
                                   :type 'error)))
            (should (equal (error-message-string err)
                           "shell sandbox must be off, read-only, or workspace-write"))))
        (should (eq 'off (harmless-session-shell-sandbox session)))
        (should (equal before (harmless-landlock-test--contents file)))
        (harmless-set-shell-sandbox 'workspace-write)
        (should (eq 'workspace-write (harmless-session-shell-sandbox session)))
        (should (equal "workspace-write"
                       (plist-get (harmless-landlock-test--summary session)
                                  :shell-sandbox)))
        (should (string-search "workspace-write"
                               (harmless-landlock-test--contents file)))
        (harmless-set-shell-sandbox 'off)
        (should (eq 'off (harmless-session-shell-sandbox session)))
        (should-not (plist-get (harmless-landlock-test--summary session)
                               :shell-sandbox))
        (should-not (string-search "shell-sandbox"
                                   (harmless-landlock-test--contents file)))))))

(ert-deftest harmless-landlock-menu-key ()
  (should (equal (harmless-menu-key 'harmless-set-shell-sandbox) "S")))

(ert-deftest harmless-landlock-summary-round-trip ()
  (harmless-landlock-test--with session
    (setf (harmless-session-shell-sandbox session) 'workspace-write)
    (harmless-session-save session)
    (let ((file (expand-file-name
                 "summary.json" (harmless-session-dir session))))
      (should (file-name-absolute-p file))
      (should (harmless-directory-strictly-under-p
               (file-name-directory file) harmless-directory))
      (should-not (equal file "/summary.json"))
      (should (equal "workspace-write"
                     (plist-get (harmless-landlock-test--summary session)
                                :shell-sandbox))))
    (harmless-session-unregister session)
    (let ((loaded (harmless-session-load (harmless-session-dir session))))
      (should (eq 'workspace-write (harmless-session-shell-sandbox loaded)))
      (setq session loaded))
    (let* ((file (expand-file-name
                  "summary.json" (harmless-session-dir session)))
           (plist (plist-put (copy-sequence
                              (harmless-landlock-test--summary session))
                             :shell-sandbox "nope")))
      (let ((coding-system-for-write 'utf-8-unix))
        (with-temp-file file
          (insert (harmless-json-text plist) "\n")))
      (harmless-session-unregister session)
      (let ((loaded (harmless-session-load (harmless-session-dir session))))
        (should (eq 'off (harmless-session-shell-sandbox loaded)))
        (should-not (string-search
                     "shell-sandbox"
                     (progn
                       (harmless-session-save loaded)
                       (harmless-landlock-test--contents file))))))))

(ert-deftest harmless-landlock-fork-and-agent-copy-the-mode ()
  (harmless-landlock-test--with session
    (setf (harmless-session-shell-sandbox session) 'workspace-write)
    (harmless-session-save session)
    (let ((child (harmless-fork--create session nil)))
      (should (eq 'workspace-write (harmless-session-shell-sandbox child)))
      (should (equal "workspace-write"
                     (plist-get (harmless-landlock-test--summary child)
                                :shell-sandbox)))
      (should (eq 'workspace-write (harmless-shell-sandbox-effective child))))
    (let ((child (harmless-agent-create session '(:prompt "look") #'ignore)))
      (should (eq 'agent (harmless-session-source child)))
      (should (eq 'workspace-write (harmless-session-shell-sandbox child)))
      (setf (harmless-session-agent child)
            (plist-put (copy-sequence (harmless-session-agent child))
                       :sandbox 'read-only))
      (should (eq 'read-only (harmless-shell-sandbox-effective child))))
    (setf (harmless-session-shell-sandbox session) 'off)
    (harmless-session-save session)
    (let ((child (harmless-agent-create session '(:prompt "look") #'ignore)))
      (should (eq 'off (harmless-session-shell-sandbox child)))
      (should-not (plist-get (harmless-landlock-test--summary child)
                             :shell-sandbox))
      (setf (harmless-session-agent child)
            (plist-put (copy-sequence (harmless-session-agent child))
                       :sandbox 'read-only))
      (should (eq 'off (harmless-shell-sandbox-effective child)))
      (let ((text (harmless-landlock-test--call
                   child '(:command "echo harmless-ll-marker"))))
        (should (string-search "harmless-ll-marker" text))
        (should-not (cl-some
                     (lambda (arg)
                       (and (stringp arg)
                            (string-search "harmless-landlock" arg)))
                     (harmless-landlock-test--argv child)))))
    (setf (harmless-session-shell-sandbox session) 'workspace-write)
    (setf (harmless-session-source session) 'agent)
    (setf (harmless-session-agent session) '(:allow all :sandbox read-only))
    (let ((tool (harmless-tool-by-name "run_shell")))
      (should (eq 'network
                  (harmless-turn--class
                   tool session '(:command "echo hi" :network t))))
      (should (equal harmless-agent-read-only-refusal
                     (harmless-agent-refuse-call
                      session tool '(:command "echo hi" :network t))))
      (should (eq 'read
                  (harmless-turn--class tool session '(:command "echo hi"))))
      (should-not (harmless-agent-refuse-call
                   session tool '(:command "echo hi"))))
    (setf (harmless-session-shell-sandbox session) 'off)
    (should (eq 'read
                (harmless-turn--class
                 (harmless-tool-by-name "run_shell")
                 session '(:command "echo hi" :network t))))
    (should-not (harmless-agent-refuse-call
                 session (harmless-tool-by-name "run_shell")
                 '(:command "echo hi" :network t)))))

(ert-deftest harmless-landlock-refuses-without-linux ()
  (harmless-landlock-test--with session
    (setf (harmless-session-shell-sandbox session) 'workspace-write)
    (let ((system-type 'darwin)
          (harmless-landlock-program "/no/such/harmless-landlock")
          (harmless-landlock--abi-cache nil)
          (calls 0))
      (cl-letf (((symbol-function 'harmless-landlock-abi)
                 (lambda ()
                   (setq calls (1+ calls))
                   1)))
        (should (equal "Error: Landlock sandbox requires Linux"
                       (harmless-landlock-test--call
                        session '(:command "echo hi"))))
        (should (= calls 0))
        (should (equal "Error: command is empty"
                       (harmless-landlock-test--call
                        session '(:command ""))))
        (setf (harmless-session-plan-mode session) t)
        (should (equal harmless-shell-plan-refusal
                       (harmless-landlock-test--call
                        session '(:command "echo hi > out.txt"))))
        (setf (harmless-session-plan-mode session) nil)
        (should-not (harmless-session-shell-jobs session))))))

(ert-deftest harmless-landlock-refuses-a-missing-helper ()
  (harmless-landlock-test--with session
    (setf (harmless-session-shell-sandbox session) 'workspace-write)
    (let ((script (expand-file-name "not-exec"
                                    (harmless-session-cwd session))))
      (let ((coding-system-for-write 'utf-8-unix))
        (with-temp-file script (insert "nope\n")))
      (set-file-modes script #o644)
      (dolist (program (list "/no/such/harmless-landlock"
                             "harmless-landlock"
                             script
                             (harmless-session-cwd session)))
        (let ((system-type 'gnu/linux)
              (harmless-landlock-program program)
              (harmless-landlock--abi-cache nil))
          (should (equal "Error: Landlock helper is missing"
                         (harmless-landlock-test--call
                          session '(:command "echo hi"))))
          (should-not harmless-landlock--abi-cache))))
    (let ((harmless-landlock-program "/no/such/harmless-landlock")
          (harmless-landlock--abi-cache nil))
      (let ((err (should-error (harmless-landlock-abi) :type 'error)))
        (should (equal (error-message-string err)
                       "Landlock helper is missing")))
      (should-not harmless-landlock--abi-cache))
    (should-not (harmless-session-shell-jobs session))))

(ert-deftest harmless-landlock-refuses-network-on-an-old-abi ()
  (harmless-landlock-test--with session
    (setf (harmless-session-shell-sandbox session) 'workspace-write)
    (let ((system-type 'gnu/linux))
      (cl-letf (((symbol-function 'harmless-landlock-abi) (lambda () 3)))
        (should (equal "Error: Landlock cannot restrict the network on this kernel"
                       (harmless-landlock-test--call
                        session '(:command "echo hi"))))
        (should (equal "Error: Landlock cannot restrict the network on this kernel"
                       (harmless-landlock-test--call
                        session '(:command "echo hi" :network :false))))
        (let ((argv (harmless-landlock-wrap
                     session t '("sh" "-c" "echo hi"))))
          (should (equal "allow"
                         (harmless-landlock-test--flag argv "--network")))
          (should (member "--read" argv))
          (should (equal "/" (harmless-landlock-test--flag argv "--read")))))
      (should (eq 'network
                  (harmless-shell-run-class
                   session '(:command "echo hi" :network t))))
      (should (eq 'read
                  (harmless-shell-run-class
                   session '(:command "echo hi" :network "yes"))))
      (should (eq 'shell
                  (harmless-shell-run-class
                   session '(:command "echo hi > out.txt" :network "yes"))))
      (setf (harmless-session-shell-sandbox session) 'off)
      (should (eq 'read
                  (harmless-shell-run-class
                   session '(:command "echo hi" :network t)))))))

(ert-deftest harmless-landlock-network-must-be-a-boolean ()
  (should (eq 'deny (harmless-landlock--network '(:command "echo"))))
  (should (eq 'deny (harmless-landlock--network '(:network nil))))
  (should (eq 'deny (harmless-landlock--network '(:network :false))))
  (should (eq 'allow (harmless-landlock--network '(:network t))))
  (should (equal "Error: network must be a boolean"
                 (harmless-landlock--network '(:network "yes"))))
  (should (equal "Error: network must be a boolean"
                 (harmless-landlock--network '("network" "yes"))))
  (should (equal "Error: network must be a boolean"
                 (harmless-landlock--network '(:network false))))
  (harmless-landlock-test--with session
    (let ((text (harmless-landlock-test--call
                 session '(:command "echo harmless-ll-marker" :network "yes"))))
      (should (string-search "harmless-ll-marker" text))
      (should-not (cl-some
                   (lambda (arg)
                     (and (stringp arg)
                          (string-search "harmless-landlock" arg)))
                   (harmless-landlock-test--argv session))))
    (setf (harmless-session-shell-sandbox session) 'workspace-write)
    (let ((jobs (length (harmless-session-shell-jobs session))))
      (should (equal "Error: network must be a boolean"
                     (harmless-landlock-test--call
                      session '(:command "echo hi" :network "yes"))))
      (should (equal "Error: network must be a boolean"
                     (harmless-landlock-test--call
                      session '("command" "echo hi" "network" "yes"))))
      (should (equal "Error: command is empty"
                     (harmless-landlock-test--call
                      session '(:command "" :network t))))
      (should (= jobs (length (harmless-session-shell-jobs session)))))))

(ert-deftest harmless-landlock-clean-dir-rejects-roots-and-foreign-paths ()
  (let ((default-directory "/")
        (dir (make-temp-file "harmless-ll-clean-" t))
        (file (make-temp-file "harmless-ll-clean-file-")))
    (unwind-protect
        (progn
          (should-not (harmless-landlock--clean-dir nil))
          (should-not (harmless-landlock--clean-dir ""))
          (should-not (harmless-landlock--clean-dir "tmp"))
          (should-not (harmless-landlock--clean-dir "/"))
          (should-not (harmless-landlock--clean-dir "//"))
          (should-not (harmless-landlock--clean-dir "/."))
          (should-not (harmless-landlock--clean-dir "/.."))
          (should-not (harmless-landlock--clean-dir "\\"))
          (should-not (harmless-landlock--clean-dir "c:/"))
          (should-not (harmless-landlock--clean-dir "c:/Users"))
          (should-not (harmless-landlock--clean-dir "~/"))
          (should-not (harmless-landlock--clean-dir file))
          (should-not (file-exists-p "/c:/Users"))
          (let ((clean (harmless-landlock--clean-dir
                        (file-name-as-directory dir))))
            (should (stringp clean))
            (should (file-name-absolute-p clean))
            (should (directory-name-p clean))
            (should (harmless-same-directory-p clean dir))
            (should-not (harmless-filesystem-root-p clean))))
      (harmless-landlock-test--delete dir temporary-file-directory)
      (when (file-exists-p file)
        (delete-file file))))
  (dolist (dev (harmless-landlock--devices))
    (should (string-prefix-p "/dev/" dev))
    (should-not (file-directory-p dev))
    (should-not (harmless-filesystem-root-p dev))))

(defun harmless-landlock-test--helper (args)
  "Run the Landlock helper with ARGS.
Return a list of the exit status, stdout, and stderr.
Standard error is a file: `call-process' does not accept a buffer there."
  (let ((program (harmless-landlock--program))
        (stdout (generate-new-buffer " *ll-out*"))
        (stderr (make-temp-file "harmless-ll-err-")))
    (unwind-protect
        (let ((status (apply #'call-process program nil
                             (list stdout stderr) nil args)))
          (list status
                (with-current-buffer stdout (buffer-string))
                (with-temp-buffer
                  (insert-file-contents stderr)
                  (buffer-string))))
      (when (buffer-live-p stdout)
        (kill-buffer stdout))
      (when (and (stringp stderr)
                 (file-exists-p stderr)
                 (file-name-absolute-p stderr)
                 (not (harmless-filesystem-root-p stderr)))
        (delete-file stderr)))))

(ert-deftest harmless-landlock-helper-refuses-bad-paths ()
  (skip-unless (eq system-type 'gnu/linux))
  (let ((program (harmless-landlock--program)))
    (should (file-name-absolute-p program))
    (should (file-executable-p program))
    (should-not (file-directory-p program)))
  (let* ((harmless-landlock--abi-cache nil)
         (abi (harmless-landlock-abi))
         (true (executable-find "true")))
    (should (integerp abi))
    (should (>= abi 4))
    (should (equal (car harmless-landlock--abi-cache)
                   (harmless-landlock--program)))
    (should (= abi (cdr harmless-landlock--abi-cache)))
    (let ((got (harmless-landlock-test--helper '("--abi"))))
      (should (eq 0 (nth 0 got)))
      (should (string-match-p "\\`[0-9]+\n\\'" (nth 1 got)))
      (should (equal "" (nth 2 got)))
      (should (>= (string-to-number (nth 1 got)) 4)))
    (should (file-name-absolute-p true))
    (let ((harmless-landlock-program true)
          (harmless-landlock--abi-cache nil))
      (let ((err (should-error (harmless-landlock-abi) :type 'error)))
        (should (equal (error-message-string err)
                       "Landlock is not available")))
      (should-not harmless-landlock--abi-cache)))
  (let ((got (harmless-landlock-test--helper
              '("--write" "/" "--network" "deny" "--" "true"))))
    (should (eq 127 (nth 0 got)))
    (should (equal "Error: refusing to sandbox-write the filesystem root\n"
                   (nth 2 got))))
  (dolist (root '("/." "//" "/.." "/../"))
    (let ((got (harmless-landlock-test--helper
                (list "--read" "/" "--write" root
                      "--network" "deny" "--" "true"))))
      (should (eq 127 (nth 0 got)))
      (should (equal "Error: refusing to sandbox-write the filesystem root\n"
                     (nth 2 got)))))
  (dolist (path '("relative" "" "c:/Users" "\\"))
    (let ((got (harmless-landlock-test--helper
                (list "--read" "/" "--write" path
                      "--network" "deny" "--" "true"))))
      (should (eq 127 (nth 0 got)))
      (should (equal "Error: sandbox path must be absolute\n" (nth 2 got)))))
  (let ((got (harmless-landlock-test--helper
              '("--read" "relative" "--network" "deny" "--" "true"))))
    (should (eq 127 (nth 0 got)))
    (should (equal "Error: sandbox path must be absolute\n" (nth 2 got))))
  (let ((got (harmless-landlock-test--helper
              '("--read" "/" "--network" "deny" "--write"))))
    (should (eq 127 (nth 0 got)))
    (should (equal "Error: sandbox path must be absolute\n" (nth 2 got))))
  (let ((got (harmless-landlock-test--helper
              '("--read" "/" "--write" "/no/such/harmless-landlock-dir"
                "--network" "deny" "--" "true"))))
    (should (eq 127 (nth 0 got)))
    (should (equal "Error: Landlock could not open /no/such/harmless-landlock-dir\n"
                   (nth 2 got))))
  (let ((got (harmless-landlock-test--helper
              '("--read" "/" "--network" "yes" "--" "true"))))
    (should (eq 127 (nth 0 got)))
    (should (equal "Error: network must be a boolean\n" (nth 2 got))))
  (let ((got (harmless-landlock-test--helper
              '("--read" "/" "--" "true"))))
    (should (eq 127 (nth 0 got)))
    (should (equal "Error: network must be a boolean\n" (nth 2 got))))
  (let ((got (harmless-landlock-test--helper '("--nope"))))
    (should (eq 127 (nth 0 got)))
    (should (equal "Error: Landlock could not restrict this process\n"
                   (nth 2 got))))
  (let ((got (harmless-landlock-test--helper
              '("--read" "/" "--network" "deny" "--"))))
    (should (eq 126 (nth 0 got)))
    (should (equal "Error: Landlock could not run\n" (nth 2 got))))
  (let ((got (harmless-landlock-test--helper
              '("--read" "/" "--network" "deny" "--"
                "/no/such/harmless-landlock-cmd"))))
    (should (eq 126 (nth 0 got)))
    (should (equal "Error: Landlock could not run /no/such/harmless-landlock-cmd\n"
                   (nth 2 got)))))

(ert-deftest harmless-landlock-confines-the-shell ()
  (skip-unless (eq system-type 'gnu/linux))
  (let* ((home (getenv "HOME"))
         (root (expand-file-name
                (format "harmless-ll-%s"
                        (file-name-nondirectory (make-temp-name "u")))
                home))
         (dir (expand-file-name "proj" root))
         (sib (expand-file-name "sib" root))
         (extra (expand-file-name "extra" root))
         (data (expand-file-name "data" root))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (provider (harmless-landlock-test--provider))
         (harmless-providers (list provider))
         (session nil)
         (tmp-files nil))
    (should (and (stringp home) (file-name-absolute-p home)))
    (should-not (harmless-filesystem-root-p home))
    (unwind-protect
        (progn
          (make-directory dir t)
          (make-directory sib t)
          (make-directory extra t)
          (dolist (parent (list temporary-file-directory
                                "/tmp" "/var/tmp" "/dev/shm"))
            (when (and (stringp parent)
                       (file-name-absolute-p parent)
                       (not (harmless-filesystem-root-p parent)))
              (should-not (or (harmless-same-directory-p dir parent)
                              (harmless-directory-strictly-under-p dir parent)))
              (should-not (or (harmless-same-directory-p sib parent)
                              (harmless-directory-strictly-under-p sib parent)))))
          (setq session (harmless-session-new
                         :cwd dir :provider provider :model "m"))
          (setf (harmless-session-shell-sandbox session) 'workspace-write)
          (let ((file (expand-file-name "out.txt" dir)))
            (should (string-prefix-p
                     "exit 0\n"
                     (harmless-landlock-test--call
                      session
                      (list :command
                            (format "echo hello > %s"
                                    (shell-quote-argument file))))))
            (should (equal "hello\n"
                           (harmless-landlock-test--contents file))))
          (let ((file (expand-file-name
                       (format "harmless-ll-%s.txt"
                               (file-name-nondirectory (make-temp-name "t")))
                       "/tmp")))
            (push file tmp-files)
            (should-not (file-exists-p file))
            (should (string-prefix-p
                     "exit 0\n"
                     (harmless-landlock-test--call
                      session
                      (list :command
                            (format "echo tmpdata > %s"
                                    (shell-quote-argument file))))))
            (should (equal "tmpdata\n"
                           (harmless-landlock-test--contents file))))
          (let ((file (expand-file-name "nope.txt" sib)))
            (should-not (string-prefix-p
                         "exit 0\n"
                         (harmless-landlock-test--call
                          session
                          (list :command
                                (format "cd -- %s && echo secret > nope.txt"
                                        (shell-quote-argument sib))))))
            (should-not (file-exists-p file)))
          (let ((file (expand-file-name "early.txt" extra)))
            (harmless-landlock-test--call
             session
             (list :command
                   (format "echo early > %s" (shell-quote-argument file))))
            (should-not (file-exists-p file)))
          (harmless-extra-add session extra)
          (let ((file (expand-file-name "added.txt" extra)))
            (should (string-prefix-p
                     "exit 0\n"
                     (harmless-landlock-test--call
                      session
                      (list :command
                            (format "echo added > %s"
                                    (shell-quote-argument file))))))
            (should (equal "added\n"
                           (harmless-landlock-test--contents file))))
          (should (string-prefix-p
                   "exit 0\n"
                   (harmless-landlock-test--call
                    session '(:command "echo hi > /dev/null"))))
          (setf (harmless-session-cwd session) (file-name-as-directory dir))
          (let ((file (expand-file-name "slash.txt" dir)))
            (should (string-prefix-p
                     "exit 0\n"
                     (harmless-landlock-test--call
                      session
                      (list :command
                            (format "echo slash > %s"
                                    (shell-quote-argument file))))))
            (should (equal "slash\n"
                           (harmless-landlock-test--contents file))))
          (let ((argv (harmless-landlock-test--argv session)))
            (should (string-suffix-p "harmless-landlock" (car argv)))
            (should (equal "/" (harmless-landlock-test--flag argv "--read")))
            (should (equal "deny"
                           (harmless-landlock-test--flag argv "--network")))
            (should-not (member "/" (harmless-landlock-test--flags
                                     argv "--write")))
            (should (cl-some
                     (lambda (path)
                       (string= (directory-file-name (file-truename path))
                                (directory-file-name (file-truename dir))))
                     (harmless-landlock-test--flags argv "--write")))
            (when (file-exists-p "/dev/null")
              (should (member "/dev/null"
                              (harmless-landlock-test--flags argv "--write")))))
          (setf (harmless-session-cwd session) dir)
          (setf (harmless-session-extra-dirs session)
                (list "/" "" nil "relative"
                      (expand-file-name "missing" root)))
          (should (string-search
                   "harmless-ll-marker"
                   (harmless-landlock-test--call
                    session '(:command "echo harmless-ll-marker"))))
          (should-not (member "/" (harmless-landlock-test--flags
                                   (harmless-landlock-test--argv session)
                                   "--write")))
          (make-directory data t)
          (let ((harmless-directory data))
            (setf (harmless-session-extra-dirs session) (list data))
            (let ((secret (expand-file-name "secret.txt" data))
                  (still (expand-file-name "still.txt" dir)))
              (harmless-landlock-test--call
               session
               (list :command
                     (format "echo secret > %s"
                             (shell-quote-argument secret))))
              (should-not (file-exists-p secret))
              (should (string-prefix-p
                       "exit 0\n"
                       (harmless-landlock-test--call
                        session
                        (list :command
                              (format "echo still > %s"
                                      (shell-quote-argument still))))))
              (should (equal "still\n"
                             (harmless-landlock-test--contents still)))
              (should-not
               (cl-some
                (lambda (path)
                  (or (string= (directory-file-name (file-truename path))
                               (directory-file-name (file-truename data)))
                      (harmless-directory-strictly-under-p path data)))
                (harmless-landlock-test--flags
                 (harmless-landlock-test--argv session) "--write")))))
          (setf (harmless-session-extra-dirs session) nil)
          (let ((missing (expand-file-name "missing" root))
                (jobs (length (harmless-session-shell-jobs session))))
            (should (equal (format "Error: Landlock could not open %s" missing)
                           (progn
                             (setf (harmless-session-cwd session) missing)
                             (harmless-landlock-test--call
                              session '(:command "echo hi")))))
            (should (= jobs (length (harmless-session-shell-jobs session))))
            (setf (harmless-session-cwd session) "/")
            (should (equal "Error: Landlock could not open /"
                           (harmless-landlock-test--call
                            session '(:command "echo hi > /harmless-ll-root.txt"))))
            (should-not (file-exists-p "/harmless-ll-root.txt")))
          (setf (harmless-session-cwd session) dir)
          (setf (harmless-session-source session) 'agent)
          (setf (harmless-session-agent session)
                '(:allow all :sandbox read-only))
          (let ((file (expand-file-name "agent.txt" dir)))
            (should-not (string-prefix-p
                         "exit 0\n"
                         (harmless-landlock-test--call
                          session
                          (list :command
                                (format "echo agent > %s"
                                        (shell-quote-argument file))))))
            (should-not (file-exists-p file))
            (should (string-search
                     "harmless-ll-marker"
                     (harmless-landlock-test--call
                      session '(:command "echo harmless-ll-marker"))))
            (should-not (member "--write"
                                (harmless-landlock-test--argv session))))
          (setf (harmless-session-source session) 'user)
          (setf (harmless-session-agent session) nil)
          (setf (harmless-session-shell-sandbox session) 'read-only)
          (let ((file (expand-file-name "ro.txt" dir))
                (tmp (expand-file-name
                      (format "harmless-ll-ro-%s.txt"
                              (file-name-nondirectory (make-temp-name "t")))
                      "/tmp")))
            (push tmp tmp-files)
            (should (string-search
                     "harmless-ll-marker"
                     (harmless-landlock-test--call
                      session '(:command "echo harmless-ll-marker"))))
            (should-not (member "--write"
                                (harmless-landlock-test--argv session)))
            (should (equal "deny"
                           (harmless-landlock-test--flag
                            (harmless-landlock-test--argv session) "--network")))
            (harmless-landlock-test--call
             session
             (list :command
                   (format "echo ro > %s" (shell-quote-argument file))))
            (should-not (file-exists-p file))
            (harmless-landlock-test--call
             session
             (list :command
                   (format "echo ro > %s" (shell-quote-argument tmp))))
            (should-not (file-exists-p tmp))
            (should-not (string-prefix-p
                         "exit 0\n"
                         (harmless-landlock-test--call
                          session '(:command "echo hi > /dev/null")))))
          (setf (harmless-session-shell-sandbox session) 'workspace-write)
          (setf (harmless-session-plan-mode session) t)
          (let ((file (expand-file-name "plan.txt" dir)))
            (should (equal harmless-shell-plan-refusal
                           (harmless-landlock-test--call
                            session
                            (list :command
                                  (format "echo plan > %s"
                                          (shell-quote-argument file))))))
            (should-not (file-exists-p file)))
          (setf (harmless-session-plan-mode session) nil)
          (let ((src (make-temp-file "harmless-ll-conn-" nil ".c"))
                (bin (make-temp-file "harmless-ll-conn-" nil ".bin")))
            (push src tmp-files)
            (push bin tmp-files)
            (with-temp-file src
              (insert "#include <stdio.h>\n#include <string.h>\n#include <errno.h>\n"
                      "#include <sys/socket.h>\n#include <netinet/in.h>\n"
                      "#include <arpa/inet.h>\n"
                      "int main(void) {\n"
                      "  int fd = socket(AF_INET, SOCK_STREAM, 0);\n"
                      "  struct sockaddr_in addr;\n"
                      "  memset(&addr, 0, sizeof addr);\n"
                      "  addr.sin_family = AF_INET;\n"
                      "  addr.sin_port = htons(1);\n"
                      "  inet_pton(AF_INET, \"127.0.0.1\", &addr.sin_addr);\n"
                      "  if (connect(fd, (struct sockaddr *)&addr, sizeof addr) == 0) {\n"
                      "    printf(\"errno 0\\n\");\n"
                      "    return 0;\n"
                      "  }\n"
                      "  printf(\"errno %d\\n\", errno);\n"
                      "  return 0;\n"
                      "}\n"))
            (should (eq 0 (call-process "gcc" nil nil nil "-O2" "-o" bin src)))
            (set-file-modes bin #o755)
            (dolist (args (list (list :command (shell-quote-argument bin))
                                (list :command (shell-quote-argument bin)
                                      :network :false)
                                (list :command (shell-quote-argument bin)
                                      :network nil)))
              (should (string-search
                       "errno 13"
                       (harmless-landlock-test--call session args))))
            (should (equal "deny"
                           (harmless-landlock-test--flag
                            (harmless-landlock-test--argv session) "--network")))
            (should (string-search
                     "errno 111"
                     (harmless-landlock-test--call
                      session (list :command (shell-quote-argument bin)
                                    :network t))))
            (should (equal "allow"
                           (harmless-landlock-test--flag
                            (harmless-landlock-test--argv session) "--network"))))
          (harmless-landlock-test--assert-not-root))
      (maphash (lambda (_key live)
                 (harmless-landlock-test--stop live))
               harmless--sessions)
      (dolist (file tmp-files)
        (when (and (stringp file) (file-exists-p file)
                   (not (harmless-filesystem-root-p file)))
          (if (file-directory-p file)
              (delete-directory file t)
            (delete-file file))))
      (harmless-landlock-test--delete root home))))
