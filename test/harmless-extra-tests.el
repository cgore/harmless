;;; harmless-extra-tests.el --- Tests for extra directories -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(defun harmless-extra-test-text (path)
  "Return the contents of PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun harmless-extra-test-error (message form)
  "Assert FORM signals MESSAGE."
  (let ((err (should-error (funcall form) :type 'error)))
    (should (equal message (error-message-string err)))))

(defmacro harmless-extra-test-env (&rest body)
  "Evaluate BODY with a project, an outside directory, and a session."
  (declare (indent 0))
  `(let* ((base (make-temp-file "harmless-extra-" t))
          (dir (expand-file-name "proj" base))
          (extra (expand-file-name "extra" base))
          (other (expand-file-name "other" base))
          (data (expand-file-name "harmless-data" base))
          (provider (harmless-make-openai-compat
                     "local" :host "127.0.0.1:9" :protocol "http"
                     :key "none" :models '("m")))
          (harmless-directory data)
          (harmless--sessions (make-hash-table :test 'equal))
          (harmless-providers nil)
          (session nil))
     (make-directory dir t)
     (make-directory extra t)
     (make-directory other t)
     (make-directory data t)
     (setq harmless-providers (list provider))
     (setq session (harmless-session-new
                    :cwd dir :provider provider :model "m"))
     ,@body))

(ert-deftest harmless-extra-refuses-bad-directories ()
  (let ((session (harmless-session--create :id "extra-bad" :cwd nil))
        (calls 0))
    (cl-letf (((symbol-function 'expand-file-name)
               (lambda (&rest _)
                 (setq calls (1+ calls))
                 "/"))
              ((symbol-function 'file-truename)
               (lambda (&rest _)
                 (setq calls (1+ calls))
                 "/")))
      (dolist (case '((nil "extra directory is empty")
                      ("" "extra directory is empty")
                      ("  " "extra directory is empty")
                      (12 "extra directory must be a string")
                      ("notes" "extra directory must be absolute")
                      ("../x" "extra directory must be absolute")
                      ("." "extra directory must be absolute")
                      ("~" "extra directory must be absolute")
                      ("~/x" "extra directory must be absolute")
                      ("~\\x" "extra directory must be absolute")
                      ("/" "extra directory is the filesystem root")
                      ("//" "extra directory is the filesystem root")
                      ("/." "extra directory is the filesystem root")
                      ("c:/" "extra directory is the filesystem root")
                      ("c:\\" "extra directory is the filesystem root")
                      ("d:/" "extra directory is the filesystem root")
                      ("C:/" "extra directory is the filesystem root")))
        (harmless-extra-test-error
         (cadr case)
         (lambda () (harmless-extra-normalize session (car case)))))
      (harmless-extra-restore
       session (list nil "" "notes" "/" "c:/" "~" "c:\\"))
      (should (= calls 0))
      (should-not (harmless-session-extra-dirs session)))))

(ert-deftest harmless-extra-reads-and-edits-outside-the-project ()
  (harmless-extra-test-env
    (let* ((note (expand-file-name "note.txt" extra))
           (proj (expand-file-name "proj.txt" dir))
           (secret (expand-file-name "secret.txt" other))
           (inside (expand-file-name "inside" dir))
           (missing (expand-file-name "missing-extra-dir" base))
           (plain (expand-file-name "plain.txt" other))
           (kept (expand-file-name "kept.txt" extra))
           (summary nil)
           (stored nil))
      (with-temp-file note (insert "UNIQUE-EXTRA\n"))
      (with-temp-file proj (insert "UNIQUE-PROJ\n"))
      (with-temp-file secret (insert "UNIQUE-NO\n"))
      (with-temp-file plain (insert "x"))
      (make-directory inside t)
      (should-not (plist-member (harmless-session-summary-plist session)
                                :extra-dirs))
      (should-not (harmless-session-shell-cwd session))
      (harmless-extra-test-error
       "extra directory is inside the project"
       (lambda () (harmless-extra-add session dir)))
      (harmless-extra-test-error
       "extra directory is inside the project"
       (lambda () (harmless-extra-add session (file-name-as-directory dir))))
      (harmless-extra-test-error
       "extra directory is inside the project"
       (lambda () (harmless-extra-add session inside)))
      (harmless-extra-test-error
       "extra directory is not a directory"
       (lambda () (harmless-extra-add session plain)))
      (let ((created nil))
        (cl-letf (((symbol-function 'make-directory)
                   (lambda (&rest _) (setq created t))))
          (harmless-extra-test-error
           "extra directory is missing"
           (lambda () (harmless-extra-add session missing)))
          (should-not created)
          (should-not (file-exists-p missing))))
      (let ((chat (harmless-session-new
                   :detached t :provider provider :model "m")))
        (harmless-extra-test-error
         "This chat is not attached to a project"
         (lambda () (harmless-extra-add chat extra)))
        (should-not (harmless-session-extra-dirs chat)))
      (setq stored (harmless-extra-add session extra))
      (should (= (length stored) 1))
      (should (equal (harmless-extra-add
                      session (file-name-as-directory extra))
                     stored))
      (should (equal (harmless-extra-add
                      session (directory-file-name extra))
                     stored))
      (let ((norm (car stored)))
        (should (file-name-absolute-p norm))
        (should-not (harmless-filesystem-root-p norm))
        (should (file-directory-p norm))
        (should (string-prefix-p
                 (file-name-as-directory (file-truename extra))
                 (file-name-as-directory (file-truename norm))))
        (should-not (harmless-tools-fs-inside-p dir norm)))
      (setq summary (harmless-session--file session "summary.json"))
      (should (file-name-absolute-p summary))
      (should-not (harmless-filesystem-root-p summary))
      (should-not (harmless-filesystem-root-p (file-name-directory summary)))
      (should (string-prefix-p
               (file-name-as-directory (file-truename data))
               (file-truename summary)))
      (should (string-search (directory-file-name (car stored))
                             (harmless-extra-test-text summary)))
      (should (plist-member (harmless-session-summary-plist session)
                            :extra-dirs))
      (should-not (harmless-session-shell-cwd session))
      (should (equal "UNIQUE-EXTRA\n"
                     (harmless-tools-fs--read session (list :path note))))
      (should (string-search
               "note.txt"
               (harmless-tools-fs--list session (list :path extra))))
      (let ((both (harmless-tools-fs--grep
                   session '(:pattern "UNIQUE-" :engine "elisp"))))
        (should (string-search "proj.txt:1:UNIQUE-PROJ" both))
        (should (string-match "\\(.+note\\.txt\\):1:UNIQUE-EXTRA" both))
        (should (file-name-absolute-p (match-string 1 both)))
        (should-not (string-search "UNIQUE-NO" both))
        (should-not (string-search "secret.txt" both)))
      (let ((only (harmless-tools-fs--grep
                   session (list :pattern "UNIQUE-" :engine "elisp"
                                 :path extra))))
        (should (string-search "UNIQUE-EXTRA" only))
        (should-not (string-search "UNIQUE-PROJ" only)))
      (let ((names (harmless-tools-fs--glob session '(:pattern "*.txt"))))
        (should (string-search "proj.txt" names))
        (should (string-match "\\(.+note\\.txt\\)" names))
        (should (file-name-absolute-p (match-string 1 names)))
        (should-not (string-search "secret.txt" names)))
      (harmless-extra-test-error
       "Path escapes project: ../other/secret.txt"
       (lambda ()
         (harmless-tools-fs-resolve session "../other/secret.txt")))
      (harmless-extra-test-error
       (format "Path escapes project: %s" secret)
       (lambda () (harmless-tools-fs-resolve session secret)))
      (harmless-extra-test-error
       "Path escapes project: /"
       (lambda () (harmless-tools-fs-resolve session "/")))
      (harmless-extra-test-error
       "Path escapes project: c:/"
       (lambda () (harmless-tools-fs-resolve session "c:/")))
      (harmless-extra-test-error
       "Path is empty"
       (lambda () (harmless-tools-fs-resolve session "")))
      (harmless-extra-test-error
       "Path is empty"
       (lambda () (harmless-tools-fs-resolve session nil)))
      (should (equal "UNIQUE-NO\n" (harmless-extra-test-text secret)))
      (let* ((resolved (harmless-tools-fs-resolve session kept))
             (wrote (harmless-tools-fs--write
                     session (list :path kept :contents "yes\n"))))
        (should (equal wrote (format "Wrote %s (%d bytes)"
                                     resolved (string-bytes "yes\n"))))
        (should (file-name-absolute-p resolved))
        (should-not (harmless-filesystem-root-p resolved))
        (should (string-prefix-p
                 (file-name-as-directory (file-truename extra))
                 (file-truename resolved)))
        (should (equal "yes\n" (harmless-extra-test-text resolved))))
      (harmless-tools-fs--replace
       session (list :path kept :old_string "yes\n" :new_string "no\n"))
      (should (equal "no\n" (harmless-extra-test-text kept)))
      (harmless-tools-fs--apply-patch
       session (list :hunks (list (list :path kept
                                        :old_string "no\n"
                                        :new_string "next\n"))))
      (should (equal "next\n" (harmless-extra-test-text kept)))
      (let ((deep (expand-file-name "sub/dir/c.txt" extra)))
        (harmless-tools-fs--write
         session (list :path deep :contents "deep\n"))
        (should (equal "deep\n" (harmless-extra-test-text deep)))
        (should (string-prefix-p
                 (file-name-as-directory (file-truename extra))
                 (file-truename deep))))
      (let ((project-file (expand-file-name "in.txt" dir))
            (resolved nil))
        (setq resolved (harmless-tools-fs-resolve session "in.txt"))
        (harmless-tools-fs--write
         session (list :path "in.txt" :contents "in\n"))
        (should (equal "in\n" (harmless-extra-test-text project-file)))
        (should (string-prefix-p
                 (file-name-as-directory (file-truename dir))
                 (file-truename resolved))))
      (let ((watched (harmless-monitor--file session note)))
        (should (file-name-absolute-p watched))
        (should (string-prefix-p
                 (file-name-as-directory (file-truename extra))
                 (file-truename watched))))
      (harmless-extra-test-error
       "Path escapes project: ../other/secret.txt"
       (lambda ()
         (harmless-monitor--file session "../other/secret.txt")))
      (setf (harmless-session-plan-mode session) t)
      (let ((blocked (expand-file-name "blocked.txt" extra)))
        (harmless-extra-test-error
         "Plan mode is on. Only plan.md may be edited, with write_plan"
         (lambda ()
           (harmless-tools-fs--write
            session (list :path blocked :contents "no\n"))))
        (should-not (file-exists-p blocked)))
      (setf (harmless-session-plan-mode session) nil)
      (should-not (harmless-session-shell-cwd session))
      (with-temp-buffer
        (setq harmless--session session)
        (harmless-add-directory (file-name-as-directory extra))
        (should (= (length (harmless-session-extra-dirs session)) 1))))))

(ert-deftest harmless-extra-roundtrip-fork-and-subagent ()
  (harmless-extra-test-env
    (let* ((note (expand-file-name "note.txt" extra))
           (more (expand-file-name "more" base))
           (missing (expand-file-name "missing-extra-dir" base))
           (inside (expand-file-name "inside" dir))
           (file (harmless-session--file session "summary.json"))
           (good nil)
           (plist nil)
           (loaded nil)
           (child nil)
           (agent nil))
      (with-temp-file note (insert "UNIQUE-EXTRA\n"))
      (make-directory more t)
      (make-directory inside t)
      (harmless-extra-add session extra)
      (harmless-extra-add session more)
      (setq good (harmless-session-extra-dirs session))
      (should (= (length good) 2))
      (setq plist (harmless-json-decode (harmless-extra-test-text file)))
      (setq plist (plist-put plist :extra-dirs
                             (list "/" "c:/" "c:\\" "notes" "" "  "
                                   "~" missing inside (car good))))
      (let ((coding-system-for-write 'utf-8-unix))
        (with-temp-file file
          (insert (harmless-json-text plist) "\n")))
      (setq loaded (harmless-session-load (harmless-session-directory session)))
      (should (equal (harmless-session-extra-dirs loaded) (list (car good))))
      (should (equal "UNIQUE-EXTRA\n"
                     (harmless-tools-fs--read loaded (list :path note))))
      (harmless-extra-test-error
       "Path escapes project: ../other/secret.txt"
       (lambda ()
         (harmless-tools-fs-resolve loaded "../other/secret.txt")))
      (should-not (file-exists-p (expand-file-name "notes" dir)))
      (should-not (file-exists-p (expand-file-name "notes" data)))
      (should-not (file-exists-p missing))
      (should (equal good (harmless-session-extra-dirs session)))
      (setq child (harmless-fork--create session nil))
      (should (equal (harmless-session-extra-dirs child) good))
      (should (equal (harmless-session-extra-dirs session) good))
      (should (equal "UNIQUE-EXTRA\n"
                     (harmless-tools-fs--read child (list :path note))))
      (should (file-name-absolute-p (harmless-session-directory child)))
      (should-not (harmless-filesystem-root-p
                   (harmless-session-directory child)))
      (should (string-prefix-p
               (file-name-as-directory (file-truename data))
               (file-truename (harmless-session-directory child))))
      (setq agent (harmless-agent-create
                   session '(:prompt "look") #'ignore))
      (should (equal (harmless-session-extra-dirs agent) good))
      (should (eq (harmless-session-source agent) 'agent))
      (should (equal (harmless-session-extra-dirs session) good))
      (should (file-name-absolute-p (harmless-session-directory agent)))
      (should-not (harmless-filesystem-root-p
                   (harmless-session-directory agent)))
      (should (string-prefix-p
               (file-name-as-directory (file-truename data))
               (file-truename (harmless-session-directory agent)))))))

(ert-deftest harmless-extra-command-key-and-menu ()
  (should (eq (lookup-key harmless-session-mode-map (kbd "+"))
              #'harmless-add-directory))
  (should-not (eq (lookup-key harmless-prompt-mode-map (kbd "+"))
                  #'harmless-add-directory))
  (should (equal (harmless-menu-key 'harmless-add-directory) "+"))
  (let ((calls 0))
    (cl-letf (((symbol-function 'harmless--context-session) (lambda () nil))
              ((symbol-function 'expand-file-name)
               (lambda (&rest _)
                 (setq calls (1+ calls))
                 "/"))
              ((symbol-function 'file-truename)
               (lambda (&rest _)
                 (setq calls (1+ calls))
                 "/")))
      (with-temp-buffer
        (harmless-extra-test-error
         "No Harmless session"
         (lambda () (harmless-add-directory "notes"))))
      (should (= calls 0)))))
