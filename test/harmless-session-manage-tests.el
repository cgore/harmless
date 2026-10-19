;;; harmless-session-manage-tests.el --- Rename, delete, export -*- lexical-binding: t; -*-

(require 'ert)
(require 'harmless)

(defun harmless-session-manage-test-signal (fn)
  "Return the error text from FN."
  (error-message-string (should-error (funcall fn) :type 'error)))

(defun harmless-session-manage-test-read (path)
  "Return PATH as Unix text."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (buffer-string)))

(defun harmless-session-manage-test-session (id cwd)
  "Return a saved session ID at CWD."
  (let ((session (harmless-session--create
                  :id id
                  :cwd cwd
                  :title "Draft"
                  :model "grok-4.6"
                  :status 'idle
                  :source 'user
                  :permission-mode 'ask
                  :messages (list (list :role :user :content "HSES-BODY")
                                  (list :role :assistant :content "HSES-REPLY"))
                  :created-at "2026-01-01T00:00:00Z"
                  :updated-at "2026-01-01T00:00:00Z")))
    (harmless-session-register session)
    (harmless-session-save session)
    session))

(ert-deftest harmless-session-rename-delete-and-export ()
  (let* ((base (make-temp-file "harmless-session-manage-" t))
         (root (expand-file-name "proj" base))
         (elsewhere (expand-file-name "elsewhere" base))
         (harmless-directory (expand-file-name ".harmless-state" base))
         (harmless--sessions (make-hash-table :test 'equal))
         (one (harmless-session-manage-test-session "hses-one" root))
         (two (harmless-session-manage-test-session "hses-two" root))
         (buf (get-buffer-create " *hses-one*"))
         (prompt (get-buffer-create " *hses-one-prompt*"))
         (dir (harmless-session-directory one))
         (kept (harmless-session-directory two)))
    (make-directory root t)
    (make-directory elsewhere t)
    (unwind-protect
        (let ((default-directory (file-name-as-directory elsewhere)))
          (should (eq (lookup-key harmless-session-mode-map (kbd "t"))
                      'harmless-rename))
          (should (eq (lookup-key harmless-session-mode-map (kbd "x"))
                      'harmless-export-session))
          (should (eq (lookup-key harmless-session-mode-map (kbd "X"))
                      'harmless-delete-session))
          (should (equal "Hello there"
                         (harmless-rename "  Hello\nthere  " one)))
          (should (equal "hses-one" (harmless-session-id one)))
          (should (equal dir (harmless-session-directory one)))
          (should (harmless-session-title-locked one))
          (should (string-match-p
                   "Hello there"
                   (harmless-session-manage-test-read
                    (expand-file-name "summary.json" dir))))
          (let ((long (make-string 80 ?a)))
            (should (equal long (harmless-session-rename one long)))
            (should (equal long (harmless-session-title one))))
          (harmless-session-append-user one "a different prompt")
          (should (equal (make-string 80 ?a) (harmless-session-title one)))
          (let* ((summary (expand-file-name "summary.json" dir))
                 (before (harmless-session-manage-test-read summary))
                 (dest (harmless-session-export one "out/chat.md"))
                 (text (harmless-session-manage-test-read dest)))
            (should (equal before (harmless-session-manage-test-read summary)))
            (should (file-regular-p dest))
            (should (file-name-absolute-p dest))
            (should (harmless-directory-strictly-under-p dest root))
            (should-not (equal (file-truename dest) (file-truename "/")))
            (should-not (file-exists-p
                         (expand-file-name "out/chat.md" elsewhere)))
            (should (string-match-p "HSES-BODY" text))
            (should (string-match-p "HSES-REPLY" text))
            (should (string-match-p (regexp-quote (make-string 80 ?a)) text))
            (should (string-match-p "hses-one" text))
            (should (string-match-p "## User" text))
            (should (string-match-p "## Assistant" text)))
          (let ((copy (harmless-session-export
                       one (expand-file-name "copy.md" elsewhere))))
            (should (file-regular-p copy))
            (should (file-name-absolute-p copy))
            (should-not (string-prefix-p
                         (file-name-as-directory root)
                         (file-name-as-directory copy)))
            (should (string-match-p
                     "HSES-REPLY"
                     (harmless-session-manage-test-read copy))))
          (setf (harmless-session-buffer one) buf)
          (setf (harmless-session-prompt-buffer one) prompt)
          (should (file-name-absolute-p dir))
          (should (harmless-directory-strictly-under-p dir harmless-directory))
          (should-not (equal (file-truename dir) (file-truename "/")))
          (should (equal "hses-one" (harmless-delete-session one)))
          (should-not (file-exists-p dir))
          (should-not (harmless-session-get "hses-one"))
          (should-not (buffer-live-p buf))
          (should-not (buffer-live-p prompt))
          (should (file-regular-p (expand-file-name "summary.json" kept)))
          (should (eq two (harmless-session-get "hses-two")))
          (should (string-match-p
                   "HSES-BODY"
                   (harmless-session-manage-test-read
                    (expand-file-name "messages.jsonl" kept)))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (when (buffer-live-p prompt) (kill-buffer prompt))
      (ignore-errors (delete-directory base t)))))

(ert-deftest harmless-session-manage-refuses-a-bad-request ()
  (let* ((base (make-temp-file "harmless-session-manage-bad-" t))
         (root (expand-file-name "proj" base))
         (harmless-directory (expand-file-name ".harmless-state" base))
         (harmless--sessions (make-hash-table :test 'equal))
         (session (harmless-session-manage-test-session "hses-bad" root))
         (dir (harmless-session-directory session))
         (before nil))
    (make-directory root t)
    (dolist (path '("/summary.json" "c:/summary.json" "d:/summary.json"
                    "/chat.md" "c:/chat.md" "d:/chat.md"))
      (push (cons path (file-exists-p path)) before))
    (unwind-protect
        (let ((default-directory (file-name-as-directory root)))
          (should (equal "No Harmless session"
                         (harmless-session-manage-test-signal
                          (lambda () (harmless-session-rename nil "Title")))))
          (should (equal "session title must be a string"
                         (harmless-session-manage-test-signal
                          (lambda () (harmless-session-rename session 42)))))
          (dolist (title (list nil "" "  " "\n" " \n "))
            (should (equal "session title is empty"
                           (harmless-session-manage-test-signal
                            (lambda ()
                              (harmless-session-rename session title))))))
          (should (equal "Draft" (harmless-session-title session)))
          (should (equal "No Harmless session"
                         (harmless-session-manage-test-signal
                          (lambda () (harmless-session-delete nil)))))
          (setf (harmless-session-status session) 'streaming)
          (should (equal "session is busy"
                         (harmless-session-manage-test-signal
                          (lambda () (harmless-session-delete session)))))
          (setf (harmless-session-status session) 'waiting-permission)
          (should (equal "session is busy"
                         (harmless-session-manage-test-signal
                          (lambda () (harmless-session-delete session)))))
          (should (eq 'waiting-permission (harmless-session-status session)))
          (should (file-regular-p (expand-file-name "summary.json" dir)))
          (setf (harmless-session-status session) 'idle)
          (dolist (bad (list "/" "c:/" "d:/" "c:\\" "C:/" "c:" "\\" "//" "/."
                            "notes"))
            (setf (harmless-session-directory session) bad)
            (should (equal (format "Refusing to delete a Harmless session at %s"
                                   bad)
                           (harmless-session-manage-test-signal
                            (lambda () (harmless-session-delete session))))))
          (should (eq session (harmless-session-get "hses-bad")))
          (should (file-regular-p (expand-file-name "summary.json" dir)))
          (setf (harmless-session-directory session) dir)
          (should (equal "No Harmless session"
                         (harmless-session-manage-test-signal
                          (lambda ()
                            (harmless-session-export nil "chat.md")))))
          (dolist (path (list nil "" "  "))
            (should (equal "session export path is empty"
                           (harmless-session-manage-test-signal
                            (lambda ()
                              (harmless-session-export session path))))))
          (should (equal "session export path must be a string"
                         (harmless-session-manage-test-signal
                          (lambda ()
                            (harmless-session-export session 42)))))
          (dolist (path '("/" "c:/" "d:/" "c:\\" "C:/" "c:" "\\" "//" "/."))
            (should (equal "session export path is the filesystem root"
                           (harmless-session-manage-test-signal
                            (lambda ()
                              (harmless-session-export session path))))))
          (should (equal "session export path must be absolute"
                         (harmless-session-manage-test-signal
                          (lambda ()
                            (harmless-session-export session "~/chat.md")))))
          (should (equal "session export path is not a file"
                         (harmless-session-manage-test-signal
                          (lambda ()
                            (harmless-session-export session "chat.md/")))))
          (should (equal "session export path escapes the project: .."
                         (harmless-session-manage-test-signal
                          (lambda ()
                            (harmless-session-export session "..")))))
          (should (equal "session export path is the filesystem root"
                         (harmless-session-manage-test-signal
                          (lambda ()
                            (harmless-session-export
                             session "c:/chat.md")))))
          (should (equal "session export path is missing"
                         (harmless-session-manage-test-signal
                          (lambda ()
                            (harmless-session-export
                             session
                             (expand-file-name "missing/nope.md" root))))))
          (should-not (file-directory-p (expand-file-name "missing" root)))
          (should-not (file-exists-p (expand-file-name "chat.md" root)))
          (setf (harmless-session-cwd session) "notes")
          (should (equal "session directory must be absolute"
                         (harmless-session-manage-test-signal
                          (lambda ()
                            (harmless-session-export session "chat.md")))))
          (should-not (file-exists-p (expand-file-name "chat.md" root)))
          (setf (harmless-session-cwd session) "")
          (should (equal "This chat is not attached to a project"
                         (harmless-session-manage-test-signal
                          (lambda ()
                            (harmless-session-export session "chat.md")))))
          (setf (harmless-session-cwd session) nil)
          (should (equal "This chat is not attached to a project"
                         (harmless-session-manage-test-signal
                          (lambda ()
                            (harmless-session-export session "chat.md")))))
          (dolist (cwd (list "/" "c:/" "d:/" "c:\\" "C:/" "c:" "\\" "//" "/."))
            (setf (harmless-session-cwd session) cwd)
            (should (equal "session directory is the filesystem root"
                           (harmless-session-manage-test-signal
                            (lambda ()
                              (harmless-session-export session "chat.md"))))))
          (dolist (pair before)
            (should (eq (cdr pair) (file-exists-p (car pair)))))
          (should (file-regular-p (expand-file-name "summary.json" dir)))
          (let ((loose (harmless-session--create
                        :id "hses-loose"
                        :cwd root
                        :status 'idle
                        :source 'user
                        :permission-mode 'ask)))
            (harmless-session-register loose)
            (should (equal "hses-loose" (harmless-session-delete loose)))
            (should-not (harmless-session-get "hses-loose"))))
      (ignore-errors (delete-directory base t)))))
