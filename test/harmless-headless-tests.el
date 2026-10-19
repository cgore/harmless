;;; harmless-headless-tests.el --- Tests for Harmless headless mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)
(require 'harmless-provider)

(cl-defstruct (harmless-headless-fake
               (:include harmless-provider)
               (:constructor harmless-headless-make-fake))
  script)

(cl-defmethod harmless-provider-complete ((provider harmless-headless-fake)
                                          _messages _tools callback)
  (when (harmless-headless-fake-script provider)
    (funcall (harmless-headless-fake-script provider) callback))
  nil)

(defun harmless-headless-test-signal (fn)
  "Return the error text from FN."
  (error-message-string (should-error (funcall fn) :type 'error)))

(defun harmless-headless-test-read (path)
  "Return PATH as text."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (buffer-string)))

(defun harmless-headless-test-roots ()
  "Return existence of task directories at filesystem roots."
  (list (file-exists-p "/tasks")
        (file-exists-p "c:/tasks")
        (file-exists-p "d:/tasks")
        (file-exists-p "/hhead.json")
        (file-exists-p "c:/hhead.json")
        (file-exists-p "d:/hhead.json")))

(ert-deftest harmless-headless-runs-one-prompt ()
  (let* ((base (make-temp-file "harmless-head-" t))
         (root (expand-file-name "proj" base))
         (elsewhere (expand-file-name "elsewhere" base))
         (harmless-directory (expand-file-name ".harmless-state" base))
         (harmless-compact-threshold nil)
         (harmless-default-provider-name nil)
         (harmless-default-model nil)
         (harmless-memory-observe-function nil)
         (harmless--sessions (make-hash-table :test 'equal))
         (provider (harmless-headless-make-fake
                    :name "fake" :host "none" :models '("m")))
         (harmless-providers (list provider))
         (calls 0))
    (make-directory root t)
    (make-directory elsewhere t)
    (setf (harmless-headless-fake-script provider)
          (lambda (callback)
            (setq calls (1+ calls))
            (funcall callback '(:text "HHEAD-REPLY"))
            (funcall callback '(:stop "stop"))))
    (let ((default-directory (file-name-as-directory elsewhere))
          (text (harmless-headless-run
                 (file-name-as-directory root)
                 "  HHEAD-PROMPT  ")))
      (should (equal text "HHEAD-REPLY"))
      (should (= calls 1))
      (let ((session (car (harmless-session-list))))
        (should (harmless-session-p session))
        (should (harmless-same-directory-p
                 (harmless-session-cwd session) root))
        (should (equal (plist-get (car (harmless-session-messages session))
                                  :content)
                       "HHEAD-PROMPT"))
        (should (equal (harmless-headless--reply session) "HHEAD-REPLY"))
        (let ((dir (harmless-session-directory session)))
          (should (file-name-absolute-p dir))
          (should (harmless-directory-strictly-under-p
                   dir harmless-directory))
          (should-not (equal (file-truename dir) (file-truename "/")))
          (let ((messages (expand-file-name "messages.jsonl" dir)))
            (should (file-regular-p messages))
            (should (string-search
                     "HHEAD-PROMPT"
                     (harmless-headless-test-read messages)))
            (should (string-search
                     "HHEAD-REPLY"
                     (harmless-headless-test-read messages))))))
      (should-not (file-exists-p (expand-file-name "notes" elsewhere)))
      (should-not (file-directory-p
                   (expand-file-name "tasks" harmless-directory))))
    (let ((command-line-args-left (list "--" root "hello")))
      (should (equal (harmless-headless--pop-arg) root))
      (should (equal (harmless-headless--pop-arg) "hello"))
      (should (null command-line-args-left)))))

(ert-deftest harmless-headless-task-finishes-without-the-parent ()
  (let* ((base (make-temp-file "harmless-head-task-" t))
         (root (expand-file-name "proj" base))
         (harmless-directory (expand-file-name ".harmless-state" base))
         (harmless-compact-threshold nil)
         (harmless-default-provider-name nil)
         (harmless-default-model nil)
         (harmless-memory-observe-function nil)
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-headless-launch-function nil)
         (provider (harmless-headless-make-fake
                    :name "fake" :host "none" :models '("m")))
         (harmless-providers (list provider))
         (calls 0)
         (launched nil)
         (during nil)
         (during-calls nil)
         (during-sessions nil))
    (make-directory root t)
    (setf (harmless-headless-fake-script provider)
          (lambda (callback)
            (setq calls (1+ calls))
            (funcall callback '(:text "HHEAD-REPLY"))
            (funcall callback '(:stop "stop"))))
    (setq harmless-headless-launch-function
          (lambda (id)
            (setq launched id)
            (setq during (harmless-headless-status id))
            (setq during-calls calls)
            (setq during-sessions
                  (file-directory-p
                   (expand-file-name "sessions" harmless-directory)))))
    (let ((default-directory (file-name-as-directory base))
          (id (harmless-headless-start
               (file-name-as-directory root)
               "  HHEAD-PROMPT  ")))
      (should (equal launched id))
      (should (harmless-headless--task-name-p id))
      (should (equal during-calls 0))
      (should-not during-sessions)
      (should (equal (plist-get during :status) "running"))
      (should (equal (plist-get during :prompt) "HHEAD-PROMPT"))
      (should (harmless-same-directory-p
               (plist-get during :directory) root))
      (should (null (harmless-session-list)))
      (setq harmless--sessions (make-hash-table :test 'equal))
      (should (equal (harmless-headless-finish id) "HHEAD-REPLY"))
      (should (= calls 1))
      (setq harmless--sessions (make-hash-table :test 'equal))
      (let ((task (harmless-headless-status id)))
        (should (equal (plist-get task :status) "done"))
        (should (equal (plist-get task :result) "HHEAD-REPLY"))
        (should (harmless-headless--task-name-p
                 (plist-get task :session-id)))
        (let ((path (harmless-headless--task-file id)))
          (should (file-name-absolute-p path))
          (should (file-regular-p path))
          (should (harmless-directory-strictly-under-p
                   path harmless-directory))
          (should-not (equal (file-truename path) (file-truename "/")))))
      (let ((files (directory-files-recursively
                    harmless-directory "messages\\.jsonl\\'")))
        (should (= 1 (length files)))
        (should (file-name-absolute-p (car files)))
        (should (harmless-directory-strictly-under-p
                 (car files) harmless-directory))
        (should-not (equal (file-truename (car files)) (file-truename "/")))
        (should (string-search
                 "HHEAD-PROMPT"
                 (harmless-headless-test-read (car files))))
        (should (string-search
                 "HHEAD-REPLY"
                 (harmless-headless-test-read (car files)))))
      (should (equal (harmless-headless-test-signal
                      (lambda () (harmless-headless-finish id)))
                     "headless task is not running"))
      (should (equal (plist-get (harmless-headless-status id) :result)
                     "HHEAD-REPLY"))
      (setf (harmless-headless-fake-script provider)
            (lambda (callback)
              (setq calls (1+ calls))
              (funcall callback '(:stop "stop"))))
      (let ((id2 (harmless-headless-start root "empty please")))
        (should (equal (harmless-headless-test-signal
                        (lambda () (harmless-headless-finish id2)))
                       "headless turn failed"))
        (should (equal (plist-get (harmless-headless-status id2) :status)
                       "error"))
        (should (equal (plist-get (harmless-headless-status id2) :error)
                       "headless turn failed")))
      (should (= calls 2))
      (setq harmless-headless-launch-function
            (lambda (task-id)
              (setq launched task-id)
              (error "HHEAD-LAUNCH")))
      (should (equal (harmless-headless-test-signal
                      (lambda () (harmless-headless-start root "again")))
                     "HHEAD-LAUNCH"))
      (should (equal (plist-get (harmless-headless-status launched) :status)
                     "error"))
      (should (equal (plist-get (harmless-headless-status launched) :error)
                     "HHEAD-LAUNCH"))
      (should (equal (plist-get (harmless-headless-status id) :result)
                     "HHEAD-REPLY")))))

(ert-deftest harmless-headless-spawn-is-not-a-child-process ()
  (let ((saw nil)
        (child nil)
        (id "hhead-launch"))
    (if (eq system-type 'windows-nt)
        (cl-letf (((symbol-function 'w32-shell-execute)
                   (lambda (op doc params show)
                     (setq saw (list op doc params show))))
                  ((symbol-function 'start-process)
                   (lambda (&rest _) (setq child t) nil))
                  ((symbol-function 'make-process)
                   (lambda (&rest _) (setq child t) nil))
                  ((symbol-function 'call-process)
                   (lambda (&rest _) (setq child t) 0)))
          (harmless-headless--spawn
           (list "C:/prog" "-Q" "x y"))
          (should (equal saw (list "open" "C:/prog" "-Q \"x y\"" 7)))
          (should-not child)
          (setq saw nil)
          (should (equal (harmless-headless-launch id) id))
          (should (equal (car saw) "open"))
          (should (equal (nth 3 saw) 7))
          (should (string-search "--batch" (nth 2 saw)))
          (should (string-search "-Q" (nth 2 saw)))
          (should (string-search "harmless-headless-work" (nth 2 saw)))
          (should (string-search id (nth 2 saw)))
          (should-not (string-search "HHEAD" (nth 2 saw)))
          (should-not child))
      (cl-letf (((symbol-function 'call-process)
                 (lambda (program &rest args)
                   (setq saw (cons program args))
                   0))
                ((symbol-function 'start-process)
                 (lambda (&rest _) (setq child t) nil)))
        (should (equal (harmless-headless-launch id) id))
        (should (equal (car saw) "sh"))
        (should (member "-c" (cdr saw)))
        (should (string-search "nohup" (car (last saw))))
        (should (string-search id (car (last saw))))
        (should-not child)))))

(ert-deftest harmless-headless-spawn-runs-after-the-caller-returns ()
  (let* ((dir (make-temp-file "harmless-head-spawn-" t))
         (marker (expand-file-name "marker.txt" dir))
         (emacs (expand-file-name invocation-name invocation-directory))
         (form (format "(write-region \"HHEAD-SPAWN\\n\" nil %S nil :silent)"
                       marker))
         (left 40))
    (should-not (file-exists-p marker))
    (harmless-headless--spawn (list emacs "-Q" "--batch" "--eval" form))
    (while (and (> left 0) (not (file-exists-p marker)))
      (sleep-for 0.25)
      (setq left (1- left)))
    (should (file-regular-p marker))
    (should (file-name-absolute-p marker))
    (should (string-search "HHEAD-SPAWN"
                           (harmless-headless-test-read marker)))))

(ert-deftest harmless-headless-refuses-a-bad-request ()
  (let* ((base (make-temp-file "harmless-head-bad-" t))
         (root (expand-file-name "proj" base))
         (harmless-directory (expand-file-name ".harmless-state" base))
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-headless-launch-function nil)
         (launched nil)
         (roots (harmless-headless-test-roots))
         (file (make-temp-file "harmless-head-file-")))
    (make-directory root t)
    (setq harmless-headless-launch-function
          (lambda (id) (setq launched id)))
    (unwind-protect
        (progn
          (dolist (prompt (list nil "" "   " "\n"))
            (should (equal (harmless-headless-test-signal
                            (lambda ()
                              (harmless-headless-run root prompt)))
                           "headless prompt is empty")))
          (should (equal (harmless-headless-test-signal
                          (lambda () (harmless-headless-run root 42)))
                         "headless prompt must be a string"))
          (dolist (directory (list nil "" "   " "\n"))
            (should (equal (harmless-headless-test-signal
                            (lambda ()
                              (harmless-headless-run directory "hello")))
                           "headless directory is empty")))
          (should (equal (harmless-headless-test-signal
                          (lambda () (harmless-headless-run 42 "hello")))
                         "headless directory must be a string"))
          (dolist (directory (list "/" "c:/" "d:/" "c:\\" "C:/" "c:"
                                   "\\" "//" "/." " / " " c:/ "))
            (should (equal (harmless-headless-test-signal
                            (lambda ()
                              (harmless-headless-run directory "hello")))
                           "headless directory is the filesystem root")))
          (let ((default-directory (file-name-as-directory root)))
            (dolist (directory (list "notes" "notes/" "sub/" "~" "~/notes"
                                     "  notes  "))
              (should (equal (harmless-headless-test-signal
                              (lambda ()
                                (harmless-headless-run directory "hello")))
                             "headless directory must be absolute")))
            (should-not (file-exists-p (expand-file-name "notes" root))))
          (should (equal (harmless-headless-test-signal
                          (lambda ()
                            (harmless-headless-run
                             (expand-file-name "missing-hhead" base)
                             "hello")))
                         "headless directory is missing"))
          (should (equal (harmless-headless-test-signal
                          (lambda ()
                            (harmless-headless-run
                             (file-name-as-directory
                              (expand-file-name "missing-hhead" base))
                             "hello")))
                         "headless directory is missing"))
          (should (equal (harmless-headless-test-signal
                          (lambda ()
                            (harmless-headless-run file "hello")))
                         "headless directory is not a directory"))
          (dolist (id (list nil "" "   "))
            (should (equal (harmless-headless-test-signal
                            (lambda () (harmless-headless-status id)))
                           "headless task is empty")))
          (should (equal (harmless-headless-test-signal
                          (lambda () (harmless-headless-status 42)))
                         "headless task must be a string"))
          (dolist (id (list "/" "c:/" "c:" "~" "a/b" "a\\b" ".." "."
                            "id one" "  ..  "))
            (should (equal (harmless-headless-test-signal
                            (lambda () (harmless-headless-status id)))
                           "headless task is not a name")))
          (should (equal (harmless-headless-test-signal
                          (lambda ()
                            (harmless-headless-status "hhead-missing")))
                         "headless task is missing"))
          (should-not launched)
          (should-not (file-directory-p
                       (expand-file-name "tasks" harmless-directory)))
          (dolist (raw (list "/" "c:/" "d:/" "c:\\" "C:/" "c:" "\\" "//" "/."))
            (let ((harmless-directory raw))
              (should (equal (harmless-headless-test-signal
                              (lambda ()
                                (harmless-headless-run root "hello")))
                             (format "Refusing to store a headless task at %s"
                                     (string-trim raw))))))
          (should-not launched)
          (let ((id nil))
            (setq harmless-headless-launch-function
                  (lambda (task-id) (setq id task-id)))
            (setq id (harmless-headless-start root "keep"))
            (delete-directory root t)
            (should (equal (harmless-headless-test-signal
                            (lambda () (harmless-headless-finish id)))
                           "headless directory is missing"))
            (should (equal (plist-get (harmless-headless-status id) :status)
                           "error"))
            (should (equal (plist-get (harmless-headless-status id) :error)
                           "headless directory is missing")))
          (let* ((bad (expand-file-name
                       "tasks/hhead-bad.json" harmless-directory))
                 (done (expand-file-name
                        "tasks/hhead-done.json" harmless-directory))
                 (other (expand-file-name
                         "tasks/hhead-other.json" harmless-directory)))
            (let ((coding-system-for-write 'utf-8-unix))
              (with-temp-file bad (insert "not-json"))
              (with-temp-file done
                (insert (harmless-json-text
                         '(:id "hhead-done" :status "done" :prompt "p"
                               :directory "D:/proj" :result "kept"
                               :error "" :session-id ""
                               :created-at "t" :updated-at "t"))))
              (with-temp-file other
                (insert (harmless-json-text
                         '(:id "hhead-nope" :status "running" :prompt "p"
                               :directory "D:/proj")))))
            (let ((before-bad (harmless-headless-test-read bad))
                  (before-done (harmless-headless-test-read done))
                  (before-other (harmless-headless-test-read other)))
              (should (equal (harmless-headless-test-signal
                              (lambda ()
                                (harmless-headless-finish "hhead-bad")))
                             "headless task is not valid"))
              (should (equal (harmless-headless-test-signal
                              (lambda ()
                                (harmless-headless-finish "hhead-done")))
                             "headless task is not running"))
              (should (equal (harmless-headless-test-signal
                              (lambda ()
                                (harmless-headless-finish "hhead-other")))
                             "headless task is not valid"))
              (should (equal (harmless-headless-test-read bad) before-bad))
              (should (equal (harmless-headless-test-read done) before-done))
              (should (equal (harmless-headless-test-read other)
                             before-other))))
          (should (equal roots (harmless-headless-test-roots))))
      (when (file-exists-p file)
        (delete-file file)))))
