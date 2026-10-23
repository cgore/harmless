;;; harmless-read-buffer-tests.el --- Tests for reading one buffer -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless-openai)
(require 'harmless-read-buffer)
(require 'harmless-session)
(require 'harmless-tools)
(require 'harmless-ui)

(defmacro harmless-read-buffer-test-signal (form)
  "Return the error message signaled by FORM."
  `(error-message-string (should-error ,form :type 'error)))

(defun harmless-read-buffer-test-session (dir)
  "Return a session rooted at DIR."
  (let ((provider (harmless-make-openai-compat
                   "local" :host "127.0.0.1:9" :protocol "http"
                   :key "none" :models '("m"))))
    (setq harmless-providers (list provider))
    (harmless-session-new :cwd dir :provider provider :model "m")))

(defun harmless-read-buffer-test-cleanup (dirs)
  "Kill buffers under DIRS and delete those directories."
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf) (not (minibufferp buf)))
      (let ((def (buffer-local-value 'default-directory buf))
            (proc (get-buffer-process buf)))
        (when (and (stringp def)
                   (cl-some (lambda (dir)
                              (and (stringp dir)
                                   (file-in-directory-p def dir)))
                            dirs))
          (when (processp proc)
            (delete-process proc))
          (with-current-buffer buf
            (setq buffer-read-only nil)
            (let ((inhibit-read-only t)
                  (kill-buffer-query-functions nil)
                  (kill-buffer-hook nil))
              (set-buffer-modified-p nil)
              (ignore-errors (kill-buffer buf))))))))
  (dolist (dir dirs)
    (when (and (stringp dir) (file-directory-p dir))
      (ignore-errors (delete-directory dir t)))))

(defun harmless-read-buffer-test-refused (session name message)
  "Assert NAME is refused with MESSAGE, including the tool wrapper."
  (should (equal message
                 (harmless-read-buffer-test-signal
                  (harmless-read-buffer session name))))
  (should (equal (concat "Error: " message)
                 (harmless-tool-call
                  (harmless-tool-by-name "harmless-tool-read-buffer")
                  session (list :name name)))))

(defun harmless-read-buffer-test-fill (name text)
  "Create a buffer named NAME containing TEXT and return it."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)))
    buf))

(ert-deftest harmless-read-buffer-returns-accessible-text ()
  (let* ((dir (make-temp-file "harmless-rbuf-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-providers nil)
         (project-find-functions nil)
         (buf nil)
         (session nil)
         (tool nil))
    (unwind-protect
        (progn
          (setq session (harmless-read-buffer-test-session dir))
          (setq buf (harmless-read-buffer-test-fill
                     "*harmless-read-visible*" "abcdef"))
          (with-current-buffer buf
            (narrow-to-region 3 5))
          (setq tool (harmless-tool-by-name "harmless-tool-read-buffer"))
          (should (eq 'read (harmless-tool-class tool)))
          (should (equal "cd" (harmless-read-buffer session (buffer-name buf))))
          (should (equal "cd" (harmless-tool-call
                               tool session
                               (list :name (buffer-name buf)))))
          (with-current-buffer buf
            (should (= (point-min) 3))
            (should (= (point-max) 5))
            (widen)
            (should (equal "abcdef" (buffer-string))))
          (with-current-buffer buf
            (erase-buffer))
          (should (equal "" (harmless-read-buffer session (buffer-name buf))))
          (with-current-buffer buf
            (insert "\n"))
          (should (equal "\n" (harmless-read-buffer session (buffer-name buf)))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (harmless-read-buffer-test-cleanup (list dir)))))

(ert-deftest harmless-read-buffer-cuts-at-the-file-cap ()
  (let* ((dir (make-temp-file "harmless-rbuf-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-providers nil)
         (project-find-functions nil)
         (text (make-string (1+ harmless-read-file-max-bytes) ?a))
         (buf nil)
         (session nil)
         (result nil))
    (unwind-protect
        (progn
          (setq session (harmless-read-buffer-test-session dir))
          (setq buf (harmless-read-buffer-test-fill "*harmless-read-long*" text))
          (setq result (harmless-read-buffer session (buffer-name buf)))
          (should (string-suffix-p "\n[truncated]" result))
          (should (= (length result)
                     (+ harmless-read-file-max-bytes
                        (length "\n[truncated]"))))
          (should (equal text (with-current-buffer buf (buffer-string)))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (harmless-read-buffer-test-cleanup (list dir)))))

(ert-deftest harmless-read-buffer-refuses-a-bad-name ()
  (let* ((dir (make-temp-file "harmless-rbuf-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-providers nil)
         (project-find-functions nil)
         (session nil))
    (unwind-protect
        (progn
          (setq session (harmless-read-buffer-test-session dir))
          (dolist (name (list nil "" "   " "\n" "\t "))
            (harmless-read-buffer-test-refused
             session name "buffer name is empty"))
          (dolist (name (list 12 t 'foo '(a)))
            (harmless-read-buffer-test-refused
             session name "buffer name must be a string"))
          (harmless-read-buffer-test-refused
           session "no-such-harmless-buffer" "buffer is not live")
          (harmless-read-buffer-test-refused
           session "missing/" "buffer is not live")
          (let ((buf (harmless-read-buffer-test-fill
                      "*harmless-read-dead*" "gone")))
            (kill-buffer buf)
            (harmless-read-buffer-test-refused
             session "*harmless-read-dead*" "buffer is not live")))
      (harmless-read-buffer-test-cleanup (list dir)))))

(ert-deftest harmless-read-buffer-reads-a-buffer-named-like-a-path ()
  (let* ((dir (make-temp-file "harmless-rbuf-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-providers nil)
         (project-find-functions nil)
         (names '("/" "c:/" "c:\\" "sub/" "a.txt\\"))
         (saved nil)
         (session nil))
    (unwind-protect
        (progn
          (setq session (harmless-read-buffer-test-session dir))
          (dolist (name names)
            (let ((old (get-buffer name)))
              (when old
                (push (cons name
                            (with-current-buffer old
                              (buffer-substring-no-properties
                               (point-min) (point-max))))
                      saved)))
            (harmless-read-buffer-test-fill name "root-ok")
            (should (equal "root-ok" (harmless-read-buffer session name)))
            (should (equal "root-ok"
                           (harmless-tool-call
                            (harmless-tool-by-name "harmless-tool-read-buffer")
                            session (list :name name))))))
      (dolist (name names)
        (let ((buf (get-buffer name))
              (old (cdr (assoc name saved))))
          (when (buffer-live-p buf)
            (if (assoc name saved)
                (with-current-buffer buf
                  (let ((inhibit-read-only t))
                    (erase-buffer)
                    (insert (or old ""))))
              (kill-buffer buf)))))
      (harmless-read-buffer-test-cleanup (list dir)))))

(ert-deftest harmless-read-buffer-refuses-hidden-and-process-buffers ()
  (let* ((dir (make-temp-file "harmless-rbuf-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-providers nil)
         (project-find-functions nil)
         (hidden nil)
         (hproc nil)
         (uproc nil)
         (session nil))
    (unwind-protect
        (progn
          (setq session (harmless-read-buffer-test-session dir))
          (setq hidden (harmless-read-buffer-test-fill
                        " *harmless-read-secret*" "SECRET"))
          (harmless-read-buffer-test-refused
           session (buffer-name hidden) "buffer is not readable")
          (harmless-read-buffer-test-refused
           session (buffer-name (window-buffer (minibuffer-window)))
           "buffer is not readable")
          (setq hproc (harmless-read-buffer-test-fill
                       "*harmless-pipe-out*" "SECRET-PROC"))
          (make-pipe-process :name "harmless-pipe" :buffer hproc :noquery t)
          (harmless-read-buffer-test-refused
           session (buffer-name hproc) "buffer is not readable")
          (setq uproc (harmless-read-buffer-test-fill "*user-pipe*" "seen"))
          (make-pipe-process :name "user-pipe" :buffer uproc :noquery t)
          (should (equal "seen" (harmless-read-buffer
                                 session (buffer-name uproc)))))
      (dolist (buf (list hidden hproc uproc))
        (when (buffer-live-p buf)
          (when (processp (get-buffer-process buf))
            (delete-process (get-buffer-process buf)))
          (kill-buffer buf)))
      (harmless-read-buffer-test-cleanup (list dir)))))

(ert-deftest harmless-read-buffer-refuses-another-session ()
  (let* ((dir (make-temp-file "harmless-rbuf-a-" t))
         (other-dir (make-temp-file "harmless-rbuf-b-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-providers nil)
         (project-find-functions nil)
         (session nil)
         (other nil))
    (unwind-protect
        (progn
          (setq session (harmless-read-buffer-test-session dir))
          (setq other (harmless-read-buffer-test-session other-dir))
          (let ((mine (harmless-ui-ensure-session-buffer session))
                (theirs (harmless-ui-ensure-session-buffer other))
                (their-prompt (harmless-ui-ensure-prompt-buffer other)))
            (with-current-buffer mine
              (let ((inhibit-read-only t))
                (insert "SECRET-THIS")))
            (with-current-buffer theirs
              (let ((inhibit-read-only t))
                (insert "SECRET-OTHER")))
            (with-current-buffer their-prompt
              (insert "SECRET-PROMPT"))
            (should (string-match-p
                     "SECRET-THIS"
                     (harmless-read-buffer session (buffer-name mine))))
            (harmless-read-buffer-test-refused
             session (buffer-name theirs) "buffer is another session")
            (harmless-read-buffer-test-refused
             session (buffer-name their-prompt) "buffer is another session")
            (with-current-buffer theirs
              (should (string-match-p "SECRET-OTHER" (buffer-string))))))
      (harmless-read-buffer-test-cleanup (list dir other-dir)))))

(provide 'harmless-read-buffer-tests)

;;; harmless-read-buffer-tests.el ends here
