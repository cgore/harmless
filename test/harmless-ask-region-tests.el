;;; harmless-ask-region-tests.el --- Tests for asking from a file buffer -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)
(require 'harmless-ask-region)
(require 'harmless-openai)
(require 'harmless-perm)
(require 'harmless-session)
(require 'harmless-ui)

(cl-defstruct (harmless-region-fake
               (:include harmless-provider)
               (:constructor harmless-region-make-fake))
  script)

(cl-defmethod harmless-provider-complete ((provider harmless-region-fake)
                                          messages tools callback)
  (ignore messages tools)
  (funcall (harmless-region-fake-script provider) messages callback)
  nil)

(defmacro harmless-region-test-signal (form)
  "Return the error message signaled by FORM."
  `(error-message-string (should-error ,form :type 'error)))

(defun harmless-region-test-bytes (path)
  "Return the raw bytes of PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun harmless-region-test-user (session)
  "Return the first user message in SESSION."
  (plist-get
   (car (seq-filter #'harmless-message-user-p
                    (harmless-session-messages session)))
   :content))

(defun harmless-region-test-cleanup (dir)
  "Kill buffers under DIR and delete DIR."
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf) (not (minibufferp buf)))
      (let ((file (buffer-file-name buf))
            (def (buffer-local-value 'default-directory buf)))
        (when (or (and (stringp file) (file-in-directory-p file dir))
                  (and (stringp def) (file-in-directory-p def dir)))
          (with-current-buffer buf
            (setq buffer-read-only nil)
            (let ((inhibit-read-only t)
                  (kill-buffer-query-functions nil)
                  (kill-buffer-hook nil))
              (set-buffer-modified-p nil)
              (ignore-errors (set-visited-file-name nil t))
              (ignore-errors (kill-buffer buf))))))))
  (when (> (count-windows) 1)
    (delete-other-windows))
  (when (and (stringp dir) (file-directory-p dir))
    (ignore-errors (delete-directory dir t))))

(defmacro harmless-region-test-with (text script &rest body)
  "Visit a temp file TEXT and run BODY with a fake provider.
SCRIPT is the provider script.  BODY sees `dir', `buf', `path',
`bytes', and `provider'.  State is written under `dir', not at /."
  (declare (indent 2))
  `(let* ((dir (make-temp-file "harmless-ask-" t))
          (path (expand-file-name "a.el" dir))
          (buf nil)
          (provider (harmless-region-make-fake
                     :name "fake" :host "none" :script ,script))
          (harmless-directory (expand-file-name ".harmless" dir))
          (harmless--sessions (make-hash-table :test 'equal))
          (harmless--config-loaded t)
          (harmless-providers (list provider))
          (project-find-functions nil)
          (bytes nil))
     (clrhash harmless-ask--pending)
     (unwind-protect
         (progn
           (let ((coding-system-for-write 'utf-8-unix))
             (write-region ,text nil path nil 'silent))
           (setq buf (find-file-noselect path))
           (with-current-buffer buf
             (setq-local require-final-newline nil)
             (set-buffer-modified-p nil))
           (setq bytes (harmless-region-test-bytes path))
           ,@body)
       (harmless-region-test-cleanup dir))))

(defun harmless-region-test-select (buf)
  "Activate the OLD region in BUF.  BUF's text is xxOLDyy."
  (with-current-buffer buf
    (should (equal "xxOLDyy" (buffer-string)))
    (transient-mark-mode 1)
    (goto-char 3)
    (push-mark 6 t t)
    (should (region-active-p))
    (should (equal "OLD" (buffer-substring-no-properties
                          (region-beginning) (region-end))))))

(ert-deftest harmless-ask-region-mode-is-off-and-keeps-c-c-c-c ()
  (should (commandp 'harmless-ask-region))
  (should (commandp 'harmless-ask-mode))
  (should-not (lookup-key harmless-ask-mode-map (kbd "C-c C-c")))
  (should (eq 'harmless-ask-region
              (lookup-key harmless-ask-mode-map (kbd "C-c . a"))))
  (with-temp-buffer
    (should-not harmless-ask-mode)
    (local-set-key (kbd "C-c C-c") #'ignore)
    (harmless-ask-mode 1)
    (should harmless-ask-mode)
    (should (eq 'ignore (key-binding (kbd "C-c C-c"))))
    (should (eq 'harmless-ask-region (key-binding (kbd "C-c . a"))))
    (let ((other (generate-new-buffer " harmless-ask-other")))
      (unwind-protect
          (with-current-buffer other
            (should-not harmless-ask-mode)
            (should-not (eq 'harmless-ask-region
                            (key-binding (kbd "C-c . a")))))
        (kill-buffer other)))))

(ert-deftest harmless-ask-region-sends-the-region ()
  (harmless-region-test-with
      "xxOLDyy"
      (lambda (_messages callback)
        (funcall callback '(:text "NEW"))
        (funcall callback '(:stop "stop")))
    (let ((session (harmless-session-new
                    :cwd dir :provider provider :model "m")))
      (harmless-region-test-select buf)
      (with-current-buffer buf
        (should-not harmless-ask-mode)
        (should (eq session
                    (car (harmless-session-for-cwd (harmless-current-cwd)))))
        (harmless-ask-region)
        (should (equal "xxOLDyy" (buffer-string)))
        (should-not (buffer-modified-p)))
      (should (equal "OLD" (harmless-region-test-user session)))
      (should-not (string-match-p "Attached Context"
                                  (harmless-region-test-user session)))
      (should (equal bytes (harmless-region-test-bytes path)))
      (should (= 1 (length (harmless-session-for-cwd
                            (harmless-session-cwd session))))))))

(ert-deftest harmless-ask-region-sends-the-defun ()
  (harmless-region-test-with
      "(defun harmless-region-test-a ()\n  1)\n\n(defun harmless-region-test-b ()\n  2)\n"
      (lambda (_messages callback)
        (funcall callback '(:text "NEW"))
        (funcall callback '(:stop "stop")))
    (let ((session (harmless-session-new
                    :cwd dir :provider provider :model "m")))
      (with-current-buffer buf
        (emacs-lisp-mode)
        (search-forward "2")
        (deactivate-mark)
        (should-not (region-active-p))
        (harmless-ask-region))
      (let ((sent (harmless-region-test-user session)))
        (should (string-match-p "harmless-region-test-b" sent))
        (should-not (string-match-p "harmless-region-test-a" sent))
        (should-not (string-match-p "Attached Context" sent)))
      (with-current-buffer buf
        (should-not (buffer-modified-p))))))

(ert-deftest harmless-ask-region-refuses-an-empty-region ()
  (harmless-region-test-with
      "(defun foo ()\n  1)\n"
      (lambda (_messages callback)
        (funcall callback '(:text "NEW"))
        (funcall callback '(:stop "stop")))
    (let ((called nil))
      (cl-letf (((symbol-function 'harmless-turn-run)
                 (lambda (&rest _)
                   (setq called t)
                   (error "ran a turn"))))
        (with-current-buffer buf
          (emacs-lisp-mode)
          (transient-mark-mode 1)
          (goto-char (point-min))
          (push-mark (point) t t)
          (should (region-active-p))
          (should (equal "region is empty"
                         (harmless-region-test-signal (harmless-ask-region))))
          (should-not called)
          (erase-buffer)
          (insert "hello")
          (deactivate-mark)
          (should-not (region-active-p))
          (should (equal "region is empty"
                         (harmless-region-test-signal (harmless-ask-region))))
          (should-not called)
          (should (equal "hello" (buffer-string))))))))

(ert-deftest harmless-ask-region-refuses-a-read-only-buffer ()
  (harmless-region-test-with
      "xxOLDyy"
      (lambda (_messages callback)
        (funcall callback '(:text "NEW"))
        (funcall callback '(:stop "stop")))
    (let ((called nil))
      (cl-letf (((symbol-function 'harmless-turn-run)
                 (lambda (&rest _)
                   (setq called t)
                   (error "ran a turn"))))
        (harmless-region-test-select buf)
        (with-current-buffer buf
          (setq buffer-read-only t)
          (should (equal "buffer is read-only"
                         (harmless-region-test-signal (harmless-ask-region))))
          (should-not called)
          (should (equal "xxOLDyy" (buffer-string))))
        (should (equal bytes (harmless-region-test-bytes path)))))))

(ert-deftest harmless-ask-region-refuses-a-buffer-that-is-not-a-file ()
  (harmless-region-test-with
      "xxOLDyy"
      (lambda (_messages callback)
        (funcall callback '(:text "NEW"))
        (funcall callback '(:stop "stop")))
    (let ((called nil)
          (hidden (get-buffer-create " *harmless-ask-hidden*"))
          (notes (get-buffer-create "*harmless-ask-notes*"))
          (session (harmless-session-new
                    :cwd dir :provider provider :model "m")))
      (unwind-protect
          (cl-letf (((symbol-function 'harmless-turn-run)
                     (lambda (&rest _)
                       (setq called t)
                       (error "ran a turn"))))
            (dolist (other (list (harmless-ui-ensure-session-buffer session)
                                 (harmless-ui-ensure-prompt-buffer session)
                                 (window-buffer (minibuffer-window))
                                 hidden
                                 notes))
              (with-current-buffer other
                (when (derived-mode-p 'harmless-prompt-mode)
                  (harmless-ask-mode 1)
                  (should (eq 'harmless-prompt-send
                              (key-binding (kbd "C-c C-c")))))
                (should (equal "buffer is not a file"
                               (harmless-region-test-signal
                                (harmless-ask-region))))
                (should-not called))))
        (dolist (other (list hidden notes))
          (when (buffer-live-p other)
            (kill-buffer other)))))))

(ert-deftest harmless-ask-region-reuses-the-live-session ()
  (harmless-region-test-with
      "xxOLDyy"
      (lambda (_messages callback)
        (funcall callback '(:text "NEW"))
        (funcall callback '(:stop "stop")))
    (let ((session (harmless-session-new
                    :cwd dir :provider provider :model "m")))
      (cl-letf (((symbol-function 'harmless-current)
                 (lambda (&rest _) (error "started a session"))))
        (harmless-region-test-select buf)
        (with-current-buffer buf
          (harmless-ask-region)))
      (should (eq session (car (harmless-session-for-cwd
                                (harmless-session-cwd session)))))
      (should (= 1 (length (harmless-session-for-cwd
                            (harmless-session-cwd session)))))
      (should (equal "OLD" (harmless-region-test-user session))))))

(ert-deftest harmless-ask-region-opens-one-session ()
  (harmless-region-test-with
      "xxOLDyy"
      (lambda (_messages callback)
        (funcall callback '(:text "NEW"))
        (funcall callback '(:stop "stop")))
    (harmless-region-test-select buf)
    (with-current-buffer buf
      (should (null (harmless-session-for-cwd (harmless-current-cwd))))
      (harmless-ask-region)
      (let ((found (harmless-session-for-cwd (harmless-current-cwd))))
        (should (= 1 (length found)))
        (should (equal "OLD" (harmless-region-test-user (car found))))
        (should (file-name-absolute-p (harmless-session-cwd (car found))))
        (should-not (harmless-filesystem-root-p
                     (harmless-session-cwd (car found))))))))

(ert-deftest harmless-ask-region-replaces-with-one-undo ()
  (harmless-region-test-with
      "xxOLDyy"
      (lambda (_messages callback)
        (funcall callback '(:text "NEW"))
        (funcall callback '(:stop "stop")))
    (let ((session (harmless-session-new
                    :cwd dir :provider provider :model "m"))
          (saved nil))
      (harmless-region-test-select buf)
      (with-current-buffer buf
        (cl-letf (((symbol-function 'save-buffer)
                   (lambda (&rest _)
                     (setq saved t)
                     (error "saved the buffer"))))
          (harmless-ask-region '(4)))
        (should (equal "xxNEWyy" (buffer-string)))
        (should (buffer-modified-p))
        (setq buffer-undo-list (primitive-undo 1 buffer-undo-list))
        (should (equal "xxOLDyy" (buffer-string))))
      (should-not saved)
      (should (equal bytes (harmless-region-test-bytes path)))
      (should (equal "OLD" (harmless-region-test-user session))))))

(ert-deftest harmless-ask-region-leaves-the-region-on-an-empty-reply ()
  (let ((calls 0))
    (harmless-region-test-with
        "xxOLDyy"
        (lambda (_messages callback)
          (setq calls (1+ calls))
          (if (= calls 1)
              (progn
                (funcall callback '(:text ""))
                (funcall callback '(:stop "stop")))
            (funcall callback '(:text "NEW"))
            (funcall callback '(:stop "stop"))))
      (let ((session (harmless-session-new
                      :cwd dir :provider provider :model "m")))
        (harmless-region-test-select buf)
        (with-current-buffer buf
          (harmless-ask-region '(4))
          (should (equal "xxOLDyy" (buffer-string)))
          (should-not (buffer-modified-p)))
        (should (equal bytes (harmless-region-test-bytes path)))
        (should (= 0 (hash-table-count harmless-ask--pending)))
        (harmless-turn-run session "later")
        (with-current-buffer buf
          (should (equal "xxOLDyy" (buffer-string)))
          (should-not (buffer-modified-p)))))))

(ert-deftest harmless-ask-region-leaves-the-region-on-blank-text ()
  (harmless-region-test-with
      "xxOLDyy"
      (lambda (_messages callback)
        (funcall callback '(:reasoning "thinking"))
        (funcall callback '(:text " \n"))
        (funcall callback '(:stop "stop")))
    (let ((session (harmless-session-new
                    :cwd dir :provider provider :model "m")))
      (harmless-region-test-select buf)
      (with-current-buffer buf
        (harmless-ask-region '(4))
        (should (equal "xxOLDyy" (buffer-string)))
        (should-not (buffer-modified-p)))
      (should (eq 'idle (harmless-session-status session)))
      (should (equal bytes (harmless-region-test-bytes path))))))

(ert-deftest harmless-ask-region-a-later-turn-drops-the-replacement ()
  (harmless-region-test-with
      "xxOLDyy"
      (lambda (_messages callback)
        (funcall callback '(:text "NEW"))
        (funcall callback '(:stop "stop")))
    (let ((session (harmless-session-new
                    :cwd dir :provider provider :model "m")))
      (with-current-buffer buf
        (puthash (harmless-session-id session)
                 (list buf (copy-marker 3) (copy-marker 6 t))
                 harmless-ask--pending))
      (harmless-turn-run session "later")
      (with-current-buffer buf
        (should (equal "xxOLDyy" (buffer-string))))
      (should (= 0 (hash-table-count harmless-ask--pending)))
      (should (equal bytes (harmless-region-test-bytes path))))))

(ert-deftest harmless-ask-region-replaces-with-the-final-answer ()
  (let ((calls 0))
    (harmless-region-test-with
        "xxOLDyy"
        (lambda (_messages callback)
          (setq calls (1+ calls))
          (if (= calls 1)
              (progn
                (funcall callback
                         '(:tool-call "1" "harmless-tool-write-file"
                           "{\"path\":\"nope.txt\",\"contents\":\"x\"}"))
                (funcall callback '(:stop "tool_calls")))
            (funcall callback '(:text "NEW"))
            (funcall callback '(:stop "stop"))))
      (let ((harmless-perm-ask-function
             (lambda (_s _c _i cb) (funcall cb 'deny)))
            (session (harmless-session-new
                      :cwd dir :provider provider :model "m")))
        (harmless-region-test-select buf)
        (with-current-buffer buf
          (harmless-ask-region '(4))
          (should (equal "xxNEWyy" (buffer-string))))
        (should (eq 'idle (harmless-session-status session)))
        (should-not (file-exists-p (expand-file-name "nope.txt" dir)))
        (should (equal bytes (harmless-region-test-bytes path)))))))

(ert-deftest harmless-ask-region-refuses-when-the-buffer-becomes-read-only ()
  (harmless-region-test-with
      "xxOLDyy"
      (lambda (_messages callback)
        (with-current-buffer buf
          (setq buffer-read-only t))
        (funcall callback '(:text "NEW"))
        (funcall callback '(:stop "stop")))
    (let ((session (harmless-session-new
                    :cwd dir :provider provider :model "m")))
      (harmless-region-test-select buf)
      (with-current-buffer buf
        (should (equal "buffer is read-only"
                       (harmless-region-test-signal
                        (harmless-ask-region '(4)))))
        (should (equal "xxOLDyy" (buffer-string))))
      (should (equal bytes (harmless-region-test-bytes path)))
      (should (eq 'idle (harmless-session-status session))))))

(ert-deftest harmless-ask-region-drops-the-replacement-on-error ()
  (harmless-region-test-with
      "xxOLDyy"
      (lambda (_messages callback)
        (funcall callback '(:error "nope")))
    (let ((session (harmless-session-new
                    :cwd dir :provider provider :model "m")))
      (harmless-region-test-select buf)
      (with-current-buffer buf
        (harmless-ask-region '(4))
        (should (equal "xxOLDyy" (buffer-string))))
      (should (eq 'error (harmless-session-status session)))
      (should (= 0 (hash-table-count harmless-ask--pending)))
      (should (equal bytes (harmless-region-test-bytes path))))))

(provide 'harmless-ask-region-tests)

;;; harmless-ask-region-tests.el ends here
