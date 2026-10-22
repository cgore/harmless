;;; harmless-editor-tests.el --- Tests for editor integration -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'dired)
(require 'harmless-openai)
(require 'harmless-provider)
(require 'harmless-session)
(require 'harmless-tools-fs)
(require 'harmless-checkpoint)
(require 'harmless-editor)
(require 'harmless-ui)
(require 'vc)

(cl-defstruct (harmless-editor-fake
               (:include harmless-provider)
               (:constructor harmless-editor-make-fake)))

(cl-defmethod harmless-provider-complete
    ((_provider harmless-editor-fake) _messages _tools _callback)
  nil)

(defvar harmless--session)

(defun harmless-editor-test-session (dir)
  "Return a session rooted at DIR."
  (let ((provider (harmless-make-openai-compat
                   "local" :host "127.0.0.1:9" :protocol "http"
                   :key "none" :models '("m")))
        (harmless-providers nil)
        (harmless--sessions (make-hash-table :test 'equal))
        (harmless-directory (expand-file-name ".harmless" dir)))
    (setq harmless-providers (list provider))
    (harmless-session-new :cwd dir :provider provider :model "m")))

(defun harmless-editor-test-file (dir name text)
  "Write TEXT to NAME in DIR with Unix newlines and return the path."
  (let ((path (expand-file-name name dir))
        (coding-system-for-write 'utf-8-unix))
    (write-region text nil path nil 'silent)
    path))

(defun harmless-editor-test-visit (path coding)
  "Visit PATH in fundamental mode with CODING."
  (let ((buf (find-file-noselect path)))
    (with-current-buffer buf
      (fundamental-mode)
      (setq buffer-file-coding-system coding)
      (setq-local require-final-newline nil))
    buf))

(defun harmless-editor-test-text (buf)
  "Return BUF's full text."
  (with-current-buffer buf
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun harmless-editor-test-bytes (path)
  "Return the raw bytes of PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun harmless-editor-test-undo (buf)
  "Undo one amalgamated edit in BUF."
  (with-current-buffer buf
    (save-restriction
      (widen)
      (setq buffer-undo-list (primitive-undo 1 buffer-undo-list)))))

(defmacro harmless-editor-test-signal (form)
  "Return the error message signaled by FORM."
  `(error-message-string (should-error ,form :type 'error)))

(defun harmless-editor-test-hook-nope ()
  "Signal from a save hook."
  (error "nope"))

(defun harmless-editor-test-hook-later ()
  "Signal from an after-save hook."
  (error "later"))

(defun harmless-editor-test-hook-bang ()
  "Insert a bang at the end of the buffer."
  (goto-char (point-max))
  (insert "!"))

(defun harmless-editor-test-chat (dir)
  "Return a session at DIR whose provider does not open a connection."
  (harmless-session-new
   :cwd dir
   :provider (harmless-editor-make-fake :name "fake" :host "none")
   :model "m"))

(defun harmless-editor-test-content (session)
  "Return the last user message stored on SESSION."
  (plist-get (car (last (harmless-session-messages session))) :content))

(defmacro harmless-editor-test-bind (dir &rest body)
  "Bind DIR, a temp data directory, and a buffer list, then run BODY.
BODY may push onto `bufs'.  Nothing is written at the filesystem root."
  (declare (indent 1))
  `(let* ((,dir (make-temp-file "harmless-edit-" t))
          (harmless-directory (expand-file-name ".harmless" ,dir))
          (harmless--sessions (make-hash-table :test 'equal))
          (harmless-providers nil)
          (bufs nil))
     (unwind-protect
         (progn ,@body)
       (harmless-editor-test-cleanup ,dir bufs))))

(defmacro harmless-editor-test-no-write (&rest body)
  "Run BODY and refuse any file write."
  `(cl-letf (((symbol-function 'write-region)
              (lambda (&rest _) (error "wrote a file")))
             ((symbol-function 'write-file)
              (lambda (&rest _) (error "wrote a file"))))
     ,@body))

(defun harmless-editor-test-cleanup (dir bufs)
  "Kill BUFS and delete DIR."
  (dolist (buf bufs)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (let ((inhibit-read-only t)
              (kill-buffer-query-functions nil)
              (kill-buffer-hook nil))
          (set-buffer-modified-p nil)
          (ignore-errors (set-visited-file-name nil t))
          (kill-buffer buf)))))
  (when (> (count-windows) 1)
    (delete-other-windows))
  (when (and (stringp dir) (file-directory-p dir))
    (ignore-errors (delete-directory dir t))))

(ert-deftest harmless-editor-read-visiting-buffer ()
  (let ((dir (make-temp-file "harmless-edit-" t))
        (bufs nil))
    (unwind-protect
        (let* ((session (harmless-editor-test-session dir))
               (path (harmless-editor-test-file dir "a.txt" "one\ntwo\nthree\n"))
               (buf (harmless-editor-test-visit path 'utf-8-unix)))
          (push buf bufs)
          (should (equal "one\ntwo\nthree\n"
                         (harmless-tools-fs--read session '(:path "a.txt"))))
          (with-current-buffer buf
            (goto-char (point-max))
            (insert "more"))
          (should (equal "one\ntwo\nthree\nmore\n[unsaved]"
                         (harmless-tools-fs--read session '(:path "a.txt"))))
          (with-current-buffer buf
            (set-buffer-modified-p nil)
            (goto-char (point-min))
            (forward-line 1)
            (narrow-to-region (point) (line-end-position)))
          (should (equal "two\n[narrowed]"
                         (harmless-tools-fs--read session '(:path "a.txt"))))
          (with-current-buffer buf
            (set-buffer-modified-p t))
          (should (equal "two\n[unsaved]\n[narrowed]"
                         (harmless-tools-fs--read session '(:path "a.txt"))))
          (with-current-buffer buf
            (widen))
          (should (equal "two\n\n[unsaved]"
                         (harmless-tools-fs--read
                          session '(:path "a.txt" :offset 2 :limit 1)))))
      (harmless-editor-test-cleanup dir bufs))))

(ert-deftest harmless-editor-write-visiting-buffer ()
  (let ((dir (make-temp-file "harmless-edit-" t))
        (bufs nil))
    (unwind-protect
        (let* ((session (harmless-editor-test-session dir))
               (path (harmless-editor-test-file dir "a.txt" "OLD"))
               (buf (harmless-editor-test-visit path 'utf-8-unix))
               (called nil))
          (push buf bufs)
          (with-current-buffer buf
            (goto-char (point-max))
            (insert "!"))
          (cl-letf (((symbol-function 'harmless-checkpoint-before)
                     (lambda (_session file) (push file called))))
            (should (equal (format "Wrote %s (%d bytes)" path (string-bytes "NEW"))
                           (harmless-tools-fs--write
                            session '(:path "a.txt" :contents "NEW"))))
            (should (equal (list path) called)))
          (should (equal "NEW" (harmless-editor-test-text buf)))
          (should (equal "NEW" (harmless-editor-test-bytes path)))
          (should-not (buffer-modified-p buf))
          (harmless-editor-test-undo buf)
          (should (equal "OLD!" (harmless-editor-test-text buf)))
          (with-current-buffer buf
            (erase-buffer)
            (insert "NEW")
            (set-buffer-modified-p nil)
            (setq harmless-tools-fs--read-tick (buffer-chars-modified-tick)))
          (should (equal (format "Wrote %s (%d bytes)" path (string-bytes "NEWER"))
                         (harmless-tools-fs--write
                          session '(:path "a.txt" :contents "NEWER"))))
          (should (equal "NEWER" (harmless-editor-test-text buf)))
          (harmless-editor-test-undo buf)
          (should (equal "NEW" (harmless-editor-test-text buf))))
      (harmless-editor-test-cleanup dir bufs))))

(ert-deftest harmless-editor-write-shows-change-and-refreshes-vc ()
  (let ((dir (make-temp-file "harmless-edit-" t))
        (bufs nil)
        (harmless--session nil))
    (unwind-protect
        (let* ((session (harmless-editor-test-session dir))
               (path (harmless-editor-test-file dir "a.txt" "hello\nworld\n"))
               (buf (harmless-editor-test-visit path 'utf-8-unix))
               (file-win (selected-window))
               (refreshed 0))
          (push buf bufs)
          (set-window-buffer file-win buf)
          (with-current-buffer buf (goto-char (point-max)))
          (select-window (split-window-below))
          (let ((trans (generate-new-buffer " *harmless-editor-transcript*")))
            (push trans bufs)
            (with-current-buffer trans
              (harmless-session-mode)
              (let ((inhibit-read-only t))
                (insert "keep"))
              (goto-char (point-max)))
            (set-window-buffer (selected-window) trans)
            (cl-letf (((symbol-function 'vc-backend) (lambda (&rest _) 'Git))
                      ((symbol-function 'vc-before-save) (lambda (&rest _) nil))
                      ((symbol-function 'vc-after-save) (lambda (&rest _) nil))
                      ((symbol-function 'vc-refresh-state)
                       (lambda (&rest _) (setq refreshed (1+ refreshed)))))
              (harmless-tools-fs--write session '(:path "a.txt" :contents "Z")))
            (should (eq (window-buffer (selected-window)) trans))
            (should (equal "keep" (with-current-buffer trans
                                    (buffer-substring-no-properties
                                     (point-min) (point-max)))))
            (should (= (with-current-buffer trans (point-max))
                       (window-point (selected-window))))
            (should (eq (window-buffer file-win) buf))
            (should (= 1 (window-point file-win)))
            (should (= 1 refreshed))
            (should (equal "Z" (harmless-editor-test-bytes path)))))
      (harmless-editor-test-cleanup dir bufs))))

(ert-deftest harmless-editor-write-uses-buffer-coding ()
  (let ((dir (make-temp-file "harmless-edit-" t))
        (bufs nil))
    (unwind-protect
        (let* ((session (harmless-editor-test-session dir))
               (path (harmless-editor-test-file dir "a.txt" "old"))
               (unix (harmless-editor-test-visit path 'utf-8-unix))
               (dos nil))
          (push unix bufs)
          (harmless-tools-fs--write session '(:path "a.txt" :contents "a\nb\n"))
          (should (equal "a\nb\n" (harmless-editor-test-bytes path)))
          (should (equal "a\nb\n" (harmless-editor-test-text unix)))
          (with-current-buffer unix
            (set-buffer-modified-p nil)
            (setq buffer-read-only nil))
          (kill-buffer unix)
          (setq bufs nil)
          (setq dos (harmless-editor-test-visit path 'utf-8-dos))
          (push dos bufs)
          (harmless-tools-fs--write session '(:path "a.txt" :contents "a\nb\n"))
          (should (equal "a\r\nb\r\n" (harmless-editor-test-bytes path)))
          (should (equal "a\nb\n" (harmless-editor-test-text dos))))
      (harmless-editor-test-cleanup dir bufs))))

(ert-deftest harmless-editor-refuses-a-stale-buffer ()
  (let ((dir (make-temp-file "harmless-edit-" t))
        (bufs nil))
    (unwind-protect
        (let* ((session (harmless-editor-test-session dir))
               (path (harmless-editor-test-file dir "a.txt" "OLD"))
               (buf (harmless-editor-test-visit path 'utf-8-unix))
               (called nil))
          (push buf bufs)
          (harmless-tools-fs--read session '(:path "a.txt"))
          (with-current-buffer buf
            (goto-char (point-max))
            (insert "X"))
          (cl-letf (((symbol-function 'harmless-checkpoint-before)
                     (lambda (&rest _) (setq called t))))
            (should (equal "buffer changed since it was read"
                           (harmless-editor-test-signal
                            (harmless-tools-fs--write
                             session '(:path "a.txt" :contents "NEW")))))
            (should (equal "buffer changed since it was read"
                           (harmless-editor-test-signal
                            (harmless-tools-fs--replace
                             session '(:path "a.txt" :old_string "OLD"
                                       :new_string "NEW")))))
            (should (equal "buffer changed since it was read"
                           (harmless-editor-test-signal
                            (harmless-tools-fs--apply-patch
                             session '(:hunks ((:path "a.txt"
                                                :old_string "OLD"
                                                :new_string "NEW")))))))
            (should-not called))
          (should (equal "OLDX" (harmless-editor-test-text buf)))
          (should (equal "OLD" (harmless-editor-test-bytes path))))
      (harmless-editor-test-cleanup dir bufs))))

(ert-deftest harmless-editor-refuses-read-only-text ()
  (let ((dir (make-temp-file "harmless-edit-" t))
        (bufs nil))
    (unwind-protect
        (let* ((session (harmless-editor-test-session dir))
               (path (harmless-editor-test-file dir "a.txt" "aa XX bb"))
               (buf (harmless-editor-test-visit path 'utf-8-unix))
               (called nil))
          (push buf bufs)
          (with-current-buffer buf
            (setq buffer-read-only t))
          (cl-letf (((symbol-function 'harmless-checkpoint-before)
                     (lambda (&rest _) (setq called t))))
            (should (equal "buffer is read-only"
                           (harmless-editor-test-signal
                            (harmless-tools-fs--write
                             session '(:path "a.txt" :contents "NEW")))))
            (should-not called))
          (with-current-buffer buf
            (setq buffer-read-only nil)
            (put-text-property 1 3 'read-only t))
          (setq called nil)
          (cl-letf (((symbol-function 'harmless-checkpoint-before)
                     (lambda (&rest _) (setq called t))))
            (should (equal "buffer is read-only"
                           (harmless-editor-test-signal
                            (harmless-tools-fs--replace
                             session '(:path "a.txt" :old_string "aa"
                                       :new_string "zz")))))
            (should-not called))
          (should (equal "aa XX bb" (harmless-editor-test-text buf)))
          (should (equal "aa XX bb" (harmless-editor-test-bytes path)))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (remove-text-properties (point-min) (point-max) '(read-only nil)))
            (put-text-property 4 6 'read-only t))
          (should (equal (format "Replaced 1 occurrence(s) in %s" path)
                         (harmless-tools-fs--replace
                          session '(:path "a.txt" :old_string "aa"
                                    :new_string "zz"))))
          (should (equal "zz XX bb" (harmless-editor-test-text buf)))
          (should (equal "zz XX bb" (harmless-editor-test-bytes path)))
          (should (get-text-property 4 'read-only buf)))
      (harmless-editor-test-cleanup dir bufs))))

(ert-deftest harmless-editor-refuses-unusable-buffers ()
  (let ((dir (make-temp-file "harmless-edit-" t))
        (bufs nil)
        (harmless--session nil))
    (unwind-protect
        (let* ((session (harmless-editor-test-session dir))
               (path (harmless-editor-test-file dir "a.txt" "OLD"))
               (hidden nil)
               (session-buf nil)
               (prompt-buf nil))
          (setq hidden (harmless-editor-test-visit path 'utf-8-unix))
          (push hidden bufs)
          (with-current-buffer hidden
            (rename-buffer " a.txt" t))
          (should (equal "buffer is not a file"
                         (harmless-editor-test-signal
                          (harmless-tools-fs--read session '(:path "a.txt")))))
          (should (equal "buffer is not a file"
                         (harmless-editor-test-signal
                          (harmless-tools-fs--write
                           session '(:path "a.txt" :contents "NEW")))))
          (should (equal "OLD" (harmless-editor-test-text hidden)))
          (should (equal "OLD" (harmless-editor-test-bytes path)))
          (with-current-buffer hidden
            (set-buffer-modified-p nil))
          (kill-buffer hidden)
          (setq bufs nil)
          (setq session-buf (generate-new-buffer "*harmless-editor-session*"))
          (push session-buf bufs)
          (with-current-buffer session-buf
            (setq buffer-read-only nil)
            (set-visited-file-name path t t)
            (harmless-session-mode))
          (should (equal "buffer is not a file"
                         (harmless-editor-test-signal
                          (harmless-tools-fs--write
                           session '(:path "a.txt" :contents "NEW")))))
          (with-current-buffer session-buf
            (set-buffer-modified-p nil)
            (set-visited-file-name nil t))
          (kill-buffer session-buf)
          (setq bufs nil)
          (setq prompt-buf (generate-new-buffer "*harmless-editor-prompt*"))
          (push prompt-buf bufs)
          (with-current-buffer prompt-buf
            (set-visited-file-name path t t)
            (harmless-prompt-mode))
          (should (equal "buffer is not a file"
                         (harmless-editor-test-signal
                          (harmless-tools-fs--replace
                           session '(:path "a.txt" :old_string "OLD"
                                     :new_string "NEW")))))
          (should (equal "OLD" (harmless-editor-test-bytes path)))
          (should (harmless-tools-fs--unusable-buffer-p
                   (window-buffer (minibuffer-window)))))
      (harmless-editor-test-cleanup dir bufs))))

(ert-deftest harmless-editor-edits-an-indirect-buffer-through-its-base ()
  (let ((dir (make-temp-file "harmless-edit-" t))
        (bufs nil))
    (unwind-protect
        (let* ((session (harmless-editor-test-session dir))
               (path (harmless-editor-test-file dir "a.txt" "OLD"))
               (base (harmless-editor-test-visit path 'utf-8-unix))
               (ind (make-indirect-buffer
                     base (generate-new-buffer-name "harmless-ind"))))
          (push base bufs)
          (push ind bufs)
          (with-current-buffer base
            (goto-char (point-max))
            (insert "!"))
          (should (equal (format "Wrote %s (%d bytes)" path (string-bytes "NEW"))
                         (harmless-tools-fs--write
                          session '(:path "a.txt" :contents "NEW"))))
          (should (equal "NEW" (harmless-editor-test-text base)))
          (should (equal "NEW" (harmless-editor-test-text ind)))
          (should (equal "NEW" (harmless-editor-test-bytes path)))
          (harmless-editor-test-undo base)
          (should (equal "OLD!" (harmless-editor-test-text base)))
          (should (equal "OLD!" (harmless-editor-test-text ind))))
      (harmless-editor-test-cleanup dir bufs))))

(ert-deftest harmless-editor-save-hook-failure-restores-the-buffer ()
  (let ((dir (make-temp-file "harmless-edit-" t))
        (bufs nil))
    (unwind-protect
        (let* ((session (harmless-editor-test-session dir))
               (path (harmless-editor-test-file dir "a.txt" "OLD"))
               (buf (harmless-editor-test-visit path 'utf-8-unix)))
          (push buf bufs)
          (with-current-buffer buf
            (add-hook 'before-save-hook #'harmless-editor-test-hook-nope nil t))
          (should (equal "buffer save failed: nope"
                         (harmless-editor-test-signal
                          (harmless-tools-fs--write
                           session '(:path "a.txt" :contents "NEW")))))
          (should (equal "OLD" (harmless-editor-test-text buf)))
          (should (equal "OLD" (harmless-editor-test-bytes path)))
          (with-current-buffer buf
            (remove-hook 'before-save-hook #'harmless-editor-test-hook-nope t)
            (add-hook 'after-save-hook #'harmless-editor-test-hook-later nil t))
          (should (equal "buffer save failed: later"
                         (harmless-editor-test-signal
                          (harmless-tools-fs--write
                           session '(:path "a.txt" :contents "NEW")))))
          (should (equal "OLD" (harmless-editor-test-text buf)))
          (should (equal "OLD" (harmless-editor-test-bytes path)))
          (with-current-buffer buf
            (remove-hook 'after-save-hook #'harmless-editor-test-hook-later t)
            (add-hook 'before-save-hook #'harmless-editor-test-hook-bang nil t))
          (should (equal (format "Wrote %s (%d bytes)" path (string-bytes "NEW!"))
                         (harmless-tools-fs--write
                          session '(:path "a.txt" :contents "NEW"))))
          (should (equal "NEW!" (harmless-editor-test-text buf)))
          (should (equal "NEW!" (harmless-editor-test-bytes path)))
          (should (equal (format "old_string not found in %s" path)
                         (harmless-editor-test-signal
                          (harmless-tools-fs--replace
                           session '(:path "a.txt" :old_string "missing"
                                     :new_string "x")))))
          (should (equal "NEW!" (harmless-editor-test-text buf)))
          (should (equal "NEW!" (harmless-editor-test-bytes path))))
      (harmless-editor-test-cleanup dir bufs))))

(ert-deftest harmless-editor-restores-narrowing ()
  (let ((dir (make-temp-file "harmless-edit-" t))
        (bufs nil))
    (unwind-protect
        (let* ((session (harmless-editor-test-session dir))
               (path (harmless-editor-test-file dir "a.txt" "abcdef"))
               (buf (harmless-editor-test-visit path 'utf-8-unix)))
          (push buf bufs)
          (with-current-buffer buf
            (narrow-to-region 3 5))
          (should (equal (format "Wrote %s (%d bytes) buffer was narrowed"
                                 path (string-bytes "ABCDEF"))
                         (harmless-tools-fs--write
                          session '(:path "a.txt" :contents "ABCDEF"))))
          (with-current-buffer buf
            (should (= 3 (point-min)))
            (should (= 5 (point-max))))
          (should (equal "ABCDEF" (harmless-editor-test-text buf)))
          (should (equal "ABCDEF" (harmless-editor-test-bytes path)))
          (with-current-buffer buf
            (widen)
            (erase-buffer)
            (insert "foo bar")
            (set-buffer-modified-p nil)
            (setq buffer-file-coding-system 'utf-8-unix)
            (setq harmless-tools-fs--read-tick nil)
            (save-buffer)
            (goto-char (point-min))
            (search-forward "bar")
            (narrow-to-region (match-beginning 0) (match-end 0)))
          (should (equal (format "Replaced 1 occurrence(s) in %s buffer was narrowed"
                                 path)
                         (harmless-tools-fs--replace
                          session '(:path "a.txt" :old_string "foo"
                                    :new_string "qux"))))
          (with-current-buffer buf
            (should (equal "bar" (buffer-string))))
          (should (equal "qux bar" (harmless-editor-test-text buf)))
          (should (equal "qux bar" (harmless-editor-test-bytes path))))
      (harmless-editor-test-cleanup dir bufs))))

(ert-deftest harmless-editor-patch-visiting-buffer ()
  (let ((dir (make-temp-file "harmless-edit-" t))
        (bufs nil))
    (unwind-protect
        (let* ((session (harmless-editor-test-session dir))
               (path (harmless-editor-test-file dir "a.txt" "one"))
               (buf (harmless-editor-test-visit path 'utf-8-unix))
               (called nil))
          (push buf bufs)
          (cl-letf (((symbol-function 'harmless-checkpoint-before)
                     (lambda (_session file) (push file called))))
            (should (equal (format "Applied 1 hunk in %s (1)" path)
                           (harmless-tools-fs--apply-patch
                            session '(:hunks ((:path "a.txt"
                                               :old_string "one"
                                               :new_string "1"))))))
            (should (equal (list path) called)))
          (should (equal "1" (harmless-editor-test-text buf)))
          (should (equal "1" (harmless-editor-test-bytes path)))
          (harmless-editor-test-undo buf)
          (should (equal "one" (harmless-editor-test-text buf)))
          (with-current-buffer buf
            (setq harmless-tools-fs--read-tick nil)
            (set-buffer-modified-p nil)
            (narrow-to-region 1 2))
          (should (equal (format "Applied 1 hunk in %s (1) buffer was narrowed" path)
                         (harmless-tools-fs--apply-patch
                          session '(:hunks ((:path "a.txt"
                                             :old_string "one"
                                             :new_string "two"))))))
          (with-current-buffer buf
            (should (buffer-narrowed-p)))
          (should (equal "two" (harmless-editor-test-text buf)))
          (should (equal (format "hunk 1: old_string not found in %s" path)
                         (harmless-editor-test-signal
                          (harmless-tools-fs--apply-patch
                           session '(:hunks ((:path "a.txt"
                                              :old_string "missing"
                                              :new_string "x")))))))
          (should (equal "two" (harmless-editor-test-text buf)))
          (should (equal "two" (harmless-editor-test-bytes path))))
      (harmless-editor-test-cleanup dir bufs))))

(ert-deftest harmless-editor-patch-rolls-back-when-a-later-save-fails ()
  (let ((dir (make-temp-file "harmless-edit-" t))
        (bufs nil))
    (unwind-protect
        (let* ((session (harmless-editor-test-session dir))
               (a (harmless-editor-test-file dir "a.txt" "alpha"))
               (b (harmless-editor-test-file dir "b.txt" "beta"))
               (abuf (harmless-editor-test-visit a 'utf-8-unix))
               (bbuf (harmless-editor-test-visit b 'utf-8-unix)))
          (push abuf bufs)
          (push bbuf bufs)
          (with-current-buffer abuf
            (goto-char (point-max))
            (insert "!"))
          (with-current-buffer bbuf
            (add-hook 'after-save-hook #'harmless-editor-test-hook-nope nil t))
          (should (equal "buffer save failed: nope"
                         (harmless-editor-test-signal
                          (harmless-tools-fs--apply-patch
                           session '(:hunks ((:path "a.txt"
                                              :old_string "alpha"
                                              :new_string "ALPHA")
                                             (:path "b.txt"
                                              :old_string "beta"
                                              :new_string "BETA")))))))
          (should (equal "alpha!" (harmless-editor-test-text abuf)))
          (should (equal "alpha" (harmless-editor-test-bytes a)))
          (should (equal "beta" (harmless-editor-test-text bbuf)))
          (should (equal "beta" (harmless-editor-test-bytes b))))
      (harmless-editor-test-cleanup dir bufs))))

(ert-deftest harmless-editor-plan-mode-refuses-a-visiting-buffer ()
  (let ((dir (make-temp-file "harmless-edit-" t))
        (bufs nil))
    (unwind-protect
        (let* ((session (harmless-editor-test-session dir))
               (path (harmless-editor-test-file dir "a.txt" "OLD"))
               (buf (harmless-editor-test-visit path 'utf-8-unix)))
          (push buf bufs)
          (with-current-buffer buf
            (goto-char (point-max))
            (insert "!"))
          (setf (harmless-session-plan-mode session) t)
          (should (equal "Plan mode is on. Only plan.md may be edited, with write_plan"
                         (harmless-editor-test-signal
                          (harmless-tools-fs--write
                           session '(:path "a.txt" :contents "NEW")))))
          (should (equal "Plan mode is on. Only plan.md may be edited, with write_plan"
                         (harmless-editor-test-signal
                          (harmless-tools-fs--replace
                           session '(:path "a.txt" :old_string "OLD"
                                     :new_string "NEW")))))
          (should (equal "Plan mode is on. Only plan.md may be edited, with write_plan"
                         (harmless-editor-test-signal
                          (harmless-tools-fs--apply-patch
                           session '(:hunks ((:path "a.txt"
                                              :old_string "OLD"
                                              :new_string "NEW")))))))
          (should (equal "OLD!" (harmless-editor-test-text buf)))
          (should (equal "OLD" (harmless-editor-test-bytes path))))
      (harmless-editor-test-cleanup dir bufs))))

(ert-deftest harmless-editor-path-refusals-leave-the-buffer ()
  (let ((dir (make-temp-file "harmless-edit-" t))
        (bufs nil))
    (unwind-protect
        (let* ((session (harmless-editor-test-session dir))
               (path (harmless-editor-test-file dir "a.txt" "OLD"))
               (buf (harmless-editor-test-visit path 'utf-8-unix))
               (slashed (harmless-tools-fs-resolve session "a.txt/"))
               (called nil))
          (push buf bufs)
          (with-current-buffer buf
            (goto-char (point-max))
            (insert "!"))
          (should (equal "Path is empty"
                         (harmless-editor-test-signal
                          (harmless-tools-fs--write session '(:path nil :contents "X")))))
          (should (equal "Path is empty"
                         (harmless-editor-test-signal
                          (harmless-tools-fs--write session '(:path "" :contents "X")))))
          (should (equal "Path escapes project: /"
                         (harmless-editor-test-signal
                          (harmless-tools-fs--read session '(:path "/")))))
          (should (equal "Path escapes project: c:/"
                         (harmless-editor-test-signal
                          (harmless-tools-fs--replace
                           session '(:path "c:/" :old_string "a" :new_string "b")))))
          (cl-letf (((symbol-function 'harmless-checkpoint-before)
                     (lambda (&rest _) (setq called t))))
            (should (equal (format "File exists: %s" path)
                           (harmless-editor-test-signal
                            (harmless-tools-fs--write
                             session '(:path "a.txt/" :contents "NEW")))))
            (should-not called)
            (should (equal (format "Cannot read %s" slashed)
                           (harmless-editor-test-signal
                            (harmless-tools-fs--read session '(:path "a.txt/")))))
            (should (equal (format "Cannot read %s" slashed)
                           (harmless-editor-test-signal
                            (harmless-tools-fs--replace
                             session '(:path "a.txt/" :old_string "OLD"
                                       :new_string "NEW")))))
            (should (equal (format "hunk 1: Cannot read %s" slashed)
                           (harmless-editor-test-signal
                            (harmless-tools-fs--apply-patch
                             session '(:hunks ((:path "a.txt/"
                                                :old_string "OLD"
                                                :new_string "NEW"))))))))
          (should (equal (format "Wrote %s (%d bytes)" path (string-bytes "REL"))
                         (harmless-tools-fs--write
                          session '(:path "a.txt" :contents "REL"))))
          (should (equal "REL" (harmless-editor-test-text buf)))
          (should (equal "REL" (harmless-editor-test-bytes path)))
          (should (equal "OLD!" (progn
                                  (harmless-editor-test-undo buf)
                                  (harmless-editor-test-text buf)))))
      (harmless-editor-test-cleanup dir bufs))))

(ert-deftest harmless-editor-no-buffer-keeps-the-file-report ()
  (let* ((dir (make-temp-file "harmless-edit-" t))
         (session (harmless-editor-test-session dir))
         (path (expand-file-name "a.txt" dir)))
    (unwind-protect
        (progn
          (should (equal (format "Wrote %s (%d bytes)" path (string-bytes "foo bar foo"))
                         (harmless-tools-fs--write
                          session '(:path "a.txt" :contents "foo bar foo"))))
          (should (equal "foo bar foo"
                         (harmless-tools-fs--read session '(:path "a.txt"))))
          (should (equal (format "Replaced 2 occurrence(s) in %s" path)
                         (harmless-tools-fs--replace
                          session '(:path "a.txt" :old_string "foo"
                                    :new_string "baz" :replace_all t))))
          (should (equal "baz bar baz"
                         (harmless-tools-fs--read session '(:path "a.txt"))))
          (should (equal "baz bar baz" (harmless-editor-test-bytes path))))
      (harmless-editor-test-cleanup dir nil))))

(ert-deftest harmless-editor-attaches-a-file-buffer ()
  (harmless-editor-test-bind dir
    (let* ((path (harmless-editor-test-file
                  dir "a.el" "(defun hello ()\n  (message \"hi\"))\n"))
           (buf (let ((default-directory dir))
                  (find-file-noselect "a.el"))))
      (push buf bufs)
      (with-current-buffer buf
        (setq-local require-final-newline nil)
        (should (file-name-absolute-p buffer-file-name))
        (should (equal buffer-file-name path))
        (should-not (directory-name-p buffer-file-name))
        (goto-char (point-min))
        (forward-line 1)
        (harmless-editor-test-no-write
         (should (equal (format (concat "buffer: %s\nfile: %s\n"
                                        "mode: emacs-lisp-mode\n"
                                        "line: 2\ncolumn: 1\n"
                                        "modified: no\nnarrowed: no\n"
                                        "line-text:   (message \"hi\"))\n"
                                        "function: hello")
                                (buffer-name) path)
                        (harmless-editor-note))))
        (forward-char 2)
        (should (string-search "\ncolumn: 3\n" (harmless-editor-note)))
        (set-buffer-modified-p t)
        (should (string-search "\nmodified: yes\n" (harmless-editor-note)))
        (set-buffer-modified-p nil)
        (goto-char (point-min))
        (forward-line 1)
        (narrow-to-region (point) (line-end-position))
        (let ((note (harmless-editor-note)))
          (should (string-search "\nline: 2\n" note))
          (should (string-search "\nnarrowed: yes\n" note))
          (should (string-search "line-text:   (message \"hi\"))" note)))
        (transient-mark-mode 1)
        (let ((use-empty-active-region nil))
          (set-mark (point))
          (activate-mark)
          (let ((note (harmless-editor-note)))
            (should (string-search "line-text:" note))
            (should-not (string-search "\nregion:" note))))
        (deactivate-mark)
        (goto-char (line-beginning-position))
        (set-mark (line-end-position))
        (activate-mark)
        (let* ((harmless-read-file-max-bytes 4)
               (region (buffer-substring-no-properties
                        (region-beginning) (region-end)))
               (cut (harmless-tools-fs--text-window region nil nil))
               (note (harmless-editor-note)))
          (should (equal cut (concat (substring region 0 4) "\n[truncated]")))
          (should (string-search (concat "region:\n" cut) note))
          (should-not (string-search "line-text:" note))
          (should (string-search "\ncolumn: 1\n" note)))
        (deactivate-mark)
        (widen)
        (let ((indirect (clone-indirect-buffer "indirect-a" nil)))
          (push indirect bufs)
          (with-current-buffer indirect
            (goto-char (point-min))
            (forward-line 1)
            (let ((note (harmless-editor-note)))
              (should (string-search "buffer: indirect-a\n" note))
              (should (string-search (format "file: %s\n" path) note))
              (should (string-search "\nline: 2\n" note))
              (should (string-search "\nfunction: hello" note)))))))))

(ert-deftest harmless-editor-names-a-non-file-buffer ()
  (harmless-editor-test-bind dir
    (harmless-editor-test-file dir "SECRET.txt" "SECRET-BODY")
    (let ((notes (get-buffer-create "notes"))
          (hidden (get-buffer-create " hidden"))
          (root (get-buffer-create "root-notes"))
          (slash (file-name-as-directory dir)))
      (push notes bufs)
      (push hidden bufs)
      (push root bufs)
      (should (directory-name-p slash))
      (with-current-buffer notes
        (fundamental-mode)
        (erase-buffer)
        (insert "SECRET")
        (harmless-editor-test-no-write
         (should (equal "buffer: notes\nmode: fundamental-mode"
                        (harmless-editor-note))))
        (should-not (string-search "SECRET" (harmless-editor-note))))
      (with-current-buffer hidden
        (erase-buffer)
        (insert "HIDDEN")
        (should-not (harmless-editor-note)))
      (with-current-buffer (window-buffer (minibuffer-window))
        (should-not (harmless-editor-note)))
      (with-current-buffer root
        (fundamental-mode)
        (erase-buffer)
        (setq default-directory "/")
        (insert "NOFILE")
        (harmless-editor-test-no-write
         (let ((note (harmless-editor-note)))
           (should (equal "buffer: root-notes\nmode: fundamental-mode" note))
           (should-not (string-search "NOFILE" note))
           (should-not (string-search "file:" note)))))
      (let ((dbuf (dired-noselect slash)))
        (push dbuf bufs)
        (with-current-buffer dbuf
          (harmless-editor-test-no-write
           (let ((note (harmless-editor-note)))
             (should (equal (format "buffer: %s\nmode: %s"
                                    (buffer-name) major-mode)
                            note))
             (should-not (string-search "file:" note))
             (should-not (string-search "SECRET-BODY" note))
             (should-not (string-search "line-text:" note)))))))))

(ert-deftest harmless-editor-refuses-an-empty-prompt ()
  (harmless-editor-test-bind dir
    (let* ((path (harmless-editor-test-file dir "a.txt" "OLD\n"))
           (buf (harmless-editor-test-visit path 'utf-8-unix))
           (session (harmless-editor-test-chat dir)))
      (push buf bufs)
      (with-current-buffer buf
        (setq harmless--session session)
        (dolist (text (list nil "" " " "\t" "\n"))
          (should (equal "Prompt is empty"
                         (harmless-editor-test-signal
                          (harmless-steer text)))))
        (should (equal "Prompt is empty"
                       (harmless-editor-test-signal
                        (harmless-prompt-send)))))
      (should-not (harmless-session-messages session))
      (should-not (harmless-session-steers session))
      (should (equal "OLD\n" (harmless-editor-test-text buf)))
      (should (equal "OLD\n" (harmless-editor-test-bytes path))))))

(ert-deftest harmless-editor-sends-the-attachment ()
  (harmless-editor-test-bind dir
    (let* ((path (harmless-editor-test-file dir "a.txt" "FIRST\nLAST\n"))
           (buf (harmless-editor-test-visit path 'utf-8-unix))
           (session (harmless-editor-test-chat dir))
           (prompt (harmless-ui-ensure-prompt-buffer session))
           (transcript (harmless-ui-ensure-session-buffer session)))
      (push buf bufs)
      (push prompt bufs)
      (push transcript bufs)
      (harmless-editor-test-file dir "b.txt" "alpha\n")
      (with-current-buffer buf
        (setq harmless--session session)
        (goto-char (point-min))
        (harmless-turn-run session "plain"))
      (should (equal "plain" (harmless-editor-test-content session)))
      (should (eq 'streaming (harmless-session-status session)))
      (with-current-buffer buf
        (goto-char (point-min))
        (let ((note (harmless-editor-note)))
          (should (string-search "line-text: FIRST" note))
          (harmless-steer "please")
          (should (equal (list (cons "please" note))
                         (harmless-session-steers session)))
          (goto-char (point-max))
          (insert "MOVED")
          (harmless-turn--take-steers session)
          (should (equal (harmless-editor-join "please" note)
                         (harmless-editor-test-content session)))
          (should-not (string-search "MOVED"
                                     (harmless-editor-test-content session)))))
      (should (equal "FIRST\nLAST\nMOVED" (harmless-editor-test-text buf)))
      (should (equal "FIRST\nLAST\n" (harmless-editor-test-bytes path)))
      (with-current-buffer buf
        (erase-buffer)
        (insert "@file:missing.txt")
        (goto-char (point-min))
        (transient-mark-mode 1)
        (set-mark (point-max))
        (activate-mark)
        (setf (harmless-session-status session) 'idle)
        (harmless-steer "please")
        (should (string-search "region:\n@file:missing.txt"
                               (harmless-editor-test-content session)))
        (should-not (string-search "context reference is missing"
                                   (harmless-editor-test-content session)))
        (deactivate-mark)
        (let ((before (length (harmless-session-messages session))))
          (setf (harmless-session-status session) 'idle)
          (should (equal "context reference is missing"
                         (harmless-editor-test-signal
                          (harmless-steer "see @file:missing.txt"))))
          (should (= before (length (harmless-session-messages session)))))
        (erase-buffer)
        (insert "FIRST\nLAST\n")
        (set-buffer-modified-p nil)
        (goto-char (point-min))
        (setf (harmless-session-status session) 'idle)
        (harmless-steer "see @file:b.txt")
        (let ((content (harmless-editor-test-content session)))
          (should (string-search "@file:b.txt\nalpha\n" content))
          (should (string-search "\nbuffer: a.txt\n" content))
          (should (string-search (format "file: %s\n" path) content))
          (should (string-search harmless-refs-header content))))
      (with-current-buffer prompt
        (erase-buffer)
        (insert "from-file"))
      (with-current-buffer buf
        (goto-char (point-min))
        (setf (harmless-session-status session) 'idle)
        (let ((note (harmless-editor-note)))
          (harmless-prompt-send)
          (should (equal (harmless-editor-join "from-file" note)
                         (harmless-editor-test-content session)))))
      (should (equal "FIRST\nLAST\n" (harmless-editor-test-text buf)))
      (should (string-empty-p (with-current-buffer prompt (buffer-string))))
      (should (equal "FIRST\nLAST\n" (harmless-editor-test-bytes path)))
      (with-current-buffer prompt
        (insert " \t"))
      (with-current-buffer buf
        (should (equal "Prompt is empty"
                       (harmless-editor-test-signal
                        (harmless-prompt-send))))
        (should (equal "FIRST\nLAST\n" (buffer-string))))
      (should (equal " \t" (with-current-buffer prompt (buffer-string))))
      (delete-other-windows)
      (let ((file-win (selected-window)))
        (set-window-buffer file-win buf)
        (with-current-buffer buf
          (goto-char (point-max)))
        (set-window-point file-win (with-current-buffer buf (point-min)))
        (split-window-below)
        (other-window 1)
        (set-window-buffer (selected-window) prompt)
        (with-current-buffer prompt
          (erase-buffer)
          (insert "from-prompt")
          (setq harmless--session session)
          (let ((note (harmless-editor-note)))
            (should (string-search "\nline: 1\n" note))
            (should (string-search "line-text: FIRST" note))
            (should-not (string-search "line-text: LAST" note))
            (setf (harmless-session-status session) 'idle)
            (harmless-prompt-send)
            (should (equal (harmless-editor-join "from-prompt" note)
                           (harmless-editor-test-content session)))))
        (should (equal "FIRST\nLAST\n" (harmless-editor-test-text buf)))
        (should (string-empty-p (with-current-buffer prompt (buffer-string))))
        (let ((notes (get-buffer-create "notes")))
          (push notes bufs)
          (with-current-buffer notes
            (fundamental-mode)
            (erase-buffer)
            (insert "SECRET"))
          (set-window-buffer (next-window nil nil (selected-frame)) notes)
          (with-current-buffer prompt
            (insert "named")
            (should-not (harmless-editor-note))
            (setf (harmless-session-status session) 'idle)
            (harmless-prompt-send))
          (should (equal "named" (harmless-editor-test-content session)))
          (should (equal "SECRET" (with-current-buffer notes (buffer-string))))))
      (delete-other-windows)
      (set-window-buffer (selected-window) prompt)
      (with-current-buffer prompt
        (insert "alone")
        (should-not (harmless-editor-note))
        (setf (harmless-session-status session) 'idle)
        (harmless-prompt-send))
      (should (equal "alone" (harmless-editor-test-content session)))
      (delete-other-windows)
      (let ((file-win (selected-window)))
        (set-window-buffer file-win buf)
        (with-current-buffer buf
          (goto-char (point-max)))
        (set-window-point file-win (with-current-buffer buf (point-min)))
        (split-window-below)
        (other-window 1)
        (set-window-buffer (selected-window) transcript)
        (with-current-buffer transcript
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "KEEP-TRANSCRIPT")))
        (with-current-buffer prompt
          (erase-buffer)
          (insert "from-transcript"))
        (with-current-buffer transcript
          (let ((note (harmless-editor-note)))
            (should (string-search "line-text: FIRST" note))
            (should-not (string-search "line-text: LAST" note))
            (setf (harmless-session-status session) 'idle)
            (harmless-prompt-send)
            (should (equal (harmless-editor-join "from-transcript" note)
                           (harmless-editor-test-content session))))
          (should (string-prefix-p "KEEP-TRANSCRIPT" (buffer-string))))
        (should (equal "FIRST\nLAST\n" (harmless-editor-test-text buf)))
        (should (string-empty-p (with-current-buffer prompt (buffer-string)))))
      (should (equal "FIRST\nLAST\n" (harmless-editor-test-bytes path))))))

(provide 'harmless-editor-tests)
