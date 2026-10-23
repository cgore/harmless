;;; harmless-visit-tests.el --- Tests for transcript FILE:LINE buttons -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless-editor)
(require 'harmless-openai)
(require 'harmless-session)
(require 'harmless-tools-fs)
(require 'harmless-ui)

(defmacro harmless-visit-test-signal (form)
  "Return the error message signaled by FORM."
  `(error-message-string (should-error ,form :type 'error)))

(defun harmless-visit-test-file (dir name text)
  "Write TEXT to NAME in DIR and return the path."
  (let ((path (expand-file-name name dir))
        (coding-system-for-write 'utf-8-unix))
    (write-region text nil path nil 'silent)
    path))

(defun harmless-visit-test-cleanup (dir bufs)
  "Kill BUFS and any buffer visiting a file under DIR, then delete DIR."
  (dolist (buf (buffer-list))
    (let ((file (buffer-file-name buf)))
      (when (and (stringp dir) (stringp file)
                 (file-in-directory-p file dir))
        (push buf bufs))))
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

(defun harmless-visit-test-buttons ()
  "Return (TEXT PATH LINE COLUMN) for each location button."
  (let ((pos (point-min))
        (out nil))
    (while (< pos (point-max))
      (let ((spec (get-text-property pos 'harmless-visit)))
        (if (not spec)
            (setq pos (1+ pos))
          (let ((next (or (next-single-property-change
                           pos 'harmless-visit nil (point-max))
                          (point-max))))
            (push (cons (buffer-substring-no-properties pos next) spec) out)
            (setq pos next)))))
    (nreverse out)))

(defmacro harmless-visit-test-refused (form message)
  "Assert FORM signals MESSAGE and does not visit a buffer."
  `(let ((harmless-visit-test--called nil))
     (cl-letf (((symbol-function 'find-file-noselect)
                (lambda (&rest _)
                  (setq harmless-visit-test--called t)
                  (error "visited a buffer"))))
       (should (equal ,message
                      (error-message-string
                       (should-error ,form :type 'error)))))
     (should-not harmless-visit-test--called)))

(ert-deftest harmless-visit-parse-reads-line-and-column ()
  (should (equal '("a.el" 2 nil) (harmless-visit-parse "a.el:2")))
  (should (equal '("lisp/a.el" 2 3) (harmless-visit-parse "lisp/a.el:2:3")))
  (should (equal '("D:/lisp/a.el" 2 3)
                 (harmless-visit-parse "D:/lisp/a.el:2:3")))
  (should (equal '("D:\\lisp\\a.el" 2 1)
                 (harmless-visit-parse "D:\\lisp\\a.el:2:1")))
  (should-not (harmless-visit-parse "12:30"))
  (should-not (harmless-visit-parse "https://example.com:443"))
  (should-not (harmless-visit-parse nil))
  (should-not (harmless-visit-parse "")))

(ert-deftest harmless-visit-opens-the-other-window ()
  (let* ((dir (make-temp-file "harmless-visit-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (provider (harmless-make-openai-compat
                    "local" :host "127.0.0.1:9" :protocol "http"
                    :key "none" :models '("m")))
         (harmless-providers (list provider))
         (extra (make-temp-file "harmless-visit-extra-" t))
         (bufs nil)
         (session (harmless-session-new :cwd dir :provider provider :model "m"))
         (path (harmless-visit-test-file dir "a.el" "aaaa\nbbbb\ncccc\n"))
         (note (harmless-visit-test-file extra "note.txt" "hello\n")))
    (unwind-protect
        (let ((transcript nil))
          (push (find-file-noselect path) bufs)
          (harmless-extra-add session extra)
          (with-current-buffer (get-file-buffer path)
            (should (equal "aaaa\nbbbb\ncccc\n" (buffer-string)))
            (narrow-to-region (point-min) (line-end-position)))
          (setq transcript (harmless-ui-ensure-session-buffer session))
          (set-window-buffer (selected-window) transcript)
          (push transcript bufs)
          (let ((full (harmless-visit-location session "a.el" 2 2)))
            (should (file-equal-p path full))
            (should (eq (current-buffer) (get-file-buffer path)))
            (should (= 2 (line-number-at-pos)))
            (should (= 2 (1+ (current-column))))
            (should (equal "b" (buffer-substring-no-properties
                                (point) (1+ (point)))))
            (should-not (buffer-narrowed-p))
            (should-not (buffer-modified-p))
            (should (get-buffer-window (current-buffer)))
            (should (get-buffer-window transcript))
            (should-not (eq (get-buffer-window (current-buffer))
                            (get-buffer-window transcript))))
          (set-window-buffer (selected-window) transcript)
          (harmless-visit-location session "a.el" 1)
          (should (eq (current-buffer) (get-file-buffer path)))
          (should (= 1 (line-number-at-pos)))
          (should (= 1 (1+ (current-column))))
          (let ((shown (harmless-visit-location session note 1 1)))
            (should (file-equal-p note shown))
            (should (eq (current-buffer) (get-file-buffer note)))
            (should (= 1 (1+ (current-column))))
            (should (equal "h" (buffer-substring-no-properties
                                (point) (1+ (point))))))
          (should (equal "aaaa\nbbbb\ncccc\n"
                         (with-current-buffer (get-file-buffer path)
                           (buffer-string)))))
      (harmless-visit-test-cleanup dir bufs)
      (harmless-visit-test-cleanup extra nil))))

(ert-deftest harmless-visit-refuses-bad-paths ()
  (let* ((dir (make-temp-file "harmless-visit-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (provider (harmless-make-openai-compat
                    "local" :host "127.0.0.1:9" :protocol "http"
                    :key "none" :models '("m")))
         (harmless-providers (list provider))
         (outside (expand-file-name
                   "harmless-visit-outside.el"
                   (file-name-directory (directory-file-name dir))))
         (bufs nil)
         (session (harmless-session-new :cwd dir :provider provider :model "m")))
    (unwind-protect
        (progn
          (harmless-visit-test-file dir "a.el" "aaaa\n")
          (make-directory (expand-file-name "sub" dir))
          (dolist (path (list nil "" "   " "\n"))
            (harmless-visit-test-refused
             (harmless-visit-location session path 1)
             "Path is empty"))
          (harmless-visit-test-refused
           (harmless-visit-location session 12 1)
           "path must be a string")
          (dolist (root '("/" "c:/" "c:\\" "C:/" "c:" "\\\\" " / " " c:/ "))
            (harmless-visit-test-refused
             (harmless-visit-location session root 1)
             "path is the filesystem root"))
          (harmless-visit-test-refused
           (harmless-visit-location session ".." 1)
           "Path escapes project: ..")
          (harmless-visit-test-refused
           (harmless-visit-location session "../nope.el" 1)
           "Path escapes project: ../nope.el")
          (harmless-visit-test-refused
           (harmless-visit-location session outside 1)
           (format "Path escapes project: %s" outside))
          (harmless-visit-test-refused
           (harmless-visit-location session "missing.el" 1)
           "Cannot read missing.el")
          (harmless-visit-test-refused
           (harmless-visit-location session "sub" 1)
           "path is not a file")
          (harmless-visit-test-refused
           (harmless-visit-location session "." 1)
           "path is not a file")
          (harmless-visit-test-refused
           (harmless-visit-location session "a.el/" 1)
           "path is not a file")
          (harmless-visit-test-refused
           (harmless-visit-location session "a.el\\" 1)
           "path is not a file")
          (harmless-visit-test-refused
           (harmless-visit-location nil "a.el" 1)
           "No Harmless session")
          (should (equal "aaaa\n"
                         (with-temp-buffer
                           (insert-file-contents
                            (expand-file-name "a.el" dir))
                           (buffer-string))))
          (should-not (get-file-buffer (expand-file-name "a.el" dir))))
      (harmless-visit-test-cleanup dir bufs)
      (ignore-errors (delete-file outside)))))

(ert-deftest harmless-visit-refuses-bad-positions ()
  (let* ((dir (make-temp-file "harmless-visit-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (provider (harmless-make-openai-compat
                    "local" :host "127.0.0.1:9" :protocol "http"
                    :key "none" :models '("m")))
         (harmless-providers (list provider))
         (session (harmless-session-new :cwd dir :provider provider :model "m")))
    (unwind-protect
        (progn
          (harmless-visit-test-file dir "a.el" "aaaa\nbbbb\n")
          (dolist (line '(0 -1 1.5 "2" t))
            (harmless-visit-test-refused
             (harmless-visit-location session "a.el" line)
             "line is not a line"))
          (dolist (column '(0 -3 1.2 "1" t))
            (harmless-visit-test-refused
             (harmless-visit-location session "a.el" 1 column)
             "column is not a column"))
          (should-not (get-file-buffer (expand-file-name "a.el" dir))))
      (harmless-visit-test-cleanup dir nil))))

(ert-deftest harmless-visit-buttons-stay-in-the-transcript ()
  (let* ((dir (make-temp-file "harmless-visit-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (provider (harmless-make-openai-compat
                    "local" :host "127.0.0.1:9" :protocol "http"
                    :key "none" :models '("m")))
         (harmless-providers (list provider))
         (other (generate-new-buffer " *visit-next-error*"))
         (bufs nil)
         (abs nil)
         (session (harmless-session-new :cwd dir :provider provider :model "m")))
    (unwind-protect
        (progn
          (setq abs (harmless-visit-test-file dir "a.el" "aaaa\nbbbb\ncccc\n"))
          (with-current-buffer other
            (setq-local next-error-function #'ignore))
          (setf (harmless-session-messages session)
                (list
                 (list :role :user :content "look at c.el:2 please")
                 (list :role :assistant
                       :content
                       (format "Open `a.el:2:2` now.\nSee %s:2:2.\n../nope.el:1\nhttps://example.com:443/index.html\nat 12:30.\n"
                               abs)
                       :tool-calls
                       (list (list :id "g" :name "harmless-tool-grep"
                                   :args "{\"pattern\":\"bbbb\"}")))
                 (list :role :tool :id "g" :name "harmless-tool-grep"
                       :content "a.el:2:bbbb\nhttps://example.com:80/nope\n")))
          (harmless-ui-render-session session)
          (let ((next-error-last-buffer other)
                (transcript (harmless-session-buffer session)))
            (push transcript bufs)
            (with-current-buffer transcript
              (should-not next-error-function)
              (should (eq (key-binding (kbd "RET")) #'harmless-ui-goto-prompt))
              (let ((buttons (harmless-visit-test-buttons)))
                (should (equal '("a.el:2:2" "a.el" 2 2)
                               (cons (car (nth 0 buttons))
                                     (cdr (nth 0 buttons)))))
                (should (equal (format "%s:2:2" abs)
                               (car (nth 1 buttons))))
                (should (equal (list abs 2 2) (cdr (nth 1 buttons))))
                (should (equal '("../nope.el:1" "../nope.el" 1 nil)
                               (cons (car (nth 2 buttons))
                                     (cdr (nth 2 buttons)))))
                (should (equal '("a.el:2" "a.el" 2 nil)
                               (cons (car (nth 3 buttons))
                                     (cdr (nth 3 buttons)))))
                (should (= 4 (length buttons))))
              (goto-char (point-min))
              (search-forward "c.el:2")
              (should-not (get-text-property (1- (point)) 'harmless-visit))
              (goto-char (point-min))
              (search-forward "example.com")
              (should-not (get-text-property (1- (point)) 'harmless-visit))
              (goto-char (point-min))
              (search-forward "12:30")
              (should-not (get-text-property (1- (point)) 'harmless-visit))
              (goto-char (point-min))
              (search-forward "a.el:2:2")
              (backward-char)
              (should (eq (key-binding (kbd "RET")) #'push-button))
              (should (eq next-error-last-buffer other))
              (set-window-buffer (selected-window) transcript)
              (push-button)
              (should (eq (current-buffer) (get-file-buffer abs)))
              (should (= 2 (line-number-at-pos)))
              (should (= 2 (1+ (current-column))))
              (should (get-buffer-window transcript))
              (should-not (eq (get-buffer-window transcript)
                              (get-buffer-window (current-buffer))))
              (with-current-buffer transcript
                (should-not next-error-function))
              (should (eq next-error-last-buffer other)))
          (with-current-buffer other
            (should (eq next-error-function #'ignore)))
          (with-current-buffer (get-file-buffer abs)
            (should-not (get-text-property (point-min) 'harmless-visit))
            (should-not next-error-function))))
      (harmless-visit-test-cleanup dir bufs)
      (when (buffer-live-p other)
        (kill-buffer other)))))

(provide 'harmless-visit-tests)
