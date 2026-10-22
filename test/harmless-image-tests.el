;;; harmless-image-tests.el --- Tests for image generation -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(defun harmless-image-test-bytes ()
  "Return a short PNG-signed byte string."
  (concat harmless-media--png-magic (unibyte-string ?H ?I ?M ?G)))

(defun harmless-image-test-read (path)
  "Return the raw bytes of PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun harmless-image-test-session (id cwd)
  "Return a session ID at CWD."
  (harmless-session--create
   :id id
   :cwd cwd
   :model "grok-4.6"
   :source 'agent
   :permission-mode 'ask
   :status 'idle
   :messages nil
   :plan-mode nil))

(defun harmless-image-test-signal (fn)
  "Return the error text from FN."
  (error-message-string (should-error (funcall fn) :type 'error)))

(ert-deftest harmless-image-writes-the-generated-bytes ()
  (let* ((base (make-temp-file "harmless-image-" t))
         (root (expand-file-name "proj" base))
         (elsewhere (expand-file-name "elsewhere" base))
         (harmless-directory (expand-file-name ".harmless-state" base))
         (session (harmless-image-test-session "himg-write" root))
         (bytes (harmless-image-test-bytes))
         (called nil)
         (harmless-image-generate-function
          (lambda (_session prompt)
            (setq called prompt)
            bytes))
         (dest nil))
    (make-directory root t)
    (make-directory elsewhere t)
    (unwind-protect
        (let ((default-directory (file-name-as-directory elsewhere)))
          (should (eq (harmless-tool-class
                       (harmless-tool-by-name "harmless-tool-generate-image"))
                      'edit))
          (setq dest (harmless-image-generate
                      session '(:prompt "  a red dot  " :path "sub/dot.png")))
          (should (equal "a red dot" called))
          (should (string-prefix-p "Wrote " dest))
          (should (string-suffix-p
                   (format " (%d bytes)" (string-bytes bytes)) dest))
          (let ((path (expand-file-name "sub/dot.png" root)))
            (should (file-regular-p path))
            (should (file-name-absolute-p path))
            (should (harmless-directory-strictly-under-p path root))
            (should-not (equal (file-truename path) (file-truename "/")))
            (should (equal bytes (harmless-image-test-read path)))
            (should (string-match-p (regexp-quote path) dest)))
          (should-not (file-exists-p (expand-file-name "sub/dot.png" elsewhere)))
          (should-not (file-exists-p (expand-file-name "dot.png" elsewhere)))
          (let* ((again (concat harmless-media--png-magic
                                (unibyte-string ?A ?G ?A ?I ?N)))
                 (harmless-image-generate-function
                  (lambda (_session prompt)
                    (setq called prompt)
                    again))
                 (wrote (harmless-image-generate
                         session '(:prompt "again" :path "sub/dot.png"))))
            (should (equal "again" called))
            (should (string-prefix-p "Wrote " wrote))
            (should (equal again
                           (harmless-image-test-read
                            (expand-file-name "sub/dot.png" root)))))
          (setq called nil)
          (should (equal "image path is not a file"
                         (harmless-image-test-signal
                          (lambda ()
                            (harmless-image-generate
                             session
                             (list :prompt "dir"
                                   :path (file-name-as-directory
                                          (concat root "/"))))))))
          (should-not called)
          (should (equal (concat harmless-media--png-magic
                                 (unibyte-string ?A ?G ?A ?I ?N))
                         (harmless-image-test-read
                          (expand-file-name "sub/dot.png" root)))))
      (ignore-errors (delete-directory base t)))))

(ert-deftest harmless-image-refuses-a-bad-request ()
  (let* ((base (make-temp-file "harmless-image-bad-" t))
         (root (expand-file-name "proj" base))
         (harmless-directory (expand-file-name ".harmless-state" base))
         (session (harmless-image-test-session "himg-bad" root))
         (called nil)
         (harmless-image-generate-function
          (lambda (_session _prompt)
            (setq called t)
            (harmless-image-test-bytes)))
         (before nil))
    (make-directory root t)
    (dolist (path '("/dot.png" "c:/dot.png" "d:/dot.png"
                    "/.harmless" "c:/.harmless" "d:/.harmless"))
      (push (cons path (file-exists-p path)) before))
    (unwind-protect
        (let ((default-directory (file-name-as-directory root)))
          (dolist (pair (list (cons nil "image prompt is empty")
                              (cons "" "image prompt is empty")
                              (cons "  " "image prompt is empty")
                              (cons 'dot "image prompt must be a string")))
            (should (equal (cdr pair)
                           (harmless-image-test-signal
                            (lambda ()
                              (harmless-image-generate
                               session (list :prompt (car pair)
                                             :path "dot.png")))))))
          (should (equal "No Harmless session"
                         (harmless-image-test-signal
                          (lambda ()
                            (harmless-image-generate
                             nil '(:prompt "x" :path "dot.png"))))))
          (should (equal "image path is empty"
                         (harmless-image-test-signal
                          (lambda ()
                            (harmless-image-generate
                             session '(:prompt "x" :path "  "))))))
          (should (equal "image path is empty"
                         (harmless-image-test-signal
                          (lambda ()
                            (harmless-image-generate
                             session '(:prompt "x"))))))
          (should (equal "image path must be a string"
                         (harmless-image-test-signal
                          (lambda ()
                            (harmless-image-generate
                             session '(:prompt "x" :path 42))))))
          (dolist (path '("/" "c:/" "d:/" "c:\\" "C:/" "c:" "\\" "//" "/."))
            (should (equal "image path is the filesystem root"
                           (harmless-image-test-signal
                            (lambda ()
                              (harmless-image-generate
                               session (list :prompt "x" :path path)))))))
          (should (equal "Error: image prompt is empty"
                         (harmless-tool-call
                          (harmless-tool-by-name "harmless-tool-generate-image")
                          session '(:prompt "" :path "dot.png"))))
          (should (equal "image path is not a file"
                         (harmless-image-test-signal
                          (lambda ()
                            (harmless-image-generate
                             session '(:prompt "x" :path "dot.png/"))))))
          (should (equal "image path must be absolute"
                         (harmless-image-test-signal
                          (lambda ()
                            (harmless-image-generate
                             session '(:prompt "x" :path "~/dot.png"))))))
          (should (equal "image path escapes the project: .."
                         (harmless-image-test-signal
                          (lambda ()
                            (harmless-image-generate
                             session '(:prompt "x" :path ".."))))))
          (should (equal "image path escapes the project: c:/dot.png"
                         (harmless-image-test-signal
                          (lambda ()
                            (harmless-image-generate
                             session '(:prompt "x" :path "c:/dot.png"))))))
          (should (equal "image generation is not configured"
                         (let ((harmless-image-generate-function nil))
                           (harmless-image-test-signal
                            (lambda ()
                              (harmless-image-generate
                               session '(:prompt "x" :path "dot.png")))))))
          (should (equal "image is not an image"
                         (let ((harmless-image-generate-function
                                (lambda (_session _prompt) "not-an-image")))
                           (harmless-image-test-signal
                            (lambda ()
                              (harmless-image-generate
                               session '(:prompt "x"
                                                 :path "missing/nope.png")))))))
          (should-not (file-exists-p (expand-file-name "dot.png" root)))
          (should-not (file-directory-p (expand-file-name "missing" root)))
          (setf (harmless-session-plan-mode session) t)
          (should (equal
                   "Plan mode is on. Only plan.md may be edited, with harmless-tool-write-plan"
                   (harmless-image-test-signal
                    (lambda ()
                      (harmless-image-generate
                       session '(:prompt "x" :path "/"))))))
          (should-not called)
          (setf (harmless-session-plan-mode session) nil)
          (dolist (dir (list "/" "c:/" "d:/" "c:\\" "C:/" "c:" "\\" "//" "/."))
            (setf (harmless-session-cwd session) dir)
            (should (equal "image directory is the filesystem root"
                           (harmless-image-test-signal
                            (lambda ()
                              (harmless-image-generate
                               session '(:prompt "x" :path "dot.png")))))))
          (dolist (pair before)
            (should (eq (cdr pair) (file-exists-p (car pair)))))
          (should-not called)
          (setf (harmless-session-cwd session) "notes")
          (should (equal "image directory must be absolute"
                         (harmless-image-test-signal
                          (lambda ()
                            (harmless-image-generate
                             session '(:prompt "x" :path "dot.png"))))))
          (should-not (file-exists-p (expand-file-name "dot.png" root)))
          (setf (harmless-session-cwd session) nil)
          (should (equal "This chat is not attached to a project"
                         (harmless-image-test-signal
                          (lambda ()
                            (harmless-image-generate
                             session '(:prompt "x" :path "dot.png"))))))
          (setf (harmless-session-cwd session) "")
          (should (equal "This chat is not attached to a project"
                         (harmless-image-test-signal
                          (lambda ()
                            (harmless-image-generate
                             session '(:prompt "x" :path "dot.png"))))))
          (setf (harmless-session-cwd session) "  ")
          (should (equal "This chat is not attached to a project"
                         (harmless-image-test-signal
                          (lambda ()
                            (harmless-image-generate
                             session '(:prompt "x" :path "dot.png"))))))
          (should-not called)
          (should (equal (harmless-ui--tool-verb "harmless-tool-generate-image") "image"))
          (should (equal (harmless-ui--tool-summary
                          "harmless-tool-generate-image" '(:path "dot.png"))
                         "dot.png"))
          (should (equal (harmless-ui--tool-summary
                          "harmless-tool-generate-image" '(:path nil))
                         ""))
          (should-not (member "harmless-tool-generate-image"
                              (mapcar #'harmless-tool-name
                                      (harmless-tools-for-session
                                       (harmless-image-test-session
                                        "himg-det" nil))))))
      (ignore-errors (delete-directory base t)))))
