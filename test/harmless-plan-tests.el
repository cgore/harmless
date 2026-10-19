;;; harmless-plan-tests.el --- Tests for Harmless plan mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'harmless-openai)
(require 'harmless-plan)
(require 'harmless-tools-fs)
(require 'harmless-skills)

(defun harmless-plan-test-session (dir)
  (let ((provider (harmless-make-openai-compat
                   "local" :host "127.0.0.1:9" :protocol "http"
                   :key "none" :models '("m")))
        (harmless-providers nil)
        (harmless--sessions (make-hash-table :test 'equal))
        (harmless-directory (expand-file-name ".harmless" dir)))
    (setq harmless-providers (list provider))
    (harmless-session-new :cwd dir :provider provider :model "m")))

(ert-deftest harmless-plan-blocks-project-edits ()
  (let* ((dir (make-temp-file "harmless-plan-" t))
         (session (harmless-plan-test-session dir)))
    (harmless-plan-enter session)
    (should (harmless-session-plan-mode session))
    (should-error (harmless-tools-fs--write
                   session '(:path "a.txt" :contents "nope"))
                  :type 'error)
    (should-error (harmless-tools-fs--replace
                   session '(:path "a.txt" :old_string "a" :new_string "b")))
    (with-temp-file (expand-file-name "keep.txt" dir)
      (insert "keep"))
    (let ((err (should-error
                (harmless-tools-fs--apply-patch
                 session '(:hunks ((:path "keep.txt"
                                    :old_string "keep"
                                    :new_string "nope"))))
                :type 'error)))
      (should (equal (error-message-string err)
                     "Plan mode is on. Only plan.md may be edited, with write_plan")))
    (should (equal "keep"
                   (harmless-tools-fs--read session '(:path "keep.txt"))))
    (should (string-match-p "Wrote plan.md"
                            (harmless-plan--tool-write
                             session '(:contents "# Plan\n\nDo the thing.\n"))))
    (should (string-match-p "Do the thing"
                            (harmless-plan--read session)))
    (let ((harmless-plan-decide-function (lambda (_session _plan) 'approve)))
      (should (string-match-p "Approved"
                              (harmless-plan--tool-exit session nil))))
    (should-not (harmless-session-plan-mode session))
    (should (string-match-p "Wrote"
                            (harmless-tools-fs--write
                             session '(:path "a.txt" :contents "ok"))))))

(ert-deftest harmless-plan-revise-keeps-mode ()
  (let* ((dir (make-temp-file "harmless-plan-revise-" t))
         (session (harmless-plan-test-session dir))
         (harmless-plan-decide-function
          (lambda (_session _plan) "Mention the tests.")))
    (harmless-plan-enter session)
    (harmless-plan--tool-write session '(:contents "# Plan\n\nSketch.\n"))
    (should (string-match-p "Mention the tests"
                            (harmless-plan--tool-exit session nil)))
    (should (harmless-session-plan-mode session))
    (should-error (harmless-plan--tool-write session '(:contents "")))))

(ert-deftest harmless-plan-persists ()
  (let* ((dir (make-temp-file "harmless-plan-save-" t))
         (session (harmless-plan-test-session dir))
         (id (harmless-session-id session)))
    (harmless-plan-enter session)
    (setq harmless--sessions (make-hash-table :test 'equal))
    (let ((loaded (harmless-session-load (harmless-session-directory session))))
      (should (string= id (harmless-session-id loaded)))
      (should (harmless-session-plan-mode loaded)))))

(ert-deftest harmless-plan-prompt-when-active ()
  (let* ((dir (make-temp-file "harmless-plan-prompt-" t))
         (session (harmless-plan-test-session dir)))
    (harmless-plan-enter session)
    (let ((messages (harmless-context-append
                     (harmless-context-messages dir nil dir)
                     (when (harmless-session-plan-mode session)
                       (harmless-plan-instructions)))))
      (should (string-match-p "write_plan"
                              (plist-get (car messages) :content)))
      (should (string-match-p "exit_plan_mode"
                              (plist-get (car messages) :content)))
      (should (string-match-p "apply_patch"
                              (plist-get (car messages) :content)))
      (should (string-match-p "shell command that can write"
                              (plist-get (car messages) :content))))))

(ert-deftest harmless-plan-file-stays-out-of-filesystem-root ()
  (let* ((dir (make-temp-file "harmless-plan-root-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (session (harmless-plan-test-session dir))
         (default-directory "/"))
    (harmless-plan-enter session)
    (setf (harmless-session-directory session) "/")
    (should (string-match-p "Wrote plan.md"
                            (harmless-plan--tool-write
                             session '(:contents "# Plan\n\nStay put.\n"))))
    (let ((file (harmless-plan-file session)))
      (should (string-prefix-p (file-name-as-directory harmless-directory) file))
      (should (file-exists-p file))
      (should-not (equal file "/plan.md"))
      (should (string-match-p "Stay put"
                              (with-temp-buffer
                                (insert-file-contents file)
                                (buffer-string)))))
    (should-not (file-exists-p "/plan.md"))
    (setf (harmless-session-directory session) nil)
    (let ((harmless-directory "/"))
      (let ((err (should-error (harmless-plan-file session) :type 'error)))
        (should (string-match-p "Refusing to store Harmless data"
                                (error-message-string err)))))
    (should-not (file-exists-p "/plan.md"))))

(defun harmless-plan-test-error (message form)
  "Assert FORM signals MESSAGE."
  (let ((err (should-error (funcall form) :type 'error)))
    (should (equal message (error-message-string err)))))

(ert-deftest harmless-plan-comment-refuses-bad-input ()
  (let ((plan "one\ntwo\nthree\n")
        (calls 0))
    (cl-letf (((symbol-function 'expand-file-name)
               (lambda (&rest _)
                 (setq calls (1+ calls))
                 "/"))
              ((symbol-function 'file-truename)
               (lambda (&rest _)
                 (setq calls (1+ calls))
                 "/"))
              ((symbol-function 'write-region)
               (lambda (&rest _)
                 (setq calls (1+ calls)))))
      (dolist (case `((nil 1 nil "hi" "plan comment line is not a line")
                      ("" 1 nil "hi" "plan comment line is not a line")
                      (12 1 nil "hi" "plan must be a string")
                      (,plan 0 nil "hi" "plan comment line is not a line")
                      (,plan -1 nil "hi" "plan comment line is not a line")
                      (,plan 4 nil "hi" "plan comment line is not a line")
                      (,plan "2" nil "hi" "plan comment line is not a line")
                      (,plan 1.5 nil "hi" "plan comment line is not a line")
                      (,plan nil nil "hi" "plan comment line is not a line")
                      (,plan 3 1 "hi" "plan comment range is backwards")
                      (,plan 1 nil nil "plan comment is empty")
                      (,plan 1 nil "" "plan comment is empty")
                      (,plan 1 nil "  " "plan comment is empty")
                      (,plan 1 nil 12 "plan comment must be a string")))
        (harmless-plan-test-error
         (nth 4 case)
         (lambda ()
           (harmless-plan-comment
            (nth 0 case) (nth 1 case) (nth 2 case) (nth 3 case)))))
      (should (equal (harmless-plan-comment plan 1 nil "/")
                     '(:start 1 :end 1 :text "/")))
      (should (equal (harmless-plan-comment plan 2 nil "c:/")
                     '(:start 2 :end 2 :text "c:/")))
      (should (equal (harmless-plan-comment plan 2 3 "  span  ")
                     '(:start 2 :end 3 :text "span")))
      (should (equal (harmless-plan-format-comments
                      (list (harmless-plan-comment plan 1 nil "/")
                            (harmless-plan-comment plan 2 3 "span")))
                     "Line 1: /\nLines 2-3: span"))
      (should (equal (harmless-plan-review-notes nil "  keep  ") "keep"))
      (should (equal (harmless-plan-review-notes nil "/") "/"))
      (should (equal (harmless-plan-review-notes nil "c:/") "c:/"))
      (should (equal (harmless-plan-review-notes nil "") ""))
      (should (equal (harmless-plan-review-notes
                      (list (harmless-plan-comment plan 1 nil "keep"))
                      "")
                     "Line 1: keep"))
      (harmless-plan-test-error
       "plan notes must be a string"
       (lambda () (harmless-plan-review-notes nil 12)))
      (should (= calls 0)))))

(ert-deftest harmless-plan-review-comments-on-a-line-or-range ()
  (let ((plan "one\ntwo\nthree\n")
        (keys '(?c ?s))
        (prompt nil))
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (question _chars)
                 (setq prompt question)
                 (pop keys)))
              ((symbol-function 'write-region)
               (lambda (&rest _) (error "plan comment wrote a file"))))
      (with-temp-buffer
        (insert plan)
        (goto-char (point-min))
        (forward-line 1)
        (let ((harmless-plan-read-comment-function (lambda () "rename it"))
              (harmless-plan-read-notes-function (lambda () "also tests")))
          (should (equal (harmless-plan-review plan)
                         "Line 2: rename it\nalso tests"))))
      (should (equal prompt
                     "Plan: a approve and build, c comment, s request changes, q quit ")))
    (setq keys '(?c ?a))
    (cl-letf (((symbol-function 'read-char-choice)
               (lambda (_question _chars) (pop keys))))
      (with-temp-buffer
        (insert plan)
        (goto-char (point-min))
        (let ((transient-mark-mode t)
              (harmless-plan-read-comment-function (lambda () "span"))
              (harmless-plan-read-notes-function
               (lambda () (error "notes were read"))))
          (push-mark (point) t t)
          (forward-line 2)
          (setq mark-active t)
          (should (equal (harmless-plan-comment-at-point plan)
                         '(:start 1 :end 2 :text "span")))
          (goto-char (point-max))
          (setq mark-active nil)
          (harmless-plan-test-error
           "plan comment line is not a line"
           (lambda () (harmless-plan-comment-at-point plan)))
          (goto-char (point-min))
          (setq mark-active nil)
          (should (eq (harmless-plan-review plan) 'approve)))))))

(ert-deftest harmless-plan-comments-do-not-change-the-file ()
  (let* ((dir (make-temp-file "harmless-plan-comment-" t))
         (harmless-directory (expand-file-name ".harmless" dir))
         (harmless--sessions (make-hash-table :test 'equal))
         (session (harmless-plan-test-session dir))
         (plan "# Plan\n\nKeep this.\nSplit this.\n")
         (file nil)
         (before nil))
    (harmless-plan-enter session)
    (harmless-plan--tool-write session (list :contents plan))
    (setq file (harmless-plan-file session))
    (setq before (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string)))
    (should (equal before plan))
    (should (file-name-absolute-p file))
    (should-not (harmless-filesystem-root-p file))
    (should-not (harmless-filesystem-root-p (file-name-directory file)))
    (should (equal file (expand-file-name
                         "plan.md" (harmless-session-dir session))))
    (should (harmless-directory-strictly-under-p
             (harmless-session-dir session) harmless-directory))
    (should (string-prefix-p
             (file-name-as-directory
              (file-truename (expand-file-name ".harmless" dir)))
             (file-truename file)))
    (let ((harmless-plan-decide-function
           (lambda (_session text)
             (harmless-plan-review-notes
              (list (harmless-plan-comment text 1 1 "keep the name")
                    (harmless-plan-comment text 3 4 "split this"))
              "/"))))
      (should (equal (harmless-plan--tool-exit session nil)
                     (concat "Revise the plan. Notes: "
                             "Line 1: keep the name\n"
                             "Lines 3-4: split this\n"
                             "/"))))
    (should (harmless-session-plan-mode session))
    (should (equal before (with-temp-buffer
                            (insert-file-contents file)
                            (buffer-string))))
    (should-not (file-exists-p "/plan.md"))))

(provide 'harmless-plan-tests)
