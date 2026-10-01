;;; harmless-ask-tests.el --- Tests for the ask_user tool -*- lexical-binding: t; -*-

(require 'ert)
(require 'harmless)
(require 'harmless-ui)

(defun harmless-ask-test-refuse (args text)
  "Call ask_user with ARGS and require error TEXT before any prompt."
  (let ((called nil))
    (let ((harmless-ask-decide-function
           (lambda (&rest _) (setq called t) "yes")))
      (let ((err (should-error (harmless-ask--tool nil args) :type 'error)))
        (should (equal (error-message-string err) text))
        (should-not called)))))

(ert-deftest harmless-ask-returns-the-chosen-text ()
  (let ((seen nil)
        (default-directory "/")
        (existed (file-exists-p "/ask_user")))
    (let ((harmless-ask-decide-function
           (lambda (_session question choices)
             (setq seen (list question choices))
             (car choices))))
      (should (equal (harmless-ask--tool
                      nil
                      '(:question "  Path?  " :choices (" lisp/ " "src")))
                     "The user chose: lisp/"))
      (should (equal seen '("Path?" ("lisp/" "src"))))
      (should (equal (harmless-ask--tool
                      nil
                      '(:question "Root?" :choices ("/")))
                     "The user chose: /"))
      (should (equal (harmless-ask--tool
                      nil
                      '(:question "Root?" :choices ["/"]))
                     "The user chose: /"))
      (should (eq existed (file-exists-p "/ask_user"))))))

(ert-deftest harmless-ask-accepts-json-arguments ()
  (let ((harmless-ask-decide-function
         (lambda (_session _question choices) (cadr choices)))
        (args (harmless-tool-parse-args
               "{\"question\":\"Ship it?\",\"choices\":[\"yes\",\"no\"]}")))
    (should (equal (harmless-ask--tool nil args) "The user chose: no"))
    (should (equal (harmless-tool-call (harmless-tool-by-name "ask_user")
                                      nil args)
                   "The user chose: no"))))

(ert-deftest harmless-ask-refuses-a-bad-question-or-choice-list ()
  (dolist (args (list nil
                      '()
                      '(:choices ("yes"))
                      '(:question nil :choices ("yes"))
                      '(:question "" :choices ("yes"))
                      '(:question "   " :choices ("yes"))
                      '(:question :false :choices ("yes"))
                      '(:question 1 :choices ("yes"))))
    (harmless-ask-test-refuse args "question is required"))
  (dolist (args (list '(:question "Ship it?")
                      '(:question "Ship it?" :choices nil)
                      '(:question "Ship it?" :choices [])
                      '(:question "Ship it?" :choices "yes")
                      '(:question "Ship it?" :choices :false)))
    (harmless-ask-test-refuse args "choices are required"))
  (let ((args (harmless-tool-parse-args
               "{\"question\":\"Ship it?\",\"choices\":[]}")))
    (harmless-ask-test-refuse args "choices are required"))
  (dolist (args (list '(:question "Ship it?" :choices (""))
                      '(:question "Ship it?" :choices ("   "))
                      '(:question "Ship it?" :choices (1))
                      '(:question "Ship it?" :choices (nil))
                      '(:question "Ship it?" :choices ("yes" ""))))
    (harmless-ask-test-refuse args "each choice must be a non-empty string"))
  (harmless-ask-test-refuse
   '(:question "Ship it?" :choices ("yes\nno"))
   "each choice must be one line")
  (harmless-ask-test-refuse
   '(:question "Ship it?" :choices ("yes" " yes "))
   "choices must be unique")
  (harmless-ask-test-refuse
   '(:question "Ship it?" :choices ("/" "/"))
   "choices must be unique"))

(ert-deftest harmless-ask-refuses-an-answer-outside-the-choices ()
  (dolist (answer (list nil "" "maybe" "/tmp"))
    (let ((harmless-ask-decide-function (lambda (&rest _) answer)))
      (let ((err (should-error
                  (harmless-ask--tool nil '(:question "Ship it?"
                                             :choices ("yes" "no")))
                  :type 'error)))
        (should (equal (error-message-string err)
                       "The question was not answered"))))))

(ert-deftest harmless-ask-reports-a-cancelled-question ()
  (let ((harmless-ask-decide-function
         (lambda (&rest _) (signal 'quit nil))))
    (should (equal (harmless-ask--tool nil '(:question "Ship it?"
                                              :choices ("yes" "no")))
                   "The user cancelled the question."))
    (should (equal (harmless-tool-call (harmless-tool-by-name "ask_user")
                                      nil
                                      '(:question "Ship it?"
                                        :choices ("yes" "no")))
                   "The user cancelled the question."))))

(ert-deftest harmless-ask-default-refuses-batch-emacs ()
  (let ((noninteractive t))
    (let ((err (should-error
                (harmless-ask-decide-default nil "Ship it?" '("yes" "no"))
                :type 'error)))
      (should (equal (error-message-string err)
                     "Cannot ask a question in batch Emacs")))))

(ert-deftest harmless-ask-tool-is-a-read-and-shows-the-question ()
  (let ((tool (harmless-tool-by-name "ask_user")))
    (should tool)
    (should (eq (harmless-tool-class tool) 'read))
    (should (equal (plist-get (harmless-tool-schema tool) :required)
                   ["question" "choices"]))
    (should (eq (harmless-tool-fn tool) #'harmless-ask--tool)))
  (should (equal (harmless-ui--tool-verb "ask_user") "ask"))
  (should (equal (harmless-ui--tool-summary
                  "ask_user"
                  '(:question "Ship it?" :choices ("yes" "no")))
                 "Ship it?"))
  (should-not (harmless-ui--tool-groupable-p "ask_user"))
  (let* ((encoded (harmless-json-text (harmless-tool-schema
                                       (harmless-tool-by-name "ask_user"))))
         (schema (harmless-json-decode encoded)))
    (should (equal (plist-get schema :type) "object"))
    (should (equal (plist-get (plist-get (plist-get schema :properties) :choices)
                              :type)
                   "array"))))

(provide 'harmless-ask-tests)
