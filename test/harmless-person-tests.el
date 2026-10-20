;;; harmless-person-tests.el --- Tests for a person address -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(defun harmless-person-test-signal (fn)
  "Return the error text from FN."
  (error-message-string (should-error (funcall fn) :type 'error)))

(defun harmless-person-test-write (path text)
  "Write TEXT to PATH with Unix newlines and return PATH."
  (make-directory (file-name-directory path) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil path nil 'silent))
  path)

(defun harmless-person-test-read (path)
  "Return PATH as a Unix-newline string."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (buffer-string)))

(defun harmless-person-test-texts (row)
  "Return ROW's history texts."
  (mapcar (lambda (item) (plist-get item :text))
          (plist-get row :history)))

(defun harmless-person-test-people (pairs)
  "Return people JSON for (NAME . CHANNEL) PAIRS."
  (concat
   "["
   (mapconcat
    (lambda (pair)
      (concat "{\"name\":" (harmless-json-text (car pair))
              ",\"connector\":\"slack\",\"channel\":"
              (harmless-json-text (cdr pair)) "}"))
    pairs
    ",")
   "]\n"))

(defun harmless-person-test-roots ()
  "Return existence of people files at filesystem roots."
  (list (file-exists-p "/desk.people.json")
        (file-exists-p "/teams")
        (file-exists-p "c:/desk.people.json")
        (file-exists-p "d:/desk.people.json")
        (file-exists-p "c:/teams")
        (file-exists-p "d:/teams")
        (file-exists-p "/sessions")
        (file-exists-p "c:/sessions")
        (file-exists-p "d:/sessions")))

(ert-deftest harmless-person-asks-and-keeps-the-receipt ()
  (let* ((parent (make-temp-file "harmless-person-" t))
         (elsewhere (expand-file-name "elsewhere" parent))
         (local (expand-file-name "local" parent))
         (harmless-directory (file-name-as-directory local))
         (default-directory (file-name-as-directory elsewhere))
         (roots (harmless-person-test-roots))
         (home-work (file-exists-p (expand-file-name "work" "~")))
         (timers (copy-sequence timer-list))
         (procs (copy-sequence (process-list)))
         (sched (hash-table-count harmless-schedule--timers))
         (old-token (getenv "SLACK_BOT_TOKEN"))
         (people (expand-file-name "teams/desk.people.json" local))
         (rig (expand-file-name "teams/desk.json" local))
         (teams (expand-file-name "teams" local))
         (row nil))
    (unwind-protect
        (progn
          (make-directory elsewhere t)
          (make-directory local t)
          (setenv "SLACK_BOT_TOKEN" nil)
          (should (commandp #'harmless-person-add))
          (should (commandp #'harmless-person-list))
          (should (commandp #'harmless-person-show))
          (should (commandp #'harmless-person-remove))
          (should (commandp #'harmless-person-ask))
          (should-not (harmless-person-list "desk"))
          (should-not (file-exists-p people))
          (should-not (file-directory-p teams))
          (dolist (empty (list nil "" "   " "\n"))
            (should (equal (harmless-person-test-signal
                            (lambda ()
                              (harmless-person-add "desk" empty "ops")))
                           "person name is empty")))
          (should (equal (harmless-person-test-signal
                          (lambda ()
                            (harmless-person-add "desk" 42 "ops")))
                         "person name must be a string"))
          (dolist (bad (list "/" "c:/" "~" "Ada" "ada-l" "../ada"
                             "ada/bo" "d:ada" "ada/" "~ada"))
            (should (equal (harmless-person-test-signal
                            (lambda ()
                              (harmless-person-add "desk" bad "ops")))
                           "person name is not a name")))
          (dolist (empty (list nil "" "   " "#" "  #  "))
            (should (equal (harmless-person-test-signal
                            (lambda ()
                              (harmless-person-add "desk" "ada" empty)))
                           "person channel is empty")))
          (should (equal (harmless-person-test-signal
                          (lambda ()
                            (harmless-person-add "desk" "ada" 42)))
                         "person channel must be a string"))
          (dolist (bad (list "/" "c:/ops" "~" "xoxb-secret" "has space"
                             "token=abc" "ops/" "../ops" "\\ops"))
            (should (equal (harmless-person-test-signal
                            (lambda ()
                              (harmless-person-add "desk" "ada" bad)))
                           "person channel is not a channel")))
          (should-not (file-exists-p
                       (expand-file-name "ada" elsewhere)))
          (should (equal home-work
                         (file-exists-p (expand-file-name "work" "~"))))
          (should (equal (harmless-person-test-signal
                          (lambda ()
                            (harmless-person-add "desk" "ada" "#ops")))
                         "team rig is missing"))
          (should-not (file-exists-p people))
          (should-not (file-directory-p teams))
          (dolist (root (list "/" "c:/" "c:" "d:/" "C:/"
                              "  /  " "  c:/  "))
            (let ((harmless-directory root))
              (should (equal (harmless-person-test-signal
                              (lambda ()
                                (harmless-person-list "desk")))
                             (format "Refusing to store a person at %s"
                                     (string-trim root))))))
          (harmless-team-add-seat "desk" "core" "lead" "builds")
          (should (file-regular-p rig))
          (should-not (harmless-person-list "desk"))
          (should-not (file-exists-p people))
          (should (equal "no person named ada"
                         (harmless-person-test-signal
                          (lambda ()
                            (harmless-person-show "desk" "ada")))))
          (should (equal "no person named ada"
                         (harmless-person-test-signal
                          (lambda ()
                            (harmless-person-remove "desk" "ada")))))
          (should (equal "no person named ada"
                         (harmless-person-test-signal
                          (lambda ()
                            (harmless-person-ask "desk" "ada" "Hi")))))
          (should-not (file-exists-p people))
          (should-not (harmless-team-rows "desk"))
          (dolist (text (list
                         "nope\n"
                         (concat "[{\"name\":\"ada\",\"connector\":"
                                 "\"slack\",\"channel\":\"ops\","
                                 "\"token\":\"xoxb-stored\"}]\n")
                         (concat "[{\"name\":\"ada\",\"connector\":"
                                 "\"slack\",\"channel\":\"ops\","
                                 "\"secret\":\"secret-value\"}]\n")
                         (concat "[{\"name\":\"ada\",\"connector\":"
                                 "\"slack\",\"channel\":\"ops\","
                                 "\"password\":\"password-value\"}]\n")
                         (concat "[{\"name\":\"ada\",\"connector\":"
                                 "\"email\",\"channel\":\"ops\"}]\n")
                         (concat "[{\"name\":\"ada\",\"connector\":"
                                 "\"slack\",\"channel\":\"ops\"},"
                                 "{\"name\":\"ada\",\"connector\":"
                                 "\"slack\",\"channel\":\"ops\"}]\n")
                         "{\"name\":\"ada\"}\n"))
            (harmless-person-test-write people text)
            (should (equal "people are not valid"
                           (harmless-person-test-signal
                            (lambda ()
                              (harmless-person-list "desk")))))
            (should (equal text
                           (harmless-person-test-read people))))
          (should-not (string-search "xoxb-stored"
                                     (harmless-person-test-read rig)))
          (should-not (string-search "secret-value"
                                     (harmless-person-test-read rig)))
          (should-not (string-search "password-value"
                                     (harmless-person-test-read rig)))
          (delete-file people)
          (make-directory people)
          (should (equal "people are not valid"
                         (harmless-person-test-signal
                          (lambda () (harmless-person-list "desk")))))
          (delete-directory people)
          (should-not (file-exists-p people))
          (should (equal (list :name "bo"
                               :address "bo@slack"
                               :connector "slack"
                               :channel "C012")
                         (harmless-person-add "desk" "  bo  " "  C012  ")))
          (should (equal (list :name "ada"
                               :address "ada@slack"
                               :connector "slack"
                               :channel "ops")
                         (harmless-person-add "desk" "ada" "#ops")))
          (should (equal '("ada" "bo") (harmless-person-list "desk")))
          (should (equal (list :name "ada"
                               :address "ada@slack"
                               :connector "slack"
                               :channel "ops")
                         (harmless-person-show "desk" "  ada  ")))
          (should (file-name-absolute-p people))
          (should (file-regular-p people))
          (should (harmless-directory-strictly-under-p people teams))
          (should (harmless-directory-strictly-under-p people local))
          (should-not (harmless-filesystem-root-p people))
          (should-not (equal (file-truename people)
                             (file-truename "/")))
          (should (equal (harmless-person-test-people
                          '(("ada" . "ops") ("bo" . "C012")))
                         (harmless-person-test-read people)))
          (let ((before (harmless-person-test-read people)))
            (should (equal "person is already registered: ada"
                           (harmless-person-test-signal
                            (lambda ()
                              (harmless-person-add "desk" "ada" "ops")))))
            (should (equal before
                           (harmless-person-test-read people))))
          (should-not (string-search "ada"
                                     (harmless-person-test-read rig)))
          (should-not (string-search "slack"
                                     (harmless-person-test-read rig)))
          (should-not (string-search "ops"
                                     (harmless-person-test-read rig)))
          (dolist (empty (list nil "" "   " "\n"))
            (should (equal (harmless-person-test-signal
                            (lambda ()
                              (harmless-person-ask "desk" "ada" empty)))
                           "person question is empty")))
          (should (equal (harmless-person-test-signal
                          (lambda ()
                            (harmless-person-ask "desk" "ada" 42)))
                         "person question must be a string"))
          (should (equal "no person named cy"
                         (harmless-person-test-signal
                          (lambda ()
                            (harmless-person-ask "desk" "cy" "Hi")))))
          (should-not (harmless-team-rows "desk"))
          (setenv "SLACK_BOT_TOKEN" "xoxb-test-token")
          (setq row (harmless-person-ask "desk" "ada" "Ship it?"))
          (should (equal "1" (plist-get row :id)))
          (should (equal "ada@slack" (plist-get row :seat)))
          (should (equal "Ship it?" (plist-get row :body)))
          (should (equal "open" (plist-get row :state)))
          (should-not (plist-get row :reason))
          (should-not (plist-get row :wake))
          (should-not (plist-get row :summary))
          (should-not (plist-get row :evidence))
          (should (equal '("opened"
                           "asked ada@slack on slack channel ops"
                           "slack was not posted")
                         (harmless-person-test-texts row)))
          (should-not (string-search
                       "xoxb-test-token"
                       (harmless-person-test-read rig)))
          (should-not (string-search
                       "xoxb-test-token"
                       (harmless-person-test-read people)))
          (let ((seen nil))
            (let ((harmless-person-slack-function
                   (lambda (token channel text)
                     (setq seen
                           (append seen
                                   (list (list token channel text)))))))
              (setq row (harmless-person-ask "desk" "ada" "Ship it?"))
              (should (equal '(("xoxb-test-token" "ops" "Ship it?"))
                             seen))
              (should (equal "2" (plist-get row :id)))
              (should (equal '("opened"
                               "asked ada@slack on slack channel ops"
                               "delivered to slack")
                             (harmless-person-test-texts row)))))
          (should-not (string-search
                       "xoxb-test-token"
                       (harmless-person-test-read rig)))
          (should-not (string-search
                       "xoxb-test-token"
                       (harmless-person-test-read people)))
          (let ((calls 0))
            (let ((harmless-person-slack-function
                   (lambda (_token _channel _text)
                     (setq calls (1+ calls)))))
              (setenv "SLACK_BOT_TOKEN" nil)
              (setq row (harmless-person-ask "desk" "ada" "Again?"))
              (should (eq 0 calls))
              (should (equal '("opened"
                               "asked ada@slack on slack channel ops"
                               "slack token is missing")
                             (harmless-person-test-texts row)))
              (setenv "SLACK_BOT_TOKEN" "   ")
              (setq row (harmless-person-ask "desk" "ada" "Blank?"))
              (should (eq 0 calls))
              (should (equal "slack token is missing"
                             (car (last (harmless-person-test-texts
                                         row)))))))
          (setenv "SLACK_BOT_TOKEN" "xoxb-test-token")
          (let ((calls 0))
            (let ((harmless-person-slack-function
                   (lambda (_token _channel _text)
                     (setq calls (1+ calls))
                     (error "down"))))
              (setq row (harmless-person-ask
                         "desk" "ada" "Once more?"))
              (should (eq 1 calls))
              (should (equal "slack was not posted"
                             (car (last (harmless-person-test-texts
                                         row)))))))
          (should-not (string-search "down"
                                     (harmless-person-test-read rig)))
          (should-not (string-search
                       "xoxb-test-token"
                       (harmless-person-test-read rig)))
          (should-not (string-search
                       "xoxb-test-token"
                       (harmless-person-test-read people)))
          (setq row (harmless-team-row-claim "desk" "1"))
          (should (equal "held" (plist-get row :state)))
          (should (equal '("opened"
                           "asked ada@slack on slack channel ops"
                           "slack was not posted"
                           "claimed")
                         (harmless-person-test-texts row)))
          (should (equal '("ada@slack" "ada@slack" "ada@slack"
                           "ada@slack" "ada@slack")
                         (mapcar (lambda (item)
                                   (plist-get item :seat))
                                 (harmless-team-rows "desk"))))
          (should-not (harmless-team-rows "desk" "core.lead"))
          (should (equal "ada" (harmless-person-remove "desk" "ada")))
          (should (equal '("bo") (harmless-person-list "desk")))
          (should (equal (harmless-person-test-people
                          '(("bo" . "C012")))
                         (harmless-person-test-read people)))
          (should (equal "ada@slack"
                         (plist-get (harmless-team-row-read "desk" "1")
                                    :seat)))
          (should (equal "no person named ada"
                         (harmless-person-test-signal
                          (lambda ()
                            (harmless-person-show "desk" "ada")))))
          (should (equal "bo" (harmless-person-remove "desk" "bo")))
          (should-not (harmless-person-list "desk"))
          (should (equal "[]\n" (harmless-person-test-read people)))
          (should (equal "no person named bo"
                         (harmless-person-test-signal
                          (lambda ()
                            (harmless-person-remove "desk" "bo")))))
          (should (equal "[]\n" (harmless-person-test-read people)))
          (should (equal "ada@slack"
                         (plist-get (harmless-team-row-read "desk" "1")
                                    :seat)))
          (should-not (file-exists-p
                       (expand-file-name "desk.people.json" elsewhere)))
          (should-not (file-directory-p
                       (expand-file-name "teams" elsewhere)))
          (should (equal roots (harmless-person-test-roots)))
          (should (equal home-work
                         (file-exists-p (expand-file-name "work" "~"))))
          (should (equal timers (copy-sequence timer-list)))
          (should (equal procs (copy-sequence (process-list))))
          (should (equal sched
                         (hash-table-count harmless-schedule--timers))))
      (setenv "SLACK_BOT_TOKEN" old-token)
      (ignore-errors (delete-directory parent t)))))
