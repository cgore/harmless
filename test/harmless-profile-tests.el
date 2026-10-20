;;; harmless-profile-tests.el --- Tests for auth profiles -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(defconst harmless-profile-test-id
  "22222222-3333-4444-5555-666666666666"
  "One session id an occupied seat can adopt.")

(defconst harmless-profile-test-token
  "sk-live-profile-token"
  "A token that must never be written.")

(defun harmless-profile-test-signal (fn)
  "Return the error text from FN."
  (error-message-string (should-error (funcall fn) :type 'error)))

(defun harmless-profile-test-write (path text)
  "Write TEXT to PATH with Unix newlines and return PATH."
  (make-directory (file-name-directory path) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil path nil 'silent))
  path)

(defun harmless-profile-test-read (path)
  "Return PATH as a Unix-newline string."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (buffer-string)))

(defun harmless-profile-test-profiles (rows)
  "Return profile JSON for ROWS of (NAME RUNTIME ENV)."
  (concat
   "["
   (mapconcat
    (lambda (row)
      (concat "{\"name\":" (harmless-json-text (nth 0 row))
              ",\"runtime\":" (harmless-json-text (nth 1 row))
              ",\"env\":" (harmless-json-text (nth 2 row))
              "}"))
    rows
    ",")
   "]\n"))

(defun harmless-profile-test-roots ()
  "Return existence of profile files at filesystem roots."
  (list (file-exists-p "/profiles.json")
        (file-exists-p "c:/profiles.json")
        (file-exists-p "d:/profiles.json")
        (file-exists-p "/profiles")
        (file-exists-p "c:/profiles")
        (file-exists-p "d:/profiles")
        (file-exists-p "/teams")
        (file-exists-p "c:/teams")
        (file-exists-p "d:/teams")
        (file-exists-p "/sessions")
        (file-exists-p "c:/sessions")
        (file-exists-p "d:/sessions")))

(ert-deftest harmless-profile-switches-and-keeps-the-conversation ()
  (let* ((parent (make-temp-file "harmless-profile-" t))
         (elsewhere (expand-file-name "elsewhere" parent))
         (local (expand-file-name "local" parent))
         (claude (expand-file-name "claude-session" parent))
         (harmless-directory (file-name-as-directory local))
         (default-directory (file-name-as-directory elsewhere))
         (roots (harmless-profile-test-roots))
         (home-work (file-exists-p (expand-file-name "work" "~")))
         (home-profiles (file-exists-p
                         (expand-file-name "profiles.json" "~")))
         (timers (copy-sequence timer-list))
         (procs (copy-sequence (process-list)))
         (sched (hash-table-count harmless-schedule--timers))
         (old-work (getenv "HARMLESS_WORK_TOKEN"))
         (old-other (getenv "HARMLESS_OTHER_TOKEN"))
         (old-claude (getenv "HARMLESS_CLAUDE_TOKEN"))
         (profiles (expand-file-name "profiles.json" local))
         (rig (expand-file-name "teams/desk.json" local))
         (hand (expand-file-name
                (concat "sessions/hand/" harmless-profile-test-id)
                local))
         (transcript (expand-file-name "messages.jsonl" hand))
         (talk "{\"role\":\"user\",\"text\":\"keep this\"}\n")
         (saved nil)
         (seat nil))
    (unwind-protect
        (progn
          (make-directory elsewhere t)
          (make-directory local t)
          (make-directory claude t)
          (setenv "HARMLESS_WORK_TOKEN" nil)
          (setenv "HARMLESS_OTHER_TOKEN" nil)
          (setenv "HARMLESS_CLAUDE_TOKEN" nil)
          (should (commandp #'harmless-profile-add))
          (should (commandp #'harmless-profile-list))
          (should (commandp #'harmless-profile-show))
          (should (commandp #'harmless-profile-remove))
          (should (commandp #'harmless-profile-switch))
          (should (fboundp #'harmless-profile-token))
          (should-not (commandp #'harmless-profile-token))
          (should-not (harmless-profile-list))
          (should-not (file-exists-p profiles))
          (dolist (empty (list nil "" "   " "\n"))
            (should (equal (harmless-profile-test-signal
                            (lambda ()
                              (harmless-profile-add
                               empty "harmless" "HARMLESS_WORK_TOKEN")))
                           "profile name is empty")))
          (should (equal (harmless-profile-test-signal
                          (lambda ()
                            (harmless-profile-add
                             42 "harmless" "HARMLESS_WORK_TOKEN")))
                         "profile name must be a string"))
          (dolist (bad (list "/" "c:/" "~" "Work" "ada-l" "../ada"
                             "ada/bo" "d:ada" "ada/" "~ada" "c:work"))
            (should (equal (harmless-profile-test-signal
                            (lambda ()
                              (harmless-profile-add
                               bad "harmless" "HARMLESS_WORK_TOKEN")))
                           "profile name is not a name")))
          (dolist (empty (list nil "" "   "))
            (should (equal (harmless-profile-test-signal
                            (lambda ()
                              (harmless-profile-add
                               "workacct" empty "HARMLESS_WORK_TOKEN")))
                           "profile runtime is empty")))
          (should (equal (harmless-profile-test-signal
                          (lambda ()
                            (harmless-profile-add
                             "workacct" 42 "HARMLESS_WORK_TOKEN")))
                         "profile runtime must be a string"))
          (dolist (bad (list "shell" "Harmless" "notes" "/"))
            (should (equal (harmless-profile-test-signal
                            (lambda ()
                              (harmless-profile-add
                               "workacct" bad "HARMLESS_WORK_TOKEN")))
                           "profile runtime is not a runtime")))
          (dolist (empty (list nil "" "   "))
            (should (equal (harmless-profile-test-signal
                            (lambda ()
                              (harmless-profile-add
                               "workacct" "harmless" empty)))
                           "profile env is empty")))
          (should (equal (harmless-profile-test-signal
                          (lambda ()
                            (harmless-profile-add
                             "workacct" "harmless" 42)))
                         "profile env must be a string"))
          (dolist (bad (list "/" "c:/" "~" "harmless_work_token"
                             "sk-live-profile-token" "xoxb-secret"
                             "token=abc" "HAS SPACE" "env/name"))
            (should (equal (harmless-profile-test-signal
                            (lambda ()
                              (harmless-profile-add
                               "workacct" "harmless" bad)))
                           "profile env is not a name")))
          (should-not (file-exists-p profiles))
          (should-not (file-exists-p
                       (expand-file-name "profiles.json" elsewhere)))
          (should (equal home-work
                         (file-exists-p (expand-file-name "work" "~"))))
          (dolist (root (list "/" "c:/" "c:" "d:/" "C:/"
                              "  /  " "  c:/  "))
            (let ((harmless-directory root))
              (should (equal (harmless-profile-test-signal
                              (lambda () (harmless-profile-list)))
                             (format "Refusing to store a profile at %s"
                                     (string-trim root))))))
          (let ((harmless-directory "relative"))
            (should (equal (harmless-profile-test-signal
                            (lambda () (harmless-profile-list)))
                           (concat "Harmless data directory must be "
                                   "absolute, not \"relative\""))))
          (should-not (file-exists-p
                       (expand-file-name "profiles.json" elsewhere)))
          (should-not (file-exists-p profiles))
          (make-directory profiles)
          (should (equal "profiles are not valid"
                         (harmless-profile-test-signal
                          (lambda () (harmless-profile-list)))))
          (delete-directory profiles)
          (should-not (file-exists-p profiles))
          (should (equal (list :name "workacct"
                               :runtime "harmless"
                               :env "HARMLESS_WORK_TOKEN")
                         (harmless-profile-add
                          "  workacct  " "  harmless  "
                          "  HARMLESS_WORK_TOKEN  ")))
          (should (equal (list :name "otheracct"
                               :runtime "harmless"
                               :env "HARMLESS_OTHER_TOKEN")
                         (harmless-profile-add
                          "otheracct" "harmless" "HARMLESS_OTHER_TOKEN")))
          (should (equal (list :name "claudeacct"
                               :runtime "claude"
                               :env "HARMLESS_CLAUDE_TOKEN")
                         (harmless-profile-add
                          "claudeacct" "claude" "HARMLESS_CLAUDE_TOKEN")))
          (should (equal '("claudeacct" "otheracct" "workacct")
                         (harmless-profile-list)))
          (let ((harmless-directory (directory-file-name local)))
            (should (equal '("claudeacct" "otheracct" "workacct")
                           (harmless-profile-list))))
          (should (equal (list :name "workacct"
                               :runtime "harmless"
                               :env "HARMLESS_WORK_TOKEN"
                               :set nil)
                         (harmless-profile-show "  workacct  ")))
          (should (file-name-absolute-p profiles))
          (should (file-regular-p profiles))
          (should (harmless-directory-strictly-under-p profiles local))
          (should-not (harmless-filesystem-root-p profiles))
          (should-not (equal (file-truename profiles)
                             (file-truename "/")))
          (should (equal (harmless-profile-test-profiles
                          '(("claudeacct" "claude"
                             "HARMLESS_CLAUDE_TOKEN")
                            ("otheracct" "harmless"
                             "HARMLESS_OTHER_TOKEN")
                            ("workacct" "harmless"
                             "HARMLESS_WORK_TOKEN")))
                         (harmless-profile-test-read profiles)))
          (let ((before (harmless-profile-test-read profiles)))
            (should (equal "profile is already registered: workacct"
                           (harmless-profile-test-signal
                            (lambda ()
                              (harmless-profile-add
                               "workacct" "harmless"
                               "HARMLESS_WORK_TOKEN")))))
            (should (equal before
                           (harmless-profile-test-read profiles))))
          (should (equal "no profile named nope"
                         (harmless-profile-test-signal
                          (lambda () (harmless-profile-show "nope")))))
          (should (equal "profile token is missing"
                         (harmless-profile-test-signal
                          (lambda ()
                            (harmless-profile-token "workacct")))))
          (setenv "HARMLESS_WORK_TOKEN" "   ")
          (should (equal (list :name "workacct"
                               :runtime "harmless"
                               :env "HARMLESS_WORK_TOKEN"
                               :set nil)
                         (harmless-profile-show "workacct")))
          (should (equal "profile token is missing"
                         (harmless-profile-test-signal
                          (lambda ()
                            (harmless-profile-token "workacct")))))
          (setenv "HARMLESS_WORK_TOKEN" harmless-profile-test-token)
          (setenv "HARMLESS_OTHER_TOKEN" "sk-other-profile-token")
          (setenv "HARMLESS_CLAUDE_TOKEN" "sk-claude-profile-token")
          (should (equal (list :name "workacct"
                               :runtime "harmless"
                               :env "HARMLESS_WORK_TOKEN"
                               :set t)
                         (harmless-profile-show "workacct")))
          (should-not (string-search
                       harmless-profile-test-token
                       (format "%S" (harmless-profile-show "workacct"))))
          (let ((before (harmless-profile-test-read profiles)))
            (should (equal harmless-profile-test-token
                           (harmless-profile-token "workacct")))
            (should (equal before
                           (harmless-profile-test-read profiles))))
          (should-not (string-search
                       harmless-profile-test-token
                       (harmless-profile-test-read profiles)))
          (should-not (string-search
                       "sk-other-profile-token"
                       (harmless-profile-test-read profiles)))
          (should-not (string-search
                       "sk-claude-profile-token"
                       (harmless-profile-test-read profiles)))
          (let ((before (harmless-profile-test-read profiles)))
            (dolist (text (list
                           "nope\n"
                           (concat "[{\"name\":\"workacct\","
                                   "\"runtime\":\"harmless\","
                                   "\"env\":\"HARMLESS_WORK_TOKEN\","
                                   "\"token\":\"sk-stored-profile\"}]\n")
                           (concat "[{\"name\":\"workacct\","
                                   "\"runtime\":\"harmless\","
                                   "\"env\":\"HARMLESS_WORK_TOKEN\","
                                   "\"secret\":\"secret-value\"}]\n")
                           (concat "[{\"name\":\"workacct\","
                                   "\"runtime\":\"harmless\","
                                   "\"env\":\"HARMLESS_WORK_TOKEN\","
                                   "\"password\":\"password-value\"}]\n")
                           (concat "[{\"name\":\"workacct\","
                                   "\"runtime\":\"nope\","
                                   "\"env\":\"HARMLESS_WORK_TOKEN\"},"
                                   "{\"name\":\"workacct\","
                                   "\"runtime\":\"harmless\","
                                   "\"env\":\"HARMLESS_WORK_TOKEN\"}]\n")
                           "{\"name\":\"workacct\"}\n"))
              (harmless-profile-test-write profiles text)
              (should (equal "profiles are not valid"
                             (harmless-profile-test-signal
                              (lambda () (harmless-profile-list)))))
              (should (equal text
                             (harmless-profile-test-read profiles))))
            (harmless-profile-test-write profiles before))
          (should (equal "team rig is missing"
                         (harmless-profile-test-signal
                          (lambda ()
                            (harmless-profile-switch
                             "desk" "core" "lead" "workacct")))))
          (should-not (file-exists-p rig))
          (harmless-team-add-seat "desk" "core" "lead" "builds")
          (harmless-team-add-seat "desk" "core" "note" "reads")
          (should (equal "team seat is not occupied"
                         (harmless-profile-test-signal
                          (lambda ()
                            (harmless-profile-switch
                             "desk" "core" "lead" "workacct")))))
          (should-not (plist-get
                       (harmless-team-seat "desk" "core" "lead")
                       :profile))
          (let ((before (harmless-profile-test-read rig))
                (stored (harmless-profile-test-read profiles)))
            (harmless-profile-test-write
             rig (concat "{\"name\":\"desk\",\"token\":\"sk-rig-token\","
                         "\"pods\":[]}\n"))
            (should (equal "team rig is not valid"
                           (harmless-profile-test-signal
                            (lambda ()
                              (harmless-team-seat "desk" "core" "lead")))))
            (should (equal (concat "{\"name\":\"desk\","
                                   "\"token\":\"sk-rig-token\","
                                   "\"pods\":[]}\n")
                           (harmless-profile-test-read rig)))
            (harmless-profile-test-write
             rig (concat "{\"name\":\"desk\",\"pods\":[{\"name\":\"core\","
                         "\"seats\":[{\"name\":\"lead\",\"role\":\"builds\","
                         "\"session\":\"\",\"work\":[],"
                         "\"address\":\"core-lead@desk\","
                         "\"profile\":\"workacct\"}]}]}\n"))
            (should (equal "workacct"
                           (plist-get
                            (harmless-team-seat "desk" "core" "lead")
                            :profile)))
            (harmless-profile-test-write
             rig (concat "{\"name\":\"desk\",\"pods\":[{\"name\":\"core\","
                         "\"seats\":[{\"name\":\"lead\",\"role\":\"builds\","
                         "\"session\":\"\",\"work\":[],"
                         "\"address\":\"core-lead@desk\","
                         "\"profile\":\"Work\"}]}]}\n"))
            (should (equal "team rig is not valid"
                           (harmless-profile-test-signal
                            (lambda ()
                              (harmless-team-seat "desk" "core" "lead")))))
            (harmless-profile-test-write rig before)
            (should (equal stored
                           (harmless-profile-test-read profiles))))
          (should-not (string-search
                       "sk-rig-token"
                       (harmless-profile-test-read rig)))
          (should-not (string-search
                       harmless-profile-test-token
                       (harmless-profile-test-read rig)))
          (harmless-profile-test-write
           (expand-file-name "summary.json" hand)
           "{\"status\":\"idle\"}\n")
          (harmless-profile-test-write transcript talk)
          (should (equal harmless-profile-test-id
                         (plist-get
                          (harmless-team-adopt
                           "desk" "core" "lead" "harmless"
                           harmless-profile-test-id)
                          :session)))
          (should (equal '("keep the tests")
                         (plist-get
                          (harmless-team-add-work
                           "desk" "core" "lead" "keep the tests")
                          :work)))
          (setq saved (harmless-profile-test-read transcript))
          (should (equal talk saved))
          (setq seat (harmless-profile-switch
                      "desk" "core" "lead" "  workacct  "))
          (should (equal (list :rig "desk"
                               :pod "core"
                               :name "lead"
                               :role "builds"
                               :address "core-lead@desk"
                               :session harmless-profile-test-id
                               :work '("keep the tests")
                               :runtime "harmless"
                               :profile "workacct")
                         seat))
          (should (equal seat
                         (harmless-team-seat "desk" "core" "lead")))
          (should (equal saved
                         (harmless-profile-test-read transcript)))
          (should (file-directory-p hand))
          (should (string-search "workacct"
                                 (harmless-profile-test-read rig)))
          (should (string-search harmless-profile-test-id
                                 (harmless-profile-test-read rig)))
          (should (string-search "keep the tests"
                                 (harmless-profile-test-read rig)))
          (should-not (string-search
                       harmless-profile-test-token
                       (harmless-profile-test-read rig)))
          (should-not (string-search
                       "HARMLESS_WORK_TOKEN"
                       (harmless-profile-test-read rig)))
          (should-not (string-search
                       harmless-profile-test-token
                       (harmless-profile-test-read profiles)))
          (setq seat (harmless-profile-switch
                      "desk" "core" "lead" "otheracct"))
          (should (equal "otheracct" (plist-get seat :profile)))
          (should (equal harmless-profile-test-id
                         (plist-get seat :session)))
          (should (equal '("keep the tests") (plist-get seat :work)))
          (should (equal "harmless" (plist-get seat :runtime)))
          (should (equal saved
                         (harmless-profile-test-read transcript)))
          (let ((before (harmless-profile-test-read rig)))
            (should (equal "profile runtime does not match"
                           (harmless-profile-test-signal
                            (lambda ()
                              (harmless-profile-switch
                               "desk" "core" "lead" "claudeacct")))))
            (should (equal before
                           (harmless-profile-test-read rig)))
            (should (equal saved
                           (harmless-profile-test-read transcript))))
          (let ((bound (harmless-team-adopt
                        "desk" "core" "note" "claude" claude)))
            (setq seat (harmless-profile-switch
                        "desk" "core" "note" "claudeacct"))
            (should (equal (plist-get bound :session)
                           (plist-get seat :session)))
            (should (equal "claude" (plist-get seat :runtime)))
            (should (equal "claudeacct" (plist-get seat :profile)))
            (should-not (plist-get seat :work)))
          (should (equal "otheracct"
                         (plist-get
                          (harmless-team-seat "desk" "core" "lead")
                          :profile)))
          (should (equal harmless-profile-test-id
                         (plist-get
                          (harmless-team-seat "desk" "core" "lead")
                          :session)))
          (let ((before (harmless-profile-test-read rig)))
            (should (equal "workacct"
                           (harmless-profile-remove "workacct")))
            (should (equal before
                           (harmless-profile-test-read rig)))
            (should (equal saved
                           (harmless-profile-test-read transcript))))
          (should (equal '("claudeacct" "otheracct")
                         (harmless-profile-list)))
          (should-not (string-search
                       harmless-profile-test-token
                       (harmless-profile-test-read profiles)))
          (should (equal harmless-profile-test-id
                         (plist-get
                          (harmless-team-release "desk" "core" "lead")
                          :session)))
          (setq seat (harmless-team-seat "desk" "core" "lead"))
          (should (equal "" (plist-get seat :session)))
          (should (equal "otheracct" (plist-get seat :profile)))
          (should-not (plist-get seat :runtime))
          (should (equal '("keep the tests") (plist-get seat :work)))
          (let ((before (harmless-profile-test-read rig)))
            (should (equal "team seat is not occupied"
                           (harmless-profile-test-signal
                            (lambda ()
                              (harmless-profile-switch
                               "desk" "core" "lead" "otheracct")))))
            (should (equal before
                           (harmless-profile-test-read rig))))
          (should (equal saved
                         (harmless-profile-test-read transcript)))
          (should (file-directory-p hand))
          (should (file-directory-p claude))
          (should-not (string-search
                       harmless-profile-test-token
                       (harmless-profile-test-read rig)))
          (should-not (string-search
                       "sk-other-profile-token"
                       (harmless-profile-test-read rig)))
          (should-not (string-search
                       "sk-claude-profile-token"
                       (harmless-profile-test-read rig)))
          (should-not (file-exists-p
                       (expand-file-name "profiles.json" elsewhere)))
          (should (equal roots (harmless-profile-test-roots)))
          (should (equal home-work
                         (file-exists-p (expand-file-name "work" "~"))))
          (should (equal home-profiles
                         (file-exists-p
                          (expand-file-name "profiles.json" "~"))))
          (should (equal timers timer-list))
          (should (equal procs (process-list)))
          (should (equal sched
                         (hash-table-count harmless-schedule--timers))))
      (setenv "HARMLESS_WORK_TOKEN" old-work)
      (setenv "HARMLESS_OTHER_TOKEN" old-other)
      (setenv "HARMLESS_CLAUDE_TOKEN" old-claude)
      (when (file-directory-p parent)
        (delete-directory parent t)))))
