;;; harmless-health-tests.el --- Tests for rig health -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(defconst harmless-health-test-a
  "44444444-5555-6666-7777-888888888888"
  "A recorded Harmless session id.")

(defconst harmless-health-test-b
  "55555555-6666-7777-8888-999999999999"
  "The id a live summary can name instead.")

(defconst harmless-health-test-c
  "66666666-7777-8888-9999-aaaaaaaaaaaa"
  "A session id whose directory can disappear.")

(defun harmless-health-test-signal (fn)
  "Return the error text from FN."
  (error-message-string (should-error (funcall fn) :type 'error)))

(defun harmless-health-test-write (path text)
  "Write TEXT to PATH with Unix newlines and return PATH."
  (make-directory (file-name-directory path) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil path nil 'silent))
  path)

(defun harmless-health-test-read (path)
  "Return PATH as a Unix-newline string."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (buffer-string)))

(defun harmless-health-test-summary (id)
  "Return summary JSON whose id is ID."
  (concat "{\"id\":" (harmless-json-text id) ",\"status\":\"idle\"}\n"))

(defun harmless-health-test-roots ()
  "Return existence of team files at filesystem roots."
  (list (file-exists-p "/teams")
        (file-exists-p "c:/teams")
        (file-exists-p "d:/teams")
        (file-exists-p "/sessions")
        (file-exists-p "c:/sessions")
        (file-exists-p "d:/sessions")))

(defun harmless-health-test-seat (report address)
  "Return the seat ADDRESS in REPORT."
  (cl-find address (plist-get report :seats)
           :key (lambda (item) (plist-get item :address))
           :test #'equal))

(defun harmless-health-test-service (report name)
  "Return the service NAME in REPORT."
  (cl-find name (plist-get report :services)
           :key (lambda (item) (plist-get item :name))
           :test #'equal))

(ert-deftest harmless-health-says-what-is-true ()
  (let* ((parent (make-temp-file "harmless-health-" t))
         (elsewhere (expand-file-name "elsewhere" parent))
         (local (expand-file-name "local" parent))
         (logf (expand-file-name "svc.log" parent))
         (disk (expand-file-name "svcdir" parent))
         (harmless-directory (file-name-as-directory local))
         (default-directory (file-name-as-directory elsewhere))
         (roots (harmless-health-test-roots))
         (home-work (file-exists-p (expand-file-name "work" "~")))
         (timers (copy-sequence timer-list))
         (procs (copy-sequence (process-list)))
         (sched (hash-table-count harmless-schedule--timers))
         (pid12 (process-attributes 12))
         (epid (number-to-string (emacs-pid)))
         (rig (expand-file-name "teams/desk.json" local))
         (dir-a (expand-file-name
                 (concat "sessions/hand/" harmless-health-test-a) local))
         (dir-b (expand-file-name
                 (concat "sessions/hand/" harmless-health-test-b) local))
         (dir-c (expand-file-name
                 (concat "sessions/hand/" harmless-health-test-c) local))
         (report nil)
         (saved nil))
    (unwind-protect
        (progn
          (make-directory elsewhere t)
          (make-directory local t)
          (make-directory disk t)
          (should (commandp #'harmless-team-health))
          (should (commandp #'harmless-team-service))
          (should (commandp #'harmless-team-service-remove))
          (should (commandp #'harmless-team-recover))
          (should (equal "team rig is missing"
                         (harmless-health-test-signal
                          (lambda () (harmless-team-health "desk")))))
          (should (equal "team rig is missing"
                         (harmless-health-test-signal
                          (lambda ()
                            (harmless-team-service "desk" "web" logf)))))
          (should-not (file-exists-p logf))
          (dolist (root (list "/" "c:/" "c:" "d:/" "C:/"
                              "  /  " "  c:/  "))
            (let ((harmless-directory root))
              (should (equal (harmless-health-test-signal
                              (lambda () (harmless-team-health "desk")))
                             (format "Refusing to store a team at %s"
                                     (string-trim root))))))
          (let ((harmless-directory "relative"))
            (should (equal (harmless-health-test-signal
                            (lambda () (harmless-team-health "desk")))
                           (concat "Harmless data directory must be "
                                   "absolute, not \"relative\""))))
          (should-not (file-exists-p
                       (expand-file-name "teams" elsewhere)))
          (harmless-team-add-seat "desk" "core" "lead" "builds")
          (harmless-team-add-seat "desk" "core" "note" "reads")
          (setq saved (harmless-health-test-read rig))
          (setq report (harmless-team-health "desk"))
          (should (equal saved (harmless-health-test-read rig)))
          (should (equal "empty"
                         (plist-get
                          (harmless-health-test-seat
                           report "core-lead@desk")
                          :health)))
          (should (equal "empty"
                         (plist-get
                          (harmless-health-test-seat
                           report "core-note@desk")
                          :health)))
          (should-not (plist-get report :services))
          (dolist (empty (list nil "" "   "))
            (should (equal (harmless-health-test-signal
                            (lambda ()
                              (harmless-team-service "desk" empty logf)))
                           "service name is empty")))
          (should (equal (harmless-health-test-signal
                          (lambda ()
                            (harmless-team-service "desk" 42 logf)))
                         "service name must be a string"))
          (dolist (bad (list "/" "c:/" "~" "web/log" "Ada-l" "../web"))
            (should (equal (harmless-health-test-signal
                            (lambda ()
                              (harmless-team-service "desk" bad logf)))
                           "service name is not a name")))
          (dolist (empty (list nil "" "   "))
            (should (equal (harmless-health-test-signal
                            (lambda ()
                              (harmless-team-service "desk" "web" empty)))
                           "service ref is empty")))
          (should (equal (harmless-health-test-signal
                          (lambda ()
                            (harmless-team-service "desk" "web" 42)))
                         "service ref must be a string"))
          (dolist (bad (list "~" "notes" "svc.log" "sub/"))
            (should (equal (harmless-health-test-signal
                            (lambda ()
                              (harmless-team-service "desk" "web" bad)))
                           "service ref must be absolute")))
          (dolist (bad (list "/" "c:/" "c:" "d:/" "\\"))
            (should (equal (harmless-health-test-signal
                            (lambda ()
                              (harmless-team-service "desk" "web" bad)))
                           "service ref is the filesystem root")))
          (should (equal (harmless-health-test-signal
                          (lambda ()
                            (harmless-team-service "desk" "web" "0")))
                         "service ref is not a ref"))
          (should (equal saved (harmless-health-test-read rig)))
          (should-not (file-exists-p logf))
          (should (equal "12"
                         (plist-get
                          (harmless-team-service "desk" "tiny" "00012")
                          :ref)))
          (should (equal pid12 (process-attributes 12)))
          (should (equal (list :name "web" :ref epid)
                         (harmless-team-service "desk" "web" epid)))
          (should (equal (list :name "log" :ref logf)
                         (harmless-team-service "desk" "log" logf)))
          (should-not (file-exists-p logf))
          (should (equal (directory-file-name disk)
                         (plist-get
                          (harmless-team-service
                           "desk" "disk" (file-name-as-directory disk))
                          :ref)))
          (setq report (harmless-team-health "desk"))
          (should (equal (if pid12 "up" "down")
                         (plist-get
                          (harmless-health-test-service report "tiny")
                          :health)))
          (should (equal "up"
                         (plist-get
                          (harmless-health-test-service report "web")
                          :health)))
          (should (equal "down"
                         (plist-get
                          (harmless-health-test-service report "log")
                          :health)))
          (should (equal "up"
                         (plist-get
                          (harmless-health-test-service report "disk")
                          :health)))
          (should (equal "empty"
                         (plist-get
                          (harmless-health-test-seat
                           report "core-lead@desk")
                          :health)))
          (should-not (plist-get
                       (harmless-health-test-seat
                        report "core-lead@desk")
                       :ref))
          (should-not (plist-get
                       (harmless-health-test-service report "web")
                       :address))
          (let ((before (harmless-health-test-read rig)))
            (should (equal "service is already registered: web"
                           (harmless-health-test-signal
                            (lambda ()
                              (harmless-team-service "desk" "web" epid)))))
            (should (equal "no service named gone"
                           (harmless-health-test-signal
                            (lambda ()
                              (harmless-team-service-remove
                               "desk" "gone")))))
            (should (equal before (harmless-health-test-read rig))))
          (should (equal "tiny"
                         (harmless-team-service-remove "desk" "tiny")))
          (should (equal pid12 (process-attributes 12)))
          (harmless-health-test-write logf "line\n")
          (setq report (harmless-team-health "desk"))
          (should (equal "up"
                         (plist-get
                          (harmless-health-test-service report "log")
                          :health)))
          (should-not (harmless-health-test-service report "tiny"))
          (should (file-regular-p logf))
          (harmless-profile-add
           "workacct" "harmless" "HARMLESS_HEALTH_TOKEN")
          (harmless-health-test-write
           (expand-file-name "summary.json" dir-a) "nope\n")
          (should (equal harmless-health-test-a
                         (plist-get
                          (harmless-team-adopt
                           "desk" "core" "lead" "harmless"
                           harmless-health-test-a)
                          :session)))
          (harmless-team-add-work "desk" "core" "lead" "keep the tests")
          (harmless-profile-switch "desk" "core" "lead" "workacct")
          (setq saved (harmless-health-test-read rig))
          (setq report (harmless-team-health "desk"))
          (should (equal saved (harmless-health-test-read rig)))
          (should (equal "disagree"
                         (plist-get
                          (harmless-health-test-seat
                           report "core-lead@desk")
                          :health)))
          (should-not (harmless-session-get harmless-health-test-a))
          (should (equal "team health cannot recover"
                         (harmless-health-test-signal
                          (lambda ()
                            (harmless-team-recover
                             "desk" "core" "lead")))))
          (should (equal saved (harmless-health-test-read rig)))
          (harmless-health-test-write
           (expand-file-name "summary.json" dir-a)
           (harmless-health-test-summary harmless-health-test-b))
          (setq saved (harmless-health-test-read rig))
          (should (equal "team health cannot recover"
                         (harmless-health-test-signal
                          (lambda ()
                            (harmless-team-recover
                             "desk" "core" "lead")))))
          (should (equal saved (harmless-health-test-read rig)))
          (should (file-directory-p dir-a))
          (harmless-health-test-write
           (expand-file-name "summary.json" dir-b)
           (harmless-health-test-summary harmless-health-test-b))
          (setq report (harmless-team-recover "desk" "core" "lead"))
          (should (equal harmless-health-test-b
                         (plist-get report :session)))
          (should (equal "live" (plist-get report :health)))
          (should (equal "harmless" (plist-get report :runtime)))
          (should (file-directory-p dir-a))
          (should (file-directory-p dir-b))
          (should-not (harmless-session-get harmless-health-test-a))
          (should-not (harmless-session-get harmless-health-test-b))
          (setq report (harmless-team-seat "desk" "core" "lead"))
          (should (equal '("keep the tests") (plist-get report :work)))
          (should (equal "workacct" (plist-get report :profile)))
          (should (equal harmless-health-test-b
                         (plist-get report :session)))
          (setq saved (harmless-health-test-read rig))
          (should (equal "team health agrees"
                         (harmless-health-test-signal
                          (lambda ()
                            (harmless-team-recover
                             "desk" "core" "lead")))))
          (should (equal saved (harmless-health-test-read rig)))
          (harmless-health-test-write
           (expand-file-name "summary.json" dir-c)
           (harmless-health-test-summary harmless-health-test-c))
          (harmless-team-adopt
           "desk" "core" "note" "harmless" harmless-health-test-c)
          (harmless-team-add-work "desk" "core" "note" "keep note")
          (delete-directory dir-c t)
          (should-not (file-directory-p dir-c))
          (setq report (harmless-team-health "desk"))
          (should (equal "missing"
                         (plist-get
                          (harmless-health-test-seat
                           report "core-note@desk")
                          :health)))
          (should (equal "live"
                         (plist-get
                          (harmless-health-test-seat
                           report "core-lead@desk")
                          :health)))
          (should (equal "up"
                         (plist-get
                          (harmless-health-test-service report "web")
                          :health)))
          (setq report (harmless-team-recover "desk" "core" "note"))
          (should (equal "" (plist-get report :session)))
          (should (equal "empty" (plist-get report :health)))
          (should-not (plist-get report :runtime))
          (should-not (file-directory-p dir-c))
          (setq report (harmless-team-seat "desk" "core" "note"))
          (should (equal '("keep note") (plist-get report :work)))
          (should-not (plist-get report :profile))
          (should (equal "" (plist-get report :session)))
          (should (equal harmless-health-test-b
                         (plist-get
                          (harmless-team-seat "desk" "core" "lead")
                          :session)))
          (should (equal "up"
                         (plist-get
                          (harmless-health-test-service
                           (harmless-team-health "desk") "log")
                          :health)))
          (should (file-regular-p logf))
          (should (file-directory-p disk))
          (setq saved (harmless-health-test-read rig))
          (harmless-health-test-write
           rig (concat "{\"name\":\"desk\",\"pods\":[],\"services\":"
                       "[{\"name\":\"web\",\"ref\":\"notes\"}]}\n"))
          (should (equal "team rig is not valid"
                         (harmless-health-test-signal
                          (lambda () (harmless-team-health "desk")))))
          (should (equal (concat "{\"name\":\"desk\",\"pods\":[],"
                                 "\"services\":[{\"name\":\"web\","
                                 "\"ref\":\"notes\"}]}\n")
                         (harmless-health-test-read rig)))
          (harmless-health-test-write
           rig (concat "{\"name\":\"desk\",\"token\":\"sk-health-token\","
                       "\"pods\":[]}\n"))
          (should (equal "team rig is not valid"
                         (harmless-health-test-signal
                          (lambda () (harmless-team-health "desk")))))
          (harmless-health-test-write rig saved)
          (should-not (string-search
                       "sk-health-token"
                       (harmless-health-test-read rig)))
          (should (equal roots (harmless-health-test-roots)))
          (should (equal home-work
                         (file-exists-p (expand-file-name "work" "~"))))
          (should (equal timers timer-list))
          (should (equal procs (process-list)))
          (should (equal sched
                         (hash-table-count harmless-schedule--timers)))
          (should (process-attributes (emacs-pid))))
      (when (file-directory-p parent)
        (delete-directory parent t)))))
