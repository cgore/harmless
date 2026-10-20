;;; harmless-fleet-tests.el --- One rig view -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(defun harmless-fleet-test-signal (fn)
  "Return the error text from FN."
  (error-message-string (should-error (funcall fn) :type 'error)))

(defun harmless-fleet-test-write (path text)
  "Write TEXT to PATH with Unix newlines and return PATH."
  (make-directory (file-name-directory path) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil path nil 'silent))
  path)

(defun harmless-fleet-test-read (path)
  "Return PATH as a Unix-newline string."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (buffer-string)))

(defun harmless-fleet-test-roots ()
  "Return existence of team files at filesystem roots."
  (list (file-exists-p "/teams")
        (file-exists-p "c:/teams")
        (file-exists-p "d:/teams")
        (file-exists-p "/sessions")
        (file-exists-p "c:/sessions")
        (file-exists-p "d:/sessions")))

(defun harmless-fleet-test-text ()
  "Return the fleet buffer as a string."
  (with-current-buffer harmless-fleet-buffer
    (buffer-string)))

(defun harmless-fleet-test-view (edge service people)
  "Return the fleet text for EDGE, SERVICE, and PEOPLE."
  (concat "Rig desk\n"
          "\n"
          "Topology\n"
          "core-lead@desk\n"
          "core-note@desk\n"
          (if edge
              "delegates_to core-lead@desk -> core-note@desk\n"
            "")
          "\n"
          "Health\n"
          "core-lead@desk empty\n"
          "core-note@desk empty\n"
          (if service "service log up\n" "")
          "\n"
          "People\n"
          people
          "\n"))

(ert-deftest harmless-fleet-shows-one-rig ()
  "One view is order, edges, health, and rows waiting on a person."
  (let* ((parent (make-temp-file "harmless-fleet-" t))
         (elsewhere (expand-file-name "elsewhere" parent))
         (local (expand-file-name "local" parent))
         (logf (expand-file-name "svc.log" parent))
         (harmless-directory (file-name-as-directory local))
         (default-directory (file-name-as-directory elsewhere))
         (roots (harmless-fleet-test-roots))
         (home-work (file-exists-p (expand-file-name "work" "~")))
         (timers (copy-sequence timer-list))
         (procs (copy-sequence (process-list)))
         (sched (hash-table-count harmless-schedule--timers))
         (sessions (hash-table-count harmless--sessions))
         (dashboard (get-buffer "*harmless*"))
         (start (current-buffer))
         (rig (expand-file-name "teams/desk.json" local))
         (lead '("core-lead@desk" "core-note@desk"))
         (edge (list (list :kind "delegates_to"
                           :from "core-lead@desk"
                           :to "core-note@desk")))
         (report nil)
         (bytes nil))
    (unwind-protect
        (progn
          (make-directory elsewhere t)
          (make-directory local t)
          (when (get-buffer harmless-fleet-buffer)
            (kill-buffer harmless-fleet-buffer))
          (should (commandp #'harmless-team-fleet))
          (should-not (harmless-tool-by-name "team_fleet"))
          (should-not (harmless-tool-by-name "fleet"))
          (dolist (empty (list nil "" "   " "\n"))
            (should (equal (harmless-fleet-test-signal
                            (lambda () (harmless-team-fleet empty)))
                           "team rig is empty")))
          (should (equal (harmless-fleet-test-signal
                          (lambda () (harmless-team-fleet 42)))
                         "team rig must be a string"))
          (should (equal (harmless-fleet-test-signal
                          (lambda () (harmless-team-fleet "desk/core")))
                         "team rig is not a name"))
          (should (equal "team rig is missing"
                         (harmless-fleet-test-signal
                          (lambda () (harmless-team-fleet "desk")))))
          (should-not (get-buffer harmless-fleet-buffer))
          (should-not (file-exists-p rig))
          (dolist (root (list "/" "c:/" "c:" "d:/" "C:/"
                              "  /  " "  c:/  "))
            (let ((harmless-directory root))
              (should (equal (harmless-fleet-test-signal
                              (lambda () (harmless-team-fleet "desk")))
                             (format "Refusing to store a team at %s"
                                     (string-trim root))))))
          (let ((harmless-directory "relative"))
            (should (equal (harmless-fleet-test-signal
                            (lambda () (harmless-team-fleet "desk")))
                           (concat "Harmless data directory must be "
                                   "absolute, not \"relative\""))))
          (should-not (file-exists-p
                       (expand-file-name "teams" elsewhere)))
          (should-not (file-exists-p
                       (expand-file-name "relative" elsewhere)))
          (should-not (file-exists-p
                       (expand-file-name "relative" parent)))
          (should-not (get-buffer harmless-fleet-buffer))
          (harmless-team-add-seat "desk" "core" "lead" "builds")
          (harmless-team-add-seat "desk" "core" "note" "reads")
          (setq bytes (harmless-fleet-test-read rig))
          (setq report (harmless-team-fleet "desk"))
          (should (equal bytes (harmless-fleet-test-read rig)))
          (should (equal report
                         (list :name "desk"
                               :order lead
                               :edges nil
                               :health (harmless-team-health "desk")
                               :people nil)))
          (should (equal (harmless-team-launch-order "desk") lead))
          (should (equal (harmless-fleet-test-text)
                         (harmless-fleet-test-view nil nil "none")))
          (should (eq (current-buffer) start))
          (should (eq dashboard (get-buffer "*harmless*")))
          (with-current-buffer harmless-fleet-buffer
            (should (eq major-mode 'special-mode))
            (should buffer-read-only))
          (setq bytes (harmless-fleet-test-read rig))
          (should (equal (harmless-fleet-test-signal
                          (lambda () (harmless-team-fleet nil)))
                         "team rig is empty"))
          (should (equal bytes (harmless-fleet-test-read rig)))
          (should (equal (harmless-fleet-test-text)
                         (harmless-fleet-test-view nil nil "none")))
          (harmless-team-edge
           "desk" "delegates_to" "core.lead" "core.note")
          (harmless-fleet-test-write logf "line\n")
          (should (equal logf
                         (plist-get
                          (harmless-team-service "desk" "log" logf)
                          :ref)))
          (harmless-team-row "desk" "core.note" "fix the door")
          (harmless-team-row-claim "desk" "1")
          (harmless-team-row-park "desk" "1" "tests failed")
          (setq bytes (harmless-fleet-test-read rig))
          (setq report (harmless-team-fleet "desk"))
          (should (equal bytes (harmless-fleet-test-read rig)))
          (should (equal (plist-get report :order) lead))
          (should (equal (plist-get report :edges) edge))
          (should (equal (plist-get report :edges)
                         (harmless-team-edges "desk")))
          (should (equal (plist-get report :health)
                         (harmless-team-health "desk")))
          (should-not (plist-get report :people))
          (should-not (harmless-team-board "desk" "waiting"))
          (should (equal (harmless-fleet-test-text)
                         (harmless-fleet-test-view t t "none")))
          (should-not (string-search "tests failed"
                                     (harmless-fleet-test-text)))
          (should (file-regular-p logf))
          (harmless-team-row "desk" "core.note" "ask a person")
          (harmless-team-row-claim "desk" "2")
          (harmless-team-row-park "desk" "2" "need a person"
                                  "Please look" "file:log")
          (setq bytes (harmless-fleet-test-read rig))
          (setq report (harmless-team-fleet "  desk  "))
          (should (equal bytes (harmless-fleet-test-read rig)))
          (should (equal (plist-get report :name) "desk"))
          (should (equal (mapcar (lambda (row) (plist-get row :id))
                                 (plist-get report :people))
                         '("2")))
          (should (equal (plist-get (car (plist-get report :people))
                                    :seat)
                         "core-note@desk"))
          (should (equal (plist-get (car (plist-get report :people))
                                    :wake)
                         "need a person"))
          (should (equal (plist-get (car (plist-get report :people))
                                    :summary)
                         "Please look"))
          (should (equal (plist-get (car (plist-get report :people))
                                    :evidence)
                         "file:log"))
          (should (equal (plist-get report :people)
                         (harmless-team-board "desk" "waiting")))
          (should (equal (mapcar (lambda (row) (plist-get row :id))
                                 (harmless-team-rows "desk"))
                         '("1" "2")))
          (should (equal (harmless-fleet-test-text)
                         (harmless-fleet-test-view
                          t t "2 core-note@desk need a person")))
          (should-not (string-search "tests failed"
                                     (harmless-fleet-test-text)))
          (should (string-search "need a person"
                                 (harmless-fleet-test-text)))
          (should (eq (current-buffer) start))
          (should (eq dashboard (get-buffer "*harmless*")))
          (let ((shown (harmless-fleet-test-text)))
            (dolist (root (list "/" "c:/" "d:/" "\\"))
              (let ((harmless-directory root))
                (should (equal (harmless-fleet-test-signal
                                (lambda ()
                                  (harmless-team-fleet "desk")))
                               (format "Refusing to store a team at %s"
                                       (string-trim root))))))
            (let ((harmless-directory "relative"))
              (should (equal (harmless-fleet-test-signal
                              (lambda () (harmless-team-fleet "desk")))
                             (concat "Harmless data directory must be "
                                     "absolute, not \"relative\""))))
            (should (equal bytes (harmless-fleet-test-read rig)))
            (should (equal shown (harmless-fleet-test-text))))
          (should (file-name-absolute-p rig))
          (should (file-name-absolute-p logf))
          (should (harmless-directory-strictly-under-p rig local))
          (should (harmless-directory-strictly-under-p logf parent))
          (should-not (equal (file-truename rig)
                             (file-truename "/")))
          (should-not (harmless-filesystem-root-p rig))
          (should-not (harmless-filesystem-root-p logf))
          (should-not (file-exists-p
                       (expand-file-name "teams" elsewhere)))
          (should-not
           (harmless-session-get
            "44444444-5555-6666-7777-888888888888"))
          (should (equal sessions
                         (hash-table-count harmless--sessions)))
          (should (equal roots (harmless-fleet-test-roots)))
          (should (equal home-work
                         (file-exists-p
                          (expand-file-name "work" "~"))))
          (should (equal timers timer-list))
          (should (equal procs (process-list)))
          (should (equal sched
                         (hash-table-count
                          harmless-schedule--timers))))
      (when (get-buffer harmless-fleet-buffer)
        (kill-buffer harmless-fleet-buffer))
      (when (file-directory-p parent)
        (delete-directory parent t)))))
