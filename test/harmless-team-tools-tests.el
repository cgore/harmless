;;; harmless-team-tools-tests.el --- Tools for a team -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(defconst harmless-team-tools-test-id
  "11111111-2222-3333-4444-555555555555"
  "One session id an occupied seat can adopt.")

(defun harmless-team-tools-test-signal (fn)
  "Return the error text from FN."
  (error-message-string (should-error (funcall fn) :type 'error)))

(defun harmless-team-tools-test-write (path text)
  "Write TEXT to PATH with Unix newlines and return PATH."
  (make-directory (file-name-directory path) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil path nil 'silent))
  path)

(defun harmless-team-tools-test-read (path)
  "Return PATH as a Unix-newline string."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (buffer-string)))

(defun harmless-team-tools-test-session (id cwd plan)
  "Return session ID at CWD, in plan mode when PLAN is non-nil."
  (harmless-session--create
   :id id
   :cwd cwd
   :model "grok-4.6"
   :source 'agent
   :permission-mode 'ask
   :status 'idle
   :messages nil
   :plan-mode plan))

(defun harmless-team-tools-test-roots ()
  "Return existence of team files at filesystem roots."
  (list (file-exists-p "/teams")
        (file-exists-p "c:/teams")
        (file-exists-p "d:/teams")
        (file-exists-p "/sessions")
        (file-exists-p "c:/sessions")
        (file-exists-p "d:/sessions")
        (file-exists-p "/snapshots")
        (file-exists-p "c:/snapshots")
        (file-exists-p "d:/snapshots")))

(defun harmless-team-tools-test-names (session)
  "Return the tool names SESSION is offered."
  (mapcar #'harmless-tool-name
          (harmless-tools-for-session session)))

(ert-deftest harmless-team-tools-boot-and-restore ()
  (let* ((parent (make-temp-file "harmless-team-tools-" t))
         (elsewhere (expand-file-name "elsewhere" parent))
         (local (expand-file-name "local" parent))
         (project (expand-file-name "project" parent))
         (harmless-directory (file-name-as-directory local))
         (default-directory (file-name-as-directory elsewhere))
         (roots (harmless-team-tools-test-roots))
         (home-work (file-exists-p (expand-file-name "work" "~")))
         (timers (copy-sequence timer-list))
         (procs (copy-sequence (process-list)))
         (sched (hash-table-count harmless-schedule--timers))
         (session (harmless-team-tools-test-session
                   "team-tools" project nil))
         (detached (harmless-team-tools-test-session
                    "team-tools-det" nil nil))
         (rig (expand-file-name "teams/starter.json" local))
         (snap (expand-file-name
                "teams/snapshots/starter/dawn.json" local))
         (hand (expand-file-name
                (concat "sessions/hand/"
                        harmless-team-tools-test-id)
                local))
         (screen (expand-file-name "messages.jsonl" hand))
         (boot nil)
         (seen nil)
         (report nil))
    (unwind-protect
        (progn
          (make-directory elsewhere t)
          (make-directory local t)
          (make-directory project t)
          (should (eq 'edit (harmless-tool-class
                             (harmless-tool-by-name "harmless-tool-team-boot"))))
          (should (eq 'read (harmless-tool-class
                             (harmless-tool-by-name "harmless-tool-team-inspect"))))
          (should (eq 'edit (harmless-tool-class
                             (harmless-tool-by-name "harmless-tool-team-message"))))
          (should (eq 'edit (harmless-tool-class
                             (harmless-tool-by-name "harmless-tool-team-snapshot"))))
          (should (eq 'edit (harmless-tool-class
                             (harmless-tool-by-name "harmless-tool-team-restore"))))
          (should (equal "boot" (harmless-ui--tool-verb "harmless-tool-team-boot")))
          (should (equal "inspect"
                         (harmless-ui--tool-verb "harmless-tool-team-inspect")))
          (should (equal "message"
                         (harmless-ui--tool-verb "harmless-tool-team-message")))
          (should (equal "snapshot"
                         (harmless-ui--tool-verb "harmless-tool-team-snapshot")))
          (should (equal "restore"
                         (harmless-ui--tool-verb "harmless-tool-team-restore")))
          (should (equal "starter"
                         (harmless-ui--tool-summary
                          "harmless-tool-team-boot" '(:spec "starter"))))
          (should (equal "starter"
                         (harmless-ui--tool-summary
                          "harmless-tool-team-inspect" '(:rig "starter"))))
          (should (equal "Ship it?"
                         (harmless-ui--tool-summary
                          "harmless-tool-team-message" '(:text "Ship it?"))))
          (should (equal "dawn"
                         (harmless-ui--tool-summary
                          "harmless-tool-team-snapshot" '(:name "dawn"))))
          (should (equal "dawn"
                         (harmless-ui--tool-summary
                          "harmless-tool-team-restore" '(:name "dawn"))))
          (dolist (name '("harmless-tool-team-boot" "harmless-tool-team-inspect" "harmless-tool-team-message"
                          "harmless-tool-team-snapshot" "harmless-tool-team-restore"))
            (should (member name
                            (harmless-team-tools-test-names session)))
            (should-not (member name
                                (harmless-team-tools-test-names
                                 detached))))
          (should (equal "No Harmless session"
                         (harmless-team-tools-test-signal
                          (lambda ()
                            (harmless-team-tools-boot nil '(:spec "starter"))))))
          (should (equal "This chat is not attached to a project"
                         (harmless-team-tools-test-signal
                          (lambda ()
                            (harmless-team-tools-boot
                             detached '(:spec "starter"))))))
          (should (equal "Error: team spec is empty"
                         (harmless-tool-call
                          (harmless-tool-by-name "harmless-tool-team-boot")
                          session nil)))
          (setf (harmless-session-plan-mode session) t)
          (dolist (fn (list
                       (lambda ()
                         (harmless-team-tools-boot
                          session '(:spec "starter")))
                       (lambda ()
                         (harmless-team-tools-message
                          session '(:rig "starter" :target "dev.build"
                                    :text "Ship it?")))
                       (lambda ()
                         (harmless-team-tools-snapshot
                          session '(:rig "starter" :name "dawn")))
                       (lambda ()
                         (harmless-team-tools-restore
                          session '(:rig "starter" :name "dawn")))))
            (should (equal (harmless-team-tools-test-signal fn)
                           "Plan mode is on. Only plan.md may be edited, with harmless-tool-write-plan")))
          (should (equal "team rig is missing"
                         (harmless-team-tools-test-signal
                          (lambda ()
                            (harmless-team-tools-inspect
                             session '(:rig "starter"))))))
          (should-not (file-directory-p
                       (expand-file-name "teams" local)))
          (setf (harmless-session-plan-mode session) nil)
          (let ((harmless-directory "/"))
            (should (equal (harmless-team-tools-test-signal
                            (lambda ()
                              (harmless-team-tools-boot
                               session '(:spec "starter"))))
                           "Refusing to store a team at /")))
          (let ((harmless-directory "c:/"))
            (should (equal (harmless-team-tools-test-signal
                            (lambda ()
                              (harmless-team-tools-boot
                               session '(:spec "starter"))))
                           "Refusing to store a team at c:/")))
          (let ((harmless-directory "  /  "))
            (should (equal (harmless-team-tools-test-signal
                            (lambda ()
                              (harmless-team-tools-boot
                               session '(:spec "starter"))))
                           "Refusing to store a team at /")))
          (dolist (empty (list nil "" "   " "\n"))
            (should (equal (harmless-team-tools-test-signal
                            (lambda ()
                              (harmless-team-tools-boot
                               session (list :spec empty))))
                           "team spec is empty")))
          (should (equal (harmless-team-tools-test-signal
                          (lambda ()
                            (harmless-team-tools-boot
                             session '(:spec 42))))
                         "team spec must be a string"))
          (dolist (bad (list "/" "c:/" "c:" "d:/"))
            (should (equal (harmless-team-tools-test-signal
                            (lambda ()
                              (harmless-team-tools-boot
                               session (list :spec bad))))
                           "team spec is the filesystem root")))
          (dolist (rel (list "rig.json" "starter/" "~" "~/work"))
            (should (equal (harmless-team-tools-test-signal
                            (lambda ()
                              (harmless-team-tools-boot
                               session (list :spec rel))))
                           "team spec must be absolute")))
          (should (equal home-work
                         (file-exists-p (expand-file-name "work" "~"))))
          (should-not (file-exists-p
                       (expand-file-name "rig.json" elsewhere)))
          (should-not (file-directory-p
                       (expand-file-name "teams" local)))
          (should (equal "team rig is empty"
                         (harmless-team-tools-test-signal
                          (lambda ()
                            (harmless-team-tools-inspect
                             session '(:rig ""))))))
          (should (equal "team rig is missing"
                         (harmless-team-tools-test-signal
                          (lambda ()
                            (harmless-team-tools-inspect
                             session '(:rig "starter"))))))
          (should (equal "team send is empty"
                         (harmless-team-tools-test-signal
                          (lambda ()
                            (harmless-team-tools-message
                             session '(:rig "starter" :target "dev.build"
                                       :text ""))))))
          (should (equal "team send must be a string"
                         (harmless-team-tools-test-signal
                          (lambda ()
                            (harmless-team-tools-message
                             session '(:rig "starter" :target "dev.build"
                                       :text 42))))))
          (should (equal "team rig is missing"
                         (harmless-team-tools-test-signal
                          (lambda ()
                            (harmless-team-tools-message
                             session '(:rig "starter" :target "dev.build"
                                       :text "Hi"))))))
          (should (equal "team snapshot is empty"
                         (harmless-team-tools-test-signal
                          (lambda ()
                            (harmless-team-tools-snapshot
                             session '(:rig "starter" :name ""))))))
          (should (equal "team snapshot must be a string"
                         (harmless-team-tools-test-signal
                          (lambda ()
                            (harmless-team-tools-snapshot
                             session '(:rig "starter" :name 42))))))
          (dolist (bad (list "/" "c:/" "dawn/" "a-b"))
            (should (equal (harmless-team-tools-test-signal
                            (lambda ()
                              (harmless-team-tools-snapshot
                               session (list :rig "starter" :name bad))))
                           "team snapshot is not a name")))
          (should (equal "team rig is missing"
                         (harmless-team-tools-test-signal
                          (lambda ()
                            (harmless-team-tools-snapshot
                             session '(:rig "starter" :name "dawn"))))))
          (should (equal "team snapshot is missing"
                         (harmless-team-tools-test-signal
                          (lambda ()
                            (harmless-team-tools-restore
                             session '(:rig "missing" :name "dawn"))))))
          (setq boot (harmless-json-decode
                      (harmless-team-tools-boot
                       session '(:spec "  starter  "))))
          (should (equal "starter" (plist-get boot :name)))
          (should (equal '(("dev-build@starter" "implementer")
                           ("dev-review@starter" "checker"))
                         (plist-get boot :seats)))
          (should (equal '("dev-build@starter" "dev-review@starter")
                         (plist-get boot :order)))
          (should (equal "delegates_to"
                         (plist-get (car (plist-get boot :edges)) :kind)))
          (should (equal "dev-build@starter"
                         (plist-get (car (plist-get boot :edges)) :from)))
          (should (equal "dev-review@starter"
                         (plist-get (car (plist-get boot :edges)) :to)))
          (should (equal (plist-get boot :name)
                         (plist-get (harmless-team-preview "starter")
                                    :name)))
          (should (equal (plist-get boot :seats)
                         (plist-get (harmless-team-preview "starter")
                                    :seats)))
          (should (equal (plist-get boot :order)
                         (plist-get (harmless-team-preview "starter")
                                    :order)))
          (should (file-name-absolute-p rig))
          (should (file-regular-p rig))
          (should (harmless-directory-strictly-under-p rig local))
          (should-not (harmless-filesystem-root-p rig))
          (should-not (equal (file-truename rig) (file-truename "/")))
          (should (string-search "dev-build@starter"
                                 (harmless-team-tools-test-read rig)))
          (let ((before (harmless-team-tools-test-read rig)))
            (should (equal "team rig already exists"
                           (harmless-team-tools-test-signal
                            (lambda ()
                              (harmless-team-tools-boot
                               session '(:spec "starter"))))))
            (should (equal before
                           (harmless-team-tools-test-read rig))))
          (let ((before (harmless-team-tools-test-read rig)))
            (setq seen (harmless-json-decode
                        (harmless-team-tools-inspect
                         session '(:rig " starter "))))
            (should (equal before
                           (harmless-team-tools-test-read rig))))
          (should (equal "starter" (plist-get seen :name)))
          (should (equal '("dev-build@starter" "dev-review@starter")
                         (mapcar (lambda (seat)
                                   (plist-get seat :address))
                                 (plist-get seen :seats))))
          (should (equal '("implementer" "checker")
                         (mapcar (lambda (seat)
                                   (plist-get seat :role))
                                 (plist-get seen :seats))))
          (should (equal '("dev-build@starter" "dev-review@starter")
                         (plist-get seen :order)))
          (should-not (plist-get seen :rows))
          (should (equal "team seat is not occupied"
                         (harmless-team-tools-test-signal
                          (lambda ()
                            (harmless-team-tools-message
                             session '(:rig "starter"
                                       :target "dev.build"
                                       :text "Ship it?"))))))
          (should (equal "team send is not a target"
                         (harmless-team-tools-test-signal
                          (lambda ()
                            (harmless-team-tools-message
                             session '(:rig "starter" :target "nope"
                                       :text "Ship it?"))))))
          (harmless-team-tools-test-write
           (expand-file-name "summary.json" hand)
           "{\"status\":\"idle\"}\n")
          (should (equal harmless-team-tools-test-id
                         (plist-get
                          (harmless-team-adopt
                           "starter" "dev" "build" "harmless"
                           harmless-team-tools-test-id)
                          :session)))
          (should (equal '("dev-build@starter")
                         (harmless-json-decode
                          (harmless-team-tools-message
                           session '(:rig "starter"
                                     :target "dev.build"
                                     :text "Ship it?")))))
          (should (file-name-absolute-p screen))
          (should (file-regular-p screen))
          (should (harmless-directory-strictly-under-p screen local))
          (should-not (equal (file-truename screen)
                             (file-truename "/")))
          (should (string-search "Ship it?"
                                 (harmless-team-tools-test-read screen)))
          (should (string-search "Ship it?"
                                 (harmless-team-tools-test-read rig)))
          (should-not (file-exists-p
                       (expand-file-name "messages.jsonl" elsewhere)))
          (let ((before (harmless-team-tools-test-read rig)))
            (should (equal "dawn"
                           (harmless-json-decode
                            (harmless-team-tools-snapshot
                             session '(:rig "starter" :name " dawn ")))))
            (should (equal before
                           (harmless-team-tools-test-read rig))))
          (should (file-name-absolute-p snap))
          (should (file-regular-p snap))
          (should (harmless-directory-strictly-under-p snap local))
          (should-not (harmless-filesystem-root-p snap))
          (should-not (equal (file-truename snap) (file-truename "/")))
          (should (string-search "dev-build@starter"
                                 (harmless-team-tools-test-read snap)))
          (let ((before (harmless-team-tools-test-read snap)))
            (should (equal "team snapshot already exists"
                           (harmless-team-tools-test-signal
                            (lambda ()
                              (harmless-team-tools-snapshot
                               session '(:rig "starter" :name "dawn"))))))
            (should (equal before
                           (harmless-team-tools-test-read snap))))
          (should (equal '("temporary")
                         (plist-get
                          (harmless-team-add-work
                           "starter" "dev" "build" "temporary")
                          :work)))
          (should (equal "team snapshot fresh must be a list"
                         (harmless-team-tools-test-signal
                          (lambda ()
                            (harmless-team-tools-restore
                             session '(:rig "starter" :name "dawn"
                                       :fresh 42))))))
          (should (equal '("temporary")
                         (plist-get
                          (harmless-team-seat "starter" "dev" "build")
                          :work)))
          (setq report (harmless-json-decode
                        (harmless-team-tools-restore
                         session '(:rig "starter" :name "dawn"))))
          (should (eq t (plist-get report :ok)))
          (should (equal '(("dev-build@starter" . "resumed")
                           ("dev-review@starter" . "rebuilt"))
                         (mapcar (lambda (seat)
                                   (cons (plist-get seat :seat)
                                         (plist-get seat :status)))
                                 (plist-get report :seats))))
          (should-not (plist-get
                       (harmless-team-seat "starter" "dev" "build")
                       :work))
          (should (string-search "Ship it?"
                                 (harmless-team-tools-test-read rig)))
          (setq report (harmless-json-decode
                        (harmless-team-tools-restore
                         session '(:rig "starter" :name "dawn"
                                   :fresh "dev.build"))))
          (should (eq t (plist-get report :ok)))
          (should (equal "fresh"
                         (plist-get (car (plist-get report :seats))
                                    :status)))
          (should (member (plist-get
                           (harmless-team-seat "starter" "dev" "build")
                           :session)
                          '(nil "")))
          (should-not (file-directory-p
                       (expand-file-name "teams" elsewhere)))
          (should (equal roots (harmless-team-tools-test-roots)))
          (should (equal home-work
                         (file-exists-p (expand-file-name "work" "~"))))
          (should (equal timers (copy-sequence timer-list)))
          (should (equal procs (copy-sequence (process-list))))
          (should (equal sched
                         (hash-table-count harmless-schedule--timers))))
      (ignore-errors (delete-directory parent t)))))
