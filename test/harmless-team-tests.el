;;; harmless-team-tests.el --- Tests for a rig of seats -*- lexical-binding: t; -*-

(require 'ert)
(require 'harmless)

(defconst harmless-team-test-session-a
  "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
  "One session id.")

(defconst harmless-team-test-session-b
  "11111111-2222-3333-4444-555555555555"
  "Another session id.")

(defun harmless-team-test-signal (fn)
  "Return the error text from FN."
  (error-message-string (should-error (funcall fn) :type 'error)))

(defun harmless-team-test-read (path)
  "Return PATH as text."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (buffer-string)))

(defun harmless-team-test-bytes (path)
  "Return PATH unchanged, for a later comparison."
  (harmless-team-test-read path))

(defun harmless-team-test-roots ()
  "Return existence of team files at filesystem roots."
  (list (file-exists-p "/teams")
        (file-exists-p "c:/teams")
        (file-exists-p "d:/teams")
        (file-exists-p "/workshop.json")
        (file-exists-p "c:/workshop.json")
        (file-exists-p "d:/workshop.json")))

(defun harmless-team-test-seat (session work)
  "Return the public plist for the workshop build seat."
  (list :rig "workshop" :pod "dev" :name "build"
        :role "implementer"
        :address "dev-build@workshop"
        :session session
        :work work))

(ert-deftest harmless-team-seat-keeps-its-work ()
  (let* ((base (make-temp-file "harmless-team-" t))
         (elsewhere (expand-file-name "elsewhere" base))
         (harmless-directory (file-name-as-directory base))
         (default-directory (file-name-as-directory elsewhere))
         (file (expand-file-name "teams/workshop.json" base))
         (other (expand-file-name "teams/first-project.json" base))
         (roots (harmless-team-test-roots)))
    (make-directory elsewhere t)
    (should (equal (harmless-team-address " workshop " " dev " " build ")
                   "dev-build@workshop"))
    (should-not (file-directory-p (expand-file-name "teams" base)))
    (should (equal (harmless-team-add-seat
                    " workshop " " dev " " build " " implementer ")
                   (harmless-team-test-seat "" nil)))
    (should (file-regular-p file))
    (should (file-name-absolute-p file))
    (should (harmless-directory-strictly-under-p file base))
    (should-not (equal (file-truename file) (file-truename "/")))
    (should-not (file-exists-p (expand-file-name "teams" elsewhere)))
    (should-not (file-exists-p (expand-file-name "notes" elsewhere)))
    (should (equal (harmless-team-add-work
                    "workshop" "dev" "build" "  fix the door  ")
                   (harmless-team-test-seat "" '("fix the door"))))
    (should (equal (harmless-team-add-work
                    "workshop" "dev" "build" "keep\nthe tests")
                   (harmless-team-test-seat
                    "" '("fix the door" "keep\nthe tests"))))
    (should (equal (harmless-team-occupy
                    "workshop" "dev" "build"
                    (concat "  " harmless-team-test-session-a "  "))
                   (harmless-team-test-seat
                    harmless-team-test-session-a
                    '("fix the door" "keep\nthe tests"))))
    (should (equal (harmless-team-occupy
                    "workshop" "dev" "build"
                    harmless-team-test-session-b)
                   (harmless-team-test-seat
                    harmless-team-test-session-b
                    '("fix the door" "keep\nthe tests"))))
    (should (equal (harmless-team-seat "workshop" "dev" "build")
                   (harmless-team-test-seat
                    harmless-team-test-session-b
                    '("fix the door" "keep\nthe tests"))))
    (let ((text (harmless-team-test-read file)))
      (should (string-search "dev-build@workshop" text))
      (should (string-search "implementer" text))
      (should (string-search "fix the door" text))
      (should (string-search "keep\\nthe tests" text))
      (should (string-search harmless-team-test-session-b text))
      (should-not (string-search harmless-team-test-session-a text)))
    (harmless-team-add-seat "workshop" "qa" "check" "checker")
    (should (equal (harmless-team-seat "workshop" "dev" "build")
                   (harmless-team-test-seat
                    harmless-team-test-session-b
                    '("fix the door" "keep\nthe tests"))))
    (let ((slash (harmless-team-add-seat
                  "first-project" "dev" "build" "/")))
      (should (equal (plist-get slash :address) "dev-build@first-project"))
      (should (equal (plist-get slash :role) "/"))
      (should (equal (plist-get slash :session) ""))
      (should (equal (plist-get slash :work) nil))
      (should (file-regular-p other))
      (should (file-name-absolute-p other))
      (should (harmless-directory-strictly-under-p other base))
      (should-not (equal (file-truename other) (file-truename "/")))
      (let ((text (harmless-team-test-read other)))
        (should (string-search "dev-build@first-project" text))
        (should (string-search "\"/\"" text))))
    (when (eq system-type 'windows-nt)
      (let ((bytes (harmless-team-test-bytes file)))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-add-seat
                           "Workshop" "Dev" "Build" "other")))
                       "team seat already exists"))
        (should (equal (harmless-team-test-bytes file) bytes)))
      (let ((review (harmless-team-add-seat
                     "workshop" "dev" "review" "checker")))
        (should (equal (plist-get review :pod) "dev"))
        (should (equal (plist-get review :rig) "workshop"))
        (should (equal (plist-get review :address)
                       "dev-review@workshop"))))
    (should (equal roots (harmless-team-test-roots)))))

(ert-deftest harmless-team-refuses-a-bad-request ()
  (let* ((base (make-temp-file "harmless-team-bad-" t))
         (file (expand-file-name "teams/workshop.json" base))
         (roots (harmless-team-test-roots)))
    (let ((harmless-directory "/"))
      (dolist (bad (list nil "" "   " "\n"))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-address bad "dev" "build")))
                       "team rig is empty")))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-address 42 "dev" "build")))
                     "team rig must be a string"))
      (dolist (bad (list "/" "c:/" "d:/" "c:\\" "C:/" "c:" "\\" "//" "/."
                         "notes/" "sub/" "~" "~/r"
                         "a/b" "a\\b" ".." "." "first--project"
                         "-rig" "rig-" "id one"))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-address bad "dev" "build")))
                       "team rig is not a name")))
      (dolist (bad (list nil "" "   "))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-address "workshop" bad "build")))
                       "team pod is empty")))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-address "workshop" 42 "build")))
                     "team pod must be a string"))
      (dolist (bad (list "dev-ops" "/" "c:" "~" ".." "a/b" "sub/"))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-address "workshop" bad "build")))
                       "team pod is not a name")))
      (dolist (bad (list nil "" "   "))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-address "workshop" "dev" bad)))
                       "team seat is empty")))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-address "workshop" "dev" 42)))
                     "team seat must be a string"))
      (dolist (bad (list "build-1" "/" "c:" "~" ".." "a/b"))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-address "workshop" "dev" bad)))
                       "team seat is not a name")))
      (dolist (bad (list nil "" "   "))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-add-seat
                           "workshop" "dev" "build" bad)))
                       "team role is empty")))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-add-seat
                         "workshop" "dev" "build" 42)))
                     "team role must be a string"))
      (let ((harmless-directory "   "))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-add-seat
                           "workshop" "dev" "build" "implementer")))
                       "Refusing to store a team at ")))
      (dolist (dir (list "/" "c:/" "d:/" "c:\\" "C:/" "c:"
                         "\\" "//" "/." " / " " c:/ "))
        (let ((harmless-directory dir))
          (should (equal (harmless-team-test-signal
                          (lambda ()
                            (harmless-team-add-seat
                             "workshop" "dev" "build" "implementer")))
                         (concat "Refusing to store a team at "
                                 (string-trim dir))))))
      (let ((harmless-directory "notes"))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-add-seat
                           "workshop" "dev" "build" "implementer")))
                       "Harmless data directory must be absolute, not \"notes\""))))
    (should (equal roots (harmless-team-test-roots)))
    (should-not (file-directory-p (expand-file-name "teams" base)))
    (should-not (file-exists-p (expand-file-name "notes" base)))
    (let ((harmless-directory (file-name-as-directory base)))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-seat "workshop" "dev" "build")))
                     "team rig is missing"))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-occupy
                         "workshop" "dev" "build"
                         harmless-team-test-session-a)))
                     "team rig is missing"))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-add-work
                         "workshop" "dev" "build" "x")))
                     "team rig is missing"))
      (should-not (file-directory-p (expand-file-name "teams" base)))
      (harmless-team-add-seat "workshop" "dev" "build" "implementer")
      (let ((bytes (harmless-team-test-bytes file)))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-add-seat
                           "workshop" "dev" "build" "other")))
                       "team seat already exists"))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-occupy
                           "workshop" "dev" "other"
                           harmless-team-test-session-a)))
                       "team seat is missing"))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-add-work
                           "workshop" "qa" "build" "x")))
                       "team seat is missing"))
        (dolist (bad (list nil "" "   "))
          (should (equal (harmless-team-test-signal
                          (lambda ()
                            (harmless-team-occupy
                             "workshop" "dev" "build" bad)))
                         "team session is empty")))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-occupy
                           "workshop" "dev" "build" 42)))
                       "team session must be a string"))
        (dolist (bad (list "/" "c:/" "c:" "not-a-uuid"
                           "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeee"))
          (should (equal (harmless-team-test-signal
                          (lambda ()
                            (harmless-team-occupy
                             "workshop" "dev" "build" bad)))
                         "team session is not an id")))
        (dolist (bad (list nil "" "   "))
          (should (equal (harmless-team-test-signal
                          (lambda ()
                            (harmless-team-add-work
                             "workshop" "dev" "build" bad)))
                         "team work is empty")))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-add-work
                           "workshop" "dev" "build" 42)))
                       "team work must be a string"))
        (should (equal (harmless-team-test-bytes file) bytes)))
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region "{" nil file nil 'silent))
      (let ((bytes (harmless-team-test-bytes file)))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-occupy
                           "workshop" "dev" "build"
                           harmless-team-test-session-a)))
                       "team rig is not valid"))
        (should (equal (harmless-team-test-bytes file) bytes)))
      (harmless-team-add-seat "sample" "dev" "build" "implementer")
      (let ((path (expand-file-name "teams/sample.json" base)))
        (with-temp-buffer
          (let ((coding-system-for-read 'utf-8-unix))
            (insert-file-contents path))
          (goto-char (point-min))
          (unless (search-forward "dev-build@sample" nil t)
            (error "fixture address missing"))
          (replace-match "dev-build@other" t t)
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region (buffer-string) nil path nil 'silent)))
        (let ((bytes (harmless-team-test-bytes path)))
          (should (equal (harmless-team-test-signal
                          (lambda ()
                            (harmless-team-seat "sample" "dev" "build")))
                         "team rig is not valid"))
          (should (equal (harmless-team-test-signal
                          (lambda ()
                            (harmless-team-add-seat
                             "sample" "dev" "qa" "checker")))
                         "team rig is not valid"))
          (should (equal (harmless-team-test-bytes path) bytes))))
      (let ((path (expand-file-name "teams/dup.json" base))
            (text (concat "{\"name\":\"dup\",\"pods\":["
                          "{\"name\":\"dev\",\"seats\":[]},"
                          "{\"name\":\"dev\",\"seats\":[]}]}")))
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region text nil path nil 'silent))
        (let ((bytes (harmless-team-test-bytes path)))
          (should (equal (harmless-team-test-signal
                          (lambda ()
                            (harmless-team-seat "dup" "dev" "build")))
                         "team rig is not valid"))
          (should (equal (harmless-team-test-bytes path) bytes))))
      (should-not (file-exists-p (expand-file-name "notes" base))))
    (should (equal roots (harmless-team-test-roots)))))

(ert-deftest harmless-team-edges-decide-launch-order ()
  (let* ((base (make-temp-file "harmless-team-edge-" t))
         (elsewhere (expand-file-name "elsewhere" base))
         (harmless-directory (file-name-as-directory base))
         (default-directory (file-name-as-directory elsewhere))
         (file (expand-file-name "teams/workshop.json" base))
         (roots (harmless-team-test-roots))
         (build "dev-build@workshop")
         (review "dev-review@workshop")
         (note "qa-note@workshop"))
    (make-directory elsewhere t)
    (let ((harmless-directory "/"))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-edge
                         "workshop" "owns" "dev.build" "dev.review")))
                     "team edge is not a kind"))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-edge
                         "workshop" "delegates_to"
                         "dev.build" "dev.review")))
                     "Refusing to store a team at /")))
    (should (equal (harmless-team-test-signal
                    (lambda ()
                      (harmless-team-edges "workshop")))
                   "team rig is missing"))
    (harmless-team-add-seat "workshop" "dev" "build" "implementer")
    (harmless-team-add-seat "workshop" "dev" "review" "checker")
    (harmless-team-add-seat "workshop" "qa" "note" "notes")
    (should (equal (harmless-team-launch-order "workshop")
                   (list build review note)))
    (should (equal (harmless-team-edges "workshop") nil))
    (should (equal (harmless-team-edge
                    "workshop" " delegates_to " " qa.note " build)
                   (list :kind "delegates_to" :from note :to build)))
    (should (equal (harmless-team-launch-order "workshop")
                   (list review note build)))
    (let ((bytes (harmless-team-test-bytes file)))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-edge
                         "workshop" "delegates_to" "dev.build" "qa.note")))
                     "team edge is a cycle"))
      (should (equal (harmless-team-test-bytes file) bytes))
      (should (equal (harmless-team-launch-order "workshop")
                     (list review note build))))
    (should (equal (harmless-team-edge
                    "workshop" "spawned_by" "dev.review" "qa.note")
                   (list :kind "spawned_by" :from review :to note)))
    (should (equal (harmless-team-launch-order "workshop")
                   (list note build review)))
    (let ((order (harmless-team-launch-order "workshop")))
      (should (equal (harmless-team-edge
                      "workshop" "can_observe" "qa.note" "dev.build")
                     (list :kind "can_observe" :from note :to build)))
      (should (equal (harmless-team-edge
                      "workshop" "collaborates_with" build "qa.note")
                     (list :kind "collaborates_with"
                           :from build :to note)))
      (should (equal (harmless-team-edge
                      "workshop" "escalates_to" "qa.note" "dev.review")
                     (list :kind "escalates_to" :from note :to review)))
      (should (equal (harmless-team-launch-order "workshop") order)))
    (should (equal (harmless-team-edges "workshop")
                   (list
                    (list :kind "delegates_to" :from note :to build)
                    (list :kind "spawned_by" :from review :to note)
                    (list :kind "can_observe" :from note :to build)
                    (list :kind "collaborates_with" :from build :to note)
                    (list :kind "escalates_to" :from note :to review))))
    (let ((text (harmless-team-test-read file)))
      (dolist (kind '("delegates_to" "spawned_by" "can_observe"
                      "collaborates_with" "escalates_to"))
        (should (string-search kind text)))
      (should (string-search build text))
      (should (string-search review text))
      (should (string-search note text)))
    (should (file-name-absolute-p file))
    (should (harmless-directory-strictly-under-p file base))
    (should-not (equal (file-truename file) (file-truename "/")))
    (should-not (file-exists-p (expand-file-name "teams" elsewhere)))
    (let ((bytes (harmless-team-test-bytes file)))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-edge
                         "workshop" "delegates_to" "dev.build" "dev.build")))
                     "team edge is the same seat"))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-edge
                         "workshop" "delegates_to"
                         "qa-note@workshop" "dev-build@workshop")))
                     "team edge already exists"))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-edge
                         "workshop" "delegates_to" "dev.absent" "dev.build")))
                     "team seat is missing"))
      (dolist (bad (list "/" "notes" "notes/" "dev-build@other" "~"))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-edge
                           "workshop" "delegates_to" bad "dev.build")))
                       "team edge is not a seat")))
      (dolist (bad (list nil "" "   "))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-edge
                           "workshop" bad "dev.build" "dev.review")))
                       "team edge is empty"))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-edge
                           "workshop" "delegates_to" bad "dev.build")))
                       "team edge is empty")))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-edge
                         "workshop" 42 "dev.build" "dev.review")))
                     "team edge must be a string"))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-edge
                         "workshop" "delegates_to" 42 "dev.build")))
                     "team edge must be a string"))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-edge
                         "workshop" "owns" "dev.build" "dev.review")))
                     "team edge is not a kind"))
      (should (equal (harmless-team-test-bytes file) bytes)))
    (when (eq system-type 'windows-nt)
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-edge
                         "Workshop" "delegates_to" "Qa.Note" "Dev.Build")))
                     "team edge already exists")))
    (should (equal roots (harmless-team-test-roots)))))
