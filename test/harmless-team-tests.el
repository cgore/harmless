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

(defun harmless-team-test-starter-plan ()
  "Return the preview plist of the built-in starter."
  (list :name "starter"
        :seats (list (list "dev-build@starter" "implementer")
                     (list "dev-review@starter" "checker"))
        :edges (list (list :kind "delegates_to"
                           :from "dev-build@starter"
                           :to "dev-review@starter"))
        :order (list "dev-build@starter"
                     "dev-review@starter")))

(defun harmless-team-test-write (path text)
  "Write TEXT to PATH and return PATH."
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil path nil 'silent))
  path)

(defun harmless-team-test-session-roots ()
  "Return existence of session directories at filesystem roots."
  (list (file-exists-p "/sessions")
        (file-exists-p "c:/sessions")
        (file-exists-p "d:/sessions")))

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

(ert-deftest harmless-team-boot-previews-and-removes ()
  (let* ((base (make-temp-file "harmless-team-boot-" t))
         (elsewhere (expand-file-name "elsewhere" base))
         (specs (expand-file-name "specs" base))
         (bundle (expand-file-name "bundle" base))
         (empty (expand-file-name "emptybundle" base))
         (harmless-directory (file-name-as-directory base))
         (default-directory (file-name-as-directory elsewhere))
         (teams (expand-file-name "teams" base))
         (file (expand-file-name "starter.json" teams))
         (roots (harmless-team-test-roots))
         (plan (harmless-team-test-starter-plan))
         (cycle (expand-file-name "cycle.json" specs))
         (self (expand-file-name "self.json" specs))
         (ghost (expand-file-name "ghost.json" specs))
         (kind (expand-file-name "kind.json" specs))
         (dup (expand-file-name "dup.json" specs))
         (brace (expand-file-name "brace.json" specs))
         (paper (expand-file-name "paper.json" specs))
         (paper-plan
          (list :name "paper"
                :seats (list (list "dev-build@paper" "implementer")
                             (list "dev-review@paper" "checker"))
                :edges (list (list :kind "spawned_by"
                                   :from "dev-build@paper"
                                   :to "dev-review@paper"))
                :order (list "dev-review@paper" "dev-build@paper")))
         (kit-plan
          (list :name "kit"
                :seats (list (list "ops-run@kit" "runner"))
                :edges nil
                :order (list "ops-run@kit"))))
    (make-directory elsewhere t)
    (make-directory specs t)
    (make-directory bundle t)
    (make-directory empty t)
    (harmless-team-test-write
     cycle
     (concat "{\"name\":\"cycle\",\"pods\":[{\"name\":\"dev\","
             "\"seats\":[{\"name\":\"build\",\"role\":\"implementer\"},"
             "{\"name\":\"review\",\"role\":\"checker\"}]}],"
             "\"edges\":[{\"kind\":\"delegates_to\","
             "\"from\":\"dev.build\",\"to\":\"dev.review\"},"
             "{\"kind\":\"delegates_to\",\"from\":\"dev.review\","
             "\"to\":\"dev.build\"}]}"))
    (harmless-team-test-write
     self
     (concat "{\"name\":\"self\",\"pods\":[{\"name\":\"dev\","
             "\"seats\":[{\"name\":\"build\",\"role\":\"implementer\"},"
             "{\"name\":\"review\",\"role\":\"checker\"}]}],"
             "\"edges\":[{\"kind\":\"delegates_to\","
             "\"from\":\"dev.build\",\"to\":\"dev.build\"}]}"))
    (harmless-team-test-write
     ghost
     (concat "{\"name\":\"ghost\",\"pods\":[{\"name\":\"dev\","
             "\"seats\":[{\"name\":\"build\",\"role\":\"implementer\"}]}],"
             "\"edges\":[{\"kind\":\"delegates_to\","
             "\"from\":\"dev.absent\",\"to\":\"dev.build\"}]}"))
    (harmless-team-test-write
     kind
     (concat "{\"name\":\"kind\",\"pods\":[{\"name\":\"dev\","
             "\"seats\":[{\"name\":\"build\",\"role\":\"implementer\"},"
             "{\"name\":\"review\",\"role\":\"checker\"}]}],"
             "\"edges\":[{\"kind\":\"owns\",\"from\":\"dev.build\","
             "\"to\":\"dev.review\"}]}"))
    (harmless-team-test-write
     dup
     (concat "{\"name\":\"dup\",\"pods\":[{\"name\":\"dev\","
             "\"seats\":[{\"name\":\"build\",\"role\":\"implementer\"},"
             "{\"name\":\"build\",\"role\":\"other\"}]}]}"))
    (harmless-team-test-write brace "{")
    (harmless-team-test-write
     paper
     (concat "{\"name\":\"paper\",\"pods\":[{\"name\":\"dev\","
             "\"seats\":[{\"name\":\"build\",\"role\":\" implementer \"},"
             "{\"name\":\"review\",\"role\":\"checker\"}]}],"
             "\"edges\":[{\"kind\":\"spawned_by\","
             "\"from\":\"dev.build\",\"to\":\"dev.review\"}]}"))
    (harmless-team-test-write
     (expand-file-name "rig.json" bundle)
     (concat "{\"name\":\"kit\",\"pods\":[{\"name\":\"ops\","
             "\"seats\":[{\"name\":\"run\",\"role\":\"runner\"}]}]}"))
    (should (equal (let ((harmless-directory "/"))
                     (harmless-team-preview "starter"))
                   plan))
    (should (equal (harmless-team-preview " starter ") plan))
    (should-not (file-directory-p teams))
    (should (equal (harmless-team-test-signal
                    (lambda ()
                      (let ((harmless-directory "/"))
                        (harmless-team-boot "starter"))))
                   "Refusing to store a team at /"))
    (should (equal (harmless-team-test-signal
                    (lambda ()
                      (let ((harmless-directory "c:"))
                        (harmless-team-boot "starter"))))
                   "Refusing to store a team at c:"))
    (should (equal (harmless-team-test-signal
                    (lambda ()
                      (let ((harmless-directory " c:/ "))
                        (harmless-team-boot "starter"))))
                   "Refusing to store a team at c:/"))
    (dolist (bad (list "/" "c:/" " / " " c:/ " "c:" "\\"))
      (should (equal (harmless-team-test-signal
                      (lambda () (harmless-team-boot bad)))
                     "team spec is the filesystem root")))
    (dolist (bad (list "~" "notes" "rig.json" "sub/" "  notes  "))
      (should (equal (harmless-team-test-signal
                      (lambda () (harmless-team-boot bad)))
                     "team spec must be absolute")))
    (dolist (bad (list nil "" "   "))
      (should (equal (harmless-team-test-signal
                      (lambda () (harmless-team-boot bad)))
                     "team spec is empty")))
    (should (equal (harmless-team-test-signal
                    (lambda () (harmless-team-boot 42)))
                   "team spec must be a string"))
    (should (equal (harmless-team-test-signal
                    (lambda ()
                      (harmless-team-boot
                       (expand-file-name "missing.json" specs))))
                   "team spec is missing"))
    (should (equal (harmless-team-test-signal
                    (lambda () (harmless-team-boot empty)))
                   "team spec is missing"))
    (dolist (bad (list cycle self ghost kind dup brace))
      (let ((bytes (harmless-team-test-bytes bad)))
        (should (equal (harmless-team-test-signal
                        (lambda () (harmless-team-boot bad)))
                       "team spec is not valid"))
        (should (equal (harmless-team-test-bytes bad) bytes))))
    (should (equal (harmless-team-test-signal
                    (lambda ()
                      (harmless-team-remove "starter" "dev" "review")))
                   "team rig is missing"))
    (should-not (file-directory-p teams))
    (should-not (file-exists-p (expand-file-name "notes" elsewhere)))
    (should-not (file-exists-p (expand-file-name "sub" elsewhere)))
    (should-not (file-exists-p (expand-file-name "rig.json" elsewhere)))
    (should-not (file-exists-p (expand-file-name "notes" base)))
    (should (equal (harmless-team-boot "starter") plan))
    (should (file-regular-p file))
    (should (file-name-absolute-p file))
    (should (harmless-directory-strictly-under-p file base))
    (should-not (equal (file-truename file) (file-truename "/")))
    (let ((text (harmless-team-test-read file)))
      (should (string-search "dev-build@starter" text))
      (should (string-search "dev-review@starter" text))
      (should (string-search "delegates_to" text))
      (should (string-search "implementer" text)))
    (should (equal (harmless-team-edges "starter")
                   (plist-get plan :edges)))
    (should (equal (harmless-team-launch-order "starter")
                   (plist-get plan :order)))
    (let ((bytes (harmless-team-test-bytes file)))
      (should (equal (harmless-team-preview "starter") plan))
      (should (equal (harmless-team-test-signal
                      (lambda () (harmless-team-boot " starter ")))
                     "team rig already exists"))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-grow
                         "starter" "dev" "build" "implementer")))
                     "team seat already exists"))
      (should (equal (harmless-team-test-bytes file) bytes)))
    (when (eq system-type 'windows-nt)
      (let ((bytes (harmless-team-test-bytes file))
            (cased (expand-file-name "cased.json" specs)))
        (harmless-team-test-write
         cased
         (concat "{\"name\":\"Starter\",\"pods\":[{\"name\":\"qa\","
                 "\"seats\":[{\"name\":\"note\",\"role\":\"notes\"}]}]}"))
        (let ((cased-bytes (harmless-team-test-bytes cased)))
          (should (equal (harmless-team-test-signal
                          (lambda () (harmless-team-boot cased)))
                         "team rig already exists"))
          (should (equal (harmless-team-test-bytes file) bytes))
          (should (equal (harmless-team-test-bytes cased) cased-bytes)))))
    (should (equal (harmless-team-grow "starter" "qa" "note" "notes")
                   (list :rig "starter" :pod "qa" :name "note"
                         :role "notes" :address "qa-note@starter"
                         :session "" :work nil)))
    (should (equal (plist-get (harmless-team-seat "starter" "dev" "build")
                              :role)
                   "implementer"))
    (should (equal (harmless-team-edges "starter")
                   (plist-get plan :edges)))
    (should (equal (harmless-team-launch-order "starter")
                   (list "dev-build@starter" "dev-review@starter"
                         "qa-note@starter")))
    (should (equal (harmless-team-remove "starter" "dev" "build")
                   (list :removed "dev-build@starter"
                         :fallback nil :work nil)))
    (should (equal (harmless-team-test-signal
                    (lambda ()
                      (harmless-team-seat "starter" "dev" "build")))
                   "team seat is missing"))
    (should (equal (harmless-team-edges "starter") nil))
    (should-not (string-search "delegates_to"
                               (harmless-team-test-read file)))
    (should-not (string-search "dev-build@starter"
                               (harmless-team-test-read file)))
    (should (equal (harmless-team-launch-order "starter")
                   (list "dev-review@starter" "qa-note@starter")))
    (harmless-team-add-work "starter" "dev" "review" "keep the tests")
    (let ((bytes (harmless-team-test-bytes file)))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-remove "starter" "dev" "review")))
                     "team seat holds work"))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-remove
                         "starter" "dev" "review" "dev.review")))
                     "team fallback is the same seat"))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-remove
                         "starter" "dev" "review" "/")))
                     "team edge is not a seat"))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-remove
                         "starter" "dev" "review" "qa.absent")))
                     "team seat is missing"))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-remove
                         "starter" "dev" "review" "")))
                     "team edge is empty"))
      (should (equal (harmless-team-test-bytes file) bytes)))
    (should (equal (harmless-team-remove
                    "starter" "dev" "review" "qa.note")
                   (list :removed "dev-review@starter"
                         :fallback "qa-note@starter"
                         :work '("keep the tests"))))
    (should (equal (harmless-team-seat "starter" "qa" "note")
                   (list :rig "starter" :pod "qa" :name "note"
                         :role "notes" :address "qa-note@starter"
                         :session "" :work '("keep the tests"))))
    (should (equal (harmless-team-test-signal
                    (lambda ()
                      (harmless-team-seat "starter" "dev" "review")))
                   "team seat is missing"))
    (should (equal (harmless-team-launch-order "starter")
                   (list "qa-note@starter")))
    (should (string-search "keep the tests"
                           (harmless-team-test-read file)))
    (should-not (string-search "dev-review@starter"
                               (harmless-team-test-read file)))
    (let ((bytes (harmless-team-test-bytes file)))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-remove "starter" "qa" "note")))
                     "team seat holds work"))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-remove
                         "starter" "qa" "note" "qa.note")))
                     "team fallback is the same seat"))
      (should (equal (harmless-team-test-bytes file) bytes)))
    (let ((bundle-file (expand-file-name "rig.json" bundle))
          (kit (expand-file-name "kit.json" teams))
          (paper-file (expand-file-name "paper.json" teams)))
      (let ((bundle-bytes (harmless-team-test-bytes bundle-file))
            (paper-bytes (harmless-team-test-bytes paper)))
        (should (equal (harmless-team-preview bundle) kit-plan))
        (should-not (file-exists-p kit))
        (should (equal (harmless-team-boot
                        (file-name-as-directory bundle))
                       kit-plan))
        (should (equal (harmless-team-test-bytes bundle-file)
                       bundle-bytes))
        (should (file-regular-p kit))
        (should (file-name-absolute-p kit))
        (should (harmless-directory-strictly-under-p kit base))
        (should-not (equal (file-truename kit) (file-truename "/")))
        (should (equal (harmless-team-test-signal
                        (lambda () (harmless-team-boot bundle)))
                       "team rig already exists"))
        (should (equal (harmless-team-remove "kit" "ops" "run")
                       (list :removed "ops-run@kit"
                             :fallback nil :work nil)))
        (should (file-regular-p kit))
        (should (equal (harmless-team-launch-order "kit") nil))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-seat "kit" "ops" "run")))
                       "team seat is missing"))
        (should-not (string-search "ops-run@kit"
                                   (harmless-team-test-read kit)))
        (should (equal (harmless-team-preview paper) paper-plan))
        (should-not (file-exists-p paper-file))
        (should (equal (harmless-team-boot paper) paper-plan))
        (should (equal (harmless-team-test-bytes paper) paper-bytes))
        (should (equal (harmless-team-launch-order "paper")
                       (plist-get paper-plan :order)))
        (should (string-search "spawned_by"
                               (harmless-team-test-read paper-file)))
        (should (file-name-absolute-p paper-file))
        (should (harmless-directory-strictly-under-p paper-file base))
        (should-not (equal (file-truename paper-file)
                           (file-truename "/")))))
    (should-not (file-exists-p (expand-file-name "teams" elsewhere)))
    (should (equal harmless-team--starter
                   (list :name "starter"
                         :pods (list
                                (list :name "dev"
                                      :seats
                                      (list (list :name "build"
                                                  :role "implementer")
                                            (list :name "review"
                                                  :role "checker"))))
                         :edges (list
                                 (list :kind "delegates_to"
                                       :from "dev.build"
                                       :to "dev.review")))))
    (should (equal roots (harmless-team-test-roots)))))

(ert-deftest harmless-team-adopts-and-releases-without-killing ()
  (let* ((base (make-temp-file "harmless-team-adopt-" t))
         (elsewhere (expand-file-name "elsewhere" base))
         (harmless-directory (file-name-as-directory base))
         (default-directory (file-name-as-directory elsewhere))
         (teams (expand-file-name "teams" base))
         (file (expand-file-name "teams/workshop.json" base))
         (claude (expand-file-name "claude-session" base))
         (codex (expand-file-name "codex-session.json" base))
         (pi (expand-file-name "pi-session" base))
         (logf (expand-file-name "server.log" base))
         (id harmless-team-test-session-b)
         (hand (expand-file-name (concat "sessions/hand/" id) base))
         (summary (expand-file-name "summary.json" hand))
         (dup (expand-file-name
               (concat "sessions/other/" harmless-team-test-session-a)
               base))
         (roots (harmless-team-test-roots))
         (session-roots (harmless-team-test-session-roots))
         (work '("keep the tests"))
         (edge (list (list :kind "delegates_to"
                           :from "dev-build@workshop"
                           :to "qa-note@workshop")))
         (pid (number-to-string (emacs-pid))))
    (make-directory elsewhere t)
    (make-directory claude t)
    (make-directory pi t)
    (harmless-team-test-write
     (expand-file-name "session.txt" claude) "claude")
    (harmless-team-test-write
     (expand-file-name "session.txt" pi) "pi")
    (harmless-team-test-write codex "codex")
    (harmless-team-test-write logf "log line\n")
    (let ((log-bytes (harmless-team-test-bytes logf))
          (codex-bytes (harmless-team-test-bytes codex)))
      (dolist (bad (list nil "" "   "))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-adopt
                           "workshop" "dev" "build" bad logf)))
                       "team runtime is empty")))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-adopt
                         "workshop" "dev" "build" 42 logf)))
                     "team runtime must be a string"))
      (dolist (bad (list "shell" "Harmless" "notes"))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-adopt
                           "workshop" "dev" "build" bad logf)))
                       "team runtime is not a runtime")))
      (let ((harmless-directory "/"))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-adopt
                           "workshop" "dev" "build" "nope" logf)))
                       "team runtime is not a runtime")))
      (dolist (bad (list nil "" "   "))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-adopt
                           "workshop" "dev" "build" "claude" bad)))
                       "team session is empty")))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-adopt
                         "workshop" "dev" "build" "claude" 42)))
                     "team session must be a string"))
      (dolist (bad (list "~" "notes" "sub/" "  notes  " "rig.json"))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-adopt
                           "workshop" "dev" "build" "claude" bad)))
                       "team session must be absolute")))
      (dolist (bad (list "/" "c:/" " / " " c:/ " "c:" "\\"))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-adopt
                           "workshop" "dev" "build" "claude" bad)))
                       "team session is the filesystem root")))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-adopt
                         "workshop" "dev" "build" "claude"
                         (expand-file-name "missing-log" base))))
                     "team session is missing"))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-adopt
                         "workshop" "dev" "build" "harmless" "not-an-id")))
                     "team session is not an id"))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-adopt
                         "workshop" "dev" "build" "terminal" "0")))
                     "team session is not running"))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-adopt
                         "workshop" "dev" "build" "terminal"
                         "2147483647")))
                     "team session is not running"))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-adopt
                         "absent" "dev" "build" "terminal" logf)))
                     "team rig is missing"))
      (let ((harmless-directory "/"))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-adopt
                           "workshop" "dev" "build" "terminal" logf)))
                       "Refusing to store a team at /")))
      (let ((harmless-directory "c:"))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-adopt
                           "workshop" "dev" "build" "terminal" logf)))
                       "Refusing to store a team at c:")))
      (should-not (file-directory-p teams))
      (should-not (file-exists-p (expand-file-name "notes" elsewhere)))
      (should-not (file-exists-p (expand-file-name "sub" elsewhere)))
      (should-not (file-directory-p (expand-file-name "sessions" base)))
      (should (equal (harmless-team-test-bytes logf) log-bytes))
      (harmless-team-add-seat "workshop" "dev" "build" "implementer")
      (harmless-team-add-seat "workshop" "qa" "note" "notes")
      (harmless-team-add-work "workshop" "dev" "build" "keep the tests")
      (harmless-team-edge
       "workshop" "delegates_to" "dev.build" "qa.note")
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-release "workshop" "dev" "build")))
                     "team seat is not occupied"))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-adopt
                         "workshop" "dev" "gone" "terminal" logf)))
                     "team seat is missing"))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-adopt
                         "workshop" "dev" "build" " harmless " id)))
                     "team session is missing"))
      (should-not (file-directory-p (expand-file-name "sessions" base)))
      (make-directory hand t)
      (make-directory dup t)
      (harmless-team-test-write summary (concat "{\"id\":\"" id "\"}"))
      (harmless-team-test-write
       (expand-file-name "summary.json" dup) "{\"id\":\"dup\"}")
      (make-directory
       (expand-file-name
        (concat "sessions/third/" harmless-team-test-session-a) base)
       t)
      (harmless-team-test-write
       (expand-file-name
        "summary.json"
        (expand-file-name
         (concat "sessions/third/" harmless-team-test-session-a) base))
       "{\"id\":\"dup2\"}")
      (let ((bytes (harmless-team-test-bytes file))
            (hand-bytes (harmless-team-test-bytes summary)))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-adopt
                           "workshop" "dev" "build" "harmless"
                           harmless-team-test-session-a)))
                       "team session is not one session"))
        (should (equal (harmless-team-test-bytes file) bytes))
        (should (equal (harmless-team-test-bytes summary) hand-bytes))
        (should (file-directory-p dup))
        (should (equal (harmless-team-adopt
                        "workshop" "dev" "build" " harmless "
                        (concat "  " id "  "))
                       (append (harmless-team-test-seat id work)
                               (list :runtime "harmless"))))
        (should (equal (harmless-team-edges "workshop") edge))
        (should (equal (harmless-team-adopt
                        "workshop" "dev" "build" "claude"
                        (file-name-as-directory claude))
                       (append (harmless-team-test-seat claude work)
                               (list :runtime "claude"))))
        (should (file-regular-p summary))
        (should (equal (harmless-team-test-bytes summary) hand-bytes))
        (should (equal (harmless-team-adopt
                        "workshop" "dev" "build" "codex" codex)
                       (append (harmless-team-test-seat codex work)
                               (list :runtime "codex"))))
        (should (file-directory-p claude))
        (should (file-regular-p
                 (expand-file-name "session.txt" claude)))
        (should (equal (harmless-team-adopt
                        "workshop" "dev" "build" "pi" pi)
                       (append (harmless-team-test-seat pi work)
                               (list :runtime "pi"))))
        (should (equal (harmless-team-test-bytes codex) codex-bytes))
        (should (equal (harmless-team-adopt
                        "workshop" "dev" "build" "terminal" logf)
                       (append (harmless-team-test-seat logf work)
                               (list :runtime "terminal"))))
        (should (file-directory-p pi))
        (should (equal (harmless-team-release
                        "workshop" "dev" "build")
                       (list :released "dev-build@workshop"
                             :runtime "terminal"
                             :session logf)))
        (should (equal (harmless-team-seat "workshop" "dev" "build")
                       (harmless-team-test-seat "" work)))
        (should (equal (harmless-team-edges "workshop") edge))
        (should (equal (harmless-team-test-bytes logf) log-bytes))
        (should (equal (harmless-team-test-bytes summary) hand-bytes))
        (should-not (string-search "claude-session"
                                   (harmless-team-test-read file)))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-release
                           "workshop" "dev" "build")))
                       "team seat is not occupied")))
      (should (equal (harmless-team-adopt
                      "workshop" "qa" "note" "terminal"
                      (concat "  " pid "  "))
                     (list :rig "workshop" :pod "qa" :name "note"
                           :role "notes"
                           :address "qa-note@workshop"
                           :session pid
                           :work nil
                           :runtime "terminal")))
      (should (process-attributes (emacs-pid)))
      (should (equal (harmless-team-remove "workshop" "qa" "note")
                     (list :removed "qa-note@workshop"
                           :fallback nil :work nil)))
      (should (process-attributes (emacs-pid)))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-seat "workshop" "qa" "note")))
                     "team seat is missing"))
      (should (equal (harmless-team-edges "workshop") nil))
      (should (equal (harmless-team-adopt
                      "workshop" "dev" "build" "claude" claude)
                     (append (harmless-team-test-seat claude work)
                             (list :runtime "claude"))))
      (should (equal (harmless-team-occupy
                      "workshop" "dev" "build" id)
                     (harmless-team-test-seat id work)))
      (should (file-directory-p claude))
      (should (equal (harmless-team-release "workshop" "dev" "build")
                     (list :released "dev-build@workshop"
                           :runtime nil
                           :session id)))
      (should (equal (harmless-team-seat "workshop" "dev" "build")
                     (harmless-team-test-seat "" work)))
      (should (file-regular-p summary))
      (should (file-regular-p logf))
      (should (equal (harmless-team-test-bytes logf) log-bytes))
      (should (equal (harmless-team-test-bytes codex) codex-bytes))
      (should (file-name-absolute-p file))
      (should (harmless-directory-strictly-under-p file base))
      (should-not (equal (file-truename file) (file-truename "/")))
      (should (file-name-absolute-p summary))
      (should (harmless-directory-strictly-under-p summary base))
      (should-not (equal (file-truename summary) (file-truename "/")))
      (should-not (file-exists-p (expand-file-name "teams" elsewhere)))
      (should (equal roots (harmless-team-test-roots)))
      (should (equal session-roots
                     (harmless-team-test-session-roots))))))

(defun harmless-team-test-messages (path)
  "Return the JSON messages in PATH."
  (mapcar #'harmless-json-decode
          (split-string (harmless-team-test-read path) "\n" t)))

(ert-deftest harmless-team-sends-and-keeps-the-transcript ()
  (let* ((base (make-temp-file "harmless-team-send-" t))
         (elsewhere (expand-file-name "elsewhere" base))
         (harmless-directory (file-name-as-directory base))
         (default-directory (file-name-as-directory elsewhere))
         (teams (expand-file-name "teams" base))
         (file (expand-file-name "teams/workshop.json" base))
         (logf (expand-file-name "server.log" base))
         (log2 (expand-file-name "server-two.log" base))
         (one-log (expand-file-name "box-one.log" base))
         (two-log (expand-file-name "box-two.log" base))
         (home (expand-file-name "screen-home" base))
         (screen (expand-file-name "screen.txt" home))
         (id harmless-team-test-session-b)
         (hand (expand-file-name (concat "sessions/hand/" id) base))
         (summary (expand-file-name "summary.json" hand))
         (messages (expand-file-name "messages.jsonl" hand))
         (roots (harmless-team-test-roots))
         (session-roots (harmless-team-test-session-roots))
         (work '("keep the tests"))
         (edge (list (list :kind "delegates_to"
                           :from "dev-build@workshop"
                           :to "qa-note@workshop")))
         (pid (number-to-string (emacs-pid))))
    (make-directory elsewhere t)
    (make-directory home t)
    (make-directory hand t)
    (harmless-team-test-write logf "ready\n")
    (harmless-team-test-write log2 "second\n")
    (harmless-team-test-write one-log "one\n")
    (harmless-team-test-write two-log "two\n")
    (harmless-team-test-write
     summary (format "{\"id\":\"%s\",\"status\":\"idle\"}" id))
    (cl-letf (((symbol-function 'start-process)
               (lambda (&rest _) (error "team send started a process")))
              ((symbol-function 'make-process)
               (lambda (&rest _) (error "team send started a process")))
              ((symbol-function 'call-process)
               (lambda (&rest _) (error "team send started a process")))
              ((symbol-function 'delete-process)
               (lambda (&rest _) (error "team send killed a process")))
              ((symbol-function 'signal-process)
               (lambda (&rest _) (error "team send killed a process")))
              ((symbol-function 'w32-shell-execute)
               (lambda (&rest _) (error "team send started a process")))
              ((symbol-function 'harmless-turn-run)
               (lambda (&rest _) (error "team send started a turn"))))
      (let ((log-bytes (harmless-team-test-bytes logf))
            (summary-bytes (harmless-team-test-bytes summary)))
        (dolist (bad (list nil "" "   " "\n"))
          (should (equal (harmless-team-test-signal
                          (lambda ()
                            (harmless-team-send "workshop" "*" bad)))
                         "team send is empty")))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-send "workshop" "*" 42)))
                       "team send must be a string"))
        (let ((harmless-directory "/"))
          (should (equal (harmless-team-test-signal
                          (lambda ()
                            (harmless-team-send "workshop" "*" nil)))
                         "team send is empty"))
          (should (equal (harmless-team-test-signal
                          (lambda ()
                            (harmless-team-send
                             "workshop" "qa.note" "hi")))
                         "Refusing to store a team at /")))
        (let ((harmless-directory "c:"))
          (should (equal (harmless-team-test-signal
                          (lambda ()
                            (harmless-team-send
                             "workshop" "qa.note" "hi")))
                         "Refusing to store a team at c:")))
        (let ((harmless-directory " c:/ "))
          (should (equal (harmless-team-test-signal
                          (lambda ()
                            (harmless-team-send
                             "workshop" "qa.note" "hi")))
                         "Refusing to store a team at c:/")))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-send "workshop" "*" "hi")))
                       "team rig is missing"))
        (should-not (file-directory-p teams))
        (should-not (file-exists-p (expand-file-name "notes" elsewhere)))
        (should-not (file-exists-p (expand-file-name "sub" elsewhere)))
        (should (equal (harmless-team-test-bytes logf) log-bytes))
        (should (equal (harmless-team-test-bytes summary) summary-bytes))
        (should-not (file-exists-p messages)))
      (harmless-team-add-seat "workshop" "dev" "build" "implementer")
      (harmless-team-add-seat "workshop" "qa" "note" "notes")
      (harmless-team-add-work "workshop" "dev" "build" "keep the tests")
      (harmless-team-edge
       "workshop" "delegates_to" "dev.build" "qa.note")
      (let ((bytes (harmless-team-test-bytes file)))
        (dolist (bad (list nil "" "   "))
          (should (equal (harmless-team-test-signal
                          (lambda ()
                            (harmless-team-send "workshop" bad "hi")))
                         "team send is empty")))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-send "workshop" 42 "hi")))
                       "team send must be a string"))
        (dolist (bad (list "nope" "notes" "sub/"))
          (should (equal (harmless-team-test-signal
                          (lambda ()
                            (harmless-team-send "workshop" bad "hi")))
                         "team send is not a target")))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-send "workshop" "," "hi")))
                       "team send is not a target"))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-send "workshop" "dev.gone" "hi")))
                       "team seat is missing"))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-send
                           "workshop" "dev.build" "hi")))
                       "team seat is not occupied"))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-capture
                           "workshop" "dev" "build")))
                       "team seat is not occupied"))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-transcript
                           "workshop" "qa" "gone")))
                       "team seat is missing"))
        (should-not (harmless-team-transcript "workshop" "qa" "note"))
        (should (equal (harmless-team-test-bytes file) bytes)))
      (should (equal (harmless-team-adopt
                      "workshop" "dev" "build" "harmless" id)
                     (append (harmless-team-test-seat id work)
                             (list :runtime "harmless"))))
      (should (equal (harmless-team-adopt
                      "workshop" "qa" "note" "terminal" logf)
                     (list :rig "workshop" :pod "qa" :name "note"
                           :role "notes"
                           :address "qa-note@workshop"
                           :session logf
                           :work nil
                           :runtime "terminal")))
      (should (equal (harmless-team-edges "workshop") edge))
      (should (equal (harmless-team-capture "workshop" "dev" "build") ""))
      (should (equal (harmless-team-capture "workshop" "qa" "note")
                     "ready\n"))
      (let ((bytes (harmless-team-test-bytes file))
            (summary-bytes (harmless-team-test-bytes summary)))
        (delete-file summary)
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-send
                           "workshop" "dev.build" "hidden")))
                       "team session is missing"))
        (should (equal (harmless-team-test-bytes file) bytes))
        (should-not (file-exists-p messages))
        (harmless-team-test-write
         summary (format "{\"id\":\"%s\",\"status\":\"idle\"}" id))
        (should (equal (harmless-team-test-bytes summary) summary-bytes)))
      (should (equal (harmless-team-send
                      "workshop" " qa.note " "  hello log  ")
                     '("qa-note@workshop")))
      (should (equal (harmless-team-test-read logf) "ready\nhello log\n"))
      (should (equal (harmless-team-capture "workshop" "qa" "note")
                     "ready\nhello log\n"))
      (should (equal (harmless-team-transcript "workshop" "qa" "note")
                     (list (list :seat "qa-note@workshop"
                                 :runtime "terminal"
                                 :session logf
                                 :text "hello log"))))
      (should (equal (harmless-team-send
                      "workshop" "dev.build" "hello session")
                     '("dev-build@workshop")))
      (should (file-regular-p messages))
      (should (file-name-absolute-p messages))
      (should (harmless-directory-strictly-under-p messages base))
      (should-not (equal (file-truename messages) (file-truename "/")))
      (should (equal (mapcar (lambda (msg) (plist-get msg :content))
                             (harmless-team-test-messages messages))
                     '("hello session")))
      (should (equal (mapcar (lambda (msg) (plist-get msg :role))
                             (harmless-team-test-messages messages))
                     '("user")))
      (should (equal (harmless-team-send
                      "workshop" "dev.build" "look>")
                     '("dev-build@workshop")))
      (should (equal (harmless-team-send "workshop" " * " "both")
                     '("dev-build@workshop" "qa-note@workshop")))
      (should (equal (harmless-team-test-read logf)
                     "ready\nhello log\nboth\n"))
      (should (equal (mapcar (lambda (msg) (plist-get msg :content))
                             (harmless-team-test-messages messages))
                     '("hello session" "look>" "both")))
      (let ((log-bytes (harmless-team-test-bytes logf))
            (msg-bytes (harmless-team-test-bytes messages))
            (rig-bytes (harmless-team-test-bytes file)))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-send
                           "workshop" "qa.note, dev.gone" "skipped")))
                       "team seat is missing"))
        (let ((harmless-directory "/"))
          (should (equal (harmless-team-test-signal
                          (lambda ()
                            (harmless-team-send
                             "workshop" "*" "root text")))
                         "Refusing to store a team at /")))
        (let ((harmless-directory "c:"))
          (should (equal (harmless-team-test-signal
                          (lambda ()
                            (harmless-team-send
                             "workshop" "*" "root text")))
                         "Refusing to store a team at c:")))
        (let ((harmless-directory " c:/ "))
          (should (equal (harmless-team-test-signal
                          (lambda ()
                            (harmless-team-send
                             "workshop" "*" "root text")))
                         "Refusing to store a team at c:/")))
        (should (equal (harmless-team-test-bytes logf) log-bytes))
        (should (equal (harmless-team-test-bytes messages) msg-bytes))
        (should (equal (harmless-team-test-bytes file) rig-bytes)))
      (harmless-team-test-write logf "ready\n>\n")
      (let ((log-bytes (harmless-team-test-bytes logf))
            (msg-bytes (harmless-team-test-bytes messages))
            (rig-bytes (harmless-team-test-bytes file)))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-send "workshop" "*" "paused")))
                       "team seat is at a prompt"))
        (should (equal (harmless-team-test-bytes logf) log-bytes))
        (should (equal (harmless-team-test-bytes messages) msg-bytes))
        (should (equal (harmless-team-test-bytes file) rig-bytes)))
      (harmless-team-test-write logf "Allow this tool?\n")
      (let ((log-bytes (harmless-team-test-bytes logf))
            (msg-bytes (harmless-team-test-bytes messages))
            (rig-bytes (harmless-team-test-bytes file)))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-send "workshop" "*" "paused")))
                       "team seat is at a permission question"))
        (should (equal (harmless-team-test-bytes logf) log-bytes))
        (should (equal (harmless-team-test-bytes messages) msg-bytes))
        (should (equal (harmless-team-test-bytes file) rig-bytes)))
      (harmless-team-test-write logf "ready\n")
      (harmless-team-test-write
       summary
       (format "{\"id\":\"%s\",\"status\":\"waiting-permission\"}" id))
      (let ((log-bytes (harmless-team-test-bytes logf))
            (msg-bytes (harmless-team-test-bytes messages))
            (summary-bytes (harmless-team-test-bytes summary))
            (rig-bytes (harmless-team-test-bytes file)))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-send
                           "workshop" "dev.build" "hidden")))
                       "team seat is at a permission question"))
        (should (equal (harmless-team-test-bytes logf) log-bytes))
        (should (equal (harmless-team-test-bytes messages) msg-bytes))
        (should (equal (harmless-team-test-bytes summary) summary-bytes))
        (should (equal (harmless-team-test-bytes file) rig-bytes)))
      (harmless-team-test-write
       summary (format "{\"id\":\"%s\",\"status\":\"streaming\"}" id))
      (let ((msg-bytes (harmless-team-test-bytes messages))
            (summary-bytes (harmless-team-test-bytes summary)))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-send
                           "workshop" "dev.build" "hidden")))
                       "team seat is at a prompt"))
        (should (equal (harmless-team-test-bytes messages) msg-bytes))
        (should (equal (harmless-team-test-bytes summary) summary-bytes)))
      (harmless-team-test-write
       summary (format "{\"id\":\"%s\",\"status\":\"idle\"}" id))
      (let ((summary-bytes (harmless-team-test-bytes summary)))
        (should (equal (harmless-team-send
                        "workshop" "qa.note" "  alpha\nbeta\n  ")
                       '("qa-note@workshop")))
        (should (equal (harmless-team-test-bytes summary) summary-bytes)))
      (should (equal (harmless-team-test-read logf) "ready\nalpha\nbeta\n"))
      (should (string-search "alpha\\nbeta" (harmless-team-test-read file)))
      (should (equal (harmless-team-release "workshop" "qa" "note")
                     (list :released "qa-note@workshop"
                           :runtime "terminal"
                           :session logf)))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-send "workshop" "qa.note" "later")))
                     "team seat is not occupied"))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-capture "workshop" "qa" "note")))
                     "team seat is not occupied"))
      (should (equal (harmless-team-transcript "workshop" "qa" "note")
                     (list (list :seat "qa-note@workshop"
                                 :runtime "terminal"
                                 :session logf
                                 :text "hello log")
                           (list :seat "qa-note@workshop"
                                 :runtime "terminal"
                                 :session logf
                                 :text "both")
                           (list :seat "qa-note@workshop"
                                 :runtime "terminal"
                                 :session logf
                                 :text "alpha\nbeta"))))
      (should (equal (harmless-team-adopt
                      "workshop" "qa" "note" "terminal"
                      (concat "  " pid "  "))
                     (list :rig "workshop" :pod "qa" :name "note"
                           :role "notes"
                           :address "qa-note@workshop"
                           :session pid
                           :work nil
                           :runtime "terminal")))
      (let ((log-bytes (harmless-team-test-bytes logf))
            (rig-bytes (harmless-team-test-bytes file)))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-send
                           "workshop" "qa.note" "into the pid")))
                       "team seat has no screen"))
        (should (equal (harmless-team-capture "workshop" "qa" "note")
                       "hello log\nboth\nalpha\nbeta"))
        (should (process-attributes (emacs-pid)))
        (should (equal (harmless-team-test-bytes logf) log-bytes))
        (should (equal (harmless-team-test-bytes file) rig-bytes)))
      (harmless-team-adopt "workshop" "qa" "note" "terminal" logf)
      (should (equal (harmless-team-send "workshop" " qa " "pod note")
                     '("qa-note@workshop")))
      (should-not (string-search "pod note"
                                 (harmless-team-test-read messages)))
      (should (string-suffix-p "pod note\n" (harmless-team-test-read logf)))
      (should (equal (harmless-team-send
                      "workshop" "dev.build, qa.note" "comma pair")
                     '("dev-build@workshop" "qa-note@workshop")))
      (should (equal (harmless-team-send "workshop" "workshop" "whole rig")
                     '("dev-build@workshop" "qa-note@workshop")))
      (should (equal (harmless-team-send "workshop" "QA.note" "case seat")
                     '("qa-note@workshop")))
      (should (equal (harmless-team-send
                      "workshop" "dev-build@workshop" "say \"hi\"")
                     '("dev-build@workshop")))
      (should (equal (harmless-team-send "workshop" "dev.build," "tail seat")
                     '("dev-build@workshop")))
      (let ((log-bytes (harmless-team-test-bytes logf)))
        (should (equal (harmless-team-release "workshop" "qa" "note")
                       (list :released "qa-note@workshop"
                             :runtime "terminal"
                             :session logf)))
        (should (equal (harmless-team-adopt
                        "workshop" "qa" "note" "terminal" log2)
                       (list :rig "workshop" :pod "qa" :name "note"
                             :role "notes"
                             :address "qa-note@workshop"
                             :session log2
                             :work nil
                             :runtime "terminal")))
        (should (equal (harmless-team-send
                        "workshop" "qa.note" "moved over")
                       '("qa-note@workshop")))
        (should (equal (harmless-team-test-read log2)
                       "second\nmoved over\n"))
        (should (equal (harmless-team-test-bytes logf) log-bytes))
        (should (equal (harmless-team-transcript "workshop" "qa" "note")
                       (list (list :seat "qa-note@workshop"
                                   :runtime "terminal"
                                   :session logf
                                   :text "hello log")
                             (list :seat "qa-note@workshop"
                                   :runtime "terminal"
                                   :session logf
                                   :text "both")
                             (list :seat "qa-note@workshop"
                                   :runtime "terminal"
                                   :session logf
                                   :text "alpha\nbeta")
                             (list :seat "qa-note@workshop"
                                   :runtime "terminal"
                                   :session logf
                                   :text "pod note")
                             (list :seat "qa-note@workshop"
                                   :runtime "terminal"
                                   :session logf
                                   :text "comma pair")
                             (list :seat "qa-note@workshop"
                                   :runtime "terminal"
                                   :session logf
                                   :text "whole rig")
                             (list :seat "qa-note@workshop"
                                   :runtime "terminal"
                                   :session logf
                                   :text "case seat")
                             (list :seat "qa-note@workshop"
                                   :runtime "terminal"
                                   :session log2
                                   :text "moved over")))))
      (should (equal (harmless-team-transcript "workshop" "dev" "build")
                     (list (list :seat "dev-build@workshop"
                                 :runtime "harmless"
                                 :session id
                                 :text "hello session")
                           (list :seat "dev-build@workshop"
                                 :runtime "harmless"
                                 :session id
                                 :text "look>")
                           (list :seat "dev-build@workshop"
                                 :runtime "harmless"
                                 :session id
                                 :text "both")
                           (list :seat "dev-build@workshop"
                                 :runtime "harmless"
                                 :session id
                                 :text "comma pair")
                           (list :seat "dev-build@workshop"
                                 :runtime "harmless"
                                 :session id
                                 :text "whole rig")
                           (list :seat "dev-build@workshop"
                                 :runtime "harmless"
                                 :session id
                                 :text "say \"hi\"")
                           (list :seat "dev-build@workshop"
                                 :runtime "harmless"
                                 :session id
                                 :text "tail seat"))))
      (should (equal (harmless-team-release "workshop" "dev" "build")
                     (list :released "dev-build@workshop"
                           :runtime "harmless"
                           :session id)))
      (should (equal (harmless-team-occupy "workshop" "dev" "build" id)
                     (harmless-team-test-seat id work)))
      (should (equal (harmless-team-send
                      "workshop" "dev.build" "from occupy")
                     '("dev-build@workshop")))
      (should (equal (car (last (harmless-team-transcript
                                 "workshop" "dev" "build")))
                     (list :seat "dev-build@workshop"
                           :runtime nil
                           :session id
                           :text "from occupy")))
      (should (equal (plist-get (car (last (harmless-team-test-messages
                                            messages)))
                                :content)
                     "from occupy"))
      (should (equal (harmless-team-seat "workshop" "dev" "build")
                     (harmless-team-test-seat id work)))
      (should (equal (harmless-team-edges "workshop") edge))
      (let ((note-lines (harmless-team-transcript "workshop" "qa" "note"))
            (log2-bytes (harmless-team-test-bytes log2)))
        (should (equal (harmless-team-remove "workshop" "qa" "note")
                       (list :removed "qa-note@workshop"
                             :fallback nil
                             :work nil)))
        (should (equal (harmless-team-transcript "workshop" "qa" "note")
                       note-lines))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-capture "workshop" "qa" "note")))
                       "team seat is missing"))
        (should (equal (harmless-team-edges "workshop") nil))
        (should (equal (harmless-team-test-bytes log2) log2-bytes))
        (should (file-regular-p logf))
        (should (process-attributes (emacs-pid))))
      (let* ((obj (harmless-json-decode (harmless-team-test-read file)))
             (msg-bytes (harmless-team-test-bytes messages))
             (log2-bytes (harmless-team-test-bytes log2)))
        (harmless-team-test-write
         file
         (harmless-json-text
          (plist-put obj :transcript
                     (list (list :seat "dev-build@workshop"
                                 :runtime "nope"
                                 :session id
                                 :text "x")))))
        (let ((bytes (harmless-team-test-bytes file)))
          (should (equal (harmless-team-test-signal
                          (lambda ()
                            (harmless-team-send
                             "workshop" "dev.build" "after bad")))
                         "team rig is not valid"))
          (should (equal (harmless-team-test-bytes file) bytes))
          (should (equal (harmless-team-test-bytes messages) msg-bytes))
          (should (equal (harmless-team-test-bytes log2) log2-bytes))))
      (harmless-team-add-seat "bare" "ops" "run" "runner")
      (should (equal (harmless-team-remove "bare" "ops" "run")
                     (list :removed "ops-run@bare"
                           :fallback nil
                           :work nil)))
      (let ((bytes (harmless-team-test-bytes
                    (expand-file-name "teams/bare.json" base))))
        (dolist (target '("*" "bare" "ops"))
          (should (equal (harmless-team-test-signal
                          (lambda ()
                            (harmless-team-send "bare" target "x")))
                         "team send is not a target")))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-send "bare" "ops.run" "x")))
                       "team seat is missing"))
        (should (equal (harmless-team-test-bytes
                        (expand-file-name "teams/bare.json" base))
                       bytes)))
      (harmless-team-add-seat "box" "box" "one" "scribe")
      (harmless-team-add-seat "box" "qa" "two" "scribe")
      (harmless-team-adopt "box" "box" "one" "terminal" one-log)
      (harmless-team-adopt "box" "qa" "two" "terminal" two-log)
      (should (equal (harmless-team-send "box" "box" "pod wins")
                     '("box-one@box")))
      (should (equal (harmless-team-test-read one-log) "one\npod wins\n"))
      (should (equal (harmless-team-test-read two-log) "two\n"))
      (should (equal (harmless-team-send "box" "*" "all box")
                     '("box-one@box" "qa-two@box")))
      (should (string-suffix-p "all box\n" (harmless-team-test-read two-log)))
      (harmless-team-add-seat "kit" "ops" "look" "watcher")
      (should (equal (harmless-team-adopt
                      "kit" "ops" "look" "claude"
                      (file-name-as-directory home))
                     (list :rig "kit" :pod "ops" :name "look"
                           :role "watcher"
                           :address "ops-look@kit"
                           :session home
                           :work nil
                           :runtime "claude")))
      (should (equal (harmless-team-capture "kit" "ops" "look") ""))
      (let ((bytes (harmless-team-test-bytes
                    (expand-file-name "teams/kit.json" base))))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-send "kit" "ops.look" "hi")))
                       "team seat has no screen"))
        (should-not (file-exists-p screen))
        (should (equal (harmless-team-test-bytes
                        (expand-file-name "teams/kit.json" base))
                       bytes)))
      (harmless-team-test-write screen "ok\n")
      (should (equal (harmless-team-send
                      "kit" "ops.look" "  hi screen  ")
                     '("ops-look@kit")))
      (should (equal (harmless-team-capture "kit" "ops" "look")
                     "ok\nhi screen\n"))
      (should (equal (harmless-team-transcript "kit" "ops" "look")
                     (list (list :seat "ops-look@kit"
                                 :runtime "claude"
                                 :session home
                                 :text "hi screen"))))
      (harmless-team-test-write screen "ok\n>\n")
      (let ((bytes (harmless-team-test-bytes screen))
            (rig (harmless-team-test-bytes
                  (expand-file-name "teams/kit.json" base))))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-send "kit" "ops" "paused")))
                       "team seat is at a prompt"))
        (should (equal (harmless-team-test-bytes screen) bytes))
        (should (equal (harmless-team-test-bytes
                        (expand-file-name "teams/kit.json" base))
                       rig)))
      (harmless-team-test-write screen "Allow this tool?\n")
      (let ((bytes (harmless-team-test-bytes screen)))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-send "kit" "ops.look" "paused")))
                       "team seat is at a permission question"))
        (should (equal (harmless-team-test-bytes screen) bytes)))
      (should (file-directory-p home))
      (should (file-regular-p screen))
      (should (file-regular-p logf))
      (should (file-regular-p log2))
      (should (file-regular-p summary))
      (should (file-name-absolute-p file))
      (should (harmless-directory-strictly-under-p file base))
      (should-not (equal (file-truename file) (file-truename "/")))
      (should (file-name-absolute-p messages))
      (should (harmless-directory-strictly-under-p messages base))
      (should-not (file-exists-p (expand-file-name "teams" elsewhere)))
      (should (equal roots (harmless-team-test-roots)))
      (should (equal session-roots
                     (harmless-team-test-session-roots))))))

(ert-deftest harmless-team-room-keeps-history-and-waits ()
  (let* ((base (make-temp-file "harmless-team-room-" t))
         (elsewhere (expand-file-name "elsewhere" base))
         (harmless-directory (file-name-as-directory base))
         (default-directory (file-name-as-directory elsewhere))
         (teams (expand-file-name "teams" base))
         (file (expand-file-name "teams/workshop.json" base))
         (roots (harmless-team-test-roots))
         (session-roots (harmless-team-test-session-roots))
         (work '("keep the tests"))
         (edge (list (list :kind "delegates_to"
                           :from "dev-build@workshop"
                           :to "qa-note@workshop")))
         (harmless-team-room-wait-seconds 0))
    (make-directory elsewhere t)
    (cl-letf (((symbol-function 'start-process)
               (lambda (&rest _) (error "team room started a process")))
              ((symbol-function 'make-process)
               (lambda (&rest _) (error "team room started a process")))
              ((symbol-function 'call-process)
               (lambda (&rest _) (error "team room started a process")))
              ((symbol-function 'delete-process)
               (lambda (&rest _) (error "team room killed a process")))
              ((symbol-function 'signal-process)
               (lambda (&rest _) (error "team room killed a process")))
              ((symbol-function 'w32-shell-execute)
               (lambda (&rest _) (error "team room started a process")))
              ((symbol-function 'harmless-turn-run)
               (lambda (&rest _) (error "team room started a turn"))))
      (dolist (bad (list nil "" "   "))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-room-say
                           "workshop" bad "Ada" "hi")))
                       "team room topic is empty")))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-room-say
                         "workshop" 42 "Ada" "hi")))
                     "team room topic must be a string"))
      (dolist (bad (list "/" "c:/" "notes/" "sub/" ".." "c:" "\\"))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-room-say
                           "workshop" bad "Ada" "hi")))
                       "team room topic is not a name")))
      (dolist (bad (list nil "" "   "))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-room-say
                           "workshop" "general" bad "hi")))
                       "team room speaker is empty")))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-room-say
                         "workshop" "general" 42 "hi")))
                     "team room speaker must be a string"))
      (dolist (bad (list nil "" "   " "\n"))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-room-say
                           "workshop" "general" "Ada" bad)))
                       "team room text is empty")))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-room-say
                         "workshop" "general" "Ada" 42)))
                     "team room text must be a string"))
      (let ((harmless-directory "/"))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-room-say
                           "workshop" nil "Ada" "hi")))
                       "team room topic is empty"))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-room-say
                           "workshop" "general" "Ada" "hi")))
                       "Refusing to store a team at /")))
      (let ((harmless-directory "c:"))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-room-say
                           "workshop" "general" "Ada" "hi")))
                       "Refusing to store a team at c:")))
      (let ((harmless-directory " c:/ "))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-room-say
                           "workshop" "general" "Ada" "hi")))
                       "Refusing to store a team at c:/")))
      (let ((harmless-directory "notes"))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-room-say
                           "workshop" "general" "Ada" "hi")))
                       "Harmless data directory must be absolute, not \"notes\"")))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-room-say
                         "workshop" "general" "Ada" "hi")))
                     "team rig is missing"))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-room-wait "workshop" "general")))
                     "team rig is missing"))
      (should-not (file-directory-p teams))
      (should-not (file-exists-p (expand-file-name "notes" elsewhere)))
      (should-not (file-exists-p (expand-file-name "notes" base)))
      (should-not (file-exists-p (expand-file-name "sub" elsewhere)))
      (harmless-team-add-seat "workshop" "dev" "build" "implementer")
      (harmless-team-add-seat "workshop" "qa" "note" "notes")
      (harmless-team-add-work "workshop" "dev" "build" "keep the tests")
      (harmless-team-edge
       "workshop" "delegates_to" "dev.build" "qa.note")
      (should-not (harmless-team-room-history "workshop"))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-room-history "workshop" "")))
                     "team room topic is empty"))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-room-wait "workshop" nil)))
                     "team room topic is empty"))
      (should (equal (harmless-team-test-signal
                      (lambda ()
                        (harmless-team-room-wait "workshop" "absent")))
                     "team room is still quiet"))
      (let ((hello (harmless-team-room-say
                    "workshop" " general " "  Ada  " "  hello room  ")))
        (should (equal (plist-get hello :topic) "general"))
        (should (equal (plist-get hello :speaker) "Ada"))
        (should (equal (plist-get hello :text) "hello room"))
        (should (string-match-p
                 "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}Z\\'"
                 (plist-get hello :at)))
        (should (equal (harmless-team-room-history "workshop" "general")
                       (list hello)))
        (should (equal (harmless-team-room-history "workshop" "GENERAL")
                       (list hello)))
        (should (equal (harmless-team-room-history "workshop")
                       (list hello))))
      (let ((deploy (harmless-team-room-say
                     "workshop" "deploy" "Ada" "  alpha\nbeta\n  ")))
        (should (equal (plist-get deploy :text) "alpha\nbeta"))
        (should (equal (harmless-team-room-history "workshop" "deploy")
                       (list deploy)))
        (should (equal (mapcar (lambda (line) (plist-get line :text))
                               (harmless-team-room-history "workshop"))
                       '("hello room" "alpha\nbeta"))))
      (should (equal (harmless-team-seat "workshop" "dev" "build")
                     (harmless-team-test-seat "" work)))
      (should (equal (harmless-team-edges "workshop") edge))
      (let ((bytes (harmless-team-test-bytes file)))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-room-wait
                           "workshop" "general")))
                       "team room is still quiet"))
        (should (equal (harmless-team-test-bytes file) bytes))
        (let ((harmless-directory "/"))
          (should (equal (harmless-team-test-signal
                          (lambda ()
                            (harmless-team-room-say
                             "workshop" "general" "Ada" "root")))
                         "Refusing to store a team at /")))
        (should (equal (harmless-team-test-bytes file) bytes)))
      (let* ((harmless-team-room-wait-seconds 2)
             (timer (run-with-timer
                     0.05 nil
                     (lambda ()
                       (harmless-team-room-say
                        "workshop" "general" "Bea" "spoke"))))
             (heard nil))
        (unwind-protect
            (setq heard (harmless-team-room-wait "workshop" "general"))
          (cancel-timer timer))
        (should (equal (plist-get heard :topic) "general"))
        (should (equal (plist-get heard :speaker) "Bea"))
        (should (equal (plist-get heard :text) "spoke"))
        (should (equal (mapcar (lambda (line) (plist-get line :text))
                               (harmless-team-room-history
                                "workshop" "general"))
                       '("hello room" "spoke"))))
      (let ((harmless-team-room-wait-seconds 0))
        (should (equal (harmless-team-test-signal
                        (lambda ()
                          (harmless-team-room-wait
                           "workshop" "general")))
                       "team room is still quiet")))
      (let* ((harmless-team-room-wait-seconds 0.25)
             (before (harmless-team-room-history "workshop" "deploy"))
             (timer (run-with-timer
                     0.05 nil
                     (lambda ()
                       (harmless-team-room-say
                        "workshop" "general" "Bea" "other topic")))))
        (unwind-protect
            (should (equal (harmless-team-test-signal
                            (lambda ()
                              (harmless-team-room-wait
                               "workshop" "deploy")))
                           "team room is still quiet"))
          (cancel-timer timer))
        (should (equal (harmless-team-room-history "workshop" "deploy")
                       before))
        (should (member "other topic"
                        (mapcar (lambda (line) (plist-get line :text))
                                (harmless-team-room-history
                                 "workshop" "general")))))
      (harmless-team-room-say
       "workshop" "notes" "Ada" "say \"hi\"\nnext")
      (should (equal (plist-get (car (harmless-team-room-history
                                      "workshop" "notes"))
                                :text)
                     "say \"hi\"\nnext"))
      (let ((drive (harmless-team-room-say
                    "workshop" "c" "Ada" "drive letter")))
        (should (equal (plist-get drive :topic) "c"))
        (should (equal (harmless-team-room-history "workshop" "c")
                       (list drive))))
      (should-not (file-exists-p (expand-file-name "notes" elsewhere)))
      (should-not (file-directory-p (expand-file-name "notes" base)))
      (let ((lines (harmless-team-room-history "workshop")))
        (should (equal (harmless-team-remove "workshop" "qa" "note")
                       (list :removed "qa-note@workshop"
                             :fallback nil
                             :work nil)))
        (should (equal (harmless-team-room-history "workshop") lines))
        (should (equal (harmless-team-edges "workshop") nil))
        (should (equal (harmless-team-seat "workshop" "dev" "build")
                       (harmless-team-test-seat "" work))))
      (harmless-team-add-seat "bare" "ops" "run" "runner")
      (harmless-team-remove "bare" "ops" "run")
      (let ((said (harmless-team-room-say
                   "bare" "general" "Ada" "empty rig")))
        (should (equal (harmless-team-room-history "bare")
                       (list said))))
      (let* ((obj (harmless-json-decode (harmless-team-test-read file)))
             (kept (harmless-team-test-bytes
                    (expand-file-name "teams/bare.json" base))))
        (harmless-team-test-write
         file
         (harmless-json-text
          (plist-put obj :room
                     (list (list :topic "general"
                                 :speaker "Ada"
                                 :text ""
                                 :at "2026-10-19T00:00:00Z")))))
        (let ((bytes (harmless-team-test-bytes file)))
          (should (equal (harmless-team-test-signal
                          (lambda ()
                            (harmless-team-room-say
                             "workshop" "general" "Ada" "after bad")))
                         "team rig is not valid"))
          (should (equal (harmless-team-test-bytes file) bytes))
          (should (equal (harmless-team-test-bytes
                          (expand-file-name "teams/bare.json" base))
                         kept))))
      (should (file-name-absolute-p file))
      (should (harmless-directory-strictly-under-p file base))
      (should-not (equal (file-truename file) (file-truename "/")))
      (should-not (file-exists-p (expand-file-name "teams" elsewhere)))
      (should (equal roots (harmless-team-test-roots)))
      (should (equal session-roots
                     (harmless-team-test-session-roots))))))
