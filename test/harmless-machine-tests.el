;;; harmless-machine-tests.el --- Tests for other machines -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(defun harmless-machine-test-signal (fn)
  "Return the error text from FN."
  (error-message-string (should-error (funcall fn) :type 'error)))

(defun harmless-machine-test-write (path text)
  "Write TEXT to PATH with Unix newlines and return PATH."
  (make-directory (file-name-directory path) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil path nil 'silent))
  path)

(defun harmless-machine-test-read (path)
  "Return PATH as a Unix-newline string."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (buffer-string)))

(defun harmless-machine-test-roots ()
  "Return existence of machine files at filesystem roots."
  (list (file-exists-p "/machines.json")
        (file-exists-p "/teams")
        (file-exists-p "c:/machines.json")
        (file-exists-p "d:/machines.json")
        (file-exists-p "c:/teams")
        (file-exists-p "d:/teams")))

(ert-deftest harmless-machine-runs-a-verb-on-a-host ()
  (let* ((parent (make-temp-file "harmless-machine-" t))
         (elsewhere (expand-file-name "elsewhere" parent))
         (local (expand-file-name "local" parent))
         (alpha (expand-file-name "alpha" parent))
         (build (expand-file-name "build" parent))
         (notdir (expand-file-name "notdir" parent))
         (missing (expand-file-name "missing" parent))
         (broken (expand-file-name "broken" parent))
         (harmless-directory local)
         (default-directory (file-name-as-directory elsewhere))
         (roots (harmless-machine-test-roots))
         (home-work (file-exists-p (expand-file-name "work" "~")))
         (timers (copy-sequence timer-list))
         (procs (copy-sequence (process-list)))
         (registry (expand-file-name "machines.json" local))
         (rig (expand-file-name "teams/desk.json" build))
         (src (expand-file-name "notes/keep.txt" alpha))
         (dst (expand-file-name "notes/keep.txt" build))
         (seat nil)
         (copied nil))
    (unwind-protect
        (progn
          (make-directory elsewhere t)
          (make-directory local t)
          (make-directory alpha t)
          (make-directory build t)
          (make-directory broken t)
          (harmless-machine-test-write notdir "x\n")
          (should (commandp #'harmless-machine-add))
          (should (commandp #'harmless-machine-call))
          (should (commandp #'harmless-machine-copy))
          (should-not (harmless-machine-list))
          (should-not (file-exists-p registry))
          (dolist (empty (list nil "" "   " "\n"))
            (should (equal (harmless-machine-test-signal
                            (lambda ()
                              (harmless-machine-add empty alpha)))
                           "machine name is empty")))
          (should (equal (harmless-machine-test-signal
                          (lambda () (harmless-machine-add 42 alpha)))
                         "machine name must be a string"))
          (dolist (bad (list "/" "c:/" "build/" "~" "a--b" "sub/name"))
            (should (equal (harmless-machine-test-signal
                            (lambda () (harmless-machine-add bad alpha)))
                           "machine name is not a name")))
          (dolist (empty (list nil "" "   "))
            (should (equal (harmless-machine-test-signal
                            (lambda ()
                              (harmless-machine-add "alpha" empty)))
                           "machine directory is empty")))
          (should (equal (harmless-machine-test-signal
                          (lambda ()
                            (harmless-machine-add "alpha" 'alpha)))
                         "machine directory must be a string"))
          (dolist (root (list "/" "c:" "c:/" "d:/" "C:/" "//" "\\"
                              "  /  " "  c:/  "))
            (should (equal (harmless-machine-test-signal
                            (lambda ()
                              (harmless-machine-add "alpha" root)))
                           "machine directory is the filesystem root")))
          (dolist (rel (list "alpha" "alpha/" "~" "~/work" "c:alpha"))
            (should (equal (harmless-machine-test-signal
                            (lambda ()
                              (harmless-machine-add "alpha" rel)))
                           "machine directory must be absolute")))
          (should-not (file-exists-p
                       (expand-file-name "alpha" elsewhere)))
          (should (equal home-work
                         (file-exists-p (expand-file-name "work" "~"))))
          (should (equal (harmless-machine-test-signal
                          (lambda ()
                            (harmless-machine-add "alpha" missing)))
                         "machine directory is missing"))
          (should (equal (harmless-machine-test-signal
                          (lambda ()
                            (harmless-machine-add "alpha" notdir)))
                         "machine directory is not a directory"))
          (should (equal (harmless-machine-test-signal
                          (lambda ()
                            (harmless-machine-add
                             "alpha" (file-name-as-directory notdir))))
                         "machine directory is not a directory"))
          (should-not (file-exists-p registry))
          (let ((harmless-directory "/"))
            (should (equal (harmless-machine-test-signal
                            (lambda () (harmless-machine-list)))
                           "Refusing to store a machine at /")))
          (let ((harmless-directory "c:/"))
            (should (equal (harmless-machine-test-signal
                            (lambda () (harmless-machine-list)))
                           "Refusing to store a machine at c:/")))
          (should (equal (list :name "build"
                               :directory (directory-file-name
                                           (expand-file-name build)))
                         (harmless-machine-add
                          "build" (file-name-as-directory build))))
          (should (equal (list :name "alpha"
                               :directory (directory-file-name
                                           (expand-file-name alpha)))
                         (harmless-machine-add "  alpha  " alpha)))
          (should (equal '("alpha" "build") (harmless-machine-list)))
          (should (file-name-absolute-p registry))
          (should (harmless-directory-strictly-under-p registry local))
          (should-not (harmless-filesystem-root-p registry))
          (should-not (equal (file-truename registry) (file-truename "/")))
          (should (equal (concat
                          "[{\"name\":\"alpha\",\"directory\":"
                          (harmless-json-text
                           (directory-file-name (expand-file-name alpha)))
                          "},{\"name\":\"build\",\"directory\":"
                          (harmless-json-text
                           (directory-file-name (expand-file-name build)))
                          "}]\n")
                         (harmless-machine-test-read registry)))
          (let ((before (harmless-machine-test-read registry)))
            (should (equal "machine is already registered: alpha"
                           (harmless-machine-test-signal
                            (lambda ()
                              (harmless-machine-add "alpha" alpha)))))
            (should (equal before
                           (harmless-machine-test-read registry))))
          (should (equal (harmless-machine-show "alpha")
                         (list :name "alpha"
                               :directory (directory-file-name
                                           (expand-file-name alpha)))))
          (dolist (verb (list nil "harmless-team-add-seat"
                              'harmless-team--add 'delete-file
                              'harmless-machine-add))
            (should (equal (harmless-machine-test-signal
                            (lambda ()
                              (harmless-machine-call "build" verb)))
                           "machine verb is not a team verb")))
          (should-not (file-exists-p (expand-file-name "teams" build)))
          (should-not (file-exists-p (expand-file-name "teams" local)))
          (setq seat (harmless-machine-call
                      "build" 'harmless-team-add-seat
                      "desk" "core" "lead" "builds"))
          (should (equal "core-lead@desk" (plist-get seat :address)))
          (should (file-regular-p rig))
          (should (file-name-absolute-p rig))
          (should (harmless-directory-strictly-under-p rig build))
          (should-not (harmless-filesystem-root-p rig))
          (should-not (equal (file-truename rig) (file-truename "/")))
          (should (string-search "core-lead@desk"
                                 (harmless-machine-test-read rig)))
          (should-not (file-exists-p (expand-file-name "teams" local)))
          (should (equal "team rig is missing"
                         (harmless-machine-test-signal
                          (lambda ()
                            (harmless-team-seat "desk" "core" "lead")))))
          (let ((host-text (harmless-machine-test-read rig)))
            (harmless-team-add-seat "desk" "core" "lead" "builds")
            (should (file-regular-p
                     (expand-file-name "teams/desk.json" local)))
            (should (equal host-text (harmless-machine-test-read rig))))
          (harmless-machine-test-write src "keep\n")
          (setq copied (harmless-machine-copy "alpha" "build"
                                              "notes/keep.txt"))
          (should (equal dst copied))
          (should (file-name-absolute-p copied))
          (should (harmless-directory-strictly-under-p copied build))
          (should-not (harmless-filesystem-root-p copied))
          (should-not (equal (file-truename copied) (file-truename "/")))
          (should (equal "keep\n" (harmless-machine-test-read copied)))
          (should (equal "keep\n" (harmless-machine-test-read src)))
          (should (equal "machine file is already there"
                         (harmless-machine-test-signal
                          (lambda ()
                            (harmless-machine-copy
                             "alpha" "build" "notes/keep.txt")))))
          (should (equal "keep\n" (harmless-machine-test-read copied)))
          (make-directory (expand-file-name "adir" alpha) t)
          (should (equal "machine file is not a file"
                         (harmless-machine-test-signal
                          (lambda ()
                            (harmless-machine-copy "alpha" "build" "adir")))))
          (should-not (file-exists-p (expand-file-name "adir" build)))
          (should (equal "machine file is missing"
                         (harmless-machine-test-signal
                          (lambda ()
                            (harmless-machine-copy
                             "alpha" "build" "notes/gone.txt")))))
          (dolist (bad (list nil "" "   "))
            (should (equal (harmless-machine-test-signal
                            (lambda ()
                              (harmless-machine-copy "alpha" "build" bad)))
                           "machine file is empty")))
          (should (equal (harmless-machine-test-signal
                          (lambda ()
                            (harmless-machine-copy "alpha" "build" 42)))
                         "machine file must be a string"))
          (dolist (bad (list "/" "c:/" "~" "~/work" "../out.txt"
                             "notes/../../out.txt" "notes/" "." ".."))
            (should (equal (harmless-machine-test-signal
                            (lambda ()
                              (harmless-machine-copy "alpha" "build" bad)))
                           "machine file escapes the machine")))
          (should-not (file-exists-p
                       (expand-file-name "out.txt" parent)))
          (should (equal "alpha" (harmless-machine-remove "alpha")))
          (should (file-directory-p alpha))
          (should (equal "keep\n" (harmless-machine-test-read src)))
          (should (equal '("build") (harmless-machine-list)))
          (should (equal "no machine named alpha"
                         (harmless-machine-test-signal
                          (lambda ()
                            (harmless-machine-call
                             "alpha" 'harmless-team-add-seat
                             "desk" "core" "lead" "builds")))))
          (let ((harmless-directory broken))
            (harmless-machine-test-write
             (expand-file-name "machines.json" broken) "nope\n")
            (should (equal (harmless-machine-test-signal
                            (lambda () (harmless-machine-list)))
                           "machines are not valid"))
            (should (equal "nope\n"
                           (harmless-machine-test-read
                            (expand-file-name "machines.json" broken))))
            (should-not (file-exists-p
                         (expand-file-name "teams" broken))))
          (should (equal roots (harmless-machine-test-roots)))
          (should (equal timers (copy-sequence timer-list)))
          (should (equal procs (copy-sequence (process-list)))))
      (ignore-errors (delete-directory parent t)))))
