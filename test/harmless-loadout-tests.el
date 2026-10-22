;;; harmless-loadout-tests.el --- Skill loadout and plugin inspect -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(defun harmless-loadout-test-signal (fn)
  "Return the error text from FN."
  (error-message-string (should-error (funcall fn) :type 'error)))

(defun harmless-loadout-test-write (path text)
  "Write TEXT to PATH with Unix newlines and return PATH."
  (make-directory (file-name-directory path) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil path nil 'silent))
  path)

(defun harmless-loadout-test-read (path)
  "Return PATH as a Unix-newline string."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (buffer-string)))

(defun harmless-loadout-test-skill (root name description body)
  "Write ROOT/NAME/SKILL.md and return that path."
  (harmless-loadout-test-write
   (expand-file-name (format "%s/SKILL.md" name) root)
   (format "---\nname: %s\ndescription: %s\n---\n%s\n"
           name description body)))

(defun harmless-loadout-test-plugin (dir)
  "Write a review-kit plugin at DIR and return DIR."
  (harmless-loadout-test-write
   (expand-file-name "plugin.json" dir)
   "{\n  \"name\": \"review-kit\",\n  \"description\": \"Review helpers\"\n}\n")
  (harmless-loadout-test-write
   (expand-file-name "skills/review-kit/SKILL.md" dir)
   "---\nname: review-kit\ndescription: Review the change.\n---\nHPL-SKILL-BODY\n")
  (harmless-loadout-test-write
   (expand-file-name "skills/blank/SKILL.md" dir)
   "")
  (harmless-loadout-test-write
   (expand-file-name "hooks.json" dir)
   "[{\"event\":\"pre\",\"type\":\"command\",\"tool\":\"harmless-tool-write-file\",\"command\":\"echo no\"}]\n")
  (harmless-loadout-test-write
   (expand-file-name "mcp.json" dir)
   (concat "{\"mcpServers\":{"
           "\"kit\":{\"command\":\"echo\",\"args\":[\"hi\"]},"
           "\"other\":{\"command\":\"echo\",\"args\":[\"yo\"]}}}\n"))
  dir)

(defun harmless-loadout-test-roots ()
  "Return existence of loadout paths at filesystem roots."
  (list (file-exists-p "/.harmless")
        (file-exists-p "/loadout")
        (file-exists-p "/loadout.managed")
        (file-exists-p "c:/.harmless")
        (file-exists-p "d:/.harmless")
        (file-exists-p "c:/loadout")
        (file-exists-p "d:/loadout")
        (file-exists-p "c:/loadout.managed")
        (file-exists-p "d:/loadout.managed")
        (file-exists-p "c:/plugins")
        (file-exists-p "d:/plugins")))

(defun harmless-loadout-test-tree (root)
  "Return files under ROOT with their text, sorted by path."
  (mapcar (lambda (path)
            (cons path (harmless-loadout-test-read path)))
          (sort (directory-files-recursively root ".") #'string<)))

(ert-deftest harmless-skill-loadout-reconciles-one-directory ()
  (let* ((parent (make-temp-file "harmless-loadout-" t))
         (elsewhere (expand-file-name "elsewhere" parent))
         (parent-proj (expand-file-name "outer" parent))
         (child (expand-file-name "child" parent-proj))
         (market (expand-file-name "market/review-kit" parent))
         (plain (expand-file-name "plain" parent))
         (gaps (expand-file-name "gaps" parent))
         (bad (expand-file-name "bad" parent))
         (notdir (expand-file-name "notdir" parent))
         (missing (expand-file-name "missing" parent))
         (state (expand-file-name ".harmless-state" parent))
         (harmless-directory state)
         (default-directory (file-name-as-directory elsewhere))
         (roots (harmless-loadout-test-roots))
         (home-work (file-exists-p (expand-file-name "work" "~")))
         (timers (copy-sequence timer-list))
         (procs (copy-sequence (process-list)))
         (declared (concat "parent-only\n"
                           "parent-plug\n"
                           "review-kit\n"
                           "local-one\n"
                           "shared\n"
                           "absent-one\n"
                           "user-only\n"))
         (plug (expand-file-name ".harmless/plugins/review-kit" child))
         (managed (expand-file-name ".harmless/loadout.managed" child))
         (before nil)
         (report nil))
    (unwind-protect
        (progn
          (make-directory elsewhere t)
          (make-directory plain t)
          (make-directory gaps t)
          (make-directory bad t)
          (harmless-loadout-test-write notdir "x\n")
          (should (commandp #'harmless-skill-loadout))
          (should (commandp #'harmless-plugin-inspect))
          (dolist (empty (list nil "" "   " "\n"))
            (should (equal (harmless-loadout-test-signal
                            (lambda () (harmless-skill-loadout empty)))
                           "skill loadout directory is empty")))
          (should (equal (harmless-loadout-test-signal
                          (lambda () (harmless-skill-loadout 42)))
                         "skill loadout directory must be a string"))
          (dolist (root (list "/" "c:" "c:/" "d:/" "C:/" "//" "\\"
                              "  /  " "  c:/  "))
            (should (equal (harmless-loadout-test-signal
                            (lambda () (harmless-skill-loadout root)))
                           "skill loadout directory is the filesystem root")))
          (dolist (rel (list "child" "child/" "~" "~/work" "c:child"))
            (should (equal (harmless-loadout-test-signal
                            (lambda () (harmless-skill-loadout rel)))
                           "skill loadout directory must be absolute")))
          (should-not (file-exists-p (expand-file-name "child" elsewhere)))
          (should (equal home-work
                         (file-exists-p (expand-file-name "work" "~"))))
          (should (equal (harmless-loadout-test-signal
                          (lambda () (harmless-skill-loadout missing)))
                         "skill loadout directory is missing"))
          (should (equal (harmless-loadout-test-signal
                          (lambda () (harmless-skill-loadout notdir)))
                         "skill loadout directory is not a directory"))
          (should (equal (harmless-loadout-test-signal
                          (lambda ()
                            (harmless-skill-loadout
                             (file-name-as-directory notdir))))
                         "skill loadout directory is not a directory"))
          (should (equal (harmless-loadout-test-signal
                          (lambda () (harmless-skill-loadout plain 'yes)))
                         "skill loadout apply is not a flag"))
          (should (equal (harmless-loadout-test-signal
                          (lambda () (harmless-skill-loadout plain "t")))
                         "skill loadout apply is not a flag"))
          (should-not (file-exists-p (expand-file-name ".harmless" plain)))
          (should (equal (harmless-loadout-test-signal
                          (lambda () (harmless-skill-loadout plain t)))
                         "skill loadout is missing"))
          (should-not (file-exists-p (expand-file-name ".harmless" plain)))
          (harmless-loadout-test-write
           (expand-file-name ".harmless/loadout" plain) "\n\n  \n")
          (should (equal (harmless-loadout-test-signal
                          (lambda () (harmless-skill-loadout plain t)))
                         "skill loadout is empty"))
          (should-not (file-exists-p
                       (expand-file-name ".harmless/loadout.managed" plain)))
          (harmless-loadout-test-write
           (expand-file-name ".harmless/loadout" bad) "# local-one\n")
          (should (equal (harmless-loadout-test-signal
                          (lambda () (harmless-skill-loadout bad t)))
                         "skill loadout is not valid"))
          (harmless-loadout-test-write
           (expand-file-name ".harmless/loadout" bad) "/\n")
          (should (equal (harmless-loadout-test-signal
                          (lambda () (harmless-skill-loadout bad t)))
                         "skill loadout is not valid"))
          (harmless-loadout-test-write
           (expand-file-name ".harmless/loadout" bad) "c:/\n")
          (should (equal (harmless-loadout-test-signal
                          (lambda () (harmless-skill-loadout bad t)))
                         "skill loadout is not valid"))
          (harmless-loadout-test-write
           (expand-file-name ".harmless/loadout" bad) "local-one\nlocal-one\n")
          (should (equal (harmless-loadout-test-signal
                          (lambda () (harmless-skill-loadout bad t)))
                         "skill loadout is not valid"))
          (should-not (file-exists-p
                       (expand-file-name ".harmless/loadout.managed" bad)))
          (delete-file (expand-file-name ".harmless/loadout" bad))
          (make-directory (expand-file-name ".harmless/loadout" bad) t)
          (should (equal (harmless-loadout-test-signal
                          (lambda () (harmless-skill-loadout bad)))
                         "skill loadout is not valid"))
          (harmless-loadout-test-skill
           (expand-file-name ".harmless/skills" parent-proj)
           "parent-only" "Parent." "PARENT-BODY")
          (harmless-loadout-test-write
           (expand-file-name ".harmless/plugins/parent-plug/plugin.json"
                             parent-proj)
           "{\n  \"name\": \"parent-plug\",\n  \"description\": \"Parent\"\n}\n")
          (harmless-loadout-test-skill
           (expand-file-name ".harmless/plugins/parent-plug/skills" parent-proj)
           "parent-plug" "Parent plugin." "PARENT-PLUG")
          (harmless-loadout-test-write
           (expand-file-name "plugins/user-kit/plugin.json" state)
           "{\n  \"name\": \"user-kit\",\n  \"description\": \"User\"\n}\n")
          (harmless-loadout-test-skill
           (expand-file-name "plugins/user-kit/skills" state)
           "user-only" "User skill." "USER-BODY")
          (harmless-loadout-test-plugin market)
          (harmless-loadout-test-plugin plug)
          (harmless-loadout-test-skill
           (expand-file-name ".harmless/skills" child)
           "local-one" "Local." "LOCAL-BODY")
          (harmless-loadout-test-skill
           (expand-file-name ".harmless/skills" child)
           "shared" "Harmless copy." "HARMLESS-SHARED")
          (harmless-loadout-test-skill
           (expand-file-name ".agents/skills" child)
           "shared" "Agents copy." "AGENTS-SHARED")
          (harmless-loadout-test-skill
           (expand-file-name ".agents/skills" child)
           "also-extra" "Extra." "EXTRA-BODY")
          (harmless-loadout-test-skill
           (expand-file-name ".grok/skills" child)
           "zeta-extra" "Later extra." "ZETA-BODY")
          (harmless-loadout-test-write
           (expand-file-name ".harmless/loadout" child) declared)
          (setq before (list (harmless-loadout-test-tree market)
                             (harmless-loadout-test-tree plug)
                             (harmless-loadout-test-read
                              (expand-file-name
                               ".harmless/skills/parent-only/SKILL.md"
                               parent-proj))
                             (harmless-loadout-test-read
                              (expand-file-name
                               "plugins/user-kit/skills/user-only/SKILL.md"
                               state))))
          (let ((info (harmless-plugin-inspect market)))
            (should (equal "review-kit" (plist-get info :name)))
            (should (equal '("review-kit") (plist-get info :skills)))
            (should (equal '("kit" "other") (plist-get info :servers)))
            (should (equal 1 (length (plist-get info :hooks))))
            (let ((hook (car (plist-get info :hooks))))
              (should (eq 'pre (plist-get hook :event)))
              (should (eq 'command (plist-get hook :type)))
              (should (equal "harmless-tool-write-file" (plist-get hook :tool)))
              (should (equal "echo no" (plist-get hook :command)))))
          (should (equal (harmless-plugin-inspect market)
                         (harmless-plugin-inspect
                          (file-name-as-directory market))))
          (should-not (file-exists-p
                       (expand-file-name ".harmless/plugins" plain)))
          (should (equal before
                         (list (harmless-loadout-test-tree market)
                               (harmless-loadout-test-tree plug)
                               (harmless-loadout-test-read
                                (expand-file-name
                                 ".harmless/skills/parent-only/SKILL.md"
                                 parent-proj))
                               (harmless-loadout-test-read
                                (expand-file-name
                                 "plugins/user-kit/skills/user-only/SKILL.md"
                                 state)))))
          (dolist (empty (list nil "" "   "))
            (should (equal (harmless-loadout-test-signal
                            (lambda () (harmless-plugin-inspect empty)))
                           "plugin directory is empty")))
          (should (equal (harmless-loadout-test-signal
                          (lambda () (harmless-plugin-inspect 42)))
                         "plugin directory is empty"))
          (dolist (root (list "/" "c:" "c:/" "d:/" "C:/" "//" "\\"))
            (should (equal (harmless-loadout-test-signal
                            (lambda () (harmless-plugin-inspect root)))
                           "plugin directory is the filesystem root")))
          (dolist (rel (list "review-kit" "review-kit/" "~" "~/work"))
            (should (equal (harmless-loadout-test-signal
                            (lambda () (harmless-plugin-inspect rel)))
                           "plugin directory must be absolute")))
          (should (equal (harmless-loadout-test-signal
                          (lambda () (harmless-plugin-inspect missing)))
                         "plugin directory is missing"))
          (should (equal (harmless-loadout-test-signal
                          (lambda () (harmless-plugin-inspect notdir)))
                         "plugin directory is not a directory"))
          (should (equal (harmless-loadout-test-signal
                          (lambda ()
                            (harmless-plugin-inspect
                             (file-name-as-directory notdir))))
                         "plugin directory is not a directory"))
          (should (equal (harmless-loadout-test-signal
                          (lambda () (harmless-plugin-inspect plain)))
                         "plugin.json is not valid"))
          (let ((broken (expand-file-name "broken" parent)))
            (harmless-loadout-test-write
             (expand-file-name "plugin.json" broken) "nope\n")
            (should (equal (harmless-loadout-test-signal
                            (lambda () (harmless-plugin-inspect broken)))
                           "plugin.json is not valid"))
            (harmless-loadout-test-write
             (expand-file-name "plugin.json" broken)
             "{\n  \"name\": \"broken\",\n  \"description\": \"Bad\"\n}\n")
            (harmless-loadout-test-write
             (expand-file-name "hooks.json" broken) "nope\n")
            (should (equal (harmless-loadout-test-signal
                            (lambda () (harmless-plugin-inspect broken)))
                           "plugin hooks are not valid"))
            (delete-file (expand-file-name "hooks.json" broken))
            (harmless-loadout-test-write
             (expand-file-name "mcp.json" broken) "nope\n")
            (should (equal (harmless-loadout-test-signal
                            (lambda () (harmless-plugin-inspect broken)))
                           "mcp.json is not valid")))
          (setq report (harmless-skill-loadout child))
          (should (equal (directory-file-name (expand-file-name child))
                         (plist-get report :directory)))
          (should (equal '("parent-only" "parent-plug" "review-kit"
                           "local-one" "shared" "absent-one" "user-only")
                         (plist-get report :declared)))
          (should (equal '("review-kit" "local-one" "shared")
                         (plist-get report :present)))
          (should (equal '("parent-only" "parent-plug" "absent-one" "user-only")
                         (plist-get report :missing)))
          (should (equal '("also-extra" "zeta-extra")
                         (plist-get report :extra)))
          (should-not (plist-get report :applied))
          (should (equal report
                         (harmless-skill-loadout
                          (file-name-as-directory child))))
          (should-not (file-exists-p managed))
          (setq report (harmless-skill-loadout child t))
          (should (eq t (plist-get report :applied)))
          (should (equal '("review-kit" "local-one" "shared")
                         (plist-get report :present)))
          (should (file-name-absolute-p managed))
          (should (harmless-directory-strictly-under-p managed child))
          (should-not (harmless-filesystem-root-p managed))
          (should-not (equal (file-truename managed) (file-truename "/")))
          (should (equal "review-kit\nlocal-one\nshared\n"
                         (harmless-loadout-test-read managed)))
          (should (equal declared
                         (harmless-loadout-test-read
                          (expand-file-name ".harmless/loadout" child))))
          (should-not (file-exists-p
                       (expand-file-name ".harmless/skills/review-kit" child)))
          (should-not (file-exists-p
                       (expand-file-name ".harmless/agents" child)))
          (should-not (file-exists-p
                       (expand-file-name "mcp.json" child)))
          (should (equal before
                         (list (harmless-loadout-test-tree market)
                               (harmless-loadout-test-tree plug)
                               (harmless-loadout-test-read
                                (expand-file-name
                                 ".harmless/skills/parent-only/SKILL.md"
                                 parent-proj))
                               (harmless-loadout-test-read
                                (expand-file-name
                                 "plugins/user-kit/skills/user-only/SKILL.md"
                                 state)))))
          (harmless-loadout-test-write
           (expand-file-name ".harmless/loadout" gaps) "absent-one\n")
          (setq report (harmless-skill-loadout gaps t))
          (should (equal '("absent-one") (plist-get report :missing)))
          (should-not (plist-get report :present))
          (should-not (plist-get report :extra))
          (should (eq t (plist-get report :applied)))
          (let ((empty-managed (expand-file-name
                                ".harmless/loadout.managed" gaps)))
            (should (file-regular-p empty-managed))
            (should (file-name-absolute-p empty-managed))
            (should (harmless-directory-strictly-under-p empty-managed gaps))
            (should-not (harmless-filesystem-root-p empty-managed))
            (should-not (equal (file-truename empty-managed)
                               (file-truename "/")))
            (should (equal "" (harmless-loadout-test-read empty-managed))))
          (harmless-loadout-test-write
           (expand-file-name ".harmless/loadout" plain) "local-one\r\n")
          (harmless-loadout-test-skill
           (expand-file-name ".harmless/skills" plain)
           "local-one" "Local." "LOCAL-BODY")
          (setq report (harmless-skill-loadout plain))
          (should (equal '("local-one") (plist-get report :present)))
          (should-not (plist-get report :missing))
          (should (equal roots (harmless-loadout-test-roots)))
          (should (equal timers (copy-sequence timer-list)))
          (should (equal procs (copy-sequence (process-list)))))
      (ignore-errors (delete-directory parent t)))))
