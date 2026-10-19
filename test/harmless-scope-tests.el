;;; harmless-scope-tests.el --- Tests for missions and slices -*- lexical-binding: t; -*-

(require 'ert)
(require 'harmless)

(defun harmless-scope-test-signal (fn)
  "Return the error text from FN."
  (error-message-string (should-error (funcall fn) :type 'error)))

(defun harmless-scope-test-read (path)
  "Return PATH as text."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (buffer-string)))

(defun harmless-scope-test-roots ()
  "Return existence of scope files at filesystem roots."
  (list (file-exists-p "/missions")
        (file-exists-p "/SPEC.md")
        (file-exists-p "c:/missions")
        (file-exists-p "d:/missions")
        (file-exists-p "c:/SPEC.md")
        (file-exists-p "d:/SPEC.md")))

(defun harmless-scope-test-tree (root)
  "Return the files under ROOT, sorted, with their text."
  (mapcar (lambda (path)
            (cons path (harmless-scope-test-read path)))
          (sort (directory-files-recursively root ".") #'string<)))

(defun harmless-scope-test-kept (path root)
  "Assert PATH is a file strictly inside ROOT."
  (should (file-name-absolute-p path))
  (should (file-regular-p path))
  (should (harmless-directory-strictly-under-p path root))
  (should-not (equal (file-truename path) (file-truename "/"))))

(ert-deftest harmless-scope-derives-progress ()
  (let* ((parent (make-temp-file "harmless-scope-" t))
         (base (expand-file-name "project" parent))
         (elsewhere (expand-file-name "elsewhere" parent))
         (notdir (expand-file-name "notdir" parent))
         (default-directory (file-name-as-directory elsewhere))
         (roots (harmless-scope-test-roots))
         (home-spec (file-exists-p
                     (expand-file-name "SPEC.md" "~")))
         (home-work (file-exists-p
                     (expand-file-name "work" "~")))
         (timers (copy-sequence timer-list))
         (procs (copy-sequence (process-list)))
         project mission slice proof judged)
    (make-directory base t)
    (make-directory elsewhere t)
    (write-region "x" nil notdir nil 'silent)
    (dolist (bad (list nil "" "   " "\n"))
      (should (equal (harmless-scope-test-signal
                      (lambda () (harmless-scope-project bad "/")))
                     "scope project is empty")))
    (should (equal (harmless-scope-test-signal
                    (lambda () (harmless-scope-project 42 "/")))
                   "scope project must be a string"))
    (dolist (bad (list "/" "c:" "c:/" "d:/" "C:/" "//" "\\"))
      (should (equal (harmless-scope-test-signal
                      (lambda () (harmless-scope-project bad "/")))
                     "scope project is the filesystem root")))
    (dolist (bad (list "notes" "notes/" "~" "~/work"))
      (should (equal (harmless-scope-test-signal
                      (lambda () (harmless-scope-project bad "/")))
                     "scope project must be absolute")))
    (should-not (file-exists-p (expand-file-name "notes" elsewhere)))
    (should (equal home-work
                   (file-exists-p (expand-file-name "work" "~"))))
    (should (equal home-spec
                   (file-exists-p (expand-file-name "SPEC.md" "~"))))
    (should (equal (harmless-scope-test-signal
                    (lambda ()
                      (harmless-scope-project
                       (expand-file-name "missing" parent) "/")))
                   "scope project is missing"))
    (should (equal (harmless-scope-test-signal
                    (lambda () (harmless-scope-project notdir "/")))
                   "scope project is not a directory"))
    (should-not (file-exists-p (expand-file-name "SPEC.md" notdir)))
    (dolist (bad (list nil "" "   " "\n"))
      (should (equal (harmless-scope-test-signal
                      (lambda () (harmless-scope-project base bad)))
                     "scope intent is empty")))
    (should (equal (harmless-scope-test-signal
                    (lambda () (harmless-scope-project base 42)))
                   "scope intent must be a string"))
    (should-not (file-exists-p (expand-file-name "SPEC.md" base)))
    (setq project (harmless-scope-project
                   (file-name-as-directory base) "/"))
    (harmless-scope-test-kept project base)
    (should (equal (file-name-nondirectory project) "SPEC.md"))
    (should (equal (harmless-scope-test-read project)
                   "# Intent\n\n/\n"))
    (should-not (file-directory-p (expand-file-name "missions" base)))
    (let ((before (harmless-scope-test-read project)))
      (should (equal (harmless-scope-test-signal
                      (lambda () (harmless-scope-project base "other")))
                     "scope project already exists"))
      (should (equal (harmless-scope-test-read project) before)))
    (should (equal (harmless-scope-show base) (list :intent "/")))
    (should (equal (harmless-scope-show base "   " "")
                   (list :intent "/")))
    (should (equal (harmless-scope-test-signal
                    (lambda () (harmless-scope-show base nil "work")))
                   "scope mission is empty"))
    (should (equal (harmless-scope-test-signal
                    (lambda () (harmless-scope-show base "   " "work")))
                   "scope mission is empty"))
    (dolist (bad (list nil "" "   " "\n"))
      (should (equal (harmless-scope-test-signal
                      (lambda ()
                        (harmless-scope-mission base bad "keep")))
                     "scope mission is empty")))
    (should (equal (harmless-scope-test-signal
                    (lambda ()
                      (harmless-scope-mission base 42 "keep")))
                   "scope mission must be a string"))
    (dolist (bad (list "a-b" "/"))
      (should (equal (harmless-scope-test-signal
                      (lambda ()
                        (harmless-scope-mission base bad "keep")))
                     "scope mission is not a name")))
    (should-not (file-exists-p (expand-file-name "missions/a-b" base)))
    (setq mission (harmless-scope-mission
                   base " alpha " "keep\nthe tests"))
    (harmless-scope-test-kept mission base)
    (should (equal (harmless-scope-test-read mission)
                   "# Intent\n\nkeep\nthe tests\n"))
    (should-not (string-search "# Specification"
                               (harmless-scope-test-read mission)))
    (should (equal (harmless-scope-show base "alpha")
                   (list :project "/" :mission "alpha"
                         :intent "keep\nthe tests")))
    (let ((before (harmless-scope-test-read mission)))
      (should (equal (harmless-scope-test-signal
                      (lambda ()
                        (harmless-scope-mission base "alpha" "other")))
                     "scope mission already exists"))
      (should (equal (harmless-scope-test-read mission) before)))
    (should (equal (harmless-scope-test-signal
                    (lambda ()
                      (harmless-scope-slice
                       base "missing" "work" "slice" "one")))
                   "scope mission is missing"))
    (should-not (file-directory-p
                 (expand-file-name "missions/missing" base)))
    (dolist (bad (list nil "" "   "))
      (should (equal (harmless-scope-test-signal
                      (lambda ()
                        (harmless-scope-slice
                         base "alpha" "work" "slice" bad)))
                     "scope spec is empty")))
    (should (equal (harmless-scope-test-signal
                    (lambda ()
                      (harmless-scope-slice
                       base "alpha" "work" "slice" 42)))
                   "scope spec must be a string"))
    (dolist (bad (list "a-b" "/"))
      (should (equal (harmless-scope-test-signal
                      (lambda ()
                        (harmless-scope-slice
                         base "alpha" bad "slice" "one")))
                     "scope slice is not a name")))
    (setq slice (harmless-scope-slice
                 base "alpha" " work " "slice"
                 "one\n\n  two  "))
    (harmless-scope-test-kept slice base)
    (should (equal (harmless-scope-test-read slice)
                   (concat "# Intent\n\nslice\n\n# Specification\n\n"
                           "one\n\n  two\n")))
    (should (equal (harmless-scope-show base "alpha" "work")
                   (list :project "/"
                         :mission "keep\nthe tests"
                         :slice "slice"
                         :spec "one\n\n  two"
                         :items '("one" "two")
                         :progress (list :items 2 :accepted 0
                                         :rejected 0 :open 2
                                         :status "open"))))
    (let ((before (harmless-scope-test-read slice)))
      (should (equal (harmless-scope-test-signal
                      (lambda ()
                        (harmless-scope-slice
                         base "alpha" "work" "other" "three")))
                     "scope slice already exists"))
      (should (equal (harmless-scope-test-read slice) before)))
    (should (equal (harmless-scope-test-signal
                    (lambda ()
                      (harmless-scope-proof
                       base "alpha" "gone" "note" "/")))
                   "scope slice is missing"))
    (dolist (bad (list nil "" "   "))
      (should (equal (harmless-scope-test-signal
                      (lambda ()
                        (harmless-scope-proof
                         base "alpha" "work" bad "/")))
                     "scope proof is empty")))
    (dolist (bad (list "a-b" "/"))
      (should (equal (harmless-scope-test-signal
                      (lambda ()
                        (harmless-scope-proof
                         base "alpha" "work" bad "/")))
                     "scope proof is not a name")))
    (should (equal (harmless-scope-test-signal
                    (lambda ()
                      (harmless-scope-proof
                       base "alpha" "work" 42 "/")))
                   "scope proof must be a string"))
    (setq proof (harmless-scope-proof
                 base "alpha" "work" " one " "/"))
    (harmless-scope-test-kept proof base)
    (should (equal (file-name-nondirectory proof) "one.md"))
    (should (equal (harmless-scope-test-read proof) "/\n"))
    (let ((before (harmless-scope-test-read proof)))
      (should (equal (harmless-scope-test-signal
                      (lambda ()
                        (harmless-scope-proof
                         base "alpha" "work" "one" "again")))
                     "scope proof already exists"))
      (should (equal (harmless-scope-test-read proof) before)))
    (harmless-scope-test-kept
     (harmless-scope-proof base "alpha" "work" "two" "second")
     base)
    (dolist (bad (list nil "" "   "))
      (should (equal (harmless-scope-test-signal
                      (lambda ()
                        (harmless-scope-judge
                         base "alpha" "work" bad "/"
                         "accept" "/" "one")))
                     "scope item is empty")))
    (dolist (bad (list 0 -1 "0" "9" 1.5 t "a"))
      (should (equal (harmless-scope-test-signal
                      (lambda ()
                        (harmless-scope-judge
                         base "alpha" "work" bad "/"
                         "accept" "/" "one")))
                     "scope item is not an item")))
    (dolist (bad (list nil "" "   "))
      (should (equal (harmless-scope-test-signal
                      (lambda ()
                        (harmless-scope-judge
                         base "alpha" "work" 1 "/"
                         bad "/" "one")))
                     "scope verdict is empty")))
    (should (equal (harmless-scope-test-signal
                    (lambda ()
                      (harmless-scope-judge
                       base "alpha" "work" 1 "/"
                       42 "/" "one")))
                   "scope verdict must be a string"))
    (dolist (bad (list "maybe" "Accept" "/"))
      (should (equal (harmless-scope-test-signal
                      (lambda ()
                        (harmless-scope-judge
                         base "alpha" "work" 1 "/"
                         bad "/" "one")))
                     "scope verdict is not a verdict")))
    (should (equal (harmless-scope-test-signal
                    (lambda ()
                      (harmless-scope-judge
                       base "alpha" "work" 1 "/"
                       "accept" "/" "missing")))
                   "scope evidence is missing"))
    (should (equal (harmless-scope-test-signal
                    (lambda ()
                      (harmless-scope-judge
                       base "alpha" "work" 1 "/"
                       "accept" "/" "a-b")))
                   "scope evidence is not a name"))
    (let ((spec-before (harmless-scope-test-read slice)))
      (setq judged (harmless-scope-judge
                    base "alpha" "work" 1 "/"
                    "accept" "/" "one"))
      (should (equal (harmless-scope-test-read slice) spec-before)))
    (should (equal (plist-get judged :item) "1"))
    (should (equal (plist-get judged :judge) "/"))
    (should (equal (plist-get judged :verdict) "accept"))
    (should (equal (plist-get judged :reason) "/"))
    (should (equal (plist-get judged :evidence) "one"))
    (should (string-match-p
             "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}Z\\'"
             (plist-get judged :at)))
    (let ((tree (harmless-scope-test-tree base)))
      (should (equal (harmless-scope-progress base "alpha" "work")
                     (list :items 2 :accepted 1 :rejected 0
                           :open 1 :status "open")))
      (should (equal (harmless-scope-test-tree base) tree))
      (should (equal (harmless-scope-show base "alpha" "  ")
                     (list :project "/" :mission "alpha"
                           :intent "keep\nthe tests"))))
    (harmless-scope-judge
     base "alpha" "work" " 2 " "ada" "accept" "because\nyes" "two")
    (should (equal (plist-get
                    (harmless-scope-progress base "alpha" "work")
                    :status)
                   "proved"))
    (harmless-scope-judge
     base "alpha" "work" 2 "ada" "reject" "no" "two")
    (should (equal (harmless-scope-progress base "alpha" "work")
                   (list :items 2 :accepted 1 :rejected 1
                         :open 0 :status "rejected")))
    (harmless-scope-judge
     base "alpha" "work" 2 "ada" "withdraw" "pause" "two")
    (should (equal (plist-get
                    (harmless-scope-progress base "alpha" "work")
                    :status)
                   "open"))
    (harmless-scope-judge
     base "alpha" "work" 2 "ada" "accept" "again" "two")
    (should (equal (plist-get
                    (harmless-scope-progress base "alpha" "work")
                    :status)
                   "proved"))
    (let* ((book (expand-file-name
                  "missions/alpha/slices/work/judgments.json" base))
           (before (harmless-scope-test-read book))
           (two (expand-file-name
                 "missions/alpha/slices/work/proof/two.md" base)))
      (should (string-search "because\\nyes" before))
      (delete-file two)
      (should (equal (plist-get
                      (harmless-scope-progress base "alpha" "work")
                      :status)
                     "open"))
      (should (equal (harmless-scope-test-read book) before))
      (delete-file proof)
      (should (equal (harmless-scope-progress base "alpha" "work")
                     (list :items 2 :accepted 0 :rejected 0
                           :open 2 :status "open")))
      (should (equal (harmless-scope-test-read book) before)))
    (let ((plain (expand-file-name
                  "missions/alpha/slices/plain" base))
          (bad (expand-file-name
                "missions/alpha/slices/bad" base)))
      (make-directory plain t)
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region "# Intent\n\nonly\n" nil
                      (expand-file-name "SPEC.md" plain) nil 'silent))
      (should (equal (harmless-scope-test-signal
                      (lambda ()
                        (harmless-scope-progress base "alpha" "plain")))
                     "scope slice is not specified"))
      (make-directory bad t)
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region "# Intent\n\nonly\n\n# Specification\n\none\n" nil
                      (expand-file-name "SPEC.md" bad) nil 'silent)
        (write-region "nope" nil
                      (expand-file-name "judgments.json" bad)
                      nil 'silent))
      (should (equal (harmless-scope-test-signal
                      (lambda ()
                        (harmless-scope-progress base "alpha" "bad")))
                     "scope judgments are not valid"))
      (should (equal (harmless-scope-test-read
                      (expand-file-name "judgments.json" bad))
                     "nope")))
    (should (equal (harmless-scope-test-roots) roots))
    (should (equal timer-list timers))
    (should (equal (process-list) procs))))
