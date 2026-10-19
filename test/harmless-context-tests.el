;;; harmless-context-tests.el --- Tests for context packs -*- lexical-binding: t; -*-

(require 'ert)
(require 'harmless)

(defun harmless-context-test-signal (fn)
  "Return the error text from FN."
  (error-message-string (should-error (funcall fn) :type 'error)))

(defun harmless-context-test-read (path)
  "Return PATH as text."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (buffer-string)))

(defun harmless-context-test-write (path text)
  "Write TEXT to PATH and return PATH."
  (make-directory (file-name-directory path) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region text nil path nil 'silent))
  path)

(defun harmless-context-test-roots ()
  "Return existence of pack directories at filesystem roots."
  (list (file-exists-p "/packs")
        (file-exists-p "c:/packs")
        (file-exists-p "d:/packs")))

(defun harmless-context-test-tree (root)
  "Return the files under ROOT, sorted, with their text."
  (mapcar (lambda (path)
            (cons path (harmless-context-test-read path)))
          (sort (directory-files-recursively root ".") #'string<)))

(ert-deftest harmless-context-reports-the-tokens ()
  (let* ((parent (make-temp-file "harmless-context-" t))
         (base (expand-file-name "project" parent))
         (elsewhere (expand-file-name "elsewhere" parent))
         (notdir (expand-file-name "notdir" parent))
         (default-directory (file-name-as-directory elsewhere))
         (roots (harmless-context-test-roots))
         (home-work (file-exists-p (expand-file-name "work" "~")))
         (timers (copy-sequence timer-list))
         (procs (copy-sequence (process-list)))
         (guide (concat "# Decisions\n\nkeep the door\n\n"
                        "## Detail\n\nmore\n\n# Other\n\ntail\n\n"
                        "# This pack does not cover\n\nlimits\n"))
         (notes "# Start\n\n/\n")
         (decisions (concat "# Decisions\n\nkeep the door\n\n"
                            "## Detail\n\nmore"))
         (detail "## Detail\n\nmore")
         (limits "# This pack does not cover\n\nlimits")
         (start "# Start\n\n/")
         (manifest (concat "# Files\n\nguide.md\nnotes.md\n\n"
                           "# fresh\n\nguide.md\nnotes.md#Start\n\n"
                           "# handover\n\nguide.md#Decisions\n\n"
                           "# post-compaction\n\nnotes.md#Start\n"))
         pack)
    (make-directory base t)
    (make-directory elsewhere t)
    (write-region "x" nil notdir nil 'silent)
    (should (equal (harmless-context--tokens "") 0))
    (should (equal (harmless-context--tokens "abcd") 1))
    (should (equal (harmless-context--tokens "abcde") 2))
    (should-not (harmless-context-list base))
    (should-not (file-directory-p (expand-file-name "packs" base)))
    (dolist (bad (list nil "" "   " "\n"))
      (should (equal (harmless-context-test-signal
                      (lambda () (harmless-context-list bad)))
                     "context project is empty")))
    (should (equal (harmless-context-test-signal
                    (lambda () (harmless-context-list 42)))
                   "context project must be a string"))
    (dolist (bad (list "/" "c:" "c:/" "d:/" "C:/" "//" "\\"))
      (should (equal (harmless-context-test-signal
                      (lambda () (harmless-context-list bad)))
                     "context project is the filesystem root")))
    (dolist (bad (list "notes" "notes/" "~" "~/work"))
      (should (equal (harmless-context-test-signal
                      (lambda () (harmless-context-list bad)))
                     "context project must be absolute")))
    (should-not (file-exists-p (expand-file-name "notes" elsewhere)))
    (should (equal home-work
                   (file-exists-p (expand-file-name "work" "~"))))
    (should (equal (harmless-context-test-signal
                    (lambda ()
                      (harmless-context-list
                       (expand-file-name "missing" parent))))
                   "context project is missing"))
    (should (equal (harmless-context-test-signal
                    (lambda () (harmless-context-list notdir)))
                   "context project is not a directory"))
    (dolist (bad (list nil "" "   "))
      (should (equal (harmless-context-test-signal
                      (lambda ()
                        (harmless-context-show base bad)))
                     "context pack is empty")))
    (dolist (bad (list "a/b" "/" "a--b" ".."))
      (should (equal (harmless-context-test-signal
                      (lambda ()
                        (harmless-context-show base bad)))
                     "context pack is not a name")))
    (should (equal (harmless-context-test-signal
                    (lambda () (harmless-context-show base 42)))
                   "context pack must be a string"))
    (should (equal (harmless-context-test-signal
                    (lambda ()
                      (harmless-context-show base "world-public")))
                   "context pack is missing"))
    (setq pack (harmless-context-test-write
                (expand-file-name
                 "packs/world-public/PACK.md" base)
                manifest))
    (harmless-context-test-write
     (expand-file-name "packs/world-public/guide.md" base) guide)
    (harmless-context-test-write
     (expand-file-name "packs/world-public/notes.md" base) notes)
    (harmless-context-test-write
     (expand-file-name "packs/world-public/other.md" base)
     "secret-other\n")
    (should (file-name-absolute-p pack))
    (should (harmless-directory-strictly-under-p pack base))
    (should-not (equal (file-truename pack) (file-truename "/")))
    (should (equal (harmless-context-list
                    (file-name-as-directory base))
                   '("world-public")))
    (should (equal (harmless-context-show base " world-public ")
                   (list :pack "world-public"
                         :files '("guide.md" "notes.md")
                         :fresh '("guide.md" "notes.md#Start")
                         :handover '("guide.md#Decisions")
                         :post-compaction '("notes.md#Start"))))
    (should (equal (harmless-context-get
                    base "world-public/guide.md#Decisions")
                   (list :ref "world-public/guide.md#Decisions"
                         :text decisions
                         :tokens (ceiling (length decisions) 4))))
    (should (equal (plist-get
                    (harmless-context-get
                     base "world-public/guide.md#Detail")
                    :text)
                   detail))
    (should (equal (plist-get
                    (harmless-context-get
                     base
                     "world-public/guide.md#this-pack-does-not-cover")
                    :text)
                   limits))
    (should (equal (plist-get
                    (harmless-context-get base "world-public/notes.md")
                    :text)
                   notes))
    (should (equal (plist-get
                    (harmless-context-get base " world-public ")
                    :text)
                   (concat guide "\n\n" notes)))
    (let ((tree (harmless-context-test-tree base))
          (fresh (harmless-context-profile
                  base "world-public" "fresh"))
          (hand (harmless-context-profile
                 base "world-public" "handover" nil)))
      (should (equal (plist-get fresh :pieces)
                     '("guide.md" "notes.md#Start")))
      (should (equal (plist-get fresh :text)
                     (concat guide "\n\n" start)))
      (should (equal (plist-get fresh :tokens)
                     (ceiling (length (plist-get fresh :text)) 4)))
      (should-not (plist-get fresh :budget))
      (should (equal (plist-get fresh :over) 0))
      (should (equal (plist-get hand :text) decisions))
      (should (equal (plist-get
                      (harmless-context-profile
                       base "world-public" "post-compaction")
                      :text)
                     start))
      (let ((short (harmless-context-profile
                    base "world-public" "handover" 1)))
        (should (equal (plist-get short :text) decisions))
        (should (equal (plist-get short :budget) 1))
        (should (equal (plist-get short :over)
                       (1- (plist-get short :tokens))))
        (should (> (plist-get short :over) 0))
        (should (string-search "keep the door"
                               (plist-get short :text))))
      (should (equal (harmless-context-test-tree base) tree)))
    (should (equal (harmless-context-test-signal
                    (lambda ()
                      (harmless-context-get
                       base "world-public/guide.md#Nope")))
                   "context section is missing"))
    (should (equal (harmless-context-test-signal
                    (lambda ()
                      (harmless-context-get
                       base "world-public/other.md")))
                   "context file is not in the pack"))
    (dolist (bad (list nil "" "   "))
      (should (equal (harmless-context-test-signal
                      (lambda () (harmless-context-get base bad)))
                     "context ref is empty")))
    (dolist (bad (list "/" "world-public/../secret" "world-public/"))
      (should (equal (harmless-context-test-signal
                      (lambda () (harmless-context-get base bad)))
                     "context ref is not a ref")))
    (should (equal (harmless-context-test-signal
                    (lambda () (harmless-context-get base 42)))
                   "context ref must be a string"))
    (should (equal (harmless-context-test-signal
                    (lambda ()
                      (harmless-context-profile
                       base "world-public" nil 0)))
                   "context situation is empty"))
    (should (equal (harmless-context-test-signal
                    (lambda ()
                      (harmless-context-profile
                       base "world-public" 42 nil)))
                   "context situation must be a string"))
    (should (equal (harmless-context-test-signal
                    (lambda ()
                      (harmless-context-profile
                       base "world-public" "later")))
                   "context situation is not a situation"))
    (dolist (bad (list 0 -1 1.5 "80" "" "   " t))
      (should (equal (harmless-context-test-signal
                      (lambda ()
                        (harmless-context-profile
                         base "world-public" "fresh" bad)))
                     "context budget is not a budget")))
    (let ((secret (harmless-context-test-write
                   (expand-file-name "secret" parent) "hidden\n"))
          (bad (harmless-context-test-write
                (expand-file-name "packs/escape/PACK.md" base)
                "# Files\n\n../secret\n")))
      (should (equal (harmless-context-test-signal
                      (lambda ()
                        (harmless-context-show base "escape")))
                     "context file escapes the pack"))
      (should (equal (harmless-context-test-read bad)
                     "# Files\n\n../secret\n"))
      (should (equal (harmless-context-test-read secret) "hidden\n"))
      (should-not (string-search
                   "hidden"
                   (harmless-context-test-signal
                    (lambda ()
                      (harmless-context-show base "escape"))))))
    (harmless-context-test-write
     (expand-file-name "packs/gone/PACK.md" base)
     "# Files\n\nmissing.md\n\n# fresh\n\nmissing.md\n")
    (should (equal (plist-get
                    (harmless-context-show base "gone") :files)
                   '("missing.md")))
    (should (equal (harmless-context-test-signal
                    (lambda ()
                      (harmless-context-get base "gone/missing.md")))
                   "context file is missing"))
    (harmless-context-test-write
     (expand-file-name "packs/typed/PACK.md" base)
     "# Files\n\ndir.md\n\n# fresh\n\ndir.md\n")
    (make-directory
     (expand-file-name "packs/typed/dir.md" base) t)
    (should (equal (harmless-context-test-signal
                    (lambda ()
                      (harmless-context-get base "typed/dir.md")))
                   "context file is not a file"))
    (harmless-context-test-write
     (expand-file-name "packs/badmd/PACK.md" base) "hello\n")
    (let ((before (harmless-context-test-read
                   (expand-file-name "packs/badmd/PACK.md" base))))
      (should (equal (harmless-context-test-signal
                      (lambda ()
                        (harmless-context-show base "badmd")))
                     "context pack is not valid"))
      (should (equal (harmless-context-test-read
                      (expand-file-name "packs/badmd/PACK.md" base))
                     before)))
    (harmless-context-test-write
     (expand-file-name "packs/bare/guide.md" base) guide)
    (harmless-context-test-write
     (expand-file-name "packs/bare/PACK.md" base)
     "# Files\n\nguide.md\n\n# fresh\n\nguide.md\n")
    (should (equal (harmless-context-test-signal
                    (lambda ()
                      (harmless-context-profile
                       base "bare" "handover")))
                   "context situation is missing"))
    (should (equal (harmless-context-list base)
                   '("badmd" "bare" "escape" "gone" "typed"
                     "world-public")))
    (should (equal (harmless-context-test-roots) roots))
    (should (equal timer-list timers))
    (should (equal (process-list) procs))))
