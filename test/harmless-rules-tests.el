;;; harmless-rules-tests.el --- Tests for path-scoped rules -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless-rules)
(require 'harmless-skills)

(defun harmless-rules-test-write (root relative text)
  "Write TEXT to `.claude/rules/RELATIVE' under ROOT and return the path."
  (let ((path (expand-file-name
               relative
               (expand-file-name ".claude/rules" root))))
    (make-directory (file-name-directory path) t)
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file path
        (insert text)))
    path))

(defun harmless-rules-test-call (args &optional name)
  "Return one assistant message calling NAME with ARGS."
  (list (list :role :assistant
              :content ""
              :tool-calls
              (list (list :id "1"
                          :name (or name "harmless-tool-read-file")
                          :args args)))))

(defun harmless-rules-test-text (dir messages)
  "Return path-rule text for DIR, or an empty string."
  (or (harmless-path-rules-text dir messages) ""))

(defun harmless-rules-test-file (path)
  "Return the text of PATH as written, with Unix newlines."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (buffer-string)))

(ert-deftest harmless-path-rules-follow-frontmatter ()
  (let* ((base (make-temp-file "harmless-rules-" t))
         (root (expand-file-name "proj" base))
         (child (expand-file-name "sub" root))
         (always (harmless-rules-test-write
                  root "a-always.md" "AAA-RULE\n"))
         (nested (harmless-rules-test-write
                  root "db/nested.md" "NESTED-ALWAYS\n"))
         (scope (harmless-rules-test-write
                 root "m-scope.md"
                 (concat "---\npaths:\n"
                         "  - \"lisp/**/*.el\"\n"
                         "  - \"*.el\"\n"
                         "---\nSCOPE-EL\n")))
         (doc (harmless-rules-test-write
               root "doc-scope.md"
               (concat "---\npaths:\n"
                       "  - doc/*\n"
                       "---\nSCOPE-DOC\n")))
         (inline (harmless-rules-test-write
                  root "inline.md"
                  (concat "---\npaths: [\"extra/*.txt\", 'keep/*.keep']\n"
                          "---\nINLINE-RULE\n")))
         (scalar (harmless-rules-test-write
                  root "scalar.md"
                  "---\npaths: \"./scalar/*.md\"\n---\nSCALAR-RULE\n"))
         (crlf (harmless-rules-test-write
                root "crlf.md"
                (concat "---\r\npaths:\r\n  - \"crlf/*\"\r\n"
                        "---\r\nCRLF-RULE\r\n")))
         (empty (harmless-rules-test-write
                 root "empty-paths.md"
                 "---\npaths: []\n---\nEMPTY-PATHS\n"))
         (blank (harmless-rules-test-write root "blank.md" "  \n"))
         (later (harmless-rules-test-write
                 root "z-always.md" "ZZZ-RULE\n"))
         (parent (harmless-rules-test-write base "parent.md" "PARENT-RULE\n"))
         (upper (harmless-rules-test-write root "upper.MD" "UPPER-MD\n"))
         (hidden (harmless-rules-test-write root ".hidden.md" "HIDDEN-RULE\n"))
         (textfile (expand-file-name
                    ".claude/rules/notes.txt" root))
         (skip-dir (expand-file-name ".claude/rules/skip.md" root)))
    (make-directory child t)
    (make-directory skip-dir t)
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file (expand-file-name "notes.txt" skip-dir)
        (insert "DIR-TEXT\n"))
      (with-temp-file textfile
        (insert "TEXT-RULE\n")))
    (let ((idle (harmless-rules-test-text root nil)))
      (should (string-match-p "AAA-RULE" idle))
      (should (string-match-p "NESTED-ALWAYS" idle))
      (should (string-match-p "ZZZ-RULE" idle))
      (should (string-match-p "UPPER-MD" idle))
      (should (string-match-p "prefer the later file" idle))
      (should (string-match-p (regexp-quote always) idle))
      (should (file-name-absolute-p always))
      (should (string-prefix-p (file-name-as-directory root) always))
      (should-not (harmless-filesystem-root-p always))
      (should (< (string-match "AAA-RULE" idle)
                 (string-match "NESTED-ALWAYS" idle)))
      (should (< (string-match "NESTED-ALWAYS" idle)
                 (string-match "ZZZ-RULE" idle)))
      (should-not (string-match-p "SCOPE-EL" idle))
      (should-not (string-match-p "SCOPE-DOC" idle))
      (should-not (string-match-p "INLINE-RULE" idle))
      (should-not (string-match-p "SCALAR-RULE" idle))
      (should-not (string-match-p "CRLF-RULE" idle))
      (should-not (string-match-p "EMPTY-PATHS" idle))
      (should-not (string-match-p "PARENT-RULE" idle))
      (should-not (string-match-p "HIDDEN-RULE" idle))
      (should-not (string-match-p "TEXT-RULE" idle))
      (should-not (string-match-p "DIR-TEXT" idle))
      (should-not (string-match-p (regexp-quote "lisp/**/*.el") idle))
      (should-not (string-match-p (regexp-quote blank) idle))
      (should-not (string-match-p (regexp-quote parent) idle)))
    (should-not (string-match-p
                 "AAA-RULE"
                 (harmless-rules-test-text child nil)))
    (let ((text (harmless-rules-test-text
                 (file-name-as-directory root) nil)))
      (should (string-match-p "AAA-RULE" text))
      (should (string-match-p (regexp-quote always) text)))
    (let ((el (harmless-rules-test-text
               root (harmless-rules-test-call '(:path "lisp/foo.el")))))
      (should (string-match-p "SCOPE-EL" el))
      (should (string-match-p "AAA-RULE" el))
      (should-not (string-match-p "SCOPE-DOC" el))
      (should (< (string-match "NESTED-ALWAYS" el)
                 (string-match "SCOPE-EL" el)))
      (should (< (string-match "SCOPE-EL" el)
                 (string-match "ZZZ-RULE" el))))
    (let ((top (harmless-rules-test-text
                root (harmless-rules-test-call '(:path "foo.el")))))
      (should (string-match-p "SCOPE-EL" top)))
    (let ((deep (harmless-rules-test-text
                 root (harmless-rules-test-call '(:path "lisp/a/foo.el")))))
      (should (string-match-p "SCOPE-EL" deep)))
    (let ((other (harmless-rules-test-text
                  root (harmless-rules-test-call '(:path "dir/foo.el")))))
      (should-not (string-match-p "SCOPE-EL" other)))
    (let ((doc-hit (harmless-rules-test-text
                    root (harmless-rules-test-call '(:path "doc/a.texi")))))
      (should (string-match-p "SCOPE-DOC" doc-hit))
      (should-not (string-match-p "SCOPE-EL" doc-hit)))
    (let ((doc-deep (harmless-rules-test-text
                     root
                     (harmless-rules-test-call '(:path "doc/sub/a.texi")))))
      (should-not (string-match-p "SCOPE-DOC" doc-deep)))
    (let ((slashes (harmless-rules-test-text
                    root
                    (harmless-rules-test-call '(:path "lisp\\a\\foo.el")))))
      (should (string-match-p "SCOPE-EL" slashes)))
    (let ((dots (harmless-rules-test-text
                 root
                 (harmless-rules-test-call
                  '(:path "./lisp/../lisp/foo.el/")))))
      (should (string-match-p "SCOPE-EL" dots)))
    (let ((escape (harmless-rules-test-text
                   root
                   (harmless-rules-test-call '(:path "../foo.el")))))
      (should-not (string-match-p "SCOPE-EL" escape))
      (should (string-match-p "AAA-RULE" escape)))
    (let ((inside (harmless-rules-test-text
                   root
                   (harmless-rules-test-call
                    (list :path (expand-file-name "lisp/foo.el" root))
                    "harmless-tool-lsp"))))
      (should (string-match-p "SCOPE-EL" inside)))
    (let ((outside (harmless-rules-test-text
                    root
                    (harmless-rules-test-call
                     (list :path (expand-file-name "outside.el" base))))))
      (should-not (string-match-p "SCOPE-EL" outside))
      (should-not (string-match-p "PARENT-RULE" outside)))
    (if (file-name-case-insensitive-p root)
        (should (string-match-p
                 "SCOPE-EL"
                 (harmless-rules-test-text
                  root
                  (harmless-rules-test-call '(:path "LISP/FOO.EL")))))
      (should-not (string-match-p
                   "SCOPE-EL"
                   (harmless-rules-test-text
                    root
                    (harmless-rules-test-call '(:path "LISP/FOO.EL"))))))
    (dolist (args '((:path nil)
                    (:path :false)
                    (:path "")
                    (:path "   ")
                    (:path "~")
                    (:path "~/")
                    (:path "/")
                    (:path "c:/")
                    (:path "c:\\")
                    (:path "d:/")
                    (:other "lisp/foo.el")))
      (should-not (string-match-p
                   "SCOPE-EL"
                   (harmless-rules-test-text
                    root (harmless-rules-test-call args)))))
    (let ((json (harmless-rules-test-text
                 root
                 (harmless-rules-test-call "{\"path\":\"lisp/foo.el\"}"))))
      (should (string-match-p "SCOPE-EL" json)))
    (let ((bad-json (harmless-rules-test-text
                     root (harmless-rules-test-call "{"))))
      (should (string-match-p "AAA-RULE" bad-json))
      (should-not (string-match-p "SCOPE-EL" bad-json)))
    (let ((hunk (harmless-rules-test-text
                 root
                 (harmless-rules-test-call
                  '(:hunks ((:path "lisp/bar.el"
                             :old_string "doc/a.texi"
                             :new_string "y")))
                  "harmless-tool-apply-patch"))))
      (should (string-match-p "SCOPE-EL" hunk))
      (should-not (string-match-p "SCOPE-DOC" hunk)))
    (should (string-match-p
             "INLINE-RULE"
             (harmless-rules-test-text
              root (harmless-rules-test-call '(:path "extra/a.txt")))))
    (should-not (string-match-p
                 "INLINE-RULE"
                 (harmless-rules-test-text
                  root
                  (harmless-rules-test-call '(:path "extra/sub/a.txt")))))
    (should (string-match-p
             "SCALAR-RULE"
             (harmless-rules-test-text
              root (harmless-rules-test-call '(:path "scalar/a.md")))))
    (should (string-match-p
             "CRLF-RULE"
             (harmless-rules-test-text
              root (harmless-rules-test-call '(:path "crlf/a")))))
    (let ((prose (harmless-rules-test-text
                  root
                  '((:role :user :content "please read lisp/foo.el")))))
      (should (string-match-p "AAA-RULE" prose))
      (should-not (string-match-p "SCOPE-EL" prose)))
    (let* ((messages (harmless-rules-test-call '(:path "lisp/foo.el")))
           (copy (copy-tree messages))
           (out (harmless-context-messages root messages))
           (system (or (harmless-messages-system-text out) "")))
      (should (equal messages copy))
      (should (string-match-p "AAA-RULE" system))
      (should (string-match-p "SCOPE-EL" system))
      (should (string-match-p "prefer the later file" system)))
    (let ((default-directory (file-name-as-directory root)))
      (let ((detached (or (harmless-messages-system-text
                           (harmless-context-messages nil nil))
                          "")))
        (should (string-match-p "detached chat" detached))
        (should-not (string-match-p "AAA-RULE" detached))))
    (should (string= (harmless-rules-test-file always) "AAA-RULE\n"))
    (should (string= (harmless-rules-test-file scope)
                     (concat "---\npaths:\n"
                             "  - \"lisp/**/*.el\"\n"
                             "  - \"*.el\"\n"
                             "---\nSCOPE-EL\n")))
    (should (string= (harmless-rules-test-file nested) "NESTED-ALWAYS\n"))
    (should (string= (harmless-rules-test-file later) "ZZZ-RULE\n"))
    (should (string= (harmless-rules-test-file doc)
                     (concat "---\npaths:\n"
                             "  - doc/*\n"
                             "---\nSCOPE-DOC\n")))
    (should (string= (harmless-rules-test-file inline)
                     (concat "---\npaths: [\"extra/*.txt\", 'keep/*.keep']\n"
                             "---\nINLINE-RULE\n")))
    (should (string= (harmless-rules-test-file scalar)
                     "---\npaths: \"./scalar/*.md\"\n---\nSCALAR-RULE\n"))
    (should (string= (harmless-rules-test-file crlf)
                     (concat "---\r\npaths:\r\n  - \"crlf/*\"\r\n"
                             "---\r\nCRLF-RULE\r\n")))
    (should (string= (harmless-rules-test-file empty)
                     "---\npaths: []\n---\nEMPTY-PATHS\n"))
    (should (string= (harmless-rules-test-file blank) "  \n"))
    (should (string= (harmless-rules-test-file upper) "UPPER-MD\n"))
    (should (string= (harmless-rules-test-file parent) "PARENT-RULE\n"))
    (should (string= (harmless-rules-test-file hidden) "HIDDEN-RULE\n"))
    (should (string= (harmless-rules-test-file textfile) "TEXT-RULE\n"))))

(ert-deftest harmless-path-rules-skip-bad-directories ()
  (let* ((base (make-temp-file "harmless-rules-skip-" t))
         (root (expand-file-name "proj" base))
         (slash-claude (file-exists-p "/.claude"))
         (drive-claude (file-exists-p "c:/.claude"))
         (d-claude (file-exists-p "d:/.claude")))
    (harmless-rules-test-write root "a-always.md" "AAA-RULE\n")
    (harmless-rules-test-write
     root "root-pattern.md"
     (concat "---\npaths:\n  - /\n  - c:/\n  - \"c:\\\\\"\n  - \"\"\n"
             "---\nROOT-PATTERN\n"))
    (let ((default-directory (file-name-as-directory root))
          (hit (harmless-rules-test-call '(:path "lisp/foo.el"))))
      (dolist (dir (list nil "" "   " "notes" "sub/" "proj" "proj/"
                         "." "./" "~" "~/" "/" "//" "/." "\\"
                         "c:" "c:/" "c:\\" "C:/" "d:/" "c:notes"))
        (let ((text (harmless-rules-test-text dir hit)))
          (should-not (string-match-p "AAA-RULE" text))
          (should-not (string-match-p "ROOT-PATTERN" text))))
      (let ((default-directory (file-name-as-directory base)))
        (should-not (string-match-p
                     "AAA-RULE"
                     (harmless-rules-test-text "proj" hit)))
        (should-not (string-match-p
                     "AAA-RULE"
                     (harmless-rules-test-text "proj/" hit))))
      (let ((text (harmless-rules-test-text root hit)))
        (should (string-match-p "AAA-RULE" text))
        (should-not (string-match-p "ROOT-PATTERN" text))))
    (let ((empty (make-temp-file "harmless-norules-" t)))
      (should-not (harmless-path-rules-text empty nil))
      (should-not (harmless-path-rules-text empty ""))
      (should-not (file-exists-p (expand-file-name ".claude" empty))))
    (should (eq slash-claude (file-exists-p "/.claude")))
    (should (eq drive-claude (file-exists-p "c:/.claude")))
    (should (eq d-claude (file-exists-p "d:/.claude")))))
