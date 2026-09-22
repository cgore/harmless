;;; harmless-memory-tests.el --- Tests for Harmless memory -*- lexical-binding: t; -*-

(require 'ert)
(require 'harmless-memory)
(require 'harmless-skills)

(defmacro harmless-memory-test-with-root (&rest body)
  "Run BODY with `harmless-directory' in a temporary directory."
  `(let ((harmless-directory (make-temp-file "harmless-memory-" t)))
     ,@body))

(ert-deftest harmless-memory-remember-read-and-index ()
  (harmless-memory-test-with-root
   (let ((cwd "/proj/harmless"))
     (let ((rel (harmless-memory-remember
                 "workspace" cwd "Two spaces" "Prefer two spaces in Elisp." "emacs")))
       (should (string-prefix-p "observations/_inbox/" rel))
       (should (string-match-p "Prefer two spaces"
                               (harmless-memory-read "workspace" cwd rel))))
     (should-error (harmless-memory-read "workspace" cwd "../auth.json"))
     (should-error (harmless-memory-read "workspace" cwd "/etc/passwd"))
     (let ((index (harmless-memory-read "workspace" cwd "MEMORY.md"))
           (catalog (harmless-memory-catalog cwd)))
       (should (string-match-p "Do not edit" index))
       (should (string-match-p "Two spaces" index))
       (should (string-match-p "Pending" catalog))
       (should (string-match-p "memory_read" catalog))
       (should-not (string-match-p "### Global" catalog))))))

(ert-deftest harmless-memory-dream-folds-inbox ()
  (harmless-memory-test-with-root
   (let ((cwd "/proj/harmless"))
     (harmless-memory-write-topic
      "workspace" cwd "emacs" "# Emacs\n\nThe editor.\n")
     (harmless-memory-remember
      "workspace" cwd "Indent" "Use two spaces." "emacs")
     (harmless-memory-remember
      "global" cwd "PRs" "Open the pull request after pushing." "git")
     (should (= 1 (harmless-memory-dream "workspace" cwd)))
     (should (= 1 (harmless-memory-dream "global" cwd)))
     (should (= 0 (harmless-memory-dream "workspace" cwd)))
     (let ((topic (harmless-memory-read "workspace" cwd "topics/emacs.md")))
       (should (string-match-p "The editor" topic))
       (should (string-match-p "## Indent" topic))
       (should (string-match-p "Use two spaces" topic)))
     (should (string-match-p "Open the pull request"
                             (harmless-memory-read "global" cwd "topics/git.md")))
     (let ((catalog (harmless-memory-catalog cwd)))
       (should (string-match-p "### Workspace" catalog))
       (should (string-match-p "### Global" catalog))
       (should (string-match-p "topics/emacs.md" catalog))
       (should-not (string-match-p "Pending:" catalog))))))

(ert-deftest harmless-memory-scopes-are-separate ()
  (harmless-memory-test-with-root
   (harmless-memory-remember "workspace" "/proj/a" "A" "workspace a" "place")
   (harmless-memory-remember "workspace" "/proj/b" "B" "workspace b" "place")
   (let ((catalog (harmless-memory-catalog "/proj/a")))
     (should (string-match-p "\\*\\*A\\*\\*" catalog))
     (should-not (string-match-p "\\*\\*B\\*\\*" catalog)))))

(ert-deftest harmless-memory-context-includes-index ()
  (harmless-memory-test-with-root
   (harmless-memory-write-topic
    "workspace" "/proj/harmless" "emacs" "# Emacs\n\nPrefer two spaces.\n")
   (let ((messages (harmless-context-messages "/proj/harmless" nil "/proj/harmless")))
     (should (eq :system (plist-get (car messages) :role)))
     (should (string-match-p "topics/emacs.md"
                             (plist-get (car messages) :content)))
     (should (string-match-p "Prefer two spaces"
                             (plist-get (car messages) :content))))))
