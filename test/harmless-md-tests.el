;;; harmless-md-tests.el --- Tests for Harmless Markdown display -*- lexical-binding: t; -*-

(require 'ert)
(require 'harmless-md)

(ert-deftest harmless-md-hides-emphasis ()
  (let ((harmless-md-use-markdown-mode nil))
    (with-temp-buffer
      (harmless-md-insert "say **bold** and `code`")
      (let ((plain (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "bold" plain))
        (should (string-match-p "code" plain))
        (should-not (string-match-p "\\*\\*" plain))
        (should-not (string-match-p "`code`" plain))))))

(ert-deftest harmless-md-heading-and-list ()
  (let ((harmless-md-use-markdown-mode nil))
    (with-temp-buffer
      (harmless-md-insert "# Title\n\n- one\n- two\n")
      (let ((plain (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "Title" plain))
        (should-not (string-match-p "^#" plain))
        (should (string-match-p "• one" plain))))))

(ert-deftest harmless-md-code-fence ()
  (let ((harmless-md-use-markdown-mode nil)
        (harmless-md-fontify-code nil))
    (with-temp-buffer
      (harmless-md-insert "before\n```elisp\n(+ 1 2)\n```\nafter\n")
      (let ((plain (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "(\\+ 1 2)" plain))
        (should-not (string-match-p "```" plain))
        (should (string-match-p "before" plain))
        (should (string-match-p "after" plain))))))

(ert-deftest harmless-md-fence-after-prose-finishes ()
  (with-temp-buffer
    (harmless-md-insert
     "See **SBCL** in `utilities.lisp`.\n\n```lisp\n(sum (loop for i from 1 to 100 collect i))\n```\n\nDone.\n")
    (let ((plain (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "SBCL" plain))
      (should (string-match-p "(sum (loop" plain))
      (should (string-match-p "Done" plain)))))

(ert-deftest harmless-md-refuses-markdown-lang-mode ()
  (let ((markdown-get-lang-mode (lambda (_lang) 'gfm-mode)))
    (should-not (harmless-md--lang-mode "markdown"))
    (should-not (harmless-md--lang-mode "md")))
  (with-temp-buffer
    (harmless-md-insert "```markdown\n# Nested\n```\n")
    (should (string-match-p "Nested"
                            (buffer-substring-no-properties
                             (point-min) (point-max))))))

(ert-deftest harmless-md-link ()
  (let ((harmless-md-use-markdown-mode nil))
    (with-temp-buffer
      (harmless-md-insert "see [Emacs](https://gnu.org) now")
      (let ((plain (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "Emacs" plain))
        (should-not (string-match-p "gnu.org" plain)))
      (goto-char (point-min))
      (search-forward "Emacs")
      (should (equal "https://gnu.org"
                     (get-text-property (match-beginning 0) 'harmless-md-url))))))

(ert-deftest harmless-md-table ()
  (let ((harmless-md-use-markdown-mode nil))
    (with-temp-buffer
      (harmless-md-insert
       "before\n\n| A | B |\n| --- | --- |\n| 1 | 22 |\n\nafter\n")
      (let ((plain (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "┌" plain))
        (should (string-match-p "│ A " plain))
        (should (string-match-p "22" plain))
        (should-not (string-match-p "| ---" plain))
        (should (string-match-p "before" plain))
        (should (string-match-p "after" plain))))))

(ert-deftest harmless-md-table-alignment ()
  (let ((harmless-md-use-markdown-mode nil))
    (with-temp-buffer
      (harmless-md-insert "| L | R |\n|:--|---:|\n| a | zz |\n")
      (let ((plain (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "│ a " plain))
        (should (string-match-p " zz │" plain))))))

(provide 'harmless-md-tests)
