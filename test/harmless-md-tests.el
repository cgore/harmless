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

(provide 'harmless-md-tests)
