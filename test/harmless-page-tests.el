;;; harmless-page-tests.el --- Tests for publishing a page -*- lexical-binding: t; -*-

(require 'ert)
(require 'harmless)

(defun harmless-page-test-signal (fn)
  "Return the error text from FN."
  (error-message-string (should-error (funcall fn) :type 'error)))

(defun harmless-page-test-read (path)
  "Return PATH as text."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (buffer-string)))

(defun harmless-page-test-write (path text)
  "Write TEXT to PATH."
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file path
      (insert text))))

(defconst harmless-page-test-markdown
  "# HPAGE-TITLE\n\nHello <script>.\n\nSee `code` and [the note](notes/a.md).\n\n```elisp\n(+ 1 2)\n```\n"
  "Markdown fixture.")

(defconst harmless-page-test-html
  (concat "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"utf-8\">\n"
          "<title>HPAGE-TITLE</title>\n</head>\n<body>\n"
          "<h1>HPAGE-TITLE</h1>\n"
          "<p>Hello &lt;script&gt;.</p>\n"
          "<p>See <code>code</code> and "
          "<a href=\"notes/a.md\">the note</a>.</p>\n"
          "<pre><code class=\"language-elisp\">(+ 1 2)</code></pre>\n"
          "</body>\n</html>\n")
  "Rendered fixture.")

(defun harmless-page-test-roots ()
  "Return existence of page files at filesystem roots."
  (list (file-exists-p "/page.html")
        (file-exists-p "c:/page.html")
        (file-exists-p "d:/page.html")
        (file-exists-p "/.harmless")
        (file-exists-p "c:/.harmless")
        (file-exists-p "d:/.harmless")))

(ert-deftest harmless-page-publishes-markdown-and-html ()
  (let* ((base (make-temp-file "harmless-page-" t))
         (root (expand-file-name "proj" base))
         (elsewhere (expand-file-name "elsewhere" base))
         (source (expand-file-name "page.md" root))
         (nested (expand-file-name "docs/page.md" root))
         (html (expand-file-name "frag.html" root))
         (whole (expand-file-name "whole.html" root))
         (default-page (expand-file-name ".harmless/pages/page.html" root))
         (relative (expand-file-name "out/page.html" root))
         (outside (expand-file-name "copy.html" elsewhere)))
    (make-directory root t)
    (make-directory elsewhere t)
    (make-directory (file-name-directory nested) t)
    (harmless-page-test-write source harmless-page-test-markdown)
    (harmless-page-test-write
     nested (replace-regexp-in-string "\n" "\r\n" harmless-page-test-markdown))
    (harmless-page-test-write html "<p>HPAGE-FRAG</p>")
    (harmless-page-test-write
     whole "<!DOCTYPE html><html><body>HPAGE-HTML</body></html>")
    (let ((default-directory (file-name-as-directory elsewhere)))
      (let ((page (harmless-publish "page.md" (file-name-as-directory root))))
        (should (equal page default-page))
        (should (file-regular-p page))
        (should (file-name-absolute-p page))
        (should (harmless-directory-strictly-under-p page root))
        (should-not (equal (file-truename page) (file-truename "/")))
        (should (equal (harmless-page-test-read page)
                       harmless-page-test-html))
        (should (equal (harmless-page-test-read source)
                       harmless-page-test-markdown)))
      (let ((page (harmless-page-publish root "docs/page.md" "out/page.html")))
        (should (equal page relative))
        (should (equal (harmless-page-test-read page)
                       harmless-page-test-html))
        (should-not (file-exists-p
                     (expand-file-name "out/page.html" elsewhere))))
      (let ((page (harmless-page-publish root "page.md" outside)))
        (should (equal page outside))
        (should (file-regular-p outside))
        (should-not (harmless-directory-strictly-under-p outside root))
        (should (equal (harmless-page-test-read outside)
                       harmless-page-test-html)))
      (harmless-page-test-write source "# Next\n\nHPAGE-NEXT\n")
      (harmless-page-publish root "page.md" "out/page.html")
      (should (string-search "HPAGE-NEXT" (harmless-page-test-read relative)))
      (should (string-search "<h1>Next</h1>" (harmless-page-test-read relative)))
      (let ((frag (harmless-page-publish root "frag.html")))
        (should (equal frag
                       (expand-file-name ".harmless/pages/frag.html" root)))
        (should (equal (harmless-page-test-read frag)
                       (concat "<!DOCTYPE html>\n<html>\n<head>\n"
                               "<meta charset=\"utf-8\">\n<title>frag</title>\n"
                               "</head>\n<body>\n<p>HPAGE-FRAG</p>\n"
                               "</body>\n</html>\n"))))
      (let ((copied (harmless-page-publish root "whole.html" "out/whole.html")))
        (should (equal (harmless-page-test-read copied)
                       "<!DOCTYPE html><html><body>HPAGE-HTML</body></html>\n"))
        (should (equal (harmless-page-test-read whole)
                       "<!DOCTYPE html><html><body>HPAGE-HTML</body></html>"))))))

(ert-deftest harmless-page-refuses-a-bad-request ()
  (let* ((base (make-temp-file "harmless-page-bad-" t))
         (root (expand-file-name "proj" base))
         (outside (expand-file-name "secret.md" base))
         (file (make-temp-file "harmless-page-file-"))
         (source (expand-file-name "page.md" root))
         (roots (harmless-page-test-roots)))
    (make-directory root t)
    (harmless-page-test-write source "# Kept\n")
    (harmless-page-test-write outside "# Secret\n")
    (harmless-page-test-write
     (expand-file-name "self.html" root) "<p>Self</p>")
    (unwind-protect
        (let ((default-directory (file-name-as-directory root))
              (before (harmless-page-test-read source)))
          (dolist (directory (list nil "" "   " "\n"))
            (should (equal (harmless-page-test-signal
                            (lambda ()
                              (harmless-page-publish directory "page.md")))
                           "page directory is empty")))
          (should (equal (harmless-page-test-signal
                          (lambda ()
                            (harmless-page-publish 42 "page.md")))
                         "page directory must be a string"))
          (dolist (directory (list "/" "c:/" "d:/" "c:\\" "C:/" "c:"
                                   "\\" "//" "/." " / " " c:/ "))
            (should (equal (harmless-page-test-signal
                            (lambda ()
                              (harmless-page-publish directory "page.md")))
                           "page directory is the filesystem root")))
          (dolist (directory (list "notes" "notes/" "sub/" "  notes  "
                                   "~" "~/proj"))
            (should (equal (harmless-page-test-signal
                            (lambda ()
                              (harmless-page-publish directory "page.md")))
                           "page directory must be absolute")))
          (should (equal (harmless-page-test-signal
                          (lambda ()
                            (harmless-page-publish
                             (expand-file-name "missing-page" base)
                             "page.md")))
                         "page directory is missing"))
          (should (equal (harmless-page-test-signal
                          (lambda ()
                            (harmless-page-publish file "page.md")))
                         "page directory is not a directory"))
          (dolist (bad (list nil "" "   "))
            (should (equal (harmless-page-test-signal
                            (lambda ()
                              (harmless-page-publish root bad)))
                           "page source is empty")))
          (should (equal (harmless-page-test-signal
                          (lambda () (harmless-page-publish root 42)))
                         "page source must be a string"))
          (dolist (bad (list "/" "c:/" "d:/" "c:\\" "C:/" "c:" "\\" "//" "/."))
            (should (equal (harmless-page-test-signal
                            (lambda () (harmless-page-publish root bad)))
                           "page source is the filesystem root")))
          (dolist (bad (list "~" "~/page.md"))
            (should (equal (harmless-page-test-signal
                            (lambda () (harmless-page-publish root bad)))
                           "page source must be absolute")))
          (should (equal (harmless-page-test-signal
                          (lambda ()
                            (harmless-page-publish root "missing.md")))
                         "page source is missing"))
          (should (equal (harmless-page-test-signal
                          (lambda ()
                            (harmless-page-publish root "page.md/")))
                         "page source is not a file"))
          (should (equal (harmless-page-test-signal
                          (lambda ()
                            (harmless-page-publish root "../secret.md")))
                         "page source escapes the project: ../secret.md"))
          (harmless-page-test-write (expand-file-name "note.txt" root) "x")
          (should (equal (harmless-page-test-signal
                          (lambda ()
                            (harmless-page-publish root "note.txt")))
                         "page source is not a page"))
          (should (equal (harmless-page-test-signal
                          (lambda ()
                            (harmless-page-publish root "page.md" "")))
                         "page path is empty"))
          (should (equal (harmless-page-test-signal
                          (lambda ()
                            (harmless-page-publish root "page.md" 42)))
                         "page path must be a string"))
          (dolist (bad (list "/" "c:/" "d:/" "c:\\" "C:/" "c:" "\\" "//" "/."))
            (should (equal (harmless-page-test-signal
                            (lambda ()
                              (harmless-page-publish root "page.md" bad)))
                           "page path is the filesystem root")))
          (should (equal (harmless-page-test-signal
                          (lambda ()
                            (harmless-page-publish root "page.md" "~/x.html")))
                         "page path must be absolute"))
          (should (equal (harmless-page-test-signal
                          (lambda ()
                            (harmless-page-publish root "page.md" "out/")))
                         "page path is not a file"))
          (should (equal (harmless-page-test-signal
                          (lambda ()
                            (harmless-page-publish root "page.md" "../x.html")))
                         "page path escapes the project: ../x.html"))
          (should (equal (harmless-page-test-signal
                          (lambda ()
                            (harmless-page-publish root "page.md" "out/page.txt")))
                         "page path is not a page"))
          (should (equal (harmless-page-test-signal
                          (lambda ()
                            (harmless-page-publish
                             root "page.md"
                             (expand-file-name "missing/nope.html" root))))
                         "page path is missing"))
          (should (equal (harmless-page-test-signal
                          (lambda ()
                            (harmless-page-publish
                             root "self.html"
                             (expand-file-name "self.html" root))))
                         "page path is the source"))
          (should (equal (harmless-page-test-read source) before))
          (should (equal (harmless-page-test-read
                          (expand-file-name "self.html" root))
                         "<p>Self</p>"))
          (should (equal (harmless-page-test-read outside) "# Secret\n"))
          (should-not (file-exists-p
                       (expand-file-name "notes" root)))
          (should-not (file-directory-p
                       (expand-file-name ".harmless" root)))
          (should-not (file-directory-p
                       (expand-file-name "out" root)))
          (should-not (file-exists-p
                       (expand-file-name "out/page.html" root)))
          (should (equal roots (harmless-page-test-roots))))
      (when (file-exists-p file)
        (delete-file file)))))
