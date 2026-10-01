;;; harmless-manual-tests.el --- Tests for the Harmless Info manual -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)
(require 'info)
(require 'harmless-transient)

(defun harmless-manual-test-root ()
  "Return the Harmless source root that this Emacs loaded."
  (file-name-directory
   (directory-file-name
    (file-name-directory
     (file-truename (locate-library "harmless.el" t))))))

(defun harmless-manual-test-file-text (file)
  "Return the contents of FILE, including any null bytes."
  (with-temp-buffer
    (let ((inhibit-null-byte-detection t))
      (insert-file-contents file))
    (buffer-string)))

(defun harmless-manual-test-nodes (file pattern)
  "Return node names matched by PATTERN in FILE, in order."
  (with-temp-buffer
    (insert (harmless-manual-test-file-text file))
    (goto-char (point-min))
    (let (nodes)
      (while (re-search-forward pattern nil t)
        (push (match-string 1) nodes))
      (nreverse nodes))))

(ert-deftest harmless-manual-directory-chooses-doc-beside-lisp ()
  (let* ((base (expand-file-name
                "harmless-missing-manual-dir"
                (file-name-as-directory (temporary-file-directory))))
         (lisp (file-name-as-directory (expand-file-name "lisp" base)))
         (doc (file-name-as-directory (expand-file-name "doc" base)))
         (info (expand-file-name "harmless.info" doc)))
    (should-not (file-exists-p base))
    (should (equal (harmless-manual-directory lisp) doc))
    (should (equal (harmless-manual-directory (directory-file-name lisp)) doc))
    (should (equal (harmless-manual-directory
                    (concat (directory-file-name base) "/lisp/../lisp"))
                   doc))
    (should (equal (harmless-manual-directory
                    (concat (directory-file-name base) "//lisp//"))
                   doc))
    (should (equal (harmless-manual-file lisp) info))
    (should-not (file-exists-p base))
    (should-not (file-exists-p doc))
    (should-not (file-exists-p info))))

(ert-deftest harmless-manual-directory-refuses-bad-lisp-directories ()
  (dolist (bad (list nil "" "lisp" "lisp/" "./lisp" "doc/../lisp"))
    (let ((err (should-error (harmless-manual-directory bad) :type 'error)))
      (should (equal (error-message-string err)
                     (format "Harmless Lisp directory must be absolute, not %S"
                             bad)))))
  (let ((doc-existed (file-exists-p "/doc")))
    (dolist (root (list "/" "/." "/lisp" "/lisp/" "/opt" "/tmp/../"))
      (let ((err (should-error (harmless-manual-directory root) :type 'error)))
        (should (equal (error-message-string err)
                       "Cannot place the Harmless manual at the filesystem root"))))
    (should (eq doc-existed (file-exists-p "/doc")))
    (should-not (file-exists-p "/harmless.info"))))

(ert-deftest harmless-manual-file-matches-the-loaded-checkout ()
  (let* ((lisp (harmless--lisp-directory))
         (file (harmless-manual-file)))
    (should (file-name-absolute-p file))
    (should (equal file (harmless-manual-file lisp)))
    (should (equal file (expand-file-name
                         "harmless.info"
                         (harmless-manual-directory lisp))))
    (should (file-readable-p file))
    (should (equal (file-truename file)
                   (file-truename
                    (expand-file-name "doc/harmless.info"
                                      (harmless-manual-test-root)))))))

(ert-deftest harmless-info-refuses-a-missing-manual ()
  (let* ((base (make-temp-file "harmless-manual-" t))
         (lisp (file-name-as-directory (expand-file-name "lisp" base)))
         (file (expand-file-name "harmless.info"
                                 (file-name-as-directory
                                  (expand-file-name "doc" base)))))
    (make-directory lisp t)
    (unwind-protect
        (cl-letf (((symbol-function 'harmless--lisp-directory)
                   (lambda () lisp)))
          (should-not (file-exists-p file))
          (let ((err (should-error (harmless-info) :type 'error)))
            (should (equal (error-message-string err)
                           (format "Harmless manual is missing: %s" file))))
          (should-not (file-exists-p file)))
      (delete-directory base t))))

(ert-deftest harmless-manual-register-adds-the-doc-directory-once ()
  (let ((Info-additional-directory-list nil)
        (dir (harmless-manual-directory (harmless--lisp-directory))))
    (should (equal (harmless-manual-register) dir))
    (should (cl-some (lambda (existing)
                       (harmless-same-directory-p existing dir))
                     Info-additional-directory-list))
    (let ((copy (copy-sequence Info-additional-directory-list)))
      (should (equal (harmless-manual-register) dir))
      (should (equal Info-additional-directory-list copy)))
    (let ((found (Info-find-file "harmless" t)))
      ;; Info-find-file returns the name without the .info suffix.
      (should (equal (file-name-nondirectory found) "harmless"))
      (should (harmless-same-directory-p
               (file-name-directory found)
               (file-name-directory (harmless-manual-file))))
      (should (file-readable-p
               (expand-file-name "harmless.info"
                                 (file-name-directory found)))))))

(ert-deftest harmless-manual-keys-open-the-manual ()
  (should (eq (lookup-key harmless-session-mode-map (kbd "h"))
              #'harmless-info))
  (should (eq (lookup-key harmless-dashboard-mode-map (kbd "h"))
              #'harmless-info))
  (should (eq (lookup-key harmless-usage-mode-map (kbd "q"))
              #'quit-window))
  (should (eq (lookup-key harmless-usage-mode-map (kbd "h"))
              #'harmless-info))
  (should-not (eq (lookup-key harmless-prompt-mode-map (kbd "h"))
                  #'harmless-info))
  (should (commandp #'harmless-info))
  (should (equal (transient--suffix-key
                  (transient-get-suffix 'harmless-menu 'harmless-info))
                 "h")))

(ert-deftest harmless-manual-info-contains-every-texi-node ()
  (let* ((root (harmless-manual-test-root))
         (texi (expand-file-name "doc/harmless.texi" root))
         (info (expand-file-name "doc/harmless.info" root))
         (dir (expand-file-name "doc/dir" root))
         (nodes (harmless-manual-test-nodes texi "^@node \\(.*\\)$"))
         (info-text (harmless-manual-test-file-text info))
         (dir-text (harmless-manual-test-file-text dir)))
    (should nodes)
    (should (member "Top" nodes))
    (should (member "File tools" nodes))
    (should (member "Manual" nodes))
    (dolist (node nodes)
      (should (string-match-p
               (format "Node: %s\\(?:[,[:space:]]\\|$\\)" (regexp-quote node))
               info-text)))
    (should (string-match-p "\\* Harmless: (harmless)\\." dir-text))
    (should-not (string-match-p "/Users/" info-text))
    (should-not (string-match-p "/Users/" dir-text))
    (should-not (string-match-p "\u2014" info-text))
    (should-not (string-match-p "\u2013" info-text))))

(provide 'harmless-manual-tests)
