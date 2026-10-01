;;; harmless-load-tests.el --- Load smoke test for Harmless -*- lexical-binding: t; -*-

(require 'ert)
(require 'harmless)

(defvar harmless-config-test-loaded nil
  "Set by a temporary config.el in `harmless-config-file-refuses-filesystem-root'.")

(ert-deftest harmless-load-provides ()
  (should (featurep 'harmless))
  (should (fboundp 'harmless))
  (should (fboundp 'harmless-reload-all-harmless))
  (should (fboundp 'harmless-make-xai))
  (should (fboundp 'harmless-make-anthropic))
  (should (fboundp 'harmless-make-openai))
  (should (harmless-login-method 'openai))
  (should (harmless-tool-by-name "read_file"))
  (should (harmless-tool-by-name "run_shell")))

(ert-deftest harmless-reload-all-harmless-reloads-sources ()
  (let* ((previous load-prefer-newer)
         (dir (file-name-directory
               (file-truename (locate-library "harmless.el" t))))
         (expected
          (cl-remove-if
           (lambda (file)
             (string= (file-name-nondirectory file) "harmless-autoloads.el"))
           (directory-files dir t "\\.el\\'"))))
    (unwind-protect
        (progn
          (setq load-prefer-newer nil)
          (let ((loaded (harmless-reload-all-harmless)))
            (should (equal loaded expected))
            (should (> (length loaded) 20))
            (should (eq load-prefer-newer t))
            (should (fboundp 'harmless-pick-model))
            (should (featurep 'harmless))))
      (setq load-prefer-newer previous))))

(ert-deftest harmless-config-file-refuses-filesystem-root ()
  (let ((harmless-directory "/")
        (default-directory "/")
        (harmless--config-loaded nil))
    (let ((err (should-error (harmless-config-file) :type 'error)))
      (should (string-match-p "Refusing to store Harmless data"
                              (error-message-string err))))
    (should-not (file-exists-p "/config.el")))
  (let* ((fallback (make-temp-file "harmless-cfg-" t))
         (harmless-directory nil)
         (default-directory "/")
         (harmless--config-loaded nil))
    (cl-letf (((symbol-function 'locate-user-emacs-file)
               (lambda (&rest _) fallback)))
      (should (equal (expand-file-name "config.el" (harmless-data-directory))
                     (harmless-config-file)))
      (with-temp-file (harmless-config-file)
        (insert ";;; -*- lexical-binding: t; -*-\n"
                "(setq harmless-config-test-loaded t)\n"))
      (setq harmless-config-test-loaded nil)
      (harmless-load-config)
      (should harmless-config-test-loaded))
    (should-not (file-exists-p "/config.el"))))

(provide 'harmless-load-tests)
