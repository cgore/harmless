;;; harmless-load-tests.el --- Load smoke test for Harmless -*- lexical-binding: t; -*-

(require 'ert)
(require 'harmless)

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

(provide 'harmless-load-tests)
