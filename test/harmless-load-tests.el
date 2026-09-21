;;; harmless-load-tests.el --- Load smoke test for Harmless -*- lexical-binding: t; -*-

(require 'ert)
(require 'harmless)

(ert-deftest harmless-load-provides ()
  (should (featurep 'harmless))
  (should (fboundp 'harmless))
  (should (fboundp 'harmless-make-xai))
  (should (fboundp 'harmless-make-anthropic))
  (should (fboundp 'harmless-make-openai))
  (should (harmless-login-method 'openai))
  (should (harmless-tool-by-name "read_file"))
  (should (harmless-tool-by-name "run_shell")))

(provide 'harmless-load-tests)
