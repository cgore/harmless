;;; harmless-session-tests.el --- Tests for Harmless sessions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless-openai)
(require 'harmless-session)
(require 'harmless-util)
(require 'harmless-ui)

(ert-deftest harmless-json-ellipsis-roundtrip ()
  (let ((s "hello…world"))
    (should (equal s (plist-get (harmless-json-decode
                                 (harmless-json-encode (list :text s)))
                                :text)))))

(ert-deftest harmless-session-save-ellipsis-utf8 ()
  (let* ((harmless-directory (make-temp-file "harmless-test-" t))
         (harmless--sessions (make-hash-table :test 'equal))
         (provider (harmless-make-openai-compat
                    "local" :host "127.0.0.1:9" :protocol "http"
                    :key "none" :models '("m")))
         (harmless-providers (list provider))
         (session (harmless-session-new :cwd harmless-directory
                                        :provider provider
                                        :model "m")))
    (harmless-session-append-user session "wait… what")
    (let ((file (expand-file-name "messages.jsonl"
                                 (harmless-session-directory session))))
      (should (file-exists-p file))
      (should (string-match-p "wait… what"
                              (with-temp-buffer
                                (let ((coding-system-for-read 'utf-8-unix))
                                  (insert-file-contents file)
                                  (buffer-string))))))))

(ert-deftest harmless-parse-model-spec ()
  (should (equal '("grok-4.6" . "xhigh")
                 (harmless-parse-model-spec "grok-4.6-xhigh")))
  (should (equal '("grok-4.6" . nil)
                 (harmless-parse-model-spec "grok-4.6")))
  (should (equal '("grok-4.6" . "xhigh")
                 (harmless-parse-model-label "grok-4.6 (xhigh)")))
  (should (member "grok-4.6 (xhigh)"
                  (harmless-model-candidates
                   (harmless-make-xai :key "none"))))
  (should (equal '("grok-4.6 (xhigh)" )
                 (list (harmless-model-label "grok-4.6" "xhigh"))))
  (should (harmless-model-supports-effort-p "claude-sonnet-4-6"))
  (should-not (harmless-model-supports-effort-p "claude-sonnet-4-5"))
  (should (member "max" (harmless-model-effort-levels "claude-sonnet-4-6")))
  (should-not (member "xhigh" (harmless-model-effort-levels "claude-sonnet-4-6")))
  (should (harmless-model-supports-effort-p "gpt-5.5"))
  (should (member "xhigh" (harmless-model-effort-levels "gpt-5.5")))
  (should (member "ultra" (harmless-model-effort-levels "gpt-6-astra"))))

(ert-deftest harmless-provider-available-p-uses-key-or-oauth ()
  (should (harmless-provider-available-p
           (harmless-make-openai-compat "local" :host "h" :key "none")))
  (let ((p (harmless-make-anthropic "Anthropic"
                                    :key nil
                                    :key-env "HARMLESS_NO_SUCH_KEY"))
        (harmless-anthropic-use-claude-auth nil))
    (cl-letf (((symbol-function 'harmless-anthropic-token) (lambda () nil)))
      (should-not (harmless-provider-available-p p)))
    (cl-letf (((symbol-function 'harmless-anthropic-token)
               (lambda () "sk-ant-oat01-test")))
      (should (harmless-provider-available-p p)))))

(ert-deftest harmless-ui-header-model-is-clickable ()
  (let* ((harmless-directory (make-temp-file "harmless-test-" t))
         (harmless--sessions (make-hash-table :test 'equal))
         (provider (harmless-make-xai :key "none"))
         (harmless-providers (list provider))
         (session (harmless-session-new :cwd harmless-directory
                                        :provider provider
                                        :model "grok-4.6"
                                        :reasoning-effort "xhigh")))
    (with-temp-buffer
      (setq harmless--session session)
      (let* ((line (harmless-ui--header-line))
             (pos (string-match "grok-4.6" line)))
        (should pos)
        (should (string-match-p "grok-4.6 (xhigh)" line))
        (should (keymapp (get-text-property pos 'keymap line)))))))

(ert-deftest harmless-session-persist-resume ()
  (let* ((harmless-directory (make-temp-file "harmless-test-" t))
         (harmless--sessions (make-hash-table :test 'equal))
         (provider (harmless-make-openai-compat
                    "local" :host "127.0.0.1:9" :protocol "http"
                    :key "none" :models '("m")))
         (harmless-providers (list provider)))
    (let ((session (harmless-session-new :cwd harmless-directory
                                         :provider provider
                                         :model "m"
                                         :title "Hello world")))
      (harmless-session-append-user session "Do the thing")
      (let ((id (harmless-session-id session))
            (dir (harmless-session-directory session)))
        (setq harmless--sessions (make-hash-table :test 'equal))
        (let ((loaded (harmless-session-load dir)))
          (should (string= id (harmless-session-id loaded)))
          (should (string= "Hello world" (harmless-session-title loaded)))
          (should (string= "m" (harmless-session-model loaded)))
          (should (string= "xhigh"
                           (let ((harmless-default-reasoning-effort "xhigh"))
                             (harmless-session-effective-reasoning-effort loaded))))
          (should (equal :user (plist-get (car (harmless-session-messages loaded)) :role)))
          (should (string= "Do the thing"
                           (plist-get (car (harmless-session-messages loaded)) :content))))))))

(ert-deftest harmless-session-resume-by-id ()
  (let* ((harmless-directory (make-temp-file "harmless-test-" t))
         (harmless--sessions (make-hash-table :test 'equal))
         (provider (harmless-make-xai :key "none"))
         (harmless-providers (list provider))
         (session (harmless-session-new :cwd harmless-directory
                                        :provider provider))
         (id (harmless-session-id session)))
    (setq harmless--sessions (make-hash-table :test 'equal))
    (should (string= id (harmless-session-id (harmless-session-resume id))))))

(ert-deftest harmless-session-stores-reasoning-effort ()
  (let* ((harmless-directory (make-temp-file "harmless-test-" t))
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless-default-reasoning-effort "xhigh")
         (provider (harmless-make-xai :key "none"))
         (harmless-providers (list provider))
         (session (harmless-session-new :cwd harmless-directory
                                        :provider provider
                                        :model "grok-4.6-xhigh"))
         (id (harmless-session-id session)))
    (should (string= "grok-4.6" (harmless-session-model session)))
    (should (string= "xhigh" (harmless-session-reasoning-effort session)))
    (should (string= "grok-4.6 (xhigh)" (harmless-session-model-label session)))
    (setq harmless--sessions (make-hash-table :test 'equal))
    (let ((loaded (harmless-session-resume id)))
      (should (string= "xhigh" (harmless-session-reasoning-effort loaded))))))

(provide 'harmless-session-tests)
