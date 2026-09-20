;;; harmless-session-tests.el --- Tests for Harmless sessions -*- lexical-binding: t; -*-

(require 'ert)
(require 'harmless-openai)
(require 'harmless-session)
(require 'harmless-util)

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
  (should (equal '("grok-4.6 (xhigh)" )
                 (list (harmless-model-label "grok-4.6" "xhigh")))))

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
