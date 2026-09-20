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

(provide 'harmless-session-tests)
