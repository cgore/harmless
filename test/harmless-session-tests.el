;;; harmless-session-tests.el --- Tests for Harmless sessions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless-openai)
(require 'harmless-session)
(require 'harmless-util)
(require 'harmless-ui)
(require 'harmless)

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
  (should (member "grok-4.7" (harmless-provider-model-list
                                 (harmless-make-xai :key "none"))))
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
    (cl-letf (((symbol-function 'harmless-anthropic-token)
               (lambda (&rest _) nil)))
      (should-not (harmless-provider-available-p p)))
    (cl-letf (((symbol-function 'harmless-anthropic-token)
               (lambda (&rest _) "sk-ant-oat01-test")))
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

(defun harmless-test-error (pattern thunk)
  "Signal from THUNK must be an error whose text matches PATTERN."
  (let ((err (should-error (funcall thunk) :type 'error)))
    (should (string-match-p pattern (error-message-string err)))
    err))

(defun harmless-test-provider ()
  "Return a local provider for session tests."
  (harmless-make-openai-compat
   "local" :host "127.0.0.1:9" :protocol "http"
   :key "none" :models '("m")))

(ert-deftest harmless-path-root-and-collapse ()
  (should (harmless-filesystem-root-p "/"))
  (should (harmless-filesystem-root-p "//"))
  (should (harmless-filesystem-root-p "/."))
  (should (harmless-filesystem-root-p "/.."))
  (should (harmless-filesystem-root-p "/sessions/.."))
  (should-not (harmless-filesystem-root-p nil))
  (should-not (harmless-filesystem-root-p ""))
  (should-not (harmless-filesystem-root-p "sessions"))
  (should-not (harmless-filesystem-root-p "/tmp"))
  (should (equal "/b" (harmless-collapse-path "/a/../b")))
  (should (equal "/" (harmless-collapse-path "/sessions/..")))
  (should (equal "/tmp/sessions/%2F/uuid"
                 (harmless-collapse-path "/tmp/sessions/%2F/uuid")))
  (should (equal "c:/Users/proj"
                 (harmless-collapse-path "c:/Users/../Users/proj")))
  (should (equal "c:/" (harmless-collapse-path "c:/")))
  (should (equal "c:/" (harmless-collapse-path "c:/Users/..")))
  (should (equal "D:/a/b" (harmless-collapse-path "D:\\a\\b\\.")))
  (should-not (harmless-filesystem-root-p "c:/"))
  (should-not (harmless-filesystem-root-p "c:/Users"))
  (should (harmless-directory-strictly-under-p "c:/proj/a" "c:/proj"))
  (should-not (harmless-directory-strictly-under-p "c:/proj" "d:/proj"))
  (should-not (harmless-directory-strictly-under-p "d:/c:/proj/a" "c:/proj"))
  (should-not (harmless-filesystem-root-p (expand-file-name "~")))
  (should (harmless-filesystem-root-p "/../.."))
  (harmless-test-error "Not an absolute path"
                       (lambda () (harmless-collapse-path "relative")))
  (harmless-test-error "Refusing to create"
                       (lambda () (harmless-ensure-directory nil)))
  (harmless-test-error "Refusing to create"
                       (lambda () (harmless-ensure-directory "")))
  (harmless-test-error "Refusing to create"
                       (lambda () (harmless-ensure-directory "/")))
  (harmless-test-error "Refusing to create"
                       (lambda () (harmless-ensure-directory "/sessions/..")))
  (should (harmless-safe-path-component-p "..foo"))
  (should (harmless-safe-path-component-p "%2Fproj"))
  (should-not (harmless-safe-path-component-p nil))
  (should-not (harmless-safe-path-component-p ""))
  (should-not (harmless-safe-path-component-p "."))
  (should-not (harmless-safe-path-component-p ".."))
  (should-not (harmless-safe-path-component-p "/tmp"))
  (should-not (harmless-safe-path-component-p "foo/bar"))
  (should-not (harmless-safe-path-component-p "foo\\bar"))
  (should (harmless-directory-strictly-under-p
           "/tmp/harmless/sessions/a" "/tmp/harmless/sessions"))
  (should (harmless-directory-strictly-under-p
           "/tmp/harmless/sessions/a/" "/tmp/harmless/sessions"))
  (should-not (harmless-directory-strictly-under-p
               "/tmp/harmless/sessions" "/tmp/harmless/sessions"))
  (should-not (harmless-directory-strictly-under-p
               "/tmp/harmless-evil/sessions/a" "/tmp/harmless/sessions"))
  (should-not (harmless-directory-strictly-under-p "/tmp/a" "/"))
  (should-not (harmless-directory-strictly-under-p "relative" "/tmp"))
  (should-not (harmless-directory-strictly-under-p nil "/tmp"))
  (should (harmless-same-directory-p "/proj/demo" "/proj/demo/"))
  (should (harmless-same-directory-p "/proj/demo/" "/proj/demo"))
  (should-not (harmless-same-directory-p "/proj/demo" "/proj/demo-evil"))
  (should-not (harmless-same-directory-p nil "/"))
  (should-not (harmless-same-directory-p "" "/"))
  (should-not (harmless-same-directory-p "relative" "/relative"))
  (let ((dir (expand-file-name "nested" (make-temp-file "harmless-mkdir-" t))))
    (should (file-directory-p (harmless-ensure-directory dir)))))

(ert-deftest harmless-absolute-directory-keeps-a-leading-slash-on-windows ()
  "A leading slash must stay a leading slash when Emacs is on Windows.
`expand-file-name' pastes the current drive onto it."
  (let ((system-type 'windows-nt))
    (should (equal "/" (harmless-absolute-directory "/")))
    (should (equal "/" (harmless-absolute-directory "//")))
    (should (equal "/" (harmless-absolute-directory "/.")))
    (should (equal "/" (harmless-absolute-directory "/sessions/..")))
    (should (equal "/tmp/" (harmless-absolute-directory "/tmp/")))
    (should (equal "/tmp" (harmless-absolute-directory "/tmp")))
    (should (equal "/proj/demo" (harmless-absolute-directory "/proj/demo")))
    (should (equal "/demo/"
                   (harmless-absolute-directory "/proj/../demo/")))
    (should (equal "/readme"
                   (harmless-expand-file-name "readme" "/")))
    (should (equal "/proj/a"
                   (harmless-expand-file-name "a/../a" "/proj")))
    (should (equal "c:/proj"
                   (let ((system-type 'gnu/linux))
                     (harmless-expand-file-name "c:/proj" "/"))))))

(ert-deftest harmless-data-directory-refuses-root ()
  (let ((default-directory "/"))
    (harmless-test-error "Refusing to store Harmless data"
                         (lambda () (harmless-resolve-data-directory "/")))
    (harmless-test-error "Refusing to store Harmless data"
                         (lambda () (harmless-resolve-data-directory "/sessions/..")))
    (harmless-test-error "must be absolute"
                         (lambda () (harmless-resolve-data-directory "relative")))
    (let* ((fallback (make-temp-file "harmless-fallback-" t))
           (resolved nil))
      (cl-letf (((symbol-function 'locate-user-emacs-file)
                 (lambda (&rest _) fallback)))
        (setq resolved (harmless-resolve-data-directory nil))
        (should (equal resolved (harmless-resolve-data-directory ""))))
      (should (string-prefix-p (file-name-as-directory fallback) resolved))
      (should-not (harmless-filesystem-root-p resolved)))
    (let ((dir (make-temp-file "harmless-data-" t)))
      (should (equal (file-name-as-directory dir)
                     (harmless-resolve-data-directory dir))))))

(ert-deftest harmless-encode-cwd-does-not-expand-missing-directory ()
  (let ((default-directory "/"))
    (should (equal "%2F" (harmless-session-encode-cwd "/")))
    (should (equal "%2Fproj%2Fdemo" (harmless-session-encode-cwd "/proj/demo/")))
    (should-not (string-search "/" (harmless-session-encode-cwd "/proj/demo")))
    (should (equal "%2Fproj%2Fmy..notes"
                   (harmless-session-encode-cwd "/proj/my..notes")))
    (harmless-test-error "must be absolute"
                         (lambda () (harmless-session-encode-cwd nil)))
    (harmless-test-error "must be absolute"
                         (lambda () (harmless-session-encode-cwd "")))
    (harmless-test-error "must be absolute"
                         (lambda () (harmless-session-encode-cwd "relative")))
    (harmless-test-error "must be absolute"
                         (lambda () (harmless-session-encode-cwd "..")))
    (harmless-test-error "must be absolute"
                         (lambda () (harmless-absolute-directory nil)))
    (harmless-test-error "must be absolute"
                         (lambda () (harmless-absolute-directory "")))
    (should-not (file-exists-p "/summary.json"))))

(ert-deftest harmless-current-cwd-requires-an-absolute-directory ()
  (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
    (with-temp-buffer
      (setq default-directory "/tmp/")
      (should (equal "/tmp/" (harmless-current-cwd))))
    (with-temp-buffer
      (setq default-directory "relative")
      (harmless-test-error "must be absolute"
                           (lambda () (harmless-current-cwd))))
    (should-not (harmless-session-for-cwd nil))
    (should-not (harmless-session-for-cwd ""))
    (should-not (harmless-session-for-cwd "relative"))))

(ert-deftest harmless-session-project-name-ignores-root-default ()
  (let ((default-directory "/"))
    (should (equal "chat" (harmless-session-project-name
                          (harmless-session--create :cwd nil))))
    (should (equal "?" (harmless-session-project-name
                        (harmless-session--create :cwd ""))))
    (should (equal "/" (harmless-session-project-name
                        (harmless-session--create :cwd "/"))))))

(defun harmless-test--assert-session-not-at-root (session data-dir)
  "Assert SESSION is stored under DATA-DIR and not at /summary.json."
  (let ((dir (harmless-session-directory session))
        (summary (expand-file-name
                  "summary.json" (harmless-session-directory session))))
    (should (string-prefix-p (file-name-as-directory data-dir) dir))
    (should (file-exists-p summary))
    (should-not (equal summary "/summary.json"))
    (should-not (harmless-filesystem-root-p dir))
    (should-not (file-exists-p "/summary.json"))
    (should-not (file-exists-p "/messages.jsonl"))))

(ert-deftest harmless-session-new-missing-cwd-keeps-storage-off-root ()
  (let ((harmless-directory (make-temp-file "harmless-test-" t))
        (harmless--sessions (make-hash-table :test 'equal))
        (default-directory "/")
        (provider (harmless-test-provider)))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
      (let ((session (harmless-session-new :provider provider :model "m")))
        (should (equal "/" (harmless-session-cwd session)))
        (harmless-test--assert-session-not-at-root session harmless-directory)
        (should (string-search "/sessions/%2F/"
                               (file-name-as-directory
                                (harmless-session-directory session)))))
      (harmless-test-error "must be absolute"
                           (lambda ()
                             (harmless-session-new :cwd ""
                                                   :provider provider
                                                   :model "m")))
      (harmless-test-error "must be absolute"
                           (lambda ()
                             (harmless-session-new :cwd "relative"
                                                   :provider provider
                                                   :model "m"))))
    (should-not (file-exists-p "/summary.json"))))

(ert-deftest harmless-session-root-cwd-stays-under-data-directory ()
  (let* ((harmless-directory (make-temp-file "harmless-test-" t))
         (harmless--sessions (make-hash-table :test 'equal))
         (default-directory "/")
         (provider (harmless-test-provider))
         (session (harmless-session-new :cwd "/" :provider provider :model "m")))
    (harmless-test--assert-session-not-at-root session harmless-directory)
    (should (string-search "/sessions/%2F/"
                           (file-name-as-directory
                            (harmless-session-directory session))))
    (harmless-session-append-user session "from root cwd")
    (should (string-match-p "from root cwd"
                            (with-temp-buffer
                              (insert-file-contents
                               (expand-file-name
                                "messages.jsonl"
                                (harmless-session-directory session)))
                              (buffer-string))))))

(ert-deftest harmless-session-blank-stored-directory-is-recomputed ()
  (let* ((harmless-directory (make-temp-file "harmless-test-" t))
         (harmless--sessions (make-hash-table :test 'equal))
         (default-directory "/")
         (provider (harmless-test-provider))
         (session (harmless-session-new :cwd "/proj/demo"
                                        :provider provider
                                        :model "m")))
    (dolist (bad '("" "/"))
      (setf (harmless-session-directory session) bad)
      (harmless-session-append-user session (format "kept %s" bad))
      (harmless-test--assert-session-not-at-root session harmless-directory))
    (should (string-search "/sessions/%2Fproj%2Fdemo/"
                           (file-name-as-directory
                            (harmless-session-directory session))))
    (should (string-match-p "kept /"
                            (with-temp-buffer
                              (insert-file-contents
                               (expand-file-name
                                "messages.jsonl"
                                (harmless-session-directory session)))
                              (buffer-string))))))

(ert-deftest harmless-session-unsafe-id-does-not-write-root ()
  (let ((harmless-directory (make-temp-file "harmless-test-" t))
        (default-directory "/"))
    (dolist (id '("" "." ".." "/" "/tmp" "foo/bar" "foo\\bar"))
      (let ((session (harmless-session--create :id id :cwd "/proj/demo")))
        (harmless-test-error "not a safe filename"
                             (lambda () (harmless-session-dir session)))
        (harmless-test-error "not a safe filename"
                             (lambda () (harmless-session-save session)))))
    (should-not (file-exists-p "/summary.json"))
    (should-not (file-exists-p "/messages.jsonl"))))

(ert-deftest harmless-session-nil-data-directory-uses-fallback-not-root ()
  (let* ((fallback (make-temp-file "harmless-fallback-" t))
         (harmless-directory nil)
         (harmless--sessions (make-hash-table :test 'equal))
         (default-directory "/")
         (provider (harmless-test-provider))
         (root-sessions "/sessions")
         (sessions-existed (file-directory-p root-sessions))
         (sessions-before
          (when sessions-existed
            (sort (directory-files root-sessions nil
                                   directory-files-no-dot-files-regexp)
                  #'string<))))
    (cl-letf (((symbol-function 'locate-user-emacs-file)
               (lambda (&rest _) fallback)))
      (let ((session (harmless-session-new :cwd "/proj/demo"
                                           :provider provider
                                           :model "m")))
        (harmless-test--assert-session-not-at-root session fallback)
        (should (string-prefix-p (file-name-as-directory fallback)
                                 (harmless-sessions-root)))
        (should (harmless-session-resume (harmless-session-id session)))))
    (should (eq sessions-existed (file-directory-p root-sessions)))
    (should (equal sessions-before
                   (when (file-directory-p root-sessions)
                     (sort (directory-files root-sessions nil
                                            directory-files-no-dot-files-regexp)
                           #'string<))))))

(ert-deftest harmless-session-load-and-list-refuse-root ()
  (let ((harmless-directory (make-temp-file "harmless-test-" t))
        (default-directory "/"))
    (harmless-test-error "Refusing to load"
                         (lambda () (harmless-session-load "/")))
    (harmless-test-error "Refusing to load"
                         (lambda () (harmless-session-load "")))
    (harmless-test-error "Refusing to load"
                         (lambda () (harmless-session-load "/tmp"))))
  (let ((harmless-directory "/")
        (default-directory "/"))
    (harmless-test-error "Refusing to store Harmless data"
                         (lambda () (harmless-session-list-on-disk)))
    (harmless-test-error "Refusing to store Harmless data"
                         (lambda () (harmless-data-directory)))
    (should-not (file-exists-p "/summary.json"))))

(ert-deftest harmless-ui-missing-cwd-does-not-become-root ()
  (let* ((harmless-directory (make-temp-file "harmless-test-" t))
         (session (harmless-session--create
                   :id "abcdef0123456789abcdef0123456789"
                   :cwd nil))
         (known (harmless-session--create
                 :id "abcdef0123456789abcdef0123456789"
                 :cwd "/proj/demo"))
         buf pbuf kbuf kpbuf)
    (unwind-protect
        (with-temp-buffer
          (setq default-directory "/")
          (setq buf (harmless-ui-ensure-session-buffer session))
          (setq pbuf (harmless-ui-ensure-prompt-buffer session))
          (dolist (b (list buf pbuf))
            (should (string-prefix-p
                     (file-name-as-directory harmless-directory)
                     (buffer-local-value 'default-directory b)))
            (should-not (equal (buffer-local-value 'default-directory b) "/")))
          (setq kbuf (harmless-ui-ensure-session-buffer known))
          (setq kpbuf (harmless-ui-ensure-prompt-buffer known))
          (should (equal (buffer-local-value 'default-directory kbuf) "/proj/demo"))
          (should (equal (buffer-local-value 'default-directory kpbuf) "/proj/demo")))
      (dolist (b (list buf pbuf kbuf kpbuf))
        (when (buffer-live-p b) (kill-buffer b)))
      (should-not (file-exists-p "/summary.json")))))

(ert-deftest harmless-session-outside-stored-directory-is-recomputed ()
  (let* ((harmless-directory (make-temp-file "harmless-test-" t))
         (harmless--sessions (make-hash-table :test 'equal))
         (default-directory "/")
         (provider (harmless-test-provider))
         (session (harmless-session-new :cwd "/proj/demo"
                                        :provider provider
                                        :model "m"))
         (root (harmless-sessions-root))
         (outside (make-temp-file "harmless-outside-" t)))
    (dolist (bad (list outside root (directory-file-name root)))
      (setf (harmless-session-directory session) bad)
      (harmless-session-append-user session "recomputed")
      (harmless-test--assert-session-not-at-root session harmless-directory)
      (should (harmless-directory-strictly-under-p
               (harmless-session-directory session) root))
      (should-not (harmless-same-directory-p
                   (harmless-session-directory session) bad)))
    (should-not (file-exists-p (expand-file-name "summary.json" outside)))
    (should-not (file-exists-p (expand-file-name "summary.json" root)))))

(ert-deftest harmless-session-keeps-an-acceptable-stored-directory ()
  (let* ((harmless-directory (make-temp-file "harmless-test-" t))
         (harmless--sessions (make-hash-table :test 'equal))
         (default-directory "/")
         (provider (harmless-test-provider))
         (session (harmless-session-new :cwd "/proj/demo"
                                        :provider provider
                                        :model "m"))
         (alt (expand-file-name
               "alt-id"
               (expand-file-name "%2Fproj%2Fdemo" (harmless-sessions-root)))))
    (harmless-ensure-directory alt)
    (setf (harmless-session-directory session) alt)
    (harmless-session-append-user session "stayed put")
    (should (harmless-same-directory-p alt (harmless-session-directory session)))
    (should (string-match-p "stayed put"
                            (with-temp-buffer
                              (insert-file-contents
                               (expand-file-name "messages.jsonl" alt))
                              (buffer-string))))))

(ert-deftest harmless-session-for-cwd-ignores-trailing-slash ()
  (let* ((harmless-directory (make-temp-file "harmless-test-" t))
         (harmless--sessions (make-hash-table :test 'equal))
         (provider (harmless-test-provider))
         (session (harmless-session-new :cwd "/proj/demo/"
                                        :provider provider
                                        :model "m")))
    (should (equal (list session) (harmless-session-for-cwd "/proj/demo")))
    (should (equal (list session) (harmless-session-for-cwd "/proj/demo/")))
    (should-not (harmless-session-for-cwd "/proj/demo-evil"))
    (should-not (harmless-session-for-cwd nil))
    (should-not (harmless-session-for-cwd ""))))

(ert-deftest harmless-current-matches-directory-without-expanding-empty ()
  (let* ((harmless-directory (make-temp-file "harmless-test-" t))
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless--config-loaded t)
         (provider (harmless-test-provider))
         (harmless-providers (list provider))
         (kept (harmless-session-new :cwd "/proj/demo/"
                                     :provider provider
                                     :model "m"))
         (blank (harmless-session-new :cwd "/proj/other"
                                      :provider provider
                                      :model "m")))
    (setf (harmless-session-cwd blank) "")
    (harmless-session-save blank)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'harmless-ui-open-session) (lambda (s) s)))
      (with-temp-buffer
        (setq default-directory "/proj/demo")
        (let ((found (harmless-current)))
          (should (string= (harmless-session-id kept)
                           (harmless-session-id found)))
          (should (harmless-same-directory-p "/proj/demo"
                                             (harmless-session-cwd found)))))
      (with-temp-buffer
        (setq default-directory "/")
        (let ((found (harmless-current)))
          (should-not (string= (harmless-session-id blank)
                               (harmless-session-id found)))
          (should (equal "/" (harmless-session-cwd found)))
          (harmless-test--assert-session-not-at-root found harmless-directory))))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'harmless-ui-open-session) (lambda (s) s)))
      (with-temp-buffer
        (setq default-directory "/proj/demo/")
        (let ((created (harmless-new "")))
          (should (harmless-same-directory-p "/proj/demo"
                                             (harmless-session-cwd created)))
          (harmless-test--assert-session-not-at-root
           created harmless-directory)))
      (with-temp-buffer
        (setq default-directory "relative")
        (harmless-test-error "must be absolute" (lambda () (harmless-new "")))))
    (should-not (file-exists-p "/summary.json"))))

(ert-deftest harmless-detached-chat-has-no-project ()
  (let* ((harmless-directory (make-temp-file "harmless-chat-" t))
         (harmless--sessions (make-hash-table :test 'equal))
         (harmless--config-loaded t)
         (default-directory "/")
         (provider (harmless-test-provider))
         (harmless-providers (list provider))
         (a (harmless-session-new :detached t :provider provider :model "m"))
         (b (harmless-session-new :detached t :provider provider :model "m"))
         (root (file-name-as-directory
                (expand-file-name "_detached" (harmless-sessions-root)))))
    (should (harmless-session-detached-p a))
    (should-not (harmless-session-cwd a))
    (should (equal "chat" (harmless-session-project-name a)))
    (should (string-prefix-p root (file-name-as-directory
                                   (harmless-session-directory a))))
    (should (string-prefix-p root (file-name-as-directory
                                   (harmless-session-directory b))))
    (should-not (equal (harmless-session-directory a)
                       (harmless-session-directory b)))
    (should (string-search "%" (harmless-session-encode-cwd "/tmp/proj")))
    (should-not (equal harmless-session-detached-component
                       (harmless-session-encode-cwd "/")))
    (harmless-test--assert-session-not-at-root a harmless-directory)
    (let ((summary (harmless-json-decode
                    (with-temp-buffer
                      (insert-file-contents
                       (expand-file-name "summary.json"
                                         (harmless-session-directory a)))
                      (buffer-string)))))
      (should-not (plist-get summary :cwd))
      (should (string-match-p "\"cwd\":null"
                              (with-temp-buffer
                                (insert-file-contents
                                 (expand-file-name "summary.json"
                                                   (harmless-session-directory a)))
                                (buffer-string)))))
    (let ((dir (harmless-session-directory a))
          (id (harmless-session-id a)))
      (harmless-session-unregister a)
      (let ((loaded (harmless-session-load dir)))
        (should (equal id (harmless-session-id loaded)))
        (should (harmless-session-detached-p loaded))
        (should (equal dir (harmless-session-directory loaded)))))
    (should-not (harmless-session-for-cwd nil))
    (should-not (harmless-session-for-cwd "/"))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'harmless-ui-open-session) (lambda (s) s)))
      (with-temp-buffer
        (setq default-directory "/")
        (let ((found (harmless-current)))
          (should-not (harmless-session-detached-p found))
          (should (equal "/" (harmless-session-cwd found)))
          (harmless-test--assert-session-not-at-root found harmless-directory)))
      (let ((chat (harmless-chat)))
        (should (harmless-session-detached-p chat))
        (harmless-test--assert-session-not-at-root chat harmless-directory)))
    (harmless-test-error
     "no project"
     (lambda ()
       (harmless-session-new :detached t :cwd "/tmp"
                             :provider provider :model "m")))
    (let ((names nil))
      (unwind-protect
          (dolist (session (list a b))
            (let ((buf (harmless-ui-ensure-session-buffer session)))
              (push (buffer-name buf) names)
              (should (string-prefix-p
                       (file-name-as-directory harmless-directory)
                       (buffer-local-value 'default-directory buf)))
              (should-not (equal (buffer-local-value 'default-directory buf) "/"))))
        (dolist (session (list a b))
          (when (buffer-live-p (harmless-session-buffer session))
            (kill-buffer (harmless-session-buffer session)))))
      (should (equal 2 (length (delete-dups names)))))
    (let* ((proj (make-temp-file "harmless-proj-" t))
           (text nil))
      (with-temp-file (expand-file-name "AGENTS.md" proj)
        (insert "PROJECT-SECRET\n"))
      (let ((default-directory (file-name-as-directory proj)))
        (setq text (harmless-messages-system-text
                    (harmless-context-messages
                     nil '((:role :user :content "hi"))))))
      (should (string-match-p "detached chat" text))
      (should-not (string-match-p "PROJECT-SECRET" (or text ""))))
    (let ((names (mapcar #'harmless-tool-name
                         (harmless-tools-for-session b))))
      (should (equal '("ask_user"
                       "browser_open"
                       "computer_click"
                       "computer_key"
                       "computer_screenshot"
                       "computer_type"
                       "list_sessions"
                       "mcp_list_resources"
                       "mcp_read_resource"
                       "memory_read"
                       "memory_remember"
                       "memory_write_topic"
                       "run_workflow"
                       "schedule"
                       "schedule_cancel"
                       "send_message"
                       "session_search"
                       "spawn_agent"
                       "todo_write"
                       "web_fetch"
                       "web_search"
                       "x_keyword_search"
                       "x_thread_fetch"
                       "x_user_search")
                     (sort names #'string<)))
      (should-not (member "read_file" names))
      (should-not (member "run_shell" names)))
    (harmless-test-error
     "not attached"
     (lambda () (harmless-memory-remember "workspace" nil "Title" "Body")))
    (let ((rel (harmless-memory-remember "global" nil "Pref" "Keep this")))
      (should (string-prefix-p "observations/_inbox/" rel))
      (should (file-exists-p
               (expand-file-name rel (harmless-memory-scope-dir "global" nil)))))
    (should-not (file-directory-p
                 (expand-file-name "workspaces" (harmless-memory-root))))
    (let ((entry (harmless-dashboard--entry-from-summary
                  (harmless-session-directory b)
                  (list :id (harmless-session-id b)
                        :cwd nil
                        :title "hello"
                        :model "m"))))
      (should (equal "chat" (aref (cadr entry) 0))))
    (should-not (file-exists-p "/summary.json"))
    (should-not (file-exists-p "/AGENTS.md"))))

(defun harmless-session-search-test-error (text thunk)
  "THUNK must signal an error whose message is exactly TEXT."
  (let ((err (should-error (funcall thunk) :type 'error)))
    (should (equal text (error-message-string err)))
    err))

(defun harmless-session-search-test-guards ()
  "Return existence of paths a search must not create.
The current directory is recorded because a leading slash follows
the current drive on Windows."
  (cons default-directory
        (mapcar (lambda (path) (cons path (file-exists-p path)))
                (list (expand-file-name "harmless/sessions"
                                        user-emacs-directory)
                      "/sessions" "/messages.jsonl"
                      "c:/sessions" "c:/messages.jsonl"
                      "c:\\sessions" "c:\\messages.jsonl"
                      "C:/sessions" "C:/messages.jsonl"
                      "C:\\sessions" "C:\\messages.jsonl"))))

(defun harmless-session-search-test-guards-unchanged (before)
  "Assert BEFORE still matches the filesystem."
  (let ((default-directory (car before)))
    (dolist (cell (cdr before))
      (should (equal (cdr cell) (file-exists-p (car cell)))))))

(defun harmless-session-search-test-bytes (file)
  "Return FILE's raw bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun harmless-session-search-test-write (root proj id summary lines)
  "Write one session under ROOT and return its messages file."
  (let ((dir (expand-file-name
              id (expand-file-name
                  proj (expand-file-name "sessions" root))))
        (coding-system-for-write 'utf-8-unix))
    (make-directory dir t)
    (with-temp-file (expand-file-name "summary.json" dir)
      (insert summary))
    (with-temp-file (expand-file-name "messages.jsonl" dir)
      (insert (mapconcat #'identity lines "\n") "\n"))
    (expand-file-name "messages.jsonl" dir)))

(defun harmless-session-search-test-summary (id title updated)
  "Return one summary.json body."
  (format "{\"id\":%S,\"title\":%S,\"updated-at\":%S,\"agent-allow\":[\"only-in-summary-tool\"]}"
          id title updated))

(ert-deftest harmless-session-search-recalls-a-transcript ()
  (let* ((root (make-temp-file "harmless-search-" t))
         (harmless-directory (file-name-as-directory root))
         (harmless--sessions (make-hash-table :test 'equal))
         (guards (harmless-session-search-test-guards))
         (tools (mapcar #'harmless-tool-name harmless-tools))
         (dash (char-to-string #x2014))
         (old-file nil)
         (old-bytes nil))
    (unwind-protect
        (progn
          (setq old-file
                (harmless-session-search-test-write
                 root "proj" "old"
                 (harmless-session-search-test-summary
                  "old" "Old notes" "2020-01-01")
                 '("{\"role\":\"user\",\"content\":\"Remember visible-phrase please\"}"
                   "not json"
                   "{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"block-phrase\"}]}")))
          (harmless-session-search-test-write
           root "proj" "new"
           (harmless-session-search-test-summary
            "new" "New notes" "2020-01-03")
           '("{\"role\":\"user\",\"content\":\"VISIBLE-PHRASE again\"}"))
          (harmless-session-search-test-write
           root "proj" "title-only"
           (harmless-session-search-test-summary
            "title-only" "Title needle here" "2020-01-02")
           '("{\"role\":\"user\",\"content\":\"nothing to see\"}"))
          (harmless-session-search-test-write
           root "proj" "blank-title"
           (harmless-session-search-test-summary
            "blank-title" "   " "2019-01-01")
           '("{\"role\":\"user\",\"content\":\"blank-title-phrase\"}"))
          (harmless-session-search-test-write
           root "proj" "num-id"
           "{\"id\":7,\"title\":\"Numeric\",\"updated-at\":\"2019-06-01\",\"agent-allow\":[\"only-in-summary-tool\"]}"
           '("{\"role\":\"user\",\"content\":\"numeric-phrase\"}"))
          (harmless-session-search-test-write
           root "proj" "literal"
           (harmless-session-search-test-summary
            "literal" "Literal" "2019-07-01")
           '("{\"role\":\"user\",\"content\":\"a.b only\"}"))
          (harmless-session-search-test-write
           root "proj" "wild"
           (harmless-session-search-test-summary
            "wild" "Wild" "2019-07-02")
           '("{\"role\":\"user\",\"content\":\"axb only\"}"))
          (with-temp-file (expand-file-name
                           "notes.txt"
                           (expand-file-name
                            "proj" (expand-file-name "sessions" root)))
            (insert "visible-phrase\n"))
          (setq old-bytes (harmless-session-search-test-bytes old-file))
          (let ((recall nil))
            (cl-letf (((symbol-function 'harmless-session-load)
                       (lambda (&rest _) (error "loaded a session")))
                      ((symbol-function 'harmless-session-resume)
                       (lambda (&rest _) (error "resumed a session")))
                      ((symbol-function 'harmless-session-list-on-disk)
                       (lambda (&rest _) (error "listed sessions"))))
              (setq recall (harmless-session-search "  visible-phrase  "))
              (should (equal "" (harmless-session-search "only-in-summary-tool")))
              (should (equal "" (harmless-session-search "missing-phrase")))
              (should (equal "" (harmless-session-search nil)))
              (should (equal "" (harmless-session-search "")))
              (should (equal "" (harmless-session-search "   "))))
            (should (string-search (concat "new " dash " New notes") recall))
            (should (string-search (concat "old " dash " Old notes") recall))
            (should (< (string-search (concat "new " dash) recall)
                       (string-search (concat "old " dash) recall)))
            (should (string-search "VISIBLE-PHRASE again" recall))
            (should (string-search "Remember visible-phrase please" recall))
            (let ((block (harmless-session-search "block-phrase")))
              (should (string-search (concat "old " dash " Old notes") block))
              (should (string-search "block-phrase" block))
              (should-not (string-search "new " block)))
            (should (equal (concat "title-only " dash " Title needle here\n"
                                   "Title needle here")
                           (harmless-session-search "title needle")))
            (should (equal "blank-title\nuser blank-title-phrase"
                           (harmless-session-search "blank-title-phrase")))
            (let ((numeric (harmless-session-search "numeric-phrase")))
              (should (string-search (concat "num-id " dash " Numeric") numeric))
              (should-not (string-search (concat "\n7 " dash) (concat "\n" numeric))))
            (let ((dotted (harmless-session-search "a.b")))
              (should (string-search "literal" dotted))
              (should (string-search "a.b only" dotted))
              (should-not (string-search "wild" dotted))
              (should-not (string-search "axb" dotted))))
          (should (equal old-bytes (harmless-session-search-test-bytes old-file)))
          (should-not (harmless-session-get "new"))
          (should (equal 0 (hash-table-count harmless--sessions)))
          (should (equal tools (mapcar #'harmless-tool-name harmless-tools)))
          (should-not (harmless-tool-by-name "only-in-summary-tool"))
          (let ((tool (harmless-tool-by-name "session_search")))
            (should (eq 'read (harmless-tool-class tool)))
            (should (equal "" (harmless-tool-call tool nil '(:query "missing-phrase"))))
            (should (equal "" (harmless-tool-call tool nil nil)))
            (should (string-search
                     (concat "new " dash " New notes")
                     (harmless-tool-call tool nil '(:query "visible-phrase"))))
            (should (equal "Error: session search query must be a string"
                           (harmless-tool-call tool nil '(:query 42)))))
          (should (equal "recall" (harmless-ui--tool-verb "session_search")))
          (should (equal "visible-phrase"
                         (harmless-ui--tool-summary
                          "session_search" '(:query "visible-phrase"))))
          (harmless-session-search-test-error
           "session search query must be a string"
           (lambda () (harmless-session-search 42))))
      (delete-directory root t))
    (harmless-session-search-test-guards-unchanged guards)))

(ert-deftest harmless-session-search-caps-sessions-bytes-and-windows ()
  (let* ((root (make-temp-file "harmless-search-cap-" t))
         (harmless-directory root)
         (harmless--sessions (make-hash-table :test 'equal))
         (guards (harmless-session-search-test-guards))
         (ellipsis (char-to-string #x2026)))
    (unwind-protect
        (let ((n 1)
              (capped nil))
          (while (<= n 6)
            (harmless-session-search-test-write
             root "proj" (format "m%02d" n)
             (format "{\"id\":%S,\"updated-at\":%S}"
                     (format "m%02d" n)
                     (format "2021-01-%02d" n))
             '("{\"role\":\"user\",\"content\":\"cap-phrase\"}"))
            (setq n (1+ n)))
          (let* ((recall (harmless-session-search "cap-phrase"))
                 (ids (mapcar (lambda (block)
                                (car (split-string block "\n" t)))
                              (split-string recall "\n\n" t))))
            (should (equal '("m06" "m05" "m04" "m03" "m02") ids))
            (should-not (member "m01" ids)))
          (setq capped (expand-file-name
                        "messages.jsonl"
                        (expand-file-name
                         "capped"
                         (expand-file-name
                          "late" (expand-file-name "sessions" root)))))
          (make-directory (file-name-directory capped) t)
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file (expand-file-name "summary.json"
                                              (file-name-directory capped))
              (insert "{\"id\":\"capped\",\"title\":\"Capped\",\"updated-at\":\"2021-02-01\"}\n"))
            (with-temp-file capped
              (insert "{\"role\":\"user\",\"content\":\"early-phrase\"}\n")
              (insert (make-string 100000 ?x))
              (insert "late-phrase")))
          (should (string-search "early-phrase"
                                (harmless-session-search "early-phrase")))
          (should (equal "" (harmless-session-search "late-phrase")))
          (should (string-search "late-phrase"
                                (harmless-session-search-test-bytes capped)))
          (let ((dir (expand-file-name
                      "dirmsg"
                      (expand-file-name
                       "proj" (expand-file-name "sessions" root)))))
            (make-directory (expand-file-name "messages.jsonl" dir) t)
            (let ((coding-system-for-write 'utf-8-unix))
              (with-temp-file (expand-file-name "summary.json" dir)
                (insert "{\"id\":\"dirmsg\",\"title\":\"Dir\",\"updated-at\":\"2021-03-01\"}\n"))
              (with-temp-file (expand-file-name
                               "hidden.txt"
                               (expand-file-name "messages.jsonl" dir))
                (insert "dir-phrase\n"))))
          (should (equal "" (harmless-session-search "dir-phrase")))
          (harmless-session-search-test-write
           root "proj" "long"
           "{\"id\":\"long\",\"title\":\"Long\",\"updated-at\":\"2021-04-01\"}"
           (list (format "{\"role\":\"user\",\"content\":%S}"
                         (concat (make-string 50 ?a)
                                 "mark-phrase"
                                 (make-string 200 ?b)))))
          (let* ((recall (harmless-session-search "mark-phrase"))
                 (snippet (cadr (split-string recall "\n" t))))
            (should (string-search "mark-phrase" snippet))
            (should (string-prefix-p ellipsis snippet))
            (should (string-suffix-p ellipsis snippet))
            (should (= 162 (length snippet)))))
      (delete-directory root t))
    (harmless-session-search-test-guards-unchanged guards)))

(ert-deftest harmless-session-search-skips-a-symlink-escape ()
  (let* ((root (make-temp-file "harmless-search-link-" t))
         (outside (make-temp-file "harmless-search-out-" t))
         (harmless-directory root)
         (harmless--sessions (make-hash-table :test 'equal))
         (guards (harmless-session-search-test-guards))
         (link (expand-file-name
                "escaped"
                (expand-file-name
                 "proj" (expand-file-name "sessions" root)))))
    (unwind-protect
        (progn
          (make-directory (file-name-directory link) t)
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file (expand-file-name "summary.json" outside)
              (insert "{\"id\":\"secret\",\"title\":\"Secret\",\"updated-at\":\"2024-01-01\"}\n"))
            (with-temp-file (expand-file-name "messages.jsonl" outside)
              (insert "{\"role\":\"user\",\"content\":\"secret-cursor-search\"}\n")))
          (condition-case nil
              (make-symbolic-link (directory-file-name outside) link)
            (file-error nil))
          (when (file-symlink-p link)
            (should (equal "" (harmless-session-search "secret-cursor-search")))
            (delete-file link)))
      (delete-directory outside t)
      (delete-directory root t))
    (harmless-session-search-test-guards-unchanged guards)))

(ert-deftest harmless-session-search-refuses-a-bad-data-directory ()
  (let ((guards (harmless-session-search-test-guards))
        (default-directory
         (file-name-as-directory (make-temp-file "harmless-search-rel-" t))))
    (unwind-protect
        (dolist (blank '("" "   " "\t\n"))
          (let ((harmless-directory blank))
            (cl-letf (((symbol-function 'harmless-sessions-root)
                       (lambda () (error "scanned the sessions root"))))
              (harmless-session-search-test-error
               "session search path is empty"
               (lambda () (harmless-session-search "x"))))))
      (dolist (root '("/" "c:/" "c:\\" "C:/" "\\" "c:" "//" "/." "D:/" "d:\\"))
        (let ((harmless-directory root))
          (cl-letf (((symbol-function 'harmless-sessions-root)
                     (lambda () (error "scanned the sessions root"))))
            (harmless-session-search-test-error
             "session search path is the filesystem root"
             (lambda () (harmless-session-search "x"))))))
      (let ((harmless-directory "/"))
        (cl-letf (((symbol-function 'harmless-sessions-root)
                   (lambda () (error "scanned the sessions root"))))
          (should (equal "" (harmless-session-search nil)))
          (should (equal "" (harmless-session-search "")))
          (should (equal "" (harmless-session-search "   ")))
          (harmless-session-search-test-error
           "session search query must be a string"
           (lambda () (harmless-session-search 42)))))
      (dolist (rel '("notes" "notes/"))
        (let ((harmless-directory rel))
          (harmless-session-search-test-error
           (format "Harmless data directory must be absolute, not %S" rel)
           (lambda () (harmless-session-search "x")))
          (should-not (file-exists-p (expand-file-name rel default-directory)))
          (should-not (file-directory-p
                       (expand-file-name "notes" default-directory)))))
      (let ((dir default-directory)
            (default-directory temporary-file-directory))
        (delete-directory dir t)))
    (harmless-session-search-test-guards-unchanged guards)))

(ert-deftest harmless-session-search-nil-directory-uses-the-data-directory ()
  (let* ((fallback (make-temp-file "harmless-search-nil-" t))
         (harmless-directory nil)
         (harmless--sessions (make-hash-table :test 'equal))
         (guards (harmless-session-search-test-guards))
         (file nil))
    (unwind-protect
        (cl-letf (((symbol-function 'locate-user-emacs-file)
                   (lambda (&rest _) fallback)))
          (setq file
                (harmless-session-search-test-write
                 fallback "proj" "nil-dir"
                 "{\"id\":\"nil-dir\",\"title\":\"Nil dir\",\"updated-at\":\"2022-01-01\"}"
                 '("{\"role\":\"user\",\"content\":\"from-nil-dir\"}")))
          (let ((recall (harmless-session-search "from-nil-dir")))
            (should (string-search "nil-dir" recall))
            (should (string-search "Nil dir" recall)))
          (should (file-exists-p file))
          (should (string-prefix-p
                   (file-name-as-directory (file-truename fallback))
                   (file-truename file)))
          (should (equal "" (harmless-session-search "missing-from-nil"))))
      (when (file-directory-p fallback)
        (delete-directory fallback t)))
    (harmless-session-search-test-guards-unchanged guards)))

(provide 'harmless-session-tests)
