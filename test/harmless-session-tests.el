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
    (should (equal "?" (harmless-session-project-name
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
         (provider (harmless-test-provider)))
    (cl-letf (((symbol-function 'locate-user-emacs-file)
               (lambda (&rest _) fallback)))
      (let ((session (harmless-session-new :cwd "/proj/demo"
                                           :provider provider
                                           :model "m")))
        (harmless-test--assert-session-not-at-root session fallback)
        (should (string-prefix-p (file-name-as-directory fallback)
                                 (harmless-sessions-root)))
        (should (harmless-session-resume (harmless-session-id session)))))
    (should-not (file-directory-p "/sessions"))))

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
  (let* ((session (harmless-session--create
                   :id "abcdef0123456789abcdef0123456789"
                   :cwd nil))
         (known (harmless-session--create
                 :id "abcdef0123456789abcdef0123456789"
                 :cwd "/proj/demo"))
         inherited buf pbuf kbuf kpbuf)
    (unwind-protect
        (with-temp-buffer
          (setq default-directory "/tmp/")
          (setq inherited default-directory)
          (setq buf (harmless-ui-ensure-session-buffer session))
          (setq pbuf (harmless-ui-ensure-prompt-buffer session))
          (should (equal (buffer-local-value 'default-directory buf) inherited))
          (should (equal (buffer-local-value 'default-directory pbuf) inherited))
          (should-not (equal (buffer-local-value 'default-directory buf) "/"))
          (setq kbuf (harmless-ui-ensure-session-buffer known))
          (setq kpbuf (harmless-ui-ensure-prompt-buffer known))
          (should (equal (buffer-local-value 'default-directory kbuf) "/proj/demo"))
          (should (equal (buffer-local-value 'default-directory kpbuf) "/proj/demo")))
      (dolist (b (list buf pbuf kbuf kpbuf))
        (when (buffer-live-p b) (kill-buffer b))))))

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

(provide 'harmless-session-tests)
