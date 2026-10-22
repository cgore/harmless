;;; harmless-x-tests.el --- Tests for Harmless X search -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless-provider)
(require 'harmless-session)
(require 'harmless-tools)
(require 'harmless-perm)
(require 'harmless-turn)
(require 'harmless-agent)
(require 'harmless-x)
(require 'harmless-ui)

(cl-defstruct (harmless-x-fake
               (:include harmless-provider)
               (:constructor harmless-x-make-fake))
  script)

(cl-defmethod harmless-provider-complete ((provider harmless-x-fake)
                                          _messages _tools callback)
  (when (harmless-x-fake-script provider)
    (funcall (harmless-x-fake-script provider) _messages callback))
  nil)

(defmacro harmless-x-test-with (data &rest body)
  "Bind a temp data directory DATA, then run BODY."
  (declare (indent 1))
  `(let* ((,data (make-temp-file "harmless-x-data-" t))
          (harmless-directory ,data)
          (harmless--sessions (make-hash-table :test 'equal))
          (harmless-x-search-function
           (lambda (_query _mode _from _to)
             (list (list :id "1" :username "ada" :name "Ada"
                         :created "2026-01-02T03:04:05.000Z"
                         :text "hello"))))
          (harmless-x-thread-function
           (lambda (_id)
             (list (list :id "1" :username "ada" :name "Ada"
                         :created "2026-01-02T03:04:05.000Z"
                         :text "hello"))))
          (harmless-x-user-function
           (lambda (_query)
             (list (list :id "9" :username "ada" :name "Ada"
                         :description "lisp"))))
          (harmless-perm-ask-function
           (lambda (_session _class _item callback)
             (funcall callback 'allow))))
     ,@body))

(defun harmless-x-test-session (&rest args)
  "Return a session.  ARGS are passed to `harmless-session-new'."
  (apply #'harmless-session-new
         :provider (harmless-x-make-fake :name "fake" :host "none")
         :model "m"
         args))

(defun harmless-x-test-call (name session args)
  "Return the NAME tool result for ARGS on SESSION."
  (harmless-tool-call (harmless-tool-by-name name) session args))

(defun harmless-x-test-not-root (session)
  "Require SESSION's directory to stay off the filesystem root."
  (let ((dir (harmless-session-directory session)))
    (should (file-name-absolute-p dir))
    (should-not (harmless-filesystem-root-p dir))
    (should (harmless-directory-strictly-under-p dir harmless-directory))
    (should-not (equal (file-truename dir)
                       (file-truename (expand-file-name "/"))))))

(defun harmless-x-test-raw-empty (session)
  "Return SESSION's messages.jsonl contents."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "messages.jsonl"
                       (harmless-session-directory session)))
    (buffer-string)))

(defconst harmless-x-test-post
  (concat "1. @ada (Ada)\n"
          "2026-01-02T03:04:05.000Z\n"
          "https://x.com/ada/status/1\n"
          "hello")
  "Formatted sample post.")

(defconst harmless-x-test-user
  (concat "1. Ada (@ada)\n"
          "https://x.com/ada\n"
          "lisp")
  "Formatted sample account.")

(ert-deftest harmless-x-refuses-a-bad-query ()
  (harmless-x-test-with _data
    (let* ((repo (make-temp-file "harmless-x-repo-" t))
           (session (harmless-x-test-session
                     :cwd repo :id "s" :title "Keep"))
           (called nil)
           (harmless-x-search-function
            (lambda (&rest _) (setq called t) nil))
           (harmless-x-user-function
            (lambda (&rest _) (setq called t) nil)))
      (dolist (name '("harmless-tool-x-keyword-search" "harmless-tool-x-user-search"))
        (dolist (query (list nil :false 42))
          (should (equal (harmless-x-test-call name session
                                               (list :query query))
                         "Error: query must be a string")))
        (dolist (query (list "" "  " "\n"))
          (should (equal (harmless-x-test-call name session
                                               (list :query query))
                         "Error: query is empty"))))
      (should-not called)
      (should (eq (harmless-session-status session) 'idle))
      (should (equal (harmless-session-title session) "Keep"))
      (should (equal (harmless-x-test-raw-empty session) ""))
      (harmless-x-test-not-root session))))

(ert-deftest harmless-x-refuses-a-bad-post-id ()
  (harmless-x-test-with _data
    (let* ((repo (make-temp-file "harmless-x-repo-" t))
           (session (harmless-x-test-session :cwd repo :id "s"))
           (called nil)
           (harmless-x-thread-function
            (lambda (id) (setq called id) nil))
           (harmless-web-retrieve-function
            (lambda (_url) (error "network was called"))))
      (dolist (id (list nil "" "  " "\n"))
        (should (equal (harmless-x-test-call "harmless-tool-x-thread-fetch" session
                                             (list :post_id id))
                       "Error: post id is empty")))
      (dolist (id (list :false 42))
        (should (equal (harmless-x-test-call "harmless-tool-x-thread-fetch" session
                                             (list :post_id id))
                       "Error: post id must be a string")))
      (dolist (id (list "sub" "sub/" "/" "c:/" "../x" "12a" "1.2"))
        (should (equal (harmless-x-test-call "harmless-tool-x-thread-fetch" session
                                             (list :post_id id))
                       "Error: post id is not an id")))
      (should-not called)
      (should (equal (harmless-x-test-call "harmless-tool-x-thread-fetch" session
                                           (list :post_id "  123  "))
                     "No posts"))
      (should (equal called "123"))
      (harmless-x-test-not-root session))))

(ert-deftest harmless-x-refuses-bad-options ()
  (harmless-x-test-with _data
    (let* ((repo (make-temp-file "harmless-x-repo-" t))
           (session (harmless-x-test-session :cwd repo :id "s"))
           (called nil)
           (harmless-x-search-function
            (lambda (query mode from to)
              (setq called (list query mode from to))
              nil)))
      (dolist (mode (list "" "  " :false 42 "sideways"))
        (should (equal (harmless-x-test-call
                        "harmless-tool-x-keyword-search" session
                        (list :query "emacs" :mode mode))
                       "Error: mode must be Top or Latest")))
      (dolist (limit (list 1.5 "8" :false))
        (should (equal (harmless-x-test-call
                        "harmless-tool-x-keyword-search" session
                        (list :query "emacs" :limit limit))
                       "Error: limit must be a positive integer")))
      (dolist (limit (list 0 -1 11))
        (should (equal (harmless-x-test-call
                        "harmless-tool-x-keyword-search" session
                        (list :query "emacs" :limit limit))
                       "Error: limit must be from 1 to 10")))
      (dolist (date (list :false 42 "" "  " "c:/" "/" "2026-1-02"
                          "20260102"))
        (should (equal (harmless-x-test-call
                        "harmless-tool-x-keyword-search" session
                        (list :query "emacs" :from_date date))
                       "Error: date must be YYYY-MM-DD")))
      (should (equal (harmless-x-test-call
                      "harmless-tool-x-keyword-search" session
                      (list :query "emacs"
                            :from_date "2026-02-01"
                            :to_date "2026-01-01"))
                     "Error: from_date is after to_date"))
      (should-not called)
      (should (equal (harmless-x-test-call
                      "harmless-tool-x-keyword-search" session
                      (list :query "  c:/  " :mode " top "
                            :from_date "  2026-01-02  "
                            :to_date "2026-01-03"))
                     "No posts"))
      (should (equal called '("c:/" "Top" "2026-01-02" "2026-01-03")))
      (setq called nil)
      (should (equal (harmless-x-test-call
                      "harmless-tool-x-keyword-search" session
                      (list :query "emacs"))
                     "No posts"))
      (should (equal called '("emacs" "Latest" nil nil)))
      (harmless-x-test-not-root session))))

(defun harmless-x-test-post-json (n)
  "Return a JSON array body of N posts."
  (let ((i 0)
        (out ""))
    (while (< i n)
      (setq i (1+ i))
      (setq out
            (concat out
                    (format "{\"id\":\"%d\",\"text\":\"p%d\",\"author_id\":\"9\",\"created_at\":\"2026-01-%02dT00:00:00.000Z\"},"
                            i i i))))
    (concat "{\"data\":[" (substring out 0 -1)
            "],\"includes\":{\"users\":[{\"id\":\"9\","
            "\"name\":\"Ada\",\"username\":\"ada\"}]}}")))

(ert-deftest harmless-x-parses-results ()
  (let* ((one (concat "{\"data\":[{\"id\":\"1\",\"text\":\" hello \","
                      "\"author_id\":\"9\","
                      "\"created_at\":\"2026-01-02T03:04:05.000Z\"}],"
                      "\"includes\":{\"users\":[{\"id\":\"9\","
                      "\"name\":\" Ada \",\"username\":\" ada \"}]}}"))
         (parsed (harmless-x-parse-posts one))
         (users (harmless-x-parse-users
                 (concat "{\"data\":[{\"id\":\"9\",\"name\":\"Ada\","
                         "\"username\":\"ada\",\"description\":\" lisp \""
                         "},{\"id\":\"8\",\"name\":\"Skip\"}]}")))
         (lookup (concat "{\"data\":{\"id\":\"10\",\"text\":\"p10\","
                         "\"author_id\":\"9\","
                         "\"created_at\":\"2026-01-10T00:00:00.000Z\","
                         "\"conversation_id\":\"99\"},"
                         "\"includes\":{\"users\":[{\"id\":\"9\","
                         "\"name\":\"Ada\",\"username\":\"ada\"}]}}"))
         (search (harmless-x-test-post-json 10))
         (thread (harmless-x-parse-thread lookup search "10"))
         (early (harmless-x-parse-thread
                 (concat "{\"data\":{\"id\":\"1\",\"text\":\"p1\","
                         "\"author_id\":\"9\","
                         "\"created_at\":\"2026-01-01T00:00:00.000Z\"}}")
                 search
                 "1")))
    (should (equal (car parsed)
                   (list :id "1" :username "ada" :name "Ada"
                         :created "2026-01-02T03:04:05.000Z"
                         :text "hello")))
    (should (equal (harmless-x-parse-posts "{}") nil))
    (should (equal (condition-case err
                       (harmless-x-parse-posts nil)
                     (error (error-message-string err)))
                   "X search failed"))
    (let ((failed (condition-case err
                      (harmless-x-parse-posts "{")
                    (error (error-message-string err)))))
      (should (member failed
                      '("X search failed: End of file while parsing JSON: 1, nil, 1"
                        "X search failed: End of file while parsing JSON: 1, 1, 1"))))
    (should (equal (car users)
                   (list :id "9" :username "ada" :name "Ada"
                         :description "lisp")))
    (should (= (length users) 1))
    (should (equal (harmless-x-parse-users "{}") nil))
    (should (= (length thread) 8))
    (should (equal (plist-get (car thread) :id) "3"))
    (should (equal (plist-get (car (last thread)) :id) "10"))
    (should (equal (mapcar (lambda (post) (plist-get post :id)) early)
                   '("1" "2" "3" "4" "5" "6" "7" "8")))
    (should (string-search "query=c%3A%2F"
                           (harmless-x-search-url "c:/" "Latest" nil nil)))
    (should (string-search "query=from%3Aada"
                           (harmless-x-search-url "from:ada" "Latest"
                                                  nil nil)))
    (should (string-search "sort_order=relevancy"
                           (harmless-x-search-url "q" "Top" nil nil)))
    (should (string-search "sort_order=recency"
                           (harmless-x-search-url "q" "Latest" nil nil)))
    (should (string-search "max_results=10"
                           (harmless-x-search-url "q" "Latest" nil nil)))
    (should (string-search "start_time=2026-01-02T00%3A00%3A00Z"
                           (harmless-x-search-url "q" "Latest"
                                                  "2026-01-02" nil)))
    (should (string-search "end_time=2026-01-03T00%3A00%3A00Z"
                           (harmless-x-search-url "q" "Latest" nil
                                                  "2026-01-03")))
    (should (equal (harmless-x-user-url "c:/")
                   (concat "https://api.x.com/2/users/search?query=c%3A%2F"
                           "&user.fields=description,name,username")))
    (should (string-prefix-p "https://api.x.com/2/tweets/123?"
                             (harmless-x-tweet-url "123")))
    (let* ((seen nil)
           (harmless-x-bearer " tok ")
           (harmless-web-retrieve-function
            (lambda (url)
              (setq seen (list url url-request-extra-headers))
              one)))
      (should (equal (car (harmless-x-search-default "c:/" "Top"
                                                     "2026-01-02"
                                                     "2026-01-03"))
                     (list :id "1" :username "ada" :name "Ada"
                           :created "2026-01-02T03:04:05.000Z"
                           :text "hello")))
      (should (equal (car seen)
                     (harmless-x-search-url "c:/" "Top"
                                            "2026-01-02" "2026-01-03")))
      (should (equal (cadr seen) '(("Authorization" . "Bearer tok")))))
    (let* ((seen nil)
           (harmless-x-bearer nil)
           (process-environment
            (cons "X_BEARER_TOKEN=from-env"
                  (cl-remove-if
                   (lambda (item)
                     (string-prefix-p "X_BEARER_TOKEN=" item))
                   process-environment)))
           (harmless-web-retrieve-function
            (lambda (url)
              (setq seen url-request-extra-headers)
              one)))
      (cl-letf (((symbol-function 'harmless-auth--auth-source)
                 (lambda (_host) "from-auth")))
        (should (equal (plist-get (car (harmless-x-parse-posts
                                        (harmless-x-get
                                         "https://api.x.com/2/users/search?query=a")))
                                  :id)
                       "1"))
        (should (equal seen '(("Authorization" . "Bearer from-env"))))))
    (let* ((host nil)
           (headers nil)
           (harmless-x-bearer nil)
           (process-environment
            (cl-remove-if
             (lambda (item)
               (string-prefix-p "X_BEARER_TOKEN=" item))
             process-environment))
           (harmless-web-retrieve-function
            (lambda (_url)
              (setq headers url-request-extra-headers)
              "{}")))
      (cl-letf (((symbol-function 'harmless-auth--auth-source)
                 (lambda (name)
                   (setq host name)
                   " from-auth ")))
        (should (equal (harmless-x-get "https://api.x.com/x") "{}"))
        (should (equal host "api.x.com"))
        (should (equal headers
                       '(("Authorization" . "Bearer from-auth")))))
      (setq harmless-x-bearer "   ")
      (setq headers 'not-called)
      (cl-letf (((symbol-function 'harmless-auth--auth-source)
                 (lambda (_host) nil)))
        (should (equal (condition-case err
                           (harmless-x-get "https://api.x.com/x")
                         (error (error-message-string err)))
                       "X is not signed in"))
        (should (eq headers 'not-called))))))

(ert-deftest harmless-x-thread-default-requests-the-conversation ()
  (let* ((seen nil)
         (harmless-x-bearer "tok")
         (lookup (concat "{\"data\":{\"id\":\"55\",\"text\":\"root\","
                         "\"author_id\":\"9\","
                         "\"created_at\":\"2026-01-01T00:00:00.000Z\","
                         "\"conversation_id\":\"99\"},"
                         "\"includes\":{\"users\":[{\"id\":\"9\","
                         "\"username\":\"ada\",\"name\":\"Ada\"}]}}"))
         (reply (concat "{\"data\":[{\"id\":\"56\",\"text\":\"reply\","
                        "\"author_id\":\"9\","
                        "\"created_at\":\"2026-01-02T00:00:00.000Z\"}]}"))
         (harmless-web-retrieve-function
          (lambda (url)
            (push url seen)
            (if (string-search "/2/tweets/55?" url)
                lookup
              reply))))
    (should (equal (mapcar (lambda (post) (plist-get post :id))
                           (harmless-x-thread-default "55"))
                   '("55" "56")))
    (should (equal (car (last seen)) (harmless-x-tweet-url "55")))
    (should (equal (car seen)
                   (harmless-x-search-url "conversation_id:99"
                                          "Latest" nil nil))))
  (let* ((seen nil)
         (harmless-x-bearer "tok")
         (lookup (concat "{\"data\":{\"id\":\"55\",\"text\":\"root\","
                         "\"author_id\":\"9\","
                         "\"created_at\":\"2026-01-01T00:00:00.000Z\"}}"))
         (harmless-web-retrieve-function
          (lambda (url)
            (push url seen)
            (if (string-search "/2/tweets/search/" url)
                "{\"data\":[]}"
              lookup))))
    (harmless-x-thread-default "55")
    (should (equal (car seen)
                   (harmless-x-search-url "conversation_id:55"
                                          "Latest" nil nil))))
  (let* ((calls 0)
         (harmless-x-bearer "tok")
         (harmless-web-retrieve-function
          (lambda (_url)
            (setq calls (1+ calls))
            "{")))
    (let ((failed (condition-case err
                      (harmless-x-thread-default "55")
                    (error (error-message-string err)))))
      (should (member failed
                      '("X search failed: End of file while parsing JSON: 1, nil, 1"
                        "X search failed: End of file while parsing JSON: 1, 1, 1"))))
    (should (= calls 1))))

(ert-deftest harmless-x-formats-results ()
  (harmless-x-test-with _data
    (let* ((repo (make-temp-file "harmless-x-repo-" t))
           (readme (expand-file-name "README" repo))
           (calls 0)
           (seen nil)
           (session (harmless-x-test-session
                     :cwd repo :id "s" :title "Keep"
                     :provider
                     (harmless-x-make-fake
                      :name "fake" :host "none"
                      :script (lambda (&rest _) (setq calls (1+ calls))))))
           (files nil)
           (readme-text nil)
           (stored nil)
           (harmless-x-search-function
            (lambda (query mode from to)
              (setq seen (list query mode from to))
              (list (list :id "1" :username "ada" :name "Ada"
                          :created "2026-01-02T03:04:05.000Z"
                          :text "hello")))))
      (with-temp-file readme (insert "keep\n"))
      (setq files (sort (directory-files repo t) #'string<))
      (setq readme-text (with-temp-buffer
                          (insert-file-contents readme)
                          (buffer-string)))
      (setq stored (sort (directory-files
                          (harmless-session-directory session) t)
                         #'string<))
      (should (equal (harmless-x-test-call
                      "harmless-tool-x-keyword-search" session
                      (list :query "  emacs  "))
                     harmless-x-test-post))
      (should (equal seen '("emacs" "Latest" nil nil)))
      (should (= calls 0))
      (should (eq (harmless-session-status session) 'idle))
      (should (equal (harmless-session-title session) "Keep"))
      (should (equal (harmless-x-test-raw-empty session) ""))
      (let ((harmless-x-search-function (lambda (&rest _) nil)))
        (should (equal (harmless-x-test-call
                        "harmless-tool-x-keyword-search" session '(:query "none"))
                       "No posts")))
      (let ((harmless-x-search-function (lambda (&rest _) "nope")))
        (should (equal (harmless-x-test-call
                        "harmless-tool-x-keyword-search" session '(:query "bad"))
                       "Error: X search failed")))
      (let ((harmless-x-search-function
             (lambda (&rest _)
               (let ((i 0) (out nil))
                 (while (< i 9)
                   (setq i (1+ i))
                   (push (list :id (number-to-string i)
                               :username "ada" :name "Ada"
                               :text (format "p%d" i))
                         out))
                 (nreverse out)))))
        (let ((text (harmless-x-test-call
                     "harmless-tool-x-keyword-search" session '(:query "many"))))
          (should (string-search "p8" text))
          (should-not (string-search "p9" text))))
      (let ((harmless-x-search-function
             (lambda (&rest _)
               (list (list :id "1" :username "ada" :name "Ada"
                           :text "only")))))
        (should (equal (harmless-x-test-call
                        "harmless-tool-x-keyword-search" session
                        (list :query "emacs" :limit 1))
                       (concat "1. @ada (Ada)\n"
                               "https://x.com/ada/status/1\n"
                               "only"))))
      (let ((harmless-x-user-function (lambda (_query) nil)))
        (should (equal (harmless-x-test-call
                        "harmless-tool-x-user-search" session '(:query "none"))
                       "No users")))
      (should (equal (harmless-x-test-call
                      "harmless-tool-x-user-search" session
                      (list "query" "ada"))
                     harmless-x-test-user))
      (let ((harmless-x-thread-function
             (lambda (_id)
               (list (list :id "7" :text "bare")))))
        (should (equal (harmless-x-test-call
                        "harmless-tool-x-thread-fetch" session '(:post_id "7"))
                       (concat "1. unknown\n"
                               "https://x.com/i/status/7\n"
                               "bare"))))
      (should (equal (harmless-ui--tool-verb "harmless-tool-x-keyword-search") "posts"))
      (should (equal (harmless-ui--tool-verb "harmless-tool-x-thread-fetch") "thread"))
      (should (equal (harmless-ui--tool-verb "harmless-tool-x-user-search") "users"))
      (should (equal (harmless-ui--tool-summary
                      "harmless-tool-x-keyword-search" '(:query "emacs"))
                     "emacs"))
      (should (equal (harmless-ui--tool-summary
                      "harmless-tool-x-thread-fetch" '(:post_id "123"))
                     "123"))
      (should (equal (sort (directory-files repo t) #'string<) files))
      (should (equal (with-temp-buffer
                       (insert-file-contents readme)
                       (buffer-string))
                     readme-text))
      (should (equal (sort (directory-files
                            (harmless-session-directory session) t)
                           #'string<)
                     stored))
      (dolist (name '("harmless-tool-x-keyword-search" "harmless-tool-x-thread-fetch" "harmless-tool-x-user-search"))
        (should (eq (harmless-tool-class (harmless-tool-by-name name))
                    'network))
        (should-not (harmless-tool-async (harmless-tool-by-name name))))
      (harmless-x-test-not-root session)
      (let ((detached (harmless-x-test-session :detached t :id "chat")))
        (let ((names (mapcar #'harmless-tool-name
                             (harmless-tools-for-session detached))))
          (should (member "harmless-tool-x-keyword-search" names))
          (should (member "harmless-tool-x-thread-fetch" names))
          (should (member "harmless-tool-x-user-search" names)))
        (should (equal (harmless-x-test-call
                        "harmless-tool-x-keyword-search" detached '(:query "emacs"))
                       harmless-x-test-post))
        (harmless-x-test-not-root detached)
        (should (string-search "/_detached/"
                               (harmless-session-directory detached))))
      (let ((harmless-x-bearer nil)
            (harmless-x-search-function #'harmless-x-search-default)
            (process-environment
             (cl-remove-if
              (lambda (item)
                (string-prefix-p "X_BEARER_TOKEN=" item))
              process-environment))
            (harmless-web-retrieve-function
             (lambda (_url) (error "network was called"))))
        (cl-letf (((symbol-function 'harmless-auth--auth-source)
                   (lambda (_host) nil)))
          (should (equal (harmless-x-test-call
                          "harmless-tool-x-keyword-search" session '(:query "emacs"))
                         "Error: X is not signed in")))))))

(ert-deftest harmless-x-permission-and-plan-mode ()
  (harmless-x-test-with _data
    (let* ((repo (make-temp-file "harmless-x-repo-" t))
           (calls 0)
           (searches nil)
           (threads nil)
           (asked nil)
           (harmless-x-search-function
            (lambda (query &rest _)
              (push query searches)
              (list (list :id "1" :username "ada" :name "Ada"
                          :created "2026-01-02T03:04:05.000Z"
                          :text "hello"))))
           (harmless-x-thread-function
            (lambda (id)
              (push id threads)
              (list (list :id id :username "ada" :name "Ada"
                          :text "hello"))))
           (session
            (harmless-session-new
             :cwd repo :id "s" :title "Keep" :model "m"
             :provider
             (harmless-x-make-fake
              :name "fake" :host "none"
              :script
              (lambda (_messages callback)
                (setq calls (1+ calls))
                (if (= calls 1)
                    (progn
                      (funcall callback
                               '(:tool-call "1" "harmless-tool-x-keyword-search"
                                 "{\"query\":\"emacs\"}"))
                      (funcall callback '(:stop "tool_calls")))
                  (funcall callback '(:text "done"))
                  (funcall callback '(:stop "stop"))))))))
      (setq harmless-perm-ask-function
            (lambda (_session class item callback)
              (push (list class item) asked)
              (funcall callback 'deny)))
      (harmless-turn-run session "go")
      (should (equal asked '((network "emacs"))))
      (should-not searches)
      (should (equal (cl-loop for msg in (harmless-session-messages session)
                              when (eq (plist-get msg :role) :tool)
                              collect (plist-get msg :content))
                     '("Error: user denied this tool call")))
      (setq calls 0)
      (setq asked nil)
      (setq session
            (harmless-session-new
             :cwd repo :id "s2" :title "Keep" :model "m"
             :permission-mode 'accept-edits
             :provider
             (harmless-x-make-fake
              :name "fake" :host "none"
              :script
              (lambda (_messages callback)
                (setq calls (1+ calls))
                (if (= calls 1)
                    (progn
                      (funcall callback
                               '(:tool-call "1" "harmless-tool-x-keyword-search"
                                 "{\"query\":\"emacs\"}"))
                      (funcall callback '(:stop "tool_calls")))
                  (funcall callback '(:text "done"))
                  (funcall callback '(:stop "stop")))))))
      (setf (harmless-session-plan-mode session) t)
      (setq harmless-perm-ask-function
            (lambda (_session class item callback)
              (push (list class item) asked)
              (funcall callback 'allow)))
      (harmless-turn-run session "go")
      (should (equal asked '((network "emacs"))))
      (should (equal searches '("emacs")))
      (should (equal (cl-loop for msg in (harmless-session-messages session)
                              when (eq (plist-get msg :role) :tool)
                              collect (plist-get msg :content))
                     (list harmless-x-test-post)))
      (should (harmless-session-plan-mode session))
      (setq calls 0)
      (setq asked nil)
      (setq searches nil)
      (setq session
            (harmless-session-new
             :cwd repo :id "s3" :title "Keep" :model "m"
             :permission-mode 'always-approve
             :provider
             (harmless-x-make-fake
              :name "fake" :host "none"
              :script
              (lambda (_messages callback)
                (setq calls (1+ calls))
                (if (= calls 1)
                    (progn
                      (funcall callback
                               '(:tool-call "1" "harmless-tool-x-thread-fetch"
                                 "{\"post_id\":\"123\"}"))
                      (funcall callback '(:stop "tool_calls")))
                  (funcall callback '(:text "done"))
                  (funcall callback '(:stop "stop")))))))
      (setq harmless-perm-ask-function
            (lambda (&rest _) (error "permission asked")))
      (harmless-turn-run session "go")
      (should-not asked)
      (should (equal threads '("123")))
      (setq calls 0)
      (setq asked nil)
      (setq threads nil)
      (setq session
            (harmless-session-new
             :cwd repo :id "s4" :title "Keep" :model "m"
             :provider
             (harmless-x-make-fake
              :name "fake" :host "none"
              :script
              (lambda (_messages callback)
                (setq calls (1+ calls))
                (if (= calls 1)
                    (progn
                      (funcall callback
                               '(:tool-call "1" "harmless-tool-x-thread-fetch"
                                 "{\"post_id\":\"123\"}"))
                      (funcall callback '(:stop "tool_calls")))
                  (funcall callback '(:text "done"))
                  (funcall callback '(:stop "stop")))))))
      (setq harmless-perm-ask-function
            (lambda (_session class item callback)
              (push (list class item) asked)
              (funcall callback 'deny)))
      (harmless-turn-run session "go")
      (should (equal asked '((network "123"))))
      (should-not threads)
      (harmless-x-test-not-root session))))

(ert-deftest harmless-x-read-only-may-search ()
  (harmless-x-test-with _data
    (let* ((repo (make-temp-file "harmless-x-repo-" t))
           (child (harmless-x-test-session
                   :cwd repo :id "child" :source 'agent :parent-id "p")))
      (setf (harmless-session-agent child)
            (list :allow 'all :sandbox 'read-only :callback nil
                  :turns 0 :max 8 :error nil :prompt nil :task nil))
      (let ((names (mapcar #'harmless-tool-name
                           (harmless-agent-filter-tools
                            child (harmless-tools-enabled)))))
        (dolist (name '("harmless-tool-x-keyword-search" "harmless-tool-x-thread-fetch"
                        "harmless-tool-x-user-search"))
          (should (member name names)))
        (should-not (member "harmless-tool-write-file" names)))
      (dolist (pair '(("harmless-tool-x-keyword-search" (:query "emacs"))
                      ("harmless-tool-x-thread-fetch" (:post_id "123"))
                      ("harmless-tool-x-user-search" (:query "ada"))))
        (should-not (harmless-agent-refuse-call
                     child (harmless-tool-by-name (car pair))
                     (cadr pair))))
      (should (equal (harmless-x-test-call
                      "harmless-tool-x-keyword-search" child '(:query "emacs"))
                     harmless-x-test-post))
      (setf (plist-get (harmless-session-agent child) :allow)
            '("harmless-tool-read-file"))
      (dolist (pair '(("harmless-tool-x-keyword-search" (:query "emacs")
                       "Error: this subagent cannot use harmless-tool-x-keyword-search")
                      ("harmless-tool-x-thread-fetch" (:post_id "123")
                       "Error: this subagent cannot use harmless-tool-x-thread-fetch")
                      ("harmless-tool-x-user-search" (:query "ada")
                       "Error: this subagent cannot use harmless-tool-x-user-search")))
        (should (equal (harmless-agent-refuse-call
                        child (harmless-tool-by-name (car pair))
                        (cadr pair))
                       (nth 2 pair))))
      (harmless-x-test-not-root child))))
