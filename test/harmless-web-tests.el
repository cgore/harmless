;;; harmless-web-tests.el --- Tests for Harmless web search -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless-provider)
(require 'harmless-session)
(require 'harmless-tools)
(require 'harmless-perm)
(require 'harmless-turn)
(require 'harmless-agent)
(require 'harmless-web)
(require 'harmless-ui)

(cl-defstruct (harmless-web-fake
               (:include harmless-provider)
               (:constructor harmless-web-make-fake))
  script)

(cl-defmethod harmless-provider-complete ((provider harmless-web-fake)
                                          _messages _tools callback)
  (when (harmless-web-fake-script provider)
    (funcall (harmless-web-fake-script provider) _messages callback))
  nil)

(defmacro harmless-web-test-with (data &rest body)
  "Bind a temp data directory DATA, then run BODY."
  (declare (indent 1))
  `(let* ((,data (make-temp-file "harmless-web-data-" t))
          (harmless-directory ,data)
          (harmless--sessions (make-hash-table :test 'equal))
          (harmless-web-search-function
           (lambda (query)
             (list (list :title "Emacs" :url "https://example.com/emacs"
                         :snippet (concat "hit " query)))))
          (harmless-perm-ask-function
           (lambda (_session _class _item callback)
             (funcall callback 'allow))))
     ,@body))

(defun harmless-web-test-session (&rest args)
  "Return a session.  ARGS are passed to `harmless-session-new'."
  (apply #'harmless-session-new
         :provider (harmless-web-make-fake :name "fake" :host "none")
         :model "m"
         args))

(defun harmless-web-test-call (session query)
  "Return the web_search tool result for QUERY on SESSION."
  (harmless-tool-call (harmless-tool-by-name "web_search")
                      session
                      (list :query query)))

(defun harmless-web-test-not-root (session)
  "Require SESSION's directory to stay off the filesystem root."
  (let ((dir (harmless-session-directory session)))
    (should (file-name-absolute-p dir))
    (should-not (harmless-filesystem-root-p dir))
    (should (harmless-directory-strictly-under-p dir harmless-directory))
    (should-not (equal (file-truename dir)
                       (file-truename (expand-file-name "/"))))))

(ert-deftest harmless-web-refuses-a-bad-query ()
  (harmless-web-test-with _data
    (let* ((repo (make-temp-file "harmless-web-repo-" t))
           (session (harmless-web-test-session :cwd repo :id "s"))
           (called nil)
           (harmless-web-search-function
            (lambda (_query) (setq called t) nil)))
      (dolist (query (list nil :false 42))
        (should (equal (harmless-web-test-call session query)
                       "Error: query must be a string")))
      (dolist (query (list "" "  " "\n"))
        (should (equal (harmless-web-test-call session query)
                       "Error: query is empty")))
      (should-not called)
      (should (eq (harmless-session-status session) 'idle))
      (should (equal (harmless-web-test-raw-empty session) ""))
      (harmless-web-test-not-root session))))

(defun harmless-web-test-raw-empty (session)
  "Return SESSION's messages.jsonl contents."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "messages.jsonl" (harmless-session-directory session)))
    (buffer-string)))

(ert-deftest harmless-web-refuses-a-bad-url ()
  (let ((harmless-web-retrieve-function
         (lambda (_url) (error "network was called"))))
    (dolist (url (list nil "" "   "))
      (should (equal (condition-case err
                         (harmless-web-get url)
                       (error (concat "Error: " (error-message-string err))))
                     "Error: url is empty")))
    (dolist (url (list 42 :false))
      (should (equal (condition-case err
                         (harmless-web-get url)
                       (error (concat "Error: " (error-message-string err))))
                     "Error: url must be a string")))
    (dolist (url (list "sub" "sub/" "/" "c:/" "../x" "file:///etc/passwd"
                       "javascript:alert(1)" "ftp://example.com/a"
                       "https://example.com/a b"))
      (should (equal (condition-case err
                         (harmless-web-get url)
                       (error (concat "Error: " (error-message-string err))))
                     "Error: url must be http or https"))))
  (let (seen)
    (let ((harmless-web-retrieve-function
           (lambda (url) (setq seen url) "body")))
      (should (equal (harmless-web-get "  https://example.com/a  ") "body"))
      (should (equal seen "https://example.com/a"))
      (should (equal (harmless-web-get "HTTP://example.com/b") "body"))
      (should (equal seen "HTTP://example.com/b")))))

(ert-deftest harmless-web-parses-duckduckgo ()
  (let* ((topics "")
         (i 0)
         (raw nil)
         (parsed nil))
    (while (< i 9)
      (setq i (1+ i))
      (setq topics
            (concat topics
                    (format "{\"Text\":\"Item %d - note\",\"FirstURL\":\"https://example.com/%d\"},"
                            i i))))
    (setq raw
          (concat "{\"Heading\":\"Emacs\",\"AbstractText\":\"An editor.\","
                  "\"AbstractURL\":\"https://www.gnu.org/software/emacs/\","
                  "\"RelatedTopics\":["
                  (substring topics 0 -1)
                  ",{\"Topics\":[{\"Text\":\"Nested - inner\","
                  "\"FirstURL\":\"https://example.com/nested\"}]},"
                  "{\"Text\":\"Dup - again\","
                  "\"FirstURL\":\"https://www.gnu.org/software/emacs/\"}]}"))
    (setq parsed (harmless-web-parse-duckduckgo raw))
    (should (= (length parsed) 8))
    (should (equal (car parsed)
                   (list :title "Emacs"
                         :url "https://www.gnu.org/software/emacs/"
                         :snippet "An editor.")))
    (should (equal (plist-get (cadr parsed) :title) "Item 1"))
    (should (equal (plist-get (cadr parsed) :url) "https://example.com/1"))
    (should (equal (plist-get (nth 7 parsed) :title) "Item 7"))
    (should-not (cl-find "https://example.com/9"
                         parsed
                         :key (lambda (item) (plist-get item :url))
                         :test #'equal))
    (should (equal (harmless-web-parse-duckduckgo "{}") nil))
    (should (equal (condition-case err
                       (harmless-web-parse-duckduckgo "{")
                     (error (error-message-string err)))
                   "web search failed: End of file while parsing JSON: 1, nil, 1"))
    (should (equal (harmless-web-search-url "emacs lisp")
                   (concat "https://api.duckduckgo.com/?format=json"
                           "&no_html=1&skip_disambig=1&q=emacs%20lisp")))
    (should (equal (harmless-web-search-url "c:/")
                   (concat "https://api.duckduckgo.com/?format=json"
                           "&no_html=1&skip_disambig=1&q=c%3A%2F")))
    (let* ((seen nil)
           (harmless-web-retrieve-function
            (lambda (url) (setq seen url) raw)))
      (should (equal (car (harmless-web-search-default "emacs lisp"))
                     (list :title "Emacs"
                           :url "https://www.gnu.org/software/emacs/"
                           :snippet "An editor.")))
      (should (equal seen (harmless-web-search-url "emacs lisp"))))))

(ert-deftest harmless-web-search-formats-results ()
  (harmless-web-test-with _data
    (let* ((repo (make-temp-file "harmless-web-repo-" t))
           (calls 0)
           (session (harmless-web-test-session
                     :cwd repo :id "s" :title "Keep"
                     :provider
                     (harmless-web-make-fake
                      :name "fake" :host "none"
                      :script (lambda (&rest _) (setq calls (1+ calls)))))))
      (should (equal (harmless-web-test-call session "  emacs  ")
                     (concat "1. Emacs\nhttps://example.com/emacs\n"
                             "hit emacs")))
      (should (= calls 0))
      (should (eq (harmless-session-status session) 'idle))
      (should (equal (harmless-session-title session) "Keep"))
      (let ((harmless-web-search-function (lambda (_query) nil)))
        (should (equal (harmless-web-test-call session "none") "No results")))
      (let ((harmless-web-search-function (lambda (_query) "nope")))
        (should (equal (harmless-web-test-call session "bad")
                       "Error: web search failed")))
      (should (equal (harmless-ui--tool-verb "web_search") "search"))
      (should (equal (harmless-ui--tool-summary "web_search" '(:query "emacs"))
                     "emacs"))
      (harmless-web-test-not-root session)
      (let ((detached (harmless-web-test-session :detached t :id "chat")))
        (should (member "web_search"
                        (mapcar #'harmless-tool-name
                                (harmless-tools-for-session detached))))
        (should (equal (harmless-web-test-call detached "emacs")
                       (concat "1. Emacs\nhttps://example.com/emacs\n"
                               "hit emacs")))
        (harmless-web-test-not-root detached)
        (should (string-search "/_detached/"
                               (harmless-session-directory detached)))))))

(ert-deftest harmless-web-permission-and-plan-mode ()
  (harmless-web-test-with _data
    (let* ((repo (make-temp-file "harmless-web-repo-" t))
           (calls 0)
           (searches nil)
           (asked nil)
           (harmless-web-search-function
            (lambda (query)
              (push query searches)
              (list (list :title "Emacs" :url "https://example.com/emacs"
                          :snippet "hit"))))
           (session
            (harmless-session-new
             :cwd repo :id "s" :title "Keep" :model "m"
             :provider
             (harmless-web-make-fake
              :name "fake" :host "none"
              :script
              (lambda (_messages callback)
                (setq calls (1+ calls))
                (if (= calls 1)
                    (progn
                      (funcall callback
                               '(:tool-call "1" "web_search"
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
             (harmless-web-make-fake
              :name "fake" :host "none"
              :script
              (lambda (_messages callback)
                (setq calls (1+ calls))
                (if (= calls 1)
                    (progn
                      (funcall callback
                               '(:tool-call "1" "web_search"
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
                     '("1. Emacs\nhttps://example.com/emacs\nhit")))
      (setq calls 0)
      (setq asked nil)
      (setq searches nil)
      (setq session
            (harmless-session-new
             :cwd repo :id "s3" :title "Keep" :model "m"
             :permission-mode 'always-approve
             :provider
             (harmless-web-make-fake
              :name "fake" :host "none"
              :script
              (lambda (_messages callback)
                (setq calls (1+ calls))
                (if (= calls 1)
                    (progn
                      (funcall callback
                               '(:tool-call "1" "web_search"
                                 "{\"query\":\"emacs\"}"))
                      (funcall callback '(:stop "tool_calls")))
                  (funcall callback '(:text "done"))
                  (funcall callback '(:stop "stop")))))))
      (setq harmless-perm-ask-function
            (lambda (&rest _) (error "permission asked")))
      (harmless-turn-run session "go")
      (should-not asked)
      (should (equal searches '("emacs")))
      (harmless-web-test-not-root session))))

(ert-deftest harmless-web-read-only-may-search ()
  (harmless-web-test-with _data
    (let* ((repo (make-temp-file "harmless-web-repo-" t))
           (child (harmless-web-test-session
                   :cwd repo :id "child" :source 'agent :parent-id "p")))
      (setf (harmless-session-agent child)
            (list :allow 'all :sandbox 'read-only :callback nil
                  :turns 0 :max 8 :error nil :prompt nil :task nil))
      (let ((names (mapcar #'harmless-tool-name
                           (harmless-agent-filter-tools
                            child (harmless-tools-enabled)))))
        (should (member "web_search" names))
        (should-not (member "write_file" names)))
      (should-not (harmless-agent-refuse-call
                   child (harmless-tool-by-name "web_search")
                   '(:query "emacs")))
      (should (equal (harmless-web-test-call child "emacs")
                     (concat "1. Emacs\nhttps://example.com/emacs\n"
                             "hit emacs")))
      (setf (plist-get (harmless-session-agent child) :allow) '("read_file"))
      (should (equal (harmless-agent-refuse-call
                      child (harmless-tool-by-name "web_search")
                      '(:query "emacs"))
                     "Error: this subagent cannot use web_search"))
      (harmless-web-test-not-root child))))
