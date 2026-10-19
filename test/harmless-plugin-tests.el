;;; harmless-plugin-tests.el --- Tests for plugin install -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(defun harmless-plugin-test-read (path)
  "Return PATH as a Unix-newline string."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (buffer-string)))

(defun harmless-plugin-test-write (file text)
  "Write TEXT to FILE with Unix newlines."
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file file
      (insert text))))

(defun harmless-plugin-test-bundle (root &optional skill-body)
  "Write a review-kit plugin and marketplace.json under ROOT.
SKILL-BODY defaults to HPL-SKILL-BODY.  Return the plugin directory."
  (let ((plug (expand-file-name "review-kit" root))
        (body (or skill-body "HPL-SKILL-BODY")))
    (harmless-plugin-test-write
     (expand-file-name "plugin.json" plug)
     "{\n  \"name\": \"review-kit\",\n  \"description\": \"Review helpers\"\n}\n")
    (harmless-plugin-test-write
     (expand-file-name "skills/review-kit/SKILL.md" plug)
     (format "---\nname: review-kit\ndescription: Review the change.\n---\n%s\n"
             body))
    (harmless-plugin-test-write
     (expand-file-name "agents/reviewer.md" plug)
     "---\nname: reviewer\ndescription: Reviews.\n---\nHPL-AGENT-BODY\n")
    (harmless-plugin-test-write
     (expand-file-name "hooks.json" plug)
     "[{\"event\":\"pre\",\"type\":\"command\",\"tool\":\"write_file\",\"command\":\"echo no\"}]\n")
    (harmless-plugin-test-write
     (expand-file-name "mcp.json" plug)
     "{\"mcpServers\":{\"kit\":{\"command\":\"echo\",\"args\":[\"hi\"]}}}\n")
    (harmless-plugin-test-write
     (expand-file-name "marketplace.json" root)
     "{\"name\":\"local\",\"plugins\":[{\"name\":\"review-kit\",\"source\":\"review-kit\"}]}\n")
    (harmless-plugin-test-write
     (expand-file-name "secret/plugin.json" root)
     "{\n  \"name\": \"secret\",\n  \"description\": \"Not listed\"\n}\n")
    plug))

(defun harmless-plugin-test-session (id cwd)
  "Return a session ID at CWD."
  (harmless-session--create
   :id id
   :cwd cwd
   :model "grok-4.6"
   :source 'agent
   :messages nil
   :plan-mode nil))

(defun harmless-plugin-test-signal (fn)
  "Return the error text from FN."
  (error-message-string (should-error (funcall fn) :type 'error)))

(ert-deftest harmless-plugin-install-copies-one-bundle ()
  (let* ((base (make-temp-file "harmless-plugin-" t))
         (root (expand-file-name "proj" base))
         (market (expand-file-name "market" base))
         (harmless-directory (expand-file-name ".harmless-state" base))
         (session (harmless-plugin-test-session "plugin-copy" root))
         (source nil)
         (dest nil))
    (unwind-protect
        (progn
          (setq source (harmless-plugin-test-bundle market))
          (should-not (harmless-skill-find root "review-kit" root))
          (should-not (harmless-agent-find root "reviewer" root))
          (setq dest (harmless-plugin-install
                      session (file-name-as-directory market) "review-kit"))
          (should (file-directory-p dest))
          (should (file-name-absolute-p dest))
          (should (harmless-directory-strictly-under-p dest root))
          (should-not (harmless-filesystem-root-p dest))
          (should-not (equal (file-truename dest) (file-truename "/")))
          (should (equal (harmless-plugin-test-read
                          (expand-file-name "plugin.json" source))
                         (harmless-plugin-test-read
                          (expand-file-name "plugin.json" dest))))
          (should (equal (harmless-plugin-test-read
                          (expand-file-name "skills/review-kit/SKILL.md" source))
                         (harmless-plugin-test-read
                          (expand-file-name "skills/review-kit/SKILL.md" dest))))
          (should (equal (harmless-plugin-test-read
                          (expand-file-name "mcp.json" source))
                         (harmless-plugin-test-read
                          (expand-file-name "mcp.json" dest))))
          (should-not (file-exists-p (expand-file-name ".harmless/skills" root)))
          (should-not (file-exists-p (expand-file-name ".harmless/agents" root)))
          (should-not (file-exists-p (expand-file-name ".harmless/mcp.json" root)))
          (should-not (file-exists-p
                       (expand-file-name "plugins" harmless-directory)))
          (should (equal '("review-kit")
                         (cl-remove-if
                          (lambda (name) (member name '("." "..")))
                          (directory-files
                           (expand-file-name ".harmless/plugins" root)))))
          (let ((skill (harmless-skill-find root "review-kit" root)))
            (should skill)
            (should (string-match-p "HPL-SKILL-BODY" (plist-get skill :body)))
            (should (harmless-directory-strictly-under-p
                     (plist-get skill :path) root)))
          (let ((text (or (harmless-skills-catalog root root) "")))
            (should (string-match-p "review-kit" text))
            (should (string-match-p "Review the change" text))
            (should-not (string-match-p "HPL-SKILL-BODY" text)))
          (let ((spec (harmless-agent-find root "reviewer" root)))
            (should spec)
            (should (string-match-p "HPL-AGENT-BODY"
                                    (or (plist-get spec :prompt) "")))
            (should (harmless-directory-strictly-under-p
                     (plist-get spec :path) root)))
          (let ((server (cl-find "kit" (harmless-mcp-servers session)
                                 :key (lambda (item) (plist-get item :name))
                                 :test #'equal)))
            (should server)
            (should (equal "echo" (plist-get server :command)))
            (should (equal '("hi") (plist-get server :args))))
          (let ((hooks (harmless-plugin-hooks session)))
            (should (equal "write_file" (plist-get (car hooks) :tool)))
            (should (eq 'command (plist-get (car hooks) :type)))
            (should (null (cdr hooks))))
          (let ((result 'unset)
                (harmless-hook-command-function
                 (lambda (_hook _payload callback)
                   (funcall callback
                            '(:status 0
                              :stdout "{\"decision\":\"block\",\"reason\":\"nope\"}"
                              :stderr "")))))
            (harmless-hook-pre
             session (harmless-tool-by-name "write_file") nil
             (lambda (value) (setq result value)))
            (should (equal "Error: hook blocked this tool call: nope" result)))
          (let ((before (harmless-plugin-test-read
                         (expand-file-name "plugin.json" dest))))
            (should (equal "plugin is already installed: review-kit"
                           (harmless-plugin-test-signal
                            (lambda ()
                              (harmless-plugin-install
                               session market "review-kit")))))
            (should (equal before
                           (harmless-plugin-test-read
                            (expand-file-name "plugin.json" dest)))))
          (should (equal (harmless-plugin-test-read
                          (expand-file-name "plugin.json" source))
                         (harmless-plugin-test-read
                          (expand-file-name "plugin.json" dest))))
          (should (equal "no plugin named secret"
                         (harmless-plugin-test-signal
                          (lambda ()
                            (harmless-plugin-install
                             session market "secret"))))))
      (ignore-errors (delete-directory base t)))))

(ert-deftest harmless-plugin-install-refuses-a-bad-request ()
  (let* ((base (make-temp-file "harmless-plugin-bad-" t))
         (root (expand-file-name "proj" base))
         (market (expand-file-name "market" base))
         (harmless-directory (expand-file-name ".harmless-state" base))
         (session (harmless-plugin-test-session "plugin-bad" root))
         (before nil))
    (unwind-protect
        (progn
          (harmless-plugin-test-bundle market)
          (dolist (pair (list (cons nil "plugin name is empty")
                              (cons "" "plugin name is empty")
                              (cons "  " "plugin name is empty")
                              (cons 'review-kit "plugin name must be a string")
                              (cons "sub/" "plugin name is not a name")
                              (cons "notes/" "plugin name is not a name")
                              (cons "/" "plugin name is not a name")
                              (cons "c:/" "plugin name is not a name")
                              (cons "~" "plugin name is not a name")
                              (cons ".." "plugin name is not a name")))
            (should (equal (cdr pair)
                           (harmless-plugin-test-signal
                            (lambda ()
                              (harmless-plugin-install
                               session market (car pair)))))))
          (should (equal "No Harmless session"
                         (harmless-plugin-test-signal
                          (lambda ()
                            (harmless-plugin-install nil market "review-kit")))))
          (should (equal "plugin scope is not a scope"
                         (harmless-plugin-test-signal
                          (lambda ()
                            (harmless-plugin-install
                             session market "review-kit" "nope")))))
          (should (equal "no plugin named other"
                         (harmless-plugin-test-signal
                          (lambda ()
                            (harmless-plugin-install
                             session market "other")))))
          (let ((default-directory (file-name-as-directory market)))
            (should (equal "plugin directory must be absolute"
                           (harmless-plugin-test-signal
                            (lambda ()
                              (harmless-plugin-install
                               session "notes" "review-kit")))))
            (should-not (file-exists-p
                         (expand-file-name ".harmless/plugins" root))))
          (dolist (path '("/.claude" "/.harmless" "c:/.claude" "c:/.harmless"
                          "d:/.claude" "d:/.harmless"))
            (push (cons path (file-exists-p path)) before))
          (dolist (dir (list "/" "c:/" "d:/" "c:\\" "C:/" "c:" "\\" "//" "/."))
            (should (equal "plugin directory is the filesystem root"
                           (harmless-plugin-test-signal
                            (lambda ()
                              (harmless-plugin-install
                               session dir "review-kit"))))))
          (dolist (pair before)
            (should (eq (cdr pair) (file-exists-p (car pair)))))
          (setf (harmless-session-cwd session) "notes")
          (let ((default-directory (file-name-as-directory root)))
            (should (equal "plugin directory must be absolute"
                           (harmless-plugin-test-signal
                            (lambda ()
                              (harmless-plugin-install
                               session market "review-kit")))))
            (should-not (harmless-skill-find root "review-kit" root)))
          (setf (harmless-session-cwd session) nil)
          (should (equal "This chat is not attached to a project"
                         (harmless-plugin-test-signal
                          (lambda ()
                            (harmless-plugin-install
                             session market "review-kit")))))
          (setf (harmless-session-cwd session) "")
          (should (equal "This chat is not attached to a project"
                         (harmless-plugin-test-signal
                          (lambda ()
                            (harmless-plugin-install
                             session market "review-kit")))))
          (setf (harmless-session-cwd session) "/")
          (should (equal "plugin directory is the filesystem root"
                         (harmless-plugin-test-signal
                          (lambda ()
                            (harmless-plugin-install
                             session market "review-kit")))))
          (should-not (file-exists-p
                       (expand-file-name ".harmless/plugins" root))))
      (ignore-errors (delete-directory base t)))))

(ert-deftest harmless-plugin-install-refuses-a-bad-bundle ()
  (let* ((base (make-temp-file "harmless-plugin-bundle-" t))
         (root (expand-file-name "proj" base))
         (market (expand-file-name "market" base))
         (harmless-directory (expand-file-name ".harmless-state" base))
         (session (harmless-plugin-test-session "plugin-bundle" root)))
    (unwind-protect
        (progn
          (harmless-plugin-test-bundle market)
          (harmless-plugin-test-write
           (expand-file-name "escape/marketplace.json" base)
           "{\"plugins\":[{\"name\":\"review-kit\",\"source\":\"../market/review-kit\"}]}\n")
          (should (equal
                   "plugin source escapes the marketplace: ../market/review-kit"
                   (harmless-plugin-test-signal
                    (lambda ()
                      (harmless-plugin-install
                       session (expand-file-name "escape" base)
                       "review-kit")))))
          (harmless-plugin-test-write
           (expand-file-name "abs/marketplace.json" base)
           "{\"plugins\":[{\"name\":\"review-kit\",\"source\":\"c:/review-kit\"}]}\n")
          (should (equal "plugin source must be relative"
                         (harmless-plugin-test-signal
                          (lambda ()
                            (harmless-plugin-install
                             session (expand-file-name "abs" base)
                             "review-kit")))))
          (harmless-plugin-test-write
           (expand-file-name "mismatch/other/plugin.json" base)
           "{\"name\":\"other\",\"description\":\"Other\"}\n")
          (harmless-plugin-test-write
           (expand-file-name "mismatch/marketplace.json" base)
           "{\"plugins\":[{\"name\":\"review-kit\",\"source\":\"other\"}]}\n")
          (should (equal "plugin name does not match"
                         (harmless-plugin-test-signal
                          (lambda ()
                            (harmless-plugin-install
                             session (expand-file-name "mismatch" base)
                             "review-kit")))))
          (harmless-plugin-test-write
           (expand-file-name "bad-hooks/review-kit/plugin.json" base)
           "{\"name\":\"review-kit\",\"description\":\"Bad hooks\"}\n")
          (harmless-plugin-test-write
           (expand-file-name "bad-hooks/review-kit/hooks.json" base)
           "[{\"event\":\"nope\",\"type\":\"command\",\"command\":\"echo no\"}]\n")
          (harmless-plugin-test-write
           (expand-file-name "bad-hooks/marketplace.json" base)
           "{\"plugins\":[{\"name\":\"review-kit\",\"source\":\"review-kit\"}]}\n")
          (should (equal "hook event is not an event"
                         (harmless-plugin-test-signal
                          (lambda ()
                            (harmless-plugin-install
                             session (expand-file-name "bad-hooks" base)
                             "review-kit")))))
          (should-not (file-exists-p
                       (expand-file-name ".harmless/plugins/review-kit" root)))
          (should (equal "marketplace is missing"
                         (harmless-plugin-test-signal
                          (lambda ()
                            (harmless-plugin-install
                             session (expand-file-name "missing" base)
                             "review-kit")))))
          (harmless-plugin-test-write
           (expand-file-name "a-file" base) "not a directory\n")
          (should (equal "marketplace is not a directory"
                         (harmless-plugin-test-signal
                          (lambda ()
                            (harmless-plugin-install
                             session (expand-file-name "a-file" base)
                             "review-kit"))))))
      (ignore-errors (delete-directory base t)))))

(ert-deftest harmless-plugin-user-copy-loses-to-the-project ()
  (let* ((base (make-temp-file "harmless-plugin-user-" t))
         (root (expand-file-name "proj" base))
         (market (expand-file-name "market" base))
         (harmless-directory (expand-file-name ".harmless-state" base))
         (session (harmless-plugin-test-session "plugin-user" nil))
         (user-dest nil))
    (unwind-protect
        (progn
          (harmless-plugin-test-bundle market "HPL-USER-BODY")
          (setq user-dest
                (harmless-plugin-install session market "review-kit" "user"))
          (should (file-directory-p user-dest))
          (should (harmless-directory-strictly-under-p
                   user-dest harmless-directory))
          (should-not (equal (file-truename user-dest) (file-truename "/")))
          (should (string-match-p
                   "HPL-USER-BODY"
                   (harmless-plugin-test-read
                    (expand-file-name "skills/review-kit/SKILL.md" user-dest))))
          (should-not (harmless-skill-find root "review-kit" root))
          (let ((skill (harmless-skill-find root "review-kit")))
            (should skill)
            (should (string-match-p "HPL-USER-BODY" (plist-get skill :body))))
          (setf (harmless-session-cwd session) root)
          (harmless-plugin-test-write
           (expand-file-name
            "review-kit/skills/review-kit/SKILL.md" market)
           "---\nname: review-kit\ndescription: Review the change.\n---\nHPL-PROJECT-BODY\n")
          ;; The marketplace source changed.  The user copy stays.
          (should (string-match-p
                   "HPL-USER-BODY"
                   (harmless-plugin-test-read
                    (expand-file-name "skills/review-kit/SKILL.md" user-dest))))
          (harmless-plugin-install session market "review-kit" 'project)
          (let ((skill (harmless-skill-find root "review-kit" root)))
            (should (string-match-p "HPL-PROJECT-BODY" (plist-get skill :body)))
            (should (harmless-directory-strictly-under-p
                     (plist-get skill :path) root)))
          (let ((open (harmless-skill-find root "review-kit")))
            (should (string-match-p "HPL-PROJECT-BODY" (plist-get open :body)))))
      (ignore-errors (delete-directory base t)))))
