;;; harmless-computer-tests.el --- Tests for computer control -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(cl-defstruct (harmless-computer-fake
               (:include harmless-provider)
               (:constructor harmless-computer-make-fake))
  script)

(cl-defmethod harmless-provider-complete ((provider harmless-computer-fake)
                                          _messages _tools callback)
  (when (harmless-computer-fake-script provider)
    (funcall (harmless-computer-fake-script provider) _messages callback))
  nil)

(defvar harmless-computer-test-calls 0
  "Provider calls in the current computer-control test.")

(defun harmless-computer-test-script (event)
  "Return a provider script that sends EVENT once, then text."
  (lambda (_messages callback)
    (setq harmless-computer-test-calls
          (1+ harmless-computer-test-calls))
    (if (= harmless-computer-test-calls 1)
        (progn
          (funcall callback event)
          (funcall callback '(:stop "tool_calls")))
      (funcall callback '(:text "done"))
      (funcall callback '(:stop "stop")))))

(defmacro harmless-computer-test-with (data &rest body)
  "Bind a temp data directory DATA, then run BODY.
The desktop, PowerShell, and the browser are not called."
  (declare (indent 1))
  `(let* ((,data (make-temp-file "harmless-computer-data-" t))
          (harmless-directory ,data)
          (harmless--sessions (make-hash-table :test 'equal))
          (harmless-computer-test-calls 0)
          (harmless-computer-capture-function
           (lambda () (error "capture was called")))
          (harmless-computer-act-function
           (lambda (&rest _) (error "act was called")))
          (harmless-computer-browse-function
           (lambda (&rest _) (error "browse was called")))
          (harmless-computer-run-function
           (lambda (&rest _) (error "powershell was called")))
          (harmless-perm-ask-function
           (lambda (_session _class _item callback)
             (funcall callback 'allow))))
     ,@body))

(defun harmless-computer-test-session (&rest args)
  "Return a session.  ARGS are passed to `harmless-session-new'."
  (apply #'harmless-session-new
         :provider (harmless-computer-make-fake :name "fake" :host "none")
         :model "m"
         args))

(defun harmless-computer-test-error (thunk)
  "Return the error string from THUNK."
  (condition-case err
      (progn (funcall thunk) "no-error")
    (error (error-message-string err))))

(defun harmless-computer-test-call (name session args)
  "Call tool NAME on SESSION with ARGS."
  (harmless-tool-call (harmless-tool-by-name name) session args))

(defun harmless-computer-test-raw (session)
  "Return SESSION's messages.jsonl, or an empty string."
  (let ((file (expand-file-name "messages.jsonl"
                                (harmless-session-directory session))))
    (if (file-regular-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (buffer-string))
      "")))

(defun harmless-computer-test-files (dir)
  "Return the names directly under DIR, sorted."
  (sort (directory-files dir t) #'string<))

(defun harmless-computer-test-not-root (session)
  "Require SESSION's directory to stay off the filesystem root."
  (let ((dir (harmless-session-directory session)))
    (should (file-name-absolute-p dir))
    (should-not (harmless-filesystem-root-p dir))
    (should (harmless-directory-strictly-under-p dir harmless-directory))
    (should-not (equal (file-truename dir)
                       (file-truename (expand-file-name "/"))))))

(defun harmless-computer-test-decoded (script)
  "Return the base64 payload embedded in PowerShell SCRIPT."
  (should (string-match "FromBase64String('\\([^']+\\)')" script))
  (decode-coding-string
   (base64-decode-string (match-string 1 script))
   'utf-8-unix))

(defun harmless-computer-test-tools (session)
  "Return the tool results recorded on SESSION."
  (cl-loop for msg in (harmless-session-messages session)
           when (eq (plist-get msg :role) :tool)
           collect (plist-get msg :content)))

(ert-deftest harmless-computer-refuses-bad-actions ()
  (harmless-computer-test-with data
    (let* ((repo (make-temp-file "harmless-computer-repo-" t))
           (readme (expand-file-name "README" repo))
           (calls 0)
           (seen nil)
           (session (harmless-computer-test-session
                     :cwd repo :id "s" :title "Keep"
                     :provider
                     (harmless-computer-make-fake
                      :name "fake" :host "none"
                      :script (lambda (&rest _) (setq calls (1+ calls))))))
           (files nil)
           (stored nil))
      (with-temp-file readme (insert "keep\n"))
      (setq files (harmless-computer-test-files repo))
      (setq stored (harmless-computer-test-files
                    (harmless-session-directory session)))
      (dolist (pair '((nil . "x must be an integer")
                      (:false . "x must be an integer")
                      (1.5 . "x must be an integer")
                      ("10" . "x must be an integer")
                      (-1 . "x must be a non-negative integer")))
        (should (equal (harmless-computer-test-call
                        "computer_click" session
                        (list :x (car pair) :y 1))
                       (concat "Error: " (cdr pair)))))
      (should (equal (harmless-computer-test-call
                      "computer_click" session '(:x 0))
                     "Error: y must be an integer"))
      (should (equal (harmless-computer-test-call
                      "computer_click" session '(:x 0 :y -1))
                     "Error: y must be a non-negative integer"))
      (dolist (button '(:false 42 "" "   " "sideways"))
        (should (equal (harmless-computer-test-call
                        "computer_click" session
                        (list :x 1 :y 2 :button button))
                       "Error: button must be left, right, or middle")))
      (dolist (text '(nil :false 42))
        (should (equal (harmless-computer-test-call
                        "computer_type" session (list :text text))
                       "Error: text must be a string")))
      (dolist (text '("" "   " "\n"))
        (should (equal (harmless-computer-test-call
                        "computer_type" session (list :text text))
                       "Error: text is empty")))
      (dolist (pair '((nil . "key is empty")
                      (:false . "key must be a string")
                      (42 . "key must be a string")
                      ("" . "key is empty")
                      ("   " . "key is empty")
                      ("\n" . "key is empty")
                      ("c:/" . "key is not a key")
                      ("sub" . "key is not a key")
                      ("f13" . "key is not a key")
                      ("f0" . "key is not a key")
                      ("f123" . "key is not a key")))
        (should (equal (harmless-computer-test-call
                        "computer_key" session (list :key (car pair)))
                       (concat "Error: " (cdr pair)))))
      (setq harmless-computer-act-function
            (lambda (action) (setq seen action)))
      (should (equal (harmless-computer-test-call
                      "computer_click" session '(:x 0 :y 0))
                     "Clicked left at 0, 0"))
      (should (equal seen '(:click t :x 0 :y 0 :button "left")))
      (should (equal (harmless-computer-test-call
                      "computer_click" session
                      '("x" 3 "y" 4 "button" " RIGHT "))
                     "Clicked right at 3, 4"))
      (should (equal seen '(:click t :x 3 :y 4 :button "right")))
      (should (equal (harmless-computer-test-call
                      "computer_click" session
                      '(:x 1 :y 2 :button "middle"))
                     "Clicked middle at 1, 2"))
      (should (equal (harmless-computer-test-call
                      "computer_type" session '(:text "/"))
                     "Typed 1 characters"))
      (should (equal seen '(:type "/")))
      (should (equal (harmless-computer-test-call
                      "computer_type" session '(:text "c:/"))
                     "Typed 3 characters"))
      (should (equal seen '(:type "c:/")))
      (should (equal (harmless-computer-test-call
                      "computer_type" session '(:text "  hi  "))
                     "Typed 6 characters"))
      (should (equal seen '(:type "  hi  ")))
      (should (equal (harmless-computer-test-call
                      "computer_type" session '("text" "c:/"))
                     "Typed 3 characters"))
      (dolist (pair '(("/" . "/")
                      ("A" . "A")
                      ("+" . "{+}")
                      ("enter" . "{ENTER}")
                      (" enter " . "{ENTER}")
                      ("tab" . "{TAB}")
                      ("escape" . "{ESC}")
                      ("backspace" . "{BS}")
                      ("delete" . "{DEL}")
                      ("up" . "{UP}")
                      ("down" . "{DOWN}")
                      ("left" . "{LEFT}")
                      ("right" . "{RIGHT}")
                      ("home" . "{HOME}")
                      ("end" . "{END}")
                      ("pageup" . "{PGUP}")
                      ("pagedown" . "{PGDN}")
                      ("space" . " ")
                      ("f1" . "{F1}")
                      ("F12" . "{F12}")
                      ("f9" . "{F9}")))
        (should (equal (harmless-computer-test-call
                        "computer_key" session (list :key (car pair)))
                       "Pressed key"))
        (should (equal seen (list :key (cdr pair)))))
      (should (= calls 0))
      (should (eq (harmless-session-status session) 'idle))
      (should (equal (harmless-session-title session) "Keep"))
      (should (equal (harmless-computer-test-raw session) ""))
      (should (equal (harmless-computer-test-files repo) files))
      (should (equal (with-temp-buffer
                       (insert-file-contents readme)
                       (buffer-string))
                     "keep\n"))
      (should (equal (harmless-computer-test-files
                      (harmless-session-directory session))
                     stored))
      (should-not (file-exists-p "/shot.png"))
      (should-not (file-exists-p "/computer"))
      (harmless-computer-test-not-root session)
      (ignore data))))

(ert-deftest harmless-computer-screenshot-stays-under-data ()
  (harmless-computer-test-with data
    (let* ((repo (make-temp-file "harmless-computer-repo-" t))
           (session (harmless-computer-test-session
                     :cwd repo :id "s" :title "Keep"))
           (file (harmless-computer-screenshot-file))
           (script nil)
           (shot nil))
      (should (file-name-absolute-p file))
      (should-not (file-exists-p file))
      (should-not (harmless-filesystem-root-p file))
      (should-not (harmless-filesystem-root-p
                   (file-name-directory file)))
      (should (harmless-directory-strictly-under-p file data))
      (should-not (harmless-directory-strictly-under-p file repo))
      (should (equal (file-name-nondirectory
                      (directory-file-name (file-name-directory file)))
                     "computer"))
      (should (string-match-p
               "\\`shot-[0-9]\\{8\\}T[0-9]\\{6\\}Z\\.png\\'"
               (file-name-nondirectory file)))
      (should-not (equal (file-truename file)
                         (file-truename
                          (expand-file-name
                           (file-name-nondirectory file) "/"))))
      (cl-letf (((symbol-function 'expand-file-name)
                 (lambda (&rest _) "/shot.png")))
        (should (equal (harmless-computer-test-error
                        #'harmless-computer-screenshot-file)
                       "Refusing to store a screenshot at /shot.png")))
      (should-not (file-exists-p "/shot.png"))
      (should-not (file-exists-p "/computer"))
      (setq harmless-computer-run-function
            (lambda (text)
              (setq script text)
              "1920 1080"))
      (setq shot (harmless-computer-capture-default))
      (should (equal (nth 0 shot) 1920))
      (should (equal (nth 1 shot) 1080))
      (should (equal (nth 2 shot) (harmless-computer-test-decoded script)))
      (should (file-name-absolute-p (nth 2 shot)))
      (should-not (file-exists-p (nth 2 shot)))
      (should (harmless-directory-strictly-under-p (nth 2 shot) data))
      (should-not (harmless-directory-strictly-under-p (nth 2 shot) repo))
      (should-not (harmless-filesystem-root-p (nth 2 shot)))
      (should (string-search "CopyFromScreen" script))
      (should (string-search "PrimaryScreen" script))
      (should-not (string-search repo script))
      (should-not (string-search (nth 2 shot) script))
      (setq harmless-computer-run-function (lambda (_text) "nope"))
      (should (equal (harmless-computer-test-error
                      #'harmless-computer-capture-default)
                     "computer capture failed"))
      (setq harmless-computer-capture-function
            (lambda () (list 8 6 (nth 2 shot))))
      (should (equal (harmless-computer-test-call
                      "computer_screenshot" session nil)
                     (format "8x6\n%s" (nth 2 shot))))
      (dolist (bad (list nil "nope" (list 1 2 "shot.png") (list 1 2 "/")
                         (list "8" 6 (nth 2 shot)) (list 8 6 "/")))
        (setq harmless-computer-capture-function (lambda () bad))
        (should (equal (harmless-computer-test-call
                        "computer_screenshot" session nil)
                       "Error: computer capture failed")))
      (setq harmless-computer-capture-function
            (lambda () (list 1 2 "c:/shot.png")))
      (should (equal (harmless-computer-test-call
                      "computer_screenshot" session nil)
                     "1x2\nc:/shot.png"))
      (should-not (file-exists-p "/shot.png"))
      (should-not (file-exists-p "/computer"))
      (harmless-computer-test-not-root session))))

(ert-deftest harmless-computer-scripts-do-not-spawn ()
  (harmless-computer-test-with _data
    (let ((ran nil)
          (script nil)
          (opened nil))
      (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil))
                ((symbol-function 'call-process)
                 (lambda (&rest _) (setq ran t) (error "ran"))))
        (should (equal (harmless-computer-test-error
                        (lambda ()
                          (harmless-computer-run-default "Write-Output 1")))
                       "computer control is not available"))
        (should-not ran)
        (should (equal (harmless-computer-test-error
                        (lambda () (harmless-computer-run-default "")))
                       "computer control is not available"))
        (should (equal (harmless-computer-test-error
                        (lambda () (harmless-computer-run-default nil)))
                       "computer control is not available"))
        (should-not ran))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (name)
                   (and (equal name "powershell") "powershell.exe")))
                ((symbol-function 'call-process)
                 (lambda (_program _infile buffer _display &rest args)
                   (with-current-buffer buffer (insert "ok"))
                   (setq script (car (last args)))
                   0)))
        (should (equal (harmless-computer-run-default "Write-Output 1")
                       "ok"))
        (should (equal script "Write-Output 1"))
        (setq script nil)
        (should (equal (harmless-computer-test-error
                        (lambda ()
                          (cl-letf (((symbol-function 'call-process)
                                     (lambda (&rest _) 7)))
                            (harmless-computer-run-default "nope"))))
                       "computer control failed")))
      (setq harmless-computer-run-function
            (lambda (text) (setq script text) ""))
      (harmless-computer-act-default
       '(:click t :x 10 :y 20 :button "left"))
      (should (string-search "SetCursorPos(10, 20)" script))
      (should (string-search "0x0002" script))
      (should (string-search "0x0004" script))
      (harmless-computer-act-default
       '(:click t :x 3 :y 4 :button "right"))
      (should (string-search "SetCursorPos(3, 4)" script))
      (should (string-search "0x0008" script))
      (should (string-search "0x0010" script))
      (harmless-computer-act-default
       '(:click t :x 5 :y 6 :button "middle"))
      (should (string-search "SetCursorPos(5, 6)" script))
      (should (string-search "0x0020" script))
      (should (string-search "0x0040" script))
      (harmless-computer-act-default '(:type "c:/"))
      (should (equal (harmless-computer-test-decoded script) "c:/"))
      (harmless-computer-act-default '(:type "/"))
      (should (equal (harmless-computer-test-decoded script) "/"))
      (harmless-computer-act-default '(:type "a+b"))
      (should (equal (harmless-computer-test-decoded script) "a{+}b"))
      (harmless-computer-act-default '(:key "{ENTER}"))
      (should (equal (harmless-computer-test-decoded script) "{ENTER}"))
      (should-not ran)
      (let ((previous harmless-browse-url-function))
        (unwind-protect
            (cl-letf (((symbol-function 'browse-url-default-browser)
                       (lambda (&rest _) (error "browser opened"))))
              (setq harmless-browse-url-function
                    (lambda (url) (push url opened)))
              (should (equal (harmless-computer-browse-default
                              "https://example.com/a")
                             "https://example.com/a"))
              (should (equal opened '("https://example.com/a"))))
          (setq harmless-browse-url-function previous))))))

(ert-deftest harmless-computer-browser-reuses-web-urls ()
  (harmless-computer-test-with _data
    (let* ((repo (make-temp-file "harmless-computer-repo-" t))
           (session (harmless-computer-test-session
                     :cwd repo :id "s" :title "Keep"))
           (opened nil))
      (dolist (pair '((nil . "url is empty")
                      ("" . "url is empty")
                      ("   " . "url is empty")
                      (:false . "url must be a string")
                      (42 . "url must be a string")
                      ("/" . "url must be http or https")
                      ("c:/" . "url must be http or https")
                      ("sub" . "url must be http or https")
                      ("file:///etc/passwd" . "url must be http or https")
                      ("javascript:alert(1)" . "url must be http or https")
                      ("http:/example.com" . "url must be http or https")))
        (should (equal (harmless-computer-test-call
                        "browser_open" session (list :url (car pair)))
                       (concat "Error: " (cdr pair)))))
      (setq harmless-computer-browse-function
            (lambda (url) (push url opened) url))
      (should (equal (harmless-computer-test-call
                      "browser_open" session
                      '(:url "  https://example.com/a  "))
                     "Opened https://example.com/a"))
      (should (equal (harmless-computer-test-call
                      "browser_open" session
                      '(:url "HTTP://example.com/b"))
                     "Opened HTTP://example.com/b"))
      (should (equal (harmless-computer-test-call
                      "browser_open" session
                      '("url" "https://example.com/c:/"))
                     "Opened https://example.com/c:/"))
      (should (equal opened '("https://example.com/c:/"
                              "HTTP://example.com/b"
                              "https://example.com/a")))
      (setf (harmless-session-plan-mode session) t)
      (setq opened nil)
      (should (equal (harmless-computer-test-call
                      "browser_open" session '(:url "https://example.com/a"))
                     harmless-computer-plan-refusal))
      (should (equal (harmless-computer-test-call
                      "computer_click" session nil)
                     harmless-computer-plan-refusal))
      (should (equal (harmless-computer-test-call
                      "computer_type" session '(:text "/"))
                     harmless-computer-plan-refusal))
      (should (equal (harmless-computer-test-call
                      "computer_key" session '(:key "/"))
                     harmless-computer-plan-refusal))
      (should-not opened)
      (should (harmless-session-plan-mode session))
      (setq harmless-computer-capture-function
            (lambda ()
              (list 2 3 (expand-file-name "computer/shot.png" _data))))
      (let ((text (harmless-computer-test-call
                   "computer_screenshot" session nil)))
        (should (string-prefix-p "2x3\n" text))
        (should-not (string-search "Plan mode is on" text)))
      (harmless-computer-test-not-root session))))

(ert-deftest harmless-computer-permissions-and-plan ()
  (harmless-computer-test-with data
    (let* ((repo (make-temp-file "harmless-computer-repo-" t))
           (files nil)
           (asked nil)
           (acted nil)
           (session nil))
      (with-temp-file (expand-file-name "README" repo) (insert "keep\n"))
      (setq files (harmless-computer-test-files repo))
      (setq harmless-computer-act-function
            (lambda (action) (push action acted)))
      (setq harmless-computer-capture-function
            (lambda ()
              (list 8 6 (expand-file-name "computer/shot.png" data))))
      (setq harmless-computer-browse-function
            (lambda (url) (push url acted) url))
      (setq session
            (harmless-session-new
             :cwd repo :id "ask" :title "Keep" :model "m"
             :provider
             (harmless-computer-make-fake
              :name "fake" :host "none"
              :script (harmless-computer-test-script
                       '(:tool-call "1" "computer_click"
                         "{\"x\":10,\"y\":20}")))))
      (setq harmless-perm-ask-function
            (lambda (_session class item callback)
              (push (list class item) asked)
              (funcall callback 'deny)))
      (setq harmless-computer-test-calls 0)
      (harmless-turn-run session "go")
      (should (equal asked '((shell "10,20"))))
      (should-not acted)
      (should (equal (harmless-computer-test-tools session)
                     '("Error: user denied this tool call")))
      (setq asked nil)
      (setq acted nil)
      (setq session
            (harmless-session-new
             :cwd repo :id "edits" :title "Keep" :model "m"
             :permission-mode 'accept-edits
             :provider
             (harmless-computer-make-fake
              :name "fake" :host "none"
              :script (harmless-computer-test-script
                       '(:tool-call "1" "computer_type"
                         "{\"text\":\"c:/\"}")))))
      (setq harmless-perm-ask-function
            (lambda (_session class item callback)
              (push (list class item) asked)
              (funcall callback 'allow)))
      (setq harmless-computer-test-calls 0)
      (harmless-turn-run session "go")
      (should (equal asked '((shell "c:/"))))
      (should (equal acted '((:type "c:/"))))
      (should (equal (harmless-computer-test-tools session)
                     '("Typed 3 characters")))
      (setq asked nil)
      (setq acted nil)
      (setq session
            (harmless-session-new
             :cwd repo :id "shot" :title "Keep" :model "m"
             :permission-mode 'always-approve
             :provider
             (harmless-computer-make-fake
              :name "fake" :host "none"
              :script (harmless-computer-test-script
                       '(:tool-call "1" "computer_screenshot" "{}")))))
      (setf (harmless-session-plan-mode session) t)
      (setq harmless-perm-ask-function
            (lambda (&rest _) (error "permission asked")))
      (setq harmless-computer-test-calls 0)
      (harmless-turn-run session "go")
      (should (harmless-session-plan-mode session))
      (should (equal (harmless-computer-test-tools session)
                     (list (format "8x6\n%s"
                                   (expand-file-name
                                    "computer/shot.png" data)))))
      (should-not (cl-find "Plan mode is on"
                           (harmless-session-messages session)
                           :test
                           (lambda (text msg)
                             (and (stringp (plist-get msg :content))
                                  (string-search
                                   text (plist-get msg :content))))))
      (setq session
            (harmless-session-new
             :cwd repo :id "plan" :title "Keep" :model "m"
             :permission-mode 'always-approve
             :provider
             (harmless-computer-make-fake
              :name "fake" :host "none"
              :script (harmless-computer-test-script
                       '(:tool-call "1" "browser_open"
                         "{\"url\":\"https://example.com/a\"}")))))
      (setf (harmless-session-plan-mode session) t)
      (setq harmless-computer-test-calls 0)
      (harmless-turn-run session "go")
      (should (equal (harmless-computer-test-tools session)
                     (list harmless-computer-plan-refusal)))
      (should (harmless-session-plan-mode session))
      (should-not acted)
      (setq asked nil)
      (setq session
            (harmless-session-new
             :cwd repo :id "key" :title "Keep" :model "m"
             :permission-mode 'always-approve
             :provider
             (harmless-computer-make-fake
              :name "fake" :host "none"
              :script (harmless-computer-test-script
                       '(:tool-call "1" "computer_key"
                         "{\"key\":\"enter\"}")))))
      (setq harmless-perm-ask-function
            (lambda (&rest _) (error "permission asked")))
      (setq harmless-computer-test-calls 0)
      (harmless-turn-run session "go")
      (should (equal acted '((:key "{ENTER}"))))
      (should (equal (harmless-computer-test-tools session)
                     '("Pressed key")))
      (should (equal (harmless-turn--item-for
                      (harmless-tool-by-name "run_shell")
                      '(:command "git status" :text "nope"))
                     "git status"))
      (should (equal (harmless-turn--item-for
                      (harmless-tool-by-name "shell_kill")
                      '(:id "1"))
                     "shell_kill"))
      (should (equal (harmless-turn--item-for
                      (harmless-tool-by-name "computer_screenshot")
                      nil)
                     "computer_screenshot"))
      (should (equal (harmless-turn--item-for
                      (harmless-tool-by-name "computer_key")
                      '(:key "enter"))
                     "enter"))
      (should (equal (harmless-turn--item-for
                      (harmless-tool-by-name "browser_open")
                      '(:url "https://example.com/a"))
                     "https://example.com/a"))
      (should (equal (harmless-ui--tool-verb "computer_screenshot")
                     "screen"))
      (should (equal (harmless-ui--tool-verb "computer_click") "click"))
      (should (equal (harmless-ui--tool-verb "computer_type") "type"))
      (should (equal (harmless-ui--tool-verb "computer_key") "key"))
      (should (equal (harmless-ui--tool-verb "browser_open") "browser"))
      (should (equal (harmless-ui--tool-summary
                      "computer_screenshot" nil)
                     ""))
      (should (equal (harmless-ui--tool-summary
                      "computer_click" '(:x 10 :y 20))
                     "10,20"))
      (should (equal (harmless-ui--tool-summary
                      "computer_type" '(:text "/"))
                     "/"))
      (should (equal (harmless-ui--tool-summary
                      "computer_key" '(:key "enter"))
                     "enter"))
      (should (equal (harmless-ui--tool-summary
                      "browser_open" '(:url "https://example.com/a"))
                     "https://example.com/a"))
      (should (equal (harmless-computer-test-files repo) files))
      (dolist (name '("computer_screenshot" "computer_click"
                      "computer_type" "computer_key" "browser_open"))
        (let ((tool (harmless-tool-by-name name)))
          (should (eq (harmless-tool-class tool) 'shell))
          (should-not (harmless-tool-async tool))
          (should-not (harmless-tool-classify tool))))
      (harmless-computer-test-not-root session))))

(ert-deftest harmless-computer-read-only-and-detached ()
  (harmless-computer-test-with _data
    (let* ((repo (make-temp-file "harmless-computer-repo-" t))
           (child (harmless-computer-test-session
                   :cwd repo :id "child" :source 'agent :parent-id "p"
                   :title "Reader"))
           (names '("computer_screenshot" "computer_click"
                    "computer_type" "computer_key" "browser_open")))
      (setf (harmless-session-agent child)
            (list :allow 'all :sandbox 'read-only :callback nil
                  :turns 0 :max 8 :error nil :prompt nil :task nil))
      (let ((offered (mapcar #'harmless-tool-name
                             (harmless-tools-for-session child))))
        (dolist (name names)
          (should-not (member name offered)))
        (should (member "run_shell" offered))
        (should (member "read_file" offered))
        (should-not (member "write_file" offered)))
      (dolist (name names)
        (should (equal (harmless-agent-refuse-call
                        child (harmless-tool-by-name name) nil)
                       harmless-agent-read-only-refusal)))
      (setf (plist-get (harmless-session-agent child) :sandbox)
            'workspace)
      (let ((offered (mapcar #'harmless-tool-name
                             (harmless-tools-for-session child))))
        (dolist (name names)
          (should (member name offered))))
      (setf (plist-get (harmless-session-agent child) :allow)
            '("read_file"))
      (setf (plist-get (harmless-session-agent child) :sandbox)
            'read-only)
      (dolist (name names)
        (should (equal (harmless-agent-refuse-call
                        child (harmless-tool-by-name name) nil)
                       (format "Error: this subagent cannot use %s"
                               name))))
      (let ((detached (harmless-computer-test-session
                       :detached t :id "chat")))
        (should (equal (sort (mapcar #'harmless-tool-name
                                     (harmless-tools-for-session detached))
                             #'string<)
                       '("ask_user"
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
                         "spawn_agent"
                         "todo_write"
                         "web_fetch"
                         "web_search"
                         "x_keyword_search"
                         "x_thread_fetch"
                         "x_user_search")))
        (should-not (member "read_file"
                            (mapcar #'harmless-tool-name
                                    (harmless-tools-for-session detached))))
        (should-not (member "run_shell"
                            (mapcar #'harmless-tool-name
                                    (harmless-tools-for-session detached))))
        (harmless-computer-test-not-root detached))
      (harmless-computer-test-not-root child))))
