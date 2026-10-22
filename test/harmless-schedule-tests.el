;;; harmless-schedule-tests.el --- Tests for one-shot schedules -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(cl-defstruct (harmless-schedule-fake
               (:include harmless-provider)
               (:constructor harmless-schedule-make-fake))
  script)

(cl-defmethod harmless-provider-complete ((provider harmless-schedule-fake)
                                          _messages _tools callback)
  (when (harmless-schedule-fake-script provider)
    (funcall (harmless-schedule-fake-script provider) _messages callback))
  nil)

(defvar harmless-schedule-test-calls 0
  "Provider calls in the current schedule test.")

(defvar harmless-schedule-test-armed nil
  "Arms recorded by the current schedule test.")

(defun harmless-schedule-test-wake-script ()
  "Return a provider script that finishes a wake with text."
  (lambda (_messages callback)
    (setq harmless-schedule-test-calls
          (1+ harmless-schedule-test-calls))
    (funcall callback '(:text "done"))
    (funcall callback '(:stop "stop"))))

(defun harmless-schedule-test-tool-script (event)
  "Return a provider script that sends EVENT once, then text."
  (lambda (_messages callback)
    (setq harmless-schedule-test-calls
          (1+ harmless-schedule-test-calls))
    (if (= harmless-schedule-test-calls 1)
        (progn
          (funcall callback event)
          (funcall callback '(:stop "tool_calls")))
      (funcall callback '(:text "done"))
      (funcall callback '(:stop "stop")))))

(defmacro harmless-schedule-test-with (data &rest body)
  "Bind a temp data directory DATA, then run BODY.
No timer starts unless BODY asks."
  (declare (indent 1))
  `(let* ((,data (make-temp-file "harmless-schedule-data-" t))
          (harmless-directory ,data)
          (harmless--sessions (make-hash-table :test 'equal))
          (harmless-schedule--timers (make-hash-table :test 'eq))
          (harmless-schedule-test-calls 0)
          (harmless-schedule-test-armed nil)
          (harmless-compact-threshold nil)
          (harmless-schedule-arm-function
           (lambda (session id at)
             (push (list session id at) harmless-schedule-test-armed)))
          (harmless-perm-ask-function
           (lambda (_session _class _item callback)
             (funcall callback 'allow))))
     ,@body))

(defun harmless-schedule-test-session (&rest args)
  "Return a session.  ARGS are passed to `harmless-session-new'."
  (apply #'harmless-session-new
         (append args
                 (list :provider
                       (harmless-schedule-make-fake
                        :name "fake" :host "none"
                        :script (harmless-schedule-test-wake-script))
                       :model "m"))))

(defun harmless-schedule-test-call (session name args)
  "Call tool NAME on SESSION with ARGS."
  (harmless-tool-call (harmless-tool-by-name name) session args))

(defun harmless-schedule-test-expect (session name args text)
  "Assert tool NAME on SESSION with ARGS returns Error: TEXT."
  (should (equal (harmless-schedule-test-call session name args)
                 (concat "Error: " text))))

(defun harmless-schedule-test-files (dir)
  "Return the names directly under DIR, sorted."
  (sort (directory-files dir t) #'string<))

(defun harmless-schedule-test-user-texts (session)
  "Return user message text stored on SESSION."
  (cl-loop for msg in (harmless-session-messages session)
           when (eq (plist-get msg :role) :user)
           collect (plist-get msg :content)))

(defun harmless-schedule-test-tool-texts (session)
  "Return tool results stored on SESSION."
  (cl-loop for msg in (harmless-session-messages session)
           when (eq (plist-get msg :role) :tool)
           collect (plist-get msg :content)))

(defun harmless-schedule-test-assert-session (session)
  "Require SESSION's directory to stay off the filesystem root."
  (let ((dir (harmless-session-dir session)))
    (should (file-name-absolute-p dir))
    (should-not (harmless-filesystem-root-p dir))
    (should (harmless-directory-strictly-under-p dir harmless-directory))
    (should-not (equal (file-truename dir)
                       (file-truename (expand-file-name "/"))))))

(defun harmless-schedule-test-raw (session name)
  "Return NAME from SESSION's directory and require that path."
  (let ((file (expand-file-name name (harmless-session-dir session))))
    (should (file-name-absolute-p file))
    (should-not (harmless-filesystem-root-p file))
    (should (harmless-directory-strictly-under-p
             (file-name-directory file) harmless-directory))
    (should-not (equal (file-truename file)
                       (file-truename (expand-file-name name "/"))))
    (if (file-regular-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (buffer-string))
      "")))

(defun harmless-schedule-test-summary (session)
  "Return SESSION's saved summary plist."
  (harmless-json-decode
   (harmless-schedule-test-raw session "summary.json")))

(defun harmless-schedule-test-prompts (items)
  "Return prompts from schedule ITEMS."
  (mapcar (lambda (item) (plist-get item :prompt)) items))

(defun harmless-schedule-test-wakes (session)
  "Return how many times SESSION's provider was called."
  (ignore session)
  harmless-schedule-test-calls)

(defun harmless-schedule-test-assert-item (item id prompt)
  "Require ITEM to be ID and PROMPT, with a future time and no timer."
  (should (equal (plist-get item :id) id))
  (should (equal (plist-get item :prompt) prompt))
  (should (= (length item) 6))
  (should-not (plist-member item :timer))
  (let ((time (encode-time (iso8601-parse (plist-get item :at)))))
    (should (time-less-p (current-time) time))))

(ert-deftest harmless-schedule-refuses-bad-arguments ()
  (harmless-schedule-test-with _data
    (let* ((repo (make-temp-file "harmless-schedule-repo-" t))
           (files (harmless-schedule-test-files repo))
           (slash (file-exists-p "/schedule"))
           (cdrive (file-exists-p "c:/schedule"))
           (session (harmless-schedule-test-session
                     :cwd repo :id "bad" :title "Keep"))
           (tool (harmless-tool-by-name "harmless-tool-schedule"))
           (cancel (harmless-tool-by-name "harmless-tool-schedule-cancel")))
      (should (eq (harmless-tool-class tool) 'edit))
      (should-not (harmless-tool-async tool))
      (should-not (harmless-tool-classify tool))
      (should (equal (plist-get (harmless-tool-schema tool) :required)
                     ["prompt"]))
      (should (equal (plist-get (harmless-tool-schema cancel) :required)
                     ["id"]))
      (should (equal (harmless-ui--tool-verb "harmless-tool-schedule") "schedule"))
      (should (equal (harmless-ui--tool-verb "harmless-tool-schedule-cancel") "cancel"))
      (should (equal (harmless-ui--tool-summary
                      "harmless-tool-schedule" '(:prompt "remind me"))
                     "remind me"))
      (should (equal (harmless-ui--tool-summary
                      "harmless-tool-schedule-cancel" '(:id "1"))
                     "1"))
      (should (equal (harmless-ui--tool-summary
                      "harmless-tool-schedule-cancel" '(:id 2))
                     "2"))
      (should (equal (harmless-ui--tool-summary "harmless-tool-schedule" nil) ""))
      (should (equal (harmless-turn--item-for tool '(:prompt "remind me"))
                     "remind me"))
      (should (equal (harmless-turn--item-for cancel '(:id 2)) "2"))
      (should (equal (harmless-turn--item-for cancel nil)
                     "harmless-tool-schedule-cancel"))
      (dolist (prompt '(nil :false 42))
        (harmless-schedule-test-expect
         session "harmless-tool-schedule" (list :prompt prompt :delay 5)
         "prompt must be a string"))
      (dolist (prompt '("" "   " "\n"))
        (harmless-schedule-test-expect
         session "harmless-tool-schedule" (list :prompt prompt :delay 5)
         "prompt is empty"))
      (harmless-schedule-test-expect
       session "harmless-tool-schedule" '(:prompt "later") "schedule time is missing")
      (harmless-schedule-test-expect
       session "harmless-tool-schedule" '(:prompt "later" :delay nil :at nil)
       "schedule time is missing")
      (harmless-schedule-test-expect
       session "harmless-tool-schedule"
       (harmless-json-decode "{\"prompt\":\"later\",\"delay\":null}")
       "schedule time is missing")
      (harmless-schedule-test-expect
       session "harmless-tool-schedule"
       (harmless-json-decode
        "{\"prompt\":\"later\",\"delay\":null,\"at\":null}")
       "schedule time is missing")
      (dolist (args '((:prompt "later" :delay :false :at "nope")
                      (:prompt "later" :delay "5" :at "nope")
                      (:prompt "later" :delay 5 :at "2099-01-01T00:00:00Z")
                      (:prompt "later" :delay 0 :at "")))
        (harmless-schedule-test-expect
         session "harmless-tool-schedule" args "schedule time must be one"))
      (dolist (delay '(:false "5" 0 -1 0.0))
        (harmless-schedule-test-expect
         session "harmless-tool-schedule" (list :prompt "later" :delay delay)
         "delay must be a positive number"))
      (harmless-schedule-test-expect
       session "harmless-tool-schedule"
       (harmless-json-decode "{\"prompt\":\"later\",\"delay\":false}")
       "delay must be a positive number")
      (dolist (at '(:false 42))
        (harmless-schedule-test-expect
         session "harmless-tool-schedule" (list :prompt "later" :at at)
         "at must be a string"))
      (dolist (at '("" "   " "\n"))
        (harmless-schedule-test-expect
         session "harmless-tool-schedule" (list :prompt "later" :at at)
         "at is empty"))
      (dolist (at '("nope" "/" "c:/"))
        (harmless-schedule-test-expect
         session "harmless-tool-schedule" (list :prompt "later" :at at)
         "at is not a time"))
      (harmless-schedule-test-expect
       session "harmless-tool-schedule" '(:prompt "later" :at "2020-01-01T00:00:00Z")
       "schedule time has passed")
      (dolist (id `(nil "" "   " "\n"))
        (harmless-schedule-test-expect
         session "harmless-tool-schedule-cancel" (list :id id) "schedule id is empty"))
      (dolist (id '(:false 42.5))
        (harmless-schedule-test-expect
         session "harmless-tool-schedule-cancel" (list :id id)
         "schedule id must be a string"))
      (harmless-schedule-test-expect
       session "harmless-tool-schedule-cancel" '(:id -2) "schedule id is not an id")
      (harmless-schedule-test-expect
       session "harmless-tool-schedule-cancel" '(:id "-3") "schedule id is not an id")
      (dolist (id '("/" "c:/" "1" 0))
        (harmless-schedule-test-expect
         session "harmless-tool-schedule-cancel" (list :id id)
         (format "no schedule %s"
                 (if (integerp id) (number-to-string id) id))))
      (should-not (harmless-session-schedules session))
      (should-not harmless-schedule-test-armed)
      (should (equal (harmless-schedule-test-call
                      session "harmless-tool-schedule"
                      (harmless-json-decode
                       "{\"prompt\":\"kept-after-failures\",\"delay\":null,\"at\":\"2099-01-01T00:00:00Z\"}"))
                     "Scheduled 1 at 2099-01-01T00:00:00Z"))
      (harmless-schedule-test-assert-item
       (car (harmless-session-schedules session))
       "1" "kept-after-failures")
      (should (equal (harmless-schedule-test-files repo) files))
      (should (eq (file-exists-p "/schedule") slash))
      (should (eq (file-exists-p "c:/schedule") cdrive))
      (should-not (file-exists-p (expand-file-name "log.txt" repo)))
      (should-not (file-directory-p (expand-file-name "sub" repo)))
      (harmless-schedule-test-assert-session session)
      (should (= 0 (harmless-schedule-test-wakes session))))))

(ert-deftest harmless-schedule-stores-cancels-and-forks ()
  (harmless-schedule-test-with _data
    (let* ((repo (make-temp-file "harmless-schedule-repo-" t))
           (files (harmless-schedule-test-files repo))
           (slash (file-exists-p "/schedule"))
           (cdrive (file-exists-p "c:/schedule"))
           (before (current-time))
           (session (harmless-schedule-test-session
                     :cwd repo :id "store" :title "Keep"))
           (other (harmless-schedule-test-session
                   :cwd repo :id "other" :title "Other")))
      (let* ((result (harmless-schedule-test-call
                      session "harmless-tool-schedule" '(:prompt "soon" :delay 1.5)))
             (item (car (harmless-session-schedules session)))
             (at (plist-get item :at))
             (time (encode-time (iso8601-parse at))))
        (should (equal result (format "Scheduled 1 at %s" at)))
        (should (time-less-p before time))
        (should (time-less-p time (time-add before (seconds-to-time 30))))
        (harmless-schedule-test-assert-item item "1" "soon")
        (let ((saved (car (plist-get (harmless-schedule-test-summary session)
                                     :schedules))))
          (should (equal (plist-get saved :id) "1"))
          (should (equal (plist-get saved :prompt) "soon"))
          (should (equal (plist-get saved :at) at))
          (should-not (plist-member saved :timer))))
      (should (equal (harmless-schedule-test-call
                      session "harmless-tool-schedule"
                      '("prompt" "second" "at" "2099-01-01T00:00:00Z"))
                     "Scheduled 2 at 2099-01-01T00:00:00Z"))
      (should (equal (harmless-schedule-test-call
                      session "harmless-tool-schedule"
                      '(:prompt "  third  "
                        :at "  2099-06-01T00:00:00Z  "))
                     "Scheduled 3 at 2099-06-01T00:00:00Z"))
      (should (equal (harmless-schedule-test-call
                      session "harmless-tool-schedule-cancel" '("id" "  1  "))
                     "Cancelled schedule 1"))
      (should-not (cl-find "soon"
                           (plist-get (harmless-schedule-test-summary session)
                                      :schedules)
                           :test (lambda (wanted item)
                                   (equal wanted (plist-get item :prompt)))))
      (setq harmless-schedule-test-calls 0)
      (harmless-schedule-fire session "1")
      (should (= 0 (harmless-schedule-test-wakes session)))
      (should (null (harmless-schedule-test-user-texts session)))
      (should (equal (harmless-schedule-test-call
                      session "harmless-tool-schedule"
                      '(:prompt "fourth" :at "2099-07-01T00:00:00Z"))
                     "Scheduled 4 at 2099-07-01T00:00:00Z"))
      (should (equal (harmless-schedule-test-call
                      session "harmless-tool-schedule-cancel" '(:id 2))
                     "Cancelled schedule 2"))
      (should (equal (harmless-schedule-test-prompts
                      (harmless-session-schedules session))
                     '("third" "fourth")))
      (harmless-turn-abort session)
      (should (equal (harmless-schedule-test-prompts
                      (harmless-session-schedules session))
                     '("third" "fourth")))
      (should (equal (harmless-schedule-test-call
                      other "harmless-tool-schedule" '(:prompt "only-other" :delay 5))
                     (format "Scheduled 1 at %s"
                             (plist-get
                              (car (harmless-session-schedules other))
                              :at))))
      (should-not (harmless-schedule--find session "1"))
      (should (equal (plist-get (car (harmless-session-schedules other))
                                :prompt)
                     "only-other"))
      (dolist (prompt '("/" "c:/" "log.txt" "  sub/  "))
        (harmless-schedule-test-call
         session "harmless-tool-schedule"
         (list :prompt prompt :at "2099-08-01T00:00:00Z")))
      (should (equal (last (harmless-schedule-test-prompts
                            (harmless-session-schedules session))
                           4)
                     '("/" "c:/" "log.txt" "sub/")))
      (let ((child (harmless-fork--create session nil)))
        (should-not (harmless-session-schedules child))
        (should-not (plist-get (harmless-schedule-test-summary child)
                               :schedules))
        (should (cl-find "fourth"
                         (plist-get (harmless-schedule-test-summary session)
                                    :schedules)
                         :test (lambda (wanted item)
                                 (equal wanted (plist-get item :prompt)))))
        (should (harmless-session-schedules session))
        (harmless-schedule-test-assert-session child))
      (should (equal (harmless-schedule-test-files repo) files))
      (should (eq (file-exists-p "/schedule") slash))
      (should (eq (file-exists-p "c:/schedule") cdrive))
      (should-not (file-exists-p (expand-file-name "log.txt" repo)))
      (should-not (file-directory-p (expand-file-name "sub" repo)))
      (harmless-schedule-test-assert-session session)
      (harmless-schedule-test-assert-session other)
      (let (timer)
        (unwind-protect
            (let ((harmless-schedule-arm-function
                   #'harmless-schedule-arm-default)
                  (timed (harmless-schedule-test-session
                          :cwd repo :id "timed" :title "Keep")))
              (should (equal (harmless-schedule-test-call
                              timed "harmless-tool-schedule"
                              '(:prompt "far" :at "2099-01-01T00:00:00Z"))
                             "Scheduled 1 at 2099-01-01T00:00:00Z"))
              (setq timer (cdr (assoc "1"
                                      (gethash timed
                                               harmless-schedule--timers))))
              (should (timerp timer))
              (should (equal (harmless-schedule-test-call
                              timed "harmless-tool-schedule-cancel" '(:id "1"))
                             "Cancelled schedule 1"))
              (should-not (assoc "1"
                                 (gethash timed harmless-schedule--timers)))
              (harmless-schedule-test-assert-session timed))
          (when (timerp timer)
            (cancel-timer timer)))))))

(ert-deftest harmless-schedule-fires-and-permissions ()
  (harmless-schedule-test-with _data
    (let* ((repo (make-temp-file "harmless-schedule-repo-" t))
           (readme (expand-file-name "README" repo))
           (files nil)
           (slash (file-exists-p "/schedule"))
           (cdrive (file-exists-p "c:/schedule")))
      (with-temp-file readme (insert "keep\n"))
      (setq files (harmless-schedule-test-files repo))
      (let ((session (harmless-schedule-test-session
                      :cwd repo :id "idle" :title "Keep")))
        (should (equal (harmless-schedule-test-call
                        session "harmless-tool-schedule"
                        '(:prompt "/" :at "2099-01-01T00:00:00Z"))
                       "Scheduled 1 at 2099-01-01T00:00:00Z"))
        (setq harmless-schedule-test-calls 0)
        (harmless-schedule-fire session "1")
        (should (= 1 (harmless-schedule-test-wakes session)))
        (should (equal (harmless-schedule-test-user-texts session) '("/")))
        (should (equal (harmless-session-title session) "Keep"))
        (should-not (harmless-session-schedules session))
        (should-not (plist-get (harmless-schedule-test-summary session)
                               :schedules))
        (should (string-search "/"
                               (harmless-schedule-test-raw
                                session "messages.jsonl")))
        (harmless-schedule-test-assert-session session))
      (let ((session (harmless-schedule-test-session
                      :cwd repo :id "busy" :title "Keep")))
        (should (equal (harmless-schedule-test-call
                        session "harmless-tool-schedule"
                        '(:prompt "c:/" :delay 9))
                       (format "Scheduled 1 at %s"
                               (plist-get
                                (car (harmless-session-schedules session))
                                :at))))
        (setf (harmless-session-status session) 'streaming)
        (setq harmless-schedule-test-calls 0)
        (harmless-schedule-fire session "1")
        (should (= 0 (harmless-schedule-test-wakes session)))
        (should (eq (harmless-session-status session) 'streaming))
        (should (equal (harmless-session-steers session) '("c:/")))
        (should (null (harmless-schedule-test-user-texts session)))
        (should-not (harmless-session-schedules session))
        (should (equal (harmless-session-title session) "Keep")))
      (let ((session (harmless-schedule-test-session
                      :cwd repo :id "paths" :title "Keep")))
        (dolist (prompt '("log.txt" "sub/"))
          (harmless-schedule-test-call
           session "harmless-tool-schedule"
           (list :prompt prompt :at "2099-01-01T00:00:00Z")))
        (setq harmless-schedule-test-calls 0)
        (harmless-schedule-fire session "1")
        (harmless-schedule-fire session "2")
        (should (equal (harmless-schedule-test-user-texts session)
                       '("log.txt" "sub/")))
        (should-not (file-exists-p (expand-file-name "log.txt" repo)))
        (should-not (file-directory-p (expand-file-name "sub" repo))))
      (let ((asked nil)
            (session (harmless-session-new
                      :cwd repo :id "ask-deny" :title "Keep" :model "m"
                      :provider
                      (harmless-schedule-make-fake
                       :name "fake" :host "none"
                       :script (harmless-schedule-test-tool-script
                                '(:tool-call "1" "harmless-tool-schedule"
                                  "{\"prompt\":\"remind me\",\"at\":\"2099-01-01T00:00:00Z\"}"))))))
        (setq harmless-perm-ask-function
              (lambda (_session class item callback)
                (push (list class item) asked)
                (funcall callback 'deny)))
        (setq harmless-schedule-test-calls 0)
        (harmless-turn-run session "go")
        (should (equal asked '((edit "remind me"))))
        (should (equal (harmless-schedule-test-tool-texts session)
                       '("Error: user denied this tool call")))
        (should-not (harmless-session-schedules session))
        (should-not (plist-get (harmless-schedule-test-summary session)
                               :schedules))
        (should (equal (harmless-session-title session) "Keep")))
      (let ((session (harmless-session-new
                      :cwd repo :id "ask-allow" :title "Keep" :model "m"
                      :provider
                      (harmless-schedule-make-fake
                       :name "fake" :host "none"
                       :script (harmless-schedule-test-tool-script
                                '(:tool-call "1" "harmless-tool-schedule"
                                  "{\"prompt\":\"remind me\",\"delay\":30}"))))))
        (setq harmless-perm-ask-function
              (lambda (_session class item callback)
                (should (equal (list class item) '(edit "remind me")))
                (funcall callback 'allow)))
        (setq harmless-schedule-test-calls 0)
        (harmless-turn-run session "go")
        (should (equal (harmless-schedule-test-tool-texts session)
                       (list (format "Scheduled 1 at %s"
                                     (plist-get
                                      (car (harmless-session-schedules
                                            session))
                                      :at)))))
        (should (equal (plist-get
                        (car (harmless-session-schedules session))
                        :prompt)
                       "remind me")))
      (let ((session (harmless-session-new
                      :cwd repo :id "edits" :title "Keep" :model "m"
                      :permission-mode 'accept-edits
                      :provider
                      (harmless-schedule-make-fake
                       :name "fake" :host "none"
                       :script (harmless-schedule-test-tool-script
                                '(:tool-call "1" "harmless-tool-schedule"
                                  "{\"prompt\":\"edit-ok\",\"at\":\"2099-02-01T00:00:00Z\"}"))))))
        (setq harmless-perm-ask-function
              (lambda (&rest _) (error "permission asked")))
        (setq harmless-schedule-test-calls 0)
        (harmless-turn-run session "go")
        (should (equal (harmless-schedule-test-tool-texts session)
                       '("Scheduled 1 at 2099-02-01T00:00:00Z")))
        (should (harmless-session-schedules session)))
      (let ((session (harmless-session-new
                      :cwd repo :id "always" :title "Keep" :model "m"
                      :permission-mode 'always-approve
                      :provider
                      (harmless-schedule-make-fake
                       :name "fake" :host "none"
                       :script (harmless-schedule-test-tool-script
                                '(:tool-call "1" "harmless-tool-schedule"
                                  "{\"prompt\":\"always-ok\",\"at\":\"2099-03-01T00:00:00Z\"}"))))))
        (setq harmless-perm-ask-function
              (lambda (&rest _) (error "permission asked")))
        (setq harmless-schedule-test-calls 0)
        (harmless-turn-run session "go")
        (should (equal (harmless-schedule-test-tool-texts session)
                       '("Scheduled 1 at 2099-03-01T00:00:00Z"))))
      (let ((asked nil)
            (session (harmless-session-new
                      :cwd repo :id "plan-ask" :title "Keep" :model "m"
                      :provider
                      (harmless-schedule-make-fake
                       :name "fake" :host "none"
                       :script (harmless-schedule-test-tool-script
                                '(:tool-call "1" "harmless-tool-schedule"
                                  "{\"prompt\":\"plan-ok\",\"at\":\"2099-04-01T00:00:00Z\"}"))))))
        (setf (harmless-session-plan-mode session) t)
        (setq harmless-perm-ask-function
              (lambda (_session class item callback)
                (push (list class item) asked)
                (funcall callback 'allow)))
        (setq harmless-schedule-test-calls 0)
        (harmless-turn-run session "go")
        (should (harmless-session-plan-mode session))
        (should (equal asked '((edit "plan-ok"))))
        (should (equal (harmless-schedule-test-tool-texts session)
                       '("Scheduled 1 at 2099-04-01T00:00:00Z"))))
      (let ((session (harmless-session-new
                      :cwd repo :id "plan-always" :title "Keep" :model "m"
                      :permission-mode 'always-approve
                      :provider
                      (harmless-schedule-make-fake
                       :name "fake" :host "none"
                       :script (harmless-schedule-test-tool-script
                                '(:tool-call "1" "harmless-tool-schedule-cancel"
                                  "{\"id\":\"1\"}"))))))
        (should (equal (harmless-schedule-test-call
                        session "harmless-tool-schedule"
                        '(:prompt "stay" :at "2099-05-01T00:00:00Z"))
                       "Scheduled 1 at 2099-05-01T00:00:00Z"))
        (setf (harmless-session-plan-mode session) t)
        (setq harmless-perm-ask-function
              (lambda (&rest _) (error "permission asked")))
        (setq harmless-schedule-test-calls 0)
        (harmless-turn-run session "go")
        (should (equal (harmless-schedule-test-tool-texts session)
                       '("Cancelled schedule 1")))
        (should-not (harmless-session-schedules session))
        (should (harmless-session-plan-mode session)))
      (should (equal (harmless-schedule-test-files repo) files))
      (should (eq (file-exists-p "/schedule") slash))
      (should (eq (file-exists-p "c:/schedule") cdrive))
      (with-temp-buffer
        (insert-file-contents readme)
        (should (equal (buffer-string) "keep\n"))))))

(ert-deftest harmless-schedule-restores-on-resume ()
  (harmless-schedule-test-with _data
    (let* ((repo (make-temp-file "harmless-schedule-repo-" t))
           (session (harmless-schedule-test-session
                     :cwd repo :id "resume" :title "Keep"))
           (dir (harmless-session-dir session))
           (loaded nil))
      (should (equal (harmless-schedule-test-call
                      session "harmless-tool-schedule"
                      '(:prompt "keep-me" :at "2099-01-01T00:00:00Z"))
                     "Scheduled 1 at 2099-01-01T00:00:00Z"))
      (should (equal (harmless-schedule-test-call
                      session "harmless-tool-schedule"
                      '(:prompt "drop-me" :at "2099-02-01T00:00:00Z"))
                     "Scheduled 2 at 2099-02-01T00:00:00Z"))
      (plist-put (nth 1 (harmless-session-schedules session))
                 :at "2020-01-01T00:00:00Z")
      (harmless-session-save session)
      (should (cl-find "drop-me"
                       (plist-get (harmless-schedule-test-summary session)
                                  :schedules)
                       :test (lambda (wanted item)
                               (equal wanted (plist-get item :prompt)))))
      (setq harmless-schedule-test-armed nil)
      (setq harmless-schedule-test-calls 0)
      (let ((saved (symbol-function 'harmless-schedule-restore)))
        (unwind-protect
            (progn
              (fmakunbound 'harmless-schedule-restore)
              (should-not (fboundp 'harmless-schedule-restore))
              (setq loaded (harmless-session-load dir))
              (should (equal (harmless-schedule-test-prompts
                              (harmless-session-schedules loaded))
                             '("keep-me" "drop-me")))
              (should-not harmless-schedule-test-armed)
              (should (= 0 (harmless-schedule-test-wakes loaded)))
              (should (cl-find "drop-me"
                               (plist-get
                                (harmless-schedule-test-summary loaded)
                                :schedules)
                               :test (lambda (wanted item)
                                       (equal wanted
                                              (plist-get item :prompt))))))
          (fset 'harmless-schedule-restore saved)))
      (setq harmless-schedule-test-armed nil)
      (setq loaded (harmless-session-load dir))
      (should (eq (car (car harmless-schedule-test-armed)) loaded))
      (should (equal (mapcar #'cadr harmless-schedule-test-armed) '("1")))
      (should (equal (harmless-schedule-test-prompts
                      (harmless-session-schedules loaded))
                     '("keep-me")))
      (should-not (cl-find "drop-me"
                           (plist-get
                            (harmless-schedule-test-summary loaded)
                            :schedules)
                           :test (lambda (wanted item)
                                   (equal wanted (plist-get item :prompt)))))
      (should (cl-find "keep-me"
                       (plist-get (harmless-schedule-test-summary loaded)
                                  :schedules)
                       :test (lambda (wanted item)
                               (equal wanted (plist-get item :prompt)))))
      (should (null (harmless-schedule-test-user-texts loaded)))
      (should (= 0 (harmless-schedule-test-wakes loaded)))
      (setf (harmless-session-provider loaded)
            (harmless-schedule-make-fake
             :name "fake" :host "none"
             :script (harmless-schedule-test-wake-script)))
      (setq harmless-schedule-test-calls 0)
      (harmless-schedule-fire loaded "1")
      (should (= 1 (harmless-schedule-test-wakes loaded)))
      (should (equal (harmless-schedule-test-user-texts loaded) '("keep-me")))
      (should (equal (harmless-session-title loaded) "Keep"))
      (should-not (harmless-session-schedules loaded))
      (should (equal (harmless-schedule-test-call
                      loaded "harmless-tool-schedule"
                      '(:prompt "next" :at "2099-03-01T00:00:00Z"))
                     "Scheduled 1 at 2099-03-01T00:00:00Z"))
      (harmless-schedule-test-assert-session loaded)
      (should (equal (harmless-session-dir loaded) dir)))))

(ert-deftest harmless-schedule-subagents-and-detached ()
  (harmless-schedule-test-with _data
    (let* ((repo (make-temp-file "harmless-schedule-repo-" t))
           (files (harmless-schedule-test-files repo))
           (slash (file-exists-p "/schedule"))
           (cdrive (file-exists-p "c:/schedule"))
           (session (harmless-schedule-test-session
                     :cwd repo :id "parent" :title "Keep"))
           (child (harmless-schedule-test-session
                   :cwd repo :id "child" :title "Child"
                   :source 'agent
                   :parent-id (harmless-session-id session))))
      (setf (harmless-session-agent child)
            (list :allow 'all :sandbox 'read-only :prompt "p"))
      (let ((offered (mapcar #'harmless-tool-name
                             (harmless-tools-for-session child))))
        (should-not (member "harmless-tool-schedule" offered))
        (should-not (member "harmless-tool-schedule-cancel" offered)))
      (should (equal (harmless-agent-refuse-call
                      child (harmless-tool-by-name "harmless-tool-schedule")
                      '(:prompt "x" :delay 5))
                     harmless-agent-read-only-refusal))
      (should (equal (harmless-agent-refuse-call
                      child (harmless-tool-by-name "harmless-tool-schedule-cancel")
                      '(:id "1"))
                     harmless-agent-read-only-refusal))
      (setf (plist-get (harmless-session-agent child) :sandbox) 'workspace)
      (let ((offered (mapcar #'harmless-tool-name
                             (harmless-tools-for-session child))))
        (should (member "harmless-tool-schedule" offered))
        (should (member "harmless-tool-schedule-cancel" offered)))
      (should (equal (harmless-schedule-test-call
                      child "harmless-tool-schedule"
                      '(:prompt "from-child" :at "2099-01-01T00:00:00Z"))
                     "Scheduled 1 at 2099-01-01T00:00:00Z"))
      (setf (plist-get (harmless-session-agent child) :allow) '("harmless-tool-read-file"))
      (setf (plist-get (harmless-session-agent child) :sandbox) 'read-only)
      (should (equal (harmless-agent-refuse-call
                      child (harmless-tool-by-name "harmless-tool-schedule") nil)
                     "Error: this subagent cannot use harmless-tool-schedule"))
      (should (equal (harmless-agent-refuse-call
                      child (harmless-tool-by-name "harmless-tool-schedule-cancel") nil)
                     "Error: this subagent cannot use harmless-tool-schedule-cancel"))
      (let ((detached (harmless-schedule-test-session
                       :detached t :id "chat" :title "Keep")))
        (let ((names (mapcar #'harmless-tool-name
                             (harmless-tools-for-session detached))))
          (should (member "harmless-tool-schedule" names))
          (should (member "harmless-tool-schedule-cancel" names))
          (should-not (member "harmless-tool-monitor" names))
          (should-not (member "harmless-tool-read-file" names)))
        (should (equal (harmless-schedule-test-call
                        detached "harmless-tool-schedule"
                        '(:prompt "log.txt" :delay 4))
                       (format "Scheduled 1 at %s"
                               (plist-get
                                (car (harmless-session-schedules detached))
                                :at))))
        (should (equal (plist-get
                        (car (plist-get
                              (harmless-schedule-test-summary detached)
                              :schedules))
                        :prompt)
                       "log.txt"))
        (harmless-schedule-test-assert-session detached))
      (should (equal (harmless-schedule-test-files repo) files))
      (should (eq (file-exists-p "/schedule") slash))
      (should (eq (file-exists-p "c:/schedule") cdrive))
      (should-not (file-exists-p (expand-file-name "log.txt" repo)))
      (harmless-schedule-test-assert-session session)
      (harmless-schedule-test-assert-session child))))
