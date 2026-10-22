;;; harmless-skill-load-tests.el --- Tests for loading a skill -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(defun harmless-skill-load-test-write (root name description body
                                           &optional tools)
  "Write ROOT/.harmless/skills/NAME/SKILL.md and return that path.
TOOLS nil omits allowed-tools.  `empty' writes [].  `blank' writes
the key and no items.  A string is one scalar.  A list is a YAML list."
  (let* ((dir (expand-file-name
               (format ".harmless/skills/%s" name) root))
         (path (expand-file-name "SKILL.md" dir))
         (tools-text
          (cond
           ((null tools) "")
           ((eq tools 'empty) "allowed-tools: []\n")
           ((eq tools 'blank) "allowed-tools:\n")
           ((stringp tools) (format "allowed-tools: %s\n" tools))
           (t (concat "allowed-tools:\n"
                      (mapconcat (lambda (tool)
                                   (format "  - %s\n" tool))
                                 tools "")))))
         (coding-system-for-write 'utf-8-unix))
    (make-directory dir t)
    (with-temp-file path
      (insert (format "---\nname: %s\ndescription: %s\n%s---\n%s\n"
                      name description tools-text body)))
    path))

(defun harmless-skill-load-test-read (path)
  "Return PATH as a Unix-newline string."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (buffer-string)))

(defun harmless-skill-load-test-session (id cwd)
  "Return a session ID at CWD that skips agent catalogs."
  (harmless-session--create
   :id id
   :cwd cwd
   :model "grok-4.6"
   :source 'agent
   :messages nil
   :plan-mode nil))

(defun harmless-skill-load-test-system (session)
  "Return the system text SESSION would send."
  (or (harmless-messages-system-text (harmless-turn--messages session))
      ""))

(defun harmless-skill-load-test-names (session)
  "Return the tool names SESSION is offered."
  (mapcar #'harmless-tool-name (harmless-tools-for-session session)))

(defun harmless-skill-load-test-clear (session)
  "Drop SESSION's loaded skill, if any."
  (when (harmless-session-p session)
    (remhash (harmless-session-id session) harmless-skill-active)))

(ert-deftest harmless-skill-load-catalog-omits-the-body ()
  (let* ((base (make-temp-file "harmless-skill-load-" t))
         (root (expand-file-name "proj" base))
         (harmless-directory (expand-file-name ".harmless-state" base)))
    (unwind-protect
        (progn
          (harmless-skill-load-test-write
           root "notes" "Take notes." "HSL-NOTES-BODY")
          (let ((text (harmless-skills-catalog root root)))
            (should (string-match-p "harmless-tool-read-file" text))
            (should (string-match-p "skill tool" text))
            (should (string-match-p "/name" text))
            (should (string-match-p "Take notes." text))
            (should-not (string-match-p "HSL-NOTES-BODY" text))))
        (ignore-errors (delete-directory base t)))))

(ert-deftest harmless-skill-load-puts-the-body-on-later-turns ()
  (let* ((base (make-temp-file "harmless-skill-load-" t))
         (root (expand-file-name "proj" base))
         (child (expand-file-name "sub" root))
         (harmless-directory (expand-file-name ".harmless-state" base))
         (harmless-compact-threshold nil)
         (session (harmless-skill-load-test-session "skill-load-body" child))
         (other (harmless-skill-load-test-session "skill-load-other" child))
         (path nil))
    (unwind-protect
        (progn
          (harmless-skill-load-test-write
           root "commit" "From the parent." "HSL-PARENT-BODY")
          (setq path (harmless-skill-load-test-write
                      child "commit" "From the child." "HSL-CHILD-BODY"))
          (let ((skill (harmless-load-skill session "Commit")))
            (should (equal "commit" (plist-get skill :name)))
            (should (equal "HSL-CHILD-BODY" (plist-get skill :body)))
            (should-not (plist-get skill :restricted))
            (should (file-name-absolute-p (plist-get skill :path)))
            (should (file-equal-p path (plist-get skill :path)))
            (should (harmless-directory-strictly-under-p
                     (plist-get skill :path) root))
            (should-not (harmless-filesystem-root-p (plist-get skill :path)))
            (should (string-match-p "HSL-CHILD-BODY"
                                    (harmless-skill-load-test-read path))))
          (should (equal "commit"
                         (plist-get (harmless-skill-active session) :name)))
          (should-not (harmless-skill-active other))
          (should-not (file-exists-p harmless-directory))
          (let* ((sent (harmless-skill-load-test-system session))
                 (held (harmless-context-messages child nil child nil nil))
                 (held-text (or (harmless-messages-system-text held) "")))
            (should (string-match-p "harmless-tool-read-file" sent))
            (should (string-match-p "Loaded skill commit" sent))
            (should (< (string-match "Loaded skill commit" sent)
                       (string-match "HSL-CHILD-BODY" sent)))
            (should-not (string-match-p "HSL-PARENT-BODY" sent))
            (should (string-match-p "harmless-tool-read-file" held-text))
            (should-not (string-match-p "HSL-CHILD-BODY" held-text)))
          (should-not (string-match-p
                       "HSL-CHILD-BODY"
                       (harmless-skill-load-test-system other)))
          (harmless-skill-load-test-clear session)
          (setf (harmless-session-cwd session)
                (file-name-as-directory child))
          (should (equal "HSL-CHILD-BODY"
                         (plist-get (harmless-skill-load session "commit")
                                    :body)))
          (should (equal nil (harmless-session-messages session))))
      (harmless-skill-load-test-clear session)
      (harmless-skill-load-test-clear other)
      (ignore-errors (delete-directory base t)))))

(ert-deftest harmless-skill-load-allowed-tools-limits-the-turn ()
  (let* ((base (make-temp-file "harmless-skill-load-" t))
         (root (expand-file-name "proj" base))
         (harmless-directory (expand-file-name ".harmless-state" base))
         (session (harmless-skill-load-test-session "skill-load-tools" root)))
    (unwind-protect
        (progn
          (setf (harmless-session-source session) nil)
          (harmless-skill-load-test-write
           root "open" "All tools." "HSL-OPEN-BODY")
          (harmless-skill-load-test-write
           root "yaml" "A YAML list." "HSL-YAML-BODY"
           '("harmless-tool-read-file" "harmless-tool-grep"))
          (harmless-skill-load-test-write
           root "scalar" "A scalar list." "HSL-SCALAR-BODY"
           "harmless-tool-read-file, harmless-tool-grep")
          (harmless-skill-load-test-write
           root "bracket" "A bracket list." "HSL-BRACKET-BODY"
           "[\"harmless-tool-read-file\", \"harmless-tool-grep\"]")
          (harmless-skill-load-test-write
           root "none" "No tools." "HSL-NONE-BODY" 'empty)
          (harmless-skill-load-test-write
           root "blank" "An empty key." "HSL-BLANK-BODY" 'blank)
          (let ((coding-system-for-write 'utf-8-unix)
                (gap (expand-file-name ".harmless/skills/gap" root))
                (loose (expand-file-name ".harmless/skills/loose" root)))
            (make-directory gap t)
            (make-directory loose t)
            (with-temp-file (expand-file-name "SKILL.md" gap)
              (insert "---\nname: gap\ndescription: Gap.\nallowed-tools:\n  - harmless-tool-read-file\n\n  - not-a-tool\n---\nHSL-GAP-BODY\n"))
            (with-temp-file (expand-file-name "SKILL.md" loose)
              (insert "---\nname: loose\ndescription: Loose.\nAllowed-tools: harmless-tool-read-file\n---\nHSL-LOOSE-BODY\n")))
          (let ((skill (harmless-skill-load session "open")))
            (should-not (plist-get skill :restricted))
            (let ((names (harmless-skill-load-test-names session)))
              (should (member "harmless-tool-write-file" names))
              (should (member "harmless-tool-skill" names))
              (should (member "harmless-tool-grep" names))))
          (dolist (name '("yaml" "scalar" "bracket"))
            (let ((skill (harmless-skill-load session name)))
              (should (plist-get skill :restricted))
              (should (equal '("harmless-tool-read-file" "harmless-tool-grep") (plist-get skill :tools)))
              (let ((names (harmless-skill-load-test-names session)))
                (should (member "harmless-tool-read-file" names))
                (should (member "harmless-tool-grep" names))
                (should-not (member "harmless-tool-write-file" names))
                (should-not (member "harmless-tool-skill" names)))))
          (dolist (name '("none" "blank"))
            (let ((skill (harmless-skill-load session name)))
              (should (plist-get skill :restricted))
              (should-not (plist-get skill :tools))
              (should (null (harmless-skill-load-test-names session)))))
          (let ((skill (harmless-skill-load session "gap")))
            (should (equal '("harmless-tool-read-file") (plist-get skill :tools)))
            (let ((names (harmless-skill-load-test-names session)))
              (should (member "harmless-tool-read-file" names))
              (should-not (member "harmless-tool-write-file" names))
              (should-not (member "not-a-tool" names))))
          (harmless-skill-load session "loose")
          (should (member "harmless-tool-write-file"
                          (harmless-skill-load-test-names session))))
      (harmless-skill-load-test-clear session)
      (ignore-errors (delete-directory base t)))))

(ert-deftest harmless-skill-load-unknown-tool-keeps-the-previous ()
  (let* ((base (make-temp-file "harmless-skill-load-" t))
         (root (expand-file-name "proj" base))
         (harmless-directory (expand-file-name ".harmless-state" base))
         (session (harmless-skill-load-test-session "skill-load-unknown" root)))
    (unwind-protect
        (progn
          (harmless-skill-load-test-write
           root "open" "All tools." "HSL-OPEN-BODY")
          (harmless-skill-load-test-write
           root "bad" "Unknown tool." "HSL-BAD-BODY" "harmless-tool-read-file, NotATool")
          (harmless-skill-load-test-write
           root "cased" "Wrong case." "HSL-CASED-BODY" "Read_File")
          (harmless-skill-load session "open")
          (let ((err (should-error (harmless-skill-load session "bad")
                                   :type 'error)))
            (should (equal "skill tool is unknown: NotATool"
                           (error-message-string err))))
          (should (equal "open"
                         (plist-get (harmless-skill-active session) :name)))
          (should (equal "Error: skill tool is unknown: Read_File"
                         (harmless-tool-call (harmless-tool-by-name "harmless-tool-skill")
                                             session '(:name "cased"))))
          (should (equal "open"
                         (plist-get (harmless-skill-active session) :name)))
          (should (member "harmless-tool-write-file"
                          (harmless-skill-load-test-names session))))
      (harmless-skill-load-test-clear session)
      (ignore-errors (delete-directory base t)))))

(ert-deftest harmless-skill-load-refuses-a-bad-name ()
  (let* ((base (make-temp-file "harmless-skill-load-" t))
         (root (expand-file-name "proj" base))
         (harmless-directory (expand-file-name ".harmless-state" base))
         (session (harmless-skill-load-test-session "skill-load-name" root)))
    (unwind-protect
        (progn
          (harmless-skill-load-test-write
           root "notes" "Take notes." "HSL-NOTES-BODY")
          (harmless-skill-load-test-write
           root "c" "A short name." "HSL-C-BODY")
          (dolist (pair (list (cons nil "skill name is empty")
                              (cons "" "skill name is empty")
                              (cons "  " "skill name is empty")
                              (cons 'commit "skill name must be a string")
                              (cons "sub/" "skill name is not a name")
                              (cons "notes/" "skill name is not a name")
                              (cons "/" "skill name is not a name")
                              (cons "c:/" "skill name is not a name")
                              (cons "c:" "skill name is not a name")
                              (cons "~" "skill name is not a name")
                              (cons ".." "skill name is not a name")
                              (cons "\\notes" "skill name is not a name")))
            (let ((err (should-error
                        (harmless-skill-load session (car pair))
                        :type 'error)))
              (should (equal (cdr pair) (error-message-string err))))
            (should-not (harmless-skill-active session)))
          (should (equal "Error: skill name is empty"
                         (harmless-tool-call (harmless-tool-by-name "harmless-tool-skill")
                                             session nil)))
          (should (equal "Error: no skill named missing"
                         (harmless-tool-call (harmless-tool-by-name "harmless-tool-skill")
                                             session '(:name "missing"))))
          (let ((err (should-error (harmless-skill-load nil "notes")
                                   :type 'error)))
            (should (equal "No Harmless session" (error-message-string err))))
          (let ((err (should-error (harmless-load-skill 'nope "notes")
                                   :type 'error)))
            (should (equal "No Harmless session" (error-message-string err))))
          (should (equal "HSL-NOTES-BODY"
                         (plist-get (harmless-skill-load session "Notes")
                                    :body)))
          (harmless-skill-load-test-clear session)
          (should (equal "HSL-NOTES-BODY"
                         (plist-get (harmless-skill-load session " Notes ")
                                    :body)))
          (harmless-skill-load-test-clear session)
          (should (equal "c"
                         (plist-get (harmless-skill-load session "c") :name)))
          (let* ((path (plist-get (harmless-skill-active session) :path))
                 (prefix (file-name-as-directory (expand-file-name root))))
            (should (file-name-absolute-p path))
            (should (string-prefix-p prefix (expand-file-name path) t))
            (should (string-match-p "HSL-C-BODY"
                                    (harmless-skill-load-test-read path)))))
      (harmless-skill-load-test-clear session)
      (ignore-errors (delete-directory base t)))))

(ert-deftest harmless-skill-load-refuses-a-bad-directory ()
  (let* ((base (make-temp-file "harmless-skill-load-" t))
         (root (expand-file-name "proj" base))
         (harmless-directory (expand-file-name ".harmless-state" base))
         (session (harmless-skill-load-test-session "skill-load-dir" root))
         (before nil))
    (unwind-protect
        (progn
          (harmless-skill-load-test-write
           root "commit" "Commit." "HSL-COMMIT-BODY")
          (dolist (path '("/.claude" "/.harmless" "c:/.claude" "c:/.harmless"
                          "d:/.claude" "d:/.harmless"))
            (push (cons path (file-exists-p path)) before))
          (let ((default-directory (file-name-as-directory root)))
            (dolist (dir (list "notes" "sub/" "." "./" ".." "~" "~/proj"))
              (setf (harmless-session-cwd session) dir)
              (let ((err (should-error (harmless-skill-load session "commit")
                                       :type 'error)))
                (should (equal "skill directory must be absolute"
                               (error-message-string err))))
              (should-not (harmless-skill-active session))))
          (dolist (dir (list nil "" "   "))
            (setf (harmless-session-cwd session) dir)
            (let ((err (should-error (harmless-skill-load session "commit")
                                     :type 'error)))
              (should (equal "This chat is not attached to a project"
                             (error-message-string err))))
            (should-not (harmless-skill-active session)))
          (dolist (dir (list "/" "c:/" "d:/" "c:\\" "C:/" "c:" "\\" "//" "/."))
            (setf (harmless-session-cwd session) dir)
            (let ((err (should-error (harmless-skill-load session "commit")
                                     :type 'error)))
              (should (equal "skill directory is the filesystem root"
                             (error-message-string err))))
            (should-not (harmless-skill-active session)))
          (dolist (pair before)
            (should (eq (cdr pair) (file-exists-p (car pair)))))
          (should-not (member "harmless-tool-skill"
                              (harmless-skill-load-test-names
                               (harmless-skill-load-test-session
                                "skill-load-detached" nil)))))
      (harmless-skill-load-test-clear session)
      (ignore-errors (delete-directory base t)))))

(ert-deftest harmless-skill-load-slash-prompt-keeps-the-user-text ()
  (let* ((base (make-temp-file "harmless-skill-load-" t))
         (root (expand-file-name "proj" base))
         (harmless-directory (expand-file-name ".harmless-state" base))
         (harmless-compact-threshold nil)
         (session (harmless-skill-load-test-session "skill-load-slash" root)))
    (unwind-protect
        (progn
          (setf (harmless-session-messages session)
                '((:role :user :content "kept")))
          (let ((err (should-error (harmless-turn-run session "/missing")
                                   :type 'error)))
            (should (equal "no skill named missing"
                           (error-message-string err))))
          (should (equal '((:role :user :content "kept"))
                         (harmless-session-messages session)))
          (should-not (harmless-skill-active session))
          (should-not (file-exists-p harmless-directory))
          (should-not (harmless-skill-prompt session "/tmp/foo"))
          (should-not (harmless-skill-prompt session "/commit\nplease"))
          (should-not (harmless-skill-prompt session "/"))
          (should-not (harmless-skill-prompt session "please /commit"))
          (should-not (harmless-skill-prompt session nil))
          (should-not (harmless-skill-prompt session ""))
          (should-not (harmless-skill-active session))
          (harmless-skill-load-test-write
           root "commit" "Commit." "HSL-SLASH-BODY")
          (setf (harmless-session-messages session) nil)
          (cl-letf (((symbol-function 'harmless-turn--call)
                     (lambda (_session) 'stopped)))
            (harmless-turn-run session "/commit please"))
          (should (equal "commit"
                         (plist-get (harmless-skill-active session) :name)))
          (should (equal "/commit please"
                         (plist-get (car (harmless-session-messages session))
                                    :content)))
          (should-not (cdr (harmless-session-messages session)))
          (let* ((dir (harmless-session-directory session))
                 (file (expand-file-name "messages.jsonl" dir))
                 (summary (expand-file-name "summary.json" dir))
                 (text (harmless-skill-load-test-read file)))
            (should (file-name-absolute-p file))
            (should (harmless-directory-strictly-under-p
                     dir (harmless-data-directory)))
            (should-not (equal (file-truename dir) (file-truename "/")))
            (should (file-exists-p file))
            (should (file-exists-p summary))
            (should (string-match-p "/commit please" text))
            (should-not (string-match-p "HSL-SLASH-BODY" text))
            (should-not (string-match-p
                         "HSL-SLASH-BODY"
                         (harmless-skill-load-test-read summary))))
          (let ((text (harmless-skill-load-test-system session)))
            (should (string-match-p "Loaded skill commit" text))
            (should (string-match-p "HSL-SLASH-BODY" text))))
      (harmless-skill-load-test-clear session)
      (ignore-errors (delete-directory base t)))))

(ert-deftest harmless-skill-load-read-only-subagent-cannot-write ()
  (let* ((base (make-temp-file "harmless-skill-load-" t))
         (root (expand-file-name "proj" base))
         (harmless-directory (expand-file-name ".harmless-state" base))
         (session (harmless-skill-load-test-session "skill-load-reader" root)))
    (unwind-protect
        (progn
          (harmless-skill-load-test-write
           root "wide" "Reads and writes." "HSL-WIDE-BODY"
           '("harmless-tool-write-file" "harmless-tool-read-file"))
          (setf (harmless-session-agent session)
                (list :name "reader" :sandbox 'read-only :allow 'all))
          (harmless-skill-load session "wide")
          (let ((names (harmless-skill-load-test-names session)))
            (should (member "harmless-tool-read-file" names))
            (should-not (member "harmless-tool-write-file" names))
            (should-not (member "harmless-tool-notebook-edit" names))
            (should-not (member "harmless-tool-skill" names))))
      (harmless-skill-load-test-clear session)
      (ignore-errors (delete-directory base t)))))

(ert-deftest harmless-skill-load-tool-is-a-read-and-not-a-plan-refusal ()
  (let* ((base (make-temp-file "harmless-skill-load-" t))
         (root (expand-file-name "proj" base))
         (harmless-directory (expand-file-name ".harmless-state" base))
         (session (harmless-skill-load-test-session "skill-load-plan" root))
         (tool (harmless-tool-by-name "harmless-tool-skill")))
    (unwind-protect
        (progn
          (harmless-skill-load-test-write
           root "notes" "Take notes." "HSL-NOTES-BODY")
          (should (eq 'read (harmless-tool-class tool)))
          (should-not (harmless-perm-needed-p session 'read))
          (should (equal "notes" (harmless-ui--tool-summary
                                  "harmless-tool-skill" '(:name "notes"))))
          (should (equal "" (harmless-ui--tool-summary "harmless-tool-skill" nil)))
          (should (equal "" (harmless-ui--tool-summary "harmless-tool-skill" '(:name nil))))
          (setf (harmless-session-plan-mode session) t)
          (should (equal "Loaded skill notes\n\nHSL-NOTES-BODY"
                         (harmless-tool-call tool session '(:name "notes"))))
          (should-not (harmless-agent-plan-refuses-p session tool '(:name "notes")))
          (should-not (harmless-workflow-plan-refuses-p
                       session tool '(:name "notes")))
          (should-not (harmless-worktree-plan-refuses-p
                       session tool '(:name "notes")))
          (should-not (harmless-computer-plan-refuses-p
                       session tool '(:name "notes")))
          (should-not (harmless-monitor-plan-refuses-p
                       session tool '(:name "notes")))
          (let ((text (harmless-skill-load-test-system session)))
            (should (string-match-p "HSL-NOTES-BODY" text))
            (should (string-match-p "Plan mode is active" text))
            (should (< (string-match "HSL-NOTES-BODY" text)
                       (string-match "Plan mode is active" text)))))
      (harmless-skill-load-test-clear session)
      (ignore-errors (delete-directory base t)))))
