;;; harmless-lsp-tests.el --- Tests for the language server tool -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'xref)
(require 'flymake)
(require 'eglot)
(require 'harmless)

(cl-defstruct (harmless-lsp-fake
               (:include harmless-provider)
               (:constructor harmless-lsp-make-fake))
  script)

(cl-defmethod harmless-provider-complete ((provider harmless-lsp-fake)
                                          _messages _tools callback)
  (when (harmless-lsp-fake-script provider)
    (funcall (harmless-lsp-fake-script provider) _messages callback))
  nil)

(defvar harmless-lsp-test-calls 0
  "Provider calls in the current lsp test.")

(defun harmless-lsp-test-error (message form)
  "Assert FORM signals MESSAGE."
  (let ((err (should-error (funcall form) :type 'error)))
    (should (equal message (error-message-string err)))))

(defun harmless-lsp-test-text (path)
  "Return the contents of PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun harmless-lsp-test-write (path text)
  "Write TEXT to PATH with Unix newlines."
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file path
      (insert text))))

(defmacro harmless-lsp-test-env (&rest body)
  "Evaluate BODY with a project, a data directory, and a session."
  (declare (indent 0))
  `(let* ((base (make-temp-file "harmless-lsp-" t))
          (dir (expand-file-name "proj" base))
          (data (expand-file-name "data" base))
          (provider (harmless-make-openai-compat
                     "local" :host "127.0.0.1:9" :protocol "http"
                     :key "none" :models '("m")))
          (harmless-directory data)
          (harmless--sessions (make-hash-table :test 'equal))
          (harmless-providers nil)
          (harmless-compact-threshold nil)
          (harmless-lsp-query-function nil)
          (session nil))
     (make-directory dir t)
     (make-directory data t)
     (setq harmless-providers (list provider))
     (setq session (harmless-session-new
                    :cwd dir :provider provider :model "m"
                    :title "LSP"))
     (unwind-protect
         (progn ,@body)
       (dolist (buf (buffer-list))
         (let ((file (buffer-file-name buf)))
           (when (and file
                      (string-prefix-p
                       (file-name-as-directory (expand-file-name dir))
                       (expand-file-name file)))
             (kill-buffer buf)))))))

(defun harmless-lsp-test-tool-script (event)
  "Return a provider script that sends EVENT once, then text."
  (lambda (_messages callback)
    (setq harmless-lsp-test-calls (1+ harmless-lsp-test-calls))
    (if (= harmless-lsp-test-calls 1)
        (progn
          (funcall callback event)
          (funcall callback '(:stop "tool_calls")))
      (funcall callback '(:text "done"))
      (funcall callback '(:stop "stop")))))

(defun harmless-lsp-test-tool-texts (session)
  "Return tool results stored on SESSION."
  (cl-loop for msg in (harmless-session-messages session)
           when (eq (plist-get msg :role) :tool)
           collect (plist-get msg :content)))

(defun harmless-lsp-test-refuse (session args message)
  "Assert ARGS on SESSION signals MESSAGE."
  (harmless-lsp-test-error
   message
   (lambda () (harmless-lsp-run session args))))

(ert-deftest harmless-lsp-refuses-bad-input ()
  (harmless-lsp-test-env
    (let ((calls 0)
          (queries 0)
          (harmless-lsp-query-function
           (lambda (&rest _)
             (setq queries (1+ queries))
             "x"))
          (chat (harmless-session-new
                 :detached t :provider provider :model "m"
                 :title "Chat")))
      (cl-letf (((symbol-function 'expand-file-name)
                 (lambda (&rest _)
                   (setq calls (1+ calls))
                   "/"))
                ((symbol-function 'file-truename)
                 (lambda (&rest _)
                   (setq calls (1+ calls))
                   "/"))
                ((symbol-function 'write-region)
                 (lambda (&rest _)
                   (setq calls (1+ calls))))
                ((symbol-function 'find-file-noselect)
                 (lambda (&rest _)
                   (setq calls (1+ calls))
                   (error "opened")))
                ((symbol-function 'eglot-ensure)
                 (lambda (&rest _)
                   (setq calls (1+ calls)))))
        (dolist (args (list nil
                            '(:path "lib.el")
                            '(:operation nil :path "lib.el")
                            '(:operation "" :path "lib.el")
                            '(:operation "   " :path "lib.el")))
          (harmless-lsp-test-refuse session args "lsp operation is empty"))
        (dolist (args (list '(:operation :false :path "lib.el")
                            '(:operation 12 :path "lib.el")
                            '(:operation ("definition") :path "lib.el")))
          (harmless-lsp-test-refuse
           session args "lsp operation must be a string"))
        (dolist (args (list '(:operation "HIGH" :path "/")
                            '(:operation "hover" :path "/")
                            '(:operation "info" :path "/")
                            '(:operation :nope :path "/")))
          (harmless-lsp-test-refuse
           session args "lsp operation is not an operation"))
        (dolist (args (list '(:operation "diagnostics")
                            '(:operation "diagnostics" :path nil)
                            '(:operation "diagnostics" :path "")
                            '(:operation "diagnostics" :path "   ")))
          (harmless-lsp-test-refuse session args "lsp path is empty"))
        (dolist (args (list '(:operation "diagnostics" :path :false)
                            '(:operation "diagnostics" :path 12)
                            '(:operation "diagnostics" :path t)))
          (harmless-lsp-test-refuse
           session args "lsp path must be a string"))
        (dolist (path '("/" "//" "/." "c:" "c:/" "c:\\" "d:/" "C:/"
                        "\\" "  /  " "  c:/  " "d:\\"))
          (harmless-lsp-test-refuse
           session
           (list :operation "diagnostics" :path path)
           "lsp path is the filesystem root"))
        (harmless-lsp-test-refuse
         session '(:operation :definition :path "/")
         "lsp path is the filesystem root")
        (harmless-lsp-test-refuse
         session '(:operation "diagnostics" :path "lib.el" :line 1)
         "lsp diagnostics takes no position")
        (harmless-lsp-test-refuse
         session '(:operation "diagnostics" :path "lib.el" :column 1)
         "lsp diagnostics takes no position")
        (harmless-lsp-test-refuse
         session '(:operation "diagnostics" :path "lib.el" :line nil)
         "lsp diagnostics takes no position")
        (dolist (args (list '(:operation "definition" :path "lib.el")
                            '(:operation "definition" :path "lib.el"
                              :line nil :column 1)
                            '(:operation "references" :path "notes"
                              :column 1)))
          (harmless-lsp-test-refuse session args "lsp line is empty"))
        (dolist (line (list 0 -1 1.5 "1" t :false))
          (harmless-lsp-test-refuse
           session
           (list :operation "definition" :path "lib.el"
                 :line line :column 1)
           "lsp line is not a line"))
        (harmless-lsp-test-refuse
         session '(:operation "definition" :path "lib.el" :line 1)
         "lsp column is empty")
        (harmless-lsp-test-refuse
         session '(:operation "references" :path "lib.el"
                   :line 1 :column nil)
         "lsp column is empty")
        (dolist (column (list 0 -1 1.2 "2" t :false))
          (harmless-lsp-test-refuse
           session
           (list :operation "definition" :path "lib.el"
                 :line 1 :column column)
           "lsp column is not a column"))
        (harmless-lsp-test-refuse
         chat '(:operation "diagnostics" :path "/")
         "lsp path is the filesystem root")
        (harmless-lsp-test-refuse
         chat '(:operation "diagnostics" :path "notes" :line 1)
         "lsp diagnostics takes no position")
        (harmless-lsp-test-refuse
         chat '(:operation "definition" :path "notes")
         "lsp line is empty")
        (harmless-lsp-test-error
         "No Harmless session"
         (lambda () (harmless-lsp-run "x" nil)))
        (harmless-lsp-test-error
         "No Harmless session"
         (lambda () (harmless-lsp-run nil nil)))
        (harmless-lsp-test-error
         "No Harmless session"
         (lambda () (harmless-lsp "x" nil)))
        (harmless-lsp-test-error
         "No Harmless session"
         (lambda () (harmless-lsp nil nil)))
        (should (equal (harmless-tool-call
                        (harmless-tool-by-name "lsp")
                        session '(:operation "hover"))
                       "Error: lsp operation is not an operation"))
        (should (= calls 0))
        (should (= queries 0)))
      (harmless-lsp-test-refuse
       session '(:operation "diagnostics" :path "no-such.txt")
       "lsp path is not a file")
      (should-not (file-exists-p (expand-file-name "no-such.txt" dir)))
      (harmless-lsp-test-refuse
       session '(:operation "diagnostics" :path "sub/")
       "lsp path is not a file")
      (should-not (file-exists-p (expand-file-name "sub" dir)))
      (should-not (file-directory-p (expand-file-name "sub" dir)))
      (harmless-lsp-test-refuse
       session (list :operation "diagnostics" :path dir)
       "lsp path is not a file")
      (harmless-lsp-test-refuse
       chat '(:operation "definition" :path "notes" :line 1 :column 1)
       "This chat is not attached to a project")
      (should-not (file-exists-p (expand-file-name "notes" dir)))
      (harmless-lsp-test-refuse
       session '(:operation "diagnostics" :path "~")
       "Path escapes project: ~")
      (let ((outside (expand-file-name "outside.txt" base)))
        (harmless-lsp-test-refuse
         session (list :operation "diagnostics" :path outside)
         (format "Path escapes project: %s" outside))
        (should-not (file-exists-p outside)))
      (let ((file (expand-file-name "lib.el" dir))
            (text "ab\nxyz\n"))
        (harmless-lsp-test-write file text)
        (harmless-lsp-test-refuse
         session '(:operation "diagnostics" :path "lib.el/")
         "lsp path is not a file")
        (should (equal (harmless-lsp-test-text file) text))
        (should-not (file-directory-p (expand-file-name "lib.el" dir))))
      (should (= queries 0))
      (should (harmless-directory-strictly-under-p
               (harmless-session-directory session) data))
      (should-not (harmless-filesystem-root-p
                   (harmless-session-directory session)))
      (should-not (member "lsp"
                          (mapcar #'harmless-tool-name
                                  (harmless-tools-for-session chat)))))))

(ert-deftest harmless-lsp-reads-definition-references-and-diagnostics ()
  (with-temp-buffer
    (insert "ab\nxyz\n")
    (harmless-lsp--goto 1 1)
    (should (eq (char-after) ?a))
    (harmless-lsp--goto 2 3)
    (should (eq (char-after) ?z))
    (harmless-lsp--goto 2 99)
    (should (= (point) (line-end-position)))
    (should (eq (char-before) ?z)))
  (should (equal (harmless-lsp--format-xrefs
                  nil
                  (list (xref-make
                         "b" (xref-make-file-location "b.el" 2 1))
                        (xref-make
                         "a" (xref-make-file-location "a.el" 3 0)))
                  "No definition")
                 "a.el:3:1\nb.el:2:2"))
  (should (equal (harmless-lsp--format-xrefs nil nil "No references")
                 "No references"))
  (should (equal (harmless-lsp--format-diagnostics
                  '((3 1 warning "unused") (1 2 error "bad")))
                 "1:2: error: bad\n3:1: warning: unused"))
  (should (equal (harmless-lsp--format-diagnostics nil)
                 "No diagnostics"))
  (should (eq (harmless-lsp--severity :warning) 'warning))
  (should (eq (harmless-lsp--severity :information) 'note))
  (should (eq (harmless-lsp--severity :foo) 'foo))
  (harmless-lsp-test-env
    (let* ((file (expand-file-name "lib.el" dir))
           (text "ab\nxyz\n")
           (seen nil)
           (buf nil))
      (harmless-lsp-test-write file text)
      (let ((harmless-lsp-query-function
             (lambda (_session op path line column)
               (setq seen (list op path line column))
               "from-query"))
            (called 0)
            (writes 0))
        (cl-letf (((symbol-function 'xref-backend-definitions)
                   (lambda (&rest _)
                     (setq called (1+ called))
                     nil))
                  ((symbol-function 'eglot-ensure)
                   (lambda (&rest _)
                     (setq called (1+ called))))
                  ((symbol-function 'find-file-noselect)
                   (lambda (&rest _)
                     (setq called (1+ called))
                     (error "opened")))
                  ((symbol-function 'write-region)
                   (lambda (&rest _)
                     (setq writes (1+ writes))))
                  ((symbol-function 'harmless-turn-run)
                   (lambda (&rest _)
                     (error "turn started"))))
          (should (equal (harmless-lsp-run
                          session
                          '(:operation "definition"
                            :path "lib.el" :line 2 :column 3))
                         "from-query")))
        (should (= called 0))
        (should (= writes 0))
        (should (eq (nth 0 seen) 'definition))
        (should (file-name-absolute-p (nth 1 seen)))
        (should-not (harmless-filesystem-root-p (nth 1 seen)))
        (should (file-equal-p (nth 1 seen) file))
        (should (string-prefix-p
                 (file-name-as-directory (expand-file-name dir))
                 (expand-file-name (nth 1 seen))))
        (should (equal (nth 2 seen) 2))
        (should (equal (nth 3 seen) 3))
        (should (equal (harmless-lsp-test-text file) text)))
      (setf (harmless-session-cwd session) (file-name-as-directory dir))
      (let ((harmless-lsp-query-function
             (lambda (_session op path line column)
               (setq seen (list op path line column))
               "slash")))
        (should (equal (harmless-lsp-run
                        session
                        '("operation" "references"
                          "path" "lib.el"
                          "line" 2 "column" 3))
                       "slash"))
        (should (eq (nth 0 seen) 'references))
        (should (file-equal-p (nth 1 seen) file))
        (should (equal (nth 2 seen) 2))
        (should (equal (nth 3 seen) 3)))
      (setf (harmless-session-cwd session) dir)
      (let ((harmless-lsp-query-function
             (lambda (&rest _) nil)))
        (harmless-lsp-test-error
         "lsp query returned nothing"
         (lambda ()
           (harmless-lsp-run
            session
            '(:operation :references :path "lib.el" :line 1 :column 1)))))
      (let ((harmless-lsp-query-function
             (lambda (&rest _) 12)))
        (harmless-lsp-test-error
         "lsp query must be a string"
         (lambda ()
           (harmless-lsp-run
            session
            '(:operation "references" :path "lib.el"
              :line 1 :column 1)))))
      (let ((extra (expand-file-name "extra" base))
            (outside nil))
        (make-directory extra)
        (setq outside (expand-file-name "out.el" extra))
        (harmless-lsp-test-write outside "z\n")
        (harmless-extra-add session extra)
        (let ((harmless-lsp-query-function
               (lambda (_session _op path _line _column)
                 (setq seen path)
                 "extra")))
          (should (equal (harmless-lsp-run
                          session
                          (list :operation "diagnostics" :path outside))
                         "extra"))
          (should (file-name-absolute-p seen))
          (should-not (harmless-filesystem-root-p seen))
          (should (file-equal-p seen outside))
          (should (string-prefix-p
                   (file-name-as-directory extra)
                   (expand-file-name seen)))
          (should (equal (harmless-lsp-test-text outside) "z\n"))))
      (setq buf (find-file-noselect file))
      (unwind-protect
          (let ((started 0)
                (point-at nil))
            (with-current-buffer buf
              (fundamental-mode)
              (goto-char (point-min))
              (set-buffer-modified-p nil))
            (cl-letf (((symbol-function 'eglot-ensure)
                       (lambda (&rest _)
                         (setq started (1+ started)))))
              (harmless-lsp-test-error
               "lsp server is not running"
               (lambda ()
                 (harmless-lsp-run
                  session
                  '(:operation "diagnostics" :path "lib.el")))))
            (should (= started 0))
            (cl-letf (((symbol-function 'eglot-ensure)
                       (lambda (&rest _)
                         (setq started (1+ started))))
                      ((symbol-function 'eglot-current-server)
                       (lambda () 'fake))
                      ((symbol-function 'xref-backend-definitions)
                       (lambda (_backend _id)
                         (setq point-at
                               (list (line-number-at-pos)
                                     (1+ (current-column))))
                         (list
                          (xref-make
                           "name"
                           (xref-make-file-location file 4 0))
                          (xref-make
                           "other"
                           (xref-make-file-location
                            (expand-file-name "other.el" dir) 1 3)))))
                      ((symbol-function 'xref-backend-references)
                       (lambda (&rest _) nil))
                      ((symbol-function 'flymake-diagnostics)
                       (lambda (&rest _)
                         (with-current-buffer buf
                           (save-excursion
                             (goto-char (point-min))
                             (list
                              (flymake-make-diagnostic
                               buf (point) (1+ (point)) :error "bad")
                              (progn
                                (forward-line 1)
                                (forward-char 1)
                                (flymake-make-diagnostic
                                 buf (point) (1+ (point))
                                 :warning "un\nused"))))))))
              (should (equal
                       (harmless-lsp-run
                        session
                        '(:operation "definition" :path "lib.el"
                          :line 2 :column 3))
                       "lib.el:4:1\nother.el:1:4"))
              (should (equal point-at '(2 3)))
              (with-current-buffer buf
                (should (= (point) (point-min))))
              (should (equal
                       (harmless-lsp-run
                        session
                        '(:operation "references" :path "lib.el"
                          :line 1 :column 1))
                       "No references"))
              (should (equal
                       (harmless-lsp-run
                        session
                        '(:operation "diagnostics" :path "lib.el"))
                       "1:1: error: bad\n2:2: warning: un used"))
              (should (= started 0))
              (should (equal (harmless-lsp-test-text file) text))
              (with-current-buffer buf
                (should-not (buffer-modified-p))))
            (cl-letf (((symbol-function 'eglot-current-server)
                       (lambda () 'fake))
                      ((symbol-function 'xref-backend-definitions)
                       (lambda (&rest _)
                         (error "No current JSON-RPC connection"))))
              (harmless-lsp-test-error
               "lsp server is not running"
               (lambda ()
                 (harmless-lsp-run
                  session
                  '(:operation "definition" :path "lib.el"
                    :line 1 :column 1)))))
            (cl-letf (((symbol-function 'eglot-current-server)
                       (lambda () 'fake))
                      ((symbol-function 'xref-backend-definitions)
                       (lambda (&rest _) (error "boom"))))
              (harmless-lsp-test-error
               "lsp query failed"
               (lambda ()
                 (harmless-lsp-run
                  session
                  '(:operation "definition" :path "lib.el"
                    :line 1 :column 1))))))
        (when (buffer-live-p buf)
          (kill-buffer buf)))
      (let ((opened 0)
            (started 0))
        (cl-letf (((symbol-function 'find-file-noselect)
                   (lambda (&rest _)
                     (setq opened (1+ opened))
                     (error "opened")))
                  ((symbol-function 'eglot-ensure)
                   (lambda (&rest _)
                     (setq started (1+ started)))))
          (harmless-lsp-test-error
           "lsp server is not running"
           (lambda ()
             (harmless-lsp-run
              session
              '(:operation "diagnostics" :path "lib.el")))))
        (should (= opened 0))
        (should (= started 0))
        (should (equal (harmless-lsp-test-text file) text)))
      (let* ((created (expand-file-name "new.el" dir))
             (body "one\n")
             (wrote (harmless-tools-fs--write
                     session (list :path "new.el" :contents body)))
             (before nil))
        (should (equal wrote
                       (format "Wrote %s (%d bytes)"
                               created (string-bytes body))))
        (setq before (harmless-lsp-test-text created))
        (let ((harmless-lsp-query-function
               (lambda (_session op _path line column)
                 (setq seen (list op line column))
                 "1:1: warning: unused")))
          (should (equal (harmless-lsp-run
                          session
                          '(:operation "diagnostics" :path "new.el"))
                         "1:1: warning: unused")))
        (should (equal (harmless-lsp-test-text created) before))
        (should (equal wrote
                       (format "Wrote %s (%d bytes)"
                               created (string-bytes body))))
        (should (eq (nth 0 seen) 'diagnostics))
        (should (null (nth 1 seen)))
        (should (null (nth 2 seen))))
      (setf (harmless-session-plan-mode session) t)
      (let ((harmless-lsp-query-function
             (lambda (&rest _) "No diagnostics")))
        (should (equal (harmless-lsp-run
                        session
                        '(:operation "diagnostics" :path "lib.el"))
                       "No diagnostics"))
        (should (harmless-session-plan-mode session))
        (should (equal (harmless-lsp-test-text file) text)))
      (should (eq (harmless-tool-class (harmless-tool-by-name "lsp"))
                  'read))
      (should-not (harmless-perm-needed-p session 'read))
      (should (equal (harmless-ui--tool-verb "lsp") "lsp"))
      (should (equal (harmless-ui--tool-summary
                      "lsp"
                      '(:operation "definition" :path "lib.el"))
                     "definition lib.el"))
      (should (equal (harmless-ui--tool-summary
                      "lsp"
                      '(:operation :references :path "lib.el"))
                     "references lib.el"))
      (should (equal (harmless-ui--tool-summary "lsp" nil) ""))
      (let ((agent (harmless-agent-create
                    session '(:prompt "look") #'ignore)))
        (should (file-name-absolute-p (harmless-session-directory agent)))
        (should-not (harmless-filesystem-root-p
                     (harmless-session-directory agent)))
        (should (harmless-directory-strictly-under-p
                 (harmless-session-directory agent) data))
        (setf (plist-get (harmless-session-agent agent) :sandbox)
              'read-only)
        (should (member "lsp"
                        (mapcar #'harmless-tool-name
                                (harmless-tools-for-session agent))))
        (setf (plist-get (harmless-session-agent agent) :allow)
              '("read_file"))
        (should (equal (harmless-agent-refuse-call
                        agent (harmless-tool-by-name "lsp") nil)
                       "Error: this subagent cannot use lsp")))
      (should (member "lsp"
                      (mapcar #'harmless-tool-name
                              (harmless-tools-for-session session))))
      (should (equal (harmless-lsp-test-text file) text))
      (should-not (file-exists-p (expand-file-name "other.el" dir)))
      (should-not (file-exists-p (expand-file-name "notes" dir)))
      (should-not (file-exists-p (expand-file-name "sub" dir))))))

(ert-deftest harmless-lsp-ask-and-plan-do-not-block ()
  (harmless-lsp-test-env
    (let ((file (expand-file-name "lib.el" dir))
          (text "ab\nxyz\n")
          (names nil)
          (harmless-lsp-query-function
           (lambda (_session op _path line column)
             (format "%s %s %s" op line column)))
          (harmless-lsp-test-calls 0))
      (harmless-lsp-test-write file text)
      (setq names (directory-files dir))
      (let ((session (harmless-session-new
                      :cwd dir :title "Ask" :model "m"
                      :provider
                      (harmless-lsp-make-fake
                       :name "fake" :host "none"
                       :script
                       (harmless-lsp-test-tool-script
                        '(:tool-call "1" "lsp"
                          "{\"operation\":\"definition\",\"path\":\"lib.el\",\"line\":2,\"column\":3}"))))))
        (setq harmless-perm-ask-function
              (lambda (&rest _) (error "permission asked")))
        (setq harmless-lsp-test-calls 0)
        (harmless-turn-run session "go")
        (should (equal (harmless-lsp-test-tool-texts session)
                       '("definition 2 3")))
        (should-not (harmless-session-plan-mode session)))
      (let ((session (harmless-session-new
                      :cwd dir :title "Plan" :model "m"
                      :provider
                      (harmless-lsp-make-fake
                       :name "fake" :host "none"
                       :script
                       (harmless-lsp-test-tool-script
                        '(:tool-call "1" "lsp"
                          "{\"operation\":\"diagnostics\",\"path\":\"lib.el\"}"))))))
        (setf (harmless-session-plan-mode session) t)
        (setq harmless-perm-ask-function
              (lambda (&rest _) (error "permission asked")))
        (setq harmless-lsp-test-calls 0)
        (harmless-turn-run session "go")
        (should (harmless-session-plan-mode session))
        (should (equal (harmless-lsp-test-tool-texts session)
                       '("diagnostics nil nil")))
        (should-not (file-exists-p (expand-file-name "plan.md" dir))))
      (should (equal (harmless-lsp-test-text file) text))
      (should (equal names (directory-files dir)))
      (should (harmless-directory-strictly-under-p
               (harmless-session-directory session) data)))))
