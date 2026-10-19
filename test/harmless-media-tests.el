;;; harmless-media-tests.el --- Tests for images, PDFs, and notebooks -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless)

(cl-defstruct (harmless-media-fake
               (:include harmless-provider)
               (:constructor harmless-media-make-fake))
  script)

(cl-defmethod harmless-provider-complete ((provider harmless-media-fake)
                                          messages _tools callback)
  (when (harmless-media-fake-script provider)
    (funcall (harmless-media-fake-script provider) messages callback))
  nil)

(defconst harmless-media-test-png
  (unibyte-string
   #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a
   #x00 #x00 #x00 #x0d)
  "PNG signature plus a few trailing bytes.")

(defconst harmless-media-test-jpeg
  (unibyte-string #xff #xd8 #xff #x00)
  "JPEG signature plus one trailing byte.")

(defconst harmless-media-test-gif
  (unibyte-string #x47 #x49 #x46 #x38 #x39 #x61)
  "GIF89a signature.")

(defconst harmless-media-test-webp
  (unibyte-string
   #x52 #x49 #x46 #x46 #x00 #x00 #x00 #x00
   #x57 #x45 #x42 #x50)
  "RIFF/WEBP signature.")

(defun harmless-media-test-error (message form)
  "Assert FORM signals MESSAGE."
  (let ((err (should-error (funcall form) :type 'error)))
    (should (equal message (error-message-string err)))))

(defun harmless-media-test-raw (path)
  "Return the raw bytes of PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun harmless-media-test-bytes (path bytes)
  "Write raw BYTES to PATH.  PATH is not a directory."
  (let ((coding-system-for-write 'no-conversion))
    (with-temp-file path
      (set-buffer-multibyte nil)
      (insert bytes))))

(defun harmless-media-test-write (path text)
  "Write TEXT to PATH with Unix newlines."
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file path
      (insert text))))

(defun harmless-media-test-text (path)
  "Return the text of PATH."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path)
      (buffer-string))))

(defun harmless-media-test-decode (path)
  "Return the notebook plist at PATH."
  (harmless-json-decode (harmless-media-test-text path)))

(defmacro harmless-media-test-env (&rest body)
  "Evaluate BODY with a project, a data directory, and a session."
  (declare (indent 0))
  `(let* ((base (make-temp-file "harmless-media-" t))
          (dir (expand-file-name "proj" base))
          (data (expand-file-name "data" base))
          (provider (harmless-make-openai-compat
                     "local" :host "127.0.0.1:9" :protocol "http"
                     :key "none" :models '("m")))
          (harmless-directory data)
          (harmless--sessions (make-hash-table :test 'equal))
          (harmless-providers nil)
          (harmless-compact-threshold nil)
          (session nil))
     (clrhash harmless-media-attachments)
     (make-directory dir t)
     (make-directory data t)
     (setq harmless-providers (list provider))
     (setq session (harmless-session-new
                    :cwd dir :provider provider :model "m"
                    :title "Media"))
     (unwind-protect
         (progn ,@body)
       (clrhash harmless-media-attachments))))

(defun harmless-media-test-under (path root)
  "Assert PATH is an absolute file strictly under ROOT."
  (should (file-name-absolute-p path))
  (should-not (harmless-filesystem-root-p path))
  (should-not (harmless-extra-root-text-p path))
  (should (string-prefix-p
           (file-name-as-directory (expand-file-name root))
           (expand-file-name path))))

(defun harmless-media-test-refuse-expand (form message)
  "Assert FORM signals MESSAGE and does not expand or write."
  (let ((calls 0))
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
                 (setq calls (1+ calls)))))
      (harmless-media-test-error message form))
    (should (= calls 0))))

(ert-deftest harmless-media-reads-images-pdfs-and-notebooks ()
  (harmless-media-test-env
    (let* ((text-file (expand-file-name "a.txt" dir))
           (png (expand-file-name "a.png" dir))
           (fake-png (expand-file-name "hello.png" dir))
           (jpeg (expand-file-name "a.jpg" dir))
           (gif (expand-file-name "a.gif" dir))
           (webp (expand-file-name "a.webp" dir))
           (pdf (expand-file-name "a.pdf" dir))
           (bad-pdf (expand-file-name "bad.pdf" dir))
           (book (expand-file-name "book.ipynb" dir))
           (empty-book (expand-file-name "empty.ipynb" dir))
           (slash (expand-file-name "a.png/" dir)))
      (harmless-media-test-write text-file "abc\ndef\n")
      (harmless-media-test-bytes png harmless-media-test-png)
      (harmless-media-test-write fake-png "hello")
      (harmless-media-test-bytes jpeg harmless-media-test-jpeg)
      (harmless-media-test-bytes gif harmless-media-test-gif)
      (harmless-media-test-bytes webp harmless-media-test-webp)
      (harmless-media-test-write
       pdf "%PDF-1.1\n(Hello PDF) Tj\n( ) Tj\n%%EOF\n")
      (harmless-media-test-write bad-pdf "hello")
      (harmless-media-test-write
       book
       (concat "{\"nbformat\":4,\"cells\":["
               "{\"cell_type\":\"code\",\"id\":\"a\","
               "\"source\":[\"print(1)\\n\"],"
               "\"outputs\":[{\"text\":\"1\"}]},"
               "{\"cell_type\":\"markdown\",\"id\":\"b\","
               "\"source\":[\"hello\"]}]}\n"))
      (harmless-media-test-write empty-book "{\"cells\":[]}\n")
      (should (equal (harmless-tools-fs--read
                      session '(:path "a.txt" :offset 2 :limit 1))
                     "def\n"))
      (let ((image (harmless-tools-fs--read
                    session '(:path "a.png" :offset 9 :limit 1)))
            (parts nil))
        (setq parts (harmless-media-image-parts image))
        (should (equal (nth 0 parts) "image/png"))
        (harmless-media-test-under (nth 1 parts) dir)
        (should (file-equal-p (nth 1 parts) png))
        (should (equal (nth 2 parts)
                       (base64-encode-string harmless-media-test-png t)))
        (should (equal (base64-decode-string (nth 2 parts))
                       harmless-media-test-png))
        (should (equal image
                       (format "Image image/png\n%s\n%d bytes\n\n%s"
                               (expand-file-name "a.png" dir)
                               (length harmless-media-test-png)
                               (base64-encode-string
                                harmless-media-test-png t)))))
      (should (equal (harmless-media-test-raw png)
                     harmless-media-test-png))
      (should (equal (harmless-tools-fs--read session '(:path "hello.png"))
                     "hello"))
      (should (equal (harmless-media-test-text fake-png) "hello"))
      (dolist (spec `(("a.jpg" "image/jpeg" ,harmless-media-test-jpeg)
                      ("a.gif" "image/gif" ,harmless-media-test-gif)
                      ("a.webp" "image/webp" ,harmless-media-test-webp)))
        (let* ((name (nth 0 spec))
               (got (harmless-tools-fs--read session (list :path name)))
               (parts (harmless-media-image-parts got)))
          (should (equal (nth 0 parts) (nth 1 spec)))
          (should (equal (base64-decode-string (nth 2 parts))
                         (nth 2 spec)))
          (should (equal (harmless-media-test-raw
                          (expand-file-name name dir))
                         (nth 2 spec)))))
      (let ((harmless-read-file-max-bytes 1)
            (before (harmless-media-test-raw png)))
        (harmless-media-test-error
         "image is too large"
         (lambda ()
           (harmless-tools-fs--read session '(:path "a.png"))))
        (should (equal (harmless-media-test-raw png) before)))
      (should (equal (harmless-tools-fs--read session '(:path "a.pdf"))
                     (format "PDF %s\nHello PDF" pdf)))
      (should (equal (harmless-media-test-text pdf)
                     "%PDF-1.1\n(Hello PDF) Tj\n( ) Tj\n%%EOF\n"))
      (let ((harmless-read-file-max-bytes 24)
            (before (harmless-media-test-text pdf)))
        (should (equal (harmless-tools-fs--read session '(:path "a.pdf"))
                       (format "PDF %s\nHello PDF\n[truncated]" pdf)))
        (should (equal (harmless-media-test-text pdf) before)))
      (harmless-media-test-error
       "pdf is not a pdf"
       (lambda ()
         (harmless-tools-fs--read session '(:path "bad.pdf"))))
      (should (equal (harmless-media-test-text bad-pdf) "hello"))
      (should-not (file-exists-p (expand-file-name "notes.pdf" dir)))
      (should (equal (harmless-tools-fs--read session '(:path "book.ipynb"))
                     (format "Notebook %s\ncell 0 code a:\nprint(1)\n\ncell 1 markdown b:\nhello"
                             book)))
      (should (equal (harmless-tools-fs--read
                      session '(:path "empty.ipynb"))
                     (format "Notebook %s\nNo cells" empty-book)))
      (harmless-media-test-write
       (expand-file-name "bad.ipynb" dir) "{")
      (harmless-media-test-error
       "notebook is not a notebook"
       (lambda ()
         (harmless-tools-fs--read session '(:path "bad.ipynb"))))
      (harmless-media-test-write
       (expand-file-name "object.ipynb" dir)
       "{\"cells\":{\"a\":1}}\n")
      (harmless-media-test-error
       "notebook is not a notebook"
       (lambda ()
         (harmless-tools-fs--read session '(:path "object.ipynb"))))
      (harmless-media-test-write
       (expand-file-name "nocells.ipynb" dir) "{\"nbformat\":4}\n")
      (harmless-media-test-error
       "notebook is not a notebook"
       (lambda ()
         (harmless-tools-fs--read session '(:path "nocells.ipynb"))))
      (let ((harmless-read-file-max-bytes 8)
            (before (harmless-media-test-text book)))
        (harmless-media-test-error
         "notebook is too large"
         (lambda ()
           (harmless-tools-fs--read session '(:path "book.ipynb"))))
        (should (equal (harmless-media-test-text book) before)))
      (let ((err (should-error
                  (harmless-tools-fs--read session '(:path "missing.txt"))
                  :type 'error)))
        (should (equal (format "Cannot read %s"
                               (expand-file-name "missing.txt" dir))
                       (error-message-string err))))
      (let ((err (should-error
                  (harmless-tools-fs--read session '(:path "a.png/"))
                  :type 'error)))
        (should (equal (format "Cannot read %s" slash)
                       (error-message-string err))))
      (should (equal (harmless-media-test-raw png)
                     harmless-media-test-png))
      (let ((wrote (harmless-tools-fs--write
                    session '(:path "new.txt" :contents "1"))))
        (should (equal wrote
                       (format "Wrote %s (%d bytes)"
                               (expand-file-name "new.txt" dir)
                               (string-bytes "1"))))
        (should (equal (harmless-tools-fs--read session '(:path "new.txt"))
                       "1")))
      (should (file-name-absolute-p (harmless-session-directory session)))
      (should-not (harmless-filesystem-root-p
                   (harmless-session-directory session)))
      (should (harmless-directory-strictly-under-p
               (harmless-session-directory session) data)))))

(ert-deftest harmless-media-edits-one-notebook-cell ()
  (harmless-media-test-env
    (let* ((book (expand-file-name "book.ipynb" dir))
           (plain (expand-file-name "notes.txt" dir))
           (body (concat "{\"nbformat\":4,\"cells\":["
                         "{\"cell_type\":\"code\",\"id\":\"a\","
                         "\"source\":[\"print(1)\\n\"],"
                         "\"outputs\":[{\"text\":\"1\"}]},"
                         "{\"cell_type\":\"markdown\",\"id\":\"b\","
                         "\"source\":[\"hello\"]}]}\n"))
           (chat (harmless-session-new
                  :detached t :provider provider :model "m"
                  :title "Chat"))
           (agent (harmless-session-new
                   :cwd dir :provider provider :model "m"
                   :title "Agent" :source 'agent
                   :parent-id (harmless-session-id session))))
      (harmless-media-test-write book body)
      (harmless-media-test-write plain "keep")
      (make-directory (expand-file-name "dir.ipynb" dir))
      (setf (harmless-session-agent agent)
            (list :allow 'all :sandbox 'read-only :prompt "look"))
      (should (equal (harmless-media-edit
                      session
                      '(:path "  book.ipynb  " :cell "a" :source "x\ny"))
                     (format "Edited cell a in %s" book)))
      (let* ((notebook (harmless-media-test-decode book))
             (cells (plist-get notebook :cells))
             (first (car cells))
             (second (cadr cells)))
        (should (equal (plist-get notebook :nbformat) 4))
        (should (equal (plist-get first :id) "a"))
        (should (equal (plist-get first :cell_type) "code"))
        (should (equal (plist-get first :source) '("x\n" "y")))
        (should (equal (plist-get (car (plist-get first :outputs)) :text)
                       "1"))
        (should (equal (plist-get second :id) "b"))
        (should (equal (plist-get second :source) '("hello"))))
      (harmless-media-test-under book dir)
      (should (equal (harmless-media-edit
                      session
                      '("path" "book.ipynb" "cell" 0 "source" "  x  "))
                     (format "Edited cell a in %s" book)))
      (should (equal (plist-get (car (plist-get
                                      (harmless-media-test-decode book)
                                      :cells))
                                :source)
                     '("  x  ")))
      (should (equal (harmless-media-edit
                      session '(:path "book.ipynb" :cell 0 :source ""))
                     (format "Edited cell a in %s" book)))
      (should (equal (plist-get (car (plist-get
                                      (harmless-media-test-decode book)
                                      :cells))
                                :source)
                     '("")))
      (let ((noid (expand-file-name "noid.ipynb" dir)))
        (harmless-media-test-write
         noid "{\"cells\":[{\"cell_type\":\"raw\",\"source\":[\"z\"]}]}\n")
        (should (equal (harmless-media-edit
                        session '(:path "noid.ipynb" :cell 0 :source "q"))
                       (format "Edited cell 0 in %s" noid)))
        (harmless-media-test-under noid dir)
        (should (equal (plist-get
                        (car (plist-get (harmless-media-test-decode noid)
                                        :cells))
                        :source)
                       '("q"))))
      (let ((before (harmless-media-test-text book)))
        (dolist (args (list '(:path "book.ipynb")
                            '(:path "book.ipynb" :cell nil :source "z")
                            '(:path "book.ipynb" :cell "" :source "z")
                            '(:path "book.ipynb" :cell "   " :source "z")))
          (harmless-media-test-error
           "notebook cell is empty"
           (lambda () (harmless-media-edit session args))))
        (dolist (cell '(:false t 1.5 -1))
          (harmless-media-test-error
           "notebook cell is not a cell"
           (lambda ()
             (harmless-media-edit
              session (list :path "book.ipynb" :cell cell :source "z")))))
        (harmless-media-test-error
         "notebook cell is missing"
         (lambda ()
           (harmless-media-edit
            session '(:path "book.ipynb" :cell "missing" :source "z"))))
        (harmless-media-test-error
         "notebook cell is missing"
         (lambda ()
           (harmless-media-edit
            session '(:path "book.ipynb" :cell 9 :source "z"))))
        (harmless-media-test-error
         "notebook source is empty"
         (lambda ()
           (harmless-media-edit
            session '(:path "book.ipynb" :cell 0))))
        (harmless-media-test-error
         "notebook source is empty"
         (lambda ()
           (harmless-media-edit
            session '(:path "book.ipynb" :cell 0 :source nil))))
        (dolist (source '(:false t 1))
          (harmless-media-test-error
           "notebook source must be a string"
           (lambda ()
             (harmless-media-edit
              session (list :path "book.ipynb" :cell 0 :source source)))))
        (should (equal (harmless-media-test-text book) before))
        (should (equal (harmless-media-test-text plain) "keep")))
      (let ((before (harmless-media-test-text book)))
        (dolist (args (list nil '(:cell 0 :source "z")
                            '(:path nil :cell 0 :source "z")
                            '(:path "" :cell 0 :source "z")
                            '(:path "   " :cell 0 :source "z")))
          (harmless-media-test-error
           "notebook path is empty"
           (lambda () (harmless-media-edit session args))))
        (dolist (path '(:false t 4))
          (harmless-media-test-error
           "notebook path must be a string"
           (lambda ()
             (harmless-media-edit
              session (list :path path :cell 0 :source "z")))))
        (dolist (path '("/" "//" "/." "c:" "c:/" "c:\\" "d:/" "C:/"
                        "\\" "  /  " "  c:/  " "d:\\"))
          (harmless-media-test-refuse-expand
           (lambda ()
             (harmless-media-edit
              session (list :path path :cell 0 :source "z")))
           "notebook path is the filesystem root"))
        (harmless-media-test-error
         "notebook is not a notebook"
         (lambda ()
           (harmless-media-edit
            session '(:path "book.ipynb/" :cell 0 :source "z"))))
        (harmless-media-test-error
         "notebook is missing"
         (lambda ()
           (harmless-media-edit
            session '(:path "notes" :cell 0 :source "z"))))
        (should-not (file-exists-p (expand-file-name "notes" dir)))
        (should-not (file-exists-p (expand-file-name "notes.ipynb" dir)))
        (harmless-media-test-error
         "notebook is not a notebook"
         (lambda ()
           (harmless-media-edit
            session '(:path "dir.ipynb" :cell 0 :source "z"))))
        (harmless-media-test-error
         "notebook is not a notebook"
         (lambda ()
           (harmless-media-edit
            session '(:path "notes.txt" :cell 0 :source "z"))))
        (should (equal (harmless-media-test-text plain) "keep"))
        (should (equal (harmless-media-test-text book) before))
        (let ((err (should-error
                    (harmless-tools-fs-resolve session "../out.ipynb")
                    :type 'error)))
          (should (equal "Path escapes project: ../out.ipynb"
                         (error-message-string err))))
        (harmless-media-test-error
         "Path escapes project: ../out.ipynb"
         (lambda ()
           (harmless-media-edit
            session '(:path "../out.ipynb" :cell 0 :source "z"))))
        (should-not (file-exists-p (expand-file-name "out.ipynb" base))))
      (harmless-media-test-error
       "No Harmless session"
       (lambda ()
         (harmless-media-edit nil '(:path "book.ipynb" :cell 0 :source "z"))))
      (harmless-media-test-error
       "This chat is not attached to a project"
       (lambda ()
         (harmless-media-edit
          chat '(:path "notes.ipynb" :cell 0 :source "z"))))
      (harmless-media-test-refuse-expand
       (lambda ()
         (harmless-media-edit chat '(:path "/" :cell 0 :source "z")))
       "notebook path is the filesystem root")
      (should (equal (harmless-tool-call
                      (harmless-tool-by-name "notebook_edit")
                      session '(:path "notes" :cell 0 :source "z"))
                     "Error: notebook is missing"))
      (should-not (file-exists-p (expand-file-name "notes" dir)))
      (setf (harmless-session-plan-mode session) t)
      (let ((before (harmless-media-test-text book)))
        (harmless-media-test-error
         "Plan mode is on. Only plan.md may be edited, with write_plan"
         (lambda ()
           (harmless-media-edit
            session '(:path "/" :cell 0 :source "z"))))
        (harmless-media-test-error
         "Plan mode is on. Only plan.md may be edited, with write_plan"
         (lambda ()
           (harmless-media-edit
            session '(:path "book.ipynb" :cell 0 :source "nope"))))
        (should (equal (harmless-media-test-text book) before))
        (should-not (file-exists-p (expand-file-name "plan.md" dir)))
        (should (harmless-session-plan-mode session)))
      (should (eq (harmless-tool-class
                   (harmless-tool-by-name "notebook_edit"))
                  'edit))
      (should (eq (harmless-session-permission-mode session) 'ask))
      (should (harmless-perm-needed-p session 'edit))
      (should-not (member "notebook_edit"
                          (mapcar #'harmless-tool-name
                                  (harmless-tools-for-session chat))))
      (should-not (member "notebook_edit"
                          (mapcar #'harmless-tool-name
                                  (harmless-tools-for-session agent))))
      (should (member "read_file"
                      (mapcar #'harmless-tool-name
                              (harmless-tools-for-session agent))))
      (should (equal (harmless-agent-refuse-call
                      agent (harmless-tool-by-name "notebook_edit") nil)
                     (concat "Error: This subagent is read-only. "
                             "A tool that can write is refused")))
      (should (equal (harmless-ui--tool-verb "notebook_edit") "notebook"))
      (should (equal (harmless-ui--tool-summary
                      "notebook_edit" '(:path "n.ipynb" :cell 0))
                     "n.ipynb 0"))
      (should (equal (harmless-ui--tool-summary
                      "notebook_edit" '(:path "n.ipynb" :cell "a"))
                     "n.ipynb a"))
      (should (equal (harmless-ui--tool-summary "notebook_edit" nil) ""))
      (should (file-name-absolute-p (harmless-session-directory session)))
      (should-not (harmless-filesystem-root-p
                   (harmless-session-directory session)))
      (should (harmless-directory-strictly-under-p
               (harmless-session-directory session) data))
      (should (file-name-absolute-p (harmless-session-directory chat)))
      (should-not (harmless-filesystem-root-p
                   (harmless-session-directory chat)))
      (should (harmless-directory-strictly-under-p
               (harmless-session-directory chat) data)))))

(ert-deftest harmless-media-attaches-an-image-to-the-prompt ()
  (harmless-media-test-env
    (let* ((png (expand-file-name "a.png" dir))
           (jpeg (expand-file-name "a.jpg" dir))
           (text-file (expand-file-name "a.txt" dir))
           (calls 0)
           (seen nil)
           (fake (harmless-media-make-fake
                  :name "fake" :host "none"
                  :script
                  (lambda (messages callback)
                    (setq calls (1+ calls))
                    (push messages seen)
                    (funcall callback '(:text "ok"))
                    (funcall callback '(:stop "stop")))))
           (turn nil)
           (chat (harmless-session-new
                  :detached t :provider provider :model "m"
                  :title "Chat")))
      (harmless-media-test-bytes png harmless-media-test-png)
      (harmless-media-test-bytes jpeg harmless-media-test-jpeg)
      (harmless-media-test-write text-file "hello")
      (let ((names (directory-files dir)))
        (let ((image (harmless-attach-image session "a.png")))
          (should (equal (plist-get image :media-type) "image/png"))
          (should (equal (plist-get image :data)
                         (base64-encode-string harmless-media-test-png t)))
          (harmless-media-test-under (plist-get image :path) dir)
          (should (file-equal-p (plist-get image :path) png)))
        (should (harmless-media-pending-p session))
        (should (equal (harmless-media-test-raw png)
                       harmless-media-test-png))
        (should (equal names (directory-files dir)))
        (harmless-attach-image session jpeg)
        (should (= (length (gethash (harmless-session-id session)
                                    harmless-media-attachments))
                   2)))
      (setq turn (harmless-session-new
                  :cwd dir :provider fake :model "m" :title "Turn"))
      (harmless-attach-image turn png)
      (harmless-attach-image turn "a.jpg")
      (should (harmless-media-pending-p turn))
      (setq calls 0)
      (setq seen nil)
      (harmless-turn-run turn "")
      (should (= calls 1))
      (should-not (harmless-media-pending-p turn))
      (let* ((user (car (last (harmless-session-messages turn)
                              (length (harmless-session-messages turn)))))
             (images nil)
             (blocks nil))
        (setq user (cl-find-if
                    (lambda (msg) (eq (plist-get msg :role) :user))
                    (harmless-session-messages turn)))
        (setq images (plist-get user :images))
        (should (equal (plist-get user :content) ""))
        (should (= (length images) 2))
        (should (equal (plist-get (car images) :media-type) "image/png"))
        (should (equal (plist-get (cadr images) :media-type) "image/jpeg"))
        (setq blocks (harmless-media-openai-user user))
        (should (= (length blocks) 2))
        (should (equal (plist-get (car blocks) :type) "image_url"))
        (should (string-prefix-p
                 "data:image/png;base64,"
                 (plist-get (plist-get (car blocks) :image_url) :url)))
        (should-not (cl-find "text" blocks
                             :key (lambda (block) (plist-get block :type))
                             :test #'equal))
        (setq blocks (harmless-media-anthropic-user user))
        (should (equal (plist-get (car blocks) :type) "image"))
        (should (equal (plist-get (plist-get (car blocks) :source)
                                  :media_type)
                       "image/png"))
        (should (equal (plist-get (plist-get (car blocks) :source) :data)
                       (base64-encode-string harmless-media-test-png t))))
      (harmless-turn-run turn "next")
      (should (= calls 2))
      (let ((users (cl-remove-if-not
                    (lambda (msg) (eq (plist-get msg :role) :user))
                    (harmless-session-messages turn))))
        (should (= (length users) 2))
        (should (plist-get (car users) :images))
        (should-not (plist-get (cadr users) :images))
        (should (equal (plist-get (cadr users) :content) "next")))
      (should-not (harmless-media-pending-p turn))
      (harmless-session-save turn)
      (let* ((loaded (harmless-session-load
                      (harmless-session-directory turn)))
             (user (cl-find-if
                    (lambda (msg) (eq (plist-get msg :role) :user))
                    (harmless-session-messages loaded)))
             (blocks (harmless-openai--format-message user)))
        (should (equal (plist-get (cadr (plist-get blocks :content)) :type)
                       "image_url"))
        (should (string-prefix-p
                 "data:image/jpeg;base64,"
                 (plist-get
                  (plist-get (cadr (plist-get blocks :content)) :image_url)
                  :url))))
      (harmless-attach-image turn png)
      (should (harmless-media-pending-p turn))
      (let ((child (harmless-fork--create
                    turn (harmless-session-messages turn))))
        (should (harmless-media-pending-p turn))
        (should-not (harmless-media-pending-p child))
        (should (plist-get
                 (cl-find-if
                  (lambda (msg) (eq (plist-get msg :role) :user))
                  (harmless-session-messages child))
                 :images))
        (should (file-name-absolute-p (harmless-session-directory child)))
        (should-not (harmless-filesystem-root-p
                     (harmless-session-directory child)))
        (should (harmless-directory-strictly-under-p
                 (harmless-session-directory child) data)))
      (harmless-media-test-error
       "Prompt is empty"
       (lambda () (harmless-turn-steer turn "  ")))
      (should (harmless-media-pending-p turn))
      (remhash (harmless-session-id session) harmless-media-attachments)
      (with-temp-buffer
        (setq harmless--session session)
        (harmless-media-test-error
         "Prompt is empty"
         (lambda () (harmless-prompt-send))))
      (with-temp-buffer
        (setq harmless--session turn)
        (insert "  ")
        (let ((before calls))
          (harmless-prompt-send)
          (should (= calls (1+ before)))))
      (let* ((text (harmless-media-image-text
                    png "image/png" harmless-media-test-png))
             (msg (list :role :tool :name "read_file" :id "1"
                        :content text))
             (openai (harmless-media-openai-tool msg))
             (anthropic (harmless-media-anthropic-tool msg))
             (plain (list :role :tool :name "read_file" :id "2"
                          :content "hello")))
        (should (equal (plist-get (car openai) :text)
                       (format "Image image/png\n%s" png)))
        (should-not (string-match-p
                     (regexp-quote (base64-encode-string
                                    harmless-media-test-png t))
                     (plist-get (car openai) :text)))
        (should (equal (plist-get (cadr openai) :type) "image_url"))
        (should (equal (plist-get (car anthropic) :text)
                       (format "Image image/png\n%s" png)))
        (should (equal (plist-get (cadr anthropic) :type) "image"))
        (should-not (harmless-media-openai-tool plain))
        (should (equal (plist-get (harmless-openai--format-message plain)
                                  :content)
                       "hello"))
        (should-not (harmless-media-openai-user
                     (list :role :user :content "hi"
                           :images '(:media-type "image/png")))))
      (with-temp-buffer
        (harmless-ui--insert-message
         (list :role :user :content "see"
               :images (list (list :media-type "image/png"
                                   :data "AAAA" :path png))))
        (should (string-match-p
                 (regexp-quote (format "[image: %s]" png))
                 (buffer-string)))
        (should-not (string-match-p "AAAA" (buffer-string))))
      (dolist (path (list nil "" "   "))
        (harmless-media-test-error
         "image path is empty"
         (lambda () (harmless-media-attach session path))))
      (dolist (path '(:false t 3))
        (harmless-media-test-error
         "image path must be a string"
         (lambda () (harmless-media-attach session path))))
      (dolist (path '("/" "//" "/." "c:" "c:/" "c:\\" "d:/" "C:/" "\\"))
        (harmless-media-test-refuse-expand
         (lambda () (harmless-media-attach session path))
         "image path is the filesystem root"))
      (harmless-media-test-refuse-expand
       (lambda () (harmless-media-attach session "~/a.png"))
       "image path must be absolute")
      (harmless-media-test-refuse-expand
       (lambda () (harmless-media-attach chat "notes"))
       "image path must be absolute")
      (harmless-media-test-refuse-expand
       (lambda () (harmless-media-attach chat "~/a.png"))
       "image path must be absolute")
      (harmless-media-test-error
       "image is not an image"
       (lambda () (harmless-media-attach session "a.png/")))
      (harmless-media-test-error
       "image is missing"
       (lambda () (harmless-media-attach session "missing.png")))
      (should-not (file-exists-p (expand-file-name "missing.png" dir)))
      (harmless-media-test-error
       "image is not an image"
       (lambda () (harmless-media-attach session "a.txt")))
      (should (equal (harmless-media-test-text text-file) "hello"))
      (make-directory (expand-file-name "sub" dir))
      (harmless-media-test-error
       "image is not an image"
       (lambda () (harmless-media-attach session "sub")))
      (let ((harmless-read-file-max-bytes 1)
            (before (harmless-media-test-raw png)))
        (harmless-media-test-error
         "image is too large"
         (lambda () (harmless-media-attach session png)))
        (should (equal (harmless-media-test-raw png) before)))
      (harmless-media-test-error
       "Path escapes project: ../a.png"
       (lambda () (harmless-media-attach session "../a.png")))
      (harmless-media-test-error
       "No Harmless session"
       (lambda () (harmless-attach-image nil png)))
      (let ((outside (expand-file-name "out.png" base)))
        (harmless-media-test-bytes outside harmless-media-test-png)
        (let ((image (harmless-attach-image chat outside)))
          (should (equal (plist-get image :path) outside))
          (should (equal (plist-get image :media-type) "image/png")))
        (should (equal (harmless-media-test-raw outside)
                       harmless-media-test-png)))
      (should (file-name-absolute-p (harmless-session-directory session)))
      (should-not (harmless-filesystem-root-p
                   (harmless-session-directory session)))
      (should (harmless-directory-strictly-under-p
               (harmless-session-directory session) data)))))
