;;; harmless-ui-tests.el --- Tests for the Harmless transcript -*- lexical-binding: t; -*-

(require 'ert)
(require 'harmless-openai)
(require 'harmless-session)
(require 'harmless-ui)

(defun harmless-ui-test-visible ()
  "Return the visible text of the current buffer."
  (let ((chunks nil)
        (pos (point-min)))
    (while (< pos (point-max))
      (let ((next (next-single-property-change pos 'invisible nil (point-max))))
        (unless (invisible-p pos)
          (push (buffer-substring-no-properties pos next) chunks))
        (setq pos (if (and next (> next pos)) next (point-max)))))
    (apply #'concat (nreverse chunks))))

(defun harmless-ui-test-session ()
  "Return a throwaway session."
  (let* ((harmless-directory (make-temp-file "harmless-test-" t))
         (harmless--sessions (make-hash-table :test 'equal))
         (provider (harmless-make-openai-compat
                    "local" :host "127.0.0.1:9" :protocol "http"
                    :key "none" :models '("m")))
         (harmless-providers (list provider)))
    (harmless-session-new :cwd harmless-directory
                          :provider provider
                          :model "m")))

(defun harmless-ui-test-render (session messages)
  "Render MESSAGES into SESSION and return its transcript buffer."
  (setf (harmless-session-messages session) messages)
  (harmless-ui-render-session session)
  (harmless-session-buffer session))

(ert-deftest harmless-ui-groups-reads-and-folds ()
  (let* ((session (harmless-ui-test-session))
         (calls (list
                 (list :id "g" :name "grep"
                       :args "{\"pattern\":\"defpackage\",\"glob\":\"**/*.lisp\"}")
                 (list :id "r1" :name "read_file"
                       :args "{\"path\":\"source/behave.lisp\",\"limit\":80}")
                 (list :id "r2" :name "read_file"
                       :args "{\"path\":\"source/control.lisp\"}")))
         (grep-hit "source/a.lisp:1:(defpackage :sigma/a")
         (buf (harmless-ui-test-render
               session
               (list
                (list :role :user :content "find packages")
                (list :role :assistant :content "Looking." :tool-calls calls)
                (list :role :tool :id "g" :name "grep" :content grep-hit)
                (list :role :tool :id "r1" :name "read_file"
                      :content "line1\nline2\n")
                (list :role :tool :id "r2" :name "read_file"
                      :content "(defun foo ())\n")))))
    (with-current-buffer buf
      (should (eq (lookup-key harmless-session-mode-map (kbd "TAB"))
                  #'harmless-ui-toggle-fold))
      (should (eq (lookup-key harmless-session-mode-map (kbd "<left>"))
                  #'harmless-ui-fold-close))
      (should (eq (lookup-key harmless-session-mode-map (kbd "<right>"))
                  #'harmless-ui-fold-open))
      (let ((visible (harmless-ui-test-visible)))
        (should (string-match-p "Looking\\." visible))
        (should (string-match-p "▸ explored  3 calls" visible))
        (should-not (string-match-p "defpackage" visible))
        (should-not (string-match-p "line1" visible))
        (should-not (string-match-p "\\[tool" (buffer-string)))
        (should-not (string-match-p "\\[result" (buffer-string)))
        (should-not (string-match-p "pattern" (buffer-string)))
        (should (string-match-p (regexp-quote grep-hit) (buffer-string))))
      (goto-char (point-min))
      (search-forward "explored")
      (harmless-ui-toggle-fold)
      (let ((visible (harmless-ui-test-visible)))
        (should (string-match-p
                 "grep  defpackage  \\*\\*/\\*\\.lisp  (1)" visible))
        (should (string-match-p "read  source/behave.lisp  (2)" visible))
        (should (string-match-p "read  source/control.lisp  (1)" visible))
        (should-not (string-match-p "line1" visible))
        (should-not (string-match-p (regexp-quote grep-hit) visible)))
      (goto-char (point-min))
      (search-forward "behave")
      (harmless-ui-fold-open)
      (should (string-match-p "line1" (harmless-ui-test-visible)))
      (harmless-ui-fold-close)
      (should-not (string-match-p "line1" (harmless-ui-test-visible)))
      (harmless-ui-fold-close)
      (should-not (string-match-p "grep" (harmless-ui-test-visible)))
      (goto-char (point-min))
      (search-forward "explored")
      (harmless-ui-toggle-all-folds)
      (should (string-match-p "line1" (harmless-ui-test-visible)))
      (should (string-match-p (regexp-quote grep-hit) (harmless-ui-test-visible)))
      (harmless-ui-toggle-all-folds)
      (should-not (string-match-p "line1" (harmless-ui-test-visible))))))

(ert-deftest harmless-ui-shell-and-edit-stay-separate ()
  (let* ((session (harmless-ui-test-session))
         (calls (list
                 (list :id "r" :name "read_file"
                       :args "{\"path\":\"source/behave.lisp\"}")
                 (list :id "s" :name "run_shell"
                       :args "{\"command\":\"git status\"}")
                 (list :id "w" :name "write_file"
                       :args "{\"path\":\"source/behave.lisp\"}")))
         (buf (harmless-ui-test-render
               session
               (list
                (list :role :assistant :content "" :tool-calls calls)
                (list :role :tool :id "r" :name "read_file" :content "one\n")
                (list :role :tool :id "s" :name "run_shell" :content "clean\n")
                (list :role :tool :id "w" :name "write_file"
                      :content "Wrote source/behave.lisp (3 bytes)")))))
    (with-current-buffer buf
      (let ((visible (harmless-ui-test-visible)))
        (should (string-match-p "▸ read  source/behave.lisp  (1)" visible))
        (should (string-match-p "▸ shell  git status  (1)" visible))
        (should (string-match-p "▸ edit  source/behave.lisp  (1)" visible))
        (should-not (string-match-p "explored" visible))
        (should-not (string-match-p "clean" visible)))
      (goto-char (point-min))
      (search-forward "shell")
      (harmless-ui-fold-open)
      (should (string-match-p "clean" (harmless-ui-test-visible))))))

(ert-deftest harmless-ui-stream-collapses-a-run ()
  (let* ((session (harmless-ui-test-session))
         (calls (list
                 (list :id "g" :name "grep"
                       :args "{\"pattern\":\"defpackage\",\"glob\":\"**/*.lisp\"}")
                 (list :id "r" :name "read_file"
                       :args "{\"path\":\"source/behave.lisp\"}"))))
    (harmless-ui--on-event session '(:text "Looking."))
    (harmless-ui--on-event session '(:tool-call "g" "grep" "{\"pattern\":"))
    (harmless-ui--on-event session '(:tool-call "g" "grep" "\"defpackage\"}"))
    (harmless-ui--on-event session '(:tool-call "r" "read_file"
                                                "{\"path\":\"source/behave.lisp\"}"))
    (with-current-buffer (harmless-session-buffer session)
      (should (= 2 (count-matches "▸" (point-min) (point-max)))))
    (harmless-ui--on-event
     session
     (list :message (list :role :assistant :content "Looking." :tool-calls calls)))
    (with-current-buffer (harmless-session-buffer session)
      (let ((visible (harmless-ui-test-visible)))
        (should (string-match-p "Looking\\." visible))
        (should (= 1 (count-matches "Looking\\." (point-min) (point-max))))
        (should (string-match-p "▸ explored  2 calls" visible))
        (should-not (string-match-p "\\[tool" (buffer-string)))
        (should-not (string-match-p "behave" visible))))
    (harmless-ui--on-event
     session
     '(:message (:role :tool :id "g" :name "grep"
                 :content "source/a.lisp:35:(defpackage :sigma/a")))
    (with-current-buffer (harmless-session-buffer session)
      (should-not (string-match-p "source/a.lisp" (harmless-ui-test-visible)))
      (should (string-match-p "source/a.lisp" (buffer-string)))
      (goto-char (point-min))
      (search-forward "explored")
      (harmless-ui-fold-open)
      (should (string-match-p "grep  defpackage  \\*\\*/\\*\\.lisp  (1)"
                              (harmless-ui-test-visible)))
      (should-not (string-match-p "source/a.lisp" (harmless-ui-test-visible))))))
