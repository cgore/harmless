;;; harmless-http-tests.el --- Tests for Harmless SSE parsing -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless-http)

(defun harmless-test--sse (chunks)
  "Push CHUNKS through the SSE parser and return events as (TYPE . DATA)."
  (let ((state (harmless-sse-state-create))
        events)
    (dolist (chunk chunks)
      (harmless-http-sse-push
       state chunk
       (lambda (type data)
         (push (cons type data) events))))
    (harmless-http-sse-flush
     state
     (lambda (type data)
       (push (cons type data) events)))
    (nreverse events)))

(ert-deftest harmless-http-sse-single-event ()
  (should (equal (harmless-test--sse '("data: hello\n\n"))
                 '((message . "hello")))))

(ert-deftest harmless-http-sse-split-chunks ()
  (should (equal (harmless-test--sse '("data: hel" "lo\n\n"))
                 '((message . "hello")))))

(ert-deftest harmless-http-sse-crlf ()
  (should (equal (harmless-test--sse '("data: hi\r\n\r\n"))
                 '((message . "hi")))))

(ert-deftest harmless-http-sse-done ()
  (should (equal (harmless-test--sse '("data: [DONE]\n\n"))
                 '((done . nil)))))

(ert-deftest harmless-http-sse-event-type ()
  (should (equal (harmless-test--sse '("event: ping\ndata: {}\n\n"))
                 '((ping . "{}")))))

(ert-deftest harmless-http-sse-multiline-data ()
  (should (equal (harmless-test--sse '("data: a\ndata: b\n\n"))
                 '((message . "a\nb")))))

(ert-deftest harmless-http-sse-fixture-openai-text ()
  (let* ((raw (with-temp-buffer
                (insert-file-contents
                 (expand-file-name "test/fixtures/openai-text.sse"
                                   default-directory))
                (buffer-string)))
         (events (harmless-test--sse (list raw))))
    (should (eq (car (car (last events))) 'done))
    (should (cl-find-if (lambda (e)
                          (and (eq (car e) 'message)
                               (string-match-p "Hello" (cdr e))))
                        events))))

(provide 'harmless-http-tests)
