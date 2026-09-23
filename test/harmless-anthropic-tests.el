;;; harmless-anthropic-tests.el --- Tests for the Anthropic assembler -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'harmless-http)
(require 'harmless-anthropic)

(ert-deftest harmless-anthropic-text-sse-fixture ()
  (let ((raw (with-temp-buffer
               (insert-file-contents
                (expand-file-name "test/fixtures/anthropic-text.sse"
                                  default-directory))
               (buffer-string)))
        (state (harmless-sse-state-create))
        (asm (harmless-anthropic-assembler-create))
        events)
    (harmless-http-sse-push
     state raw
     (lambda (type data)
       (harmless-anthropic-handle-event
        asm type data
        (lambda (event) (push event events)))))
    (setq events (nreverse events))
    (should (equal '(:text "Hi") (cl-find :text events :key #'car)))
    (should (cl-find '(:text " there") events :test #'equal))
    (should (eq :stop (car (car (last events)))))))

(ert-deftest harmless-anthropic-error-json-is-not-a-stop ()
  (let ((asm (harmless-anthropic-assembler-create))
        events)
    (harmless-anthropic-handle-event
     asm 'message
     "{\"type\":\"error\",\"error\":{\"type\":\"rate_limit_error\",\"message\":\"Error\"}}"
     (lambda (event) (push event events)))
    (setq events (nreverse events))
    (should (equal '(:error "rate_limit_error") (car events)))
    (should-not (cl-find :stop events :key #'car))
    (should-not (harmless-anthropic-assembler-emitted-stop asm))))

(ert-deftest harmless-anthropic-skips-empty-assistant ()
  (should (equal '((:role "user" :content "Hi\n\nStill there?"))
                 (harmless-anthropic--format-messages
                  '((:role :user :content "Hi")
                    (:role :assistant :content "")
                    (:role :user :content "Still there?"))))))

(ert-deftest harmless-anthropic-tool-use-block ()
  (let ((asm (harmless-anthropic-assembler-create))
        events)
    (cl-labels ((emit (e) (push e events)))
      (harmless-anthropic-handle-event
       asm 'content_block_start
       "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"t1\",\"name\":\"read_file\"}}"
       #'emit)
      (harmless-anthropic-handle-event
       asm 'content_block_delta
       "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"path\\\":\\\"a\\\"}\"}}"
       #'emit)
      (harmless-anthropic-finish asm #'emit))
    (setq events (nreverse events))
    (should (equal "t1" (nth 1 (cl-find :tool-call events :key #'car))))
    (should (equal "read_file" (nth 2 (cl-find :tool-call events :key #'car))))))

(provide 'harmless-anthropic-tests)
