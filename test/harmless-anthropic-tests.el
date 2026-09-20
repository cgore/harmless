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
