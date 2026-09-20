;;; harmless-openai-tests.el --- Tests for the OpenAI assembler -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'harmless-http)
(require 'harmless-openai)

(defun harmless-test--collect-openai (payloads)
  "Run PAYLOADS (JSON strings) through the OpenAI assembler."
  (let ((asm (harmless-openai-assembler-create))
        events)
    (dolist (p payloads)
      (harmless-openai-handle-payload
       asm p
       (lambda (event) (push event events))))
    (harmless-openai-finish asm (lambda (event) (push event events)))
    (nreverse events)))

(ert-deftest harmless-openai-payload-includes-effort ()
  (let ((json (harmless-openai--payload nil "grok-4.6" nil nil t "xhigh")))
    (should (string-match-p "\"reasoning_effort\":\"xhigh\"" json))
    (should (string-match-p "\"model\":\"grok-4.6\"" json))))

(ert-deftest harmless-openai-text-deltas ()
  (let ((events (harmless-test--collect-openai
                 '("{\"choices\":[{\"delta\":{\"content\":\"Hello\"}}]}"
                   "{\"choices\":[{\"delta\":{\"content\":\"!\"},\"finish_reason\":\"stop\"}]}"))))
    (should (equal '(:text "Hello") (cl-find :text events :key #'car)))
    (should (equal '(:text "!") (nth 1 (seq-filter (lambda (e) (eq (car e) :text)) events))))
    (should (equal '(:stop "stop") (car (last events))))))

(ert-deftest harmless-openai-two-tool-calls ()
  (let* ((events (harmless-test--collect-openai
                  '("{\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"call_1\",\"function\":{\"name\":\"read_file\",\"arguments\":\"{\\\"path\\\":\"}}]}}]}"
                    "{\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"\\\"a.txt\\\"}\"}}]}}]}"
                    "{\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":1,\"id\":\"call_2\",\"function\":{\"name\":\"list_dir\",\"arguments\":\"{\\\"path\\\":\\\".\\\"}\"}}]}}]}"
                    "{\"choices\":[{\"delta\":{},\"finish_reason\":\"tool_calls\"}]}")))
         (calls (seq-filter (lambda (e) (eq (car e) :tool-call)) events)))
    (should (>= (length calls) 2))
    (should (cl-find "call_1" calls :key #'cadr :test #'equal))
    (should (cl-find "call_2" calls :key #'cadr :test #'equal))
    (should (equal '(:stop "tool_calls") (car (last events))))))

(ert-deftest harmless-openai-sse-fixture-tools ()
  (let ((raw (with-temp-buffer
               (insert-file-contents
                (expand-file-name "test/fixtures/openai-tools.sse"
                                  default-directory))
               (buffer-string)))
        (state (harmless-sse-state-create))
        (asm (harmless-openai-assembler-create))
        events)
    (harmless-http-sse-push
     state raw
     (lambda (type data)
       (pcase type
         ('done (harmless-openai-finish
                 asm (lambda (event) (push event events))))
         ('message (harmless-openai-handle-payload
                    asm data
                    (lambda (event) (push event events)))))))
    (setq events (nreverse events))
    (should (cl-find :tool-call events :key #'car))
    (should (equal :stop (car (car (last events)))))))

(provide 'harmless-openai-tests)
