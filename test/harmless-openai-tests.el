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

(ert-deftest harmless-anthropic-payload-includes-effort ()
  (require 'harmless-anthropic)
  (let ((json (harmless-anthropic--payload nil "claude-sonnet-4-6" nil nil t "high")))
    (should (string-match-p "output_config" json))
    (should (string-match-p "\"effort\":\"high\"" json))))

(ert-deftest harmless-openai-payload-includes-effort ()
  (let ((json (harmless-openai--payload nil "grok-4.6" nil nil t "xhigh")))
    (should (string-match-p "\"reasoning_effort\":\"xhigh\"" json))
    (should (string-match-p "\"model\":\"grok-4.6\"" json))))

(ert-deftest harmless-openai-responses-detail-error ()
  (let ((asm (harmless-openai-responses-assembler-create))
        events)
    (harmless-openai-responses-handle
     asm 'message
     "{\"detail\":\"The 'gpt-5.4' model is not supported when using Codex with a ChatGPT account.\"}"
     (lambda (event) (push event events)))
    (setq events (nreverse events))
    (should (equal :error (car (car events))))
    (should (string-match-p "gpt-5.4" (cadr (car events))))
    (should (eq :stop (car (car (last events)))))))

(ert-deftest harmless-openai-responses-payload ()
  (let ((json (harmless-openai--responses-payload
               nil "gpt-5.4"
               '((:role :user :content "hi"))
               nil t "xhigh")))
    (should (string-match-p "\"store\":false" json))
    (should (string-match-p "\"effort\":\"xhigh\"" json))
    (should (string-match-p "\"role\":\"user\"" json))
    (should (string-match-p "\"model\":\"gpt-5.4\"" json))
    (should-not (string-match-p "reasoning_effort" json))))

(ert-deftest harmless-openai-responses-input-tools ()
  (let ((items (harmless-openai--responses-input
                '((:role :assistant :content "ok"
                   :tool-calls ((:id "c1" :name "read_file"
                                 :args "{\"path\":\"a\"}")))
                  (:role :tool :id "c1" :content "data")))))
    (should (equal "assistant" (plist-get (nth 0 items) :role)))
    (should (equal "function_call" (plist-get (nth 1 items) :type)))
    (should (equal "c1" (plist-get (nth 1 items) :call_id)))
    (should (equal "function_call_output" (plist-get (nth 2 items) :type)))
    (should (equal "c1" (plist-get (nth 2 items) :call_id)))
    (should (equal "data" (plist-get (nth 2 items) :output)))))

(ert-deftest harmless-openai-responses-input-fills-missing-tool-output ()
  (let ((items (harmless-openai--responses-input
                '((:role :assistant :content ""
                   :tool-calls ((:id "c1" :name "read_file" :args "{}")
                                (:id "c2" :name "list_dir" :args "{}")))
                  (:role :user :content "What is 3+5?")))))
    (should (equal "function_call" (plist-get (nth 0 items) :type)))
    (should (equal "c1" (plist-get (nth 0 items) :call_id)))
    (should (equal "function_call" (plist-get (nth 1 items) :type)))
    (should (equal "function_call_output" (plist-get (nth 2 items) :type)))
    (should (equal "c1" (plist-get (nth 2 items) :call_id)))
    (should (equal "function_call_output" (plist-get (nth 3 items) :type)))
    (should (equal "c2" (plist-get (nth 3 items) :call_id)))
    (should (equal "user" (plist-get (nth 4 items) :role)))
    (should (equal "What is 3+5?" (plist-get (nth 4 items) :content)))))

(ert-deftest harmless-openai-responses-input-partial-tool-results ()
  (let ((items (harmless-openai--responses-input
                '((:role :assistant :tool-calls
                   ((:id "c1" :name "read_file" :args "{}")
                    (:id "c2" :name "list_dir" :args "{}")))
                  (:role :tool :id "c1" :content "ok")
                  (:role :user :content "hi")))))
    (should (equal "ok" (plist-get (nth 2 items) :output)))
    (should (equal "c1" (plist-get (nth 2 items) :call_id)))
    (should (equal "function_call_output" (plist-get (nth 3 items) :type)))
    (should (equal "c2" (plist-get (nth 3 items) :call_id)))
    (should (string-match-p "not run" (plist-get (nth 3 items) :output)))
    (should (equal "user" (plist-get (nth 4 items) :role)))))

(ert-deftest harmless-openai-responses-text-sse-fixture ()
  (let ((raw (with-temp-buffer
               (insert-file-contents
                (expand-file-name "test/fixtures/openai-responses-text.sse"
                                  default-directory))
               (buffer-string)))
        (state (harmless-sse-state-create))
        (asm (harmless-openai-responses-assembler-create))
        events)
    (harmless-http-sse-push
     state raw
     (lambda (type data)
       (pcase type
         ('done (harmless-openai-responses-finish
                 asm (lambda (event) (push event events))))
         (_ (harmless-openai-responses-handle
             asm type data
             (lambda (event) (push event events)))))))
    (setq events (nreverse events))
    (should (equal '(:text "Hi") (cl-find :text events :key #'car)))
    (should (cl-find '(:text " there") events :test #'equal))
    (should (equal '(:usage 3 2) (cl-find :usage events :key #'car)))
    (should (eq :stop (car (car (last events)))))))

(ert-deftest harmless-openai-responses-tool-call-stream ()
  (let ((asm (harmless-openai-responses-assembler-create))
        events)
    (cl-labels ((emit (e) (push e events)))
      (harmless-openai-responses-handle
       asm 'response.output_item.added
       "{\"type\":\"response.output_item.added\",\"item\":{\"id\":\"fc_1\",\"type\":\"function_call\",\"call_id\":\"c1\",\"name\":\"read_file\",\"arguments\":\"\"}}"
       #'emit)
      (harmless-openai-responses-handle
       asm 'response.function_call_arguments.delta
       "{\"type\":\"response.function_call_arguments.delta\",\"item_id\":\"fc_1\",\"delta\":\"{\\\"path\\\":\"}"
       #'emit)
      (harmless-openai-responses-handle
       asm 'response.function_call_arguments.delta
       "{\"type\":\"response.function_call_arguments.delta\",\"item_id\":\"fc_1\",\"delta\":\"\\\"a\\\"}\"}"
       #'emit)
      (harmless-openai-responses-handle
       asm 'response.output_item.done
       "{\"type\":\"response.output_item.done\",\"item\":{\"id\":\"fc_1\",\"type\":\"function_call\",\"call_id\":\"c1\",\"name\":\"read_file\",\"arguments\":\"{\\\"path\\\":\\\"a\\\"}\"}}"
       #'emit)
      (harmless-openai-responses-finish asm #'emit))
    (setq events (nreverse events))
    (let ((calls (seq-filter (lambda (e) (eq (car e) :tool-call)) events)))
      (should (equal "c1" (nth 1 (car calls))))
      (should (equal "read_file" (nth 2 (car calls))))
      (should (= 2 (length calls)))
      (should (eq :stop (car (car (last events))))))))

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
