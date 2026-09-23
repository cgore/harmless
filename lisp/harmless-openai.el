;;; harmless-openai.el --- OpenAI-compatible provider for Harmless -*- lexical-binding: t; -*-

;;;; Copyright (c) 2026, Christopher Mark Gore,
;;;; Soli Deo Gloria,
;;;; All rights reserved.
;;;;
;;;; 22 Forest Glade Court, Saint Charles, Missouri 63304 USA.
;;;; Web: http://cgore.com
;;;; Email: cgore@cgore.com
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are met:
;;;;
;;;;     * Redistributions of source code must retain the above copyright
;;;;       notice, this list of conditions and the following disclaimer.
;;;;
;;;;     * Redistributions in binary form must reproduce the above copyright
;;;;       notice, this list of conditions and the following disclaimer in the
;;;;       documentation and/or other materials provided with the distribution.
;;;;
;;;;     * Neither the name of Christopher Mark Gore nor the names of other
;;;;       contributors may be used to endorse or promote products derived from
;;;;       this software without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;;; LIABLE FOR DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;;
;; Chat Completions SSE for OpenAI, xAI, Ollama, llama.cpp, vLLM, and any
;; other OpenAI-compatible host.  ChatGPT browser-login tokens use the
;; Codex Responses API on chatgpt.com instead of api.openai.com.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-log)
(require 'harmless-http)
(require 'harmless-provider)
(require 'harmless-xai)
(require 'harmless-openai-oauth)

(declare-function harmless-tool-name "harmless-tools")
(declare-function harmless-tool-description "harmless-tools")
(declare-function harmless-tool-schema "harmless-tools")

(cl-defstruct (harmless-openai-provider
               (:include harmless-provider)
               (:constructor harmless-openai-provider-create)
               (:copier nil)))

(cl-defstruct (harmless-openai-assembler
               (:constructor harmless-openai-assembler-create)
               (:copier nil))
  (tools (make-hash-table :test 'eql))
  finish-reason
  (emitted-stop nil)
  (saw-done nil))

(defun harmless-make-openai-compat (name &rest args)
  "Return an OpenAI-compatible provider named NAME.
Keyword ARGS: :host :protocol :endpoint :key :key-env :models
:extra-headers :stream."
  (harmless-openai-provider-create
   :name name
   :host (or (plist-get args :host) "api.openai.com")
   :protocol (or (plist-get args :protocol) "https")
   :endpoint (or (plist-get args :endpoint) "/v1/chat/completions")
   :key (plist-get args :key)
   :key-env (or (plist-get args :key-env) "OPENAI_API_KEY")
   :models (plist-get args :models)
   :extra-headers (plist-get args :extra-headers)
   :stream (if (plist-member args :stream) (plist-get args :stream) t)))

(defun harmless-make-openai (&rest args)
  "Return an OpenAI provider.
The first argument may be a connection NAME (default \"OpenAI\").
Remaining ARGS are keyword arguments as in `harmless-make-openai-compat'."
  (let ((name "OpenAI"))
    (when (and args (not (keywordp (car args))))
      (setq name (pop args)))
    (apply #'harmless-make-openai-compat name
           :host "api.openai.com"
           :key-env "OPENAI_API_KEY"
           :models (or (plist-get args :models)
                       '("gpt-5.4" "gpt-5.4-mini" "gpt-5.3-codex"
                         "gpt-5.2" "gpt-5" "gpt-4o"))
           args)))

(defun harmless-make-xai (&rest args)
  "Return an xAI / Grok provider.
The first argument may be a connection NAME (default \"xAI\").
Remaining ARGS are keyword arguments as in `harmless-make-openai-compat'."
  (let ((name "xAI"))
    (when (and args (not (keywordp (car args))))
      (setq name (pop args)))
    (apply #'harmless-make-openai-compat name
           :host "api.x.ai"
           :key-env "XAI_API_KEY"
           :models (or (plist-get args :models)
                       '("grok-4.6" "grok-4.5" "grok-4" "grok-3" "grok-3-mini"))
           args)))

(defun harmless-openai--tool-entry (asm index)
  "Return the tool-call plist for INDEX in ASM, creating it if needed."
  (or (gethash index (harmless-openai-assembler-tools asm))
      (let ((entry (list :id nil :name nil :args "")))
        (puthash index entry (harmless-openai-assembler-tools asm))
        entry)))

(defun harmless-openai-handle-payload (asm payload emit)
  "Parse JSON PAYLOAD into canonical events via EMIT, updating ASM.
PAYLOAD is a decoded plist, or a JSON string.  Returns ASM.
This is the unit-tested core of the OpenAI backend."
  (when (stringp payload)
    (setq payload (harmless-json-decode-safe payload)))
  (when payload
    (when-let* ((err (plist-get payload :error)))
      (funcall emit (list :error (or (plist-get err :message)
                                     (format "%s" err)))))
    (when-let* ((counts (harmless-openai--usage-counts
                         (plist-get payload :usage))))
      (funcall emit (list :usage (car counts) (cdr counts))))
    (when-let* ((choice (car (plist-get payload :choices))))
      (when-let* ((reason (plist-get choice :finish_reason)))
        (unless (eq reason :null)
          (setf (harmless-openai-assembler-finish-reason asm)
                (if (stringp reason) reason (format "%s" reason)))))
      (let ((delta (or (plist-get choice :delta)
                       (plist-get choice :message))))
        (when delta
          (when-let* ((text (plist-get delta :content)))
            (when (and (stringp text) (not (string-empty-p text)))
              (funcall emit (list :text text))))
          (let ((reasoning (or (plist-get delta :reasoning)
                               (plist-get delta :reasoning_content))))
            (when (and (stringp reasoning) (not (string-empty-p reasoning)))
              (funcall emit (list :reasoning reasoning))))
          (dolist (tc (plist-get delta :tool_calls))
            (let* ((idx (or (plist-get tc :index) 0))
                   (entry (harmless-openai--tool-entry asm idx))
                   (fn (plist-get tc :function))
                   (id (plist-get tc :id))
                   (name (and fn (plist-get fn :name)))
                   (args (and fn (plist-get fn :arguments))))
              (when (and id (stringp id) (not (string-empty-p id)))
                (setf (plist-get entry :id) id))
              (when (and name (stringp name) (not (string-empty-p name)))
                (setf (plist-get entry :name) name))
              (when (stringp args)
                (setf (plist-get entry :args)
                      (concat (plist-get entry :args) args))
                (funcall emit (list :tool-call
                                    (plist-get entry :id)
                                    (plist-get entry :name)
                                    args)))))))))
  asm)

(defun harmless-openai-finish (asm emit)
  "Emit a final :stop event for ASM unless one was already sent."
  (unless (harmless-openai-assembler-emitted-stop asm)
    (setf (harmless-openai-assembler-emitted-stop asm) t)
    (funcall emit (list :stop
                        (or (harmless-openai-assembler-finish-reason asm)
                            "stop"))))
  asm)

(defun harmless-openai--format-tool (tool)
  "Return an OpenAI tool object for a `harmless-tool' TOOL."
  (list :type "function"
        :function
        (list :name (harmless-tool-name tool)
              :description (or (harmless-tool-description tool) "")
              :parameters (or (harmless-tool-schema tool)
                              '(:type "object" :properties nil)))))

(defun harmless-openai--format-messages (messages)
  "Convert canonical MESSAGES to OpenAI chat messages."
  (mapcar #'harmless-openai--format-message messages))

(defun harmless-openai--format-message (msg)
  "Convert one canonical MSG plist to an OpenAI message object."
  (let ((role (plist-get msg :role)))
    (pcase role
      ((or :tool 'tool)
       (list :role "tool"
             :tool_call_id (plist-get msg :id)
             :content (or (plist-get msg :content) "")))
      ((or :system 'system "system")
       (list :role "system"
             :content (or (plist-get msg :content) "")))
      ((or :assistant 'assistant)
       (let ((out (list :role "assistant"
                        :content (or (plist-get msg :content) "")))
             (calls (plist-get msg :tool-calls)))
         (when calls
           (setq out (append out
                             (list :tool_calls
                                   (mapcar #'harmless-openai--format-tool-call
                                           calls)))))
         out))
      (_
       (list :role "user"
             :content (or (plist-get msg :content) ""))))))

(defun harmless-openai--format-tool-call (tc)
  "Convert canonical tool-call TC to OpenAI shape."
  (list :id (plist-get tc :id)
        :type "function"
        :function
        (list :name (plist-get tc :name)
              :arguments (let ((args (plist-get tc :args)))
                           (cond
                            ((stringp args) args)
                            (args (harmless-json-encode args))
                            (t "{}"))))))

(defun harmless-openai--usage-counts (usage)
  "Return (PROMPT . COMPLETION) from a usage object USAGE.
Accepts both Chat Completions and Responses field names.  Returns nil
when USAGE is not an object, which is how streaming chunks say \"no
usage yet.\""
  (when (consp usage)
    (cons (or (plist-get usage :prompt_tokens)
              (plist-get usage :input_tokens)
              0)
          (or (plist-get usage :completion_tokens)
              (plist-get usage :output_tokens)
              0))))

(defun harmless-openai--payload (_provider model messages tools stream &optional effort)
  "Build the Chat Completions request body."
  (let ((body (list :model model
                    :messages (harmless-openai--format-messages messages)
                    :stream (and stream t))))
    (when stream
      (setq body (append body (list :stream_options '(:include_usage t)))))
    (when tools
      (setq body (append body
                         (list :tools (mapcar #'harmless-openai--format-tool
                                              tools)))))
    (when effort
      (setq body (append body (list :reasoning_effort effort))))
    (harmless-json-encode body)))

(cl-defstruct (harmless-openai-responses-assembler
               (:constructor harmless-openai-responses-assembler-create)
               (:copier nil))
  (tools (make-hash-table :test 'equal))
  finish-reason
  (emitted-stop nil))

(defun harmless-openai--use-codex-p (provider)
  "Return non-nil if PROVIDER should use the ChatGPT Codex Responses API."
  (and (fboundp 'harmless-openai-official-p)
       (harmless-openai-official-p provider)
       (fboundp 'harmless-openai-token)
       (harmless-openai-token provider)))

(defun harmless-openai--event-name (type payload)
  "Return a string event name from SSE TYPE and JSON PAYLOAD."
  (cond
   ((and type (symbolp type) (not (eq type 'message)))
    (symbol-name type))
   ((and type (stringp type) (not (string= type "message")))
    type)
   ((plist-get payload :type)
    (format "%s" (plist-get payload :type)))
   (t "message")))

(defun harmless-openai--responses-tool-entry (asm key)
  "Return the Responses tool-call plist for KEY in ASM."
  (or (gethash key (harmless-openai-responses-assembler-tools asm))
      (let ((entry (list :item-id key :id nil :name nil :args "" :streamed nil)))
        (puthash key entry (harmless-openai-responses-assembler-tools asm))
        entry)))

(defun harmless-openai--responses-tool (tool)
  "Return a Responses API function tool for TOOL."
  (list :type "function"
        :name (harmless-tool-name tool)
        :description (or (harmless-tool-description tool) "")
        :parameters (or (harmless-tool-schema tool)
                        '(:type "object" :properties nil))))

(defun harmless-openai--responses-close-open (open out)
  "Append dummy function_call_output items for OPEN call ids onto OUT.
Return (OPEN . OUT) with OPEN empty.  Codex rejects a function_call
that has no matching output later in the input list."
  (dolist (id (nreverse open))
    (when id
      (push (list :type "function_call_output"
                  :call_id id
                  :output "Error: tool was not run")
            out)))
  (cons nil out))

(defun harmless-openai--responses-input (messages)
  "Convert canonical MESSAGES to a Responses API input list.
Every `function_call' is paired with a `function_call_output'.  If the
session never stored a tool result (user interrupted, new prompt, HTTP
error), a dummy output is inserted so Codex does not 400."
  (let (out open)
    (dolist (msg messages)
      (pcase (plist-get msg :role)
        ((or :system 'system "system") nil)
        ((or :tool 'tool)
         (let ((id (plist-get msg :id)))
           (when (and id (member id open))
             (setq open (cl-remove id open :test #'equal))
             (push (list :type "function_call_output"
                         :call_id id
                         :output (or (plist-get msg :content) ""))
                   out))))
        ((or :assistant 'assistant)
         (let ((closed (harmless-openai--responses-close-open open out)))
           (setq open (car closed)
                 out (cdr closed)))
         (let ((text (plist-get msg :content))
               (calls (plist-get msg :tool-calls)))
           (when (and text (not (string-empty-p text)))
             (push (list :role "assistant" :content text) out))
           (dolist (tc calls)
             (let ((id (plist-get tc :id)))
               (push (list :type "function_call"
                           :call_id id
                           :name (plist-get tc :name)
                           :arguments
                           (let ((args (plist-get tc :args)))
                             (cond
                              ((stringp args) args)
                              (args (harmless-json-encode args))
                              (t "{}"))))
                     out)
               (when id
                 (push id open))))))
        (_
         (let ((closed (harmless-openai--responses-close-open open out)))
           (setq open (car closed)
                 out (cdr closed)))
         (push (list :role "user"
                     :content (or (plist-get msg :content) ""))
               out))))
    (setq out (cdr (harmless-openai--responses-close-open open out)))
    (nreverse out)))

(declare-function harmless-messages-system-text "harmless-instructions")

(defun harmless-openai--responses-payload (_provider model messages tools stream
                                                    &optional effort)
  "Build a Codex / Responses API request body."
  (let ((body (list :model model
                    :input (harmless-openai--responses-input messages)
                    :stream (and stream t)
                    :store :false)))
    (when-let* ((system (harmless-messages-system-text messages)))
      (setq body (append body (list :instructions system))))
    (when tools
      (setq body (append body
                         (list :tools (mapcar #'harmless-openai--responses-tool
                                              tools)))))
    (when effort
      (setq body (append body (list :reasoning (list :effort effort)))))
    (harmless-json-encode body)))

(defun harmless-openai-responses-finish (asm emit)
  "Emit a final :stop event for Responses ASM unless one was already sent."
  (unless (harmless-openai-responses-assembler-emitted-stop asm)
    (setf (harmless-openai-responses-assembler-emitted-stop asm) t)
    (funcall emit (list :stop
                        (or (harmless-openai-responses-assembler-finish-reason asm)
                            "stop"))))
  asm)

(defun harmless-openai--error-text (payload)
  "Return an error string from PAYLOAD, or nil."
  (let ((err (or (plist-get payload :detail)
                 (plist-get payload :error)
                 (plist-get (plist-get payload :response) :error)
                 (plist-get payload :message))))
    (cond
     ((null err) nil)
     ((stringp err) err)
     ((plist-get err :message))
     (t (format "%s" err)))))

(defun harmless-openai--responses-emit-error (payload emit)
  "Emit an :error event from PAYLOAD if it contains one."
  (when-let* ((text (harmless-openai--error-text payload)))
    (funcall emit (list :error text))))

(defun harmless-openai--responses-full (asm payload emit)
  "Emit canonical events from a complete Responses PAYLOAD."
  (dolist (item (plist-get payload :output))
    (pcase (format "%s" (plist-get item :type))
      ("message"
       (dolist (part (plist-get item :content))
         (pcase (format "%s" (plist-get part :type))
           ("output_text"
            (when-let* ((text (plist-get part :text)))
              (when (and (stringp text) (not (string-empty-p text)))
                (funcall emit (list :text text)))))
           ("refusal"
            (funcall emit (list :error
                                (or (plist-get part :refusal)
                                    "refusal")))))))
      ("function_call"
       (funcall emit (list :tool-call
                           (or (plist-get item :call_id)
                               (plist-get item :id))
                           (plist-get item :name)
                           (or (plist-get item :arguments) ""))))
      ("reasoning"
       (dolist (part (or (plist-get item :summary)
                         (plist-get item :content)))
         (when-let* ((text (or (plist-get part :text)
                               (plist-get part :delta))))
           (when (and (stringp text) (not (string-empty-p text)))
             (funcall emit (list :reasoning text))))))))
  (when-let* ((usage (plist-get payload :usage)))
    (funcall emit (list :usage
                        (or (plist-get usage :input_tokens) 0)
                        (or (plist-get usage :output_tokens) 0))))
  (harmless-openai-responses-finish asm emit))

(defun harmless-openai-responses-handle (asm type data emit)
  "Handle one Responses SSE event TYPE with JSON DATA, updating ASM."
  (let ((payload (if (stringp data) (harmless-json-decode-safe data) data)))
    (when payload
      (let ((etype (harmless-openai--event-name type payload)))
        (cond
         ((member etype '("error" "response.failed"))
          (harmless-openai--responses-emit-error payload emit)
          (unless (plist-get payload :error)
            (funcall emit (list :error "openai error")))
          (harmless-openai-responses-finish asm emit))
         ((member etype '("response.output_text.delta" "response.text.delta"))
          (when-let* ((text (or (plist-get payload :delta)
                                (plist-get payload :text))))
            (when (and (stringp text) (not (string-empty-p text)))
              (funcall emit (list :text text)))))
         ((member etype '("response.reasoning_summary_text.delta"
                          "response.reasoning.delta"))
          (when-let* ((text (or (plist-get payload :delta)
                                (plist-get payload :text))))
            (when (and (stringp text) (not (string-empty-p text)))
              (funcall emit (list :reasoning text)))))
         ((string= etype "response.output_item.added")
          (harmless-openai--responses-item-added asm payload))
         ((string= etype "response.function_call_arguments.delta")
          (harmless-openai--responses-args-delta asm payload emit))
         ((string= etype "response.output_item.done")
          (harmless-openai--responses-item-done asm payload emit))
         ((member etype '("response.completed" "response.incomplete"))
          (let ((resp (or (plist-get payload :response) payload)))
            (when-let* ((usage (plist-get resp :usage)))
              (funcall emit (list :usage
                                  (or (plist-get usage :input_tokens) 0)
                                  (or (plist-get usage :output_tokens) 0))))
            (harmless-openai--responses-emit-error resp emit)
            (harmless-openai-responses-finish asm emit)))
         ((string= etype "message")
          (cond
           ((harmless-openai--error-text payload)
            (harmless-openai--responses-emit-error payload emit)
            (harmless-openai-responses-finish asm emit))
           ((plist-get payload :output)
            (harmless-openai--responses-full asm payload emit)))))))
    asm))

(defun harmless-openai--responses-item-added (asm payload)
  "Record function-call metadata from an output_item.added PAYLOAD."
  (when-let* ((item (plist-get payload :item)))
    (when (equal (format "%s" (plist-get item :type)) "function_call")
      (let ((entry (harmless-openai--responses-tool-entry
                    asm
                    (or (plist-get item :id)
                        (plist-get payload :item_id)
                        (plist-get payload :output_index)))))
        (when-let* ((cid (plist-get item :call_id)))
          (setf (plist-get entry :id) cid))
        (when-let* ((name (plist-get item :name)))
          (setf (plist-get entry :name) name))))))

(defun harmless-openai--responses-args-delta (asm payload emit)
  "Emit a tool-call argument delta from PAYLOAD."
  (let* ((key (or (plist-get payload :item_id)
                  (plist-get payload :output_index)))
         (delta (or (plist-get payload :delta) ""))
         (entry (harmless-openai--responses-tool-entry asm key)))
    (setf (plist-get entry :args)
          (concat (plist-get entry :args) delta)
          (plist-get entry :streamed) t)
    (funcall emit (list :tool-call
                        (or (plist-get entry :id) key)
                        (plist-get entry :name)
                        delta))))

(defun harmless-openai--responses-item-done (asm payload emit)
  "Emit a tool-call from output_item.done when arguments were not streamed."
  (when-let* ((item (plist-get payload :item)))
    (when (equal (format "%s" (plist-get item :type)) "function_call")
      (let* ((key (or (plist-get item :id)
                      (plist-get payload :item_id)))
             (entry (harmless-openai--responses-tool-entry asm key)))
        (when-let* ((cid (plist-get item :call_id)))
          (setf (plist-get entry :id) cid))
        (when-let* ((name (plist-get item :name)))
          (setf (plist-get entry :name) name))
        (unless (plist-get entry :streamed)
          (let ((args (or (plist-get item :arguments) "")))
            (setf (plist-get entry :args) args)
            (funcall emit (list :tool-call
                                (or (plist-get entry :id) key)
                                (plist-get entry :name)
                                args))))))))

(defun harmless-openai--auth-headers (provider)
  "Return Authorization and extra headers for PROVIDER.
Browser-login OAuth tokens win over an API key."
  (let* ((xai (and (harmless-xai-provider-p provider)
                   (harmless-xai-token provider)))
         (openai (and (not xai)
                      (harmless-openai--use-codex-p provider)))
         (oauth (or xai (and openai (harmless-openai-token provider))))
         (key (or oauth (harmless-provider-resolve-key provider)))
         (headers (copy-sequence (or (harmless-provider-headers provider) nil))))
    (when (and key (not (string= key "none")))
      (push (cons "Authorization" (format "Bearer %s" key)) headers)
      (when xai
        (push (cons "xai-grok-cli" key) headers))
      (when openai
        (push (cons "originator" "harmless") headers)
        (push '("OpenAI-Beta" . "responses=v1") headers)
        (push '("Accept" . "text/event-stream") headers)
        (push (cons "session_id" (harmless-uuid)) headers)
        (when-let* ((acct (harmless-openai-account-id provider)))
          (push (cons "ChatGPT-Account-Id" acct) headers))))
    headers))

(cl-defmethod harmless-provider-complete ((provider harmless-openai-provider)
                                          messages tools callback)
  "Complete MESSAGES using the OpenAI-compatible PROVIDER."
  (let* ((model (or harmless-current-model
                    (harmless-provider-default-model provider)))
         (effort harmless-current-reasoning-effort)
         (stream (harmless-provider-stream provider))
         (codex (harmless-openai--use-codex-p provider))
         (chat-asm (and (not codex) (harmless-openai-assembler-create)))
         (resp-asm (and codex (harmless-openai-responses-assembler-create)))
         (url (if codex
                  harmless-openai-codex-responses-url
                (harmless-provider-url provider)))
         (headers (harmless-openai--auth-headers provider))
         (body (if codex
                   (harmless-openai--responses-payload
                    provider model messages tools stream effort)
                 (harmless-openai--payload
                  provider model messages tools stream effort)))
         (emit (lambda (event) (funcall callback event))))
    (harmless-log "openai POST %s model=%s stream=%s" url model stream)
    (harmless-http-post-stream
     url headers body
     (lambda (type data)
       (if codex
           (pcase type
             ('done (harmless-openai-responses-finish resp-asm emit))
             ('error
              (funcall emit (list :error data))
              (harmless-openai-responses-finish resp-asm emit))
             (_ (harmless-openai-responses-handle resp-asm type data emit)))
         (pcase type
           ('done
            (setf (harmless-openai-assembler-saw-done chat-asm) t)
            (harmless-openai-finish chat-asm emit))
           ('error
            (funcall emit (list :error data))
            (harmless-openai-finish chat-asm emit))
           ('message
            (harmless-openai-handle-payload chat-asm data emit)))))
     (lambda (status)
       (let ((stopped (if codex
                          (harmless-openai-responses-assembler-emitted-stop
                           resp-asm)
                        (harmless-openai-assembler-emitted-stop chat-asm))))
         (pcase status
           ('abort (funcall emit '(:error "aborted")))
           ('error
            (unless stopped
              (funcall emit '(:error "http error"))))
           ('ok
            (unless stopped
              (if codex
                  (harmless-openai-responses-finish resp-asm emit)
                (harmless-openai-finish chat-asm emit))))))
     (lambda (limits)
       (when limits
         (funcall emit (list :limits limits))))))))

(provide 'harmless-openai)

;;; harmless-openai.el ends here
