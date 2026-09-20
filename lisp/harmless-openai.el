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
;; other OpenAI-compatible host.  Assembles canonical events from deltas.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-log)
(require 'harmless-http)
(require 'harmless-provider)

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
  "Return an OpenAI provider.  ARGS are as `harmless-make-openai-compat'."
  (apply #'harmless-make-openai-compat "OpenAI"
         :host "api.openai.com"
         :key-env "OPENAI_API_KEY"
         :models (or (plist-get args :models)
                     '("gpt-4o" "gpt-4.1" "o4-mini"))
         args))

(defun harmless-make-xai (&rest args)
  "Return an xAI / Grok provider.  ARGS are as `harmless-make-openai-compat'."
  (apply #'harmless-make-openai-compat "xAI"
         :host "api.x.ai"
         :key-env "XAI_API_KEY"
         :models (or (plist-get args :models)
                     '("grok-4.6" "grok-4.5" "grok-4" "grok-3" "grok-3-mini"))
         args))

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
    (when-let* ((usage (plist-get payload :usage)))
      (funcall emit (list :usage
                          (or (plist-get usage :prompt_tokens) 0)
                          (or (plist-get usage :completion_tokens) 0))))
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

(defun harmless-openai--payload (_provider model messages tools stream)
  "Build the Chat Completions request body."
  (let ((body (list :model model
                    :messages (harmless-openai--format-messages messages)
                    :stream (and stream t))))
    (when tools
      (setq body (append body
                         (list :tools (mapcar #'harmless-openai--format-tool
                                              tools)))))
    (harmless-json-encode body)))

(defun harmless-openai--auth-headers (provider)
  "Return Authorization and extra headers for PROVIDER."
  (let* ((key (harmless-provider-resolve-key provider))
         (headers (copy-sequence (or (harmless-provider-headers provider) nil))))
    (when (and key (not (string= key "none")))
      (push (cons "Authorization" (format "Bearer %s" key)) headers))
    headers))

(cl-defmethod harmless-provider-complete ((provider harmless-openai-provider)
                                          messages tools callback)
  "Complete MESSAGES using the OpenAI-compatible PROVIDER."
  (let* ((model (or harmless-current-model
                    (harmless-provider-default-model provider)))
         (stream (harmless-provider-stream provider))
         (asm (harmless-openai-assembler-create))
         (url (harmless-provider-url provider))
         (headers (harmless-openai--auth-headers provider))
         (body (harmless-openai--payload provider model messages tools stream))
         (emit (lambda (event) (funcall callback event))))
    (harmless-log "openai POST %s model=%s stream=%s" url model stream)
    (harmless-http-post-stream
     url headers body
     (lambda (type data)
       (pcase type
         ('done
          (setf (harmless-openai-assembler-saw-done asm) t)
          (harmless-openai-finish asm emit))
         ('error
          (funcall emit (list :error data)))
         ('message
          (harmless-openai-handle-payload asm data emit))))
     (lambda (status)
       (pcase status
         ('abort (funcall emit '(:error "aborted")))
         ('error
          (unless (harmless-openai-assembler-emitted-stop asm)
            (funcall emit '(:error "http error"))))
         ('ok
          (unless (harmless-openai-assembler-emitted-stop asm)
            (harmless-openai-finish asm emit))))))))

(provide 'harmless-openai)

;;; harmless-openai.el ends here
