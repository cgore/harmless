;;; harmless-anthropic.el --- Anthropic Messages provider for Harmless -*- lexical-binding: t; -*-

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
;; Anthropic Messages API with tool_use and thinking, mapped to canonical
;; Harmless events.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-log)
(require 'harmless-http)
(require 'harmless-provider)
(require 'harmless-anthropic-oauth)

(declare-function harmless-tool-name "harmless-tools")
(declare-function harmless-tool-description "harmless-tools")
(declare-function harmless-tool-schema "harmless-tools")

(cl-defstruct (harmless-anthropic-provider
               (:include harmless-provider)
               (:constructor harmless-anthropic-provider-create)
               (:copier nil)))

(cl-defstruct (harmless-anthropic-assembler
               (:constructor harmless-anthropic-assembler-create)
               (:copier nil))
  (blocks (make-hash-table :test 'eql))
  stop-reason
  (emitted-stop nil))

(defun harmless-make-anthropic (name &rest args)
  "Return an Anthropic provider named NAME.
NAME may be omitted if the first argument is a keyword, in which case
the provider is named \"Anthropic\".  Keyword ARGS: :host :protocol
:endpoint :key :key-env :models :extra-headers :stream."
  (when (keywordp name)
    (setq args (cons name args)
          name "Anthropic"))
  (harmless-anthropic-provider-create
   :name (or name "Anthropic")
   :host (or (plist-get args :host) "api.anthropic.com")
   :protocol (or (plist-get args :protocol) "https")
   :endpoint (or (plist-get args :endpoint) "/v1/messages")
   :key (plist-get args :key)
   :key-env (or (plist-get args :key-env) "ANTHROPIC_API_KEY")
   :models (or (plist-get args :models)
               '("claude-sonnet-4-6"
                 "claude-sonnet-5"
                 "claude-opus-4-6"
                 "claude-opus-4-7"
                 "claude-opus-4-8"
                 "claude-opus-5"
                 "claude-sonnet-4-5"
                 "claude-haiku-4-5"))
   :extra-headers (plist-get args :extra-headers)
   :stream (if (plist-member args :stream) (plist-get args :stream) t)))

(defun harmless-anthropic--block (asm index)
  "Return the content-block plist for INDEX in ASM."
  (or (gethash index (harmless-anthropic-assembler-blocks asm))
      (let ((entry (list :type nil :id nil :name nil :args "" :text "")))
        (puthash index entry (harmless-anthropic-assembler-blocks asm))
        entry)))

(defun harmless-anthropic--sym (x)
  "Intern X if it is a string."
  (cond
   ((symbolp x) x)
   ((stringp x) (intern x))
   (t x)))

(defun harmless-anthropic-handle-event (asm type data emit)
  "Handle one Anthropic SSE event TYPE with JSON DATA string, updating ASM."
  (let ((payload (if (stringp data) (harmless-json-decode-safe data) data)))
    (when payload
    (let ((etype (harmless-anthropic--sym
                  (or type (plist-get payload :type) 'message))))
      (pcase etype
        ((or 'error 'message_error)
         (funcall emit (list :error (or (plist-get payload :error)
                                        (plist-get payload :message)
                                        "anthropic error"))))
        ('content_block_start
         (let* ((index (plist-get payload :index))
                (block (plist-get payload :content_block))
                (entry (harmless-anthropic--block asm index))
                (btype (harmless-anthropic--sym (plist-get block :type))))
           (setf (plist-get entry :type) btype)
           (when (eq btype 'tool_use)
             (setf (plist-get entry :id) (plist-get block :id)
                   (plist-get entry :name) (plist-get block :name)))))
        ('content_block_delta
         (let* ((index (plist-get payload :index))
                (delta (plist-get payload :delta))
                (entry (harmless-anthropic--block asm index))
                (dtype (harmless-anthropic--sym (plist-get delta :type))))
           (pcase dtype
             ((or 'text_delta 'text)
              (let ((text (or (plist-get delta :text) "")))
                (setf (plist-get entry :text)
                      (concat (plist-get entry :text) text))
                (funcall emit (list :text text))))
             ((or 'thinking_delta 'thinking)
              (funcall emit (list :reasoning (or (plist-get delta :thinking)
                                                 (plist-get delta :text)
                                                 ""))))
             ((or 'input_json_delta 'input_json)
              (let ((partial (or (plist-get delta :partial_json) "")))
                (setf (plist-get entry :args)
                      (concat (plist-get entry :args) partial))
                (funcall emit (list :tool-call
                                    (plist-get entry :id)
                                    (plist-get entry :name)
                                    partial)))))))
        ('message_delta
         (let ((delta (plist-get payload :delta))
               (usage (plist-get payload :usage)))
           (when-let* ((reason (and delta (plist-get delta :stop_reason))))
             (setf (harmless-anthropic-assembler-stop-reason asm)
                   (if (stringp reason) reason (format "%s" reason))))
           (when usage
             (funcall emit (list :usage
                                 (or (plist-get usage :input_tokens) 0)
                                 (or (plist-get usage :output_tokens) 0))))))
        ('message_stop
         (harmless-anthropic-finish asm emit))
        ('message
         ;; Non-streaming full message object.
         (dolist (block (plist-get payload :content))
           (pcase (plist-get block :type)
             ((or 'text "text")
              (funcall emit (list :text (or (plist-get block :text) ""))))
             ((or 'thinking "thinking")
              (funcall emit (list :reasoning (or (plist-get block :thinking) ""))))
             ((or 'tool_use "tool_use")
              (funcall emit (list :tool-call
                                  (plist-get block :id)
                                  (plist-get block :name)
                                  (harmless-json-encode
                                   (or (plist-get block :input) '())))))))
         (when-let* ((usage (plist-get payload :usage)))
           (funcall emit (list :usage
                               (or (plist-get usage :input_tokens) 0)
                               (or (plist-get usage :output_tokens) 0))))
         (setf (harmless-anthropic-assembler-stop-reason asm)
               (or (plist-get payload :stop_reason) "stop"))
         (harmless-anthropic-finish asm emit)))))
    asm))

(defun harmless-anthropic-finish (asm emit)
  "Emit :stop for ASM unless already emitted."
  (unless (harmless-anthropic-assembler-emitted-stop asm)
    (setf (harmless-anthropic-assembler-emitted-stop asm) t)
    (funcall emit (list :stop
                        (or (harmless-anthropic-assembler-stop-reason asm)
                            "end_turn"))))
  asm)

(defun harmless-anthropic--format-tools (tools)
  "Convert TOOLS to Anthropic input_schema tools."
  (mapcar (lambda (tool)
            (list :name (harmless-tool-name tool)
                  :description (or (harmless-tool-description tool) "")
                  :input_schema (or (harmless-tool-schema tool)
                                    '(:type "object" :properties nil))))
          tools))

(defun harmless-anthropic--format-messages (messages)
  "Convert canonical MESSAGES to Anthropic messages (no system)."
  (let (out pending-tools)
    (dolist (msg messages)
      (let ((role (plist-get msg :role)))
        (pcase role
          ((or :tool 'tool)
           (push (list :type "tool_result"
                       :tool_use_id (plist-get msg :id)
                       :content (or (plist-get msg :content) ""))
                 pending-tools))
          (_
           (when pending-tools
             (push (list :role "user" :content (nreverse pending-tools)) out)
             (setq pending-tools nil))
           (push (harmless-anthropic--format-one msg) out)))))
    (when pending-tools
      (push (list :role "user" :content (nreverse pending-tools)) out))
    (nreverse out)))

(defun harmless-anthropic--format-one (msg)
  "Format one non-tool MSG."
  (let ((role (plist-get msg :role)))
    (pcase role
      ((or :assistant 'assistant)
       (let ((content nil)
             (text (plist-get msg :content))
             (calls (plist-get msg :tool-calls)))
         (when (and text (not (string-empty-p text)))
           (push (list :type "text" :text text) content))
         (dolist (tc calls)
           (push (list :type "tool_use"
                       :id (plist-get tc :id)
                       :name (plist-get tc :name)
                       :input (let ((args (plist-get tc :args)))
                                (cond
                                 ((stringp args)
                                  (or (harmless-json-decode-safe args) '()))
                                 (args args)
                                 (t '()))))
                 content))
         (list :role "assistant" :content (or (nreverse content)
                                              (list (list :type "text" :text ""))))))
      (_
       (list :role "user" :content (or (plist-get msg :content) ""))))))

(defun harmless-anthropic--payload (provider model messages tools stream &optional effort)
  "Build a Messages API body."
  (ignore provider)
  (let ((body (list :model model
                    :max_tokens (if (member effort '("xhigh" "max")) 64000 32000)
                    :messages (harmless-anthropic--format-messages messages)
                    :stream (and stream t))))
    (when tools
      (setq body (append body (list :tools (harmless-anthropic--format-tools tools)))))
    (when effort
      (setq body (append body (list :output_config (list :effort effort)))))
    (harmless-json-encode body)))

(defun harmless-anthropic--headers (provider)
  "Return Anthropic HTTP headers for PROVIDER.
An OAuth access token is sent as Bearer with the oauth beta header.
An API key is sent as x-api-key."
  (let* ((oauth (and (fboundp 'harmless-anthropic-token)
                     (harmless-anthropic-token provider)))
         (key (harmless-provider-resolve-key provider))
         (headers (copy-sequence (or (harmless-provider-headers provider) nil))))
    (push '("anthropic-version" . "2023-06-01") headers)
    (push '("Content-Type" . "application/json") headers)
    (cond
     (oauth
      (push (cons "Authorization" (format "Bearer %s" oauth)) headers)
      (push (cons "anthropic-beta" harmless-anthropic-oauth-beta) headers))
     (key
      (push (cons "x-api-key" key) headers)))
    headers))

(cl-defmethod harmless-provider-complete ((provider harmless-anthropic-provider)
                                          messages tools callback)
  "Complete MESSAGES using Anthropic PROVIDER."
  (let* ((model (or harmless-current-model
                    (harmless-provider-default-model provider)))
         (stream (harmless-provider-stream provider))
         (asm (harmless-anthropic-assembler-create))
         (url (harmless-provider-url provider))
         (headers (harmless-anthropic--headers provider))
         (body (harmless-anthropic--payload provider model messages tools stream
                                            harmless-current-reasoning-effort))
         (emit (lambda (event) (funcall callback event))))
    (harmless-log "anthropic POST %s model=%s" url model)
    (harmless-http-post-stream
     url headers body
     (lambda (type data)
       (pcase type
         ('done (harmless-anthropic-finish asm emit))
         ('error (funcall emit (list :error data)))
         (_ (harmless-anthropic-handle-event asm type data emit))))
     (lambda (status)
       (pcase status
         ('abort (funcall emit '(:error "aborted")))
         ('error
          (unless (harmless-anthropic-assembler-emitted-stop asm)
            (funcall emit '(:error "http error"))))
         ('ok
          (unless (harmless-anthropic-assembler-emitted-stop asm)
            (harmless-anthropic-finish asm emit))))))))

(provide 'harmless-anthropic)

;;; harmless-anthropic.el ends here
