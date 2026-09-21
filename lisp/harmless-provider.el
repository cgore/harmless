;;; harmless-provider.el --- Provider protocol for Harmless -*- lexical-binding: t; -*-

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
;; Provider structs and the complete/abort generic functions.  Backends emit
;; canonical events: (:text S) (:reasoning S) (:tool-call ID NAME DELTA)
;; (:usage PROMPT COMPLETION) (:stop REASON) (:error ERR).

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'harmless-util)
(require 'harmless-auth)
(require 'harmless-http)

(cl-defstruct (harmless-provider
               (:constructor nil)
               (:copier nil))
  name
  host
  protocol
  endpoint
  key
  key-env
  models
  extra-headers
  (stream t))

(defcustom harmless-providers nil
  "List of `harmless-provider' objects.
Build them with `harmless-make-openai-compat', `harmless-make-xai',
`harmless-make-openai', or `harmless-make-anthropic'."
  :type '(repeat sexp)
  :group 'harmless)

(defcustom harmless-default-provider-name nil
  "Name of the default provider, matching `harmless-provider-name'.
Nil means the first entry of `harmless-providers'."
  :type '(choice (const nil) string)
  :group 'harmless)

(defcustom harmless-default-model nil
  "Default model id for new sessions.
Nil means the first model advertised by the default provider."
  :type '(choice (const nil) string)
  :group 'harmless)

(defconst harmless-reasoning-efforts '("low" "medium" "high" "xhigh")
  "Known reasoning-effort values for Grok.")

(defconst harmless-anthropic-reasoning-efforts
  '("low" "medium" "high" "xhigh" "max")
  "Known effort values for Claude (`output_config.effort`).")

(defcustom harmless-default-reasoning-effort nil
  "Default reasoning effort when a session does not set its own.
Nil omits the parameter (grok-4.6 then uses high).  Typical values:
low, medium, high, xhigh."
  :type '(choice (const :tag "Provider default" nil)
                 (const "low")
                 (const "medium")
                 (const "high")
                 (const "xhigh")
                 string)
  :group 'harmless)

(defvar harmless-current-model nil
  "Model id for the in-flight completion, bound by the turn loop.")

(defvar harmless-current-reasoning-effort nil
  "Reasoning effort for the in-flight completion, bound by the turn loop.")

(defun harmless-parse-model-spec (spec)
  "Return (MODEL . EFFORT) from SPEC.
SPEC may be \"grok-4.6\" or \"grok-4.6-xhigh\"."
  (if (and spec
           (string-match "\\`\\(.+\\)-\\(low\\|medium\\|high\\|xhigh\\|max\\|ultra\\)\\'" spec))
      (cons (match-string 1 spec) (match-string 2 spec))
    (cons spec nil)))

(defun harmless-parse-model-label (label)
  "Return (MODEL . EFFORT) from a display LABEL.
Accepts \"grok-4.6\", \"grok-4.6 (xhigh)\", or \"grok-4.6-xhigh\"."
  (cond
   ((and label
         (string-match "\\`\\(.+\\) (\\(low\\|medium\\|high\\|xhigh\\|max\\|ultra\\))\\'" label))
    (cons (match-string 1 label) (match-string 2 label)))
   (t (harmless-parse-model-spec label))))

(defun harmless-model-label (model &optional effort)
  "Return MODEL with optional EFFORT in parentheses."
  (cond
   ((and model effort) (format "%s (%s)" model effort))
   (model model)
   (t "?")))

(defun harmless-model-supports-effort-p (model)
  "Return non-nil if MODEL accepts a reasoning-effort parameter."
  (and model
       (or (string-match-p "\\`grok-4" model)
           (string-match-p "\\`gpt-5" model)
           (string-match-p "\\`gpt-6" model)
           (string-equal model "gpt-reserve")
           (and (string-prefix-p "claude-" model)
                (not (string-match-p "haiku" model))
                (not (string-match-p "sonnet-4-5" model))
                (not (string-match-p "opus-4\\'" model))))))

(defun harmless-model-effort-levels (model)
  "Return allowed effort strings for MODEL, or nil."
  (cond
   ((not (harmless-model-supports-effort-p model)) nil)
   ((string-match-p "\\`gpt-6\\|gpt-5\\.6-sol\\|gpt-5\\.6-terra" model)
    '("low" "medium" "high" "xhigh" "max" "ultra"))
   ((string-match-p "gpt-5\\.6-luna\\|gpt-reserve" model)
    '("low" "medium" "high" "xhigh" "max"))
   ((or (string-prefix-p "grok-" model)
        (string-prefix-p "gpt-5" model)
        (string-prefix-p "gpt-" model))
    harmless-reasoning-efforts)
   ((string-match-p "sonnet-4-6\\|opus-4-6" model)
    '("low" "medium" "high" "max"))
   ((string-prefix-p "claude-" model)
    harmless-anthropic-reasoning-efforts)
   (t harmless-reasoning-efforts)))

(defun harmless-provider-login-vendor (provider)
  "Return the login-method id for PROVIDER, or nil if it has no browser login."
  (cond
   ((and (fboundp 'harmless-xai-provider-p)
         (harmless-xai-provider-p provider))
    'xai)
   ((and (fboundp 'harmless-anthropic-provider-p)
         (harmless-anthropic-provider-p provider))
    'anthropic)
   ((and (fboundp 'harmless-openai-official-p)
         (harmless-openai-official-p provider))
    'openai)
   (t nil)))

(defun harmless-provider-model-list (provider)
  "Return model ids currently offered by PROVIDER.
ChatGPT OAuth uses the Codex catalog rather than api.openai.com ids."
  (cond
   ((and provider
         (fboundp 'harmless-openai-official-p)
         (harmless-openai-official-p provider)
         (fboundp 'harmless-openai-token)
         (harmless-openai-token provider)
         (boundp 'harmless-openai-codex-models))
    harmless-openai-codex-models)
   (t (and provider (harmless-provider-models provider)))))

(defun harmless-model-candidates (provider)
  "Return completing-read candidates for PROVIDER's models.
Reasoning models are expanded to one entry per effort level."
  (let (out)
    (dolist (model (or (harmless-provider-model-list provider) nil))
      (if (harmless-model-supports-effort-p model)
          (dolist (effort harmless-reasoning-efforts)
            (push (harmless-model-label model effort) out))
        (push model out)))
    (nreverse out)))

(defun harmless-provider-has-key-p (provider)
  "Return non-nil if PROVIDER has an API key configured."
  (let ((key (harmless-provider-resolve-key provider)))
    (and key (not (string-empty-p key)))))

(defun harmless-provider-available-p (provider)
  "Return non-nil if PROVIDER has a usable login or API key."
  (cond
   ((and (fboundp 'harmless-xai-provider-p)
         (harmless-xai-provider-p provider))
    (or (and (fboundp 'harmless-xai-token) (harmless-xai-token provider))
        (harmless-provider-has-key-p provider)))
   ((and (fboundp 'harmless-anthropic-provider-p)
         (harmless-anthropic-provider-p provider))
    (or (and (fboundp 'harmless-anthropic-token)
             (harmless-anthropic-token provider))
        (harmless-provider-has-key-p provider)))
   ((and (fboundp 'harmless-openai-official-p)
         (harmless-openai-official-p provider))
    (or (and (fboundp 'harmless-openai-token)
             (harmless-openai-token provider))
        (harmless-provider-has-key-p provider)))
   (t (harmless-provider-has-key-p provider))))

(defun harmless-available-providers ()
  "Return configured providers that Harmless can currently use."
  (cl-remove-if-not #'harmless-provider-available-p harmless-providers))

(cl-defgeneric harmless-provider-complete (provider messages tools callback)
  "Ask PROVIDER to complete MESSAGES with TOOLS.
CALLBACK is called with one canonical event list at a time.
Returns a process object that `harmless-provider-abort' can cancel,
or nil for a non-streaming request.")

(cl-defgeneric harmless-provider-abort (provider process)
  "Abort PROCESS previously returned by `harmless-provider-complete'."
  (ignore provider)
  (harmless-http-abort process))

(cl-defgeneric harmless-provider-capabilities (provider)
  "Return a list of capability symbols for PROVIDER."
  (ignore provider)
  '(stream tools))

(defun harmless-provider-url (provider)
  "Return the HTTP URL for PROVIDER."
  (format "%s://%s%s"
          (or (harmless-provider-protocol provider) "https")
          (harmless-provider-host provider)
          (or (harmless-provider-endpoint provider) "")))

(defun harmless-provider-resolve-key (provider)
  "Return PROVIDER's API key via `harmless-auth-key'."
  (harmless-auth-key (harmless-provider-host provider)
                     (harmless-provider-key provider)
                     (harmless-provider-key-env provider)))

(defun harmless-provider-headers (provider)
  "Return extra headers for PROVIDER as an alist."
  (harmless-provider-extra-headers provider))

(defun harmless-default-provider ()
  "Return the default provider object, or nil."
  (cond
   ((and harmless-default-provider-name harmless-providers)
    (or (cl-find harmless-default-provider-name harmless-providers
                 :key #'harmless-provider-name :test #'string=)
        (car harmless-providers)))
   (t (car harmless-providers))))

(defun harmless-provider-default-model (provider)
  "Return a model id for PROVIDER."
  (or (let ((models (harmless-provider-model-list provider)))
        (and harmless-default-model
             (member harmless-default-model models)
             harmless-default-model))
      (car (harmless-provider-model-list provider))))

(defun harmless-register-provider (provider)
  "Add PROVIDER to `harmless-providers' by name, replacing any previous."
  (setq harmless-providers
        (cons provider
              (cl-remove (harmless-provider-name provider)
                         harmless-providers
                         :key #'harmless-provider-name
                         :test #'string=)))
  provider)

(provide 'harmless-provider)

;;; harmless-provider.el ends here
