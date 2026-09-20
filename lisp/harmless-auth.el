;;; harmless-auth.el --- API key lookup for Harmless -*- lexical-binding: t; -*-

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
;; Resolve API keys from an explicit value, the environment, then auth-source.
;; Keys are never written to the log.

;;; Code:

(require 'auth-source)
(require 'subr-x)
(require 'harmless-util)

(defun harmless-auth-key (host key key-env)
  "Return an API key for HOST.
KEY wins if it is a non-empty string or a function of no arguments.
Otherwise KEY-ENV is read with `getenv'.  Finally auth-source is
searched for HOST with user `apikey'."
  (or (cond
       ((functionp key) (funcall key))
       ((and (stringp key) (not (string-empty-p key))) key)
       (t nil))
      (and key-env (getenv key-env))
      (harmless-auth--auth-source host)))

(defun harmless-auth--auth-source (host)
  "Return a secret from auth-source for HOST, if any."
  (when host
    (let ((found (nth 0 (auth-source-search
                         :host host
                         :user "apikey"
                         :max 1
                         :require '(:secret)))))
      (when found
        (let ((secret (plist-get found :secret)))
          (if (functionp secret) (funcall secret) secret))))))

(provide 'harmless-auth)

;;; harmless-auth.el ends here
