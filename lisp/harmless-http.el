;;; harmless-http.el --- HTTP and SSE client for Harmless -*- lexical-binding: t; -*-

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
;; POST JSON over curl (SSE, unbuffered) with a url.el fallback.  The SSE
;; parser is pure and unit-tested; curl is only the transport.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'harmless-util)
(require 'harmless-log)

(defcustom harmless-curl-program "curl"
  "Curl executable used for streaming HTTP.
Nil means always use `url-retrieve'."
  :type '(choice (const :tag "Use url.el" nil) string)
  :group 'harmless)

(cl-defstruct (harmless-sse-state
               (:constructor harmless-sse-state-create)
               (:copier nil))
  (buf ""))

(defun harmless-http-use-curl-p ()
  "Return non-nil if streaming curl is available."
  (and harmless-curl-program
       (executable-find harmless-curl-program)))

(defun harmless-http-sse-push (state chunk emit)
  "Feed CHUNK into SSE STATE and call EMIT for each complete event.
EMIT is called as (EMIT TYPE DATA).  TYPE is the event name as a
symbol (`message' if omitted), or `done' when the payload is `[DONE]'.
DATA is the joined data string, or nil for `done'."
  (let ((buf (concat (harmless-sse-state-buf state)
                     (string-replace "\r\n" "\n" chunk))))
    (while-let ((end (string-search "\n\n" buf)))
      (let ((block (substring buf 0 end)))
        (setq buf (substring buf (+ end 2)))
        (harmless-http--sse-dispatch-block block emit)))
    (setf (harmless-sse-state-buf state) buf)
    state))

(defun harmless-http-sse-flush (state emit)
  "Dispatch any trailing SSE block in STATE that lacked a final blank line."
  (let ((buf (string-trim (harmless-sse-state-buf state))))
    (setf (harmless-sse-state-buf state) "")
    (unless (string-empty-p buf)
      (harmless-http--sse-dispatch-block buf emit))))

(defun harmless-http--sse-dispatch-block (block emit)
  "Parse one SSE BLOCK and call EMIT if it contains data."
  (let (event data-parts)
    (dolist (line (split-string block "\n"))
      (cond
       ((string-prefix-p ":" line))
       ((string-prefix-p "event:" line)
        (setq event (intern (string-trim (substring line 6)))))
       ((string-prefix-p "data:" line)
        (let ((payload (substring line 5)))
          (when (string-prefix-p " " payload)
            (setq payload (substring payload 1)))
          (push payload data-parts)))))
    (cond
     (data-parts
      (let ((data (string-join (nreverse data-parts) "\n")))
        (if (string= data "[DONE]")
            (funcall emit 'done nil)
          (funcall emit (or event 'message) data))))
     ;; HTTP error bodies are raw JSON, not SSE.
     ((and (not event)
           (string-match-p "\\`[ \t\n]*[{[]" block))
      (funcall emit 'message (string-trim block))))))

(defun harmless-http-post-stream (url headers body on-event on-done)
  "POST BODY to URL as JSON and stream SSE events.
HEADERS is an alist of extra header names to values.  ON-EVENT is
(TYPE DATA) as in `harmless-http-sse-push'.  ON-DONE is called with
`ok', `error', or `abort'.  Returns the process, or nil if curl is
unavailable (then `url-retrieve' is used and ON-DONE runs later)."
  (if (harmless-http-use-curl-p)
      (harmless-http--curl-stream url headers body on-event on-done)
    (harmless-http--url-post url headers body on-event on-done)
    nil))

(defun harmless-http-abort (process)
  "Abort PROCESS if it is a live process."
  (when (and process (process-live-p process))
    (process-put process 'harmless-aborted t)
    (delete-process process)))

(defun harmless-http--status-from-headers (text)
  "Return the last HTTP status code in TEXT, or nil."
  (let (code)
    (dolist (line (split-string (or text "") "\n"))
      (when (string-match "HTTP/[0-9.]+ \\([0-9]+\\)" line)
        (setq code (string-to-number (match-string 1 line)))))
    code))

(defun harmless-http--curl-stream (url headers body on-event on-done)
  "Start a curl process for URL.  Return the process."
  (let* ((header-file (make-temp-file "harmless-http-hdr-"))
         (args (append (list harmless-curl-program
                             "-sS" "-N" "--no-buffer"
                             "-D" header-file
                             "-X" "POST"
                             "-H" "Content-Type: application/json"
                             "--data-binary" "@-")
                       (mapcan (lambda (h)
                                 (list "-H" (format "%s: %s" (car h) (cdr h))))
                               headers)
                       (list url)))
         (proc (make-process
                :name "harmless-http"
                :buffer nil
                :command args
                :connection-type 'pipe
                :coding '(utf-8-unix . utf-8-unix)
                :stderr (get-buffer-create " *harmless-curl-stderr*")
                :filter #'harmless-http--curl-filter
                :sentinel #'harmless-http--curl-sentinel)))
    (process-put proc 'harmless-sse (harmless-sse-state-create))
    (process-put proc 'harmless-on-event on-event)
    (process-put proc 'harmless-on-done on-done)
    (process-put proc 'harmless-aborted nil)
    (process-put proc 'harmless-header-file header-file)
    (process-send-string proc body)
    (process-send-eof proc)
    proc))

(defun harmless-http--curl-filter (proc chunk)
  "Process filter: parse SSE CHUNK for PROC."
  (let ((state (process-get proc 'harmless-sse))
        (on-event (process-get proc 'harmless-on-event)))
    (when (and state on-event)
      (harmless-http-sse-push state chunk on-event))))

(defun harmless-http--curl-sentinel (proc _change)
  "Process sentinel for curl PROC."
  (unless (process-live-p proc)
    (let* ((on-event (process-get proc 'harmless-on-event))
           (on-done (process-get proc 'harmless-on-done))
           (state (process-get proc 'harmless-sse))
           (aborted (process-get proc 'harmless-aborted))
           (status (process-exit-status proc))
           (header-file (process-get proc 'harmless-header-file))
           (http (and header-file (file-readable-p header-file)
                      (harmless-http--status-from-headers
                       (with-temp-buffer
                         (insert-file-contents header-file)
                         (buffer-string)))))
           (http-err (and (numberp http) (>= http 400))))
      (when (and header-file (file-exists-p header-file))
        (ignore-errors (delete-file header-file)))
      (when (and state on-event)
        (harmless-http-sse-flush state on-event))
      (when (and http-err on-event)
        (harmless-log "http %s" http)
        (funcall on-event 'error (format "http %s" http)))
      (when on-done
        (funcall on-done
                 (cond
                  (aborted 'abort)
                  (http-err 'error)
                  ((eq status 0) 'ok)
                  (t 'error)))))))

(defun harmless-http--url-post (url headers body on-event on-done)
  "Non-streaming POST via `url-retrieve'."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         (cons '("Content-Type" . "application/json") headers))
        (url-request-data body))
    (url-retrieve
     url
     (lambda (status)
       (let ((err (plist-get status :error)))
         (if err
             (progn
               (funcall on-event 'error (format "%s" err))
               (funcall on-done 'error))
           (goto-char (point-min))
           (when (re-search-forward "\n\n" nil t)
             (let ((payload (buffer-substring-no-properties (point) (point-max))))
               (funcall on-event 'message (string-trim payload))))
           (funcall on-done 'ok))))
     nil t t)))

(provide 'harmless-http)

;;; harmless-http.el ends here
