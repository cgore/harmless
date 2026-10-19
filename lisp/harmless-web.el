;;; harmless-web.el --- Web search and fetch for Harmless -*- lexical-binding: t; -*-

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
;; `web_search' sends a query to DuckDuckGo over https and returns the
;; title, URL, and snippet of each result.  `web_fetch' reads one http
;; or https page and returns its text.  Both tools use the class
;; `network', so accept-edits still asks.  Nothing is written into the
;; project.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'harmless-util)
(require 'harmless-tools)

(defvar harmless-web-retrieve-function #'harmless-web-retrieve-default
  "Function (URL) that returns a response body.
URL is already known to be http or https.")

(defvar harmless-web-search-function #'harmless-web-search-default
  "Function (QUERY) that returns result plists.
Each plist has :title, :url, and :snippet.  QUERY is a non-empty
string.")

(defconst harmless-web-search-limit 8
  "Most results one search returns.")

(defun harmless-web-url (value)
  "Return VALUE when it is an http or https URL, or signal.
Nil, \"\", and whitespace are empty.  Any other scheme is refused,
including a filesystem path."
  (cond
   ((null value) (error "url is empty"))
   ((not (stringp value)) (error "url must be a string"))
   (t
    (setq value (string-trim value))
    (cond
     ((string-empty-p value) (error "url is empty"))
     ((string-match-p "[ \t\n\r]" value)
      (error "url must be http or https"))
     ((not (string-match "\\`\\([a-zA-Z][a-zA-Z0-9+.-]*\\):" value))
      (error "url must be http or https"))
     ((not (member (downcase (match-string 1 value)) '("http" "https")))
      (error "url must be http or https"))
     ((let ((case-fold-search t))
        (not (string-match-p "\\`https?://" value)))
      (error "url must be http or https"))
     (t value)))))

(defun harmless-web-get (url)
  "Return the body of URL.
URL must be http or https.  The project is not written."
  (funcall harmless-web-retrieve-function (harmless-web-url url)))

(defun harmless-web-retrieve-default (url)
  "GET URL with `url-retrieve-synchronously' and return the body."
  (let ((buf (url-retrieve-synchronously url t t 20)))
    (unless (buffer-live-p buf)
      (error "web request failed"))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (unless (re-search-forward "\n\n" nil t)
            (error "web request failed"))
          (let ((status (bound-and-true-p url-http-response-status)))
            (when (and (integerp status) (not (<= 200 status 299)))
              (error "web request failed: %s" status))
            (buffer-substring-no-properties (point) (point-max))))
      (kill-buffer buf))))

(defun harmless-web-search-url (query)
  "Return the https search URL for QUERY."
  (concat "https://api.duckduckgo.com/?format=json&no_html=1"
          "&skip_disambig=1&q="
          (url-hexify-string query)))

(defun harmless-web--title (text)
  "Return a short title taken from TEXT."
  (setq text (string-trim text))
  (when (string-match " - " text)
    (setq text (substring text 0 (match-beginning 0))))
  (if (> (length text) 120)
      (substring text 0 120)
    text))

(defun harmless-web--result (text url)
  "Return one result plist for TEXT at URL, or nil."
  (when (and (stringp text) (not (string-empty-p (string-trim text)))
             (stringp url) (not (string-empty-p (string-trim url))))
    (list :title (harmless-web--title text)
          :url (string-trim url)
          :snippet (string-trim text))))

(defun harmless-web--collect (item acc)
  "Push ITEM and its nested topics onto ACC.  Return the new list."
  (when (consp item)
    (let ((topics (plist-get item :Topics)))
      (if topics
          (dolist (child topics)
            (setq acc (harmless-web--collect child acc)))
        (let ((one (harmless-web--result (plist-get item :Text)
                                        (plist-get item :FirstURL))))
          (when one
            (push one acc))))))
  acc)

(defun harmless-web--take (items)
  "Return the first unique ITEMS, at most `harmless-web-search-limit'."
  (let (seen out)
    (dolist (item items)
      (when (< (length out) harmless-web-search-limit)
        (let ((url (plist-get item :url)))
          (unless (member url seen)
            (push url seen)
            (push item out)))))
    (nreverse out)))

(defun harmless-web-parse-duckduckgo (raw)
  "Return result plists from DuckDuckGo JSON body RAW."
  (unless (stringp raw)
    (error "web search failed"))
  (let* ((data (condition-case err
                   (harmless-json-decode raw)
                 (error (error "web search failed: %s"
                               (error-message-string err)))))
         (items nil)
         (abstract (plist-get data :AbstractText))
         (abstract-url (plist-get data :AbstractURL))
         (heading (plist-get data :Heading)))
    (when (and (stringp abstract) (not (string-empty-p (string-trim abstract)))
               (stringp abstract-url)
               (not (string-empty-p (string-trim abstract-url))))
      (push (list :title (if (and (stringp heading)
                                  (not (string-empty-p (string-trim heading))))
                             (harmless-web--title heading)
                           (harmless-web--title abstract))
                  :url (string-trim abstract-url)
                  :snippet (string-trim abstract))
            items))
    (dolist (item (plist-get data :RelatedTopics))
      (setq items (harmless-web--collect item items)))
    (dolist (item (plist-get data :Results))
      (setq items (harmless-web--collect item items)))
    (harmless-web--take (nreverse items))))

(defun harmless-web-search-default (query)
  "Search DuckDuckGo for QUERY and return result plists."
  (harmless-web-parse-duckduckgo
   (harmless-web-get (harmless-web-search-url query))))

(defun harmless-web--format-results (results)
  "Return RESULTS as numbered text, or \"No results\"."
  (if (null results)
      "No results"
    (let ((n 0))
      (mapconcat
       (lambda (item)
         (setq n (1+ n))
         (let ((snippet (plist-get item :snippet)))
           (concat (format "%d. %s\n%s" n
                           (or (plist-get item :title) "")
                           (or (plist-get item :url) ""))
                   (if (and (stringp snippet) (not (string-empty-p snippet)))
                       (concat "\n" snippet)
                     ""))))
       results
       "\n\n"))))

(defun harmless-web--query (args)
  "Return the trimmed query in ARGS, or signal."
  (let ((query (harmless-tool-arg args :query)))
    (unless (stringp query)
      (error "query must be a string"))
    (setq query (string-trim query))
    (when (string-empty-p query)
      (error "query is empty"))
    query))

(defun harmless-web-search (session args)
  "web_search implementation."
  (ignore session)
  (let ((results (funcall harmless-web-search-function
                          (harmless-web--query args))))
    (unless (and (listp results) (cl-every #'consp results))
      (error "web search failed"))
    (harmless-web--format-results results)))

(defconst harmless-web-fetch-limit 100000
  "Most characters of page text one fetch returns.")

(defconst harmless-web--block-tag-re
  (concat "</?\\(?:p\\|div\\|li\\|tr\\|ul\\|ol\\|pre\\|table\\|"
          "title\\|header\\|footer\\|section\\|article\\|"
          "blockquote\\|h[1-6]\\)\\b[^>]*>")
  "Regexp for an HTML tag that becomes a new line.")

(defun harmless-web--looks-like-html (text)
  "Return non-nil when TEXT looks like HTML."
  (let ((case-fold-search t))
    (or (string-match-p "\\`[[:space:]]*<" text)
        (string-match-p
         (concat "<\\(?:!doctype\\|html\\|head\\|body\\|div\\|"
                 "p\\|br\\|span\\|a\\|li\\|h[1-6]\\)\\b")
         text))))

(defun harmless-web--drop-blocks (text)
  "Remove script, style, and noscript elements from TEXT."
  (let ((case-fold-search t))
    (dolist (tag '("script" "style" "noscript"))
      (setq text
            (replace-regexp-in-string
             (format "<%s\\b\\(?:.\\|\n\\)*?</%s>" tag tag)
             "" text t t)))
    text))

(defun harmless-web--decode-numeric (text regexp base)
  "Replace REGEXP matches in TEXT with characters in BASE."
  (replace-regexp-in-string
   regexp
   (lambda (match)
     (if (string-match regexp match)
         (let ((n (string-to-number (match-string 1 match) base)))
           (if (<= 1 n #x10FFFF)
               (char-to-string n)
             match))
       match))
   text t t))

(defun harmless-web--decode-entities (text)
  "Return TEXT with common HTML entities decoded once."
  (setq text (harmless-web--decode-numeric text "&#\\([0-9]+\\);" 10))
  (setq text
        (harmless-web--decode-numeric
         text "&#[xX]\\([0-9A-Fa-f]+\\);" 16))
  (let ((case-fold-search t))
    (dolist (pair '(("&lt;" . "<")
                    ("&gt;" . ">")
                    ("&quot;" . "\"")
                    ("&apos;" . "'")
                    ("&nbsp;" . " ")
                    ("&amp;" . "&")))
      (setq text (replace-regexp-in-string
                  (regexp-quote (car pair)) (cdr pair) text t t))))
  text)

(defun harmless-web--html-text (text)
  "Return TEXT with tags removed and entities decoded."
  (let ((case-fold-search t))
    (setq text (replace-regexp-in-string
                "<!--\\(?:.\\|\n\\)*?-->" "" text t t))
    (setq text (harmless-web--drop-blocks text))
    (setq text (replace-regexp-in-string
                "<\\(?:br\\|hr\\)\\b[^>]*>" "\n" text t t))
    (setq text (replace-regexp-in-string
                harmless-web--block-tag-re "\n" text t t))
    (setq text (replace-regexp-in-string "<[^>]*>" "" text t t)))
  (setq text (harmless-web--decode-entities text))
  (setq text (replace-regexp-in-string "[ \t\r\f]+" " " text t t))
  (setq text (replace-regexp-in-string " *\n *" "\n" text t t))
  (setq text (replace-regexp-in-string "\n\\{3,\\}" "\n\n" text t t))
  (string-trim text))

(defun harmless-web--limit-text (text)
  "Cut TEXT at `harmless-web-fetch-limit'."
  (if (> (length text) harmless-web-fetch-limit)
      (concat (substring text 0 harmless-web-fetch-limit)
              "\n[truncated]")
    text))

(defun harmless-web--page (body)
  "Return text for response BODY.
A body that is not a string is an error.  HTML is reduced to text.
A body longer than four times `harmless-web-fetch-limit' is cut
before that reduction.  The result is at most that limit.  A cut
result ends with \"[truncated]\".  An empty page is \"Empty page\"."
  (unless (stringp body)
    (error "web request failed"))
  (when (> (length body) (* 4 harmless-web-fetch-limit))
    (setq body (substring body 0 (* 4 harmless-web-fetch-limit))))
  (setq body
        (if (harmless-web--looks-like-html body)
            (harmless-web--html-text body)
          (string-trim body)))
  (if (string-empty-p body)
      "Empty page"
    (harmless-web--limit-text body)))

(defun harmless-web-fetch (session args)
  "web_fetch implementation."
  (ignore session)
  (harmless-web--page
   (harmless-web-get (harmless-tool-arg args :url))))

(defun harmless-web-register ()
  "Register the web_search and web_fetch tools."
  (harmless-register-tool
   (harmless-tool-create
    :name "web_search"
    :description "Search the web. QUERY is the text to look up. The result lists a title, a URL, and a snippet for each hit. Nothing is written into the project."
    :class 'network
    :schema '(:type "object"
              :properties (:query (:type "string"
                                   :description "Text to search for"))
              :required ["query"])
    :fn #'harmless-web-search))
  (harmless-register-tool
   (harmless-tool-create
    :name "web_fetch"
    :description "Fetch one http or https page and return its text. URL is the address. HTML is reduced to text. Nothing is written into the project."
    :class 'network
    :schema '(:type "object"
              :properties (:url (:type "string"
                                 :description "http or https address"))
              :required ["url"])
    :fn #'harmless-web-fetch)))

(harmless-web-register)

(provide 'harmless-web)

;;; harmless-web.el ends here
