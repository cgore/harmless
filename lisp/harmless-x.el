;;; harmless-x.el --- X search for Harmless -*- lexical-binding: t; -*-

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
;; `x_keyword_search' finds recent posts.  `x_thread_fetch' reads one
;; post and its conversation.  `x_user_search' finds accounts.  The
;; class is `network', so accept-edits still asks.  Nothing is written
;; into the project.  A missing bearer token is refused before any
;; request.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'harmless-util)
(require 'harmless-auth)
(require 'harmless-tools)
(require 'harmless-web)

(defvar harmless-x-bearer nil
  "Bearer token for api.x.com.
Nil reads `X_BEARER_TOKEN', then auth-source for host api.x.com.")

(defvar harmless-x-search-function #'harmless-x-search-default
  "Function (QUERY MODE FROM TO) that returns post plists.
MODE is \"Top\" or \"Latest\".  FROM and TO are YYYY-MM-DD or nil.
Each plist has :id, :username, :name, :created, and :text.")

(defvar harmless-x-thread-function #'harmless-x-thread-default
  "Function (ID) that returns post plists for one conversation.")

(defvar harmless-x-user-function #'harmless-x-user-default
  "Function (QUERY) that returns user plists.
Each plist has :id, :username, :name, and :description.")

(defconst harmless-x-result-limit 8
  "Default number of X results.")

(defconst harmless-x-result-max 10
  "Most X results one call returns.")

(defun harmless-x-bearer ()
  "Return the trimmed X bearer token, or nil."
  (let ((token (harmless-auth-key "api.x.com"
                                  harmless-x-bearer
                                  "X_BEARER_TOKEN")))
    (when (stringp token)
      (setq token (string-trim token))
      (unless (string-empty-p token)
        token))))

(defun harmless-x-get (url)
  "GET URL with the X bearer token.
A missing token is refused before the request."
  (let ((token (harmless-x-bearer)))
    (unless token
      (error "X is not signed in"))
    (let ((url-request-extra-headers
           `(("Authorization" . ,(concat "Bearer " token)))))
      (harmless-web-get url))))

(defun harmless-x--time-params (from to)
  "Return the start_time and end_time query suffix for FROM and TO."
  (concat
   (if from
       (concat "&start_time="
               (url-hexify-string (concat from "T00:00:00Z")))
     "")
   (if to
       (concat "&end_time="
               (url-hexify-string (concat to "T00:00:00Z")))
     "")))

(defun harmless-x-search-url (query mode from to)
  "Return the recent-search URL for QUERY.
MODE is \"Top\" or \"Latest\".  FROM and TO are YYYY-MM-DD or nil."
  (concat "https://api.x.com/2/tweets/search/recent?query="
          (url-hexify-string query)
          "&max_results=" (number-to-string harmless-x-result-max)
          "&tweet.fields=created_at,author_id,text"
          "&expansions=author_id&user.fields=name,username"
          "&sort_order="
          (if (equal mode "Top") "relevancy" "recency")
          (harmless-x--time-params from to)))

(defun harmless-x-tweet-url (id)
  "Return the tweet lookup URL for ID."
  (concat "https://api.x.com/2/tweets/" id
          "?tweet.fields=created_at,author_id,conversation_id,text"
          "&expansions=author_id,referenced_tweets.id"
          "&user.fields=name,username"))

(defun harmless-x-user-url (query)
  "Return the user-search URL for QUERY."
  (concat "https://api.x.com/2/users/search?query="
          (url-hexify-string query)
          "&user.fields=description,name,username"))

(defun harmless-x--decode (raw)
  "Return the plist for JSON RAW, or signal."
  (unless (stringp raw)
    (error "X search failed"))
  (condition-case err
      (harmless-json-decode raw)
    (error (error "X search failed: %s" (error-message-string err)))))

(defun harmless-x--nonempty (value)
  "Return trimmed VALUE when it is a non-empty string, else nil."
  (when (and (stringp value) (not (string-empty-p (string-trim value))))
    (string-trim value)))

(defun harmless-x--as-list (node)
  "Return NODE as a list of plists.
One plist becomes a one-element list.  Anything else becomes nil."
  (cond
   ((null node) nil)
   ((and (consp node) (keywordp (car node)))
    (list node))
   ((and (listp node) (cl-every #'consp node))
    node)
   (t nil)))

(defun harmless-x--collect-users (data table)
  "Store users from DATA in TABLE, keyed by id."
  (dolist (user (plist-get (plist-get data :includes) :users))
    (let ((id (and (consp user) (harmless-x--nonempty (plist-get user :id)))))
      (when id
        (puthash id user table))))
  table)

(defun harmless-x--user-table (data)
  "Return a hash table of users in DATA."
  (harmless-x--collect-users data (make-hash-table :test 'equal)))

(defun harmless-x--normalize-post (tweet users)
  "Return a post plist for TWEET, using USERS, or nil."
  (let ((id (harmless-x--nonempty (plist-get tweet :id))))
    (when id
      (let ((author (and users (gethash (or (plist-get tweet :author_id) "")
                                       users))))
        (list :id id
              :username (and author
                             (harmless-x--nonempty
                              (plist-get author :username)))
              :name (and author
                         (harmless-x--nonempty (plist-get author :name)))
              :created (harmless-x--nonempty (plist-get tweet :created_at))
              :text (string-trim (or (plist-get tweet :text) "")))))))

(defun harmless-x--normalize-user (user)
  "Return a user plist for USER, or nil."
  (let ((id (harmless-x--nonempty (plist-get user :id)))
        (username (harmless-x--nonempty (plist-get user :username))))
    (when (and id username)
      (list :id id
            :username username
            :name (harmless-x--nonempty (plist-get user :name))
            :description (string-trim
                          (or (plist-get user :description) ""))))))

(defun harmless-x-parse-posts (raw)
  "Return post plists from a recent-search body RAW."
  (let* ((data (harmless-x--decode raw))
         (users (harmless-x--user-table data)))
    (delq nil
          (mapcar (lambda (tweet)
                    (harmless-x--normalize-post tweet users))
                  (harmless-x--as-list (plist-get data :data))))))

(defun harmless-x-parse-users (raw)
  "Return user plists from a user-search body RAW."
  (delq nil
        (mapcar #'harmless-x--normalize-user
                (harmless-x--as-list
                 (plist-get (harmless-x--decode raw) :data)))))

(defun harmless-x--dedupe (posts)
  "Return POSTS with the first copy of each id."
  (let (seen out)
    (dolist (post posts)
      (let ((id (plist-get post :id)))
        (when (and (stringp id) (not (member id seen)))
          (push id seen)
          (push post out))))
    (nreverse out)))

(defun harmless-x--by-time (posts)
  "Return POSTS sorted by :created, oldest first."
  (sort (copy-sequence posts)
        (lambda (a b)
          (string-lessp (or (plist-get a :created) "")
                        (or (plist-get b :created) "")))))

(defun harmless-x--window (posts focal limit)
  "Return at most LIMIT POSTS, keeping FOCAL when it is present.
POSTS are ordered oldest first.  The window prefers the middle
around FOCAL.  A missing FOCAL keeps the newest posts."
  (setq posts (harmless-x--dedupe (harmless-x--by-time posts)))
  (let ((n (length posts)))
    (if (or (<= n limit) (< limit 1))
        (if (< limit 1) nil posts)
      (let ((idx (cl-position focal posts
                              :key (lambda (post) (plist-get post :id))
                              :test #'equal)))
        (unless idx
          (setq idx (1- n)))
        (let ((start (max 0 (min (- idx (/ limit 2)) (- n limit)))))
          (cl-subseq posts start (+ start limit)))))))

(defun harmless-x-parse-thread (lookup search focal)
  "Return post plists for FOCAL from lookup JSON and search JSON."
  (let ((one (harmless-x--decode lookup))
        (many (harmless-x--decode search))
        (users (make-hash-table :test 'equal)))
    (harmless-x--collect-users one users)
    (harmless-x--collect-users many users)
    (harmless-x--window
     (delq nil
           (mapcar (lambda (tweet)
                     (harmless-x--normalize-post tweet users))
                   (append
                    (harmless-x--as-list
                     (plist-get (plist-get one :includes) :tweets))
                    (harmless-x--as-list (plist-get one :data))
                    (harmless-x--as-list (plist-get many :data)))))
     focal
     harmless-x-result-limit)))

(defun harmless-x--conversation-id (data id)
  "Return the conversation id in DATA, or ID."
  (let ((tweet (plist-get data :data)))
    (or (and (consp tweet)
             (keywordp (car tweet))
             (harmless-x--nonempty (plist-get tweet :conversation_id)))
        id)))

(defun harmless-x-search-default (query mode from to)
  "Search recent posts for QUERY and return post plists."
  (harmless-x-parse-posts
   (harmless-x-get (harmless-x-search-url query mode from to))))

(defun harmless-x-thread-default (id)
  "Return post plists for the conversation around ID."
  (let* ((lookup (harmless-x-get (harmless-x-tweet-url id)))
         (conv (harmless-x--conversation-id (harmless-x--decode lookup) id)))
    (harmless-x-parse-thread
     lookup
     (harmless-x-get
      (harmless-x-search-url (concat "conversation_id:" conv)
                             "Latest" nil nil))
     id)))

(defun harmless-x-user-default (query)
  "Search accounts for QUERY and return user plists."
  (harmless-x-parse-users (harmless-x-get (harmless-x-user-url query))))

(defun harmless-x--query (args)
  "Return the trimmed query in ARGS, or signal."
  (let ((query (harmless-tool-arg args :query)))
    (unless (stringp query)
      (error "query must be a string"))
    (setq query (string-trim query))
    (when (string-empty-p query)
      (error "query is empty"))
    query))

(defun harmless-x--post-id (args)
  "Return the post id in ARGS, or signal."
  (let ((id (harmless-tool-arg args :post_id)))
    (cond
     ((null id) (error "post id is empty"))
     ((not (stringp id)) (error "post id must be a string"))
     (t
      (setq id (string-trim id))
      (when (string-empty-p id)
        (error "post id is empty"))
      (unless (string-match-p "\\`[0-9]+\\'" id)
        (error "post id is not an id"))
      id))))

(defun harmless-x--mode (args)
  "Return Top or Latest from ARGS.  A missing mode is Latest."
  (let ((mode (harmless-tool-arg args :mode)))
    (cond
     ((null mode) "Latest")
     ((not (stringp mode)) (error "mode must be Top or Latest"))
     (t
      (setq mode (downcase (string-trim mode)))
      (cond
       ((string-equal mode "latest") "Latest")
       ((string-equal mode "top") "Top")
       (t (error "mode must be Top or Latest")))))))

(defun harmless-x--limit (args)
  "Return how many results ARGS asks for."
  (let ((limit (harmless-tool-arg args :limit)))
    (cond
     ((null limit) harmless-x-result-limit)
     ((and (integerp limit) (<= 1 limit harmless-x-result-max))
      limit)
     ((integerp limit)
      (error "limit must be from 1 to 10"))
     (t (error "limit must be a positive integer")))))

(defun harmless-x--date (args key)
  "Return the YYYY-MM-DD value of KEY in ARGS, or nil when absent."
  (let ((value (harmless-tool-arg args key)))
    (cond
     ((null value) nil)
     ((not (stringp value)) (error "date must be YYYY-MM-DD"))
     (t
      (setq value (string-trim value))
      (if (string-match-p
           "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" value)
          value
        (error "date must be YYYY-MM-DD"))))))

(defun harmless-x--check-range (from to)
  "Signal when FROM is later than TO."
  (when (and from to (string-lessp to from))
    (error "from_date is after to_date")))

(defun harmless-x--take (items limit)
  "Return the first unique ITEMS, at most LIMIT."
  (let ((items (harmless-x--dedupe items)))
    (if (<= (length items) limit)
        items
      (cl-subseq items 0 limit))))

(defun harmless-x--who (post)
  "Return the handle line for POST."
  (let ((user (plist-get post :username))
        (name (plist-get post :name)))
    (cond
     ((and user name) (format "@%s (%s)" user name))
     (user (concat "@" user))
     (t "unknown"))))

(defun harmless-x--post-url (post)
  "Return the status URL for POST."
  (let ((id (plist-get post :id))
        (user (plist-get post :username)))
    (if user
        (format "https://x.com/%s/status/%s" user id)
      (format "https://x.com/i/status/%s" id))))

(defun harmless-x--format-post (n post)
  "Return numbered text for POST at position N."
  (let ((lines (list (format "%d. %s" n (harmless-x--who post))))
        (created (plist-get post :created))
        (text (or (plist-get post :text) "")))
    (when created
      (setq lines (append lines (list created))))
    (setq lines (append lines (list (harmless-x--post-url post))))
    (unless (string-empty-p text)
      (setq lines (append lines (list text))))
    (mapconcat #'identity lines "\n")))

(defun harmless-x--format-posts (posts)
  "Return POSTS as numbered text, or \"No posts\"."
  (if (null posts)
      "No posts"
    (let ((n 0))
      (mapconcat
       (lambda (post)
         (setq n (1+ n))
         (harmless-x--format-post n post))
       posts
       "\n\n"))))

(defun harmless-x--user-title (user)
  "Return the name line for USER."
  (let ((name (plist-get user :name))
        (username (plist-get user :username)))
    (if name
        (format "%s (@%s)" name username)
      (concat "@" username))))

(defun harmless-x--format-user (n user)
  "Return numbered text for USER at position N."
  (let ((desc (or (plist-get user :description) ""))
        (lines (list (format "%d. %s" n (harmless-x--user-title user))
                     (format "https://x.com/%s"
                             (plist-get user :username)))))
    (unless (string-empty-p desc)
      (setq lines (append lines (list desc))))
    (mapconcat #'identity lines "\n")))

(defun harmless-x--format-users (users)
  "Return USERS as numbered text, or \"No users\"."
  (if (null users)
      "No users"
    (let ((n 0))
      (mapconcat
       (lambda (user)
         (setq n (1+ n))
         (harmless-x--format-user n user))
       users
       "\n\n"))))

(defun harmless-x--result-list (items)
  "Signal unless ITEMS is a list of plists."
  (unless (and (listp items) (cl-every #'consp items))
    (error "X search failed"))
  items)

(defun harmless-x-keyword-search (session args)
  "x_keyword_search implementation."
  (ignore session)
  (let* ((query (harmless-x--query args))
         (mode (harmless-x--mode args))
         (limit (harmless-x--limit args))
         (from (harmless-x--date args :from_date))
         (to (harmless-x--date args :to_date)))
    (harmless-x--check-range from to)
    (harmless-x--format-posts
     (harmless-x--take
      (harmless-x--result-list
       (funcall harmless-x-search-function query mode from to))
      limit))))

(defun harmless-x-thread-fetch (session args)
  "x_thread_fetch implementation."
  (ignore session)
  (harmless-x--format-posts
   (harmless-x--take
    (harmless-x--result-list
     (funcall harmless-x-thread-function (harmless-x--post-id args)))
    harmless-x-result-limit)))

(defun harmless-x-user-search (session args)
  "x_user_search implementation."
  (ignore session)
  (let ((query (harmless-x--query args))
        (limit (harmless-x--limit args)))
    (harmless-x--format-users
     (harmless-x--take
      (harmless-x--result-list
       (funcall harmless-x-user-function query))
      limit))))

(defun harmless-x-register ()
  "Register the X search tools."
  (harmless-register-tool
   (harmless-tool-create
    :name "x_keyword_search"
    :description "Search recent X posts. QUERY is the text, including search operators. MODE is Top or Latest. FROM_DATE and TO_DATE are YYYY-MM-DD, and TO_DATE is exclusive. Nothing is written into the project."
    :class 'network
    :schema '(:type "object"
              :properties (:query (:type "string"
                                   :description "Post search query")
                           :mode (:type "string"
                                  :description "Top or Latest")
                           :limit (:type "integer"
                                   :description "How many posts, from 1 to 10")
                           :from_date (:type "string"
                                      :description "First day, YYYY-MM-DD")
                           :to_date (:type "string"
                                    :description "Day after the last day, YYYY-MM-DD"))
              :required ["query"])
    :fn #'harmless-x-keyword-search))
  (harmless-register-tool
   (harmless-tool-create
    :name "x_thread_fetch"
    :description "Fetch one X post and its conversation. POST_ID is the numeric id. Nothing is written into the project."
    :class 'network
    :schema '(:type "object"
              :properties (:post_id (:type "string"
                                    :description "Numeric post id"))
              :required ["post_id"])
    :fn #'harmless-x-thread-fetch))
  (harmless-register-tool
   (harmless-tool-create
    :name "x_user_search"
    :description "Search X accounts. QUERY matches a name, a handle, or a bio. Nothing is written into the project."
    :class 'network
    :schema '(:type "object"
              :properties (:query (:type "string"
                                   :description "Account search query")
                           :limit (:type "integer"
                                   :description "How many accounts, from 1 to 10"))
              :required ["query"])
    :fn #'harmless-x-user-search)))

(harmless-x-register)

(provide 'harmless-x)

;;; harmless-x.el ends here
