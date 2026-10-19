;;; harmless-todo.el --- A session work list for Harmless -*- lexical-binding: t; -*-

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
;; `todo_write' keeps the list of work for one session.  Each item
;; has an id, content, a status, and the ids it depends on.  The
;; list is saved with the session.  A fork copies it.  A subagent
;; starts with an empty list.  Nothing is written into the project.
;; This is not a queue row and it does not survive as team state.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-tools)

(defconst harmless-todo-statuses
  '(pending in_progress completed cancelled)
  "Status symbols a todo item can store.")

(defconst harmless-todo--absent (make-symbol "absent")
  "Marker for a field the caller did not send.")

(defun harmless-todo--name (key)
  "Return the field name of KEY."
  (cond
   ((keywordp key) (substring (symbol-name key) 1))
   ((symbolp key) (symbol-name key))
   ((stringp key) key)
   (t "")))

(defun harmless-todo--has (plist key)
  "Return non-nil when PLIST sends KEY.
Nil and JSON null are absent only when the key is missing.  A
present null is a value, including an empty dependency list."
  (when (and (listp plist) (proper-list-p plist))
    (let ((name (harmless-todo--name key)))
      (cl-loop for (k) on plist by #'cddr
               thereis (and (or (symbolp k) (stringp k))
                            (string= (harmless-todo--name k) name))))))

(defun harmless-todo--get (plist key)
  "Return KEY from PLIST, accepting a keyword or a string."
  (when (harmless-todo--has plist key)
    (let ((name (harmless-todo--name key)))
      (cl-loop for (k v) on plist by #'cddr
               when (and (or (symbolp k) (stringp k))
                         (string= (harmless-todo--name k) name))
               return v))))

(defun harmless-todo--items-p (value)
  "Return non-nil when VALUE is a list of items, including empty.
A plist is one item, not a list of items.  Null and an empty
array are both nil, and both count as an empty list."
  (or (null value)
      (and (listp value)
           (proper-list-p value)
           (not (harmless-plist-p value)))))

(defun harmless-todo--merge (args)
  "Return non-nil when ARGS updates by id.
A missing merge and JSON null default to updating.  :false
replaces the list."
  (let ((raw (if (harmless-todo--has args :merge)
                 (harmless-todo--get args :merge)
               harmless-todo--absent)))
    (cond
     ((or (eq raw harmless-todo--absent) (null raw) (eq raw t)) t)
     ((eq raw :false) nil)
     (t (error "todo merge must be a boolean")))))

(defun harmless-todo--id (raw)
  "Return the trimmed id of RAW."
  (unless (harmless-todo--has raw :id)
    (error "todo id is empty"))
  (let ((value (harmless-todo--get raw :id)))
    (cond
     ((null value) (error "todo id is empty"))
     ((not (stringp value)) (error "todo id must be a string"))
     (t
      (setq value (string-trim value))
      (when (string-empty-p value)
        (error "todo id is empty"))
      value))))

(defun harmless-todo--content (raw existing-p)
  "Return the trimmed content of RAW.
EXISTING-P allows the field to be omitted."
  (if (not (harmless-todo--has raw :content))
      (if existing-p
          harmless-todo--absent
        (error "todo content is empty"))
    (let ((value (harmless-todo--get raw :content)))
      (cond
       ((null value) (error "todo content is empty"))
       ((not (stringp value))
        (error "todo content must be a string"))
       (t
        (setq value (string-trim value))
        (when (string-empty-p value)
          (error "todo content is empty"))
        value)))))

(defun harmless-todo--status (value)
  "Return the status symbol for VALUE."
  (cond
   ((or (keywordp value) (symbolp value))
    (setq value (symbol-name value))
    (when (string-prefix-p ":" value)
      (setq value (substring value 1))))
   ((stringp value) (setq value (string-trim value)))
   (t (error "todo status is not a status")))
  (let ((status (intern value)))
    (unless (memq status harmless-todo-statuses)
      (error "todo status is not a status"))
    status))

(defun harmless-todo--field-status (raw existing-p)
  "Return the status of RAW, or pending for a new item.
EXISTING-P allows the field to be omitted.  JSON null is omitted."
  (if (or (not (harmless-todo--has raw :status))
          (null (harmless-todo--get raw :status)))
      (if existing-p harmless-todo--absent 'pending)
    (harmless-todo--status (harmless-todo--get raw :status))))

(defun harmless-todo--dep (value)
  "Return one trimmed dependency id from VALUE."
  (cond
   ((null value) (error "todo depends is empty"))
   ((not (stringp value))
    (error "todo depends must be a string"))
   (t
    (setq value (string-trim value))
    (when (string-empty-p value)
      (error "todo depends is empty"))
    value)))

(defun harmless-todo--depends (raw existing-p)
  "Return the dependency ids of RAW.
EXISTING-P allows the field to be omitted.  Null and an empty
array clear the dependencies."
  (if (not (harmless-todo--has raw :depends))
      (if existing-p harmless-todo--absent nil)
    (let ((value (harmless-todo--get raw :depends)))
      (cond
       ((null value) nil)
       ((or (not (listp value))
            (not (proper-list-p value))
            (harmless-plist-p value))
        (error "todo depends must be a list"))
       (t (mapcar #'harmless-todo--dep value))))))

(defun harmless-todo--item-p (raw)
  "Return non-nil when RAW looks like one todo item."
  (and (proper-list-p raw)
       (consp raw)
       (or (symbolp (car raw)) (stringp (car raw)))))

(defun harmless-todo--find (items id)
  "Return the item in ITEMS whose id is ID, or nil."
  (cl-find id items
           :key (lambda (item) (plist-get item :id))
           :test #'equal))

(defun harmless-todo--make (id content status depends)
  "Return one stored todo item."
  (list :id id :content content :status status :depends depends))

(defun harmless-todo--parse-item (raw existing-p)
  "Return a parsed item for RAW.
EXISTING-P is non-nil when this id is already on the list.
Omitted fields use `harmless-todo--absent'."
  (unless (harmless-todo--item-p raw)
    (error "todo must be an item"))
  (harmless-todo--make
   (harmless-todo--id raw)
   (harmless-todo--content raw existing-p)
   (harmless-todo--field-status raw existing-p)
   (harmless-todo--depends raw existing-p)))

(defun harmless-todo--overlay (item parsed)
  "Return ITEM with the fields PARSED sends."
  (harmless-todo--make
   (plist-get parsed :id)
   (if (eq (plist-get parsed :content) harmless-todo--absent)
       (plist-get item :content)
     (plist-get parsed :content))
   (if (eq (plist-get parsed :status) harmless-todo--absent)
       (or (plist-get item :status) 'pending)
     (plist-get parsed :status))
   (if (eq (plist-get parsed :depends) harmless-todo--absent)
       (plist-get item :depends)
     (plist-get parsed :depends))))

(defun harmless-todo--merge-one (items parsed)
  "Update or append PARSED in ITEMS."
  (let ((id (plist-get parsed :id))
        (found nil)
        (out nil))
    (dolist (item items)
      (if (equal (plist-get item :id) id)
          (progn
            (setq found t)
            (push (harmless-todo--overlay item parsed) out))
        (push item out)))
    (setq out (nreverse out))
    (unless found
      (setq out (append out (list (harmless-todo--overlay nil parsed)))))
    out))

(defun harmless-todo--back-edge (items)
  "Return (ID . DEP) for one cycle edge in ITEMS, or nil."
  (let ((seen (make-hash-table :test 'equal))
        (stack (make-hash-table :test 'equal))
        (found nil))
    (cl-labels
        ((deps (id)
           (let ((item (harmless-todo--find items id)))
             (and item (plist-get item :depends))))
         (visit (id)
           (cond
            (found nil)
            ((or (gethash id stack) (gethash id seen)) nil)
            (t
             (puthash id t stack)
             (dolist (dep (deps id))
               (cond
                (found nil)
                ((gethash dep stack) (setq found (cons id dep)))
                (t (visit dep))))
             (remhash id stack)
             (puthash id t seen)))))
      (dolist (item items)
        (visit (plist-get item :id)))
      found)))

(defun harmless-todo--check-graph (items)
  "Signal when ITEMS have a bad dependency.  Return ITEMS."
  (let ((ids (mapcar (lambda (item) (plist-get item :id)) items)))
    (dolist (item items)
      (let ((seen nil))
        (dolist (dep (plist-get item :depends))
          (when (member dep seen)
            (error "todo depends is duplicate"))
          (push dep seen)
          (when (equal dep (plist-get item :id))
            (error "todo depends on itself"))
          (unless (member dep ids)
            (error "todo depends is missing")))))
    (when (harmless-todo--back-edge items)
      (error "todo depends is a cycle")))
  items)

(defun harmless-todo--parse-list (raw)
  "Return the item list RAW, or signal."
  (cond
   ((not (harmless-todo--has raw :todos))
    (error "todos must be a list"))
   ((not (harmless-todo--items-p (harmless-todo--get raw :todos)))
    (error "todos must be a list"))
   (t (or (harmless-todo--get raw :todos) nil))))

(defun harmless-todo--apply (base merge incoming)
  "Return BASE updated by INCOMING.
MERGE nil replaces BASE.  The graph is checked before return."
  (let ((seen nil)
        (parsed nil))
    (dolist (raw incoming)
      (unless (harmless-todo--item-p raw)
        (error "todo must be an item"))
      (let* ((id (harmless-todo--id raw))
             (existing (and merge (harmless-todo--find base id))))
        (when (member id seen)
          (error "todo id is duplicate"))
        (push id seen)
        (push (harmless-todo--parse-item raw (and existing t)) parsed)))
    (setq parsed (nreverse parsed))
    (harmless-todo--check-graph
     (if merge
         (let ((items (copy-tree base)))
           (dolist (item parsed)
             (setq items (harmless-todo--merge-one items item)))
           items)
       (mapcar (lambda (item) (harmless-todo--overlay nil item))
               parsed)))))

(defun harmless-todo-update (session args)
  "Update SESSION's todo list from ARGS and save when it changes.
Return the list text.  A refused call leaves the list as it was
and writes nothing."
  (unless (harmless-session-p session)
    (error "No Harmless session"))
  (let* ((merge (harmless-todo--merge args))
         (incoming (harmless-todo--parse-list args))
         (next (harmless-todo--apply
                (harmless-session-todos session) merge incoming)))
    (unless (equal next (harmless-session-todos session))
      (setf (harmless-session-todos session) next)
      (harmless-session-save session))
    (harmless-todo-format next)))

(defun harmless-todo--line (item)
  "Return one display line for ITEM."
  (let ((deps (plist-get item :depends)))
    (concat
     (format "[%s] %s: %s"
             (symbol-name (plist-get item :status))
             (plist-get item :id)
             (plist-get item :content))
     (if deps
         (concat " (depends " (mapconcat #'identity deps ", ") ")")
       ""))))

(defun harmless-todo-format (items)
  "Return the text of ITEMS, or \"No todos\" when there are none."
  (if items
      (mapconcat #'harmless-todo--line items "\n")
    "No todos"))

(defun harmless-todo-header (session)
  "Return a header suffix for SESSION's open todos, or \"\"."
  (let ((open (cl-count-if
               (lambda (item)
                 (memq (plist-get item :status) '(pending in_progress)))
               (harmless-session-todos session))))
    (if (> open 0)
        (format "  todos %d" open)
      "")))

(defun harmless-todo-summary (session)
  "Return a :todos entry, or nil when SESSION has none."
  (let ((items (harmless-session-todos session)))
    (when items
      (list :todos (copy-tree items)))))

(defun harmless-todo--clean-item (raw)
  "Return a stored item for RAW, or nil when RAW is not usable.
Dependencies are trimmed strings.  Unknown ids and cycles are
removed later."
  (condition-case nil
      (when (harmless-todo--item-p raw)
        (let ((id (harmless-todo--id raw))
              (content (harmless-todo--content raw nil))
              (status (harmless-todo--field-status raw nil))
              (depends (harmless-todo--depends raw nil)))
          (harmless-todo--make id content status depends)))
    (error nil)))

(defun harmless-todo--known-deps (item ids)
  "Return ITEM with dependencies limited to other ids in IDS."
  (let ((seen nil)
        (deps nil))
    (dolist (dep (plist-get item :depends))
      (when (and (not (member dep seen))
                 (not (equal dep (plist-get item :id)))
                 (member dep ids))
        (push dep seen)
        (push dep deps)))
    (harmless-todo--make
     (plist-get item :id)
     (plist-get item :content)
     (plist-get item :status)
     (nreverse deps))))

(defun harmless-todo--drop-edge (items id dep)
  "Return ITEMS without the dependency DEP of ID."
  (mapcar
   (lambda (item)
     (if (equal (plist-get item :id) id)
         (harmless-todo--make
          (plist-get item :id)
          (plist-get item :content)
          (plist-get item :status)
          (cl-remove dep (plist-get item :depends) :test #'equal))
       item))
   items))

(defun harmless-todo--from-summary (raw)
  "Return (ITEMS . DROPPED) for saved RAW.
DROPPED is non-nil when a bad item or dependency was removed.
Nil and an empty list drop nothing."
  (cond
   ((null raw) (cons nil nil))
   ((not (harmless-todo--items-p raw)) (cons nil t))
   (t
    (let ((items nil)
          (dropped nil))
      (dolist (raw-item raw)
        (let ((clean (harmless-todo--clean-item raw-item)))
          (cond
           ((null clean) (setq dropped t))
           ((harmless-todo--find items (plist-get clean :id))
            (setq dropped t))
           (t (push clean items)))))
      (setq items (nreverse items))
      (let ((ids (mapcar (lambda (item) (plist-get item :id)) items)))
        (setq items
              (mapcar
               (lambda (item)
                 (let ((next (harmless-todo--known-deps item ids)))
                   (unless (equal (plist-get item :depends)
                                  (plist-get next :depends))
                     (setq dropped t))
                   next))
               items)))
      (let ((edge (harmless-todo--back-edge items)))
        (while edge
          (setq dropped t)
          (setq items (harmless-todo--drop-edge
                       items (car edge) (cdr edge)))
          (setq edge (harmless-todo--back-edge items))))
      (cons items dropped)))))

(defun harmless-todo-restore (session raw)
  "Keep usable todos from RAW on SESSION.
A bad item is dropped.  A missing dependency, a dependency on
itself, a repeated dependency, and a cycle edge are dropped.
Save when the kept list differs from RAW."
  (let* ((parsed (harmless-todo--from-summary raw))
         (items (car parsed)))
    (setf (harmless-session-todos session) items)
    (when (cdr parsed)
      (condition-case nil
          (harmless-session-save session)
        (error nil)))
    items))

(defun harmless-todo-tool (session args)
  "todo_write implementation."
  (harmless-todo-update session args))

(defun harmless-todo-register ()
  "Register the todo tool."
  (harmless-register-tool
   (harmless-tool-create
    :name "todo_write"
    :description "Update this session's todo list. Each item has id, content, status, and depends. Status is pending, in_progress, completed, or cancelled. merge true, the default, updates by id. merge false replaces the list. The list is saved with the session. A dependency is recorded and does not block a status change. Nothing is written into the project."
    :class 'read
    :schema '(:type "object"
              :properties (:merge (:type "boolean"
                                   :description "True updates by id. False replaces the list.")
                           :todos (:type "array"
                                   :description "Items to write."
                                   :items (:type "object"
                                           :properties (:id (:type "string")
                                                      :content (:type "string")
                                                      :status (:type "string")
                                                      :depends (:type "array"
                                                                :items (:type "string"))))))
              :required ["todos"])
    :fn #'harmless-todo-tool)))

(harmless-todo-register)

(provide 'harmless-todo)

;;; harmless-todo.el ends here
