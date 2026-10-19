;;; harmless-media.el --- Images, PDFs, and notebooks for Harmless -*- lexical-binding: t; -*-

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
;; `read_file' returns an image, PDF text, or notebook cells when the
;; file is one of those.  `notebook_edit' replaces one cell's source.
;; `harmless-attach-image' queues an image on the next user turn.
;; Nothing is written except a notebook edit.  A read does not start
;; a converter and does not change the file.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-tools)
(require 'harmless-tools-fs)

(declare-function harmless-ensure-configured "harmless")
(declare-function harmless--context-session "harmless")
(declare-function harmless-current "harmless")

(defvar harmless-media-attachments (make-hash-table :test 'equal)
  "Queued prompt images, keyed by session id.
Each value is a list of (:media-type :data :path) plists.
The queue is not saved.  Sending a turn takes it.")

(defconst harmless-media--png-magic
  (unibyte-string #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a)
  "Raw PNG signature.")

(defconst harmless-media--jpeg-magic
  (unibyte-string #xff #xd8 #xff)
  "Raw JPEG signature.")

(defun harmless-media--name (key)
  "Return the field name of KEY."
  (cond
   ((keywordp key) (substring (symbol-name key) 1))
   ((symbolp key) (symbol-name key))
   ((stringp key) key)
   (t "")))

(defun harmless-media--has (plist key)
  "Return non-nil when PLIST sends KEY."
  (when (and (listp plist) (proper-list-p plist))
    (let ((name (harmless-media--name key)))
      (cl-loop for (k) on plist by #'cddr
               thereis (and (or (symbolp k) (stringp k))
                            (string= (harmless-media--name k) name))))))

(defun harmless-media--get (plist key)
  "Return KEY from PLIST."
  (when (harmless-media--has plist key)
    (let ((name (harmless-media--name key)))
      (cl-loop for (k v) on plist by #'cddr
               when (and (or (symbolp k) (stringp k))
                         (string= (harmless-media--name k) name))
               return v))))

(defun harmless-media--bytes (path n)
  "Return the first N bytes of PATH, or the whole file when N is nil.
PATH is not created."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (if n
        (insert-file-contents-literally path nil 0 n)
      (insert-file-contents-literally path))
    (buffer-string)))

(defun harmless-media--head-p (bytes prefix)
  "Return non-nil when BYTES begins with raw PREFIX."
  (and (stringp bytes)
       (stringp prefix)
       (>= (length bytes) (length prefix))
       (equal (substring bytes 0 (length prefix)) prefix)))

(defun harmless-media-type (bytes)
  "Return an image media type for BYTES, or nil."
  (cond
   ((harmless-media--head-p bytes harmless-media--png-magic) "image/png")
   ((harmless-media--head-p bytes harmless-media--jpeg-magic) "image/jpeg")
   ((and (stringp bytes)
         (>= (length bytes) 6)
         (or (equal (substring bytes 0 6) "GIF87a")
             (equal (substring bytes 0 6) "GIF89a")))
    "image/gif")
   ((and (stringp bytes)
         (>= (length bytes) 12)
         (equal (substring bytes 0 4) "RIFF")
         (equal (substring bytes 8 12) "WEBP"))
    "image/webp")
   (t nil)))

(defun harmless-media--notebook-name-p (path)
  "Return non-nil when PATH names a notebook."
  (and (stringp path)
       (string-match-p "\\.ipynb\\'" (downcase path))))

(defun harmless-media--pdf-name-p (path)
  "Return non-nil when PATH names a PDF."
  (and (stringp path)
       (string-match-p "\\.pdf\\'" (downcase path))))

(defun harmless-media-read-p (path)
  "Return non-nil when `read_file' should treat PATH as media.
A missing file is not media."
  (and (stringp path)
       (not (directory-name-p path))
       (file-regular-p path)
       (not (file-directory-p path))
       (or (harmless-media--notebook-name-p path)
           (harmless-media--pdf-name-p path)
           (harmless-media-type
            (condition-case nil
                (harmless-media--bytes path 16)
              (error nil))))))

(defun harmless-media--size (path)
  "Return the byte size of PATH, or nil."
  (let ((size (file-attribute-size (file-attributes path))))
    (and (integerp size) size)))

(defun harmless-media--cap ()
  "Return the positive byte cap for a media read."
  (if (and (integerp harmless-read-file-max-bytes)
           (> harmless-read-file-max-bytes 0))
      harmless-read-file-max-bytes
    100000))

(defun harmless-media-image-text (path type bytes)
  "Return the `read_file' text for image BYTES at PATH of TYPE."
  (format "Image %s\n%s\n%d bytes\n\n%s"
          type path (length bytes)
          (base64-encode-string bytes t)))

(defun harmless-media-image-parts (text)
  "Return (TYPE PATH DATA) for an image TEXT, or nil."
  (when (and (stringp text)
             (string-match
              (concat "\\`Image \\(image/[A-Za-z0-9.+-]+\\)\n"
                      "\\(.*\\)\n"
                      "\\([0-9]+\\) bytes\n\n"
                      "\\([A-Za-z0-9+/=]+\\)\\'")
              text))
    (list (match-string 1 text)
          (match-string 2 text)
          (match-string 4 text))))

(defun harmless-media--read-image (path)
  "Return the image text for PATH."
  (let ((size (harmless-media--size path)))
    (when (or (null size) (> size (harmless-media--cap)))
      (error "image is too large")))
  (let* ((bytes (harmless-media--bytes path nil))
         (type (harmless-media-type bytes)))
    (unless type
      (error "image is not an image"))
    (harmless-media-image-text path type bytes)))

(defun harmless-media--pdf-strings (bytes)
  "Return literal strings found in PDF BYTES."
  (let ((case-fold-search nil)
        (start 0)
        (out nil))
    (while (string-match "(\\([^()\\\\\0]*\\))" bytes start)
      (let ((text (match-string 1 bytes))
            (next (match-end 0)))
        (unless (string-empty-p (string-trim text))
          (push text out))
        (setq start next)))
    (nreverse out)))

(defun harmless-media--read-pdf (path)
  "Return text extracted from the PDF at PATH."
  (let* ((size (or (harmless-media--size path) 0))
         (cap (harmless-media--cap))
         (cut (> size cap))
         (bytes (harmless-media--bytes path (when cut cap))))
    (unless (and (>= (length bytes) 5)
                 (equal (substring bytes 0 5) "%PDF-"))
      (error "pdf is not a pdf"))
    (let ((text (mapconcat #'identity
                           (harmless-media--pdf-strings bytes)
                           "\n")))
      (concat "PDF " path "\n"
              (if (string-empty-p text) "No text" text)
              (if cut "\n[truncated]" "")))))

(defun harmless-media--join-source (source)
  "Return notebook SOURCE as one string."
  (cond
   ((stringp source) source)
   ((and (listp source) (proper-list-p source))
    (mapconcat (lambda (part) (if (stringp part) part "")) source ""))
   (t "")))

(defun harmless-media--lines (source)
  "Return SOURCE as Jupyter source lines."
  (if (string-empty-p source)
      (list "")
    (let ((start 0)
          (n (length source))
          (out nil))
      (while (< start n)
        (let ((nl (string-search "\n" source start)))
          (if nl
              (progn
                (push (substring source start (1+ nl)) out)
                (setq start (1+ nl)))
            (push (substring source start) out)
            (setq start n))))
      (nreverse out))))

(defun harmless-media--notebook (path)
  "Return the notebook plist at PATH."
  (let ((size (harmless-media--size path)))
    (when (or (null size) (> size (harmless-media--cap)))
      (error "notebook is too large")))
  (let ((raw (condition-case nil
                 (with-temp-buffer
                   (let ((coding-system-for-read 'utf-8-unix))
                     (insert-file-contents path))
                   (harmless-json-decode (buffer-string)))
               (error nil))))
    (unless (and (harmless-plist-p raw)
                 (harmless-media--has raw :cells))
      (error "notebook is not a notebook"))
    (let ((cells (plist-get raw :cells)))
      (unless (and (listp cells)
                   (proper-list-p cells)
                   (not (harmless-plist-p cells))
                   (cl-every #'harmless-plist-p cells))
        (error "notebook is not a notebook")))
    raw))

(defun harmless-media--cell-label (cell index)
  "Return one heading for CELL at INDEX."
  (let ((type (plist-get cell :cell_type))
        (id (plist-get cell :id)))
    (format "cell %d %s%s:"
            index
            (if (stringp type) type "raw")
            (if (and (stringp id) (not (string-empty-p id)))
                (concat " " id)
              ""))))

(defun harmless-media--read-notebook (path)
  "Return the cells of the notebook at PATH."
  (let* ((notebook (harmless-media--notebook path))
         (cells (plist-get notebook :cells))
         (index 0)
         (parts nil))
    (if (null cells)
        (format "Notebook %s\nNo cells" path)
      (dolist (cell cells)
        (push (concat (harmless-media--cell-label cell index) "\n"
                      (harmless-media--join-source
                       (plist-get cell :source)))
              parts)
        (setq index (1+ index)))
      (concat "Notebook " path "\n"
              (mapconcat #'identity (nreverse parts) "\n")))))

(defun harmless-media-read (_session path _args)
  "Return media text for PATH.
Images, PDFs, and notebooks are recognized.  The file is not
changed.  Line offset and limit apply only to text files."
  (cond
   ((harmless-media-type
     (condition-case nil
         (harmless-media--bytes path 16)
       (error nil)))
    (harmless-media--read-image path))
   ((harmless-media--pdf-name-p path)
    (harmless-media--read-pdf path))
   ((harmless-media--notebook-name-p path)
    (harmless-media--read-notebook path))
   (t (error "image is not an image"))))

(defun harmless-media--path-text (args empty type-error)
  "Return the path text in ARGS.
EMPTY and TYPE-ERROR are the refusals.  A filesystem root is
refused here and is not expanded."
  (unless (harmless-media--has args :path)
    (error "%s" empty))
  (let ((value (harmless-media--get args :path)))
    (cond
     ((null value) (error "%s" empty))
     ((eq value :false) (error "%s" type-error))
     ((not (stringp value)) (error "%s" type-error)))
    (setq value (string-trim value))
    (when (string-empty-p value)
      (error "%s" empty))
    (when (harmless-extra-root-text-p value)
      (error "notebook path is the filesystem root"))
    (when (directory-name-p value)
      (error "notebook is not a notebook"))
    value))

(defun harmless-media--cell-ref (args)
  "Return the cell selector in ARGS."
  (unless (harmless-media--has args :cell)
    (error "notebook cell is empty"))
  (let ((value (harmless-media--get args :cell)))
    (cond
     ((null value) (error "notebook cell is empty"))
     ((eq value :false) (error "notebook cell is not a cell"))
     ((stringp value)
      (setq value (string-trim value))
      (when (string-empty-p value)
        (error "notebook cell is empty"))
      value)
     ((and (integerp value) (>= value 0)) value)
     (t (error "notebook cell is not a cell")))))

(defun harmless-media--source (args)
  "Return the new cell source in ARGS."
  (unless (harmless-media--has args :source)
    (error "notebook source is empty"))
  (let ((value (harmless-media--get args :source)))
    (cond
     ((null value) (error "notebook source is empty"))
     ((eq value :false) (error "notebook source must be a string"))
     ((not (stringp value)) (error "notebook source must be a string"))
     (t value))))

(defun harmless-media--cell-index (cells selector)
  "Return the index of SELECTOR in CELLS, or nil."
  (cond
   ((integerp selector)
    (and (< selector (length cells)) selector))
   ((stringp selector)
    (let ((index 0)
          (found nil))
      (dolist (cell cells)
        (when (and (not found)
                   (stringp (plist-get cell :id))
                   (string= (plist-get cell :id) selector))
          (setq found index))
        (setq index (1+ index)))
      found))
   (t nil)))

(defun harmless-media-edit (session args)
  "Replace one notebook cell's source.
ARGS is :path, :cell, and :source.  Other cells stay.  A missing
notebook is not created."
  (unless (harmless-session-p session)
    (error "No Harmless session"))
  (harmless-tools-fs--reject-plan-edit session)
  (let* ((text (harmless-media--path-text
                args "notebook path is empty"
                "notebook path must be a string"))
         (selector (harmless-media--cell-ref args))
         (source (harmless-media--source args))
         (full (harmless-tools-fs-resolve session text)))
    (unless (file-exists-p full)
      (error "notebook is missing"))
    (unless (and (file-regular-p full)
                 (not (file-directory-p full))
                 (harmless-media--notebook-name-p full))
      (error "notebook is not a notebook"))
    (let* ((notebook (harmless-media--notebook full))
           (cells (plist-get notebook :cells))
           (index (harmless-media--cell-index cells selector)))
      (unless index
        (error "notebook cell is missing"))
      (let ((cell (copy-sequence (nth index cells))))
        (setq cell (plist-put cell :source
                              (harmless-media--lines source)))
        (setf (nth index cells) cell))
      (setq notebook (plist-put notebook :cells cells))
      (let ((coding-system-for-write 'utf-8-unix))
        (write-region (concat (harmless-json-text notebook) "\n")
                      nil full nil 'silent))
      (harmless-tools-fs-revert-visiting full)
      (let ((id (plist-get (nth index cells) :id)))
        (format "Edited cell %s in %s"
                (if (and (stringp id) (not (string-empty-p id)))
                    id
                  (number-to-string index))
                full)))))

(defun harmless-media-edit-tool (session args)
  "notebook_edit implementation."
  (harmless-media-edit session args))

(defun harmless-media--queue (session)
  "Return SESSION's queued images."
  (gethash (harmless-session-id session) harmless-media-attachments))

(defun harmless-media-pending-p (session)
  "Return non-nil when SESSION has a queued image."
  (and (harmless-session-p session)
       (consp (harmless-media--queue session))))

(defun harmless-media--image-file (session text)
  "Return an absolute image file for TEXT in SESSION."
  (when (harmless-extra-root-text-p text)
    (error "image path is the filesystem root"))
  (when (string-prefix-p "~" text)
    (error "image path must be absolute"))
  (when (directory-name-p text)
    (error "image is not an image"))
  (let ((full
         (cond
          ((harmless-session-detached-p session)
           (unless (file-name-absolute-p text)
             (error "image path must be absolute"))
           text)
          (t (harmless-tools-fs-resolve session text)))))
    (when (or (harmless-filesystem-root-p full)
              (harmless-extra-root-text-p full))
      (error "image path is the filesystem root"))
    (unless (file-exists-p full)
      (error "image is missing"))
    (unless (and (file-regular-p full) (not (file-directory-p full)))
      (error "image is not an image"))
    (let ((size (harmless-media--size full)))
      (when (or (null size) (> size (harmless-media--cap)))
        (error "image is too large")))
    (let* ((bytes (harmless-media--bytes full nil))
           (type (harmless-media-type bytes)))
      (unless type
        (error "image is not an image"))
      (list :media-type type
            :data (base64-encode-string bytes t)
            :path full))))

(defun harmless-media-attach (session path)
  "Queue PATH as an image on SESSION's next user turn.
The file is read and not changed.  The queue is not saved."
  (unless (harmless-session-p session)
    (error "No Harmless session"))
  (cond
   ((null path) (error "image path is empty"))
   ((eq path :false) (error "image path must be a string"))
   ((not (stringp path)) (error "image path must be a string")))
  (setq path (string-trim path))
  (when (string-empty-p path)
    (error "image path is empty"))
  (let ((image (harmless-media--image-file session path))
        (queued (copy-sequence (harmless-media--queue session))))
    (setq queued (append queued (list image)))
    (puthash (harmless-session-id session)
             queued harmless-media-attachments)
    image))

(defun harmless-media-consume (session)
  "Move SESSION's queued images onto its latest user message.
Return the images, or nil when there were none."
  (when (harmless-session-p session)
    (let ((images (harmless-media--queue session)))
      (remhash (harmless-session-id session) harmless-media-attachments)
      (when images
        (let ((tail (last (harmless-session-messages session))))
          (when tail
            (setcar tail (plist-put (copy-sequence (car tail))
                                    :images images)))))
      images)))

(defun harmless-media--image-blocks (images text openai)
  "Return provider content blocks for IMAGES and TEXT.
OPENAI selects image_url blocks.  Otherwise Anthropic image blocks."
  (let ((blocks nil))
    (when (and (stringp text) (not (string-empty-p text)))
      (push (if openai
                (list :type "text" :text text)
              (list :type "text" :text text))
            blocks))
    (dolist (image images)
      (let ((type (plist-get image :media-type))
            (data (plist-get image :data)))
        (when (and (stringp type) (stringp data))
          (push (if openai
                    (list :type "image_url"
                          :image_url
                          (list :url (concat "data:" type ";base64," data)))
                  (list :type "image"
                        :source (list :type "base64"
                                      :media_type type
                                      :data data)))
                blocks))))
    (nreverse blocks)))

(defun harmless-media-openai-user (msg)
  "Return OpenAI content for user MSG, or nil when it stays text."
  (let ((images (plist-get msg :images)))
    (when (and (listp images)
               (not (harmless-plist-p images))
               images)
      (harmless-media--image-blocks images (plist-get msg :content) t))))

(defun harmless-media-openai-tool (msg)
  "Return OpenAI content for tool MSG, or nil when it stays text."
  (let ((parts (and (equal (plist-get msg :name) "read_file")
                    (harmless-media-image-parts (plist-get msg :content)))))
    (when parts
      (harmless-media--image-blocks
       (list (list :media-type (nth 0 parts) :data (nth 2 parts)))
       (format "Image %s\n%s" (nth 0 parts) (nth 1 parts))
       t))))

(defun harmless-media-anthropic-user (msg)
  "Return Anthropic content for user MSG, or nil when it stays text."
  (let ((images (plist-get msg :images)))
    (when (and (listp images)
               (not (harmless-plist-p images))
               images)
      (harmless-media--image-blocks
       images (plist-get msg :content) nil))))

(defun harmless-media-anthropic-tool (msg)
  "Return Anthropic tool_result content, or nil when it stays text."
  (let ((parts (and (equal (plist-get msg :name) "read_file")
                    (harmless-media-image-parts (plist-get msg :content)))))
    (when parts
      (harmless-media--image-blocks
       (list (list :media-type (nth 0 parts) :data (nth 2 parts)))
       (format "Image %s\n%s" (nth 0 parts) (nth 1 parts))
       nil))))

(defun harmless-media-read-args ()
  "Read an image file from the minibuffer."
  (list (read-file-name "Image: " nil nil t)))

;;;###autoload
(defun harmless-attach-image (&optional session path)
  "Queue PATH as an image on SESSION's next prompt.
The file must already exist.  Harmless does not copy it and does
not change it.  A filesystem root is refused."
  (interactive (list nil (car (harmless-media-read-args))))
  (cond
   ((harmless-session-p session) nil)
   ((and (called-interactively-p 'any) (null session))
    (harmless-ensure-configured)
    (unless (harmless--context-session)
      (harmless-current))
    (setq session (harmless--context-session)))
   (t (error "No Harmless session")))
  (unless (harmless-session-p session)
    (error "No Harmless session"))
  (let ((image (harmless-media-attach session path)))
    (when (called-interactively-p 'any)
      (message "Attached %s" (plist-get image :path)))
    image))

(defun harmless-media-register ()
  "Register the notebook edit tool."
  (harmless-register-tool
   (harmless-tool-create
    :name "notebook_edit"
    :description "Replace the source of one notebook cell. path is a project .ipynb file. cell is a cell id or a 0-based index. source is the new source. Other cells stay. The file is not created."
    :class 'edit
    :schema '(:type "object"
              :properties
              (:path (:type "string" :description "Notebook path.")
               :cell (:description "Cell id or 0-based index.")
               :source (:type "string" :description "New cell source."))
              :required ("path" "cell" "source"))
    :fn #'harmless-media-edit-tool)))

(harmless-media-register)

(provide 'harmless-media)

;;; harmless-media.el ends here
