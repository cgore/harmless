;;; harmless-image.el --- Generate an image into the project -*- lexical-binding: t; -*-

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
;; `generate_image' asks `harmless-image-generate-function' for bytes
;; and writes them inside the project.  A relative path is resolved
;; against the session directory, not `default-directory'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-tools)
(require 'harmless-tools-fs)
(require 'harmless-media)

(defvar harmless-image-generate-function nil
  "Function of (SESSION PROMPT) that returns image bytes, or nil.
Nil means image generation is not configured.  The function is not
called until the path is accepted.  Plan mode does not call it.")

(defun harmless-image--root-text-p (text)
  "Return non-nil if TEXT is \"/\" or a drive root."
  (or (harmless-filesystem-root-p text)
      (and (stringp text)
           (or (string-match-p "\\`[A-Za-z]:[/\\]?\\'" (string-trim text))
               (harmless-extra-root-text-p text)))))

(defun harmless-image--prompt (prompt)
  "Return PROMPT trimmed, or signal."
  (cond
   ((null prompt) (error "image prompt is empty"))
   ((not (stringp prompt)) (error "image prompt must be a string"))
   ((string-empty-p (string-trim prompt))
    (error "image prompt is empty"))
   (t (string-trim prompt))))

(defun harmless-image--project (session)
  "Return SESSION's project directory, or signal.
A detached chat and a blank cwd signal \"This chat is not attached
to a project\".  A relative path signals \"image directory must be
absolute\".  The filesystem root signals \"image directory is the
filesystem root\".  This does not consult `default-directory'."
  (when (or (not (harmless-session-p session))
            (harmless-session-detached-p session))
    (error "This chat is not attached to a project"))
  (let ((cwd (harmless-session-cwd session)))
    (cond
     ((or (not (stringp cwd))
          (string-empty-p (string-trim cwd)))
      (error "This chat is not attached to a project"))
     ((harmless-image--root-text-p cwd)
      (error "image directory is the filesystem root"))
     ((or (string-prefix-p "~" (string-trim cwd))
          (not (file-name-absolute-p cwd)))
      (error "image directory must be absolute"))
     (t (file-name-as-directory (directory-file-name cwd))))))

(defun harmless-image--destination (project path)
  "Return the absolute file PATH inside PROJECT, or signal.
A relative PATH is expanded against PROJECT.  \"~\" is not expanded.
The filesystem root is refused."
  (cond
   ((null path) (error "image path is empty"))
   ((not (stringp path)) (error "image path must be a string"))
   (t
    (let ((raw (string-trim path)))
      (cond
       ((string-empty-p raw) (error "image path is empty"))
       ((harmless-image--root-text-p raw)
        (error "image path is the filesystem root"))
       ((string-prefix-p "~" raw)
        (error "image path must be absolute"))
       (t
        (let ((full (if (file-name-absolute-p raw)
                        raw
                      (expand-file-name raw project))))
          (when (or (harmless-image--root-text-p full)
                    (and (file-exists-p full)
                         (equal (file-truename full) (file-truename "/"))))
            (error "image path is the filesystem root"))
          (when (or (directory-name-p full)
                    (string-suffix-p "/" raw)
                    (string-suffix-p "\\" raw))
            (error "image path is not a file"))
          (let ((parent (file-name-directory full)))
            (unless (and (stringp parent)
                         (not (harmless-image--root-text-p parent))
                         (or (harmless-same-directory-p parent project)
                             (harmless-directory-strictly-under-p
                              parent project)))
              (error "image path escapes the project: %s" raw)))
          full)))))))

(defun harmless-image--bytes (session prompt)
  "Return image bytes for PROMPT in SESSION, or signal."
  (unless (functionp harmless-image-generate-function)
    (error "image generation is not configured"))
  (let ((bytes (funcall harmless-image-generate-function session prompt)))
    (unless (stringp bytes)
      (error "image is not an image"))
    (setq bytes (encode-coding-string bytes 'no-conversion))
    (unless (harmless-media-type bytes)
      (error "image is not an image"))
    bytes))

(defun harmless-image-generate (session args)
  "Generate an image for SESSION from ARGS and write it.
Return `Wrote PATH (N bytes)'.  Nothing is written when the prompt,
the path, or the bytes are refused.  Plan mode does not generate."
  (unless (harmless-session-p session)
    (error "No Harmless session"))
  (let ((prompt (harmless-image--prompt (harmless-tool-arg args :prompt))))
    (harmless-tools-fs--reject-plan-edit session)
    (let* ((project (harmless-image--project session))
           (path (harmless-image--destination
                  project (harmless-tool-arg args :path)))
           (bytes (harmless-image--bytes session prompt))
           (parent (file-name-directory path)))
      (unless (file-directory-p parent)
        (make-directory parent t))
      (let ((coding-system-for-write 'no-conversion))
        (write-region bytes nil path nil 'silent))
      (format "Wrote %s (%d bytes)" path (string-bytes bytes)))))

(defun harmless-image-register ()
  "Register the image generation tool."
  (harmless-register-tool
   (harmless-tool-create
    :name "generate_image"
    :description "Generate an image from a prompt and write it to path. path stays inside the project. A relative path is resolved against the session directory."
    :class 'edit
    :schema '(:type "object"
              :properties
              (:prompt (:type "string"
                        :description "What the image should show.")
               :path (:type "string"
                      :description "File path inside the project."))
              :required ("prompt" "path"))
    :fn #'harmless-image-generate)))

(harmless-image-register)

(provide 'harmless-image)

;;; harmless-image.el ends here
