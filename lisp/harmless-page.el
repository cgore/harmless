;;; harmless-page.el --- Publish Markdown or HTML as a page -*- lexical-binding: t; -*-

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
;; `harmless-page-publish' writes one HTML page from a Markdown or
;; HTML file in the project.  Markdown is escaped and rendered.
;; HTML is copied.  The page is a file.  Nothing is uploaded.

;;; Code:

(require 'subr-x)
(require 'harmless-util)
(require 'harmless-tools-fs)

(defun harmless-page--root-text-p (text)
  "Return non-nil if TEXT is \"/\" or a drive root.
TEXT is not expanded.  A relative path and \"~\" are not roots."
  (and (stringp text)
       (or (harmless-extra-root-text-p text)
           (harmless-filesystem-root-p text))))

(defun harmless-page--directory (directory)
  "Return DIRECTORY as an absolute project directory, or signal.
Nil, \"\", and whitespace signal \"page directory is empty\".
A non-string signals \"page directory must be a string\".
\"/\", a drive root, and \"c:\" signal \"page directory is the
filesystem root\".  A relative path, including \"~\", signals
\"page directory must be absolute\" and is not expanded.  A
missing path signals \"page directory is missing\".  A file
signals \"page directory is not a directory\".  A trailing
slash is the same directory.  This does not create DIRECTORY."
  (cond
   ((null directory) (error "page directory is empty"))
   ((not (stringp directory))
    (error "page directory must be a string")))
  (setq directory (string-trim directory))
  (cond
   ((string-empty-p directory)
    (error "page directory is empty"))
   ((harmless-page--root-text-p directory)
    (error "page directory is the filesystem root"))
   ((or (string-prefix-p "~" directory)
        (not (file-name-absolute-p directory)))
    (error "page directory must be absolute")))
  (setq directory (directory-file-name (expand-file-name directory)))
  (cond
   ((harmless-page--root-text-p directory)
    (error "page directory is the filesystem root"))
   ((not (file-exists-p directory))
    (error "page directory is missing"))
   ((not (file-directory-p directory))
    (error "page directory is not a directory"))
   (t directory)))

(defun harmless-page--escape (text)
  "Return TEXT safe to insert into HTML."
  (setq text (replace-regexp-in-string "&" "&amp;" text t t))
  (setq text (replace-regexp-in-string "<" "&lt;" text t t))
  (setq text (replace-regexp-in-string ">" "&gt;" text t t))
  (replace-regexp-in-string "\"" "&quot;" text t t))

(defun harmless-page--inline (text)
  "Return TEXT as HTML with code spans and links."
  (let ((i 0)
        (n (length text))
        (out ""))
    (while (< i n)
      (let ((code (string-match "`" text i))
            (link (string-match "\\[\\([^][]*\\)\\](\\([^)]*\\))" text i)))
        (cond
         ((and code (or (not link) (< code link)))
          (setq out (concat out
                            (harmless-page--escape
                             (substring text i code))))
          (let ((end (string-match "`" text (1+ code))))
            (if (not end)
                (setq out (concat out
                                  (harmless-page--escape
                                   (substring text code)))
                      i n)
              (setq out (concat out "<code>"
                                (harmless-page--escape
                                 (substring text (1+ code) end))
                                "</code>")
                    i (1+ end)))))
         (link
          (let ((href (match-string 2 text))
                (label (match-string 1 text))
                (end (match-end 0)))
            (setq out (concat out
                              (harmless-page--escape
                               (substring text i link))
                              "<a href=\""
                              (harmless-page--escape href)
                              "\">"
                              (harmless-page--escape label)
                              "</a>")
                  i end)))
         (t
          (setq out (concat out
                            (harmless-page--escape (substring text i))))
          (setq i n)))))
    out))

(defun harmless-page--paragraph (text)
  "Return TEXT as one HTML paragraph."
  (concat "<p>" (harmless-page--inline (string-trim text)) "</p>\n"))

(defun harmless-page--markdown-body (text)
  "Return Markdown TEXT as HTML body elements.
Headings, paragraphs, fenced code, inline code, and links are
rendered.  Other markup is escaped text."
  (let* ((lines (split-string text "\n"))
         (out "")
         (para nil)
         (i 0)
         (n (length lines)))
    (while (< i n)
      (let ((line (nth i lines)))
        (cond
         ((string-match "\\````\\(.*\\)\\'" line)
          (when para
            (setq out (concat out (harmless-page--paragraph para))
                  para nil))
          (let ((lang (string-trim (match-string 1 line)))
                (code nil)
                (closed nil))
            (setq i (1+ i))
            (while (and (< i n) (not closed))
              (if (string-match "\\````\\'" (nth i lines))
                  (setq closed t)
                (setq code (concat (or code "")
                                   (if code "\n" "")
                                   (nth i lines))))
              (setq i (1+ i)))
            (setq out (concat out "<pre><code"
                              (if (string-empty-p lang)
                                  ""
                                (concat " class=\"language-"
                                        (harmless-page--escape lang)
                                        "\""))
                              ">"
                              (harmless-page--escape (or code ""))
                              "</code></pre>\n"))))
         ((string-match "\\`\\(#+\\) \\(.*\\)\\'" line)
          (when para
            (setq out (concat out (harmless-page--paragraph para))
                  para nil))
          (let ((level (min 6 (length (match-string 1 line)))))
            (setq out (concat out
                              (format "<h%d>" level)
                              (harmless-page--inline (match-string 2 line))
                              (format "</h%d>\n" level))))
          (setq i (1+ i)))
         ((string-empty-p (string-trim line))
          (when para
            (setq out (concat out (harmless-page--paragraph para))
                  para nil))
          (setq i (1+ i)))
         (t
          (setq para (if para (concat para " " line) line)
                i (1+ i))))))
    (when para
      (setq out (concat out (harmless-page--paragraph para))))
    out))

(defun harmless-page--title (text fallback)
  "Return the first heading in TEXT, or FALLBACK."
  (let ((title nil))
    (dolist (line (split-string text "\n"))
      (when (and (not title)
                 (string-match "\\`#+ \\(.*\\)\\'" line))
        (setq title (string-trim (match-string 1 line)))))
    (if (and (stringp title) (not (string-empty-p title)))
        title
      fallback)))

(defun harmless-page--document (title body)
  "Return a full HTML document with TITLE and BODY."
  (unless (string-suffix-p "\n" body)
    (setq body (concat body "\n")))
  (concat "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"utf-8\">\n<title>"
          (harmless-page--escape title)
          "</title>\n</head>\n<body>\n"
          body
          "</body>\n</html>\n"))

(defun harmless-page--normalize (text)
  "Return TEXT with CR LF reduced to LF."
  (replace-regexp-in-string "\r\n?" "\n" (or text "")))

(defun harmless-page--read (path)
  "Return PATH as text."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents path))
    (harmless-page--normalize (buffer-string))))

(defun harmless-page--kind (path)
  "Return `markdown' or `html' for PATH, or signal."
  (let ((ext (downcase (or (file-name-extension path) ""))))
    (cond
     ((member ext '("md" "markdown")) 'markdown)
     ((member ext '("html" "htm")) 'html)
     (t (error "page source is not a page")))))

(defun harmless-page--html (source kind)
  "Return the page text for SOURCE of KIND."
  (let ((text (harmless-page--read source)))
    (if (eq kind 'markdown)
        (harmless-page--document
         (harmless-page--title
          text
          (file-name-sans-extension (file-name-nondirectory source)))
         (harmless-page--markdown-body text))
      (if (string-match-p "<!doctype\\|<html" text)
          (if (string-suffix-p "\n" text) text (concat text "\n"))
        (harmless-page--document
         (file-name-sans-extension (file-name-nondirectory source))
         text)))))

(defun harmless-page--not-file-p (raw full)
  "Return non-nil if RAW or FULL names a directory."
  (or (directory-name-p full)
      (and (stringp raw)
           (or (string-suffix-p "/" raw)
               (string-suffix-p "\\" raw)))))

(defun harmless-page--under-project (project full raw)
  "Return FULL when it stays inside PROJECT.
RAW is the path the user gave, used in the error text.  A missing
parent inside PROJECT is created."
  (when (harmless-page--not-file-p raw full)
    (error "page path is not a file"))
  (when (or (harmless-page--root-text-p full)
            (and (file-exists-p full)
                 (equal (file-truename full) (file-truename "/"))))
    (error "page path is the filesystem root"))
  (let ((parent (file-name-directory full)))
    (unless (and (stringp parent)
                 (not (harmless-page--root-text-p parent))
                 (or (harmless-same-directory-p parent project)
                     (harmless-directory-strictly-under-p parent project)))
      (error "page path escapes the project: %s" raw))
    (when (equal (directory-file-name (file-truename parent))
                 (directory-file-name (file-truename "/")))
      (error "page path is the filesystem root"))
    (unless (file-directory-p parent)
      (make-directory parent t)))
  full)

(defun harmless-page--absolute-dest (path)
  "Return absolute file PATH, or signal.
The parent must already exist.  It is not created."
  (when (harmless-page--not-file-p path path)
    (error "page path is not a file"))
  (when (or (harmless-page--root-text-p path)
            (and (file-exists-p path)
                 (equal (file-truename path) (file-truename "/"))))
    (error "page path is the filesystem root"))
  (let ((parent (file-name-directory path)))
    (unless (and (stringp parent)
                 (not (harmless-page--root-text-p parent)))
      (error "page path is the filesystem root"))
    (when (equal (directory-file-name (file-truename parent))
                 (directory-file-name (file-truename "/")))
      (error "page path is the filesystem root"))
    (unless (file-directory-p parent)
      (error "page path is missing")))
  path)

(defun harmless-page--dest-extension-p (path)
  "Return non-nil if PATH ends in .html or .htm."
  (member (downcase (or (file-name-extension path) ""))
          '("html" "htm")))

(defun harmless-page--default-dest (project source)
  "Return the default page path for SOURCE inside PROJECT."
  (let ((name (file-name-sans-extension
               (file-name-nondirectory source))))
    (unless (harmless-safe-path-component-p name)
      (error "page source is not a page"))
    (harmless-page--under-project
     project
     (expand-file-name (concat ".harmless/pages/" name ".html") project)
     (concat ".harmless/pages/" name ".html"))))

(defun harmless-page--destination (project source path)
  "Return the absolute page file for SOURCE in PROJECT.
Nil PATH uses `.harmless/pages/NAME.html'.  A relative PATH stays
inside PROJECT.  An absolute PATH is written only when its parent
already exists."
  (cond
   ((null path)
    (harmless-page--default-dest project source))
   ((not (stringp path))
    (error "page path must be a string"))
   (t
    (let ((raw (string-trim path)))
      (cond
       ((string-empty-p raw)
        (error "page path is empty"))
       ((harmless-page--root-text-p raw)
        (error "page path is the filesystem root"))
       ((string-prefix-p "~" raw)
        (error "page path must be absolute"))
       ((file-name-absolute-p raw)
        (when (harmless-page--not-file-p raw raw)
          (error "page path is not a file"))
        (unless (harmless-page--dest-extension-p raw)
          (error "page path is not a page"))
        (harmless-page--absolute-dest raw))
       (t
        (let ((full (expand-file-name raw project)))
          (when (harmless-page--not-file-p raw full)
            (error "page path is not a file"))
          (unless (harmless-page--dest-extension-p full)
            (error "page path is not a page"))
          (harmless-page--under-project project full raw))))))))

(defun harmless-page--source (project source)
  "Return the absolute source file for SOURCE inside PROJECT.
A relative SOURCE is expanded against PROJECT, not
`default-directory'.  \"~\" is not expanded."
  (cond
   ((null source) (error "page source is empty"))
   ((not (stringp source))
    (error "page source must be a string")))
  (setq source (string-trim source))
  (cond
   ((string-empty-p source)
    (error "page source is empty"))
   ((harmless-page--root-text-p source)
    (error "page source is the filesystem root"))
   ((string-prefix-p "~" source)
    (error "page source must be absolute")))
  (let ((raw source)
        (full (if (file-name-absolute-p source)
                  source
                (expand-file-name source project))))
    (when (harmless-page--not-file-p raw full)
      (error "page source is not a file"))
    (when (or (harmless-page--root-text-p full)
              (and (file-exists-p full)
                   (equal (file-truename full) (file-truename "/"))))
      (error "page source is the filesystem root"))
    (unless (harmless-directory-strictly-under-p full project)
      (error "page source escapes the project: %s" raw))
    (cond
     ((not (file-exists-p full))
      (error "page source is missing"))
     ((not (file-regular-p full))
      (error "page source is not a file"))
     (t full))))

(defun harmless-page-publish (directory source &optional path)
  "Publish SOURCE as an HTML page inside DIRECTORY.
Return the absolute page path.  Markdown becomes a document.
An HTML file that already has a document is copied.  A fragment
is wrapped.  SOURCE is not modified.  Nil PATH writes
`.harmless/pages/NAME.html'."
  (setq directory (harmless-page--directory directory))
  (let* ((full (harmless-page--source directory source))
         (kind (harmless-page--kind full))
         (dest (harmless-page--destination directory full path))
         (before (harmless-page--read full))
         (html (harmless-page--html full kind))
         (coding-system-for-write 'utf-8-unix))
    (when (or (equal full dest)
              (and (eq system-type 'windows-nt)
                   (equal (downcase full) (downcase dest))))
      (error "page path is the source"))
    (write-region html nil dest nil 'silent)
    (unless (equal before (harmless-page--read full))
      (error "page source changed"))
    dest))

(provide 'harmless-page)

;;; harmless-page.el ends here
