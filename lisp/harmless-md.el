;;; harmless-md.el --- Display Markdown in Harmless transcripts -*- lexical-binding: t; -*-

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
;; Insert assistant Markdown as displayed text: emphasis markers hidden,
;; headings and code styled.  Uses markdown-mode when it is loaded; otherwise
;; a small built-in renderer.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)

(defvar markdown-hide-markup)
(defvar markdown-hide-markup-in-view-modes)
(defvar markdown-fontify-code-blocks-natively)
(declare-function gfm-view-mode "markdown-mode")
(declare-function markdown-get-lang-mode "markdown-mode")

(defcustom harmless-md-use-markdown-mode t
  "When non-nil, render with `gfm-view-mode' if markdown-mode is available."
  :type 'boolean
  :group 'harmless)

(defcustom harmless-md-fontify-code t
  "When non-nil, syntax-highlight fenced code blocks by language."
  :type 'boolean
  :group 'harmless)

(defface harmless-md-h1 '((t :inherit info-title-1))
  "Face for Markdown heading 1."
  :group 'harmless)

(defface harmless-md-h2 '((t :inherit info-title-2))
  "Face for Markdown heading 2."
  :group 'harmless)

(defface harmless-md-h3 '((t :inherit info-title-3))
  "Face for Markdown heading 3."
  :group 'harmless)

(defface harmless-md-h4 '((t :weight bold))
  "Face for Markdown heading 4+."
  :group 'harmless)

(defface harmless-md-code
  '((t :inherit (fixed-pitch font-lock-constant-face)))
  "Face for inline Markdown code."
  :group 'harmless)

(defface harmless-md-code-block
  '((t :inherit (fixed-pitch font-lock-string-face) :extend t))
  "Face for fenced Markdown code blocks."
  :group 'harmless)

(defface harmless-md-quote
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for Markdown block quotes."
  :group 'harmless)

(defface harmless-md-link
  '((t :inherit link))
  "Face for Markdown links."
  :group 'harmless)

(defun harmless-md-insert (text)
  "Insert TEXT at point as displayed Markdown."
  (let ((text (harmless-ensure-utf8 (or text ""))))
    (if (and harmless-md-use-markdown-mode
             (require 'markdown-mode nil t)
             (fboundp 'gfm-view-mode))
        (insert (harmless-md--via-markdown-mode text))
      (harmless-md--insert-builtin text))))

(defun harmless-md--via-markdown-mode (text)
  "Return TEXT fontified by `gfm-view-mode', markup hidden."
  (with-temp-buffer
    (insert text)
    (let ((markdown-hide-markup t)
          (markdown-hide-markup-in-view-modes t)
          (markdown-fontify-code-blocks-natively
           harmless-md-fontify-code))
      (delay-mode-hooks
        (gfm-view-mode))
      (font-lock-ensure)
      (let ((s (buffer-substring (point-min) (point-max))))
        (remove-list-of-text-properties
         0 (length s)
         '(read-only keymap local-map) s)
        s))))

(defun harmless-md--starts-at (string index prefix)
  "Return non-nil if STRING has PREFIX at INDEX."
  (let ((n (length prefix)))
    (and (<= (+ index n) (length string))
         (string= (substring string index (+ index n)) prefix))))

(defun harmless-md--faces (face extra)
  "Combine FACE and EXTRA into a face spec."
  (cond
   ((and face extra) (list face extra))
   (t (or face extra))))

(defun harmless-md--insert-code-block (lang code)
  "Insert fenced CODE for LANG."
  (unless (string-suffix-p "\n" code)
    (setq code (concat code "\n")))
  (when (and lang (not (string-empty-p lang)))
    (insert (propertize (concat " " lang "\n") 'face 'shadow)))
  (let ((fontified (and harmless-md-fontify-code
                        (harmless-md--fontify-lang lang code))))
    (insert (or fontified
                (propertize code 'face 'harmless-md-code-block)))))

(defun harmless-md--lang-mode (lang)
  "Return a major mode symbol for LANG, or nil."
  (when (and lang (not (string-empty-p lang)))
    (or (and (fboundp 'markdown-get-lang-mode)
             (markdown-get-lang-mode lang))
        (let ((sym (intern (concat lang "-mode"))))
          (and (fboundp sym) sym))
        (pcase lang
          ((or "elisp" "emacs-lisp" "el") 'emacs-lisp-mode)
          ((or "js" "javascript") 'js-mode)
          ((or "ts" "typescript") 'typescript-mode)
          ((or "py" "python") 'python-mode)
          ((or "sh" "bash" "shell") 'sh-mode)
          ((or "c") 'c-mode)
          ((or "c++" "cpp") 'c++-mode)
          ((or "json") 'js-json-mode)
          ((or "yaml" "yml") 'yaml-mode)
          ((or "html") 'html-mode)
          (_ nil)))))

(defun harmless-md--fontify-lang (lang code)
  "Return CODE with native font-lock for LANG, or nil on failure."
  (when-let* ((mode (harmless-md--lang-mode lang)))
    (condition-case nil
        (with-temp-buffer
          (insert code)
          (delay-mode-hooks
            (funcall mode)
            (ignore-errors (font-lock-mode 1))
            (font-lock-ensure (point-min) (point-max)))
          (buffer-substring (point-min) (point-max)))
      (error nil))))

(defun harmless-md--insert-heading (level title)
  "Insert a heading of LEVEL with TITLE."
  (insert (propertize title
                      'face (pcase level
                              (1 'harmless-md-h1)
                              (2 'harmless-md-h2)
                              (3 'harmless-md-h3)
                              (_ 'harmless-md-h4)))
          "\n"))

(defun harmless-md--insert-inline (text extra-face)
  "Insert inline Markdown from TEXT with EXTRA-FACE."
  (let ((i 0)
        (n (length text)))
    (while (< i n)
      (cond
       ((and (< (1+ i) n)
             (eq (aref text i) ?`)
             (not (eq (aref text (1+ i)) ?`)))
        (let ((j (string-search "`" text (1+ i))))
          (if (not j)
              (progn (insert "`") (setq i (1+ i)))
            (insert (propertize (substring text (1+ i) j)
                                'face (harmless-md--faces 'harmless-md-code extra-face)))
            (setq i (1+ j)))))
       ((harmless-md--starts-at text i "**")
        (let ((j (string-search "**" text (+ i 2))))
          (if (not j)
              (progn (insert "*") (setq i (1+ i)))
            (insert (propertize (substring text (+ i 2) j)
                                'face (harmless-md--faces 'bold extra-face)))
            (setq i (+ j 2)))))
       ((harmless-md--starts-at text i "__")
        (let ((j (string-search "__" text (+ i 2))))
          (if (not j)
              (progn (insert "_") (setq i (1+ i)))
            (insert (propertize (substring text (+ i 2) j)
                                'face (harmless-md--faces 'bold extra-face)))
            (setq i (+ j 2)))))
       ((harmless-md--starts-at text i "~~")
        (let ((j (string-search "~~" text (+ i 2))))
          (if (not j)
              (progn (insert "~") (setq i (1+ i)))
            (insert (propertize (substring text (+ i 2) j)
                                'face '(:strike-through t)))
            (setq i (+ j 2)))))
       ((and (eq (aref text i) ?*)
             (or (= i 0) (memq (aref text (1- i)) '(?\s ?\t))))
        (let ((j (string-search "*" text (1+ i))))
          (if (or (not j) (= j (1+ i)))
              (progn (insert "*") (setq i (1+ i)))
            (insert (propertize (substring text (1+ i) j)
                                'face (harmless-md--faces 'italic extra-face)))
            (setq i (1+ j)))))
       ((and (eq (aref text i) ?!)
             (< (1+ i) n)
             (eq (aref text (1+ i)) ?\[)
             (string-match "!\\[\\([^]]*\\)\\](\\([^)]*\\))" text i)
             (= (match-beginning 0) i))
        (insert (propertize (format "[image: %s]" (match-string 1 text))
                            'face 'harmless-md-link
                            'help-echo (match-string 2 text)
                            'harmless-md-url (match-string 2 text)))
        (setq i (match-end 0)))
       ((and (eq (aref text i) ?\[)
             (string-match "\\[\\([^]]+\\)\\](\\([^)]*\\))" text i)
             (= (match-beginning 0) i))
        (insert (propertize (match-string 1 text)
                            'face 'harmless-md-link
                            'help-echo (match-string 2 text)
                            'harmless-md-url (match-string 2 text)
                            'mouse-face 'highlight))
        (setq i (match-end 0)))
       (t
        (insert (if extra-face
                    (propertize (string (aref text i)) 'face extra-face)
                  (string (aref text i))))
        (setq i (1+ i)))))))

(defun harmless-md--insert-prose (text)
  "Insert non-fenced Markdown TEXT."
  (dolist (line (split-string text "\n"))
    (cond
     ((string-match "^\\(######\\|#####\\|####\\|###\\|##\\|#\\) \\(.*\\)$" line)
      (harmless-md--insert-heading (length (match-string 1 line))
                                   (match-string 2 line)))
     ((string-match "^> ?\\(.*\\)$" line)
      (insert (propertize "│ " 'face 'harmless-md-quote))
      (harmless-md--insert-inline (match-string 1 line) 'harmless-md-quote)
      (insert "\n"))
     ((string-match "^[-*+] \\(.*\\)$" line)
      (insert "• ")
      (harmless-md--insert-inline (match-string 1 line) nil)
      (insert "\n"))
     ((string-match "^\\([0-9]+\\)\\. \\(.*\\)$" line)
      (insert (match-string 1 line) ". ")
      (harmless-md--insert-inline (match-string 2 line) nil)
      (insert "\n"))
     ((string-match "^\\(?:---\\|\\*\\*\\*\\|___\\)\\s-*$" line)
      (insert (propertize "────────\n" 'face 'shadow)))
     (t
      (harmless-md--insert-inline line nil)
      (insert "\n")))))

(defun harmless-md--insert-builtin (text)
  "Insert TEXT using the built-in Markdown renderer."
  (let ((start 0)
        (len (length text)))
    (while (< start len)
      (if (string-match "^\\(```\\|~~~\\)\\([^\n]*\\)\n" text start)
          (let* ((fence-beg (match-beginning 0))
                 (fence (match-string 1 text))
                 (info (string-trim (match-string 2 text)))
                 (body-beg (match-end 0)))
            (when (> fence-beg start)
              (harmless-md--insert-prose (substring text start fence-beg)))
            (let ((close (string-match
                          (concat "^" (regexp-quote fence) "[ \t]*$")
                          text body-beg)))
              (if (not close)
                  (progn
                    (harmless-md--insert-prose (substring text fence-beg))
                    (setq start len))
                (harmless-md--insert-code-block
                 (car (split-string info))
                 (substring text body-beg close))
                (setq start (match-end 0))
                (when (and (< start len) (eq (aref text start) ?\n))
                  (setq start (1+ start))))))
        (harmless-md--insert-prose (substring text start))
        (setq start len)))))

(provide 'harmless-md)

;;; harmless-md.el ends here
