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

(defcustom harmless-md-use-markdown-mode nil
  "Obsolete.  Rendering always uses the built-in renderer.
`gfm-view-mode' font-lock calls `markdown-get-lang-mode' and does not
return when a fenced block is drawn from a process filter."
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

(defface harmless-md-table
  '((t :inherit fixed-pitch))
  "Face for Markdown table grid."
  :group 'harmless)

(defface harmless-md-table-header
  '((t :inherit (fixed-pitch bold)))
  "Face for Markdown table header cells."
  :group 'harmless)

(defun harmless-md-insert (text)
  "Insert TEXT at point as displayed Markdown."
  (harmless-md--insert-document (harmless-ensure-utf8 (or text ""))))

(defun harmless-md--insert-chunk (text)
  "Insert a non-table, non-fence Markdown chunk.
The built-in renderer is used on purpose.  `gfm-view-mode' font-lock
calls `markdown-get-lang-mode' and `markdown-search-until-condition',
and those do not return on fenced blocks inside the process filter."
  (when (and text (not (string-empty-p text)))
    (harmless-md--insert-prose text)))

(defun harmless-md--bol-p (text pos)
  "Return non-nil if POS is the beginning of a line in TEXT."
  (or (eq pos 0)
      (eq (aref text (1- pos)) ?\n)))

(defconst harmless-md--markdown-modes
  '(markdown-mode gfm-mode gfm-view-mode markdown-view-mode)
  "Modes that must not be used to highlight a fenced block.
`gfm-view-mode' font-lock calls `markdown-get-lang-mode' and then
fontifies that mode.  A markdown fence, or an unclosed fence, sends
that search into a loop inside the process filter.")

(defun harmless-md--via-markdown-mode (text)
  "Return TEXT fontified by `gfm-view-mode', markup hidden.
Code fences are highlighted by `harmless-md--fontify-lang' instead.
Leaving native fence fontification on here re-enters markdown-mode."
  (with-temp-buffer
    (insert text)
    (let ((markdown-hide-markup t)
          (markdown-hide-markup-in-view-modes t)
          (markdown-fontify-code-blocks-natively nil))
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
  "Return a major mode symbol for LANG, or nil.
This does not call `markdown-get-lang-mode'.  That function scans
`auto-mode-alist' and tree-sitter, and it does not return when a
fence is being highlighted from a process filter."
  (when (and lang (not (string-empty-p lang)))
    (let ((mode (or (let ((sym (intern (concat lang "-mode"))))
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
      (unless (memq mode harmless-md--markdown-modes)
        mode))))

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

(defun harmless-md--split-row (line)
  "Split a GFM table LINE into cell strings."
  (let* ((s (string-trim line))
         (s (if (string-prefix-p "|" s) (substring s 1) s))
         (s (if (string-suffix-p "|" s) (substring s 0 -1) s)))
    (mapcar #'string-trim (split-string s "|"))))

(defun harmless-md--table-row-p (line)
  "Return non-nil if LINE looks like a GFM table row."
  (string-match-p "\\`[ \t]*|" line))

(defun harmless-md--delimiter-cell-p (cell)
  "Return non-nil if CELL is a GFM alignment delimiter."
  (string-match-p "\\`:?-+:?\\'" cell))

(defun harmless-md--align (cell)
  "Return alignment symbol for a delimiter CELL."
  (let ((left (string-prefix-p ":" cell))
        (right (string-suffix-p ":" cell)))
    (cond
     ((and left right) 'center)
     (right 'right)
     (t 'left))))

(defun harmless-md--pad-list (items n fill)
  "Pad ITEMS to length N with FILL."
  (let ((items (append items nil)))
    (while (< (length items) n)
      (setq items (append items (list fill))))
    (cl-subseq items 0 n)))

(defun harmless-md--line-at (text pos)
  "Return (NEXT-POS LINE) for the line in TEXT starting at POS."
  (let* ((eol (or (string-search "\n" text pos) (length text)))
         (line (substring text pos eol))
         (next (if (< eol (length text)) (1+ eol) eol)))
    (list next line)))

(defun harmless-md--parse-table (text start)
  "If a GFM table starts at START, return (END ROWS ALIGNS), else nil."
  (when (and (harmless-md--bol-p text start)
             (< start (length text)))
    (let* ((header (harmless-md--line-at text start))
           (header-line (nth 1 header)))
      (when (harmless-md--table-row-p header-line)
        (let* ((sep-pos (nth 0 header)))
          (when (< sep-pos (length text))
            (let* ((sep (harmless-md--line-at text sep-pos))
                   (sep-cells (and (harmless-md--table-row-p (nth 1 sep))
                                   (harmless-md--split-row (nth 1 sep)))))
              (when (and sep-cells (cl-every #'harmless-md--delimiter-cell-p sep-cells))
                (let ((rows (list (harmless-md--split-row header-line)))
                      (aligns (mapcar #'harmless-md--align sep-cells))
                      (pos (nth 0 sep)))
                  (while (and (< pos (length text))
                              (let ((ln (nth 1 (harmless-md--line-at text pos))))
                                (and (harmless-md--table-row-p ln)
                                     (not (string-empty-p (string-trim ln))))))
                    (let ((ln (harmless-md--line-at text pos)))
                      (push (harmless-md--split-row (nth 1 ln)) rows)
                      (setq pos (nth 0 ln))))
                  (list pos (nreverse rows) aligns))))))))))

(defun harmless-md--cell-width (text)
  "Return display width of inline Markdown TEXT."
  (with-temp-buffer
    (harmless-md--insert-inline text nil)
    (string-width (buffer-substring-no-properties (point-min) (point-max)))))

(defun harmless-md--insert-padded-cell (text width align)
  "Insert Markdown cell TEXT padded to WIDTH with ALIGN."
  (let* ((inner (with-temp-buffer
                  (harmless-md--insert-inline text nil)
                  (buffer-string)))
         (w (string-width (substring-no-properties inner)))
         (pad (max 0 (- width w)))
         (left (pcase align
                 ('right pad)
                 ('center (/ pad 2))
                 (_ 0)))
         (right (- pad left)))
    (insert (make-string left ?\s)
            inner
            (make-string right ?\s))))

(defun harmless-md--hline (widths left mid right)
  "Insert a box-drawing rule for column WIDTHS."
  (insert
   (propertize
    (concat left
            (mapconcat (lambda (w) (make-string (+ w 2) ?─)) widths mid)
            right
            "\n")
    'face 'harmless-md-table)))

(defun harmless-md--insert-table-row (cells widths aligns header)
  "Insert one table row."
  (insert (propertize "│" 'face 'harmless-md-table))
  (cl-loop for cell in cells
           for width in widths
           for align in aligns
           do
           (insert (propertize " " 'face 'harmless-md-table))
           (let ((beg (point)))
             (harmless-md--insert-padded-cell cell width align)
             (when header
               (add-face-text-property beg (point) 'harmless-md-table-header))
             (add-face-text-property beg (point) 'harmless-md-table))
           (insert (propertize " │" 'face 'harmless-md-table)))
  (insert "\n"))

(defun harmless-md--insert-table (rows aligns)
  "Insert a pretty table from ROWS and ALIGNS."
  (let* ((ncols (apply #'max 1 (mapcar #'length rows)))
         (aligns (harmless-md--pad-list aligns ncols 'left))
         (rows (mapcar (lambda (r) (harmless-md--pad-list r ncols "")) rows))
         (widths (make-list ncols 0)))
    (dolist (row rows)
      (setq widths
            (cl-mapcar (lambda (w c) (max w (harmless-md--cell-width c)))
                       widths row)))
    (harmless-md--hline widths "┌" "┬" "┐")
    (harmless-md--insert-table-row (car rows) widths aligns t)
    (harmless-md--hline widths "├" "┼" "┤")
    (dolist (row (cdr rows))
      (harmless-md--insert-table-row row widths aligns nil))
    (harmless-md--hline widths "└" "┴" "┘")))

(defun harmless-md--next-special (text start)
  "Return the next fence or table position after START, or end of TEXT."
  (let ((p (if (harmless-md--bol-p text start)
               start
             (let ((nl (string-search "\n" text start)))
               (if nl (1+ nl) (length text))))))
    (when (= p start)
      (let ((nl (string-search "\n" text start)))
        (setq p (if nl (1+ nl) (length text)))))
    (catch 'found
      (while (< p (length text))
        (when (or (and (or (harmless-md--starts-at text p "```")
                           (harmless-md--starts-at text p "~~~"))
                       (harmless-md--bol-p text p))
                  (harmless-md--parse-table text p))
          (throw 'found p))
        (let ((nl (string-search "\n" text p)))
          (setq p (if nl (1+ nl) (length text)))))
      (length text))))

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

(defun harmless-md--insert-document (text)
  "Insert TEXT, pretty-printing tables and fenced code."
  (let ((start 0)
        (len (length text)))
    (while (< start len)
      (cond
       ((and (harmless-md--bol-p text start)
             (or (harmless-md--starts-at text start "```")
                 (harmless-md--starts-at text start "~~~"))
             (string-match "^\\(```\\|~~~\\)\\([^\n]*\\)\n" text start)
             (eq (match-beginning 0) start))
        (let* ((fence (match-string 1 text))
               (info (string-trim (match-string 2 text)))
               (body-beg (match-end 0))
               (close (string-match
                       (concat "^" (regexp-quote fence) "[ \t]*$")
                       text body-beg))
               ;; insert-code-block runs font-lock, which clobbers match-data.
               (close-end (and close (match-end 0))))
          (if (not close)
              (progn
                (harmless-md--insert-chunk (substring text start))
                (setq start len))
            (harmless-md--insert-code-block
             (car (split-string info))
             (substring text body-beg close))
            (setq start close-end)
            (when (and (< start len) (eq (aref text start) ?\n))
              (setq start (1+ start))))))
       ((harmless-md--parse-table text start)
        (let* ((parsed (harmless-md--parse-table text start))
               (next (nth 0 parsed)))
          (harmless-md--insert-table (nth 1 parsed) (nth 2 parsed))
          (setq start (if (> next start) next len))))
       (t
        (let ((next (harmless-md--next-special text start)))
          (when (<= next start)
            (setq next len))
          (harmless-md--insert-chunk (substring text start next))
          (setq start next)))))))

(provide 'harmless-md)

;;; harmless-md.el ends here
