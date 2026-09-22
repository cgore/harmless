;;; harmless-skills.el --- Skill discovery for Harmless -*- lexical-binding: t; -*-

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
;; A skill is a directory containing SKILL.md.  Each turn lists the
;; name, description, and path.  The model reads the file when the
;; skill applies.  Discovery walks the same directories as project
;; instructions and reads Harmless, Grok, Claude, and Codex layouts.
;; A directory closer to the session wins.  In one directory the
;; later vendor wins: agents, codex, claude, grok, then harmless.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-instructions)
(require 'harmless-memory)

(defconst harmless-skill-vendors
  '("agents" "codex" "claude" "grok" "harmless")
  "Dot-directory names that contain a skills folder, lowest priority first.")

(defconst harmless-skill-admin-root "/etc/codex/skills"
  "Machine-wide Codex skill directory, scanned before any user or project skill.")

(defun harmless-skill-roots (dir &optional stop)
  "Return skill directories that apply to DIR, lowest priority first.
STOP is passed to `harmless-instruction-directories'."
  (let (roots)
    (when (file-directory-p harmless-skill-admin-root)
      (push harmless-skill-admin-root roots))
    (dolist (ancestor (harmless-instruction-directories dir stop))
      (dolist (vendor harmless-skill-vendors)
        (push (expand-file-name
               "skills"
               (expand-file-name (concat "." vendor) ancestor))
              roots)))
    (nreverse roots)))

(defun harmless-skills--find (dir seen)
  "Return SKILL.md files under DIR.  SEEN records visited truename keys."
  (when (file-directory-p dir)
    (let ((key (file-truename dir)))
      (unless (gethash key seen)
        (puthash key t seen)
        (let (found)
          (dolist (name (directory-files dir))
            (unless (or (member name '("." ".."))
                        (eq (aref name 0) ?.)
                        (string= name "node_modules"))
              (let ((path (expand-file-name name dir)))
                (cond
                 ((file-directory-p path)
                  (setq found (append found (harmless-skills--find path seen))))
                 ((and (string= name "SKILL.md") (file-readable-p path))
                  (setq found (append found (list path))))))))
          found)))))

(defun harmless-skill--unquote (value)
  "Strip one pair of matching quotes from VALUE."
  (if (and (>= (length value) 2)
           (member (aref value 0) '(?\" ?'))
           (eq (aref value 0) (aref value (1- (length value)))))
      (substring value 1 -1)
    value))

(defun harmless-skill--normalize-name (name)
  "Return NAME as a lowercase hyphenated skill id."
  (let ((slug (replace-regexp-in-string
               "[^a-z0-9]+" "-"
               (downcase (string-trim (or name ""))))))
    (setq slug (replace-regexp-in-string "\\`-+" "" slug))
    (replace-regexp-in-string "-+\\'" "" slug)))

(defun harmless-skill--first-paragraph (text)
  "Return the first paragraph of TEXT as one line, or nil."
  (let ((lines nil)
        (done nil))
    (dolist (line (split-string (or text "") "\n"))
      (unless done
        (setq line (string-trim line))
        (cond
         ((string-empty-p line)
          (when lines (setq done t)))
         (t (push line lines)))))
    (when lines
      (string-join (nreverse lines) " "))))

(defun harmless-skill--frontmatter (text)
  "Return (NAME DESCRIPTION BODY) from TEXT.
NAME and DESCRIPTION are nil when the field is absent.  BODY is the
markdown after the frontmatter, or all of TEXT when there is none."
  (if (not (string-match "\\`---[ \t]*\n\\(\\(?:.*\n\\)*?\\)---[ \t]*\n?" text))
      (list nil nil text)
    (let ((name nil)
          (description nil))
      (dolist (line (split-string (match-string 1 text) "\n" t))
        (when (string-match "\\`\\([A-Za-z0-9_-]+\\):[ \t]*\\(.*\\)\\'" line)
          (let ((key (match-string 1 line))
                (value (harmless-skill--unquote (string-trim (match-string 2 line)))))
            (cond
             ((string= key "name") (setq name value))
             ((and (string= key "description")
                   (not (member value '("" ">" "|" ">-" "|-" ">"))))
              (setq description value))))))
      (list name description (substring text (match-end 0))))))

(defun harmless-skill--parse (path)
  "Return a skill plist for PATH, or nil when it has no usable summary."
  (let* ((raw (with-temp-buffer
                (let ((coding-system-for-read 'utf-8-unix))
                  (insert-file-contents path))
                (buffer-string)))
         (parsed (harmless-skill--frontmatter raw))
         (name (harmless-skill--normalize-name
                (or (nth 0 parsed)
                    (file-name-nondirectory
                     (directory-file-name (file-name-directory path))))))
         (description (or (nth 1 parsed)
                          (harmless-skill--first-paragraph (nth 2 parsed)))))
    (when (and (not (string-empty-p name))
               description
               (not (string-empty-p (string-trim description))))
      (list :name name
            :description (harmless-truncate (string-trim description) 500)
            :path path))))

(defun harmless-skills-discover (dir &optional stop)
  "Return skills that apply to DIR, sorted by name.
A closer directory overrides a farther one.  STOP bounds the walk."
  (let ((by-name (make-hash-table :test 'equal))
        (seen (make-hash-table :test 'equal))
        (skills nil))
    (dolist (root (harmless-skill-roots dir stop))
      (dolist (file (harmless-skills--find root seen))
        (when-let* ((skill (harmless-skill--parse file)))
          (puthash (plist-get skill :name) skill by-name))))
    (maphash (lambda (_name skill) (push skill skills)) by-name)
    (sort skills (lambda (a b)
                   (string< (plist-get a :name) (plist-get b :name))))))

(defun harmless-skills-catalog (dir &optional stop)
  "Return the skill list for DIR, or nil when there are no skills.
STOP is passed to `harmless-skills-discover'."
  (let ((skills (harmless-skills-discover dir stop)))
    (when skills
      (concat
       "Skills available for this session.  Read a skill's SKILL.md with read_file when it applies to the task.\n\n"
       (mapconcat
        (lambda (skill)
          (format "- %s (%s): %s"
                  (plist-get skill :name)
                  (plist-get skill :path)
                  (plist-get skill :description)))
        skills
        "\n")))))

(defun harmless-context-messages (dir messages &optional stop)
  "Return MESSAGES with instructions, skills, and memory for DIR.
STOP bounds the instruction and skill walks.  The session transcript
is not modified."
  (harmless-context-append
   (harmless-context-append
    (harmless-instructions-apply dir messages stop)
    (harmless-skills-catalog dir stop))
   (harmless-memory-catalog dir)))

(provide 'harmless-skills)

;;; harmless-skills.el ends here
