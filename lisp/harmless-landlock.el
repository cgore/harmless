;;; harmless-landlock.el --- Landlock sandbox for the Harmless shell -*- lexical-binding: t; -*-

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
;; Build argv for `src/harmless-landlock'.  The session decides whether
;; the shell is sandboxed.  This file does not start the process.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-tools)
(require 'harmless-tools-fs)

(defcustom harmless-landlock-program nil
  "Absolute path of the Landlock helper, or nil to locate it.
Nil uses `harmless-landlock' in `../src/' beside `harmless.el'.
A relative path is refused when a command is sandboxed."
  :type '(choice (const nil) file)
  :group 'harmless)

(defvar harmless-landlock--abi-cache nil
  "Cons of (PROGRAM . ABI) after a successful Landlock probe.
A failed probe is not stored.")

(defun harmless-landlock--program ()
  "Return the Landlock helper path.
`harmless-landlock-program' wins, including a relative path.
Otherwise this is `../src/harmless-landlock' beside `harmless.el'."
  (or harmless-landlock-program
      (let ((lib (locate-library "harmless.el" t)))
        (and (stringp lib)
             (expand-file-name
              "../src/harmless-landlock"
              (file-name-directory lib))))))

(defun harmless-landlock--probe (program)
  "Return the Landlock ABI of PROGRAM, or nil."
  (with-temp-buffer
    (let ((status (condition-case nil
                      (call-process program nil
                                    (list (current-buffer) nil)
                                    nil "--abi")
                    (error 127))))
      (when (and (integerp status) (zerop status))
        (let ((text (string-trim (buffer-string))))
          (when (string-match-p "\\`[0-9]+\\'" text)
            (string-to-number text)))))))

(defun harmless-landlock-abi ()
  "Return the Landlock ABI of the helper.
Signal when the helper is missing or Landlock is unavailable.
A successful probe is remembered for that program path."
  (let ((program (harmless-landlock--program)))
    (unless (and (stringp program)
                 (file-name-absolute-p program)
                 (file-executable-p program)
                 (not (file-directory-p program)))
      (error "Landlock helper is missing"))
    (if (and (consp harmless-landlock--abi-cache)
             (equal (car harmless-landlock--abi-cache) program)
             (integerp (cdr harmless-landlock--abi-cache)))
        (cdr harmless-landlock--abi-cache)
      (let ((abi (harmless-landlock--probe program)))
        (unless (and (integerp abi) (>= abi 1))
          (error "Landlock is not available"))
        (setq harmless-landlock--abi-cache (cons program abi))
        abi))))

(defun harmless-landlock--present-p (args key)
  "Return non-nil when KEY is present in plist ARGS.
A string name is accepted too."
  (and (listp args)
       (or (plist-member args key)
           (let ((name (if (keywordp key)
                           (substring (symbol-name key) 1)
                         key)))
             (cl-loop for k in args by #'cddr
                      thereis (and (stringp k) (string= k name)))))))

(defun harmless-landlock--network (args)
  "Return `allow', `deny', or an error string for ARGS.
Omitted, nil, and `:false' deny the network.  `t' allows it.
Any other value is \"Error: network must be a boolean\"."
  (if (not (harmless-landlock--present-p args :network))
      'deny
    (let ((value (harmless-tool-arg args :network)))
      (cond
       ((or (null value) (eq value :false)) 'deny)
       ((eq value t) 'allow)
       (t "Error: network must be a boolean")))))

(defun harmless-landlock--clean-dir (path)
  "Return PATH as a directory Landlock may write, or nil.
PATH must exist, be absolute, and not be the filesystem root.
A relative path, `~', and a drive path are not expanded."
  (when (and (stringp path)
             (not (string-empty-p path))
             (file-name-absolute-p path)
             (not (string-prefix-p "~" path))
             (not (harmless-foreign-absolute-p path))
             (not (harmless-filesystem-root-p path))
             (file-directory-p path))
    (let ((full (file-truename (directory-file-name path))))
      (when (and (stringp full)
                 (file-name-absolute-p full)
                 (not (harmless-filesystem-root-p full))
                 (file-directory-p full))
        (file-name-as-directory full)))))

(defun harmless-landlock--data-dir ()
  "Return the Harmless data directory, or nil when it is refused."
  (condition-case nil
      (harmless-data-directory)
    (error nil)))

(defun harmless-landlock--covers-p (parent path)
  "Return non-nil when PATH is PARENT or strictly inside it.
A symlink is compared by its real directory too."
  (or (harmless-same-directory-p path parent)
      (harmless-directory-strictly-under-p path parent)
      (let ((path-real (and (file-exists-p path) (file-truename path)))
            (parent-real (and (file-exists-p parent)
                              (file-truename parent))))
        (and (stringp path-real)
             (stringp parent-real)
             (or (harmless-same-directory-p path-real parent-real)
                 (harmless-directory-strictly-under-p
                  path-real parent-real))))))

(defun harmless-landlock--blocked-data-p (session path)
  "Return non-nil if PATH must not be its own write root.
The data directory, and a directory inside it, are blocked
unless PATH is the project or strictly inside the project."
  (let ((data (harmless-landlock--data-dir))
        (project (condition-case nil
                     (harmless-session-require-project session)
                   (error nil))))
    (and (stringp path)
         (stringp data)
         (harmless-landlock--covers-p data path)
         (not (and (stringp project)
                   (harmless-landlock--covers-p project path))))))

(defun harmless-landlock--project (session)
  "Return SESSION's project as a Landlock write directory.
Signal when that directory cannot be opened."
  (let ((raw (harmless-session-require-project session)))
    (or (harmless-landlock--clean-dir raw)
        (error "Landlock could not open %s" raw))))

(defun harmless-landlock--add-dir (session path out)
  "Return OUT plus PATH when Landlock may write that directory."
  (let ((clean (harmless-landlock--clean-dir path)))
    (if (and clean
             (not (harmless-landlock--blocked-data-p session clean))
             (not (cl-some (lambda (have)
                             (harmless-same-directory-p have clean))
                           out)))
        (append out (list clean))
      out)))

(defun harmless-landlock--devices ()
  "Return device files Landlock may write.
A missing path is left out.  A directory is left out.  A truename
outside `/dev/' is left out."
  (let (out)
    (dolist (path '("/dev/null" "/dev/zero" "/dev/full"
                    "/dev/tty" "/dev/urandom" "/dev/random"))
      (when (and (stringp path)
                 (file-name-absolute-p path)
                 (file-exists-p path)
                 (not (file-directory-p path))
                 (not (harmless-filesystem-root-p path)))
        (let ((full (file-truename path)))
          (when (and (stringp full)
                     (file-name-absolute-p full)
                     (not (file-directory-p full))
                     (not (harmless-filesystem-root-p full))
                     (string-prefix-p "/dev/" full)
                     (not (member full out)))
            (push full out)))))
    (nreverse out)))

(defun harmless-landlock--write-paths (session)
  "Return write paths for a workspace-write sandbox of SESSION.
The project is first and must be openable.  An unusable extra
directory is skipped.  Device files come last."
  (let ((out (list (harmless-landlock--project session))))
    (dolist (extra (harmless-extra--usable session))
      (setq out (harmless-landlock--add-dir session extra out)))
    (dolist (temp (list temporary-file-directory
                        (getenv "TMPDIR")
                        "/tmp" "/var/tmp" "/dev/shm"))
      (when (and (stringp temp)
                 (not (string-empty-p temp))
                 (file-name-absolute-p temp)
                 (not (string-prefix-p "~" temp))
                 (not (harmless-foreign-absolute-p temp)))
        (setq out (harmless-landlock--add-dir session temp out))))
    (dolist (dev (harmless-landlock--devices))
      (unless (member dev out)
        (setq out (append out (list dev)))))
    out))

(defun harmless-landlock-wrap (session allow command)
  "Return argv that runs COMMAND under SESSION's Landlock sandbox.
ALLOW non-nil permits network connections.  COMMAND is the shell
argv.  Signal when this system cannot apply the sandbox.
`read-only' adds no write rule.  `workspace-write' may write the
project, added directories, temporary directories, and devices."
  (unless (eq system-type 'gnu/linux)
    (error "Landlock sandbox requires Linux"))
  (let ((abi (harmless-landlock-abi))
        (mode (harmless-shell-sandbox-effective session))
        (argv (list (harmless-landlock--program) "--read" "/")))
    (when (and (not allow) (< abi 4))
      (error "Landlock cannot restrict the network on this kernel"))
    (when (eq mode 'workspace-write)
      (dolist (path (harmless-landlock--write-paths session))
        (setq argv (append argv (list "--write" path)))))
    (append argv
            (list "--network" (if allow "allow" "deny") "--")
            command)))

(provide 'harmless-landlock)

;;; harmless-landlock.el ends here
