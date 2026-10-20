;;; harmless-computer.el --- Screen and browser control -*- lexical-binding: t; -*-

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
;; See the screen, click, and type.  Open a browser the user can
;; watch.  The class is `shell', so accept-edits still asks.
;; Nothing is written into the project.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'harmless-util)
(require 'harmless-session)
(require 'harmless-tools)
(require 'harmless-web)

(defvar harmless-computer-capture-function
  #'harmless-computer-capture-default
  "Function () that returns (WIDTH HEIGHT PATH).
PATH is a PNG under the Harmless data directory.")

(defvar harmless-computer-act-function #'harmless-computer-act-default
  "Function (ACTION) that clicks, types, or presses a key.
ACTION is a plist: :click, :type, or :key.")

(defvar harmless-computer-browse-function
  #'harmless-computer-browse-default
  "Function (URL) that opens URL in a browser the user can watch.")

(defvar harmless-computer-run-function #'harmless-computer-run-default
  "Function (SCRIPT) that runs one PowerShell script.
Return stdout.  A missing PowerShell is refused before a process
starts.")

(defconst harmless-computer-plan-refusal
  "Error: Plan mode is on. Computer control is refused"
  "Tool result when plan mode refuses a computer action.")

(defconst harmless-computer--keys
  '(("enter" . "{ENTER}")
    ("tab" . "{TAB}")
    ("escape" . "{ESC}")
    ("backspace" . "{BS}")
    ("delete" . "{DEL}")
    ("up" . "{UP}")
    ("down" . "{DOWN}")
    ("left" . "{LEFT}")
    ("right" . "{RIGHT}")
    ("home" . "{HOME}")
    ("end" . "{END}")
    ("pageup" . "{PGUP}")
    ("pagedown" . "{PGDN}")
    ("space" . " "))
  "Key names and their SendKeys text.")

(defun harmless-computer-plan-refuses-p (session tool _args)
  "Return a refusal when plan mode blocks TOOL on SESSION.
A screenshot is still allowed.  The caller refuses before
asking for permission."
  (and (harmless-session-plan-mode session)
       (member (harmless-tool-name tool)
               '("computer_click" "computer_type"
                 "computer_key" "browser_open"))
       harmless-computer-plan-refusal))

(defun harmless-computer--plan (session name)
  "Signal when plan mode refuses NAME on SESSION."
  (when (and session
             (harmless-session-plan-mode session)
             (member name '("computer_click" "computer_type"
                            "computer_key" "browser_open")))
    (error "Plan mode is on. Computer control is refused")))

(defun harmless-computer--coord (args key)
  "Return the integer KEY in ARGS, or signal."
  (let ((value (harmless-tool-arg args key))
        (name (substring (symbol-name key) 1)))
    (cond
     ((null value) (error "%s must be an integer" name))
     ((not (integerp value)) (error "%s must be an integer" name))
     ((< value 0) (error "%s must be a non-negative integer" name))
     (t value))))

(defun harmless-computer--button (args)
  "Return the mouse button in ARGS.
A missing button is left."
  (let ((button (harmless-tool-arg args :button)))
    (cond
     ((null button) "left")
     ((not (stringp button))
      (error "button must be left, right, or middle"))
     (t
      (setq button (downcase (string-trim button)))
      (unless (member button '("left" "right" "middle"))
        (error "button must be left, right, or middle"))
      button))))

(defun harmless-computer--text (args)
  "Return the text in ARGS.
\"/\" and \"c:/\" are text, not paths."
  (let ((text (harmless-tool-arg args :text)))
    (cond
     ((null text) (error "text must be a string"))
     ((not (stringp text)) (error "text must be a string"))
     (t
      (when (string-empty-p (string-trim text))
        (error "text is empty"))
      text))))

(defun harmless-computer--sendkeys (text)
  "Return TEXT escaped for SendKeys."
  (let (parts)
    (dolist (ch (string-to-list text))
      (push (cond
             ((eq ch ?\n) "{ENTER}")
             ((eq ch ?\t) "{TAB}")
             ((eq ch ?\r) "")
             ((member ch '(?+ ?^ ?% ?~ ?\( ?\) ?\{ ?\} ?\[ ?\]))
              (format "{%c}" ch))
             (t (char-to-string ch)))
            parts))
    (mapconcat #'identity (nreverse parts) "")))

(defun harmless-computer--key (args)
  "Return the SendKeys text for the key in ARGS."
  (let ((key (harmless-tool-arg args :key)))
    (cond
     ((null key) (error "key is empty"))
     ((not (stringp key)) (error "key must be a string"))
     (t
      (setq key (string-trim key))
      (when (string-empty-p key)
        (error "key is empty"))
      (let ((named (cdr (assoc (downcase key) harmless-computer--keys)))
            (fn (and (string-match-p "\\`[fF][1-9][0-2]?\\'" key)
                     (string-to-number (substring key 1)))))
        (cond
         (named named)
         ((and fn (<= 1 fn 12))
          (format "{F%d}" fn))
         ((and (= (length key) 1)
               (not (member (aref key 0) '(?\n ?\r ?\t))))
          (harmless-computer--sendkeys key))
         (t (error "key is not a key"))))))))

(defun harmless-computer--b64 (text)
  "Return TEXT as unpadded-safe base64 on one line."
  (base64-encode-string
   (encode-coding-string text 'utf-8-unix) t))

(defun harmless-computer-screenshot-file ()
  "Return a new PNG path under the Harmless data directory.
The file is not created.  The filesystem root is not a parent."
  (let* ((dir (expand-file-name "computer" (harmless-data-directory)))
         (stamp (format-time-string "%Y%m%dT%H%M%SZ" nil t))
         (file (expand-file-name (format "shot-%s.png" stamp) dir)))
    (when (or (harmless-filesystem-root-p dir)
              (harmless-filesystem-root-p file)
              (equal file "/shot.png")
              (not (file-name-absolute-p file)))
      (error "Refusing to store a screenshot at %s" file))
    file))

(defun harmless-computer-run-default (script)
  "Run PowerShell SCRIPT and return stdout.
PowerShell is refused before a process starts when it is missing."
  (unless (and (stringp script) (not (string-empty-p script)))
    (error "computer control is not available"))
  (let ((program (executable-find "powershell")))
    (unless program
      (error "computer control is not available"))
    (let ((buf (generate-new-buffer " *harmless-computer*")))
      (unwind-protect
          (let ((status (call-process
                         program nil buf nil
                         "-NoProfile" "-NonInteractive" "-Command" script)))
            (unless (eq status 0)
              (error "computer control failed"))
            (with-current-buffer buf
              (buffer-string)))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(defun harmless-computer--script (body)
  "Return a PowerShell script that stops on errors, then BODY."
  (concat "$ErrorActionPreference = 'Stop'\n" body))

(defun harmless-computer-capture-default ()
  "Capture the primary screen to a PNG under the data directory."
  (let* ((file (harmless-computer-screenshot-file))
         (dir (file-name-directory file)))
    (harmless-ensure-directory dir)
    (let* ((script
            (harmless-computer--script
             (concat
              "Add-Type -AssemblyName System.Windows.Forms\n"
              "Add-Type -AssemblyName System.Drawing\n"
              "$p = [Text.Encoding]::UTF8.GetString("
              "[Convert]::FromBase64String('"
              (harmless-computer--b64 file) "'))\n"
              "$b = [System.Windows.Forms.Screen]::PrimaryScreen.Bounds\n"
              "$bmp = New-Object System.Drawing.Bitmap "
              "$b.Width, $b.Height\n"
              "$g = [System.Drawing.Graphics]::FromImage($bmp)\n"
              "$g.CopyFromScreen($b.Location, "
              "[System.Drawing.Point]::Empty, $b.Size)\n"
              "$bmp.Save($p)\n"
              "$g.Dispose(); $bmp.Dispose()\n"
              "Write-Output ($b.Width.ToString() + ' ' + "
              "$b.Height.ToString())\n")))
           (out (funcall harmless-computer-run-function script))
           (parts (and (stringp out)
                       (split-string (string-trim out) "[ \t\n\r]+" t))))
      (unless (and parts
                   (= (length parts) 2)
                   (string-match-p "\\`[0-9]+\\'" (car parts))
                   (string-match-p "\\`[0-9]+\\'" (cadr parts)))
        (error "computer capture failed"))
      (list (string-to-number (car parts))
            (string-to-number (cadr parts))
            file))))

(defun harmless-computer--click-script (x y button)
  "Return a PowerShell script that clicks BUTTON at X, Y."
  (let ((down (pcase button
                ("right" "0x0008")
                ("middle" "0x0020")
                (_ "0x0002")))
        (up (pcase button
              ("right" "0x0010")
              ("middle" "0x0040")
              (_ "0x0004"))))
    (harmless-computer--script
     (concat
      "Add-Type @'\n"
      "using System;\n"
      "using System.Runtime.InteropServices;\n"
      "public class HarmlessMouse {\n"
      "  [DllImport(\"user32.dll\")]\n"
      "  public static extern bool SetCursorPos(int x, int y);\n"
      "  [DllImport(\"user32.dll\")]\n"
      "  public static extern void mouse_event(uint f, uint x, uint y,"
      " uint d, UIntPtr e);\n"
      "}\n"
      "'@\n"
      (format "[HarmlessMouse]::SetCursorPos(%d, %d)\n" x y)
      (format "[HarmlessMouse]::mouse_event(%s, 0, 0, 0, [UIntPtr]::Zero)\n"
              down)
      (format "[HarmlessMouse]::mouse_event(%s, 0, 0, 0, [UIntPtr]::Zero)\n"
              up)))))

(defun harmless-computer--keys-script (sendkeys)
  "Return a PowerShell script that sends SENDKEYS."
  (harmless-computer--script
   (concat
    "Add-Type -AssemblyName System.Windows.Forms\n"
    "$t = [Text.Encoding]::UTF8.GetString("
    "[Convert]::FromBase64String('"
    (harmless-computer--b64 sendkeys) "'))\n"
    "[System.Windows.Forms.SendKeys]::SendWait($t)\n")))

(defun harmless-computer-act-default (action)
  "Perform ACTION on the desktop."
  (let ((script
         (cond
          ((plist-get action :click)
           (harmless-computer--click-script
            (plist-get action :x)
            (plist-get action :y)
            (plist-get action :button)))
          ((plist-member action :type)
           (harmless-computer--keys-script
            (harmless-computer--sendkeys (plist-get action :type))))
          ((plist-member action :key)
           (harmless-computer--keys-script (plist-get action :key)))
          (t (error "computer control is not available")))))
    (funcall harmless-computer-run-function script)
    nil))

(defun harmless-computer-browse-default (url)
  "Open URL in the desktop browser."
  (harmless-browse-url url)
  url)

(defun harmless-computer-screenshot (session _args)
  "computer_screenshot implementation."
  (ignore session)
  (let ((shot (funcall harmless-computer-capture-function)))
    (unless (and (listp shot)
                 (= (length shot) 3)
                 (integerp (nth 0 shot))
                 (integerp (nth 1 shot))
                 (stringp (nth 2 shot))
                 (harmless-path-absolute-p (nth 2 shot))
                 (not (harmless-filesystem-root-p (nth 2 shot))))
      (error "computer capture failed"))
    (format "%dx%d\n%s" (nth 0 shot) (nth 1 shot) (nth 2 shot))))

(defun harmless-computer-click (session args)
  "computer_click implementation."
  (harmless-computer--plan session "computer_click")
  (let ((x (harmless-computer--coord args :x))
        (y (harmless-computer--coord args :y))
        (button (harmless-computer--button args)))
    (funcall harmless-computer-act-function
             (list :click t :x x :y y :button button))
    (format "Clicked %s at %d, %d" button x y)))

(defun harmless-computer-type (session args)
  "computer_type implementation."
  (harmless-computer--plan session "computer_type")
  (let ((text (harmless-computer--text args)))
    (funcall harmless-computer-act-function (list :type text))
    (format "Typed %d characters" (length text))))

(defun harmless-computer-key (session args)
  "computer_key implementation."
  (harmless-computer--plan session "computer_key")
  (let ((key (harmless-computer--key args)))
    (funcall harmless-computer-act-function (list :key key))
    "Pressed key"))

(defun harmless-computer-browse (session args)
  "browser_open implementation."
  (harmless-computer--plan session "browser_open")
  (let ((url (harmless-web-url (harmless-tool-arg args :url))))
    (funcall harmless-computer-browse-function url)
    (concat "Opened " url)))

(defun harmless-computer-register ()
  "Register the computer and browser tools."
  (harmless-register-tool
   (harmless-tool-create
    :name "computer_screenshot"
    :description "Capture the primary screen. The result is the size and a PNG path under the Harmless data directory. Nothing is written into the project."
    :class 'shell
    :schema '(:type "object")
    :fn #'harmless-computer-screenshot))
  (harmless-register-tool
   (harmless-tool-create
    :name "computer_click"
    :description "Click the desktop at X and Y. BUTTON is left, right, or middle. Nothing is written into the project."
    :class 'shell
    :schema '(:type "object"
              :properties (:x (:type "integer" :description "Horizontal pixel")
                           :y (:type "integer" :description "Vertical pixel")
                           :button (:type "string"
                                    :description "left, right, or middle"))
              :required ["x" "y"])
    :fn #'harmless-computer-click))
  (harmless-register-tool
   (harmless-tool-create
    :name "computer_type"
    :description "Type TEXT into the window that has focus. Nothing is written into the project."
    :class 'shell
    :schema '(:type "object"
              :properties (:text (:type "string"
                                  :description "Characters to type"))
              :required ["text"])
    :fn #'harmless-computer-type))
  (harmless-register-tool
   (harmless-tool-create
    :name "computer_key"
    :description "Press one KEY, such as enter, tab, escape, or a single character. Nothing is written into the project."
    :class 'shell
    :schema '(:type "object"
              :properties (:key (:type "string"
                                 :description "Key name or one character"))
              :required ["key"])
    :fn #'harmless-computer-key))
  (harmless-register-tool
   (harmless-tool-create
    :name "browser_open"
    :description "Open URL in the desktop browser so the user can watch. Only http and https are accepted. Nothing is written into the project."
    :class 'shell
    :schema '(:type "object"
              :properties (:url (:type "string"
                                 :description "http or https URL"))
              :required ["url"])
    :fn #'harmless-computer-browse)))

(harmless-computer-register)

(provide 'harmless-computer)

;;; harmless-computer.el ends here
