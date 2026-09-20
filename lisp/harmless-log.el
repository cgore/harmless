;;; harmless-log.el --- Logging for Harmless -*- lexical-binding: t; -*-

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
;; Writes diagnostic lines to *harmless-log*.  Never pass API keys here.

;;; Code:

(require 'harmless-util)

(defconst harmless-log-buffer-name "*harmless-log*"
  "Name of the Harmless log buffer.")

(defcustom harmless-log-max-lines 2000
  "Maximum number of lines kept in the Harmless log buffer."
  :type 'integer
  :group 'harmless)

(defun harmless-log (fmt &rest args)
  "Append a formatted line to the Harmless log buffer.
FMT and ARGS are like `format'."
  (let ((line (apply #'format fmt args)))
    (with-current-buffer (get-buffer-create harmless-log-buffer-name)
      (goto-char (point-max))
      (insert (format-time-string "%H:%M:%S ") line "\n")
      (when (> (count-lines (point-min) (point-max)) harmless-log-max-lines)
        (goto-char (point-min))
        (forward-line (- (count-lines (point-min) (point-max))
                         harmless-log-max-lines))
        (delete-region (point-min) (point))))))

(provide 'harmless-log)

;;; harmless-log.el ends here
