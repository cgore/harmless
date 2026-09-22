;;; harmless-transient.el --- Transient menus for Harmless -*- lexical-binding: t; -*-

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
;; Transient prefix for session, model, and permission commands.

;;; Code:

(require 'transient)
(require 'harmless-session)
(require 'harmless-provider)
(require 'harmless-ui)
(require 'harmless-dashboard)
(require 'harmless-memory)

(declare-function harmless-new "harmless")
(declare-function harmless-current "harmless")
(declare-function harmless-switch "harmless")
(declare-function harmless-set-model "harmless")
(declare-function harmless-pick-model "harmless")
(declare-function harmless-set-reasoning-effort "harmless")
(declare-function harmless-set-permission-mode "harmless")
(declare-function harmless-login "harmless-auth")
(declare-function harmless-logout "harmless-auth")

;;;###autoload
(transient-define-prefix harmless-menu ()
  "Harmless commands."
  [["Session"
    ("n" "New" harmless-new)
    ("c" "Current project" harmless-current)
    ("s" "Switch" harmless-switch)
    ("d" "Dashboard" harmless-dashboard)
    ("a" "Abort" harmless-abort)]
   ["Options"
    ("m" "Model" harmless-pick-model)
    ("e" "Effort" harmless-set-reasoning-effort)
    ("p" "Permissions" harmless-set-permission-mode)]
   ["Account"
    ("l" "Log in" harmless-login)
    ("o" "Log out" harmless-logout)]
   ["Memory"
    ("r" "Remember" harmless-remember)
    ("D" "Dream" harmless-dream)]])

(provide 'harmless-transient)

;;; harmless-transient.el ends here
