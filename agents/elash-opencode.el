;;; elash-opencode.el --- OpenCode agent configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/elash

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; OpenCode agent configuration for elash.
;;
;;; Code:

(require 'elash)

(defgroup elash-opencode nil
  "OpenCode agent configuration."
  :group 'elash)

(defcustom elash-opencode-command '("opencode" "acp")
  "Command and parameters for the OpenCode ACP client."
  :type '(repeat string)
  :group 'elash-opencode)

;;;###autoload
(defun elash-start-opencode ()
  "Start an OpenCode agent session."
  (interactive)
  (elash-start elash-opencode-command "OpenCode" nil))

;;;###autoload
(defun elash-resume-opencode ()
  "Resume an OpenCode agent session."
  (interactive)
  (elash-resume elash-opencode-command "OpenCode" nil))

(provide 'elash-opencode)
;;; elash-opencode.el ends here
