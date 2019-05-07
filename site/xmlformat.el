;;; xmlformat.el --- Reformat XML using xmllint -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Paul C. Mantz

;; Author: Paul C. Mantz <pcmantz@mcpantz.org>

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Uses reformatter to provide XML formatting commands.

;;; Code:

(require 'reformatter)

;;;###autoload (autoload 'xmlformat-buffer "xmlformat" nil t)
;;;###autoload (autoload 'xmlformat-region "xmlformat" nil t)
;;;###autoload (autoload 'xmlformat-on-save-mode "xmlformat" nil t)
(reformatter-define xmlformat
  :program "xmllint"
  :args (list "--format" "-")
  :lighter " XMLFmt"
  :group 'xmlformat)

(provide 'xmlformat)
;;; xmlformat.el ends here
