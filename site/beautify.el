;;; beautify.el --- Use the https://beautifier.io/ formatter -*- lexical-binding: t; -*-

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

;; Uses the https://beautifier.io/ formatters to format code.  Using these
;; tools from the command line instead of built-in to Emacs allows for more
;; editor-agnostic configuration.

;;; Code:

(require 'reformatter)

;;;###autoload (autoload 'beautify-js-buffer "beautify-js" nil t)
;;;###autoload (autoload 'beautify-js-region "beautify-js" nil t)
;;;###autoload (autoload 'beautify-js-on-save-mode "beautify-js" nil t)
(reformatter-define beautify-js
  :program "js-beautify"
  :args '("-")
  :lighter " JSFmt"
  :group 'beautify-js)

;;;###autoload (autoload 'beautify-css-buffer "beautify-css" nil t)
;;;###autoload (autoload 'beautify-css-region "beautify-css" nil t)
;;;###autoload (autoload 'beautify-css-on-save-mode "beautify-css" nil t)
(reformatter-define beautify-css
  :program "css-beautify"
  :args '("-")
  :lighter " CSSFmt"
  :group 'beautify-css)

;;;###autoload (autoload 'beautify-html-buffer "beautify-html" nil t)
;;;###autoload (autoload 'beautify-html-region "beautify-html" nil t)
;;;###autoload (autoload 'beautify-html-on-save-mode "beautify-html" nil t)
(reformatter-define beautify-html
  :program "html-beautify"
  :args '("-")
  :lighter " HTMLFmt"
  :group 'beautify-html)

(provide 'beautify)
;;; beautify.el ends here
