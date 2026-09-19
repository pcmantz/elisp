;;; my-eglot.el --- eglot configuration -*- lexical-binding: t -*-

;; Author: Paul C. Mantz
;; Maintainer: Paul C. Mantz

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(use-package eglot
  :hook
  (rust-mode . eglot-ensure)
  (eglot-managed-mode . eldoc-box-hover-mode)
  :config
  ;; rust-analyzer triggers on-type formatting on . = < > { ( | + and eglot
  ;; requests it synchronously, so each one blocks until the server answers.
  (setq eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))

  (add-to-list 'eglot-server-programs
    `(rust-mode . ("rust-analyzer" :initializationOptions
                    ( :procMacro (:enable t)
                      :cargo ( :buildScripts (:enable t)
                               :features "all"))))))

(provide 'my-eglot)

;;; my-eglot.el ends here
