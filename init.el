;;; init.el --- config script for elisp packages -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(add-to-list 'load-path (concat user-emacs-directory "lisp/config"))
(add-to-list 'load-path (concat user-emacs-directory "lisp/tools"))
(add-to-list 'load-path (concat user-emacs-directory "lisp/langs"))
(add-to-list 'load-path (concat user-emacs-directory "lisp/org"))
(add-to-list 'load-path (concat user-emacs-directory "site"))

;; use elpaca for package management
(require 'my-elpaca)
(elpaca-wait)

;;
;; global requirements
;;
(use-package cl-lib :ensure nil :demand t)

(use-package dash :demand t)
(use-package f :demand t)
(use-package s :demand t)

(use-package bind-key :demand t)
(use-package delight :demand t)
(elpaca-wait)

;; miscellaneous requirements
(use-package inflections)
(use-package string-inflection)
;;
;; load custom modules
;;

;; global scope modules
(require 'my-defuns)
(require 'my-env)
(require 'my-bindings)
(require 'my-autocompletion)

;; org mode needs to be loaded early
(require 'my-org)
(elpaca-wait)

(require 'my-minor-modes)

;; tools & formatters needed by language modules
(require 'my-formatters)
(require 'my-prodigy)
(require 'my-terminal)
(require 'my-vc)
(require 'my-mcp)
(elpaca-wait)

;; language modes
(require 'my-emacs-lisp)
(require 'my-cc)
(require 'my-clojure)
(require 'my-css)
(require 'my-elixir)
(require 'my-haskell)
(require 'my-java)
(require 'my-js)
(require 'my-perl)
(require 'my-ruby)
(require 'my-rust)
(require 'my-sql)
(require 'my-tex)
(require 'my-ts)
(require 'my-misc-data-modes)
(require 'my-misc-major-modes)

(elpaca-wait)

;; site libraries
(require 'xmlformat)

;;
;; startup preferences
;;
(setq inhibit-startup-message t)
(setq initial-scratch-message ";; Scratch Buffer")

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(provide 'init)
;;; init.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
