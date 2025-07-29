;;; init.el --- config script for elisp packages -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(add-to-list 'load-path (concat user-emacs-directory "site"))
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; use elpaca for package management
(require 'my-elpaca)
(elpaca-wait)

;;
;; global requirements
;;
(use-package cl-lib :ensure nil :demand t)
(elpaca-wait)

(use-package dash :demand t)
(use-package f :demand t)
(use-package s :demand t)

(use-package bind-key :demand t)
(elpaca-wait)

(use-package delight :demand t)
(elpaca-wait)

;; miscellaneous requirements
(use-package inflections)
(use-package reformatter)
(use-package string-inflection)

(elpaca-wait)

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

;; minor modes
(require 'my-minor-modes)

;; major modes
(require 'my-major-modes)

(elpaca-wait)

(require 'my-cc)
(require 'my-clojure)
(require 'my-css)
(require 'my-elixir)
(require 'my-emacs-lisp)
(require 'my-haskell)
(require 'my-java)
(require 'my-js)
(require 'my-perl)
(require 'my-prodigy)
(require 'my-ruby)
(require 'my-sql)
(require 'my-tex)

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
