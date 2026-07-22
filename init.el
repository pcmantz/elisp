;;; init.el --- config script for elisp packages -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(add-to-list 'load-path (concat user-emacs-directory "lisp/config"))
(add-to-list 'load-path (concat user-emacs-directory "lisp/tools"))
(add-to-list 'load-path (concat user-emacs-directory "lisp/langs"))

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
;; subsequent packages rely on bind-key and delight being available
(elpaca-wait)

;; miscellaneous requirements
(use-package inflections)
(use-package string-inflection)
;;
;; load custom modules
;;

;; config
(require 'my-env)

(require 'my-appearance)
(require 'my-autocompletion)
(require 'my-backups)
(require 'my-bindings)
(require 'my-navigation)

(require 'my-defuns)

;; tools
(require 'my-consult)
(require 'my-minor-modes)
(require 'my-projects)
(require 'my-terminal)
(require 'my-vc)
(require 'my-formatters)
(require 'my-mcp)

(require 'my-org)

;; languages
(require 'my-treesitter)

(require 'my-elixir)
(require 'my-ruby)
(require 'my-rust)

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
