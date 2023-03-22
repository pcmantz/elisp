;;; init.el --- config script for elisp packages

;;; Commentary:

;;; Code:

;; Set up local package directories
(defvar elisp-dir
  (file-name-directory (or (buffer-file-name) load-file-name))
  "Location where all the Elisp is held.")

(add-to-list 'load-path (concat elisp-dir "site"))
(add-to-list 'load-path (concat elisp-dir "lisp"))

;; use elpaca for package management
(require 'my-elpaca)

;; global requirements
(use-package cl-lib :elpaca nil :demand t)
(use-package blackout :demand t)
(use-package dash :demand t)
(use-package s :demand t)
(use-package bind-key :demand t)
(use-package f :demand t)

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
(require 'my-helm)

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
(require 'my-php)
(require 'my-prodigy)
(require 'my-ruby)
(require 'my-sql)

;; site libraries
(require 'beautify)
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
