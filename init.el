;;; init.el --- config script for elisp packages

;;; Commentary:

(require 'cl)

;;; Code:

;;
;; Graphical stuff I'm never going to use
;;
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;
;; add elisp/{lisp,site} to the load path
;;
(defun add-to-load-path (path)
  "Does exactly what is says."
  (add-to-list 'load-path path))

(defvar elisp-dir
  (file-name-directory (or (buffer-file-name) load-file-name))
  "Location where all the Elisp is held.")

(add-to-list 'load-path (concat elisp-dir "site"))
(add-to-list 'load-path (concat elisp-dir "lisp"))

;;
;; emacs package manager
;;
(require 'package)
(dolist (source
         '(("gnu" . "https://elpa.gnu.org/packages/")
           ("melpa" . "https://melpa.org/packages/")))
  (add-to-list 'package-archives source t))

;; autoload everything
(package-initialize)

(defun read-lines (file)
  "Return a list of lines in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string
     (buffer-string) "\n" t)))

;; retrieve a list of expected packages
(defvar my-packages (mapcar #'intern (read-lines (concat elisp-dir "my-packages"))))

;; check for new packages (package versions)
(unless package-archive-contents
  (package-refresh-contents))

;; install any packages that are not present
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; play it again sam
(package-initialize)

;; global requirements
(require 'use-package)
(require 'diminish)
(require 'bind-key)

;;
;; load custom modules
;;

;; global scope modules
(require 'my-env)
(require 'my-bindings)
(require 'my-defuns)

;; minor modes
(require 'my-minor-modes)

;; major modes
(require 'my-major-modes)
(require 'my-perl)
(require 'my-haskell)
(require 'my-cc)
(require 'my-js)
(require 'my-php)
(require 'my-java)
(require 'my-emacs-lisp)
(require 'my-ruby)
(require 'my-org)
(require 'my-prodigy)

;; site libraries
(require 'beautify)
(require 'xmlformat)

;;
;; startup preferences
;;
(setq inhibit-startup-message t)
(setq initial-scratch-message ";; Scratch Buffer")

(put 'downcase-region 'disabled nil)

(provide 'init)
;;; init.el ends here
(put 'upcase-region 'disabled nil)
