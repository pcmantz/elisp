;;; init.el --- config script for elisp packages

;;; Commentary:

(require 'cl-lib)
(require 'package)
;;; Code:

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
(dolist (source
         '(("gnu" . "https://elpa.gnu.org/packages/")
           ("melpa" . "https://melpa.org/packages/")))
  (add-to-list 'package-archives source t))

;; autoload everything
(package-initialize)

;; This is only needed once, near the top of the file
(eval-when-compile
  (if (not (package-installed-p 'use-package))
    (package-install 'use-package))
  (require 'use-package))

(use-package dash)
(use-package s)

(defun read-lines (file)
  "Return a list of lines in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string
     (buffer-string) "\n" t)))

(defun write-list-lines (list file)
  "Writes all LIST elements one-per-line to FILE."
  (with-temp-buffer
    (insert (s-join "\n" (-map 'symbol-name list)))
    (insert "\n")
    (write-file file)))

;; check for new packages (package versions)
(unless package-archive-contents
  (package-refresh-contents))

;; Retrieve a list of packages for installation
(defvar my-packages-file (concat elisp-dir "my-packages"))

(defvar my-packages (mapcar #'intern (read-lines my-packages-file)))
(setq package-installed-packages
  (-distinct (-concat (-sort 'string< my-packages) (-sort 'string< package-selected-packages))))

;; install any packages that are not present
(package-install-selected-packages)

;; play it again sam
(package-initialize)

;; Write all the installed files
(write-list-lines package-installed-packages my-packages-file)

;; global requirements
(use-package diminish)
(use-package bind-key)

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
