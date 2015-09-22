;;; init.el --- config script for elisp packages

(require 'cl)

;;
;; Graphical stuff I'm never going to use
;;
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;
;; add elisp/ and its subdirectories to the load path
;;
(setq elisp-dir (file-name-directory (or (buffer-file-name) load-file-name)))

(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir elisp-dir)
           (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)
      (byte-recompile-directory my-lisp-dir 0)))

;;
;; emacs package manager
;;
(require 'package)
(dolist (source
         '(("melpa" . "http://melpa.milkbox.net/packages/")
           ("marmalade" . "http://marmalade-repo.org/packages/")))
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

;;
;; startup preferences
;;
(setq inhibit-startup-message t)
(setq initial-scratch-message ";; Scratch Buffer")

;; end init.el
