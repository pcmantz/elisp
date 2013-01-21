;;; init.el --- config script for elisp packages

(require 'cl)

;;
;; Graphical stuff I'm never going to use, and no one else shoud either
;;
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;
;; add elisp/ and its subdirectories to the load path
;;
(setq elisp-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path elisp-dir)

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
(package-initialize)

(defvar my-packages

  '(ack-and-a-half apache-mode browse-kill-ring coffee-mode
                   cperl-mode csv-mode escreen gist haml-mode
                   haskell-mode magit markdown-mode melpa
                   monokai-theme multi-term php-mode ruby-tools
                   rvm s sass-mode scss-mode smex yaml-mode
                   workgroups yasnippet zenburn-theme)
  "A list of packages to be installed at launch.")

(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

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
(require 'my-ruby)
(require 'my-org)

;;
;; startup preferences
;;

;; avoid recursive loads by loading tramp right here and now
(require 'tramp)

;; startup stuff
(setq inhibit-startup-message t)
(setq initial-scratch-message ";; Scratch Buffer")

;; end init.el
