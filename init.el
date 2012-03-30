;;; init.el --- config script for elisp packages

;;
;; Graphical stuff I'm never going to use, and no one else shoud either
;;

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;
;; add elisp/ and its subdirectories to the load path
;;

(setq elisp-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

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
(dolist (source '(("technomancy" . "http://repo.technomancy.us/emacs/")
                  ("elpa" . "http://tromey.com/elpa/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")))
  (add-to-list 'package-archives source t))
(package-initialize)

;;
;; load custom modules
;;

;; global scope modules
(require 'my-env)
(require 'my-bindings)
(require 'my-defuns)

(require 'my-remember)

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

(setq inhibit-startup-message t)
(setq initial-scratch-message ";; Scratch Buffer")

;; end init.el
