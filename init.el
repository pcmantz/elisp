;;; init.el --- config script for elisp packages

;; Copyright (C) 2010 Paul C. Mantz, all rights reserved


;;
;; elisp mode configuration
;;

;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(setq yaml-indent-offset 4)

;; haskell-mode
(require 'haskell-mode)

;; icicles
(require 'icicles)
(icy-mode t)

;; multi-term
(require 'multi-term)
(setq multi-term-program "/bin/bash")

;; perltidy
;; code copied from Emacs::PDE
;; http://search.cpan.org/~yewenbin/Emacs-PDE-0.2.16/
(require 'perltidy)

(defcustom pde-perltidy-prefix "\C-c\C-t"
  "*prefix key for perltidy commands"
  :type 'string
  :group 'pde)

(defvar pde-perltidy-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-r" 'perltidy-region)
    (define-key map "\C-b" 'perltidy-buffer)
    (define-key map "\C-s" 'perltidy-subroutine)
    (define-key map "\C-t" 'perltidy-dwim)
    map)
  "*Keymap for perltidy commands.")
