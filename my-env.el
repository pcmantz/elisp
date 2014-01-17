;; my-env.el

;; miscellaneous environment variables and global scope modes

;;
;; encoding
;;

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;
;; configuration
;;

;; keep local customizations out of this file
(setq custom-file (concat elisp-dir "my-custom-file.el"))
(load custom-file)

;;
;; path
;;

;; quick and dirty hack to pull my shell environment from my bash config.  I
;; suppose if I ever switch to zsh I'll have problems
(if (not (getenv "TERM_PROGRAM"))
    (setenv "PATH"
            (shell-command-to-string "source $HOME/.bashrc && printf $PATH")))

;;
;; interaction
;;

(defalias 'yes-or-no-p 'y-or-n-p)

;; ido for files
(require 'ido)
(eval-after-load 'ido
  '(progn
    (ido-mode t)
    (setq
     ido-everywhere t
     ido-enable-flex-matching t)));; enable fuzzy matching

;; smex for menus
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; projectile: for managing projects
(require 'projectile)
(eval-after-load 'projectile
  '(progn
     (projectile-global-mode t))

;; TODO: Fill in these functions
;(define-key projectile-mode-map (kbd "C-b") 'projectile-ibuffer)

;;
;; backups
;;

(setq
 version-control t   ;; version backup files
 backup-by-copying t ;; don't clobber symlinks
 delete-old-versions t
 kept-new-versions 4
 kept-old-versions 2
 vc-make-backup-files t)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat elisp-dir "backups")))))

;;
;; TRAMP
;;
;; NOTE: To avoid recursive loads, we load tramp right here and now
;;
(setq tramp-syntax 'url)
(require 'tramp)


;;
;; display
;;

;; colors! the colors!
(require 'font-lock)
(eval-after-load 'font-lock
  '(progn
     (global-font-lock-mode t)
     (setq font-lock-maximum-decoration t)
     (ansi-color-for-comint-mode-on)))

;; graphical config
(when (display-graphic-p)
  (tooltip-mode -1)
  (blink-cursor-mode -1)
  (mouse-wheel-mode t)
  (xterm-mouse-mode t)
  (set-foreground-color "white")
  (set-background-color "black")
  (set-cursor-color "green")
  (cond ((string= window-system "x")   ;; X11 window system
         (set-face-attribute 'default nil :font "Inconsolata-9")
         (setq
          x-select-enable-clipboard t
          interprogram-paste-function 'x-cut-buffer-or-selection-value))
        ((string= window-system "ns")  ;; Apple OS X
         (set-face-attribute 'default nil :font "Inconsolata-11"))
        ((string= window-system "w32") ;; MS-Windows
         (set-face-attribute 'default nil :font "Consolas-9"))
        (t nil)))

;; uniquify: unique buffer names
(require 'uniquify)
(eval-after-load 'uniquify
 '(progn
   (setq uniquify-buffer-name-style 'reverse
         uniquify-separator "|"
         uniquify-after-kill-buffer-p t
         uniquify-ignore-buffers-re "^\\*")))

;; buffer listing
(require 'ibuffer)
(eval-after-load 'ibuffer
    '(progn
       (global-set-key (kbd "C-x C-b") 'ibuffer)
       (setq
        ibuffer-default-sorting-mode 'major-mode
        ibuffer-always-show-last-buffer t
        ibuffer-view-ibuffer t)))

;; frame config
(line-number-mode t)
(column-number-mode t)

;; text display config
(show-paren-mode t)
(transient-mark-mode)
(setq-default
 fill-column 78
 tab-width 4
 indent-tabs-mode nil)

(provide 'my-env)
;; end my-env.el
