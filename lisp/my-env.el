;; my-env --- Basic environment configuration

;;; Commentary:

;; miscellaneous environment variables and global scope modes

;;; Code:

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
;; path (via exec-path-from-shell)
;;
(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :config
    (progn
      (setq exec-path-from-shell-check-startup-files nil)
      (exec-path-from-shell-initialize))))

;;
;; backups
;;

(setq
 backup-by-copying t ;; don't clobber symlinks
 version-control t   ;; version backup files
 kept-new-versions 10
 kept-old-versions 0
 delete-old-versions t
 vc-make-backup-files t)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat elisp-dir "backups")))))

;;
;; TRAMP
;;
;; NOTE: To avoid recursive loads, we load tramp right here and now
(require 'tramp)

;;
;; display
;;

;; colors! the colors!
(use-package font-lock
  :config
  (progn
     (global-font-lock-mode t)
     (setq font-lock-maximum-decoration t)
     (ansi-color-for-comint-mode-on)))

;; graphical config
(when (display-graphic-p)
  (blink-cursor-mode -1)
  (mouse-wheel-mode t)
  (xterm-mouse-mode t)
  (use-package zenburn-theme
    :config
    (load-theme 'zenburn))
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
(use-package uniquify
  :config
  (progn
    (setq uniquify-buffer-name-style 'forward
          uniquify-separator "/"
          uniquify-after-kill-buffer-p t
          uniquify-ignore-buffers-re "^\\*")))

;; buffer listing
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (progn
    (setq
     ibuffer-default-sorting-mode 'major-mode
     ibuffer-always-show-last-buffer t
     ibuffer-view-ibuffer t)
    (use-package ibuffer-vc
      :config
      (add-hook 'ibuffer-mode-hook 'ibuffer-vc-set-filter-groups-by-vc-root))))

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

;;
;; interaction
;;

(defalias 'yes-or-no-p 'y-or-n-p)

(setq echo-keystrokes 0.1)

;; helm - Crazy powerful matching
;; NOTE: Further keybinding present in my-bindings.el
(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b". helm-mini)
         ("M-y" . helm-show-kill-ring))
  :config
  (progn
    (helm-mode 1)
    (diminish 'helm-mode)
    (defadvice helm-find-file (before make-directory-maybe (filename &optional wildcards) activate)
      "Create parent directory if not exists while visiting file."
      (unless (file-exists-p filename)
        (let ((dir (file-name-directory filename)))
          (unless (file-exists-p dir)
            (make-directory dir)))))
    (helm-autoresize-mode 1)))
(use-package helm-config)

;; projectile: for managing projects
(use-package projectile
  :config
  (progn
    (projectile-global-mode t)))

(use-package helm-projectile
  :config (helm-projectile-on))

;; whitespace configuration
;; TODO: Make individual customizations for major modes
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq whitespace-line-column 120)
(define-key global-map (kbd "C-x W") 'whitespace-mode)

(provide 'my-env)
;;; my-env.el ends here
