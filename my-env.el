;; my-env.el

;; miscellaneous environment variables and global scope modes

;; encoding

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; interaction

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'ido)
(eval-after-load 'ido
  '(progn
    (ido-mode t)
    (setq ido-enable-flex-matching t)));; enable fuzzy matching

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(setq x-select-enable-clipboard t
      interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; colors! the colors!

(require 'font-lock)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(ansi-color-for-comint-mode-on)

;; graphical config

(when window-system
  (mouse-wheel-mode t)
  (blink-cursor-mode -1)
  (tooltip-mode -1)
  (xterm-mouse-mode t)
  (set-foreground-color "white")
  (set-background-color "black")
  (set-cursor-color "green")
  (cond ((string= window-system "x") ;; X window system
         (set-face-attribute 'default nil :font "Inconsolata-9"))
        (t  ;; terminal mode
         nil)))

;; frame config

(line-number-mode t)
(column-number-mode t)

;; uniquify: unique buffer names
(require 'uniquify)
(eval-after-load 'uniquify
 '(progn
   (setq uniquify-buffer-name-style 'reverse
         uniquify-separator "|"
         uniquify-after-kill-buffer-p t
         uniquify-ignore-buffers-re "^\\*")))

;; text display config

(show-paren-mode t)
(transient-mark-mode t)

(setq-default fill-column 78
              indent-tabs-mode nil)

(setq-default tab-width 4)

;; buffer listing

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-default-sorting-mode 'major-mode)
(setq ibuffer-always-show-last-buffer t)
(setq ibuffer-view-ibuffer t)

(provide 'my-env)
;; end my-env.el
