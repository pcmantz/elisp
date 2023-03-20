;; my-env --- Basic environment configuration

;;; Commentary:

;; miscellaneous environment variables and global scope modes

;;; Code:

(require 'dash)

;;
;; Graphical stuff I'm never going to use
;;
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

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
(if (not (file-exists-p custom-file))
    (write-region "" nil custom-file))
(load custom-file)

;;
;; path (via exec-path-from-shell)
;;
(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :custom
    (exec-path-from-shell-check-startup-files nil)
    :config
    (exec-path-from-shell-initialize)))

;;
;; backups
;;

(setq
 backup-by-copying t ;; don't clobber symlinks
 version-control t   ;; version backup files
 kept-new-versions 6
 kept-old-versions 2
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
(use-package tramp
  :demand t)

;;
;; display
;;

;; colors! the colors!
(use-package font-lock
  :demand t
  :custom
  (font-lock-maximum-decoration t)
  :config
  (global-font-lock-mode t)
  (ansi-color-for-comint-mode-on))

;; tree-sitter: when font lock isn't enough.

(use-package treesit
  :custom
  ((treesit-font-lock-level 4)))

(use-package treesit-auto
  :config
  (--map (add-to-list 'treesit-language-source-alist it)
    '('(typescript   . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src" nil nil))
      '(tsx          . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src" nil nil))
      '(elixir       . ("https://github.com/elixir-lang/tree-sitter-elixir" nil nil nil nil))
      '(heex-ts-mode . ("https://github.com/phoenixframework/tree-sitter-heex" nil nil nil nil))
      '(ruby         . ("https://github.com/tree-sitter/tree-sitter-ruby" nil nil nil nil))
      '(scss         . ("https://github.com/serenadeai/tree-sitter-scss" nil nil nil nil))))
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;; graphical config

(defun enable-fira-code-ligatures ()
  "This function enables the ligatures found in the Fira Code font."
  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
         (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
         (36 . ".\\(?:>\\)")
         (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
         (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
         (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
         (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
         (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
         (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
         (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
         (48 . ".\\(?:x[a-zA-Z]\\)")
         (58 . ".\\(?:::\\|[:=]\\)")
         (59 . ".\\(?:;;\\|;\\)")
         (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
         (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
         (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
         (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
         (91 . ".\\(?:]\\)")
         (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
         (94 . ".\\(?:=\\)")
         (119 . ".\\(?:ww\\)")
         (123 . ".\\(?:-\\)")
         (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
         (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)"))))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                `([,(cdr char-regexp) 0 font-shape-gstring])))))

(when (display-graphic-p)
  (blink-cursor-mode -1)
  (mouse-wheel-mode t)
  (xterm-mouse-mode t)
  (use-package metalheart-theme
    :config
    (load-theme 'metalheart)
    (set-cursor-color "Purple"))
  (cond ((string= window-system "x")   ;; X11 window system
         (set-face-attribute 'default nil :font "Inconsolata-9")
         (setq select-enable-clipboard t
               interprogram-paste-function 'x-cut-buffer-or-selection-value))
        ((string= window-system "ns")  ;; Apple OS X
         (set-face-attribute 'default nil :font "Inconsolata-11"))
        ((string= window-system "w32") ;; MS-Windows
         (set-face-attribute 'default nil :font "Consolas-9"))
        (t nil)))

;; all-the-icons
(use-package all-the-icons
  :defer 0.5)

(use-package all-the-icons-dired
  :blackout
  :hook (dired-mode . all-the-icons-dired-mode))

;; uniquify: unique buffer names
(use-package uniquify
  :demand t
  :config
  (progn
    (setq uniquify-buffer-name-style 'forward
          uniquify-separator "/"
          uniquify-after-kill-buffer-p t
          uniquify-ignore-buffers-re "^\\*")))


;; dired
(use-package dired-subtree
  :bind
  (:map dired-mode-map
    ("i" . dired-subtree-insert)
    (";" . dired-subtree-remove)))

;; buffer listing
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-default-sorting-mode 'major-mode)
  (ibuffer-always-show-last-buffer t)
  (ibuffer-view-ibuffer t)
  :config
  (use-package ibuffer-vc
    :config
    (add-hook 'ibuffer-mode-hook 'ibuffer-vc-set-filter-groups-by-vc-root)))

;; frame config
(line-number-mode t)
(column-number-mode t)

;; text display config
(show-paren-mode t)
(transient-mark-mode)
(setq-default fill-column 100
              tab-width 4
              indent-tabs-mode nil)

;; audible bells are horrible
(setq visible-bell t)

;;
;; interaction
;;

(defalias 'yes-or-no-p 'y-or-n-p)

(setq echo-keystrokes 0.1)

;; helm - Crazy powerful matching
;; NOTE: Further keybinding present in my-bindings.el
(use-package helm
  :bind (("M-x" . helm-M-x)
         ("<f13>" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b". helm-mini)
          ("M-y" . helm-show-kill-ring))
  :blackout
  :config
  (helm-mode 1)
  (defadvice helm-find-file (before make-directory-maybe (filename &optional wildcards) activate)
    "Create parent directory if not exists while visiting file."
    (unless (file-exists-p filename)
      (let ((dir (file-name-directory filename)))
        (unless (file-exists-p dir)
          (make-directory dir)))))
  (helm-autoresize-mode 1))

(use-package helm-config)

;; projectile: for managing projects
(use-package projectile
  :blackout " ☄️"
  :bind-keymap
  (("C-c p" . projectile-command-map)
   ("s-p" . projectile-command-map))
  :custom
    ;; HACK: latest implementation of projectile-mode-line is horribly un-performant and runs after
    ;; each keypress, hammering the CPU
    (projectile-mode-line . '(:eval (format " Pj<%s>" (projectile-project-name))))
  :config
  (defun projectile-multi-term-in-root ()
    "Invoke `multi-term' in the project's root."
    (interactive)
    (projectile-with-default-dir (projectile-project-root) (multi-term)))

    (define-key projectile-command-map (kbd "x m") 'projectile-multi-term-in-root)
  (projectile-mode t))

(use-package helm-projectile
  :config
  (helm-projectile-on))

;; whitespace configuration
;; TODO: Make individual customizations for major modes
(use-package whitespace
  :demand t
  :bind ("C-x W" . 'whitespace-mode)
  :custom
  ((whitespace-line-column 100))
  :config
  (add-hook 'before-save-hook 'whitespace-cleanup))

(use-package editorconfig
  :blackout
  :config
  (editorconfig-mode 1))

(use-package keyfreq
  :demand t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(provide 'my-env)
;;; my-env.el ends here
