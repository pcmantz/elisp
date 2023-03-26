;; my-env --- Basic environment configuration

;;; Commentary:

;; miscellaneous environment variables and global scope modes

;;; Code:

(use-package emacs
  :elpaca nil
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  :custom
  (echo-keystrokes 0.1)
  (enable-recursive-minibuffers t)
  (fill-column 100)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (tab-width 4)
  (visible-bell t))

(use-package simple
  :elpaca nil
  :bind
  ("M-z" . zap-to-char)
  :custom
  (column-number-mode t)
  (line-number-mode t)
  (transient-mark-mode t)
  (indent-tabs-mode nil))

;; Graphical stuff I'm never going to use. may be obsolete due to early-init.el
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; encoding
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

;; path (via exec-path-from-shell)
(use-package exec-path-from-shell
  :if
  (memq window-system '(mac ns))
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

;;
;; backups
;;

(use-package files
  :elpaca nil
  :custom
  (backup-by-copying t) ;; don't clobber symlinks
  (version-control t)   ;; version backup files
  (kept-new-versions 6)
  (kept-old-versions 2)
  (delete-old-versions t)
  (backup-directory-alist ;; Write backup files to own directory
   `(("." . ,(expand-file-name (concat elisp-dir "backups"))))))

(use-package vc-hooks
  :elpaca nil
  :custom
  (vc-make-backup-files t))

;;
;; TRAMP
;;
 ;; NOTE: To avoid recursive loads, we load tramp right here and now
(use-package tramp :elpaca nil :demand t)

;;
;; display
;;

;; colors! the colors!
(use-package font-lock
  :demand t
  :elpaca nil
  :custom
  (font-lock-maximum-decoration t)
  :config
  (global-font-lock-mode t)
  (ansi-color-for-comint-mode-on))

;; tree-sitter: when font lock isn't enough.
(use-package treesit
  :elpaca nil
  :custom
  ((treesit-font-lock-level 4)))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (--map (add-to-list 'treesit-language-source-alist it)
         '('(typescript   . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src" nil nil))
           '(tsx          . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src" nil nil))
           '(elixir       . ("https://github.com/elixir-lang/tree-sitter-elixir" nil nil nil nil))
           '(heex-ts-mode . ("https://github.com/phoenixframework/tree-sitter-heex" nil nil nil nil))
           '(ruby         . ("https://github.com/tree-sitter/tree-sitter-ruby" nil nil nil nil))
           '(scss         . ("https://github.com/serenadeai/tree-sitter-scss" nil nil nil nil))
           '(yaml         . ("https://github.com/ikatyang/tree-sitter-yaml" nil nil nil nil))))

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

(use-package metalheart-theme
  :if (display-graphic-p)
  :config
  (load-theme 'metalheart)
  (set-cursor-color "Purple"))

(when (display-graphic-p)
  (blink-cursor-mode -1)
  (mouse-wheel-mode t)
  (xterm-mouse-mode t)
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
  :demand t
  :defer 0.5)

(use-package all-the-icons-dired
  :blackout t
  :hook (dired-mode . all-the-icons-dired-mode))

;; uniquify: unique buffer names
(use-package uniquify
  :elpaca nil
  :demand t
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-separator "/")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

;; dired
(use-package dired-subtree
  :bind
  (:map dired-mode-map
        ("i" . dired-subtree-insert)
        (";" . dired-subtree-remove)))

;; buffer listing
(use-package ibuffer
  :elpaca nil
  :bind ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-default-sorting-mode 'major-mode)
  (ibuffer-always-show-last-buffer t)
  (ibuffer-view-ibuffer t))

(use-package all-the-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package ibuffer-vc
  :config
  (add-hook 'ibuffer-mode-hook 'ibuffer-vc-set-filter-groups-by-vc-root))

;; text display config
(use-package paren
  :elpaca nil
  :config
  (show-paren-mode t))

;;
;; interaction
;;

(defalias 'yes-or-no-p 'y-or-n-p)

;; projectile: for managing projects
(use-package projectile
  :bind-keymap
  (("C-c p" . projectile-command-map)
   ("s-p" . projectile-command-map))
  :custom
  (projectile-dynamic-mode-line t)
  (projectile-mode-line-prefix " 🎯")
  :init
  (projectile-mode t)
  :config
  (defun projectile-multi-term-in-root ()
    "Invoke `multi-term' in the project's root."
    (interactive)
    (projectile-with-default-dir (projectile-project-root) (multi-term)))

  (define-key projectile-command-map (kbd "x m") 'projectile-multi-term-in-root))


;; whitespace configuration
;; TODO: Make individual customizations for major modes
(use-package whitespace
  :elpaca nil
  :demand t
  :bind ("C-x W" . 'whitespace-mode)
  :custom
  ((whitespace-line-column 100))
  :config
  (add-hook 'before-save-hook 'whitespace-cleanup))

(use-package editorconfig
  :blackout t
  :config
  (editorconfig-mode 1))

(use-package keyfreq
  :demand t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(provide 'my-env)
;;; my-env.el ends here
