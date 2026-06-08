;;;; my-treesitter --- Tree-sitter Configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tree-sitter integration: when font lock isn't enough.

;;; Code:

(use-package treesit
  :ensure nil
  :custom
  ((treesit-font-lock-level 4)))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package treesit-fold
  :ensure t
  :bind
  ("C-c f" . treesit-fold-toggle)
  :custom
  (treesit-fold-line-count-show t)
  (treesit-fold-line-count-format " ▼ %d lines")
  :config
  (set-face-attribute 'treesit-fold-replacement-face nil
                      :foreground "#808080"
                      :box nil
                      :weight 'bold)
  (setq treesit-fold-indicators-fringe 'right-fringe)
  (global-treesit-fold-indicators-mode)
  (global-treesit-fold-mode))

(provide 'my-treesitter)
;;; my-treesitter.el ends here
