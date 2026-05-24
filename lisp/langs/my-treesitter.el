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

(provide 'my-treesitter)
;;; my-treesitter.el ends here
