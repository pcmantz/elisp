;;;; my-treesitter --- Tree-sitter Configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tree-sitter integration: when font lock isn't enough.

;;; Code:

(use-package treesit
  :ensure nil
  :custom
  ((treesit-font-lock-level 4)
   (treesit-auto-install-grammar 'ask)
   (treesit-enabled-modes t))
  :config
  (defun pcm/treesit-sync-mode-remaps (&rest _)
    "Map registered tree-sitter modes onto `major-mode-remap-alist'.

Built-in `treesit-enabled-modes' only maps the modes that are
registered when it is set, so re-run this after libraries load to
pick up tree-sitter modes defined by third-party packages."
    (when (treesit-available-p)
      (dolist (remap treesit-major-mode-remap-alist)
        (if (or (eq treesit-enabled-modes t)
                (memq (cdr remap) treesit-enabled-modes))
            (add-to-list 'major-mode-remap-alist remap)
          (setq major-mode-remap-alist
                (delete remap major-mode-remap-alist))))))
  (pcm/treesit-sync-mode-remaps)
  (add-hook 'after-load-functions #'pcm/treesit-sync-mode-remaps))

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
