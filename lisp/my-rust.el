;;; my-rust.el --- configurations for rust -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t)
  :custom
  (indent-tabs-mode nil)
  (electric-indent-mode nil)
  (rust-indent-offset 4))

(use-package rustic
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
  (rustic-lsp-client 'eglot))

(provide 'my-rust)

;;; my-rust.el ends here
