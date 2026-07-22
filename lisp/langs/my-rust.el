;;; my-rust.el --- configurations for rust -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(use-package rust-mode
  :after (treesit-auto)
  :init
  (setq rust-mode-treesitter-derive t)
  :hook
  (rust-mode-hook . (lambda ()
                      (setq-local indent-tabs-mode nil)
                      (setq-local electric-indent-mode nil)))
  :custom
  (rust-indent-offset 4))

(use-package rustic
  :after (rust-mode)
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
  (rustic-lsp-client 'eglot))

(provide 'my-rust)

;;; my-rust.el ends here
