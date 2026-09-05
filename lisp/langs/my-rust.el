;;; my-rust.el --- configurations for rust -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(use-package rust-mode
  :after (treesit-auto)
  :init
  (setq rust-mode-treesitter-derive t))

(provide 'my-rust)

;;; my-rust.el ends here
