;;; my-elixir.el --- Configuration for Elixir programming -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration for Elixir programming.

;;; Code:

(use-package elixir-mode
  :init
  (add-to-list 'interpreter-mode-alist '("elixir" . elixir-mode)))

(use-package elixir-yasnippets)

(use-package heex-ts-mode)

(provide 'my-elixir)

;;; my-elixir.el ends here
