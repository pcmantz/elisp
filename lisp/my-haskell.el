;; my-haskell.el

;; haskell-mode
(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation t)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode t)

  (autoload 'turn-on-haskell-doc-mode "haskell-doc" nil t)
  (add-to-list 'auto-mode-alist '("\\.hs" . haskell-mode)))

(provide 'my-haskell)
;;; my-haskell.el ends here
