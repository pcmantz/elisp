;; my-haskell.el

;; haskell-mode
(require 'haskell-mode)
(require 'inf-haskell)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

(autoload 'turn-on-haskell-doc-mode "haskell-doc" nil t)
(add-to-list 'auto-mode-alist '("\\.hs" . haskell-mode))

(provide 'my-haskell)
;; end my-haskell.el
