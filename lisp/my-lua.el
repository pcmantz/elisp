;; my-lua.el

(defun my-lua-defaults ()
  (setq lua-indent-level tab-width))
(setq my-lua-hook 'my-lua-defaults)

(add-hook 'lua-mode-hook (lambda () (run-hooks 'my-lua-hook)) t)

(provide 'my-lua)
;; end my-lua.el
