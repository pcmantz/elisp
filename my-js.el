;; my-js.el


(defalias 'javascript-mode 'js2-mode)
(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'javascript-mode)
       (setcdr pair 'js2-mode)))
 (append auto-mode-alist interpreter-mode-alist))
(add-to-list 'auto-mode-alist '("*\\.js$" . js2-mode))

(setq js2-basic-offset 4)

(defun my-js-defaults ()
  (setq js2-basic-offset 2)
  (define-key js2-mode-map (kbd "C-M-\\") 'web-beautify-js))

(setq my-js-hook 'my-js-defaults)
(add-hook 'js2-mode-hook (lambda () (run-hooks 'my-js-hook)) t)

(provide 'my-js)
;; end my-js.el
