;; my-js --- Javascript configuration

;;; Commentary:

;;; Code:

(use-package js2-mode
  :mode (("*\\.js$" . js2-mode))
  :config
  (progn
    (defun my-js-defaults ()
      (setq js2-basic-offset 2)))
  :config
  (progn
    (defalias 'javascript-mode 'js2-mode)
    (mapc
     (lambda (pair)
       (if (eq (cdr pair) 'javascript-mode)
           (setcdr pair 'js2-mode)))
     (append auto-mode-alist interpreter-mode-alist))
    (defvar my-js-hook 'my-js-defaults)
    (add-hook 'js2-mode-hook (lambda () (run-hooks 'my-js-hook)) t)))

(use-package web-beautify
  :config
  (define-key js2-mode-map (kbd "C-M-\\") 'web-beautify-js))

(provide 'my-js)
;;; my-js.el ends here
