;; my-js --- Javascript configuration

;;; Commentary:

;;; Code:

(defun set-javascript-mode-to-js2-mode ()
    "Replace `javascript-mode' with `js2-mode' in `auto-mode-alist' and `interpreter-mode-alist'."
    (mapc
     (lambda (pair)
       (if (eq (cdr pair) 'javascript-mode)
           (setcdr pair 'js2-mode)))
     (append auto-mode-alist interpreter-mode-alist)))

(set-javascript-mode-to-js2-mode)
(defalias 'javascript-mode 'js2-mode)

(use-package js2-mode
  :mode (("*\\.js$" . js2-mode))
  :config
  (progn
    (defun my-js-defaults ()
      (setq js2-basic-offset 2)))
  :config
  (progn
    (set-javascript-mode-to-js2-mode)
    (defvar my-js-hook 'my-js-defaults)
    (add-hook 'js2-mode-hook (lambda () (run-hooks 'my-js-hook)) t)))


(use-package web-beautify
  :bind
  (:map js2-mode-map
   ( "C-M-\\" . web-beautify-js)))

(provide 'my-js)
;;; my-js.el ends here
