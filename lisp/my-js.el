;;; my-js --- Javascript configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun set-javascript-mode-to-rjsx-mode ()
    "Replace `javascript-mode' with `rjsx-mode' in `auto-mode-alist' and `interpreter-mode-alist'."
    (mapc
     (lambda (pair)
       (if (eq (cdr pair) 'javascript-mode)
           (setcdr pair 'rjsx-mode)))
     (append auto-mode-alist interpreter-mode-alist)))

(set-javascript-mode-to-rjsx-mode)
(defalias 'javascript-mode 'rjsx-mode)

(use-package rjsx-mode
  :mode
  (("\\.jsx?$" . rjsx-mode))
  :custom
  (js2-basic-offset 2)
  :config
  (set-javascript-mode-to-rjsx-mode))

(provide 'my-js)
;;; my-js.el ends here
