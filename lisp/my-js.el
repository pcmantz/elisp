;; my-js --- Javascript configuration

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
  :mode (("\\.jsx?$" . rjsx-mode))
  :config
  (progn
    (set-javascript-mode-to-rjsx-mode)
    (setq js2-basic-offset 2)))

(provide 'my-js)
;;; my-js.el ends here
