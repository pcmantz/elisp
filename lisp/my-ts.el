;; my-ts --- Typescript configuration

;;; Commentary:

;;; Code:
(use-package typescript
  :mode (("\\.ts$" . ts-mode))
  :config
  (progn
    (custom-set-variables
     '(typescript-indent-level 2)
     '(typescript-expr-indent-offset 0))
    (flycheck-add-mode 'javascript-eslint 'typescript-mode)))

(provide 'my-ts)
;;; my-js.el ends here
