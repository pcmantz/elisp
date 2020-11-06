;; my-ts --- Typescript configuration

;;; Commentary:

;;; Code:
(require 'reformatter)

(use-package typescript
  :mode (("\\.ts$" . ts-mode))
  :config
  (progn
    (custom-set-variables
     '(typescript-indent-level 2)
     '(typescript-expr-indent-offset 0))
    (flycheck-add-mode 'javascript-eslint 'typescript-mode)))

(provide 'my-ts)

(setq tslint-path "tslint")

(reformatter-define tslint
  :program tslint-path
  :args '("--fix"))

;;; my-ts.el ends here
