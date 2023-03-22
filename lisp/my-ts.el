;; my-ts --- Typescript configuration

;;; Commentary:

;;; Code:
(use-package typescript
  :mode (("\\.ts$" . ts-mode))
  :custom
  (typescript-indent-level 2)
  (typescript-expr-indent-offset 0)
  :config
  (flycheck-add-mode 'javascript-eslint 'typescript-mode))

(defvar tslint-path "tslint")

(reformatter-define tslint
  :program tslint-path
  :args '("--fix"))

(provide 'my-ts)
;;; my-ts.el ends here
