;; my-ts --- Typescript configuration

;;; Commentary:

;;; Code:

(use-package typescript-ts-mode
  :elpaca nil)

(defvar tslint-path "tslint")

(reformatter-define tslint
  :program tslint-path
  :args '("--fix"))

(provide 'my-ts)
;;; my-ts.el ends here
