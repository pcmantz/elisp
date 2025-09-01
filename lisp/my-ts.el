;; my-ts --- Typescript configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package typescript-ts-mode
  :ensure nil)

(defvar tslint-path "tslint")

(reformatter-define tslint
  :program tslint-path
  :args '("--fix"))

(provide 'my-ts)
;;; my-ts.el ends here
