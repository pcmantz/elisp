;;; my-javascript.el --- configurations for (Java/Type)Script -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(use-package add-node-modules-path
  :hook ((js-ts-mode typescript-ts-mode tsx-ts-mode) . add-node-modules-path))

(provide 'my-javascript)

;;; my-javascript.el ends here
