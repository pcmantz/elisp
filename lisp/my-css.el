;;; my-css.el --- Code for editing CSS and related formats -*- lexical-binding: t -*-

;;; Commentary:

;; Code for editing CSS and related formats.

;;; Code:

;; scss-mode
(use-package scss-mode
  :custom
  (css-set-offset 2))

(use-package css-mode
  :elpaca nil
  :custom
  (css-set-offset 2))

(use-package sass-mode)

(provide 'my-css)

;;; my-css.el ends here
