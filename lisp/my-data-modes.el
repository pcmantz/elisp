;;; my-data-modes --- Major modes for data formats -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package yaml-mode
  :ensure nil
  :mode
  (("\\.ya?ml$" . yaml-mode))
  :custom (yaml-indent-offset 2))

(use-package rnc-mode
  :ensure nil
  :custom
  (rnc-indent-level 'tab-width))

(use-package csv-mode
  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode))

(use-package markdown-mode
  :mode  (("\\.md$" . markdown-mode)
          ("\\.markdown$" . markdown-mode)))

(use-package hcl-mode
  :mode (("\\.tf(?:state)" . hcl-mode)
         ("\\.json" . hcl-mode)))

(use-package ansible-vault)
(use-package apache-mode)
(use-package docker-compose-mode)
(use-package dockerfile-mode)
(use-package gradle-mode)
(use-package graphviz-dot-mode)
(use-package groovy-mode)
(use-package pkl-mode)

(provide 'my-data-modes)
;;; my-data-modes ends here
