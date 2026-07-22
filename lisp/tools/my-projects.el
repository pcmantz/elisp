;;; my-projects.el --- Project management  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; projectile: for managing projects
(use-package projectile
  :ensure t
  :bind-keymap
  (("C-c p" . projectile-command-map)
   ("s-p" . projectile-command-map))
  :delight " 🎯"
  :config
  (projectile-mode t))

;; mise: load per-project tasks and environment variables
(use-package mise
  :hook (after-init . global-mise-mode))

(provide 'my-projects)
;;; my-projects.el ends here
