;;; my-helm.el --- Configuration for Helm Autocompletion Framework -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

;; helm - Crazy powerful matching
;; NOTE: Further keybinding present in my-bindings.el
(use-package helm
  :blackout t
  :bind (("M-x" . helm-M-x)
         ("<f13>" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b". helm-mini)
          ("M-y" . helm-show-kill-ring))
  :config
  (helm-mode 1)
  (defadvice helm-find-file (before make-directory-maybe (filename &optional wildcards) activate)
    "Create parent directory if not exists while visiting file."
    (unless (file-exists-p filename)
      (let ((dir (file-name-directory filename)))
        (unless (file-exists-p dir)
          (make-directory dir)))))
  (helm-autoresize-mode 1))

(use-package helm-ag)

(use-package helm-projectile
  :config
  (helm-projectile-on))

(provide 'my-helm)

;;; my-helm.el ends here
