;;; my-navigation.el --- Buffer and file navigation  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; uniquify: unique buffer names
(use-package uniquify
  :ensure nil
  :demand t
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-separator "/")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

;; dired
(use-package dired-subtree
  :ensure t
  :bind
  (:map dired-mode-map
        ("i" . dired-subtree-insert)
        (";" . dired-subtree-remove)))

;; buffer listing
(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-default-sorting-mode 'major-mode)
  (ibuffer-always-show-last-buffer t)
  (ibuffer-view-ibuffer t))

(use-package all-the-icons-ibuffer
  :ensure t
  :init
  (add-hook 'ibuffer-mode-hook #'all-the-icons-ibuffer-mode))

(use-package ibuffer-vc
  :ensure t
  :config
  (add-hook 'ibuffer-mode-hook 'ibuffer-vc-set-filter-groups-by-vc-root))

(provide 'my-navigation)
;;; my-navigation.el ends here
