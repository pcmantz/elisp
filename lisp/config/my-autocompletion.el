;;; my-autocompletion.el --- Autocompletion config -*- lexical-binding: t -*-

;;; Commentary:

;; Autcompletion configuration.

;;; Code:

(use-package vertico
  :demand t
  :init
  (vertico-mode)
  :custom
  (vertico-resize t))

(use-package savehist
  :ensure nil
  :demand t
  :config
  (savehist-mode))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :demand t
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind
  (("M-A" . marginalia-cycle)
    :map minibuffer-local-map
    ("M-A" . marginalia-cycle))
  :config
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
  (marginalia-mode))

(use-package all-the-icons-completion
  :demand t
  :ensure
  (:host github :repo "iyefrat/all-the-icons-completion")
  :config
  (all-the-icons-completion-mode))

(use-package corfu
  :demand t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.5)
  (corfu-auto-prefix 2))

(use-package nerd-icons-corfu
  :after nerd-icons
  :config
  (nerd-icons-corfu-mode))

(use-package cape
  :demand t
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package symbol-overlay
  :demand t
  :config
  (add-hook 'prog-mode-hook #'symbol-overlay-mode))

(provide 'my-autocompletion)

;;; my-autocompletion.el ends here
