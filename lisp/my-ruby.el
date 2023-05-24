;; my-ruby --- Configurations for Ruby

;;; Commentary:

;;; Code:

(use-package ruby-mode
  :elpaca nil
  :blackout "♦️")

(use-package ruby-ts-mode
  :elpaca nil
  :blackout "♦️")

;; (use-package rubocop
;;   :blackout
;;   :hook (ruby-mode . rubocop-mode))

(use-package ruby-tools
  :blackout
  :init
  (add-hook 'ruby-ts-mode-hook #'ruby-tools-mode))

(use-package yard-mode
  :blackout
  :init
  (add-hook 'ruby-ts-mode-hook #'yard-mode))

;; NOTE: This is really useful, but less necessary with direnv mode
(use-package chruby)

(use-package projectile-rails
  :init
  (add-hook 'projectile-mode-hook #'projectile-rails))

(use-package haml-mode)
(use-package rails-log-mode)
(use-package slim-mode)

(provide 'my-ruby)
;;;  my-ruby.el ends here
