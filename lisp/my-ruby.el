;; my-ruby --- Configurations for Ruby

;;; Commentary:

;;; Code:

(use-package ruby-mode
  :elpaca nil
  :blackout "♦️")

(use-package ruby-ts-mode
  :elpaca nil
  :blackout "♦️")

(use-package rubocop
  :blackout
  :hook (ruby-mode . rubocop-mode))

(use-package ruby-tools
  :blackout
  :hook (ruby-mode . ruby-tools-mode))

(use-package yard-mode
  :blackout
  :hook ruby-mode)

;; NOTE: This is really useful, but less necessary with direnv mode
(use-package chruby)

(use-package projectile-rails
  :hook projectile-mode)

(use-package haml-mode)
(use-package rails-log-mode)
(use-package slim-mode)

(provide 'my-ruby)
;;;  my-ruby.el ends here
