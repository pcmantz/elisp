;; my-ruby --- Configurations for Ruby

;;; Commentary:

;;; Code:

(use-package ruby-mode
  :blackout "♦️")

(use-package ruby-ts-mode
  :blackout "♦️")

(use-package rubocop
  :blackout
  :hook (ruby-mode . rubocop-mode))

(use-package ruby-tools
  :blackout
  :hook (ruby-mode . ruby-tools-mode))

(use-package robe
  :blackout
  :hook (ruby-mode . robe-mode))

(use-package rspec-mode
  :blackout
  :commands rspec-install-snippets
  :hook (ruby-mode . rspec-mode)
  :config
  (rspec-install-snippets))

(use-package yard-mode
  :blackout
  :hook (ruby-mode . yard-mode))

;; NOTE: This is really useful, but less necessary with direnv mode
(use-package chruby)

(use-package projectile-rails
  :init
  (add-hook 'projectile-mode-hook 'projectile-rails-on))

(defun ruby-hash-arrows-to-keys-region (beg end)
  "Replace symbol-arrow hash syntax with the newer 1.9 Javascript-like syntax."
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char beg)
    (while (re-search-forward "['\"]\\([[:alnum:]_]+\\)['\"]\\s-+=>\\s-+" nil t)
      (replace-match "\\1: " t nil))))

(defun ruby-hash-arrows-to-keys-buffer ()
  "Convert all hash arrows in the buffer to javascript-like keys."
  (interactive)
  (ruby-hash-arrows-to-keys-region (point-min) (point-max)))

(provide 'my-ruby)
;;;  my-ruby.el ends here
