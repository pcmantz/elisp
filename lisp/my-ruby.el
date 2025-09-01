;; my-ruby --- Configurations for Ruby  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package ruby-mode
  :after (bind-key)
  :ensure nil
  :delight "♦️"
  :config
  (add-hook 'ruby-base-mode-hook
    (lambda ()
      (bind-key "C-c r" 'pcm/ruby-refactor-transient))))

(use-package ruby-tools
  :delight
  :config
  (add-hook 'ruby-base-mode-hook #'ruby-tools-mode))

(use-package ruby-refactor
  :after (transient ruby-mode)
  :commands pcm/ruby-refactor-transient
  :config
  (transient-define-prefix pcm/ruby-refactor-transient ()
    "My custom Transient menu for Ruby refactoring. Stolen from
https://johnhame.link/posts/tweaking-emacs-for-ruby-development-in-2023/"
    [["Refactor"
       ("e" "Extract Region to Method" ruby-refactor-extract-to-method)
       ("v" "Extract Local Variable" ruby-refactor-extract-local-variable)
       ("l" "Extract to let" ruby-refactor-extract-to-let)
       ("c" "Extract Constant" ruby-refactor-extract-constant)
       ("r" "Rename Local Variable or Method (LSP)" eglot-rename)
       ("{" "Toggle block style" ruby-toggle-block)
       ("'" "Toggle string quotes" ruby-toggle-string-quotes)
       ]
      ["Actions"
        ("d" "Documentation Buffer" eldoc-doc-buffer)
        ("q" "Quit" transient-quit-one)
        ("C" "Run a REPL" inf-ruby-console-auto)
        ("TAB" "Switch to REPL" ruby-switch-to-inf)]]))

(use-package rubocop
  :delight
  :config
  (add-hook 'ruby-base-mode-hook #'rubocop-mode))

(use-package yard-mode
  :delight
  :config
  (add-hook 'ruby-base-mode-hook #'yard-mode))

(use-package inf-ruby
  :config
  (add-hook 'after-init-hook 'inf-ruby-switch-setup)
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter-and-focus)
  (add-hook 'ruby-base-mode 'inf-ruby-minor-mode)
  (inf-ruby-enable-auto-breakpoint))

(use-package projectile-rails
  :init
  (add-hook 'projectile-mode-hook #'projectile-rails))

(use-package haml-mode)
(use-package rails-log-mode)
(use-package slim-mode)

(provide 'my-ruby)

;;; my-ruby.el ends here
