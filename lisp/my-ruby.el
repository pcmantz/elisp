;; my-ruby --- Configurations for Ruby

;;; Commentary:

;;; Code:

(defun set-ruby-mode-to-enh-ruby-mode ()
    "Replace `ruby-mode' with `enh-ruby-mode' in `auto-mode-alist' and `interpreter-mode-alist'."
    (mapc
     (lambda (pair)
       (if (eq (cdr pair) 'ruby-mode)
           (setcdr pair 'enh-ruby-mode)))
     (append auto-mode-alist interpreter-mode-alist)))

;; enh-ruby-mode
(use-package enh-ruby-mode
  :commands enh-ruby-mode
  :mode
  ;; (rx (and (or "Gem" "Rake" "Cap" "Vagrant") "file" (opt ".lock")))
  (("\\(?:Cap\\|Gem\\|Rake\\|Vagrant\\)file\\(?:\\.lock\\)?" . enh-ruby-mode)
   ("\\.rake$" . enh-ruby-mode)
   ("\\.rabl$" . enh-ruby-mode))
  :init
  (progn
    (defalias 'ruby-mode 'enh-ruby-mode)
    (set-ruby-mode-to-enh-ruby-mode))
  :config
  (progn
    (setq enh-ruby-deep-indent-paren nil)))

(use-package rubocop
  :diminish rubocop-mode
  :hook (enh-ruby-mode . rubocop-mode))
(use-package ruby-tools
  :diminish ruby-tools-mode
  :hook (enh-ruby-mode . ruby-tools-mode))
(use-package robe
  :diminish robe-mode
  :hook (enh-ruby-mode . robe-mode))
(use-package rspec-mode
  :diminish rspec-mode
  :hook (enh-ruby-mode . rspec-mode)
  :config
  (progn
    (rspec-install-snippets)))
(use-package yard-mode
  :diminish yard-mode
  :hook (enh-ruby-mode . yard-mode))


;; NOTE: This is really useful, but less necessary with direnv mode
(use-package chruby)

(use-package projectile-rails
  :init
  (add-hook 'projectile-mode-hook 'projectile-rails-on)
  :config
  (set-ruby-mode-to-enh-ruby-mode))

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

(set-ruby-mode-to-enh-ruby-mode)

(provide 'my-ruby)
;;;  my-ruby.el ends here
