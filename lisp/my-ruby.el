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
    (defun my-ruby-defaults () (setq enh-ruby-deep-indent-paren nil))
    (defalias 'my-ruby-defaults 'my-ruby-hook)
    (add-hook 'enh-ruby-mode-hook (lambda () (run-hooks 'my-ruby-hook)) t)
    (add-hook 'enh-ruby-mode-hook 'rubocop-mode)
    (add-hook 'enh-ruby-mode-hook 'robe-mode)
    (add-hook 'enh-ruby-mode-hook 'ruby-tools-mode)))

(use-package rubocop
  :diminish rubocop-mode)
(use-package ruby-tools
  :diminish ruby-tools)
(use-package robe)
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

(provide 'my-ruby)
;;;  my-ruby.el ends here
