;; my-ruby --- Configurations for Ruby

;;; Commentary:

;;; Code:

;; enh-ruby-mode
(use-package enh-ruby-mode
  :mode
  ;; (rx (and (or "Gem" "Rake" "Cap" "Vagrant") "file" (opt ".lock")))
  (("\\(?:Cap\\|Gem\\|Rake\\|Vagrant\\)file\\(?:\\.lock\\)?" . enh-ruby-mode)
   ("\\.rake$" . enh-ruby-mode)
   ("\\.rabl$" . enh-ruby-mode))
  :config
  (progn
    (defalias 'ruby-mode 'enh-ruby-mode)))

(defun my-ruby-defaults ()
  (setq enh-ruby-deep-indent-paren nil))

(setq my-ruby-hook 'my-ruby-defaults)
(add-hook 'enh-ruby-mode-hook (lambda () (run-hooks 'my-ruby-hook)) t)

(require 'rubocop)
(add-hook 'enh-ruby-mode-hook 'rubocop-mode)
(eval-after-load 'rubocop '(diminish 'rubocop-mode))

(require 'ruby-tools)
(add-hook 'enh-ruby-mode-hook 'ruby-tools-mode)
(eval-after-load 'ruby-tools '(diminish 'ruby-tools-mode))

(require 'robe)
(add-hook 'enh-ruby-mode-hook 'robe-mode)

(require 'chruby)

(defun ruby-hash-arrows-to-keys-region (beg end)
  "Replace symbol-arrow hash syntax with the newer 1.9 Javascript-like syntax."
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char beg)
    (while (re-search-forward "['\"]\\([[:alnum:]_]+\\)['\"]\\s-+=>\\s-+" nil t)
      (replace-match "\\1: " t nil))))

(defun ruby-hash-arrows-to-keys-buffer ()
  (interactive)
  (ruby-hash-arrows-to-keys-region (point-min) (point-max)))

(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'ruby-mode)
       (setcdr pair 'enh-ruby-mode)))
 (append auto-mode-alist interpreter-mode-alist))

(provide 'my-ruby)
;;end my-ruby.el
