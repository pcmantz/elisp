;; my-ruby.el

(defalias 'ruby-mode 'enh-ruby-mode)
(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'ruby-mode)
       (setcdr pair 'enh-ruby-mode)))
 (append auto-mode-alist interpreter-mode-alist))

;; (rx (and (or "Gem" "Rake" "Cap" "Vagrant") "file" (opt ".lock")))
(add-to-list 'auto-mode-alist '("\\(?:Cap\\|Gem\\|Rake\\|Vagrant\\)file\\(?:\\.lock\\)?" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rabl$" . enh-ruby-mode))

(defun my-ruby-defaults ()
  (setq
   enh-ruby-deep-paren nil))

(setq my-ruby-hook 'my-ruby-defaults)

(add-hook 'enh-ruby-mode-hook
          (lambda () (run-hooks 'my-ruby-hook)) t)
(add-hook 'enh-ruby-mode-hook
          (lambda ()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (delete-trailing-whitespace)))))
          t)

(require 'ruby-tools)
(add-hook 'enh-ruby-mode-hook 'ruby-tools-mode)

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

(provide 'my-ruby)
;;end my-ruby.el
