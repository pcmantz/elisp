;; my-ruby.el

;; (rx (and (or "Gem" "Rake" "Cap") "file" (opt ".lock")))
(add-to-list 'auto-mode-alist '("\\(Gem\\|Rake\\|Cap\\)file\\(?:\\.lock\\)?" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rabl$" . ruby-mode))


(require 'rvm)
(rvm-use-default)

(require 'ruby-tools)

(add-hook 'ruby-mode-hook
          (lambda ()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (delete-trailing-whitespace)))))
          t)

(defun ruby-hash-arrows-to-keys-region (beg end)
  "Replace symbol-arrow hash syntax with the newer 1.9 Javascript-like syntax."
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char beg)
    (while (re-search-forward ":\\([[:alnum:]_]+\\)\\s-+=>\\s-+" nil t)
      (replace-match "\\1: " t nil))))

(defun ruby-hash-arrows-to-keys-buffer ()
  (interactive)
  (ruby-hash-arrows-to-keys-region (point-min) (point-max)))


(provide 'my-ruby)
;;end my-ruby.el
