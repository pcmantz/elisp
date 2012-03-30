;; my-ruby.el

;; (rx (and (or "Gem" "Rake" "Cap") "file" (opt ".lock")))
(add-to-list 'auto-mode-alist '("\\(Gem\\|Rake\\|Cap\\)file\\(?:\\.lock\\)?" . ruby-mode))

(add-hook 'ruby-mode-hook
          (lambda ()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (delete-trailing-whitespace))))))

(provide 'my-ruby)
;;end my-ruby.el
