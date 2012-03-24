;; my-ruby.el

;; (rx (and (or "Gem" "Rake" "Cap") "file" (opt ".lock")))
(add-to-list 'auto-mode-alist '("\\(Gem\\|Rake\\|Cap\\)file\\(?:\\.lock\\)?" . ruby-mode))

(provide 'my-ruby)
;;end my-ruby.el
