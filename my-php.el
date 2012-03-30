;; my-php.el

(add-to-list 'auto-mode-alist '("\\.php$"  . php-mode))
(add-to-list 'auto-mode-alist '("\\.phpt$" . php-mode))

(add-hook 'php-mode-hook
          (lambda ()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (delete-trailing-whitespace))))))

(provide 'my-php)
;; end my-php.el
