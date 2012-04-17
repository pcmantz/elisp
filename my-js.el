;; my-js.el

(add-to-list 'auto-mode-alist '("*\\.js$" . javascript-mode))

(add-hook 'javascript-mode-hook
          (lambda ()
            (add-hook 'local-write-file-hooks
                      '(lambda ()
                         (save-excursion
                           (delete-trailing-whitespace)))))
          t)

(provide 'my-js)
;; end my-js.el
