;; my-java.el

(defun my-java-mode-hook ()
  (setq indent-tabs-mode nil)
  (setq tab-width 4))

(add-hook 'java-mode-hook 'my-java-mode-hook t)
(add-hook 'java-mode-hook
          (lambda ()
            (add-hook 'local-write-file-hooks
                      '(lambda ()
                         (save-excursion
                           (delete-trailing-whitespace)))))
          t)

(provide 'my-java)
;; end my-java.el
