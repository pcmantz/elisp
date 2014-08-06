;; my-emacs-lisp.el

(defun my-emacs-lisp-mode-hook ()
  "Configuration for emacs-lisp-mode"
  ;; nothing yet
  )

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook t)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-hook 'local-write-file-hooks
                      '(lambda ()
                         (save-excursion
                           (delete-trailing-whitespace)))))
          t)

(provide 'my-emacs-lisp)
;; end my-emacs-lisp.el
