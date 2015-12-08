;; my-cc.el

(defun my-c-mode-common-hook ()
  (c-set-style "k&r")
  (setq tab-width 4
        c-basic-offset 4
        c-electric-flag nil
        c-tab-always-indent t))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook t)

(provide 'my-cc)
;; end my-cc.el
