;; my-remember.el

;; remember mode.  makes entering org mode stuff easier
(require 'remember)

(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key global-map [(control meta ?r)] 'remember)

(setq
 remember-annotation-functions '(org-remember-annotation)
 remember-handler-functions    '(org-remember-handler))

(provide 'my-remember)
;; end my-remember.el
