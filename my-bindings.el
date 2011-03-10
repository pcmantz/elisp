;; my-bindings.el

(define-key global-map (kbd "<f5>")  'revert-buffer)
(define-key global-map (kbd "<f11>") 'fullscreen)

(define-key global-map (kbd "C-z") nil)   ;; stopping emacs is useless

(defalias 'qrr 'query-replace-regexp)
(defalias 'rr 'replace-regexp)

(defalias 'tail-mode 'auto-revert-tail-mode)

;; yasnippet

(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat elisp-dir "/git/yasnippet/snippets"))
(yas/load-directory (concat elisp-dir "/snippets"))

(provide 'my-bindings)

;; end my-bindings.el
