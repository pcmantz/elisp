;; my-bindings.el

(define-key global-map (kbd "<f5>") 'revert-buffer)

(define-key global-map (kbd "C-z") nil)   ;; stopping emacs is useless

(defalias 'qrr 'query-replace-regexp)
(defalias 'rr 'replace-regexp)

(defalias 'tail-mode 'auto-revert-tail-mode)

;; yasnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat elisp-dir "/yasnippet/snippets"))
(yas/load-directory (concat elisp-dir "/snippets"))
(setq yas/global-mode t)

;; hippie-expand
(require 'hippie-exp)

(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-lisp-symbol))

(global-set-key (kbd "M-/") 'hippie-expand)

(provide 'my-bindings)