;; my-bindings.el

(define-key global-map (kbd "<f5>")  'revert-buffer)
(define-key global-map (kbd "<f11>") 'fullscreen)

(define-key global-map (kbd "C-z") nil)   ;; stopping emacs is useless

(defalias 'qr 'query-replace)
(defalias 'rs 'replace-string)
(defalias 'qrr 'query-replace-regexp)
(defalias 'rr 'replace-regexp)

(defalias 'tail-mode 'auto-revert-tail-mode)

;; enable default disabled bindings
(put 'set-goal-column 'disabled nil)

;; windmove for navigating buffers
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; escreen
(require 'escreen)
(require 'ido-escreen)
(escreen-install)
(setq escreen-prefix-char (kbd "C-z"))
(global-set-key escreen-prefix-char 'escreen-prefix) 
(add-to-list 'same-window-buffer-names "*Escreen List*")
(add-hook 'escreen-goto-screen-hook
          'escreen-enable-number-mode-if-more-than-one-screen)
(defalias 'escreen-goto-screen 'ido-escreen-goto-screen)

;; yasnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat elisp-dir "/git/yasnippet/snippets"))
(yas/load-directory (concat elisp-dir "/snippets"))
(setq yas/prompt-functions '(yas/ido-prompt yas/completing-prompt))

(set-default 'yas/dont-activate
             #'(lambda ()
                 (or buffer-read-only
                     (and yas/root-directory
                          (null (yas/get-snippet-tables))))))

(provide 'my-bindings)
;; end my-bindings.el
