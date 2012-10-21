;; my-bindings.el

;;
;; Emacs Core Bindings
;;

(define-key global-map (kbd "<f5>")  'revert-buffer)
(define-key global-map (kbd "<f11>") 'fullscreen)

(define-key global-map (kbd "C-z") nil)       ;; stopping emacs is useless
(define-key global-map (kbd "C-x C-c") nil)   ;; don't make it easy to kill emacs
(define-key global-map (kbd "<f2> <f2>") nil) ;; whatever this is is frustrating

(define-key global-map (kbd "C-s") 'isearch-forward-regexp)
(define-key global-map (kbd "C-r") 'isearch-backward-regexp)

(define-key global-map (kbd "C-M-s") 'isearch-forward)
(define-key global-map (kbd "C-M-r") 'isearch-backward-regexp)

;; enable default disabled bindings
(put 'set-goal-column 'disabled nil)

;; windmove for navigating buffers
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; winner-mode for window undo/redo
(winner-mode t)

;;
;; Aliases
;;

(defalias 'qr 'query-replace)
(defalias 'rs 'replace-string)
(defalias 'qrr 'query-replace-regexp)
(defalias 'rr 'replace-regexp)

(defalias 'lf 'load-file)
(defalias 'll 'load-library)

(defalias 'tail-mode 'auto-revert-tail-mode)
(defalias 'quit-emacs 'save-buffers-kill-terminal)

;;
;; Packages
;;

;; escreen
(require 'escreen)
(require 'ido-escreen)
(escreen-install)
(setq escreen-prefix-char (kbd "C-z"))
(global-set-key escreen-prefix-char 'escreen-prefix)
(add-to-list 'same-window-buffer-names "*Escreen List*")
(add-hook 'escreen-goto-screen-hook
          'escreen-enable-number-mode-if-more-than-one-screen)
(define-key escreen-map (kbd "<backspace>") 'escreen-goto-prev-screen)

;; yasnippet
(require 'yasnippet)
(yas--initialize)
(yas-load-directory (concat elisp-dir "/snippets"))
(setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))
(set-default 'yas--dont-activate
             #'(lambda ()
                 (or buffer-read-only
                     (and yas-snippet-dirs
                          (null (yas--get-snippet-tables))))))

;; remember
(require 'remember)
(define-key global-map (kbd "<f2>") 'remember)
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(setq
 remember-annotation-functions '(org-remember-annotation)
 remember-handler-functions    '(org-remember-handler))

(provide 'my-bindings)
;; end my-bindings.el
