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

;;
;; Packages
;;

;; workgroups
(require 'workgroups)
(setq wg-prefix-key (kbd "C-z")
      wg-no-confirm t
      wg-file (concat elisp-dir "/workgroups")
      wg-use-faces nil
      wg-switch-on-load nil)

(defun wg-load-default ()
  "Run `wg-load' on `wg-file'."
  (interactive)
  (wg-load wg-file))

(defun wg-save-default ()
  "Run `wg-save' on `wg-file'."
  (interactive)
  (when wg-list
    (with-temp-message ""
      (wg-save wg-file))))

(define-key wg-map (kbd "g") 'wg-switch-to-workgroup)
(define-key wg-map (kbd "C-l") 'wg-load-default)
(define-key wg-map (kbd "C-s") 'wg-save-default)
(define-key wg-map (kbd "<backspace>") 'wg-switch-left)
(workgroups-mode 1)
(add-hook 'auto-save-hook 'wg-save-default)
(add-hook 'kill-emacs-hook 'wg-save-default)

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
