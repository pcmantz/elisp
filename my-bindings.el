;; my-bindings.el

;;
;; Emacs Core Bindings
;;

(define-key global-map (kbd "<f5>")  'revert-buffer)
(define-key global-map (kbd "<f11>") 'fullscreen)

(define-key global-map (kbd "C-z") nil)       ;; stopping emacs is useless
(define-key global-map (kbd "C-x C-c") nil)   ;; don't make it easy to kill emacs
(define-key global-map (kbd "<f2> <f2>") nil) ;; whatever this is is frustrating

(define-key global-map (kbd "M-x") 'helm-M-x)

(define-key global-map (kbd "C-s") 'isearch-forward-regexp)
(define-key global-map (kbd "C-r") 'isearch-backward-regexp)

(define-key global-map (kbd "C-M-s") 'isearch-forward)
(define-key global-map (kbd "C-M-r") 'isearch-backward-regexp)

(define-key global-map (kbd "C-M-g") 'goto-line)
(define-key global-map (kbd "C-S-l") 'goto-line)
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(define-key global-map (kbd "M-y") 'helm-show-kill-ring)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-z") 'zap-to-char)
(global-set-key (kbd "M-Z") 'zap-up-to-char)

(define-key global-map (kbd "C-x K") 'kill-buffer-and-window)

(define-key global-map (kbd "C-x F") 'msg-buffer-filename)
(define-key global-map (kbd "C-c b") 'magit-blame)

;; enable default disabled bindings
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; windmove for navigating buffers
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; winner-mode for window undo/redo
(winner-mode t)

;; better goto-line
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

;; Navigate through edit points
(require 'goto-chg)

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
      wg-morph-on nil
      wg-file (concat elisp-dir "/workgroups")
      wg-use-faces nil
      wg-switch-on-load nil)
(eval-after-load 'workgroups '(diminish 'workgroups-mode))

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
(add-hook 'auto-save-hook 'wg-save-default t)
(add-hook 'kill-emacs-hook 'wg-save-default t)

;; yasnippet
(require 'yasnippet)
(yas-initialize)
(yas-load-directory (concat elisp-dir "/snippets"))
(setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))
(set-default 'yas--dont-activate
             #'(lambda ()
                 (or buffer-read-only
                     (and yas-snippet-dirs
                          (null (yas--get-snippet-tables))))))
(eval-after-load 'yasnippet '(diminish 'yas-minor-mode))

(setq default-cursor-color "green")
(setq yasnippet-can-fire-cursor-color "purple" )

;; It will test whether it can expand, if yes, cursor color -> green.
(defun yasnippet-can-fire-p (&optional field)
  (interactive)
  (setq yas--condition-cache-timestamp (current-time))
  (let (templates-and-pos)
    (unless (and yas-expand-only-for-last-commands
                 (not (member last-command yas-expand-only-for-last-commands)))
      (setq templates-and-pos (if field
                                  (save-restriction
                                    (narrow-to-region (yas--field-start field)
                                                      (yas--field-end field))
                                    (yas--current-key))
                                (yas--current-key))))
    (and templates-and-pos (first templates-and-pos))))

(defun yasnippet-change-cursor-color-when-can-fire (&optional field)
  (interactive)
  (set-cursor-color (if (yasnippet-can-fire-p)
                        yasnippet-can-fire-cursor-color
                      default-cursor-color)))

; As pointed out by Dmitri, this will make sure it will update color when needed.
;; (add-hook 'post-command-hook 'yasnippet-change-cursor-color-when-can-fire)

(provide 'my-bindings)
;; end my-bindings.el
