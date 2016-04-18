;; my-bindings --- Binding for Emacs

;;; Commentary:

;;; Code:

;;
;; Emacs Core Bindings
;;

(define-key global-map (kbd "<f5>")  'revert-buffer)
(define-key global-map (kbd "<f11>") 'fullscreen)

(define-key global-map (kbd "C-z") nil)       ;; stopping emacs is useless
(define-key global-map (kbd "C-x C-c") nil)   ;; don't make it easy to kill emacs
(define-key global-map (kbd "<f2> <f2>") nil) ;; whatever this is is frustrating

(define-key global-map (kbd "M-x") 'helm-M-x)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)

(define-key global-map (kbd "C-s") 'isearch-forward-regexp)
(define-key global-map (kbd "C-r") 'isearch-backward-regexp)

(define-key global-map (kbd "C-M-s") 'isearch-forward)
(define-key global-map (kbd "C-M-r") 'isearch-backward)

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

;; bindings for multiple-cursors
(use-package multiple-cursors
  :bind-keymap ("C-|" . multiple-cursors-keymap)
  :init
  (progn
    (defvar multiple-cursors-keymap (make-sparse-keymap))

    (define-key multiple-cursors-keymap (kbd "n") 'mc/mark-next-like-this)
    (define-key multiple-cursors-keymap (kbd "p") 'mc/mark-previous-like-this)
    (define-key multiple-cursors-keymap (kbd "a") 'mc/mark-all-like-this)
    (define-key multiple-cursors-keymap (kbd "r") 'mc/mark-all-in-region-regexp)
    (define-key multiple-cursors-keymap (kbd "d") 'mc/mark-next-like-this-dwim)

    (define-key multiple-cursors-keymap (kbd "M-n") 'mc/mark-next-word-like-this)
    (define-key multiple-cursors-keymap (kbd "M-p") 'mc/mark-previous-word-like-this)
    (define-key multiple-cursors-keymap (kbd "M-a") 'mc/mark-all-words-like-this)))

;; enable default disabled bindings
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; windmove for navigating buffers
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; ace-window for navigating buffers. Let's give this a try.
(use-package ace-window
  :bind ("M-p" . ace-window))

;; winner-mode for window undo/redo
(winner-mode t)

;; better goto-line
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

;; Navigate through edit points
(use-package goto-chg)

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
(use-package workgroups
  :diminish
  :config
  (progn
    (setq wg-prefix-key (kbd "C-z")
          wg-no-confirm t
          wg-morph-on nil
          wg-file (concat elisp-dir "/workgroups")
          wg-use-faces nil
          wg-switch-on-load nil)
    (define-key wg-map (kbd "g") 'wg-switch-to-workgroup)
    (define-key wg-map (kbd "C-l") 'wg-load-default)
    (define-key wg-map (kbd "C-s") 'wg-save-default)
    (define-key wg-map (kbd "<backspace>") 'wg-switch-left)
    (add-hook 'auto-save-hook 'wg-save-default t)
    (add-hook 'kill-emacs-hook 'wg-save-default t)
    (workgroups-mode 1)))

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



;; default-text-scale
(use-package default-text-scale
  :bind (("C-M-+" . default-text-scale-increase)
         ("C-M--" . default-text-scale-decrease)))

;; yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (progn
    (yas-global-mode 1)
    (yas-load-directory (concat elisp-dir "/snippets"))
    (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))
    (set-default 'yas--dont-activate
                 #'(lambda ()
                     (or buffer-read-only
                         (and yas-snippet-dirs
                              (null (yas--get-snippet-tables))))))))

(defvar default-cursor-color "green")
(defvar yasnippet-can-fire-cursor-color "purple")

(defun yasnippet-can-fire-p (&optional field)
  "Check to see if a snippet can fire at point."
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
  "Change the cursor color if a snippet can expand at point."
  (interactive)
  (set-cursor-color (if (yasnippet-can-fire-p)
                        yasnippet-can-fire-cursor-color
                      default-cursor-color)))

; As pointed out by Dmitri, this will make sure it will update color when needed.
;; (add-hook 'post-command-hook 'yasnippet-change-cursor-color-when-can-fire)

(provide 'my-bindings)
;;; my-bindings ends here
