;; my-bindings --- Binding for Emacs

;;; Commentary:

;;; Code:

;;
;; Emacs Core Bindings
;;

(use-package files
  :ensure nil
  :bind ("<f5>" . revert-buffer))

(bind-key "<f11>" 'toggle-frame-fullscreen)

(unbind-key "C-z")       ;; stopping emacs is useless
(unbind-key "C-x C-c")   ;; don't make it easy to kill emacs
;; (unbind-key "<f2> <f2>") ;; whatever this is is frustrating

(use-package isearch
  :ensure nil
  :bind
  (( "C-s" . isearch-forward-regexp)
   ( "C-r" . isearch-backward-regexp)
   ( "C-M-s" . isearch-forward)
   ( "C-M-r" . isearch-backward)))

(use-package misc
  :ensure nil
  :bind ("M-Z" . zap-up-to-char))

(use-package window
  :ensure nil
  :bind (("C-x K" . kill-buffer-and-window)))

(bind-key "C-x F" 'msg-buffer-filename)

;; bindings for multiple-cursors
(use-package multiple-cursors
  :bind ("C-|". pcm/multiple-cursors-transient)
  :init
  (transient-define-prefix pcm/multiple-cursors-transient ()
    "Run multiple-cursors with a transient."
    [["Lines"
       ("n" "Mark next like this" mc/mark-next-like-this)
       ("p" "Mark previous like this" mc/mark-previous-like-this)
       ("a" "Mark all like this" mc/mark-all-like-this)
       ("r" "Mark all in region (regexp)" mc/mark-all-in-region-regexp)]
      ["Words"
        ("M-n" "Mark next word like this" mc/mark-next-word-like-this)
        ("M-p" "Mark previous word like this" mc/mark-previous-word-like-this)
        ("M-a" "mark all words like this" mc/mark-all-words-like-this)]]))

; expand the marked region in semantic increments (negative prefix to reduce region)
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; enable default disabled bindings
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; windmove for navigating buffers
(use-package windmove
  :ensure nil
  :config (windmove-default-keybindings))

;; ace-window for navigating buffers. Let's give this a try.
(use-package ace-window
  :bind ("C-c w" . ace-window))

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

;; winner-mode for window undo/redo
(use-package winner
  :ensure nil
  :config
  (winner-mode t))

;;
;; Aliases
;;
(defalias 'tail-mode 'auto-revert-tail-mode)

;;
;; Packages
;;

;; perspective -- workspace (perspective) handling for emacs
(use-package perspective
  :demand t
  :bind
  (("C-x b" . persp-switch-to-buffer*)
   ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-mode-prefix-key (kbd "C-z"))
  (persp-show-modestring nil)
  (persp-sort 'access)
  :config
  ;; setq here to ensure it gets set immediately before `persp-state-load'
  (setq persp-state-default-file (concat user-emacs-directory "perspective"))
  (persp-mode)
  (add-hook 'persp-switch-hook #'persp-state-save)
  (add-hook 'kill-emacs-hook #'persp-state-save)
  (persp-state-load persp-state-default-file))

;; default-text-scale
(use-package default-text-scale
  :bind
  (("C-M-+" . default-text-scale-increase)
   ("C-M--" . default-text-scale-decrease)))

;; yasnippet
(use-package yasnippet
  :demand t
  :blackout " ✂️"
  :custom
  (yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))
  :config
  (yas-global-mode)
  (yas-load-directory (concat user-emacs-directory "/snippets"))
  (set-default 'yas--dont-activate
    #'(lambda ()
        (or buffer-read-only
          (and yas-snippet-dirs
            (null (yas--get-snippet-tables)))))))

(use-package yasnippet-snippets)

(use-package dash-at-point
  :bind
  (("C-c d" . dash-at-point)
   ("C-c D" . dash-at-point-with-docset)))

(provide 'my-bindings)
;;; my-bindings ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
