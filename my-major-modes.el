;; my-major-modes.el

;; grab bag file for major modes that don't warrant their own config file
;; (yet)

;; diff-mode
(setq diff-switches "-u")
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$"  . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(eval-after-load 'yaml-mode
  (setq yaml-indent-offset 'tab-width))

;; sql-indent
(eval-after-load "sql"
  '(load-library "sql-indent"))

;; magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (if (fboundp 'magit-completing-read-function) 
         (setq magit-completing-read-function 'ido-completing-read))))

;; note: may have to amend for unicode project names
(add-to-list 'same-window-regexps "\\*magit: [[:ascii:]]\\*")

;; multi-term
(require 'multi-term)
(setq multi-term-program "/bin/bash")

;; rnc-mode
(add-to-list 'auto-mode-alist '("\\.rnc\\'" . rnc-mode))
(autoload 'rnc-mode "rnc-mode"
  "Major mode for editing Relax NG Compact schema files." t)
(eval-after-load 'rnc-mode
  '(progn (setq rnc-indent-level 'tab-width)
          (setq rnc-mode-map (make-sparse-keymap))
          (define-key rnc-mode-map "\C-c\C-c" 'comment-region)))


;; csv-mode
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;; browse-kill-ring
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(provide 'my-major-modes)
;; end my-major-modes.el
