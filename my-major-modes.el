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
(add-to-list 'auto-mode-alist '("\\.yml$"  . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(eval-after-load 'yaml-mode
  (setq yaml-indent-offset 'tab-width))

;; sql-indent
(eval-after-load "sql"
  '(load-library "sql-indent"))

;; clojure-mode

;; slime
(require 'slime)
(slime-setup)
(eval-after-load "slime"
  '(progn (slime-setup '(slime-repl))))

;; magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-diff-options "-w")
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))

;; multi-term
(require 'multi-term)
(setq multi-term-program "/bin/bash")

;; rnc-mode
(setq auto-mode-alist
      (cons '("\\.rnc\\'" . rnc-mode) auto-mode-alist))

;; csv-mode
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;; browse-kill-ring
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(provide 'my-major-modes)
;; end my-major-modes.el
