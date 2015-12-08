;; my-major-modes.el

;; grab bag file for major modes that don't warrant their own config file
;; (yet)

;;
;; programming language modes
;;

;; sql-indent
(eval-after-load 'sql
  '(load-library "sql-indent"))

;; (rx (and line-start "*SQL" (0+ anything)  "*"))
(add-to-list 'same-window-regexps '("^\\*SQL\\(?:.\\|\n\\)*\\*" . nil))

(add-hook 'sql-interactive-mode-hook
          (lambda () (toggle-truncate-lines t)))

;;
;; standalone applications
;;

;; magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(eval-after-load 'magit
  '(progn
     (if (fboundp 'magit-completing-read-function)
         (setq magit-completing-read-function 'helm-comp-read))))

;; (rx (and line-start "*magit" (zero-or-one (and "-" (or "refs"))) ":" (0+ anything)  "*"))
(add-to-list 'same-window-regexps '("^\\*magit\\(?:-\\(?:refs\\)\\)?:\\(?:.\\|\n\\)*\\*" . nil))

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-arguments)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-arguments "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-arguments (remove "-w" magit-diff-arguments))
  (magit-refresh))

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

;; multi-term
(require 'multi-term)
(setq multi-term-program "/bin/bash")
(add-hook 'term-mode-hook (lambda () (yas-minor-mode -1)))

;; browse-kill-ring
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;;
;; markup modes
;;

;; diff-mode
(setq diff-switches "-u")
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

;; yaml-mode
(add-to-list 'auto-mode-alist '("\\.yml$"  . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(autoload 'yaml-mode "yaml-mode" "Major mode for editing YAML files." t)
(eval-after-load 'yaml-mode
  (setq yaml-indent-offset 'tab-width))

;; rnc-mode
(add-to-list 'auto-mode-alist '("\\.rnc\\'" . rnc-mode))
(autoload 'rnc-mode "rnc-mode" "Major mode for editing Relax NG Compact schema files." t)
(eval-after-load 'rnc-mode
  '(progn
     (setq rnc-indent-level 'tab-width)
     (setq rnc-mode-map (make-sparse-keymap))
     (define-key rnc-mode-map "\C-c\C-c" 'comment-region)))

;; csv-mode
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode" "Major mode for editing csv files." t)

;; markdown-mode
(add-to-list 'auto-mode-alist '("\\.md$"       . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(autoload 'markdown-mode "markdown-mode" "Major mode for editing markdown files." t)

(provide 'my-major-modes)
;; end my-major-modes.el


;;
;; Fundamental mode overrides
;;

(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer))
          t)
