;; my-major-modes --- Major modes in common use

;;; Commentary:

;; grab bag file for major modes that don't warrant their own config file
;; (yet)

;;; Code:

;;
;; programming language modes
;;

;; sql-indent
(use-package sql)
(use-package sql-indent)

(use-package sqlformat
  :custom
  (sqlformat-command 'pgformatter)
  ;;; NOTE: The "-p" option specifies ignored areas from formatting and this will be mode-dependent
  ;;; for embedded SQL strings. The one configured here is intended for interpolated ruby code for
  ;;; SQL embedded in double-quoted/HEREdoc strings.
  (sqlformat-args '("-s" "2" "-u" "2" "-p" "'\\#\\{[^}]*\\}'")))

;; (rx (and line-start "*SQL" (0+ anything)  "*"))
(add-to-list 'same-window-regexps '("^\\*SQL\\(?:.\\|\n\\)*\\*" . nil))

(add-hook 'sql-interactive-mode-hook
          (lambda () (toggle-truncate-lines t)))

;;
;; standalone applications
;;

;; magit
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-c b" . magit-blame))
  :config
  (progn
    (if (fboundp 'magit-completing-read-function)
        (setq magit-completing-read-function 'helm-comp-read))
    ;; (rx (and line-start "*magit" (zero-or-one (and "-" (or "refs"))) ":" (0+ anything)  "*"))
    (add-to-list 'same-window-regexps '("^\\*magit\\(?:-\\(?:refs\\)\\)?:\\(?:.\\|\n\\)*\\*" . nil))
    (add-to-list 'magit-repository-directories '("~/git" . 1))
    (add-to-list 'magit-repository-directories '("~/Documents" . 1))
    (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)))

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

;; multi-term
(use-package multi-term
  :custom
  (multi-term-program "/bin/bash")
  :config
  (progn
    (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1)))))

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
(use-package yaml-mode
  :mode (("\\.ya?ml$"  . yaml-mode))
  :custom (yaml-indent-offset 2))

;; rnc-mode
(use-package rnc-mode
  :config
  (progn
    (setq rnc-indent-level 'tab-width)))

;; csv-mode
(use-package csv-mode
  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode))

;; markdown-mode
(use-package markdown-mode
  :mode  (("\\.md$" . markdown-mode)
          ("\\.markdown$" . markdown-mode)))

;; hcl-mode
(use-package hcl-mode
  :mode (("\\.tf(?:state)" . hcl-mode)
         ("\\.json" . hcl-mode)))

(use-package plantuml-mode
  :mode (("\\.plantuml$" . plantuml-mode)))

(use-package web-mode
  :mode (("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.hbs\\'". web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.p?html\\'" . web-mode)
          ("\\.tpl\\.php\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-engines-alist '(("php" . "\\.phtml\\'")
                             ("blade" . "\\.blade\\.")
                             ("ctemplate" . "\\.hbs\\."))))

;; scss-mode
(use-package scss-mode
  :custom
  (css-set-offset 2))

(use-package css-mode
  :custom
  (css-set-offset 2))

;; protobuf-mode
(use-package protobuf-mode
  :custom
  (c-basic-offset 2 "Set the tab width to 2.")
  (indent-tabs-mode nil "No tabs."))

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

(provide 'my-major-modes)
;;; my-major-modes.el ends here
