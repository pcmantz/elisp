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

;; (rx (and line-start "*SQL" (0+ anything)  "*"))
(add-to-list 'same-window-regexps '("^\\*SQL\\(?:.\\|\n\\)*\\*" . nil))

(add-hook 'sql-interactive-mode-hook
          (lambda () (toggle-truncate-lines t)))

;;
;; standalone applications
;;

;; magit
(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (progn
    (if (fboundp 'magit-completing-read-function)
        (setq magit-completing-read-function 'helm-comp-read))
    ;; (rx (and line-start "*magit" (zero-or-one (and "-" (or "refs"))) ":" (0+ anything)  "*"))
    (add-to-list 'same-window-regexps '("^\\*magit\\(?:-\\(?:refs\\)\\)?:\\(?:.\\|\n\\)*\\*" . nil))
    (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)))

(defun dir-git-project? (dir)
  "Return true if DIR is a git project directory. False otherwise."
  (f-exists? (f-join dir ".git")))

(defun add-to-list-git-subdirs (list dir)
  "Add the subdirectories of DIR to LIST."
  (let* ((subdirs (f-directories dir)))
    (add-to-list 'list
                 (-map (lambda (dir) (dir . 0))
                       (-filter 'dir-git-project? subdirs)))))

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
  :config
  (progn
    (setq multi-term-program "/bin/bash")
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
  :config (setq yaml-indent-offset 2))

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
  :mode (("\\.p?html\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode))
  :config
  (progn
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)))

;; scss-mode
(use-package scss-mode
  :config
  (progn
    (custom-set-variables
     '(css-indent-offset 2))))

(use-package css-mode
  :config
  (progn
    (custom-set-variables
     '(css-indent-offset 2))))

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
