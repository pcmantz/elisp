;; my-major-modes --- Major modes in common use

;;; Commentary:

;; grab bag file for major modes that don't warrant their own config file
;; (yet)

;;; Code:

;;
;; standalone applications
;;

;; magit
(use-package magit
  :bind
  ("C-x g" . magit-status)
  ("C-c b" . magit-blame)
  :init
  (add-to-list 'same-window-regexps '("^magit:[^z-a]*" . nil))
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
(use-package diff-mode
  :elpaca nil
  :custom
  (diff-switches "-u")
  :config
  (set-face-foreground 'diff-added "green4")
  (set-face-foreground 'diff-removed "red3"))

;; yaml-mode
(use-package yaml-mode
  :elpaca nil
  :mode
  (("\\.ya?ml$" . yaml-mode))
  :custom (yaml-indent-offset 2))

;; rnc-mode
(use-package rnc-mode
  :custom
  (rnc-indent-level 'tab-width))

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
  :mode
  (("\\.(?:plant)uml$" . plantuml-mode))
  :custom
  (plantuml-executable-path "/usr/local/bin/plantuml")
  (plantuml-default-exec-mode 'executable))

(use-package flycheck-plantuml)

(use-package web-mode
  :mode
  (("\\.[agj]sp\\'" . web-mode)
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

;; sh-mode
(use-package sh-script
  :elpaca nil
  :custom
  (sh-basic-offset 2))

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
          (lambda () (define-key shell-mode-map (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer))
          t)


;; Miscellaneous modes
(use-package apache-mode)
(use-package docker-compose-mode)
(use-package dockerfile-mode)
(use-package gradle-mode)
(use-package graphviz-dot-mode)
(use-package groovy-mode)
(use-package php-mode)

(provide 'my-major-modes)
;;; my-major-modes.el ends here
