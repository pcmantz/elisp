;; my-vc --- Version control tools  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package transient
  :ensure t
  :demand t)

(use-package transient-dwim
  :ensure t
  :bind ("M-=" . transient-dwim-dispatch))

(use-package transient-compile
  :ensure
  (:host github :repo "gavv/transient-compile"))

;; magit
(use-package magit
  :bind
  ("C-x g" . magit-status)
  ("C-c b" . magit-blame)
  :custom
  (magit-status-show-untracked-files 'all)
  :init
  (add-to-list 'same-window-regexps '("^magit:[^z-a]*" . nil))
  :config
  (add-to-list 'same-window-regexps '("^\\*magit\\(?:-\\(?:refs\\)\\)?:\\(?:.\\|\n\\)*\\*" . nil))
  (add-to-list 'magit-repository-directories '("~/git" . 1))
  (add-to-list 'magit-repository-directories '("~/Documents" . 1))
  (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)
  (add-hook 'magit-mode-hook
    (lambda () (add-to-list 'magit-buffer-diff-args "--algorithm=histogram"))))

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

(use-package git-modes)

(use-package diff-hl
  :config (global-diff-hl-mode))

(use-package diff-mode
  :ensure nil
  :custom
  (diff-switches "-u")
  :config
  (set-face-foreground 'diff-added "green4")
  (set-face-foreground 'diff-removed "red3"))

(use-package beads :ensure (:host github :repo "ChristianTietze/beads.el"))

(provide 'my-vc)
;;; my-vc.el ends here
