;; my-env --- Basic environment configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; miscellaneous environment variables and global scope modes

;;; Code:

(use-package emacs
  :ensure nil
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  :custom
  (echo-keystrokes 0.1)
  (enable-recursive-minibuffers t)
  (fill-column 100)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (tab-width 4)
  (visible-bell t)

  ;; defined in paragraphs.el but whatever
  (sentence-end-double-space nil))

(use-package simple
  :ensure nil
  :bind
  ("M-z" . zap-to-char)
  :custom
  (column-number-mode t)
  (line-number-mode t)
  (transient-mark-mode t)
  (indent-tabs-mode nil))

;;
;; configuration
;;

;; keep local customizations out of this file
(use-package cus-edit
  :ensure nil
  :custom
  (custom-file (concat user-emacs-directory "my-custom-file.el"))
  :hook
  (elpaca-after-init . (lambda () (load custom-file 'noerror)))
  :config
  (if (not (file-exists-p custom-file))
    (write-region "" nil custom-file)))

;; path (via exec-path-from-shell)
(use-package exec-path-from-shell
  :if
  (memq window-system '(mac ns))
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

;;
;; TRAMP
;;
 ;; NOTE: To avoid recursive loads, we load tramp right here and now
(use-package tramp :ensure nil :demand t)

;; text display config
(use-package paren
  :ensure nil
  :config
  (show-paren-mode t))

;;
;; interaction
;;

(defalias 'yes-or-no-p 'y-or-n-p)

;; whitespace configuration
;; TODO: Make individual customizations for major modes
(use-package whitespace
  :ensure nil
  :demand t
  :bind ("C-x W" . 'whitespace-mode)
  :custom
  ((whitespace-line-column 100))
  :config
  (add-hook 'before-save-hook 'whitespace-cleanup))

(use-package editorconfig
  :delight " 🖋️⚙️"
  :config
  (editorconfig-mode 1))

(use-package keyfreq
  :demand t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package nerd-icons
  :demand t)

(provide 'my-env)
;;; my-env.el ends here
