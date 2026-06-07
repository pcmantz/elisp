;; my-terminal --- Terminal emulator configurations  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; kill shell buffer on C-d when process is dead
(use-package shell
  :ensure nil
  :config
  (defun comint-delchar-or-eof-or-kill-buffer (arg)
    (interactive "p")
    (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
      (comint-delchar-or-maybe-eof arg)))
  (add-hook 'shell-mode-hook
            (lambda () (define-key shell-mode-map (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer))
            t))

;; comint / shell mode: ANSI color in shell/compile buffers
(use-package font-lock
  :ensure nil
  :config
  (ansi-color-for-comint-mode-on))

(use-package ghostel :ensure t :demand t)
(use-package vterm :ensure t :demand t)

(use-package vtermux
  :ensure nil
  :demand t
  :after (vterm ghostel)
  :elpaca (vtermux :host nil :repo "~/git/vtermux")
  :bind ("C-c v" . vtermux-run)
  :config
  ;; shells
  (vtermux-define bash)
  (vtermux-define zsh)

  ;; dev tools
  (vtermux-define pitchfork :args '("tui"))
  (vtermux-define claude)
  (vtermux-define opencode
    :program "opencode"
    :directory :project
    :args (lambda (dir) (list "attach" "http://localhost:4096" "--dir" dir)))

  ;; ops tools
  (vtermux-define btop)
  (vtermux-define htop))

(provide 'my-terminal)

;;; my-terminal.el ends here
