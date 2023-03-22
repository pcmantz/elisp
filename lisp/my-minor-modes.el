;; my-minor-modes --- Miscellaneous minor modes that aren't complicated enough
;; to warrant their own file

;;; Commentary:

;;; Code:

;; I don't use this and I want to use origami
(unbind-key "C-o")

(use-package origami
  :bind
    ("C-o o" . origami-recursively-toggle-node)
    ("C-o O" . origami-open-node-recursively)
    ("C-o c" . origami-close-node-recursively)
    ("C-o C" . origami-close-all-nodes)
    ("C-o A" . origami-open-all-nodes)
  :config
  (global-origami-mode))

(use-package direnv
  :config
  (direnv-mode)
  (let* ((new-non-file-modes '(magit-mode magit-status-mode magit-refs-mode magit-diff-mode)))
        (mapcar (lambda (nf-mode) (add-to-list 'direnv-non-file-modes nf-mode))
                new-non-file-modes)))

(use-package flycheck
  :blackout " 🪰✅"
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-idle-change-delay 2)
  (flycheck-temp-prefix ".flycheck")
  :config
  (flycheck-define-checker ruby-reek
    "A Ruby smeel checker using reek
See URL `https://github.com/troessner/reek'."
    :command ("reek" "--format=xml"
               source-original)
    :standard-input t
    :error-parser flycheck-parse-checkstyle
    :modes (enh-ruby-mode ruby-mode)
    :next-checkers ((info . ruby-rubocop)))
  (add-to-list 'flycheck-checkers 'ruby-reek)

  (flycheck-define-checker proselint
    "A linter for prose."
    :command
    ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
       (id (one-or-more (not (any " "))))
       (message) line-end))
    :modes
    (text-mode markdown-mode gfm-mode tex-mode latex-mode))
  (add-to-list 'flycheck-checkers 'proselint)

  (setq-default flycheck-disabled-checkers
    (append flycheck-disabled-checkers '(javascript-jshint ruby-reek)))

  (flycheck-add-mode 'javascript-eslint 'js2-mode)

  ;; use local eslint from node_modules before global
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                   (or (buffer-file-name) default-directory)
                   "node_modules"))
            (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                        root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

  ;; Enable flycheck globally
  (global-flycheck-mode))

(use-package flycheck-status-emoji)

;; pandoc-mode
(use-package pandoc-mode)

;; company
(use-package company
  :blackout " 🏭"
  :custom
  (company-idle-delay 0.5)
  :config
  (global-company-mode))

(provide 'my-minor-modes)
;;; my-minor-modes.el ends here
