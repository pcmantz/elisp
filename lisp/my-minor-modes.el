;; my-minor-modes --- Miscellaneous minor modes that aren't complicated enough
;; to warrant their own file

;;; Commentary:

;;; Code:

;; outline-minor-mode stuff.  Not requiring this since I don't need it
;; everywhere, but it is useful.

;; I don't use this and I want to use origami
(global-unset-key (kbd "C-o"))

(use-package origami
  :config
  (progn
    (define-key global-map (kbd "C-o o") 'origami-recursively-toggle-node)
    (define-key global-map (kbd "C-o O") 'origami-open-node-recursively)
    (define-key global-map (kbd "C-o c") 'origami-close-node-recursively)
    (define-key global-map (kbd "C-o C") 'origami-close-all-nodes)
    (define-key global-map (kbd "C-o A") 'origami-open-all-nodes)

    (global-origami-mode)))

(use-package direnv
  :config
  (progn
    (direnv-mode)
    (let ((new-non-file-modes '('magit-mode
                                'magit-status-mode
                                'magit-refs-mode
                                'magit-diff-mode)))
      (mapcar (lambda (nf-mode) (add-to-list 'direnv-non-file-modes nf-mode))
              new-non-file-modes))))

(use-package flycheck
  :config
  (progn
    (setq-default flycheck-temp-prefix ".flycheck")

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

    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers '(javascript-jshint)))

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

    (setq flycheck-check-syntax-automatically '(save mode-enabled))

    ;; The following is not used while idle-change is not in the above list
    (setq flycheck-idle-change-delay 2)
    (global-flycheck-mode)))

(use-package flycheck-status-emoji)

(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
        (id (one-or-more (not (any " "))))
        (message) line-end))
  :modes (text-mode markdown-mode gfm-mode tex-mode latex-mode))
(add-to-list 'flycheck-checkers 'proselint)

;; pandoc-mode
(use-package pandoc-mode)

;; company
(use-package company
  :config
  (progn
    (global-company-mode)))

(provide 'my-minor-modes)
;;; my-minor-modes.el ends here
