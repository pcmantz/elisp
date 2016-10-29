;; my-minor-modes --- Miscellaneous minor modes that aren't complicated enough
;; to warrant their own file

;;; Commentary:

;;; Code:

;; outline-minor-mode stuff.  Not requiring this since I don't need it
;; everywhere, but it is useful.

(use-package origami
  :config
  (progn
    (global-origami-mode)))

(use-package flycheck
  :config
  (progn
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
