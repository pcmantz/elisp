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

(use-package flycheck
  :config
  (progn
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
