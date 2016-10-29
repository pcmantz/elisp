;; my-minor-modes --- Miscellaneous minor modes that aren't complicated enough
;; to warrant their own file

;;; Commentary:

;;; Code:

;; outline-minor-mode stuff.  Not requiring this since I don't need it
;; everywhere, but it is useful.

;; Outline-minor-mode key map
(define-prefix-command 'cm-map nil "Outline-")

;; HIDE
(define-key cm-map "q" 'hide-sublevels)    ; Hide everything but the top-level headings
(define-key cm-map "t" 'hide-body)         ; Hide everything but headings (all body lines)
(define-key cm-map "o" 'hide-other)        ; Hide other branches
(define-key cm-map "c" 'hide-entry)        ; Hide this entry's body
(define-key cm-map "l" 'hide-leaves)       ; Hide body lines in this entry and sub-entries
(define-key cm-map "d" 'hide-subtree)      ; Hide everything in this entry and sub-entries
;; SHOW
(define-key cm-map "a" 'show-all)          ; Show (expand) everything
(define-key cm-map "e" 'show-entry)        ; Show this heading's body
(define-key cm-map "i" 'show-children)     ; Show this heading's immediate child sub-headings
(define-key cm-map "k" 'show-branches)     ; Show all sub-headings under this heading
(define-key cm-map "s" 'show-subtree)      ; Show (expand) everything in this heading & below
;; MOVE
(define-key cm-map "u" 'outline-up-heading)                ; Up
(define-key cm-map "n" 'outline-next-visible-heading)      ; Next
(define-key cm-map "p" 'outline-previous-visible-heading)  ; Previous
(define-key cm-map "f" 'outline-forward-same-level)        ; Forward - same level
(define-key cm-map "b" 'outline-backward-same-level)       ; Backward - same level

(global-set-key (kbd "C-c o") cm-map)

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
