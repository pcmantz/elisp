;;; my-eldoc.el --- eldoc configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; Eglot remaps `display-local-help' (C-h .) to `eldoc-doc-buffer', which
;; calls a bare `display-buffer' and so takes over a normal window that then
;; persists, refreshing on every subsequent eldoc update.  `eldoc-box' shows
;; the same content in a transient childframe instead, dismissed by moving
;; point or with C-g.
;;
;; Only the on-demand command is wired up here.  `eldoc-box-hover-at-point-mode'
;; would make the childframe follow point, but its README notes a noticeable
;; slow-down, and the eldoc path in eglot buffers is already latency-sensitive.

;;; Code:

(use-package eldoc-box
  :after eglot
  :bind (:map eglot-mode-map
              ([remap display-local-help] . eldoc-box-help-at-point))
  :custom
  (eldoc-box-only-multi-line t)
  (eldoc-box-max-pixel-width  600)
  (eldoc-box-max-pixel-height 400))

(provide 'my-eldoc)
;;; my-eldoc.el ends here
