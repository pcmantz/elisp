;;; my-eldoc.el --- eldoc configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; `eldoc-box' shows eldoc documentation in a transient childframe.  We
;; turn on `eldoc-box-hover-at-point-mode' in every buffer that enables
;; `eldoc-mode', so documentation pops up at point.  The eglot `C-h .'
;; remap is handled in my-eglot.el.
;;

;;; Code:

(use-package eldoc-box
  :ensure t
  :hook
  (eldoc-mode . eldoc-box-hover-at-point-mode)
  :init
  (setq eldoc-box-only-multi-line t)
  :custom
  (eldoc-box-max-pixel-width  600)
  (eldoc-box-max-pixel-height 400))

(provide 'my-eldoc)
;;; my-eldoc.el ends here
