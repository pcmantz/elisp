;;;; my-emacs-lisp --- Emacs Lisp Configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; Local customizations for emacs lisp. Not much to see here.

;;; Code:

(use-package elisp-mode
  :ensure nil
  :config
  (setq indent-tabs-mode nil))

(provide 'my-emacs-lisp)
;;; my-emacs-lisp.el ends here
