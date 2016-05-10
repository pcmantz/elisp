;; my-perl --- Configuration for Perl

;;; Commentary:

;;; Code:

(defun set-perl-mode-to-cperl-mode ()
    "Replace `perl-mode' with `cperl-mode' in `auto-mode-alist' and `interpreter-mode-alist'."
    (mapc
     (lambda (pair)
       (if (eq (cdr pair) 'perl-mode)
           (setcdr pair 'cperl-mode)))
     (append auto-mode-alist interpreter-mode-alist)))

(defalias 'perl-mode 'cperl-mode)
(set-perl-mode-to-cperl-mode)

(use-package cperl-mode
  :mode (("\\.t$" . cperl-mode))
  :init
  (progn
    (set-perl-mode-to-cperl-mode)
    (defun my-perl-defaults ()
      (if (fboundp 'cperl-init-faces) (cperl-init-faces))
      (setq
       cperl-electric-keywords      nil
       cperl-electric-parens        nil
       cperl-auto-newline           nil
       cperl-indent-parens-as-block t
       cperl-indent-level           tab-width
       cperl-close-paren-offset     (- tab-width))
      (set-face-background 'cperl-array-face nil)
      (set-face-background 'cperl-hash-face nil))
    (setq my-perl-hook 'my-perl-defaults))
  :config
  (progn
    (add-hook 'cperl-mode-hook (lambda () (run-hooks 'my-perl-hook)) t)))

;; perltidy
;; code copied from Emacs::PDE
;; http://search.cpan.org/~yewenbin/Emacs-PDE-0.2.16/
(use-package perltidy
  :bind
  (:map cperl-mode-map
   ("C-M-\\" . perltidy-region)
   ("C-c C-q" . perltidy-subroutine)))

(provide 'my-perl)
;;; my-perl.el ends here
