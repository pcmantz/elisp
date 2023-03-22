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
  :elpaca nil
  :mode (("\\.t$" . cperl-mode))
  :custom
  (cperl-electric-keywords nil)
  (cperl-electric-parens nil)
  (cperl-auto-newline nil)
  (cperl-indent-parens-as-block t)
  (cperl-indent-level tab-width)
  (cperl-close-paren-offset (- tab-width))
  :init
  (set-perl-mode-to-cperl-mode)
  :config
  (set-face-background 'cperl-array-face nil)
  (set-face-background 'cperl-hash-face nil))

;; perltidy
;; code copied from Emacs::PDE
;; http://search.cpan.org/~yewenbin/Emacs-PDE-0.2.16/
(use-package perltidy
  :elpaca nil
  :bind
  (:map cperl-mode-map
   ("C-M-\\" . perltidy-region)
   ("C-c C-q" . perltidy-subroutine)))

(use-package tt-mode)

(provide 'my-perl)
;;; my-perl.el ends here
