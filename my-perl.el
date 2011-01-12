;; my-perl.el

;; cperl-mode
(require 'cperl-mode)
(add-to-list 'auto-mode-alist '("\\.t$" . cperl-mode))
(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'perl-mode)
       (setcdr pair 'cperl-mode)))
 (append auto-mode-alist interpreter-mode-alist))

(setq cperl-electric-keywords nil)
(setq cperl-auto-newline nil)
(setq cperl-indent-parens-as-block t
      cperl-indent-level           tab-width
      cperl-close-paren-offset     (- tab-width))

;; perltidy
;; code copied from Emacs::PDE
;; http://search.cpan.org/~yewenbin/Emacs-PDE-0.2.16/
(require 'perltidy)

(define-key cperl-mode-map (kbd "C-M-\\") 'perltidy-region)

(provide'my-perl)

