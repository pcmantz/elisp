;; my-perl.el

;; cperl-mode
(require 'cperl-mode)
(add-to-list 'auto-mode-alist '("\\.t$" . cperl-mode))
(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'perl-mode)
       (setcdr pair 'cperl-mode)))
 (append auto-mode-alist interpreter-mode-alist))

;; configure to taste
(eval-after-load 'cperl-mode
  '(progn
     ((setq
       cperl-electric-keywords      nil
       cperl-electric-parens        nil
       cperl-auto-newline           nil
       cperl-indent-parens-as-block t
       cperl-indent-level           tab-width
       cperl-close-paren-offset     (- tab-width))
      (if (fboundp cperl-init-faces) (cperl-init-faces)))))

;; perltidy
;; code copied from Emacs::PDE
;; http://search.cpan.org/~yewenbin/Emacs-PDE-0.2.16/
(require 'perltidy)

(define-key cperl-mode-map (kbd "C-M-\\") 'perltidy-region)

(provide'my-perl)
;; end my-perl.el
