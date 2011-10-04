;; my-perl.el

;; load my cperl-mode
(load-library (concat elisp-dir "git/cperl-mode/cperl-mode.elc"))
(require 'cperl-mode)

;; cperl-mode
(add-to-list 'auto-mode-alist '("\\.t$" . cperl-mode))
(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'perl-mode)
       (setcdr pair 'cperl-mode)))
 (append auto-mode-alist interpreter-mode-alist))

;; configure to taste
(eval-after-load 'cperl-mode
  (if (fboundp 'cperl-init-faces) (cperl-init-faces)))
(setq
 cperl-electric-keywords      nil
 cperl-electric-parens        nil
 cperl-auto-newline           nil
 cperl-indent-parens-as-block t
 cperl-indent-level           tab-width
 cperl-close-paren-offset     (- tab-width))

;; perltidy
;; code copied from Emacs::PDE
;; http://search.cpan.org/~yewenbin/Emacs-PDE-0.2.16/
(require 'perltidy)

(define-key cperl-mode-map (kbd "C-M-\\") 'perltidy-region)
(define-key cperl-mode-map (kbd "C-c C-q") 'perltidy-subroutine)

(provide 'my-perl)
;; end my-perl.el
