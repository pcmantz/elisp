;; my-perl.el

;; load my cperl-mode
(load-library (concat elisp-dir "git/cperl-mode/cperl-mode.elc"))

;; cperl-mode
(add-to-list 'auto-mode-alist '("\\.t$" . cperl-mode))
(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'perl-mode)
       (setcdr pair 'cperl-mode)))
 (append auto-mode-alist interpreter-mode-alist))

;; perltidy
;; code copied from Emacs::PDE
;; http://search.cpan.org/~yewenbin/Emacs-PDE-0.2.16/
(require 'perltidy)

;; configure to taste
(defun my-perl-defaults ()
  (outline-minor-mode)
  (if (fboundp 'cperl-init-faces) (cperl-init-faces))
  (setq
   cperl-electric-keywords      nil
   cperl-electric-parens        nil
   cperl-auto-newline           nil
   cperl-indent-parens-as-block t
   cperl-indent-level           tab-width
   cperl-close-paren-offset     (- tab-width))
  (set-face-background 'cperl-array-face nil)
  (set-face-background 'cperl-hash-face nil)
  (define-key cperl-mode-map (kbd "C-M-\\") 'perltidy-region)
  (define-key cperl-mode-map (kbd "C-c C-q") 'perltidy-subroutine))

(setq my-perl-hook 'my-perl-defaults)

(add-hook 'cperl-mode-hook
          (lambda () (run-hooks 'my-perl-hook)) t)
(add-hook 'cperl-mode-hook
          (lambda ()
            (add-hook 'local-write-file-hooks
                      '(lambda ()
                         (save-excursion
                           (delete-trailing-whitespace)))))
          t)

(provide 'my-perl)
;; end my-perl.el
