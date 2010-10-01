;;; init.el --- config script for elisp packages

(require 'cl)

;;
;; elisp mode configuration
;;

;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

(setq yaml-indent-offset 4)

;; haskell-mode
(require 'haskell-mode)
(require 'inf-haskell)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

(autoload 'turn-on-haskell-doc-mode "haskell-doc" nil t)

(add-to-list 'auto-mode-alist '("\\.hs" . haskell-mode))

;; magit
(require 'magit)

(global-set-key (kbd "C-x g") 'magit-status)
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))


;; icicles
(require 'icicles)
(icy-mode t)

;; multi-term
(require 'multi-term)
(setq multi-term-program "/bin/bash")

;; perltidy
;; code copied from Emacs::PDE
;; http://search.cpan.org/~yewenbin/Emacs-PDE-0.2.16/
(require 'perltidy)

(define-key cperl-mode-map (kbd "C-M-\\") 'perltidy-region)

;; yasnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/elisp/yasnippet/snippets")
(yas/load-directory "~/elisp/snippets")
(setq yas/global-mode t)

;; hippie-expand
(require 'hippie-exp)

(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-lisp-symbol))

(global-set-key (kbd "M-/") 'hippie-expand)

;; rnc-mode
(require 'rnc-mode)
(setq auto-mode-alist
      (cons '("\\.rnc\\'" . rnc-mode) auto-mode-alist))

;; ibuffer-git (not yet customized)
(require 'ibuffer-git)

;; eproject
(require 'eproject)
(require 'eproject-extras)

(define-project-type perl (generic)
  (or (look-for "Makefile.PL") (look-for "Build.PL"))
  :relevant-files ("\\.pm$" "\\.t$" "\\.pl$" "\\.PL$")
  :irrelevant-files ("inc/" "blib/" "cover_db/")
  :mxdeclare-project-p (lambda (root)
                         (file-exists-p
                          (concat root
                                  ".mxdeclare_project")))
  :file-name-map (lambda (root)
                   (lambda (root file)
                     (cond ((string-match "^lib/\\(.+\\)[.]pm$" file)
                            (let ((m (match-string 1 file)))
                              (while (string-match "/" m)
                                (setf m (replace-match "::" nil nil m)))
                              m))
                           (t file))))
  :main-file "Makefile.PL")

(defun cperl-mxdeclare-project-p ()
  "Determine if this project should use MooseX::Declare class definitions."
  (ignore-errors
    (eproject-attribute :is-mxdeclare-project)))

;; browse-kill-ring
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; mode-compile.el
(require 'mode-compile)

(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key (kbd "C-c c") 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key (kbd "C-c k") 'mode-compile-kill)
