;; my-major-modes.el

;; grab bag file for major modes that don't warrant their own config file
;; (yet)

;;
;; programming language modes
;;

;; sql-indent
(eval-after-load "sql"
  '(load-library "sql-indent"))

;; sh-mode
(add-hook 'sh-mode-hook
          (lambda ()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (delete-trailing-whitespace)))))
          t)

;;
;; standalone applications
;;

;; browse-kill-ring
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (if (fboundp 'magit-completing-read-function)
         (setq magit-completing-read-function 'ido-completing-read))
     (add-to-list 'same-window-regexps "\\*magit: [[:ascii:]]\\*")))

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

;; multi-term
(require 'multi-term)
(setq multi-term-program "/bin/bash")
(add-hook 'term-mode-hook (lambda () (yas-minor-mode -1)))

;; browse-kill-ring
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; ack-and-a-half
(require 'ack-and-a-half)
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;;
;; markup modes
;;

;; diff-mode
(setq diff-switches "-u")
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

;; yaml-mode
(add-to-list 'auto-mode-alist '("\\.yml$"  . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(autoload 'yaml-mode "yaml-mode" "Major mode for editing YAML files." t)
(eval-after-load 'yaml-mode
  (setq yaml-indent-offset 'tab-width))

;; rnc-mode
(add-to-list 'auto-mode-alist '("\\.rnc\\'" . rnc-mode))
(autoload 'rnc-mode "rnc-mode" "Major mode for editing Relax NG Compact schema files." t)
(eval-after-load 'rnc-mode
  '(progn
     (setq rnc-indent-level 'tab-width)
     (setq rnc-mode-map (make-sparse-keymap))
     (define-key rnc-mode-map "\C-c\C-c" 'comment-region)))

;; csv-mode
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode" "Major mode for editing csv files." t)

;; markdown-mode
(add-to-list 'auto-mode-alist '("\\.md$"       . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(autoload 'markdown-mode "markdown-mode" "Major mode for editing markdown files." t)

(provide 'my-major-modes)
;; end my-major-modes.el


;;
;; Fundamental mode overrides
;;

(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer))
          t)
