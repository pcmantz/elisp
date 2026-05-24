;;; my-appearance.el --- Appearance, theme, and font configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; colors! the colors!
(use-package font-lock
  :demand t
  :ensure nil
  :custom
  (font-lock-maximum-decoration t)
  :config
  (global-font-lock-mode t))

;; graphical config

(defun enable-fira-code-ligatures ()
  "This function enables the ligatures found in the Fira Code font."
  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                 (36 . ".\\(?:>\\)")
                 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                 (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                 (48 . ".\\(?:x[a-zA-Z]\\)")
                 (58 . ".\\(?:::\\|[:=]\\)")
                 (59 . ".\\(?:;;\\|;\\)")
                 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                 (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                 (91 . ".\\(?:]\\)")
                 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                 (94 . ".\\(?:=\\)")
                 (119 . ".\\(?:ww\\)")
                 (123 . ".\\(?:-\\)")
                 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)"))))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring])))))

(use-package metalheart-theme
  :ensure t
  :if (display-graphic-p)
  :config
  (load-theme 'metalheart)
  (set-cursor-color "Purple"))

(when (display-graphic-p)
  (blink-cursor-mode -1)
  (mouse-wheel-mode t)
  (xterm-mouse-mode t)
  (cond ((string= window-system "x")   ;; X11 window system
         (set-face-attribute 'default nil :font "Inconsolata-9")
         (setq select-enable-clipboard t
               interprogram-paste-function 'x-cut-buffer-or-selection-value))
        ((string= window-system "ns")  ;; Apple OS X
         (set-face-attribute 'default nil :font "Inconsolata-11"))
        ((string= window-system "w32") ;; MS-Windows
         (set-face-attribute 'default nil :font "Consolas-9"))
        (t nil)))

;; all-the-icons
(use-package all-the-icons
  :ensure t
  :demand t
  :defer 0.5)

(use-package all-the-icons-dired
  :ensure t
  :delight
  :init
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

(provide 'my-appearance)
;;; my-appearance.el ends here
