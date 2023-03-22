;;; early-init.el --- pre-graphics config -*- lexical-binding: t -*-

;;; Commentary:

;; Loading Emacs as quickly as possible!

;;; Code:

;; Use elpaca for package management
(setq package-enable-at-startup nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq ring-bell-function #'ignore
      inhibit-startup-screen t)

(provide 'early-init)
;;; early-init.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
