;;; my-backups.el --- Backup file configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package files
  :ensure nil
  :demand t
  :custom
  (backup-by-copying t)
  (version-control t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (delete-old-versions t)
  (backup-directory-alist
   `(("." . ,(expand-file-name (concat user-emacs-directory "backups"))))))

(use-package vc-hooks
  :demand t
  :ensure nil
  :custom
  (vc-make-backup-files t))

(provide 'my-backups)
;;; my-backups.el ends here
