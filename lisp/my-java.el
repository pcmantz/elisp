;;; my-java.el --- Java Configuration  -*- lexical-binding: t; -*-

;;; Commentary:

(defun my-java-mode-hook ()
  (setq indent-tabs-mode nil)
  (setq tab-width 4))

(add-hook 'java-mode-hook 'my-java-mode-hook t)

(provide 'my-java)
;;; my-java.el ends here
