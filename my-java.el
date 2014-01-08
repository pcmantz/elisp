;; my-java.el

(defun my-java-mode-hook ()
  (setq indent-tabs-mode nil)
  (setq tab-width 4))

(add-hook 'java-mode-hook 'my-java-mode-hook)

