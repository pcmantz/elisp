;; my-defuns.el

;; helpful functions for every day usage of perl

(defun msg-buffer-filename () (interactive)
  (message (buffer-file-name)))

(defun untabify-buffer () (interactive)
  (untabify (point-min) (point-max)))

(provide 'my-defuns)