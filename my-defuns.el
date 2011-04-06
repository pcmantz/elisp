;; my-defuns.el

;; helpful functions for every day usage of perl

(defun msg-buffer-filename () (interactive)
  (message (buffer-file-name)))

(defun untabify-buffer () (interactive)
  (untabify (point-min) (point-max)))

(defun fullscreen () (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(provide 'my-defuns)
;; end my-defuns.el
