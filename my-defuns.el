;; my-defuns.el -- helpful functions for every-day usage

;;
;; Interactive functions
;;

(defun msg-buffer-filename () (interactive)
  (message (buffer-file-name)))

(defun untabify-buffer () (interactive)
  (untabify (point-min) (point-max)))

(defun fullscreen () (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

;;
;; elisp utils
;;

(defun add-to-load-path (path)
  (add-to-list 'load-path path))

(defun range (first-arg &optional last-arg)
  "Super-naïve implementation of a range function.  Update this
do to this in reverse."
  (let* ((start (if (null last-arg) 0         first-arg))
         (end   (if (null last-arg) first-arg last-arg))
         (iter start)
         (range-list '()))
    (while (<  iter end)
      (add-to-list 'range-list iter 1)
      (setq iter (+ iter 1)))
    range-list))

(provide 'my-defuns)
;; end my-defuns.el
