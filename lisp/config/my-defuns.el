;; my-defuns.el -- helpful functions for every-day usage  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;
;; Interactive functions
;;

(defun msg-buffer-filename ()
  "Prints the file name of the buffer to the minibar."
  (interactive)
  (message (buffer-file-name)))

(defun untabify-buffer ()
  "Untabify the entire buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun fullscreen ()
  "Fullscreen from within Emacs, ignoring UI interaction."
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(defun dos2unix (buffer)
  "Automate M-% C-q C-m RET C-q C-j RET ."
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match (string ?\C-j) nil t))))

(defun reopen-this-file-as-sudo ()
  "Reopen the current buffer with sudo via tramp."
  (interactive)
  (when-let* ((p (point))
              (file-name (buffer-file-name)))
    (find-alternate-file (concat "/sudo::" file-name))
    (goto-char p)))

;;
;; elisp utils
;;

(defun get-string-from-file (file-path)
  "Return file-path's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun range (first-arg &optional last-arg)
  "Super-naïve implementation of a range function.  Update this
to do this in reverse."
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
