;;; dir-subdir.el --- find all subdirs -*- lexical-binding: t -*-

(require 'cl)
(require 'dash)

(defun dir-subdirs-descend-p (path)
  "Given a absolute file path, determine if dir-subdirs should
  recur on this file. In particular, the path must be a directory
  and start with and alphanumeric character. Should also ignore
  revision control directories."
  (and
   (file-directory-p path)
   (string-match-p "\\`[[:alnum:]]" (file-name-nondirectory path))))

(defun dir-subdirs (path)
  "Returns a list of the subdirectories of a path."
  (let* ((pending (list (file-truename path)))
         (dirs '()))
    (while pending
      (let* ((this-dir (pop pending))
             (these-subdirs (-filter
                             #'dir-subdirs-descend-p
                             (directory-files this-dir t))))
        (push this-dir dirs)
        (setq pending (append pending these-subdirs))))
    (reverse dirs)))

(provide 'dir-subdirs)
;; end dir-subdirs.el
