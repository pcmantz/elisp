;;; init.el --- config script for elisp packages

;; Copyright (C) 2010 Paul C. Mantz, all rights reserved

;;
;; helpful functions
;;
(defun rel-dir-add (path) 
  (add-to-list 'load-path (concat default-directory path)))

;;
;; elisp mode configuration
;;
  
;; yaml-mode
(rel-dir-add "yaml-mode")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(setq yaml-indent-offset 4)

;; haskell-mode
(rel-dir-add "haskell-mode")

