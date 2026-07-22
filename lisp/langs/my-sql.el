;;; my-sql.el --- Configuration for editing SQL -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(use-package sql
  :ensure nil)

(add-hook 'sql-interactive-mode-hook
          (lambda () (toggle-truncate-lines t)))

(use-package sql-indent)

;; (rx (and line-start "*SQL" (0+ anything)  "*"))
(add-to-list 'same-window-regexps '("^\\*SQL[^z-a]*\\*" . nil))

(provide 'my-sql)

;;; my-sql.el ends here
