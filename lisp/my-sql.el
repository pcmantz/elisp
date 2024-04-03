;;; my-sql.el --- Configuration for editing SQL -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(use-package sql
  :ensure nil)

(add-hook 'sql-interactive-mode-hook
          (lambda () (toggle-truncate-lines t)))

(use-package sql-indent)

(use-package sqlformat
  :custom
  (sqlformat-command 'pgformatter)
  ;;; NOTE: The "-p" option specifies ignored areas from formatting and this will be mode-dependent
  ;;; for embedded SQL strings. The one configured here is intended for interpolated ruby code for
  ;;; SQL embedded in double-quoted/HEREdoc strings.
  (sqlformat-args '("-s" "2" "-u" "2" "-p" "'\\#\\{[^}]*\\}'")))

;; (rx (and line-start "*SQL" (0+ anything)  "*"))
(add-to-list 'same-window-regexps '("^\\*SQL[^z-a]*\\*" . nil))

(provide 'my-sql)

;;; my-sql.el ends here
