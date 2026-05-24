;; my-vtermux --- Configurations for vtermux  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package vtermux
  :ensure nil
  :config
  (vtermux-define btop)
  (vtermux-define claude)
  (vtermux-define opencode :args "-m")
  (vtermux-define pitchfork :args "tui"))

(provide 'my-vtermux)

;;; my-vtermux.el ends here
