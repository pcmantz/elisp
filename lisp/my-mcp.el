;;; my-mcp.el --- MCP server configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package mcp-server
  :demand t
  :elpaca (mcp-server :host github :repo "rhblind/emacs-mcp-server"
                      :files ("*.el" "tools/*.el" "mcp-wrapper.py" "mcp-wrapper.sh"))
  :init
  (add-hook 'emacs-startup-hook #'mcp-server-start-unix))

(provide 'my-mcp)
;;; my-mcp.el ends here
