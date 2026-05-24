;;; my-mcp.el --- MCP server configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package mcp-server
  :elpaca (mcp-server :host github :repo "rhblind/emacs-mcp-server"
                      :files ("*.el" "tools/*.el" "mcp-wrapper.py" "mcp-wrapper.sh"))
  :demand t
  :config
  (mcp-server-start-unix))

(provide 'my-mcp)
;;; my-mcp.el ends here
