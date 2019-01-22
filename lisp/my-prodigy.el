;; my-prodigy --- Config for Prodigy.el

;;; Commentary

(use-package prodigy
  :config
  (progn
    (prodigy-define-tag
      :name 'thin
      :ready-message "Listening on 0\\.0\\.0\\.0:[0-9]+, CTRL\\+C to stop")

    (prodigy-define-tag
      :name 'webrick
      :ready-message "WEBrick::HTTPServer#start: pid=[0-9]+ port=[0-9]+")

    (prodigy-define-tag
      :name 'mongrel
      :ready-message "Ctrl-C to shutdown server")

    (prodigy-define-tag
      :name 'rails
      :tags '(thin mongrel webrick))))

(provide 'my-prodigy)
;;; my-prodigy.el ends here
