;; ido-escreen.el --- use ido-mode with escreen

(defun ido-escreen-goto-screen (&optional dont-update-current)
  (interactive)
  (run-hooks 'escreen-goto-screen-before-hook)
  (let* ((screen-list (sort (escreen-configuration-screen-numbers) '<))
         (number (string-to-int (ido-completing-read 
                                 "Go to escreen number: "
                                 (mapcar 'number-to-string
                                         screen-list)
                                 nil t)))
         (screen-data (escreen-configuration-escreen number)))
    (or screen-data
        (error "escreen: %d: invalid screen number." number))
    (or dont-update-current
        (escreen-save-current-screen-configuration))
    (escreen-restore-screen-map screen-data)
    (setq escreen-current-screen-string (int-to-string number))
    (or dont-update-current
        (setq escreen-last-screen-number escreen-current-screen-number))
    (setq escreen-current-screen-number number))
  (run-hooks 'escreen-goto-screen-hook))

(provide 'ido-escreen)
;; escreen-ido.el ends here