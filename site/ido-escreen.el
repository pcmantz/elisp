;; ido-escreen.el --- use ido-mode with escreen

(defun ido-escreen-goto-screen ()
  (interactive)
  (let ((number
         (string-to-number (helm-comp-read
                            "Go to escreen number: "
                            (mapcar 'number-to-string screen-list)
                            nil t))))
    (escreen-goto-screen number)))

(provide 'ido-escreen)
;; escreen-ido.el ends here
