;; my-misc-major-modes --- Miscellaneous major modes  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package plantuml-mode
  :mode
  (("\\.(?:p(?:lant))uml$" . plantuml-mode))
  :custom
  (plantuml-executable-path "/usr/local/bin/plantuml")
  (plantuml-default-exec-mode 'executable))

(use-package flycheck-plantuml)

(use-package web-mode
  :mode
  (("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.hbs\\'". web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.p?html\\'" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-engines-alist '(("php" . "\\.phtml\\'")
                            ("blade" . "\\.blade\\.")
                            ("ctemplate" . "\\.hbs\\."))))

(use-package sh-script
  :ensure nil
  :custom
  (sh-basic-offset 2))

(use-package php-mode)

(provide 'my-misc-major-modes)
;;; my-web.el ends here
