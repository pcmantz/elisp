;; my-formatters -- external programs for formatting code -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

(use-package reformatter
  :config
  (reformatter-define sleek
    :program "sleek"
    :lighter " S"
    :args '())

  (reformatter-define taplo
    :program "taplo"
    :lighter " T"
    :args '("fmt" "-"))

  (reformatter-define yq
    :program "yq"
    :lighter "yq"
    :args '("-P"))

  (reformatter-define prettier-js
    :program "prettier"
    :lighter " P"
    :args '("--parser" "babel"))

  (reformatter-define prettier-css
    :program "prettier"
    :lighter " P"
    :args '("--parser" "scss"))


  (reformatter-define prettier-html
    :program "prettier"
    :lighter " P"
    :args '("--parser" "html"))

  (reformatter-define prettier-json
    :program "prettier"
    :lighter " P"
    :args '("--parser" "json"))

  (reformatter-define prettier-md
    :program "prettier"
    :lighter " P"
    :args '("--parser" "markdown"))

  (reformatter-define prettier-graphql
    :program "prettier"
    :lighter " P"
    :args '("--parser" "graphql")))

(provide 'my-formatters)
;;; my-formatters ends here
