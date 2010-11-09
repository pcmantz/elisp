
(require 'eproject)

(define-project-type perl (generic)
  (or (look-for "Makefile.PL") (look-for "Build.PL"))
  :relevant-files ("\\.pm$" "\\.t$" "\\.pl$" "\\.PL$")
  :irrelevant-files ("inc/" "blib/" "cover_db/")
  :mxdeclare-project-p (lambda (root)
                         (file-exists-p
                          (concat root
                                  ".mxdeclare_project")))
  :file-name-map (lambda (root)
                   (lambda (root file)
                     (cond ((string-match "^lib/\\(.+\\)[.]pm$" file)
                            (let ((m (match-string 1 file)))
                              (while (string-match "/" m)
                                (setf m (replace-match "::" nil nil m)))
                              m))
                           (t file))))
  :main-file "Makefile.PL")

(defun cperl-mxdeclare-project-p ()
  "Determine if this project should use MooseX::Declare class definitions."
  (ignore-errors
    (eproject-attribute :is-mxdeclare-project)))
