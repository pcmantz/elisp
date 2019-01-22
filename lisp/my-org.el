;; my-org.el ---  org-mode configuration

;;; Commentary:

;;; Code:

(use-package dir-subdirs)

(use-package org
  :mode (("\\.org$" . org-mode))
  :bind-keymap ("C-c t" . org-todo-state-map)
  :bind (("C-c l" . org-store-link))
  :init
  (progn
    (defvar org-todo-state-map (make-sparse-keymap))

    (define-key org-todo-state-map "t" (lambda nil (interactive) (org-todo "TODO")))
    (define-key org-todo-state-map "s" (lambda nil (interactive) (org-todo "STARTED")))
    (define-key org-todo-state-map "w" (lambda nil (interactive) (org-todo "WAITING")))
    (define-key org-todo-state-map "l" (lambda nil (interactive) (org-todo "DELEGATED")))
    (define-key org-todo-state-map "d" (lambda nil (interactive) (org-todo "DONE")))
    (define-key org-todo-state-map "C" (lambda nil (interactive) (org-todo "CANCELLED")))
    (define-key org-todo-state-map "D" (lambda nil (interactive) (org-todo "DEFERRED")))
    (define-key org-todo-state-map "a" (lambda nil (interactive) (org-todo "APPT")))
    (define-key org-todo-state-map "f" (lambda nil (interactive) (org-todo "FINISHED"))))
  :config
  (progn
    (defvar org-directory "~/org")
    (defvar my-org-dirs (dir-subdirs org-directory))

    (defun reload-org-dirs ()
      "Reloads the org directories."
      (interactive)
      (setq my-org-dirs (dir-subdirs org-directory))
      (setq org-agenda-files my-org-dirs))

    (setq
     ;; keybindings
     org-replace-disputed-keys t
     org-startup-folded nil

     ;; Todo transition
     org-todo-keywords '((sequence "TODO" "STARTED" "WAITING" "DELEGATED" "|" "DONE")
                         (sequence "|" "CANCELLED")
                         (sequence "|" "DEFERRED")
                         (sequence "APPT" "|" "FINISHED"))
     org-deadline-warning-days 14

     ;; refile configuration
     org-refile-use-outline-path t ; use full outline paths for refile targets
     org-refile-targets '((nil :maxlevel . 9)
                          (org-agenda-files :maxlevel . 9)))

    ;; Make windmove work in org-mode:
    (add-hook 'org-shiftup-final-hook 'windmove-up)
    (add-hook 'org-shiftleft-final-hook 'windmove-left)
    (add-hook 'org-shiftdown-final-hook 'windmove-down)
    (add-hook 'org-shiftright-final-hook 'windmove-right)

    (reload-org-dirs)))

;; TODO: Need to refactor this into the use-package hooks

;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))


(use-package org-journal
  :config
  (progn
    (setq
     org-journal-file-format "%Y/%m/%Y-%m-%d.org"
     org-journal-date-format "%A, %b  %d, %Y")))

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))

(use-package org-capture
  :bind ("<f2>" . org-capture)
  :config
  (progn
    (setq org-capture-templates
          '(("e" "Journal entry" entry (function org-journal-find-location)
             "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
            ("t" "Todo" entry (function org-journal-find-location)
             "* TODO %?\n  %i\n  %a\n  %T\n")
            ("a" "Appointment" entry (function org-journal-find-location)
             "* APPT %?\n SCHEDULED %^T\n %u\n  %T\n")))))

(use-package org-agenda
  :bind
  (("C-c a" . org-agenda)
   :map org-agenda-mode-map
   ("C-n" . next-line)
   ("C-p" . previous-line))
  :config
  (progn
    (setq
     org-agenda-include-diary  t
     org-agenda-span 14
     org-agenda-show-all-dates t
     org-agenda-skip-deadline-if-done t
     org-agenda-skip-scheduled-if-done t
     org-agenda-start-on-weekday nil
     org-agenda-custom-commands
     '(("d" todo "DELEGATED" nil)
       ("w" todo "WAITING" nil)
       ("c" todo "DONE|CANCELLED|DEFERRED|FINISHED" nil)
       ("W" agenda "" ((org-agenda-ndays 21)))
       ("A" agenda ""
        ((org-agenda-skip-function
          (lambda nil
            (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
         (org-agenda-ndays 1)
         (org-agenda-overriding-header "Today's Priority #A tasks: ")))
       ("u" alltodo ""
        ((org-agenda-skip-function
          (lambda nil
            (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
                                      (quote regexp) "<[^>\n]+>")))
         (org-agenda-overriding-header "Unscheduled TODO entries: ")))))
    (reload-org-dirs)))

(use-package ox-pandoc)

(use-package org-jira
  :config
  (progn
    (define-key org-jira-entry-mode-map (kbd "C-c i i") 'org-jira-get-issue)))

(provide 'my-org)
;;; my-org.el ends here
