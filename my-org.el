;; my-org.el

;; org-mode configuration

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(setq org-log-done t)

(setq my-org-dirs '("~/org" "~/org/projects"))

(eval-after-load "org"
  '(progn
     (define-prefix-command 'org-todo-state-map)
     (define-key org-mode-map (kbd "C-c x") 'org-todo-state-map)

     (define-key org-todo-state-map "x"
       #'(lambda nil (interactive) (org-todo "CANCELLED")))
     (define-key org-todo-state-map "d"
       #'(lambda nil (interactive) (org-todo "DONE")))
     (define-key org-todo-state-map "f"
       #'(lambda nil (interactive) (org-todo "DEFERRED")))
     (define-key org-todo-state-map "l"
       #'(lambda nil (interactive) (org-todo "DELEGATED")))
     (define-key org-todo-state-map "s"
       #'(lambda nil (interactive) (org-todo "STARTED")))
     (define-key org-todo-state-map "w"
       #'(lambda nil (interactive) (org-todo "WAITING")))

     (setq
      org-default-notes-file    "~/org/notes.org"
      org-deadline-warning-days 14
      org-reverse-note-order t
      org-fast-tag-selection-single-key (quote expert)
      org-remember-store-without-prompt t
      org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9))
      org-completion-use-ido t      ; use ido-mode
      org-outline-path-complete-in-steps nil ; targets complete w/ ido-mode
      org-refile-use-outline-path t ; use full outline paths for refile targets
      org-remember-templates
      '(("Todo" ?t "* TODO %?\n  %u" "~/org/todo.org"  "Tasks")
        ("Note" ?n "* %u %?"         "~/org/notes.org" "Notes")
        ("Appt" ?a "* APPT %?\n  SCHEDULED: %^T\n  %u"
         "~/org/appts.org" "Appointments")))))

(eval-after-load "org-agenda"
  '(progn
     (define-key org-agenda-mode-map (kbd "C-n") 'next-line)
     (define-key org-agenda-keymap   (kbd "C-n") 'next-line)
     (define-key org-agenda-mode-map (kbd "C-p") 'previous-line)
     (define-key org-agenda-keymap   (kbd "C-p") 'previous-line)

     (setq
      org-agenda-files          my-org-dirs
      org-agenda-include-diary  t
      org-agenda-ndays          14
      org-agenda-show-all-dates t
      org-agenda-skip-deadline-if-done  t
      org-agenda-skip-scheduled-if-done t
      org-agenda-start-on-weekday       nil
      org-agenda-custom-commands
      '(("d" todo "DELEGATED" nil)
        ("c" todo "DONE|DEFERRED|CANCELLED" nil)
        ("w" todo "WAITING" nil)
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
          (org-agenda-overriding-header "Unscheduled TODO entries: ")))))))

(provide 'my-org)
;; end my-org.el
