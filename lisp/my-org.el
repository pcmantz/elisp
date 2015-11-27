;; my-org.el
;; org-mode configuration

(require 'dir-subdirs)

(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "<f2>") 'org-capture)

(setq my-org-dirs (dir-subdirs "~/org"))

(define-prefix-command 'org-todo-state-map)
(define-key org-todo-state-map "t"
  (lambda nil (interactive) (org-todo "TODO")))
(define-key org-todo-state-map "s"
  (lambda nil (interactive) (org-todo "STARTED")))
(define-key org-todo-state-map "w"
  (lambda nil (interactive) (org-todo "WAITING")))
(define-key org-todo-state-map "l"
  (lambda nil (interactive) (org-todo "DELEGATED")))
(define-key org-todo-state-map "d"
  (lambda nil (interactive) (org-todo "DONE")))
(define-key org-todo-state-map "C"
  (lambda nil (interactive) (org-todo "CANCELLED")))
(define-key org-todo-state-map "D"
  (lambda nil (interactive) (org-todo "DEFERRED")))
(define-key org-todo-state-map "a"
  (lambda nil (interactive) (org-todo "APPT")))
(define-key org-todo-state-map "f"
  (lambda nil (interactive) (org-todo "FINISHED")))

(eval-after-load 'org
  '(progn
     (define-key org-mode-map (kbd "C-c t") 'org-todo-state-map)

     (setq
      ;; keybindings
      org-replace-disputed-keys t

      ;; Todo transition
      org-todo-keywords '((sequence "TODO" "STARTED" "WAITING" "DELEGATED" "|" "DONE")
                          (sequence "|" "CANCELLED")
                          (sequence "|" "DEFERRED")
                          (sequence "APPT" "|" "FINISHED"))
      org-deadline-warning-days 14
      org-fast-tag-selection-single-key 'expert

      ;; refile configuration
      org-refile-use-outline-path t ; use full outline paths for refile targets
      org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9))

      ;; org-capture config
      org-default-notes-file "~/org/incoming.org"
      org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/incoming.org" "Todos")
         "* TODO %?\n  %i\n  %a")
        ("n" "Note" entry (file+headline "~/org/incoming.org" "Notes")
         "* %u %?")
        ("a" "Appointment" entry (file+headline "~/org/incoming.org" "Appointments")
         "* APPT %?\n SCHEDULED %^T\n %u")))

     ;; Make windmove work in org-mode:
     (add-hook 'org-shiftup-final-hook 'windmove-up)
     (add-hook 'org-shiftleft-final-hook 'windmove-left)
     (add-hook 'org-shiftdown-final-hook 'windmove-down)
     (add-hook 'org-shiftright-final-hook 'windmove-right)))

(eval-after-load 'org-agenda
  '(progn
     ;; TODO: decide if I even need these
     (define-key org-agenda-mode-map (kbd "C-n") 'next-line)
     (define-key org-agenda-mode-map (kbd "C-p") 'previous-line)

     (setq
      org-agenda-include-diary  t
      org-agenda-ndays          14
      org-agenda-show-all-dates t
      org-agenda-skip-deadline-if-done  t
      org-agenda-skip-scheduled-if-done t
      org-agenda-start-on-weekday       nil
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
          (org-agenda-overriding-header "Unscheduled TODO entries: ")))))))

(require 'org-agenda)

(defun reload-org-dirs ()
  "Reloads the org directories."
  (interactive)
  (setq my-org-dirs (dir-subdirs "~/org"))
  (setq org-agenda-files my-org-dirs))

(reload-org-dirs)

(provide 'my-org)
;; end my-org.el
