;; my-org.el ---  org-mode configuration

;;; Commentary:

;;; Code:

(defvar my-org-todo-active-statuses
  '(("TODO" . "t") ("STARTED" . "s") ("BLOCKED" . "b"))
  "List of pairs of active statuses and transition key.")

(defvar my-org-todo-finished-statuses
  '(("CANCELLED" . "C") ("DONE" . "d"))
  "List of pairs of finished statuses and transition key.")

(defvar my-org-todo-all-statuses
  (append my-org-todo-active-statuses my-org-todo-finished-statuses)
  "List of all statuses and keys.")

(use-package org
  :demand t
  :mode (("\\.org$" . org-mode))
  :bind-keymap
  ("C-c t" . org-todo-state-map)
  :bind
  (("C-c l" . org-store-link)
   :map org-todo-state-map
   ("t" . (lambda nil (interactive) (org-todo "TODO")))
   ("s" . (lambda nil (interactive) (org-todo "STARTED")))
   ("b" . (lambda nil (interactive) (org-todo "BLOCKED")))
   ("C" . (lambda nil (interactive) (org-todo "CANCELLED")))
   ("d" . (lambda nil (interactive) (org-todo "DONE"))))
  :custom
  (org-log-done 'note)
  (org-replace-disputed-keys t)
  (org-startup-folded nil)
  (org-todo-keywords '((sequence "TODO" "STARTED" "BLOCKED")
                       (sequence "DONE" "CANCELLED")))
  (org-deadline-warning-days 14)
  (org-refile-use-outline-path t) ; use full outline paths for refile targets
  (org-refile-targets '((nil :maxlevel . 9)
                        (org-agenda-files :maxlevel . 9)))
  :init
  (defvar org-todo-state-map (make-sparse-keymap))
  (dolist
    (key-action '(("t" . (lambda nil (interactive) (org-todo "TODO")))
                  ("s" . (lambda nil (interactive) (org-todo "STARTED")))
                  ("b" . (lambda nil (interactive) (org-todo "BLOCKED")))
                  ("C" . (lambda nil (interactive) (org-todo "CANCELLED")))
                  ("d" . (lambda nil (interactive) (org-todo "DONE")))))
    (define-key org-todo-state-map (car key-action) (cdr key-action)))

  :config
  ;; Make windmove work in org-mode:
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right))

(use-package ob
  :elpaca nil
  :after (org)
  :config
  ;; active Org-babel languages
  (org-babel-do-load-languages
    'org-babel-load-languages
    '(;; other Babel languages
       (plantuml . t))))

(use-package doct
  :ensure t
  :commands (doct))

(use-package org-capture
  :elpaca nil
  :after (org)
  :bind ("<f2>" . org-capture)
  :custom
  (org-capture-templates
    (doct
      '(("Journal"
         :keys "j"
         :function (lambda () (org-reload) (org-journal-new-entry t) (goto-char (point-min)))
         :children ((:group "Status"
                     :template ("* %(format-time-string org-journal-time-format) %{status-header}"
                                 ":PROPERTIES:"
                                 ":Mood: %^{Mood}p"
                                 ":END:\n"
                                 "%?")
                     :children (("Daily Check-In" :keys "c" :status-header "Daily Check-In")
                                ("Daily Check-Out" :keys "o" :status-header "Daily Check-Out")))
                    ("Todo" :keys "t" :template ("* TODO %?" "%i" "%T"))
                    ("Appointment" :keys "a" :template ("* APPT %?" "SCHEDULED %^T" "%u" "%T"))
                    ("Source Block"
                     :keys "s"
                     :template ("* %^{Title}\n"
                                "#+begin_src %?"
                                "%i"
                                "#+end_src"))
                    ("Journal Entry"
                     :keys "e"
                     :template ("* %(format-time-string org-journal-time-format) %^{Title}\n"
                                 "%i%?"))))
        ("Recipes"
         :keys "r"
         :file "~/Documents/recipes/recipes.org"
         :children (("Capture"
                     :keys "c"
                     :template ("%(org-chef-get-recipe-from-url)"))
                    ("Manual"
                      :keys "m"
                      :template ("* %^{Recipe Title}"
                                 ":PROPERTIES:"
                                 ":source-url:"
                                 ":servings:"
                                 ":prep-time:"
                                 ":cook-time:"
                                 ":ready-in:"
                                 ":END:\n"
                                 "** Ingredients\n"
                                 "%?"
                                 "** Directions\n\n"))))))))

(use-package org-chef)

(use-package org-journal
  :demand t
  :after (org)
  :custom
  (org-journal-enable-agenda-integration t)
  (org-journal-file-format "%Y/%m/%Y-%m-%d.org")
  (org-journal-date-format "%A, %b  %d, %Y")
  (org-journal-carryover-items
    (s-join "|"
      (-map (lambda (status)
              (s-concat "TODO=\"" (s-upcase (car status)) "\""))
        my-org-todo-active-statuses))))

(use-package org-agenda
  :elpaca nil
  :bind
  (("C-c a" . org-agenda)
    :map org-agenda-mode-map
    ("C-n" . next-line)
    ("C-p" . previous-line))
  :custom
  (org-agenda-span 14)
  (org-agenda-show-all-dates t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-start-on-weekday nil)
  (org-agenda-custom-commands
    '(("c" todo "DONE|CANCELLED" nil)
      ("x" todo "TODO|STARTED|BLOCKED" nil)
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
  (add-to-list 'same-window-regexps '("*Org Agenda*". nil)))

(use-package ox-pandoc)

(provide 'my-org)
;;; my-org.el ends here
