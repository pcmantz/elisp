;;; vtermux.el --- Define multiple vterm-based interactive programs -*- lexical-binding: t; -*-

;; Author: Paul C. Mantz
;; Keywords: terminals, processes
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (vterm "0.0") (project "0.3.0"))

;;; Commentary:
;; Provides `vtermux-define', a macro to declaratively define
;; project-scoped vterm instances for arbitrary CLI/TUI programs.
;; Think "tmux for Emacs" — each program gets its own buffer family,
;; scoped to project root, with automatic label management.
;;
;; Basic usage:
;;
;;   (require 'vtermux)
;;
;;   (vtermux-define claude                     ; M-x claude / claude-next / etc.
;;     :program "claude")
;;
;;   (vtermux-define btop)                      ; program defaults to symbol name
;;
;;   (vtermux-define opencode                   ; with arguments
;;     :program "opencode"
;;     :args "-m")
;;
;; Each definition generates the following commands:
;;   NAME        – launch an instance in the current project.
;;                 If no instance exists, creates one. If any exist,
;;                 prompts for a label (defaults to first unused number).
;;   NAME-new    – always create a new instance; always prompts for label.
;;   NAME-select – pick any live instance via completing-read.
;;   NAME-next   – cycle forward through instances in the current project.
;;   NAME-prev   – cycle backward through instances in the current project.
;;
;; Buffer naming:
;;   *<bufname> - <project-root>*              — unnamed instance
;;   *<bufname> - <project-root> (<label>)*    — labeled instance
;;
;; The label prompt defaults to the next unused integer when labels
;; follow the "1", "2", "3"… convention, matching tmux behavior.
;; You can always enter a custom label instead.
;;
;; When a program's process exits, the buffer is automatically killed
;; unless `vtermux-kill-buffer-on-exit' is nil.

;;; Code:
(require 'cl-lib)
(require 'vterm)
(require 'project)

(defgroup vtermux nil
  "Manage multiple vterm-based application instances."
  :group 'vterm)

(defcustom vtermux-kill-buffer-on-exit t
  "Non-nil kills the buffer when the underlying process exits."
  :type 'boolean
  :group 'vtermux)

(defun vtermux--project-root ()
  "Get the project root for the current directory."
  (project-root
   (or (project-current) `(transient . ,default-directory))))

(defun vtermux--next-label (buffers)
  "Return the first unused positive integer label for BUFFERS.
Extracts numeric labels from buffer names matching the vtermux
`(NUMBER)*' suffix and returns the smallest missing number as a string."
  (let (nums)
    (dolist (buf buffers)
      (when (and (buffer-live-p buf)
                 (string-match " (\\([0-9]+\\))\\*\\'" (buffer-name buf)))
        (push (string-to-number (match-string 1 (buffer-name buf))) nums)))
    (setq nums (sort (cl-delete-duplicates nums :test #'=) #'<))
    (number-to-string
     (catch 'next
       (let ((i 1))
         (dolist (n nums)
           (when (/= n i) (throw 'next i))
           (setq i (1+ n)))
         i)))))

;;;###autoload
(defmacro vtermux-define (name &rest args)
  "Define a vtermux application NAME.
NAME is a symbol used as the prefix for all generated functions.

Generated commands:
  NAME        – launch an instance (prompts for label when one exists)
  NAME-new    – always create a new instance with label prompt
  NAME-select – pick a live instance via completing-read
  NAME-next   – cycle forward through current project instances
  NAME-prev   – cycle backward through current project instances

Keyword arguments:
  :program STRING    - executable to run (default: (symbol-name NAME))
  :buffer-name STRING - base buffer name (default: (symbol-name NAME))
  :args STRING       - command line arguments string (default: nil)"
  (declare (indent 1))
  (let* ((prog (or (plist-get args :program) (symbol-name name)))
         (bufname (or (plist-get args :buffer-name) (symbol-name name)))
         (cmd-args (plist-get args :args))
         (prog-var (intern (format "%s-program" name)))
         (bufname-var (intern (format "%s-buffer-name" name)))
         (args-var (intern (format "%s-args" name)))
         (buf-list-var (intern (format "%s-buffer-list" name)))
         (fn (intern (symbol-name name)))
         (fn-new (intern (format "%s-new" name)))
         (fn-select (intern (format "%s-select" name)))
         (fn-next (intern (format "%s-next" name)))
         (fn-prev (intern (format "%s-prev" name)))
         (fn--fmt-name (intern (format "%s--format-buffer-name" name)))
         (fn--proj-bufs (intern (format "%s--project-buffers" name)))
         (fn--create (intern (format "%s--create-buffer" name)))
         (fn--handle-close (intern (format "%s--handle-close" name)))
         (fn--kill-hook (intern (format "%s--kill-buffer-hook" name)))
         (fn--switch (intern (format "%s--switch" name))))
    `(progn
       (defcustom ,prog-var ,prog
         ,(format "Program to run for `%s'." name)
         :type 'string
         :group 'vtermux)
       (defcustom ,bufname-var ,bufname
         ,(format "Base buffer name for `%s'." name)
         :type 'string
         :group 'vtermux)
       (defcustom ,args-var ,cmd-args
         ,(format "Command line arguments for `%s'." name)
         :type '(choice (const :tag "None" nil) string)
         :group 'vtermux)
       (defvar ,buf-list-var nil
         ,(format "List of `%s' vterm buffers." name))

       (defun ,fn--fmt-name (project-root &optional label)
          ,(concat
            (format "Format buffer name for `%s'.\n\n" name)
            "PROJECT-ROOT is the abbreviated project directory.
         LABEL is an optional disambiguating string for multiple instances.")
         (let ((root (abbreviate-file-name project-root)))
           (if label
               (format "*%s - %s (%s)*" ,bufname-var root label)
             (format "*%s - %s*" ,bufname-var root))))

       (defun ,fn--proj-bufs (&optional project-root)
         ,(format "Return live `%s' buffers for PROJECT-ROOT." name)
         (let* ((root (abbreviate-file-name
                       (or project-root (vtermux--project-root))))
                (prefix (format "*%s - %s" ,bufname-var root)))
           (cl-remove-if-not
            (lambda (buf)
              (and (buffer-live-p buf)
                   (string-prefix-p prefix (buffer-name buf))))
            ,buf-list-var)))

       (defun ,fn--create (project-root &optional label)
         ,(format "Create a `%s' vterm buffer at PROJECT-ROOT." name)
         (let* ((name (,fn--fmt-name project-root label))
                (default-directory project-root)
                (vterm-shell (if ,args-var
                                 (format "%s %s" ,prog-var ,args-var)
                               ,prog-var))
                (buffer (generate-new-buffer name)))
           (with-current-buffer buffer
             (vterm-mode)
             (,fn--handle-close)
             (add-hook 'kill-buffer-hook #',fn--kill-hook nil t))
           (setq ,buf-list-var
                 (nconc ,buf-list-var (list buffer)))
           buffer))

       (defun ,fn--handle-close ()
          ,(concat
            (format "Kill buffer when the `%s' process exits.\n\n" name)
            "Only acts when `vtermux-kill-buffer-on-exit' is non-nil.")
         (when-let* ((proc (get-buffer-process (current-buffer))))
           (set-process-sentinel
            proc
            (lambda (proc change)
              (when (and vtermux-kill-buffer-on-exit
                         (string-match "\\(finished\\|exited\\)" change))
                (kill-buffer (process-buffer proc)))))))

       (defun ,fn--kill-hook ()
         ,(format "Remove current buffer from `%s' list." name)
         (setq ,buf-list-var
               (delq (current-buffer) ,buf-list-var)))

        ;;;###autoload
        (defun ,fn ()
           ,(concat
             (format "Launch %s in the current project.\n\n" prog)
             "If no instance exists for the project, create one.
          If instances exist, prompt for a label and create a new one.")
          (interactive)
          (let* ((root (vtermux--project-root))
                 (existing (,fn--proj-bufs root)))
            (if existing
                (let ((default-label (vtermux--next-label existing)))
                  (switch-to-buffer
                   (,fn--create root (read-string
                                      ,(format "Label for new %s instance: " prog)
                                      nil nil default-label))))
              (switch-to-buffer (,fn--create root)))))

        ;;;###autoload
       (defun ,fn-new ()
          ,(format "Create a new %s instance in the current project." prog)
          (interactive)
          (let* ((root (vtermux--project-root))
                 (existing (,fn--proj-bufs root))
                 (default-label (vtermux--next-label existing)))
            (switch-to-buffer
             (,fn--create root (read-string
                                ,(format "Label for new %s instance: " prog)
                                nil nil default-label)))))

       ;;;###autoload
       (defun ,fn-select ()
         ,(format "Select a %s buffer with completing-read." prog)
         (interactive)
         (let ((buffers (cl-remove-if-not #'buffer-live-p ,buf-list-var)))
           (if buffers
               (switch-to-buffer
                (completing-read ,(format "%s instance: " prog)
                                 (mapcar #'buffer-name buffers) nil t))
             (message ,(format "No %s instances running." prog)))))

       ;;;###autoload
       (defun ,fn-next (&optional offset)
         ,(format "Switch to the next %s buffer, skipping OFFSET buffers." prog)
         (interactive "P")
         (,fn--switch 'next (or offset 1)))

       ;;;###autoload
       (defun ,fn-prev (&optional offset)
         ,(format "Switch to the previous %s buffer, skipping OFFSET buffers." prog)
         (interactive "P")
         (,fn--switch 'prev (or offset 1)))

       (defun ,fn--switch (direction offset)
          ,(format "Switch DIRECTION by OFFSET in the %s buffer list." name)
          (let* ((root (vtermux--project-root))
                 (buffers (,fn--proj-bufs root)))
            (if (null buffers)
                (,fn)
              (let* ((len (length buffers))
                     (idx (cl-position (current-buffer) buffers))
                     (target (mod (if (eq direction 'next)
                                     (+ (or idx -1) offset)
                                   (- (or idx 1) offset))
                                 len)))
                (switch-to-buffer (nth target buffers))))))

       (let ((cell (assq ',name vtermux--registry)))
         (if cell
             (setcdr cell ,prog-var)
           (push (cons ',name ,prog-var) vtermux--registry)))
       ',name)))

(defvar vtermux--registry nil
  "Alist of (NAME . PROGRAM) for all defined vtermux applications.")

(provide 'vtermux)
;;; vtermux.el ends here
