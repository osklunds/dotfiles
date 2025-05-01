;;;  -*- lexical-binding: t; -*-

(require 'ol-completing-read-ivy)
;; (require 'ol-completing-read-own)
;; (require 'ol-completing-read-vertico)
(require 'ol-project)

(require 'grep)

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defun ol-dwim-use-project-root (&optional prefer-project-root)
  (let ((root (ol-project-root)))
    (cond
     ((and root prefer-project-root) root)
     ;; todo: don't have vterm here, but files aren't found if using project root
     ((cl-member major-mode '(dired-mode vterm-mode)) nil)
     (t root))))

(defun ol-can-use-rg ()
  (executable-find "rg" 'remote))

(defun ol-can-use-git ()
  (and (executable-find "git" 'remote)
       (locate-dominating-file default-directory ".git")))

(defun ol-can-use-gnu-cmd ()
  ;; Hopefully a not-so-bold assumption
  t)

;; -----------------------------------------------------------------------------
;; Find file name
;; -----------------------------------------------------------------------------

(defun ol-dwim-find-file-name (&optional prefer-project-root)
  (interactive "P")
  (if-let ((root (ol-dwim-use-project-root prefer-project-root)))
      (ol-find-file-name root "project")
    (ol-find-file-name default-directory "cwd"))
  )
(ol-define-key ol-override-map "M-q" #'ol-dwim-find-file-name)
(setq ol-switch-to-project-action #'ol-dwim-find-file-name)

;; todo: prompt for dir of double M-U
(defun ol-find-file-name (dir prompt-dir-part)
  (cl-destructuring-bind (name cmd _pred) (ol-find-file-name-method)
    (let* ((default-directory dir)
           ;; Don't use shell-command because some shells slow to start
           ;; due to bashrc, and also cleaner to skip the middle-man.
           (candidates (apply #'ol-process-lines-ignore-status cmd))
           (prompt (format "Find file name [%s %s]: " prompt-dir-part name))
           (selected (completing-read
                      prompt
                      candidates
                      nil ;; predicate
                      t ;; require-match
                      nil ;; initial-input
                      'ol-find-file-name-name
                      )))
      (find-file selected))))

(defun ol-process-lines-ignore-status (cmd &rest args)
  (if (file-remote-p default-directory)
      (apply #'ol-remote-process-lines-ignore-status cmd args)
    ;; If not remote, use process-lines-ignore-status because it seems
    ;; to be slightly faster
    (apply #'process-lines-ignore-status cmd args)))

(defun ol-remote-process-lines-ignore-status (cmd &rest args)
  "Variant of `process-lines-ignore-status' that works over tramp."
  (with-temp-buffer
    (apply #'process-file cmd nil (current-buffer) nil args)
    (split-string (buffer-substring-no-properties (point-min) (point-max))
                  "\n" t)))

(defconst ol-find-file-name-methods
  `(("rg" ("rg" "--files") ,#'ol-can-use-rg)
    ("git" ("git" "ls-files") ,#'ol-can-use-git)
    ("find" ("find" "." "-not" "(" "-path" "*.git/*" "-prune" ")") ,#'ol-can-use-gnu-cmd)))

(defun ol-find-file-name-method ()
  (cl-find-if (lambda (method) (funcall (nth 2 method))) ol-find-file-name-methods))

;; -----------------------------------------------------------------------------
;; Find file content
;; -----------------------------------------------------------------------------

(defun ol-dwim-find-file-content (&optional prefer-project-root)
  (interactive "P")
  (if-let ((root (ol-dwim-use-project-root prefer-project-root)))
      (ol-find-file-content root "project")
    (ol-find-file-content default-directory "cwd"))
  )
(ol-define-key ol-override-map "M-e" #'ol-dwim-find-file-content)

(defun ol-find-file-content (dir prompt-dir-part)
  (if (file-remote-p dir)
      ;; Don't use async for remote since it opens so many tramp connections
      (ol-sync-find-file-content dir prompt-dir-part)
    (ol-async-find-file-content dir prompt-dir-part)))

(defun ol-async-find-file-content (dir prompt-dir-part)
  (cl-destructuring-bind (name func _pred) (ol-async-find-file-content-method)
    (let* ((default-directory dir)
           (prompt (format "Find file content [%s %s]: " prompt-dir-part name)))
      (funcall func prompt))))

;; Note that functions, not commands, because the framework might have some
;; extra goodies comapred to a plain command.
(defconst ol-async-find-file-content-methods
  `(("rg" ol-ripgrep ,#'ol-can-use-rg)
    ("git" ol-git-grep ,#'ol-can-use-git)
    ("grep" ol-grep ,#'ol-can-use-gnu-cmd)))

(defun ol-async-find-file-content-method ()
  (cl-find-if (lambda (method) (funcall (nth 2 method))) ol-async-find-file-content-methods))

(defun ol-sync-find-file-content (dir prompt-dir-part)
  (cl-destructuring-bind (name cmd _pred) (ol-sync-find-file-content-method)
    (let* ((default-directory dir)
           (prompt (format "Find file content [sync %s %s]: " prompt-dir-part name))
           (pattern (read-from-minibuffer
                     prompt
                     nil ;; initial-contents
                     nil ;; keymap
                     nil ;; read
                     'ol-sync-find-file-content ;; history
                     ))
           (buffer-name (format "*ol-sync-find-file-content: %s*" pattern))
           (buffer (get-buffer-create buffer-name)))
      (shell-command (format "%s %s" cmd pattern) buffer)
      (with-current-buffer buffer
        (setq default-directory dir)
        (grep-mode))
      (switch-to-buffer-other-window buffer))))

(defconst ol-sync-find-file-content-methods
  `(("rg" "rg --no-heading --line-number --with-filename" ,#'ol-can-use-rg)
    ("git" "git --no-pager grep" ,#'ol-can-use-git)
    ("grep" "grep -n -I -r" ,#'ol-can-use-gnu-cmd)))

(defun ol-sync-find-file-content-method ()
  (cl-find-if (lambda (method) (funcall (nth 2 method))) ol-sync-find-file-content-methods))

;; -----------------------------------------------------------------------------
;; Shell command
;; -----------------------------------------------------------------------------

;; for file name: --no-ignore is relevant
;; for file content: --ignore and globs and filetypes are relevant

;; Useful examples (todo: make them templates you can complete-read)
;; rg --files --no-ignore -- file name filtered
;; rg --no-ignore -g '*.el'
;; find . -name '*.el'

(defun ol-dwim-shell-command (&optional prefer-project-root)
  (interactive "P")
  (if-let ((root (ol-dwim-use-project-root prefer-project-root)))
      (ol-shell-command root "project")
    (ol-shell-command default-directory "cwd"))
  )
(ol-define-key ol-normal-leader-map "m c" #'ol-dwim-shell-command)

(defun ol-shell-command (dir prompt-dir-part)
  (let* ((default-directory dir)
         (prompt (format "Shell command [%s]: " prompt-dir-part))
         (selection (ol-completing-read-shell-command
                     prompt
                     'ol-shell-command)))
    (ol-shell-command-open-selection selection)))

(defun ol-shell-command-open-selection (selection)
  (cond
   ;; example: .emacs_config/config/ol-file.el:44:(defun ol-save-silently ()
   ((string-match "\\(.*\\):\\([0-9]+\\):" selection)
    (let* ((file (match-string 1 selection))
           (line (match-string 2 selection)))
      (unless (file-exists-p file)
        (user-error "No such file"))
      (find-file file)
      (goto-char (point-min))
      (forward-line (string-to-number line))))
   (t
    (unless (file-exists-p selection)
      (user-error "No such file"))
    (find-file selection))))

(provide 'ol-completing-read)
