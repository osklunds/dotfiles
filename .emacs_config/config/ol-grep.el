;; -*- lexical-binding: t -*-

(require 'grep)

(require 'ol-file-name)

(setc grep-use-null-device nil)
(setc grep-use-headings t)
(setc compilation-always-kill t)
(setc compilation-message-face nil)
;; In terminal, prevent scroll of buffer when clicking result
(setc compilation-context-lines t)

;; -----------------------------------------------------------------------------
;; Commands and options
;; -----------------------------------------------------------------------------

(defconst ol-rg-command "rg --color=always --no-heading -n -H -0 -S -- ")
(defconst ol-git-grep-command "git --no-pager grep --color=always -n -- ")
(defconst ol-grep-command "grep --color=always -E -n -I -r -Z -- ")

(defconst ol-grep-commands
  `((,ol-rg-command ,#'ol-can-use-rg)
    (,ol-git-grep-command ,#'ol-can-use-git)
    (,ol-grep-command ,#'ol-can-use-gnu-cmd)))

(defun ol-select-grep-command ()
  (car (cl-find-if (lambda (method) (funcall (cadr method))) ol-grep-commands)))

;; -----------------------------------------------------------------------------
;; Grepping
;; -----------------------------------------------------------------------------

(defun ol-grep (&optional prefer-project-root)
  (interactive "P")
  (setc grep-command (ol-select-grep-command))
  (let* ((default-directory (ol-dwim-root prefer-project-root)))
    (call-interactively #'grep)))

;; -----------------------------------------------------------------------------
;; Keybinds
;; -----------------------------------------------------------------------------

(ol-define-key ol-override-map "M-e" #'ol-grep)

(ol-evil-define-key 'normal compilation-button-map "o" #'compile-goto-error)
(ol-evil-define-key 'normal compilation-mode-map "o" #'compile-goto-error)
(ol-evil-define-key 'normal grep-mode-map "o" #'compile-goto-error)
(ol-evil-define-key 'normal grep-mode-map "O" #'ol-compile-goto-error-other-window)

(defun ol-compile-goto-error-other-window ()
  (interactive)
  (ol-split-window)
  (compile-goto-error))

(ol-define-key grep-mode-map "C-x C-q" #'ol-grep-read-only-mode)

;; -----------------------------------------------------------------------------
;; imenu
;; -----------------------------------------------------------------------------

(defun ol-grep-imenu-create-index-function ()
  (goto-char (point-min))
  (let* ((res nil))
    (while (not (eobp))
      (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
             (props (text-properties-at (point)))
             (face (plist-get props 'font-lock-face)))
        (when (eq face 'grep-heading)
          (push `(,line . ,(point)) res)))
      (forward-line 1))
    (reverse res)))

(defun ol-grep-files-imenu ()
  (setq imenu-create-index-function #'ol-grep-imenu-create-index-function))

(add-hook 'grep-mode-hook #'ol-grep-files-imenu)

;; -----------------------------------------------------------------------------
;; Editable grep buffer
;; -----------------------------------------------------------------------------

(defun ol-grep-read-only-mode ()
  (interactive)
  (unless (eq major-mode 'grep-mode)
    (user-error "Only works in grep-mode"))
  (if buffer-read-only
      (progn
        (use-local-map (make-sparse-keymap))
        (local-set-key (kbd "C-x C-q") #'ol-grep-read-only-mode)
        (read-only-mode -1))
    (ol-grep-apply-changes)
    (use-local-map grep-mode-map)
    (read-only-mode t)))

(defun ol-grep-apply-changes ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when-let* ((props (text-properties-at (point)))
                  (msg (plist-get props 'compilation-message))
                  (loc (compilation--message->loc msg))
                  (file-name (caar (compilation--loc->file-struct loc)))
                  (line-num (compilation--loc->line loc))
                  (line-text (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))))
        (when (string-match "^[^\0]+\0[0-9]+:\\(.+\\)$" line-text)
          (let ((new-content (match-string 1 line-text)))
            ;; todo: don't run mode hooks etc
            (with-current-buffer (find-file-noselect file-name)
              (save-excursion
                (goto-char (point-min))
                (forward-line (1- line-num))
                (delete-region (line-beginning-position) (line-end-position))
                (insert new-content))
              (ol-save-silently)))))
      (forward-line 1)))
  (message "Applied grep changes"))

;; -----------------------------------------------------------------------------
;; Buffer name
;; -----------------------------------------------------------------------------

(defvar ol-grep-command-args nil)

(defun ol-get-grep-command-args (command-args)
  (setq ol-grep-command-args command-args))

(advice-add 'grep :after #'ol-get-grep-command-args)

(defun ol-grep-buffer-name ()
  (rename-buffer (generate-new-buffer-name (concat "*grep* " ol-grep-command-args))))

(add-hook 'grep-mode-hook #'ol-grep-buffer-name)

;; -----------------------------------------------------------------------------
;; Results as file names
;; -----------------------------------------------------------------------------

(defun ol-grep-goto-file ()
  (interactive)
  (let* ((line (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))))
    (find-file line)))

(ol-evil-define-key 'normal grep-mode-map "x" #'ol-grep-goto-file)

(provide 'ol-grep)

