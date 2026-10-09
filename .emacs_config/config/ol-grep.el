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

(ol-evil-define-key 'normal grep-mode-map "x" #'grep-change-to-grep-edit-mode)

(ol-evil-define-key 'normal compilation-button-map "o" #'compile-goto-error)
(ol-evil-define-key 'normal compilation-mode-map "o" #'compile-goto-error)
(ol-evil-define-key 'normal grep-mode-map "o" #'compile-goto-error)
(ol-evil-define-key 'normal grep-mode-map "O" #'ol-compile-goto-error-other-window)

(defun ol-compile-goto-error-other-window ()
  (interactive)
  (ol-split-window)
  (compile-goto-error))

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

(provide 'ol-grep)

