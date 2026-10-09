;; -*- lexical-binding: t -*-

(require 'grep)

(require 'ol-project)

(setc grep-use-null-device nil)
(setc grep-use-headings t)

(defun ol-dwim-root (&optional prefer-project-root)
  (let ((root (ol-project-root)))
    (cond
     ((and root prefer-project-root) root)
     ;; todo: don't have vterm here, but files aren't found if using project root
     ((cl-member major-mode '(dired-mode vterm-mode)) default-directory)
     (t root))))

;; -----------------------------------------------------------------------------
;; Commands and options
;; -----------------------------------------------------------------------------

(defconst ol-rg-command "rg --color=always --no-heading -n -H -0 -S -- ")
(defconst ol-git-grep-command "git --no-pager grep --color=always -n -- ")
(defconst ol-grep-command "grep --color=always -E -n -I -r -Z -- ")

(defun ol-can-use-rg ()
  (executable-find "rg" 'remote))

(defun ol-can-use-git ()
  (and (executable-find "git" 'remote)
       (locate-dominating-file default-directory ".git")))

(defun ol-can-use-gnu-cmd ()
  t)

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
  (let* ((default-directory (ol-dwim-use-project-root prefer-project-root)))
    (call-interactively #'grep)))

(ol-define-key ol-override-map "M-e" #'ol-grep)

(provide 'ol-grep)

