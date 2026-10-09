;; -*- lexical-binding: t -*-

(require 'grep)

(setc grep-use-null-device nil)
(setc grep-use-headings t)

(ol-define-key ol-override-map "M-e" #'ol-grep)

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

(defun ol-grep ()
  (interactive)
  (setc grep-command (ol-select-grep-command))
  (call-interactively #'grep))

(provide 'ol-grep)

