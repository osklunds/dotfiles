;; -*- lexical-binding: nil -*-

(require 'ol-find-replace)
(require 'ol-evil)
(require 'ol-project)
(require 'ol-file)

(require 'magit)

;; -----------------------------------------------------------------------------
;; Faces
;; -----------------------------------------------------------------------------

(defface ol-evil-normal-state-mode-line-face nil "")
(defface ol-evil-insert-state-mode-line-face nil "")
(defface ol-evil-visual-state-mode-line-face nil "")
(defface ol-evil-emacs-state-mode-line-face nil "")
(defface ol-evil-operator-state-mode-line-face nil "")
(defface ol-evil-plain-state-mode-mode-line-face nil "")
(defface ol-buffer-name-mode-line-face nil "")

;; I don't remember why I did this
(ol-set-face 'mode-line :overline 'unspecified :underline 'unspecified)

;; I don't remember why I did this
(if (display-graphic-p)
    (ol-set-face 'mode-line :background "#d7e4e8")
  (ol-set-face 'mode-line :background "#cccccc"))

(ol-set-face 'mode-line-inactive
             :background "#e9eded"
             :overline 'unspecified
             :underline 'unspecified)

(ol-set-face 'ol-buffer-name-mode-line-face
             :weight 'bold)

(ol-copy-face-fg-bg 'ol-evil-normal-state-mode-line-face 'font-lock-comment-face)
(ol-copy-face-fg-bg 'ol-evil-insert-state-mode-line-face 'font-lock-keyword-face)
(ol-copy-face-fg-bg 'ol-evil-visual-state-mode-line-face 'warning)
(ol-copy-face-fg-bg 'ol-evil-emacs-state-mode-line-face 'font-lock-builtin-face)
(ol-copy-face-fg-bg 'ol-evil-plain-state-mode-mode-line-face 'ol-match-face)

(dolist (face '(ol-evil-normal-state-mode-line-face
                ol-evil-insert-state-mode-line-face
                ol-evil-visual-state-mode-line-face
                ol-evil-emacs-state-mode-line-face
                ol-evil-operator-state-mode-line-face
                ol-evil-plain-state-mode-mode-line-face
                ))
  (ol-set-face face :weight 'bold))

;; -----------------------------------------------------------------------------
;; Left part
;; -----------------------------------------------------------------------------

(defun ol-search-hits-segment ()
  (when (and (mode-line-window-selected-p) anzu--state evil-ex-search-start-point)
    (format "(%d/%d)  " anzu--current-position anzu--total-matched)))

(defun ol-evil-segment ()
  (let ((evil-face (cond
                    (ol-plain-state-mode     'ol-evil-plain-state-mode-mode-line-face)
                    ((evil-normal-state-p)   'ol-evil-normal-state-mode-line-face)
                    ((evil-insert-state-p)   'ol-evil-insert-state-mode-line-face)
                    ((evil-visual-state-p)   'ol-evil-visual-state-mode-line-face)
                    ((evil-emacs-state-p)    'ol-evil-emacs-state-mode-line-face)
                    ((evil-operator-state-p) 'ol-evil-operator-state-mode-line-face)
                    (t                       'ol-evil-normal-state-mode-line-face))))
    (propertize
     (concat
      (truncate-string-to-width (string-pad (upcase (symbol-name evil-state)) 9 32) 6))
     'face evil-face)))

(defun ol-buffer-name-segment ()
  (list -50 (propertize "%b" 'face 'ol-buffer-name-mode-line-face)))

(defun ol-file-state-segment ()
  (cond
   ((ol-save-p) "A")
   (buffer-read-only "%%")
   ((buffer-modified-p) "*")
   (t "-")))

(defun ol-relative-position-segment ()
  ;; % for the % itself
  ;; %% for preserving % after format below
  ;; %%%% for preserving %% inside mode-line-format itself
  (format "%4d%%%%" (/ (point) 0.01 (point-max))))

;; -----------------------------------------------------------------------------
;; Right part
;; -----------------------------------------------------------------------------

(defvar-local ol-branch-name-segment nil)
(defun ol-branch-name-segment ()
  (interactive)
  (setq ol-branch-name-segment (ol-get-current-branch)))

(ol-define-key ol-normal-leader-map "m b" #'ol-branch-name-segment)

(add-hook 'after-revert-hook 'ol-branch-name-segment)
(add-hook 'find-file-hook 'ol-branch-name-segment)
;; To handle e.g. magit-status when the buffer has no file
(add-hook 'after-change-major-mode-hook 'ol-branch-name-segment)

(defun ol-get-current-branch ()
  (if-let ((branch (magit-get-current-branch)))
      branch
    (if-let* ((detached-at-status (magit-git-string "status"))
              (detached-at (ol-regexp-group "HEAD detached at \\(.+\\)"
                                            detached-at-status 1)))
        detached-at
      (when-let ((commit-id (magit-git-string "rev-parse" "HEAD")))
        (substring commit-id 0 7)))))

;; No need to cache since (ol-project-name) already is fast and cached
(defun ol-project-name-segment ()
  (ol-project-name))

;; -----------------------------------------------------------------------------
;; Putting it all together
;; -----------------------------------------------------------------------------

(setq-default mode-line-format
              (list 
               "   "
               '(:eval (ol-search-hits-segment))
               '(:eval (ol-evil-segment))
               "  " '(:eval (ol-buffer-name-segment))
               " " '(:eval (ol-file-state-segment))
               " " "%l:%c"
               "" '(:eval (ol-relative-position-segment))
               'mode-line-format-right-align
               'ol-branch-name-segment
               "  " '(:eval (ol-project-name-segment))
               "    "
               ))

;; Use 'window so that olivetti-mode doesn't cause the elements to appear too
;; far to the left
(setc mode-line-right-align-edge 'window)

;; Workaround to make sure also the messages buffer has the correct value
(with-current-buffer (get-buffer "*Messages*")
  (setq mode-line-format (default-value 'mode-line-format)))

(provide 'ol-mode-line)
