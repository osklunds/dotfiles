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

;; -----------------------------------------------------------------------------
;; Left part
;; -----------------------------------------------------------------------------

(defun ol-search-hits-segment ()
  (when (and (mode-line-window-selected-p) ol-evil-is-searching)
    (pcase-let* ((`(,current . ,total) (ol-search-count-cached)))
      (format "(%d/%d)  " current total))))

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

;; Manually update if out-of-sync and gr doesn't work since not a file buffer
(ol-define-key ol-normal-leader-map "m b" #'ol-branch-name-segment)

(add-hook 'after-revert-hook 'ol-branch-name-segment)
(add-hook 'find-file-hook 'ol-branch-name-segment)

;; Avoid after-change-major-mode-hook because slow for .el.gz files and
;; is called every time minibuffer closes
;; The below are needed for modes that don't have files because the above hooks
;; are not called
(add-hook 'dired-mode-hook #'ol-branch-name-segment)
(add-hook 'magit-mode-hook #'ol-branch-name-segment)

(defun ol-get-current-branch ()
  (unless (active-minibuffer-window)
    (if-let* ((branch (magit-get-current-branch)))
        branch
      (if-let* ((detached-at-status (magit-git-string "status"))
                (detached-at (ol-regexp-group "HEAD detached at \\(.+\\)"
                                              detached-at-status 1)))
          detached-at
        (when-let* ((commit-id (magit-git-string "rev-parse" "HEAD")))
          (substring commit-id 0 7))))))

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

;; Refresh for all buffers. Needed for *Messages* but can be useful for all
(dolist (buf (buffer-list))
  (with-current-buffer buf
    (setq mode-line-format (default-value 'mode-line-format))))

;; -----------------------------------------------------------------------------
;; header-line
;; -----------------------------------------------------------------------------

(setq-default header-line-format
              (list
               "   "
               '(:eval (ol-file-name-segment))))

(provide 'ol-mode-line)
