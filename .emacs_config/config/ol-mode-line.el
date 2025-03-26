
(require 'ol-find-replace)
(require 'ol-evil)
(require 'ol-ivy)

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
(ol-copy-face-fg-bg 'ol-evil-plain-state-mode-mode-line-face 'company-tooltip-common)

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

(defun ol-mode-line-left-part ()
  (quote ((:eval (ol-search-hits-segment))
          (:eval (ol-evil-segment))
          "  " (:eval (ol-buffer-name-segment))
          " " (:eval (ol-file-state-segment))
          " " "%l:%c"
          "" (:eval (ol-relative-position-segment)))))

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
  (propertize "%b" 'face 'ol-buffer-name-mode-line-face))

(defun ol-file-state-segment ()
  (if (and (not (cl-member major-mode '(wdired-mode)))
           (or buffer-read-only (not buffer-file-name)))
      "%%%%"
    (if (buffer-modified-p) "*" "-")))

(defun ol-relative-position-segment ()
  (format "%4d%%%%%%%%" (/ (point) 0.01 (point-max))))
;; TODO the amount of % escaping above means there are too many layers

;; -----------------------------------------------------------------------------
;; Right part
;; -----------------------------------------------------------------------------

(defun ol-mode-line-right-part ()
  (quote ((:eval ol-branch-name-segment)
          "  " ((:eval (ol-project-name-segment)))
          )))

(defvar-local ol-branch-name-segment nil)
(defun ol-branch-name-segment ()
  (setq ol-branch-name-segment (if-let ((branch (ol-get-current-branch)))
                                   branch
                                 "")))

(add-hook 'after-revert-hook 'ol-branch-name-segment)
(add-hook 'find-file-hook 'ol-branch-name-segment)
;; To handle e.g. magit-status when the buffer has no file
(add-hook 'after-change-major-mode-hook 'ol-branch-name-segment)

(defun ol-get-current-branch ()
  (if-let ((branch (magit-get-current-branch)))
      branch
    (when-let ((commit-id (magit-git-string "rev-parse" "HEAD")))
        (substring commit-id 0 7))))

;; No need to cache since (ol-project-name) already is fast and cached
(defun ol-project-name-segment ()
  (if-let ((name (ol-project-name)))
      name
    ""))

;; -----------------------------------------------------------------------------
;; Putting it all together
;; -----------------------------------------------------------------------------

;; Modeline stuff copied (and then modified) from
;; https://www.reddit.com/r/emacs/comments/1333621/wrote_a_custom_modeline_with_some_help_from/
(defun ol-render-mode-line (left right)
  (let* ((left-formatted (format-mode-line left))
         (right-formatted (format-mode-line right))
         (total-width (- (window-total-width) 5))
         (available-width (- total-width (length left-formatted) 1))
         (align-format-string (format "%%s %%%ds " available-width))
         (formatted (format align-format-string left-formatted right-formatted))
         (truncated (truncate-string-to-width formatted total-width)))
    (concat "   " truncated "  ")))

(setq-default mode-line-format
              (quote ((:eval (ol-render-mode-line
                              (ol-mode-line-left-part)
                              (ol-mode-line-right-part))))))

;; Workaround to make sure also the messages buffer has the correct value
(with-current-buffer (get-buffer "*Messages*")
  (setq mode-line-format (default-value 'mode-line-format)))

(provide 'ol-mode-line)
