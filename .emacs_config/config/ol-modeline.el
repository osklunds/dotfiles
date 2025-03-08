
(require 'ol-find-replace)
(require 'ol-evil)

(require 'projectile)
(require 'magit)

;;;; ---------------------------------------------------------------------------
;;;; Faces
;;;; ---------------------------------------------------------------------------

(defface ol-evil-normal-state-mode-line-face '() "")
(defface ol-evil-insert-state-mode-line-face '() "")
(defface ol-evil-visual-state-mode-line-face '() "")
(defface ol-evil-emacs-state-mode-line-face '() "")
(defface ol-evil-operator-state-mode-line-face '() "")
(defface ol-buffer-name-mode-line-face '() "")

(ol-set-face 'mode-line :overline 'unspecified :underline 'unspecified)

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

(dolist (face '(ol-evil-normal-state-mode-line-face
                ol-evil-insert-state-mode-line-face
                ol-evil-visual-state-mode-line-face
                ol-evil-emacs-state-mode-line-face
                ol-evil-operator-state-mode-line-face))
  (ol-set-face face :weight 'bold))

;;;; ---------------------------------------------------------------------------
;;;; Left part
;;;; ---------------------------------------------------------------------------

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
  (let ((evil-face (cond ((evil-normal-state-p)   'ol-evil-normal-state-mode-line-face)
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

;;;; ---------------------------------------------------------------------------
;;;; Right part
;;;; ---------------------------------------------------------------------------

(defun ol-mode-line-right-part ()
  (quote ((:eval (ol-branch-name-segment))
          "  " ((:eval (ol-project-name-segment)))
          )))

(defun ol-branch-name-segment ()
  (if-let ((branch (ol-get-current-branch)))
      branch
    ""))

(defvar ol-branch-cache nil)

;; todo: update mode-line async and cache result
(defun ol-get-current-branch ()
  (if-let ((branch (magit-get-current-branch)))
      branch
    (when-let ((commit-id (magit-git-string "rev-parse" "HEAD")))
      (if-let ((cached (cdr (assoc commit-id ol-branch-cache))))
          cached
        (let ((regex "HEAD detached at \\(.+\\)")
              (status (magit-git-string "status")))
          (string-match regex status)
          (when-let ((calculated (match-string 1 status)))
            (add-to-list 'ol-branch-cache `(,commit-id . ,calculated))
            calculated))))))

(defun ol-project-name-segment ()
  (let* ((name (projectile-project-name)))
    (if (string-equal name "-")
        ""
      name)))

;;;; ---------------------------------------------------------------------------
;;;; Putting it all together
;;;; ---------------------------------------------------------------------------

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

(provide 'ol-modeline)
