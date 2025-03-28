;; -*- lexical-binding: nil -*-

(require 'ol-util)
(require 'ol-evil)

(defun ol-window-setup-hook ()
  (toggle-frame-maximized))

(add-hook 'window-setup-hook 'ol-window-setup-hook)

(ol-define-key evil-normal-state-map "q" 'quit-window)

;;;; ---------------------------------------------------------------------------
;;;; Balanced windows
;;;; ---------------------------------------------------------------------------

;; Copied/inspired from https://github.com/wbolster/emacs-balanced-windows
(dolist (fn '(delete-window quit-window split-window))
  (advice-add fn :after #'ol-balance-windows-advice))

(defun ol-balance-windows-advice (&rest _)
  (balance-windows))

;;;; ---------------------------------------------------------------------------
;;;; Only two windows
;;;; ---------------------------------------------------------------------------

(defvar ol-split-style 'vertical)

(defun ol-toggle-split-style ()
  (setq ol-split-style (if (eq ol-split-style 'vertical)
                           'horizontal
                         'vertical)))

(defun ol-split-based-on-style ()
  (if (eq ol-split-style 'vertical)
      (split-window-right)
    (split-window-below)))

(defun ol-split-window-sensibly (&optional window)
  (interactive)
  (let ((window (or window (selected-window))))
    (and (= 1 (count-windows))
         (with-selected-window window
           (ol-split-based-on-style)))))

(setc split-window-preferred-function #'ol-split-window-sensibly)

;;;; ---------------------------------------------------------------------------
;;;; Transposing
;;;; ---------------------------------------------------------------------------

(defun ol-transpose-windows ()
  (interactive)
  (if (not (equal (length (window-list)) 2))
      (message "Can't transpose if not exactly two windows")
    (ol-toggle-split-style)
    (let* ((this (selected-window))
           (other (next-window this))
           (left-top-selected (if (or (window-in-direction 'left)
                                      (window-in-direction 'above))
                                  nil
                                t)))
      (delete-window other)
      (ol-split-window-sensibly)
      (when left-top-selected
        (other-window 1))
      (switch-to-buffer (other-buffer))
      (other-window 1))))

;;;; ---------------------------------------------------------------------------
;;;; Splitting
;;;; ---------------------------------------------------------------------------

(defun ol-split-window ()
  (interactive)
  (let ((current-point (point))
        (current-window-start (window-start)))
    (switch-to-buffer-other-window (current-buffer))
    (set-window-point (selected-window) current-point)
    (set-window-start (selected-window) current-window-start)))

(ol-define-key ol-override-map "M-d" 'ol-split-window)

(defun ol-force-split-window ()
  (interactive)
  (split-window-right)
  (evil-window-right 1))

(ol-define-key ol-override-map "M-r" 'ol-force-split-window)


(provide 'ol-windows)
