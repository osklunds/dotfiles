;; -*- lexical-binding: nil -*-

(require 'ol-util)

(setq-default tab-width 4)
(setq-default evil-shift-width 4)
(setq-default c-basic-offset 4)

(setq-default indent-tabs-mode nil)

(setq-default fill-column 80)

(defun ol-hide-chars ()
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'after-change-major-mode-hook 'ol-hide-chars)

(setc yank-excluded-properties t)

(setc text-mode-ispell-word-completion nil)

(when (featurep 'ol-corfu)
  (defun ol-add-text-mode-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super #'ol-capf-abbrev
                                       #'ol-capf-dabbrev))))

  (add-hook 'text-mode-hook #'ol-add-text-mode-capf)
  ;; Will be overriden by e.g. tlc or lisp hook if available
  (add-hook 'prog-mode-hook #'ol-add-text-mode-capf))

(provide 'ol-text)
