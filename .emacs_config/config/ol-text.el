;; -*- lexical-binding: t -*-

(require 'ol-util)

(setq-default tab-width 4)
(setq-default evil-shift-width 4)
(setq-default c-basic-offset 4)

(setq-default indent-tabs-mode nil)

(setq-default fill-column 80)

(defun ol-insert-tab ()
  (interactive)
  (insert "    "))

(defun ol-hide-chars ()
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'after-change-major-mode-hook 'ol-hide-chars)

(setc yank-excluded-properties t)

(setc text-mode-ispell-word-completion nil)

(provide 'ol-text)
