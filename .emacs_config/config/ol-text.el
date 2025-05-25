;; -*- lexical-binding: nil -*-

(require 'ol-util)
(require 'ol-corfu)

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

(add-hook 'text-mode-hook
          (lambda () (ol-set-capfs
                      '(ol-capf-abbrev ol-capf-dabbrev))))

;; Will be overriden by e.g. tlc or lisp hook if available from more
;; specific major mode
(add-hook 'prog-mode-hook
          (lambda ()
            (ol-set-capfs
             '(ol-capf-abbrev ol-capf-dabbrev))))

(provide 'ol-text)
