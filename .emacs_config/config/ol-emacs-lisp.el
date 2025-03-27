;; -*- lexical-binding: nil -*-

(require 'ol-evil)

(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  '(
    ("dbg" "(message \"oskar: %s\" @@)")
    ("sep1" ";; -----------------------------------------------------------------------------\n;; @@\n;; -----------------------------------------------------------------------------")
    ("sep2" ";;;; ---------------------------------------------------------------------------\n;;;; @@\n;;;; ---------------------------------------------------------------------------")
    ("sep3" ";;;;;; -------------------------------------------------------------------------\n;;;;;; @@\n;;;;;; -------------------------------------------------------------------------")
    ))

(defun ol-eval-region ()
  (interactive)
  (call-interactively 'eval-region)
  (message "eval-region"))

(defun ol-eval-buffer ()
  (interactive)
  (call-interactively 'eval-buffer)
  (message "eval-buffer"))

(ol-define-key ol-visual-leader-map "e r" 'ol-eval-region)
(ol-define-key ol-normal-leader-map "e b" 'ol-eval-buffer)

(defun ol-indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(ol-evil-define-key 'normal emacs-lisp-mode-map "C-c C-q" 'ol-indent-buffer)

(provide 'ol-emacs-lisp)
