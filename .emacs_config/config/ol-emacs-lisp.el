;; -*- lexical-binding: nil -*-

(require 'ol-evil)
(require 'ol-corfu)

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

(defun ol-add-emacs-lisp-capf ()
  (setq-local completion-at-point-functions
              (list (cape-capf-super #'ol-capf-abbrev
                                     #'ol-capf-emacs-lisp
                                     #'ol-capf-dabbrev))))

(defun ol-capf-emacs-lisp ()
  (cape-wrap-properties #'elisp-completion-at-point
                        :annotation-function (lambda (_) " Lisp")))

(add-hook 'emacs-lisp-mode-hook #'ol-add-emacs-lisp-capf)

(provide 'ol-emacs-lisp)
