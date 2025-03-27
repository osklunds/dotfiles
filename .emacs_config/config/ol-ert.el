;; -*- lexical-binding: nil -*-

(require 'ol-evil)

(require 'ert)

(defun ol-assert-equal (exp act &optional label)
  (when (not (equal exp act))
    (message "")
    (message "")
    (message "")
    (message "-----------------------------------------------------------------------------")
    (message "Assert failed. label: '%s'" label)
    (message "Exp: '%s'" exp)
    (message "Act: '%s'" act)
    (message "-----------------------------------------------------------------------------")
    (message "")
    (message "")
    (message "")
    (sleep-for 1)
    )
  (should (equal exp act)))

(defun ol-assert (exp &optional label)
  (ol-assert-equal t (not (not exp)) label))

(defun ol-assert-not (exp &optional label)
  (ol-assert-equal nil exp label))

(ol-evil-define-key 'normal emacs-lisp-mode-map "SPC SPC e" 'ert)

(provide 'ol-ert)
