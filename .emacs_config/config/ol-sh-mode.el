;; -*- lexical-binding: t -*-

(require 'ol-corfu)
(require 'sh-script)

(defun ol-add-sh-mode-capf ()
  (setq-local completion-at-point-functions
              (list (cape-capf-super #'ol-capf-abbrev
                                     #'ol-capf-sh-mode
                                     #'ol-capf-dabbrev))))

(defun ol-capf-sh-mode ()
  (cape-wrap-properties #'sh-completion-at-point-function
                        :annotation-function (lambda (_) " Shell")))

(add-hook 'sh-mode-hook #'ol-add-sh-mode-capf)

(provide 'ol-sh-mode)
