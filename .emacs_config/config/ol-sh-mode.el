;; -*- lexical-binding: t -*-

(require 'ol-corfu)

(require 'sh-script)

(defun ol-capf-sh-mode ()
    (cape-wrap-properties #'sh-completion-at-point-function
                          :annotation-function (lambda (_) " Shell")))

(add-hook 'sh-mode-hook
          (lambda () (ol-set-capfs
                      '(ol-capf-abbrev ol-capf-sh-mode ol-capf-dabbrev))))

(provide 'ol-sh-mode)
