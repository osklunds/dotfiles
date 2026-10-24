;; -*- lexical-binding: t -*-

(require 'tramp-cmds)

(require 'ol-evil)

(defun ol-clean-tramp ()
  (interactive)
  (tramp-cleanup-all-connections)
  (tramp-cleanup-all-buffers))

(ol-define-key ol-normal-leader-map "m t" #'ol-clean-tramp)

(defun ol-low-verbose-tramp-sh-handle-insert-directory (func &rest args)
  (let* ((tramp-verbose -1)) ;; 0 is not enough
    (apply func args)))

(advice-add 'tramp-sh-handle-insert-directory :around
            #'ol-low-verbose-tramp-sh-handle-insert-directory)

(provide 'ol-tramp)
