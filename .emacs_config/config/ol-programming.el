;; -*- lexical-binding: t -*-

(require 'ol-util)
(require 'ol-evil)

(require 'imenu)
(require 'counsel)
(require 'org)

;; -----------------------------------------------------------------------------
;; imenu
;; -----------------------------------------------------------------------------

(defun ol-symbol-search (&optional arg)
  (interactive "P")
  (if (equal major-mode 'org-mode)
      (org-goto)
    (when arg
      (setq imenu--index-alist nil))
    (counsel-imenu)))

(ol-define-key ol-normal-leader-map "m s" 'ol-symbol-search)

(setc imenu-max-item-length 200)

(defun ol-counsel-imenu-advice (&rest _args)
  (evil-set-jump))

(advice-add 'counsel-imenu-action :before 'ol-counsel-imenu-advice)

(provide 'ol-programming)
