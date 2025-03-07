
(require 'ol-util)

(defun ol-symbol-search (&optional arg)
  (interactive "P")
  (if (equal major-mode 'org-mode)
      (org-goto)
    (when arg
      (setq imenu--index-alist nil))
    (counsel-imenu)))

(ol-define-normal-leader-key "ms" 'ol-symbol-search)

(setc imenu-max-item-length 200)

(ol-global-set-key "M-/" 'evilnc-comment-or-uncomment-lines)

(defun ol-counsel-imenu-advice (&rest _args)
  (evil-set-jump))

(advice-add 'counsel-imenu-action :before 'ol-counsel-imenu-advice)

(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))

(provide 'ol-programming)
