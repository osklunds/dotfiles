
(require 'yaml-mode)

(add-hook 'yaml-mode-hook #'display-fill-column-indicator-mode)

(ol-evil-define-key 'normal yaml-mode-map "gc" 'ol-evilnc-comment-operator)

(provide 'ol-yaml)
