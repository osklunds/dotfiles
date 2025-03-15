
(require 'ol-company-mode)

(require 'company-box)

(add-hook 'company-mode-hook 'company-box-mode)

(ol-set-face 'company-box-background :background ol-white)
(ol-set-face 'company-box-selection :background ol-completion-selection-color)

(provide 'ol-company-box-mode)
