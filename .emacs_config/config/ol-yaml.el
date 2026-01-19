;; -*- lexical-binding: t -*-

(require 'yaml-mode)

(require 'ol-evil)

(add-hook 'yaml-mode-hook #'display-fill-column-indicator-mode)

(ol-evil-define-key 'normal yaml-mode-map "gc" 'ol-evilnc-comment-operator)

(provide 'ol-yaml)
