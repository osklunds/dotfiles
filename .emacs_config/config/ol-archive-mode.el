;; -*- lexical-binding: nil -*-

(require 'ol-evil)
(require 'ol-dired)

(require 'arc-mode)

(defun ol-archive-up-directory ()
  (interactive)
  (if archive-superior-buffer
      (switch-to-buffer archive-superior-buffer)
    (ol-dired)))

;; Keybinds to mimic dired
(ol-evil-define-key 'normal archive-mode-map "o" 'archive-extract)
(ol-evil-define-key 'normal archive-mode-map "i" 'ol-archive-up-directory)

(ol-evil-define-key 'normal archive-mode-map "C" 'archive-copy-file)

(provide 'ol-archive-mode)
