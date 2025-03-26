;; -*- lexical-binding: t -*-

(require 'ol-evil)
(require 'ol-dired)

(require 'tar-mode)

(defun ol-tar-up-directory ()
  (interactive)
  (if tar-superior-buffer
      (switch-to-buffer tar-superior-buffer)
    (ol-dired)))

;; Keybinds to mimic dired
(ol-evil-define-key 'normal tar-mode-map "o" 'tar-view)
(ol-evil-define-key 'normal tar-mode-map "i" 'ol-tar-up-directory)

(defun ol-add-tar-font-lock-keywords ()
  (font-lock-add-keywords
   nil
   (list
    (list ".*PaxHeader.*" (list 0 ''file-name-shadow))
    )))

(add-hook 'tar-mode-hook 'ol-add-tar-font-lock-keywords)

(provide 'ol-tar-mode)
