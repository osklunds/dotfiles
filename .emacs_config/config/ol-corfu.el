;; -*- lexical-binding: t -*-

(require 'ol-evil)
(require 'ol-completion-in-region)

(require 'corfu)
(require 'cape)
(require 'tiny-lsp-client)

;; -----------------------------------------------------------------------------
;; Corfu
;; -----------------------------------------------------------------------------

(global-corfu-mode)

(setc corfu-auto t)
(setc corfu-auto-prefix 1)
(setc corfu-auto-delay 0)
(setc corfu-cycle t)
(setc corfu-sort-override-function nil)

;; -----------------------------------------------------------------------------
;; Keybinds
;; -----------------------------------------------------------------------------

(ol-define-key corfu-map 'return #'corfu-quit)
(ol-define-key corfu-map "C-j" #'corfu-next)
(ol-define-key corfu-map "C-k" #'corfu-previous)

(defun ol-on-completion-in-region-mode ()
  (if completion-in-region-mode
      (ol-override-mode -1)
    (ol-override-mode t)))

(add-hook 'completion-in-region-mode-hook #'ol-on-completion-in-region-mode)

(add-hook 'evil-insert-state-exit-hook #'corfu-quit)

(defun ol-complete-or-insert-tab ()
  (interactive)
  (if completion-in-region-mode
      (corfu-complete)
    (ol-insert-tab)))

(ol-define-key evil-insert-state-map 'tab #'ol-complete-or-insert-tab)

;; -----------------------------------------------------------------------------
;; Capfs
;; -----------------------------------------------------------------------------

(setc cape-dabbrev-check-other-buffers nil)
(setc cape-dabbrev-min-length 2)

(provide 'ol-corfu)
