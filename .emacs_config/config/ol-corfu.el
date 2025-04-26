;; -*- lexical-binding: t -*-

(require 'ol-evil)
(require 'ol-completion-in-region)
(require 'ol-colors)

(require 'corfu)
(require 'cape)
(require 'tiny-lsp-client)

;; -----------------------------------------------------------------------------
;; Corfu config
;; -----------------------------------------------------------------------------

(global-corfu-mode)

(setc corfu-auto t)
(setc corfu-auto-prefix 1)
(setc corfu-auto-delay 0)
(setc corfu-cycle t)
(setc corfu-sort-override-function nil)
(setc corfu-on-exact-match nil)

;; todo: consider ol-completion-style but ^ and some other separator
(defun ol-set-corfu-completion-style ()
  (setq-local completion-styles '(basic)))

(add-hook 'corfu-mode-hook #'ol-set-corfu-completion-style)

;; -----------------------------------------------------------------------------
;; Keybinds
;; -----------------------------------------------------------------------------

(ol-define-key corfu-map 'return #'corfu-quit)
(ol-define-key corfu-map "C-n" #'corfu-quit)
(ol-define-key corfu-map 'tab #'corfu-complete)
(ol-define-key corfu-map "C-j" #'corfu-next)
(ol-define-key corfu-map "C-k" #'corfu-previous)

(defun ol-on-completion-in-region-mode ()
  (if completion-in-region-mode
      (ol-override-mode -1)
    (ol-override-mode t)))

(add-hook 'completion-in-region-mode-hook #'ol-on-completion-in-region-mode)

(add-hook 'evil-insert-state-exit-hook #'corfu-quit)

;; -----------------------------------------------------------------------------
;; Faces
;; -----------------------------------------------------------------------------

(set-face-attribute 'corfu-default nil
                    :background 'unspecified
                    :foreground 'unspecified
                    :weight 'unspecified
                    :inherit '(ol-candidate-face fixed-pitch))

(set-face-attribute 'corfu-current nil
                    :inherit '(ol-selection-face fixed-pitch)
                    :foreground 'unspecified
                    :background 'unspecified)

;; -----------------------------------------------------------------------------
;; Capfs
;; -----------------------------------------------------------------------------

(setc cape-dabbrev-check-other-buffers nil)
(setc cape-dabbrev-min-length 2)

(defun ol-capf-abbrev ()
  (cape-wrap-properties #'cape-abbrev
                        :annotation-function (lambda (_) "Abbrev")))

(defun ol-capf-dabbrev ()
  (cape-wrap-properties #'cape-dabbrev
                        :annotation-function (lambda (_) "Dabbrev")))

(provide 'ol-corfu)
