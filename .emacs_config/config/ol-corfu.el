;; -*- lexical-binding: t -*-

(require 'ol-evil)
(require 'ol-completion-in-region)
(require 'ol-colors)

(require 'corfu)
;; To fix compiler warning
(eval-and-compile
  (defvar corfu-terminal-mode nil))
(require 'corfu-terminal)
(require 'cape)

;; -----------------------------------------------------------------------------
;; Corfu config
;; -----------------------------------------------------------------------------

(if (display-graphic-p)
    (global-corfu-mode t)
  ;; corfu-terminal is much slower than company. So only use corfu in minibuffer
  ;; until emacs 31, where corfu is supported in terminal natively.
  (corfu-terminal-mode t)
  (add-hook 'minibuffer-setup-hook #'corfu--minibuffer-on))

(setc corfu-auto t)
(setc corfu-auto-prefix 1)
(setc corfu-auto-delay 0.01)
(setc corfu-cycle t)
(setc corfu-sort-override-function nil)
(setc corfu-on-exact-match nil)

;; I want this behavior. However, the preview is not deleted unless exactly
;; corfu-quit is called. Calling corfu-quit from my own fun etc doesn't work
;; and results in the preview not being deleted.
(setc corfu-preview-current nil)

;; todo: consider ol-completion-style but ^ and some other separator
(defun ol-set-corfu-completion-style ()
  (setq-local completion-styles '(basic)))

(add-hook 'corfu-mode-hook #'ol-set-corfu-completion-style)

(set-face-attribute 'completions-common-part nil
                    :foreground 'unspecified
                    :inherit 'ol-match-face)

(set-face-attribute 'completions-first-difference nil
                    :weight 'normal
                    :foreground 'unspecified)

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

(add-hook 'evil-insert-state-exit-hook #'ol-quit-corfu-on-normal-state)

(defun ol-quit-corfu-on-normal-state (&rest _)
  (corfu-quit))

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
(setc cape-dabbrev-min-length 3)
(setc dabbrev-case-fold-search nil)

(defun ol-capf-abbrev ()
  (cape-wrap-properties #'cape-abbrev
                        :annotation-function (lambda (_) " Abbrev")))

(defun ol-capf-dabbrev ()
  (cape-wrap-properties #'cape-dabbrev
                        :annotation-function (lambda (_) " Dabbrev")))

(defun ol-set-capfs (capfs)
  ;; If not corfu, company might be used. Don't want double dabbrev
  (when global-corfu-mode
    (setq-local completion-at-point-functions
                (list (apply #'cape-capf-super capfs)))))

(provide 'ol-corfu)
