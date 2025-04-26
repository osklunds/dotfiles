;; -*- lexical-binding: nil -*-

(require 'ol-util)
(require 'ol-colors)
(require 'ol-evil)
(require 'ol-completion-in-region)

(require 'company)

;; -----------------------------------------------------------------------------
;; General
;; -----------------------------------------------------------------------------

(global-company-mode t)

;; Workaround due to what seems to be a company bug. Happened at last uplift.
;; When I get time I should report this bug.
(advice-add 'company-post-command :around #'ol-inhibit-message-advice)

;; -----------------------------------------------------------------------------
;; Backend
;; -----------------------------------------------------------------------------

(setc company-backends '((
                          company-abbrev
                          :separate
                          company-capf
                          :separate
                          company-dabbrev-code
                          )))
(make-variable-buffer-local 'company-backends)

(defun ol-no-company-capf ()
  (setq-local company-backends '((
                                  company-abbrev
                                  :separate
                                  company-dabbrev-code
                                  ))))

(add-hook 'sh-mode-hook 'ol-no-company-capf)

;; -----------------------------------------------------------------------------
;; dabbrev
;; -----------------------------------------------------------------------------

(setc company-dabbrev-minimum-length 2)
(setc company-dabbrev-other-buffers nil)
(setc company-dabbrev-code-other-buffers nil)
(setc company-dabbrev-code-everywhere t)
(setc company-dabbrev-code-modes t)

;; -----------------------------------------------------------------------------
;; Frontend
;; -----------------------------------------------------------------------------

(setc company-minimum-prefix-length 1)
(setc company-idle-delay 0.0)
(setc company-selection-wrap-around t)
(setc company-tooltip-align-annotations t)
(setc company-format-margin-function 'company-text-icons-margin)

(add-hook 'evil-insert-state-exit-hook 'company-abort)

(setc company-frontends '(
                          company-pseudo-tooltip-unless-just-one-frontend
                          company-preview-if-just-one-frontend
                          ))

;; -----------------------------------------------------------------------------
;; Colors
;; -----------------------------------------------------------------------------

(set-face-attribute 'company-tooltip-selection nil
                    :inherit 'ol-selection-face
                    :background 'unspecified)

;; inhert from fixed-pitch to make the popup better when variable-pitch-mode is used
(ol-set-face 'company-tooltip :background ol-white :inherit '(fixed-pitch))

(ol-set-face 'company-tooltip-scrollbar-thumb :background "#4087f2")
(ol-set-face 'company-tooltip-scrollbar-track :background 'unspecified :inherit 'tooltip)

;; -----------------------------------------------------------------------------
;; Keybinds
;; -----------------------------------------------------------------------------

(ol-define-key company-active-map 'return 'company-abort)
(ol-define-key company-active-map "C-g" nil)
(ol-define-key company-active-map "C-n" nil)
(ol-define-key company-active-map 'tab 'company-complete-selection)
(ol-define-key company-active-map "C-j" 'company-select-next)
(ol-define-key company-active-map "C-k" 'company-select-previous)

(ol-define-key prog-mode-map 'tab 'company-indent-or-complete-common)

(provide 'ol-company-mode)
