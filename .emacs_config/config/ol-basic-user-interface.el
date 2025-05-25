;; -*- lexical-binding: nil -*-

(require 'ol-colors)
(require 'ol-util)
(require 'ol-evil)

(fset 'yes-or-no-p 'y-or-n-p)

;; To make sure e.g. ♝ are monospaced
(set-fontset-font t 'symbol
                  (font-spec :family "DejaVu Sans Mono"))

(global-hl-line-mode)
(make-variable-buffer-local 'global-hl-line-mode)

;; The column at e.g. 80 chars
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(setc help-window-select t)

;; -----------------------------------------------------------------------------
;; Visual line mode
;; -----------------------------------------------------------------------------

(global-visual-line-mode t)
(setq-default visual-line-mode t)

(defun ol-truncate-lines ()
  (interactive)
  (visual-line-mode -1)
  (toggle-truncate-lines 1))

(ol-define-key ol-normal-leader-map "m v" #'ol-truncate-lines)

;; -----------------------------------------------------------------------------
;; Reduce Clutter
;; -----------------------------------------------------------------------------

(setc inhibit-startup-screen t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(blink-cursor-mode -1)

(setq visible-bell nil ring-bell-function #'ignore)

(setq frame-title-format "Emacs")

(setq mouse-highlight nil)
(setq show-help-function nil)
(setq command-error-function 'help-command-error-confusable-suggestions)

(setc native-comp-async-report-warnings-errors nil)

(setc warning-minimum-level :error)

(setc display-hourglass nil)

(setq-default fringe-indicator-alist
              '((continuation nil nil)
                (truncation left-arrow right-arrow)
                (overlay-arrow . right-triangle)
                (up . up-arrow)
                (down . down-arrow)
                (top top-left-angle top-right-angle)
                (bottom bottom-left-angle bottom-right-angle top-right-angle top-left-angle)
                (top-bottom left-bracket right-bracket top-right-angle top-left-angle)
                (empty-line . empty-line) (unknown . question-mark)))

(setc view-inhibit-help-message t)

;; -----------------------------------------------------------------------------
;; Line numbers
;; -----------------------------------------------------------------------------

(global-display-line-numbers-mode t)
(setc display-line-numbers-type 'visual)
(setc display-line-numbers-grow-only t)
(setc display-line-numbers-width-start 10000)

;; -----------------------------------------------------------------------------
;; Trailing whitespace
;; -----------------------------------------------------------------------------

(setq-default show-trailing-whitespace nil)

(defun ol-toggle-show-trailing-whitespace ()
  (interactive)
  (setq-local show-trailing-whitespace (not show-trailing-whitespace))
  (message "Toggled show trailing. Now: %s" show-trailing-whitespace))

(ol-define-key ol-normal-leader-map "m w" 'ol-toggle-show-trailing-whitespace)

(provide 'ol-basic-user-interface)
