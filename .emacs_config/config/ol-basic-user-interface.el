
(require 'ol-colors)
(require 'ol-util)

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default show-trailing-whitespace nil)

(defun ol-toggle-show-trailing-whitespace ()
  (interactive)
  (setq-local show-trailing-whitespace (not show-trailing-whitespace))
  (message "Toggled show trailing. Now: %s" show-trailing-whitespace))

(ol-define-normal-leader-key "mw" 'ol-toggle-show-trailing-whitespace)

;; To make sure e.g. ♝ are monospaced
(set-fontset-font t 'symbol
                  (font-spec :family "DejaVu Sans Mono"))

(global-hl-line-mode)
(make-variable-buffer-local 'global-hl-line-mode)

;; The column at e.g. 80 chars
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(global-visual-line-mode t)
(setq-default visual-line-mode t)

(ol-set-face 'default :height 90)

(defconst ol-white "#ffffff") ;; ff works better than white in terminal
(defconst ol-black "#000000")

(ol-set-face 'default :foreground ol-black :background ol-white)
(ol-set-face 'font-lock-comment-face :foreground "#5f8700")
(ol-set-face 'font-lock-string-face :foreground "#d78700")

(unless (display-graphic-p)
  (ol-set-face 'lazy-highlight :background "#c2d3f7" :foreground ol-white)
  (ol-set-face 'hl-line :background "#eeeeee"))

(setc help-window-select t)

;;;; ---------------------------------------------------------------------------
;;;; Reduce Clutter
;;;; ---------------------------------------------------------------------------

(setc inhibit-startup-screen t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

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

;;;; ---------------------------------------------------------------------------
;;;; Line numbers
;;;; ---------------------------------------------------------------------------

(global-display-line-numbers-mode t)
(setc display-line-numbers-type 'visual)
(setc display-line-numbers-grow-only t)
(setc display-line-numbers-width-start 10000)

;;;; ---------------------------------------------------------------------------
;;;; No mouse
;;;;----------------------------------------------------------------------------

(require 'ivy)

(defun ol-no-op ()
  (interactive))

(dolist (key '("<mouse-1>"
               "<mouse-2>"
               "<mouse-3>"
               "<down-mouse-1>"
               "<down-mouse-2>"
               "<down-mouse-3>"
               "<double-mouse-1>"
               "<double-mouse-2>"
               "<double-mouse-3>"
               "<triple-mouse-1>"
               "<triple-mouse-2>"
               "<triple-mouse-3>"
               "<drag-mouse-1>"
               "<drag-mouse-2>"
               "<drag-mouse-3>"
               "<vertical-line> <down-mouse-1>"
               "<vertical-line> <mouse-1>"
               "<mode-line> <down-mouse-1>"
               "<mode-line> <mouse-1>"
               "<escape>"))
  (ol-global-set-key key 'ol-no-op)
  (ol-override-key key 'ol-no-op)
  (ol-define-key button-map key nil)
  (ol-define-key ivy-minibuffer-map key nil)
  (ol-define-key ivy-occur-mode-map key nil)
  (ol-define-key ivy-occur-grep-mode-map key nil)
  (ol-define-key evil-normal-state-map key nil)
  (ol-define-key evil-motion-state-map key nil)
  (ol-define-key evil-visual-state-map key nil)
  (ol-define-key evil-insert-state-map key nil))

(setc mouse-1-click-follows-link nil)




(provide 'ol-basic-user-interface)
