
(require 'ol-util)
(require 'ol-colors)
(require 'ol-evil)

(require 'company)
;; (require 'company-box)

;; -----------------------------------------------------------------------------
;; General
;; -----------------------------------------------------------------------------

(global-company-mode t)

;; -----------------------------------------------------------------------------
;; Snippet expansion
;; -----------------------------------------------------------------------------

;; Copied from https://stackoverflow.com/a/15389612
(defadvice expand-abbrev (after ol-expand-abbrev activate)
  ;; if there was an expansion
  (if ad-return-value
      ;; start idle timer to ensure insertion of abbrev activator
      ;; character (e.g. space) is finished
      (run-with-idle-timer 0 nil
                           (lambda ()
                             ;; if there is the string "@@" in the
                             ;; expansion then move cursor there and
                             ;; delete the string
                             (let ((cursor "@@"))
                               (if (search-backward cursor last-abbrev-location t)
                                   (delete-char (length cursor))))))))

(setc save-abbrevs 'silently)

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

;; (add-hook 'company-mode-hook 'company-box-mode)

(add-hook 'evil-insert-state-exit-hook 'company-abort)

(setc company-frontends '(
                          company-pseudo-tooltip-unless-just-one-frontend
                          company-preview-if-just-one-frontend
                          ))

;; -----------------------------------------------------------------------------
;; Colors
;; -----------------------------------------------------------------------------

;; Making ivy and company look consistent
(dolist (face '(ivy-minibuffer-match-face-1
                ivy-minibuffer-match-face-2
                ivy-minibuffer-match-face-3
                ivy-minibuffer-match-face-4))
  (ol-copy-face-fg-bg face 'company-tooltip-common)
  (ol-set-face face :weight 'bold)
  (ol-set-face face :background ol-white))

(defconst ol-completion-selection-color "#d7e4e8")

(ol-set-face 'ivy-current-match :weight 'bold)
(ol-set-face 'ivy-current-match :background ol-completion-selection-color)
(ol-set-face 'company-tooltip :background ol-white)
(ol-set-face 'company-tooltip-selection :background ol-completion-selection-color)
;; (ol-set-face 'company-box-background :background ol-white)
;; (ol-set-face 'company-box-selection :background ol-completion-selection-color)

(ol-set-face 'company-tooltip-scrollbar-thumb :background "#4087f2")
(ol-set-face 'company-tooltip-scrollbar-track :background nil :inherit 'tooltip)

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
