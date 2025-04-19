;; -*- lexical-binding: t -*-

(require 'ol-evil)
(require 'ol-completion-style)

(require 'embark)

(require 'icomplete)
(require 'delsel) ;; for minibuffer-keyboard-quit

;; -----------------------------------------------------------------------------
;; icomplete-vertical
;; -----------------------------------------------------------------------------

(icomplete-vertical-mode t)
(setq icomplete-scroll t)
(setc icomplete-show-matches-on-no-input t)
(setc icomplete-compute-delay 0)
(setc icomplete-max-delay-chars 0)
(setc resize-mini-windows 'grow-only)
(setc icomplete-prospects-height 20)

(set-face-attribute 'icomplete-selected-match nil
                    :background "#dddddd"
                    )

(ol-define-key icomplete-vertical-mode-minibuffer-map "C-n" #'minibuffer-keyboard-quit)
(ol-define-key icomplete-vertical-mode-minibuffer-map "C-j" #'icomplete-forward-completions)
(ol-define-key icomplete-vertical-mode-minibuffer-map "C-k" #'icomplete-backward-completions)
(ol-define-key icomplete-vertical-mode-minibuffer-map 'tab
               #'icomplete-force-complete-and-exit)
(ol-define-key icomplete-vertical-mode-minibuffer-map 'return
               #'icomplete-force-complete)

;; Otherwise minibuffer-complete-word is called
(ol-define-key minibuffer-local-completion-map "SPC" nil)

(ol-define-key icomplete-vertical-mode-minibuffer-map "M-o" #'embark-collect)

(set-face-attribute 'completions-common-part nil
                    :foreground 'unspecified)

;; -----------------------------------------------------------------------------
;; Completion style
;; -----------------------------------------------------------------------------

(setc completion-styles '(ol))

;; -----------------------------------------------------------------------------
;; Faces
;; -----------------------------------------------------------------------------

;; Needed for switch-to-buffer. Still not perfect

(set-face-attribute 'completions-highlight nil
                    :inherit 'ol-match-face)

(set-face-attribute 'completions-first-difference nil
                    :inherit 'ol-match-face)

(set-face-attribute 'completions-common-part nil
                    :foreground 'unspecified
                    :inherit 'ol-match-face)


;; -----------------------------------------------------------------------------
;; Completing read wrappers
;; -----------------------------------------------------------------------------

(defun ol-switch-to-buffer ()
  "Similar to `switch-to-buffer slightly different behavior that makes me like
it more."
  (interactive)
  (switch-to-buffer (completing-read
                     "Switch to buffer: "
                     (internal-complete-buffer-except))))

(ol-define-key ol-override-map "C-j" #'ol-switch-to-buffer)


(provide 'ol-completing-read-own)
