
(require 'ol-util)

(require 'vertico)

(vertico-mode)

;; -----------------------------------------------------------------------------
;; Keymaps
;; -----------------------------------------------------------------------------

(ol-override-key "C-j" 'switch-to-buffer)

(ol-define-key minibuffer-local-map "C-j" 'next-line)
(ol-define-key minibuffer-local-map "C-k" 'previous-line)
(ol-define-key minibuffer-local-map "C-n" 'minibuffer-keyboard-quit)
(ol-define-key minibuffer-local-map 'tab 'vertico-exit)
(ol-define-key minibuffer-local-map 'return 'vertico-exit-input)
(ol-define-key minibuffer-local-map "M-i" 'vertico-insert)

;; -----------------------------------------------------------------------------
;; Behavior
;; -----------------------------------------------------------------------------

(setc vertico-count 20)
(setc vertico-cycle t)

(provide 'ol-vertico)
