
(require 'ol-util)
(require 'ivy)

(setc ivy-height 20)
(setc ivy-wrap t)

(ivy-mode t)

(defun ol-ivy-switch-buffer ()
  "Copy of ivy-switch-buffer, but allow visible buffers in preselect"
  (interactive)
  (let ((visible-ok t))
    (ivy-read "Switch to buffer: " #'internal-complete-buffer
              :keymap ivy-switch-buffer-map
              :preselect (buffer-name (other-buffer (current-buffer) visible-ok))
              :action #'ivy--switch-buffer-action
              :matcher #'ivy--switch-buffer-matcher
              :caller 'ivy-switch-buffer)))

(ol-override-key "C-j" 'ol-ivy-switch-buffer)
(ol-global-set-key "C-x C-b" 'ol-ivy-switch-buffer)

(require 'counsel)

(ivy-configure 'counsel-M-x
  :initial-input ""
  :display-transformer-fn #'counsel-M-x-transformer)

(ol-override-key "M-x" 'counsel-M-x)
(ol-global-set-key "C-x C-f" 'counsel-find-file)

(defun ol-swiper--cleanup-advice (func &rest args)
  ;; So that swiper highlights are always cleaned up
  (let ((lazy-highlight-cleanup t))
    (apply func args)))

(advice-add 'swiper--cleanup :around 'ol-swiper--cleanup-advice)

(setc swiper-faces '(swiper-match-face-1
                     swiper-match-face-2
                     swiper-match-face-2
                     swiper-match-face-2))

(setq swiper-background-faces '(swiper-background-match-face-1
                                swiper-background-match-face-2
                                swiper-background-match-face-2
                                swiper-background-match-face-2))

(ol-define-key ivy-minibuffer-map 'tab 'ivy-alt-done) ;; Exit, and use current selection
(ol-define-key ivy-minibuffer-map 'return 'ivy-immediate-done) ;; Exit, and use current input
(ol-define-key ivy-minibuffer-map "M-i" 'ivy-insert-current) ;; Insert the current candidate as input

(ol-define-key ivy-minibuffer-map "C-j" 'ivy-next-line)
(ol-define-key ivy-minibuffer-map "C-k" 'ivy-previous-line)
(ol-define-key ivy-minibuffer-map "C-n" 'minibuffer-keyboard-quit)
(ol-define-key minibuffer-local-map "C-n" 'minibuffer-keyboard-quit)

(ol-define-key ivy-switch-buffer-map "C-k" 'ivy-previous-line)
(ol-define-key ivy-switch-buffer-map "C-d" 'ivy-switch-buffer-kill)

(ol-evil-define-key 'motion ivy-occur-grep-mode-map "o" 'ivy-occur-press)
(ol-evil-define-key 'motion ivy-occur-grep-mode-map "O" 'ivy-occur-press-and-switch)
(ol-evil-define-key 'normal ivy-occur-mode-map "o" 'ivy-occur-press)
(ol-evil-define-key 'normal ivy-occur-mode-map "O" 'ivy-occur-press-and-switch)

(provide 'ol-ivy)
