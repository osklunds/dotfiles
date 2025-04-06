
(require 'ol-evil)
(require 'ol-completion-style)
(require 'orderless)
(require 'embark)

(require 'icomplete)

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

(setc completion-styles '(orderless basic))
(defun ol-orderfull (component)
  (string-join (split-string component) ".*"))

(setc orderless-component-separator #'list)
(setc orderless-matching-styles '(ol-orderfull))

(ol-define-key ol-override-map "C-j" #'switch-to-buffer)

(ol-define-key icomplete-vertical-mode-minibuffer-map "C-n" #'minibuffer-keyboard-quit)
(ol-define-key icomplete-vertical-mode-minibuffer-map "C-j" #'icomplete-forward-completions)
(ol-define-key icomplete-vertical-mode-minibuffer-map "C-k" #'icomplete-backward-completions)
(ol-define-key icomplete-vertical-mode-minibuffer-map 'tab
               #'icomplete-force-complete-and-exit)
(ol-define-key icomplete-vertical-mode-minibuffer-map 'return
               #'icomplete-force-complete)

(ol-define-key icomplete-vertical-mode-minibuffer-map "M-o" #'embark-collect)

;; even here space inserts -
(defun ol-switch-to-buffer ()
  (interactive)
  (let* ((buffers (buffer-list))
         (buffer-names (mapcar (lambda (buffer) (with-current-buffer buffer
                                                  (buffer-name)))
                               buffers)))
  (switch-to-buffer (completing-read
                     "Switch to buffer: "
                     buffer-names))))

(provide 'ol-completing-read-own)
