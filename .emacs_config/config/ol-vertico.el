
(require 'ol-util)

(require 'vertico)
(require 'embark)

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
(ol-define-key minibuffer-local-map "M-o" 'embark-collect)

;; -----------------------------------------------------------------------------
;; Behavior
;; -----------------------------------------------------------------------------

(setc vertico-count 20)
(setc vertico-cycle t)

;; -----------------------------------------------------------------------------
;; Find file name
;; -----------------------------------------------------------------------------

(defun ol-find-file-name ()
  (interactive)
  (let* ((candidates (split-string (shell-command-to-string "rg --files") "\n" t))
         (selected (completing-read
                    "Find file name: "
                    candidates
                    nil
                    t
                    nil
                    'ol-find-file-name
                    )))
    (find-file selected)))
(ol-override-key "M-q" 'ol-find-file-name)


(provide 'ol-vertico)
