;;;  -*- lexical-binding: t; -*-

(require 'ol-util)
(require 'ol-ert)

(require 'vertico)
(require 'embark)
(require 'consult)
(require 'orderless)

(vertico-mode)

;; -----------------------------------------------------------------------------
;; Vertico
;; -----------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; Key bindings
;;;; ---------------------------------------------------------------------------

(ol-define-key ol-override-map "C-j" 'switch-to-buffer)

(ol-define-key minibuffer-local-map "C-j" 'next-line)
(ol-define-key minibuffer-local-map "C-k" 'previous-line)
(ol-define-key minibuffer-local-map "C-n" 'minibuffer-keyboard-quit)
(ol-define-key minibuffer-local-map 'tab 'vertico-exit)
(ol-define-key minibuffer-local-map 'return 'vertico-exit-input)
(ol-define-key minibuffer-local-map "M-i" 'vertico-insert)
(ol-define-key minibuffer-local-map "M-o" 'embark-collect)


;;;; ---------------------------------------------------------------------------
;;;; Behavior
;;;; ---------------------------------------------------------------------------

(setc vertico-count 20)
(setc vertico-cycle t)

;; -----------------------------------------------------------------------------
;; Consult
;; -----------------------------------------------------------------------------

(setc consult-async-min-input 0)
(setc consult-find-args "find . -not ( -path *.git/* -prune )")
(setc consult-fd-args "fd --full-path --color=never --hidden --exclude *.git/*")
(setc consult-async-split-style 'none)

(ol-define-key ol-override-map "M-q" #'consult-fd)
(ol-define-key ol-override-map "M-e" #'consult-ripgrep)

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
(ol-define-key ol-override-map "M-q" 'ol-find-file-name)

;; -----------------------------------------------------------------------------
;; Orderless
;; -----------------------------------------------------------------------------

(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

(provide 'ol-vertico)
