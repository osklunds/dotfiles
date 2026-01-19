;; -*- lexical-binding: t -*-

(require 'ol-evil)
(require 'ol-completing-read-framework)
(require 'ol-completing-read-applications)

(setq ol-switch-to-project-action #'ol-dwim-find-file-name)

;; -----------------------------------------------------------------------------
;; Keybinds
;; -----------------------------------------------------------------------------

(ol-define-key ol-override-map "C-j" #'ol-switch-to-buffer)
(ol-define-key ol-normal-leader-map "m d" #'ol-dummy-completion)
(ol-define-key ol-normal-leader-map "m a" #'ol-dwim-async-find-file-name)
(ol-define-key ol-override-map "M-q" #'ol-dwim-find-file-name)
(ol-define-key ol-override-map "M-e" #'ol-dwim-find-file-content)

;;;; ---------------------------------------------------------------------------
;;;; Inside minibuffer
;;;; ---------------------------------------------------------------------------

;; Otherwise minibuffer-complete-word is called
(ol-define-key minibuffer-local-completion-map "SPC" nil)

(ol-define-key minibuffer-mode-map
               "C-n" #'minibuffer-keyboard-quit)

;; Need for icomplete too to override default keybind
(ol-define-key icomplete-vertical-mode-minibuffer-map
               "C-n" #'minibuffer-keyboard-quit)

(ol-define-key icomplete-vertical-mode-minibuffer-map
               "C-j" #'ol-icomplete-forward)

(ol-define-key icomplete-vertical-mode-minibuffer-map
               "M-j" #'ol-icomplete-forward-many)

(ol-define-key icomplete-vertical-mode-minibuffer-map
               "C-k" #'ol-icomplete-backward)

(ol-define-key icomplete-vertical-mode-minibuffer-map
               "M-k" #'ol-icomplete-backward-many)

(ol-define-key icomplete-vertical-mode-minibuffer-map
               'tab #'ol-icomplete-dwim-tab)

(ol-define-key icomplete-vertical-mode-minibuffer-map
               'return #'ol-icomplete-dwim-return)

(ol-define-key icomplete-vertical-mode-minibuffer-map
               "C-d" #'ol-icomplete-delete-action)

(ol-define-key icomplete-vertical-mode-minibuffer-map
               "M-i" #'ol-icomplete-insert-current-selection)

(ol-define-key icomplete-vertical-mode-minibuffer-map
               "DEL" #'ol-icomplete-dwim-del)

(ol-define-key icomplete-vertical-mode-minibuffer-map
               "~" #'ol-icomplete-dwim-tilde)

;; To prevent help menu from opening
(ol-define-key icomplete-vertical-mode-minibuffer-map
               "C-h" (lambda () (interactive)))

(ol-define-key icomplete-vertical-mode-minibuffer-map
               "C-M-i" #'ol-icomplete-insert-include-glob)

(ol-define-key icomplete-vertical-mode-minibuffer-map
               "C-M-e" #'ol-icomplete-insert-exclude-glob)

(ol-define-key icomplete-vertical-mode-minibuffer-map
               "C-M-f" #'ol-icomplete-insert-fixed-strings-option)

(ol-define-key icomplete-vertical-mode-minibuffer-map
               "C-M-s" #'ol-icomplete-insert-case-sensitive-option)

(ol-define-key icomplete-vertical-mode-minibuffer-map
               "C-M-n" #'ol-icomplete-insert-no-ignore-option)

(ol-define-key icomplete-vertical-mode-minibuffer-map
               "C-M-o" #'ol-icomplete-maybe-insert-options-separator)

(ol-define-key icomplete-vertical-mode-minibuffer-map
               "C-M-d" #'ol-icomplete-print-async-debug-info)

;;;; ---------------------------------------------------------------------------
;;;; Collect
;;;; ---------------------------------------------------------------------------

(ol-define-key icomplete-vertical-mode-minibuffer-map "M-o" #'ol-collect)
(ol-evil-define-key 'normal ol-collect-mode-map "RET" #'ol-select)
(ol-evil-define-key 'normal ol-collect-mode-map "o" #'ol-select)

(ol-evil-define-key 'normal compilation-button-map "o" #'ol-async-goto-result)
(ol-evil-define-key 'normal compilation-mode-map "o" #'ol-async-goto-result)
(ol-evil-define-key 'normal grep-mode-map "o" #'ol-async-goto-result)
(ol-evil-define-key 'normal grep-mode-map "O" #'ol-async-goto-result-other-window)

;; -----------------------------------------------------------------------------
;; Async face attributes
;; -----------------------------------------------------------------------------

(set-face-attribute 'grep-heading nil
                    :foreground "#000000"
                    :background "#eeeee")

(set-face-attribute 'match nil
                    :inherit 'ol-match-face
                    :weight 'normal
                    :foreground 'unspecified
                    :background 'unspecified)

(set-face-attribute 'compilation-line-number nil
                    :foreground 'unspecified
                    :underline nil
                    :inherit 'font-lock-string-face)

(provide 'ol-completing-read-settings)
