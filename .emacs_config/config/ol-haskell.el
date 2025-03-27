;; -*- lexical-binding: nil -*-

(require 'ol-util)
(require 'ol-vterm)
(require 'ol-evil)

(require 'haskell-mode)
(require 'haskell-mode-autoloads)

;; -----------------------------------------------------------------------------
;; LSP
;;------------------------------------------------------------------------------

;; (add-hook 'haskell-mode-hook 'lsp)

;; -----------------------------------------------------------------------------
;; Indentation
;;------------------------------------------------------------------------------

(setc haskell-indentation-layout-offset 4)
(setc haskell-indentation-starter-offset 4)
(setc haskell-indentation-left-offset 4)
(setc haskell-indentation-where-pre-offset 4)
(setc haskell-indentation-where-post-offset 4)

(make-local-variable 'evil-auto-indent)

(add-hook 'haskell-mode-hook
          (lambda ()
            (setq evil-auto-indent nil)))

;; -----------------------------------------------------------------------------
;; Terminal integration
;;------------------------------------------------------------------------------

(defun ol-haskell-load-current-module ()
  (interactive)
  (when-let ((module (haskell-guess-module-name)))
    (ol-send-cmd-to-visible-vterm-buffers (concat ":l " module))))

(ol-evil-define-key 'normal haskell-mode-map "C-c l" 'ol-haskell-load-current-module)

(provide 'ol-haskell)
