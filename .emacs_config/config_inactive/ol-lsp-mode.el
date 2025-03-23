
(require 'ol-util)
(require 'ol-evil)

(require 'lsp-mode)

;; Reduce noise
(setc lsp-enable-symbol-highlighting nil)
(setc lsp-modeline-code-actions-enable nil)
(setc lsp-modeline-diagnostics-enable nil)
(setc lsp-diagnostics-provider :none) ;; TODO: try out toggling diagnostics
(setc lsp-ui-sideline-enable nil)
(setc lsp-modeline-workspace-status-enable nil)
(setc lsp-lens-enable nil)
(setc lsp-ui-doc-enable nil)
(setc lsp-headerline-breadcrumb-enable nil)
(setc lsp-eldoc-enable-hover nil)
(setc lsp-signature-auto-activate nil)
(setc lsp-enable-snippet nil)
(setc flycheck-indication-mode nil)

(setc lsp-auto-guess-root t) ;; so that new files don't ask about project
(setc lsp-completion-provider :none) ;; to prevent overriding my own company backends
(setc lsp-response-timeout 10)
(setc lsp-enable-file-watchers nil) ;; to prevent "nested too deep" warning
(setc lsp-log-io nil)
;; (setc lsp-log-io t) ;; Enable for easier debugging

(setc lsp-completion-enable-additional-text-edit nil)
(setc lsp-completion-default-behaviour :insert)

(setc lsp-enable-on-type-formatting nil)

(ol-define-key ol-normal-leader-map "m r" 'lsp-rename)

(add-hook 'lsp-after-apply-edits-hook
          (lambda (operation)
            (when (eq operation 'rename)
              (save-buffer))))

(provide 'ol-lsp-mode)
