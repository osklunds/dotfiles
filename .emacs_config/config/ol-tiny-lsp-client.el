
(require 'ol-util)
(require 'ol-evil)

(require 'tiny-lsp-client)

(setc tlc-log-io t)
(setc tlc-log-stderr t)
(setc tlc-log-rust-debug t)
(setc tlc-log-emacs-debug t)
(setc tlc-log-to-stdio nil)

(add-hook 'tlc-mode-hook 'tlc-use-xref)
(add-hook 'tlc-mode-hook 'tlc-use-capf)

(add-hook 'rust-mode-hook 'tlc-mode)

(ol-define-normal-leader-key "l l" 'tlc-open-log-file)
(ol-define-normal-leader-key "l i" 'tlc-info)
(ol-define-normal-leader-key "l r" 'tlc-restart-server)
(ol-define-normal-leader-key "l s" 'tlc-stop-server)

(provide 'ol-tiny-lsp-client)

