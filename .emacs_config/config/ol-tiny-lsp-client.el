
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

(ol-define-normal-leader-key "ll" 'tlc-open-log-file)
(ol-define-normal-leader-key "li" 'tlc-info)
(ol-define-normal-leader-key "lr" 'tlc-restart-server)
(ol-define-normal-leader-key "ls" 'tlc-stop-server)

(provide 'ol-tiny-lsp-client)

