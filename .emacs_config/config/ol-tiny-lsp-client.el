
;; I'm experimenting with my own lsp client. Not ready to be made public yet :)

(add-to-list 'load-path "/home/oskar/own_repos/tiny-lsp-client/")

(require 'tiny-lsp-client)

(setc tlc-log-io t)
(setc tlc-log-stderr t)
(setc tlc-log-rust-debug nil)
(setc tlc-log-emacs-debug nil)
(setc tlc-log-to-stdio nil)

(setq load-path (cons  "/usr/lib64/erlang/lib/tools-4.1/emacs" load-path))
(setq erlang-root-dir "/usr/lib64/erlang")
(setq exec-path (cons "/usr/lib64/erlang/bin" exec-path))
(require 'erlang-start)

(add-hook 'c++-mode-hook 'ol-tlc-hook)
(add-hook 'erlang-mode-hook 'ol-tlc-hook)
(add-hook 'rust-mode-hook 'ol-tlc-hook)
;; (remove-hook 'rust-mode-hook 'ol-tlc-hook)

(defun ol-tlc-hook ()
  (setc lsp-log-io t)
  (setq lsp--show-message nil)
  (tlc-mode)
  (ol-prefer-tlc-xref)
  )

(defun ol-prefer-tlc-xref ()
  (setq-local xref-backend-functions '(tlc-xref-backend lsp--xref-backend t)))

(defun ol-tlc-xref-debug (result)
  (if result
      (message "tiny-lsp-client found definition")
    (message "tiny-lsp-client FAILED"))
  result)

(setc tlc-find-root-function 'tlc-dev-find-root-function)

(advice-add 'tlc-xref-backend
            :filter-return
            'ol-tlc-xref-debug)
(ol-define-normal-leader-key "ll" 'tlc-open-log-file)
(ol-define-normal-leader-key "li" 'tlc-info)
(ol-define-normal-leader-key "lr" 'tlc-restart-server)
(ol-define-normal-leader-key "ls" 'tlc-stop-server)

