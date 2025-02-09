
;; I'm experimenting with my own lsp client. Not ready to be made public yet :)

(add-to-list 'load-path "/home/oskar/own_repos/tiny-lsp-client/")

(require 'tiny-lsp-client)

(add-hook 'rust-mode-hook 'tlc-mode)

(defun ol-prefer-tlc-xref ()
  (setq-local xref-backend-functions '(tlc-xref-backend lsp--xref-backend t)))

(add-hook 'rust-mode-hook 'ol-prefer-tlc-xref)

(defun ol-tlc-xref-debug (result)
  (if result
      (message "tiny-lsp-client found definition")
    (message "tiny-lsp-client FAILED"))
  result)

(advice-add 'xref-backend-definitions
            :filter-return
            'ol-tlc-xref-debug)

