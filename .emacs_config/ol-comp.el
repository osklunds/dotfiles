
(setq load-path (append load-path
                        '("~/.emacs_config/config")
                        '("~/.emacs_config/packages_own/")
                        '("~/own_repos/tiny-lsp-client/")
                        (file-expand-wildcards "~/.emacs_config/packages/*")
                        (file-expand-wildcards "~/.emacs_config/packages/*/src/*")
                        (file-expand-wildcards "~/.emacs_config/packages/*/clients")
                        (file-expand-wildcards "~/.emacs_config/packages/*/lisp"))
      )

(setq byte-compile-error-on-warn t)
