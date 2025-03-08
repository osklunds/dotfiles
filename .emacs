
;; ---------------------------------------------------------------------------
;; Debugability
;; ---------------------------------------------------------------------------

(setq debug-on-error t)

(setq eval-expression-print-level 10000)
(setq eval-expression-print-length 10000)

;; ---------------------------------------------------------------------------
;; Loading config
;; ---------------------------------------------------------------------------

(setq load-path (append load-path
                        '("~/.emacs_config/config")
                        '("~/.emacs_config/packages_own/")
                        '("~/own_repos/tiny-lsp-client/")
                        (file-expand-wildcards "~/.emacs_config/packages/*")
                        (file-expand-wildcards "~/.emacs_config/packages/*/src/*")
                        (file-expand-wildcards "~/.emacs_config/packages/*/clients")
                        (file-expand-wildcards "~/.emacs_config/packages/*/lisp"))
      )

(setq custom-file (concat user-emacs-directory "/custom.el"))

(setq vc-follow-symlinks t)

(save-window-excursion
  (byte-recompile-directory "~/.emacs_config/config" 0)
  (byte-recompile-directory "~/.emacs_config/packages" 0)
  (byte-recompile-directory "~/.emacs_config/packages_own" 0))

(dolist (file (directory-files "~/.emacs_config/config/" nil "\\.el$"))
  (require (intern (file-name-sans-extension file))))

;; ---------------------------------------------------------------------------
;; Undo stuff needed just when loading emacs config
;; ---------------------------------------------------------------------------

(setq debug-on-error nil)
