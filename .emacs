
;; ---------------------------------------------------------------------------
;; Debugability
;; ---------------------------------------------------------------------------

(setq debug-on-error t)

(setq eval-expression-print-level 10000)
(setq eval-expression-print-length 10000)

;; ---------------------------------------------------------------------------
;; Setting up package loading
;; ---------------------------------------------------------------------------

(setq load-path (append load-path
                        '("~/.emacs_config/config")
                        (file-expand-wildcards "~/.emacs_config/packages/*")
                        (file-expand-wildcards "~/.emacs_config/packages/*/src/*")
                        (file-expand-wildcards "~/.emacs_config/packages/*/clients")
                        (file-expand-wildcards "~/.emacs_config/packages/*/lisp")))

;; ---------------------------------------------------------------------------
;; Loading config
;; ---------------------------------------------------------------------------

(setq custom-file (concat user-emacs-directory "/custom.el"))

(setq vc-follow-symlinks t)

(dolist (file '("main"
                "rust"
                "haskell-config"
                "go"
                "java-config"
                "merge-survival-knife"
                "merge-commit-analyzer"
                "docker"
                "vterm-config"))
  (let ((path (concat "~/.emacs_config/config/" file ".el")))
    (load path)))

;; ---------------------------------------------------------------------------
;; Undo stuff needed just when loading emacs config
;; ---------------------------------------------------------------------------

(setq debug-on-error nil)
