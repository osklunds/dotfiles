
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
                        (file-expand-wildcards "~/.emacs_config/packages/*")
                        (file-expand-wildcards "~/.emacs_config/packages/*/lisp")))

(defun ol-require (package)
  (require package))

;; ---------------------------------------------------------------------------
;; Loading config
;; ---------------------------------------------------------------------------

(setq custom-file (concat user-emacs-directory "/custom.el"))

(setq vc-follow-symlinks t)

(dolist (file '("main"
                "rust"
                "haskell"
                "key_bindings"
                "colors"))
  (let ((path (concat "~/.emacs_config/config/" file ".el")))
    (load path)))
