
(setq debug-on-error t)

;; ---------------------------------------------------------------------------
;; Setting up package loading
;; ---------------------------------------------------------------------------

(setq load-path (append load-path (file-expand-wildcards "~/.emacs_config/packages/*")))
(setq load-path (add-to-list 'load-path "~/.emacs_config/packages/magit/lisp/"))
(setq load-path (add-to-list 'load-path "~/.emacs_config/packages/with-editor/lisp/"))

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
