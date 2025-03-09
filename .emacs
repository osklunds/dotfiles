
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
                        (file-expand-wildcards "~/.emacs_config/packages_own/*")
                        (file-expand-wildcards "~/.emacs_config/packages/*")
                        (file-expand-wildcards "~/.emacs_config/packages/*/src/*")
                        (file-expand-wildcards "~/.emacs_config/packages/*/clients")
                        (file-expand-wildcards "~/.emacs_config/packages/*/lisp"))
      )

(setq custom-file (concat user-emacs-directory "/custom.el"))

(setq vc-follow-symlinks t)

(setq load-prefer-newer t)

;; todo: after this was compiled for the first time, needed to restart emacs.
;; Consider doing what auto-compile-mode by magit author does.

(defun ol-compile-own ()
  (interactive)
  (save-window-excursion
    (byte-recompile-directory "~/.emacs_config/config" 0)
    (byte-recompile-directory "~/.emacs_config/packages_own" 0)))

(defun ol-compile-packages ()
  (interactive)
  (save-window-excursion
    (byte-recompile-directory "~/.emacs_config/packages" 0)))

(defun ol-compile-all ()
  (interactive)
  (ol-compile-own)
  (ol-compile-packages))

;; Compiling seems to load, and I don't want to load all packages, so
;; only compile my own, which are the most frequently changed.
(ol-compile-own)

(dolist (file (directory-files "~/.emacs_config/config/" nil "\\.el$"))
  (require (intern (file-name-sans-extension file))))

;; For some reason, isn't killed without this idle timer
(run-with-idle-timer 0 nil (lambda ()
                             (kill-buffer "*Compile-Log*")
                             (delete-other-windows)))

;; ---------------------------------------------------------------------------
;; Undo stuff needed just when loading emacs config
;; ---------------------------------------------------------------------------

(setq debug-on-error nil)
