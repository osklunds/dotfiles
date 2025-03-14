
;; ---------------------------------------------------------------------------
;; Debugability
;; ---------------------------------------------------------------------------

(setq debug-on-error t)

(setq eval-expression-print-level 10000)
(setq eval-expression-print-length 10000)

;; ---------------------------------------------------------------------------
;; Loading config
;; ---------------------------------------------------------------------------

(setq package-archives nil)
(setq custom-file (concat user-emacs-directory "/custom.el"))
(setq vc-follow-symlinks t)
(setq load-prefer-newer t)

(setq ol-repo-root
      (file-name-directory
       (file-truename
        (or load-file-name buffer-file-name))))
(setq ol-emacs-dir (file-name-concat ol-repo-root ".emacs_config"))

(let ((default-directory ol-emacs-dir))
  (normal-top-level-add-subdirs-to-load-path))

;; todo: after this was compiled for the first time, needed to restart emacs.
;; Consider doing what auto-compile-mode by magit author does.

(defun ol-compile-own ()
  (interactive)
  (save-window-excursion
    (byte-recompile-directory (file-name-concat ol-emacs-dir "config") 0)
    (byte-recompile-directory (file-name-concat ol-emacs-dir "packages_own") 0)))

(defun ol-compile-packages ()
  (interactive)
  (save-window-excursion
    (byte-recompile-directory (file-name-concat ol-emacs-dir "packages") 0)))

(defun ol-compile-all ()
  (interactive)
  (ol-compile-own)
  (ol-compile-packages))

;; Compiling seems to load, and I don't want to load all packages, so
;; only compile my own, which are the most frequently changed.
(ol-compile-own)

(dolist (file (directory-files (file-name-concat ol-emacs-dir "config") nil "\\.el$"))
  (require (intern (file-name-sans-extension file))))

;; For some reason, isn't killed without this idle timer
(run-with-idle-timer 0 nil (lambda ()
                             (kill-buffer "*Compile-Log*")
                             (delete-other-windows)))

;; ---------------------------------------------------------------------------
;; Undo stuff needed just when loading emacs config
;; ---------------------------------------------------------------------------

(setq debug-on-error nil)
