;; -*- lexical-binding: nil -*-

;; ---------------------------------------------------------------------------
;; Debugability
;; ---------------------------------------------------------------------------

(setq debug-on-error t)

(setq eval-expression-print-level 10000)
(setq eval-expression-print-length 10000)

;; -----------------------------------------------------------------------------
;; Load path
;; -----------------------------------------------------------------------------

(setq package-archives nil)
(setq custom-file (concat user-emacs-directory "/custom.el"))
(setq vc-follow-symlinks t)
(setq load-prefer-newer t)

(setq ol-repo-root
      (file-name-directory
       (file-truename
        (or load-file-name buffer-file-name))))
(setq ol-emacs-dir (file-name-concat ol-repo-root ".emacs_config"))

(add-to-list 'load-path (file-name-concat ol-emacs-dir "config"))
(add-to-list 'load-path (file-name-concat ol-emacs-dir "packages_own"))
(let ((default-directory (file-name-concat ol-emacs-dir "packages/")))
  (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory (file-name-concat ol-emacs-dir "packages_own/")))
  (normal-top-level-add-subdirs-to-load-path))

(defun ol-add-all-to-load-path ()
  (interactive)
  (let ((default-directory ol-emacs-dir))
    (normal-top-level-add-subdirs-to-load-path)))

;; -----------------------------------------------------------------------------
;; Loading packages and config
;; -----------------------------------------------------------------------------

;; Naive and simplified method
(defun ol-require-advice (feature &optional filename _noerror)
  (when-let ((filename (locate-file (format "%s.el" feature) load-path)))
    (when (string-prefix-p ol-repo-root filename)
      (byte-recompile-file filename nil 0))))

(advice-add 'require :before 'ol-require-advice)

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

(dolist (file (directory-files (file-name-concat ol-emacs-dir "config") nil "\\.el$"))
  (message "Loading '%s'" (file-name-sans-extension file))
  (require (intern (file-name-sans-extension file))))

;; For some reason, isn't killed without this idle timer
(run-with-idle-timer 0 nil (lambda ()
                             (when (buffer-live-p "*Compile-Log*")
                               (kill-buffer "*Compile-Log*"))
                             (delete-other-windows)))

;; ---------------------------------------------------------------------------
;; Undo stuff needed just when loading emacs config
;; ---------------------------------------------------------------------------

(setq debug-on-error nil)
