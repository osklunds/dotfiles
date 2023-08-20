
(setq debug-on-error t)

;; ---------------------------------------------------------------------------
;; Setting up package loading
;; ---------------------------------------------------------------------------

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Copied/inspired from https://github.com/bling/dotemacs
(defun ol-require (package)
  (progn
    (unless (or (package-installed-p package)
                (require package nil 'noerror))
      (unless (assoc package package-archive-contents)
        (package-refresh-contents))
      (package-install package))
    (require package)))

;; Below is an alternative for "offline" usage
;; To load packages from a downloaded and extracted tar
;; Can also use expand-wildcard
;; (add-to-list 'load-path "~/.emacs_config/packages/magit-20230731.1514")
;; (add-to-list 'load-path "~/.emacs_config/packages/vdiff-magit-20220518.1948")
;; (add-to-list 'load-path "~/.emacs_config/packages/vdiff-20230621.201")
;;
;; (defun ol-require (package)
;;   (require package))

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
