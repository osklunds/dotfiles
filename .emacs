
(setq debug-on-error t)

;; To load packages from a downloaded and extracted tar
;; (add-to-list 'load-path "~/.emacs_config/packages/magit-20230731.1514")
;; (add-to-list 'load-path "~/.emacs_config/packages/vdiff-magit-20220518.1948")
;; (add-to-list 'load-path "~/.emacs_config/packages/vdiff-20230621.201")

(dolist (file '("main"
                "key_bindings"))
  (let ((path (concat "~/.emacs_config/config/" file ".el")))
    (load path)))
