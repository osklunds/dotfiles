
(setq debug-on-error t)

(dolist (file '("main"
                "key_bindings"))
  (let ((path (concat "~/.emacs_config/config/" file ".el")))
    (load path)))
