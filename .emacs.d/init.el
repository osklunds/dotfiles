
(setq debug-on-error t)

(dolist (file '("main"
                "key_bindings"))
  (let ((path (concat user-emacs-directory "config/" file ".el")))
    (load path)))
