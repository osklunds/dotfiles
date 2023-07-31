
(setq debug-on-error t)

(dolist (file '("main"))
  (let ((path (concat user-emacs-directory "config/" file ".el")))
    (load path)))
