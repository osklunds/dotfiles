
(defvar ol-project-root-functions '(ol-project-git-root))

(defun ol-project-git-root ()
  (when-let ((root (locate-dominating-file default-directory ".git")))
    (file-truename root)))

(defvar-local ol-project-root 'unknown)

(defun ol-project-root ()
  (if (eq 'unknown ol-project-root)
      (setq ol-project-root (ol-project-root-compute))
    ol-project-root))

(defun ol-project-root-compute ()
  (run-hook-with-args-until-success 'ol-project-root-functions))

(defvar-local ol-project-name 'unknown)

(defun ol-project-name ()
  (if (eq 'unknown ol-project-name)
      (setq ol-project-name (ol-project-name-compute))
    ol-project-name))

(defun ol-project-name-compute ()
  (when-let ((root (ol-project-root)))
    (file-name-nondirectory (directory-file-name root))))
    

(provide 'ol-project)
