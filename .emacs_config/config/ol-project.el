
(defvar ol-project-root-functions '(ol-project-git-root))

(defun ol-project-git-root ()
  (when-let ((root (locate-dominating-file default-directory ".git")))
    (file-truename root)))

(defun ol-project-root ()
  (run-hook-with-args-until-success 'ol-project-root-functions))

(defun ol-project-name ()
  (when-let ((root (ol-project-root)))
    (file-name-nondirectory (directory-file-name root))))
    

(provide 'ol-project)
