
(require 'ol-projectile)

;; -----------------------------------------------------------------------------
;; Root
;; -----------------------------------------------------------------------------

(defun ol2-dwim-use-project-root (&optional prefer-project-root)
  (let ((root (projectile-project-root)))
    (cond
     ((and root prefer-project-root) root)
     ;; todo: don't have vterm here, but files aren't found if using project root
     ((cl-member major-mode '(dired-mode vterm-mode)) nil)
     (t root))))

;; -----------------------------------------------------------------------------
;; File name
;; -----------------------------------------------------------------------------

(defun ol2-dwim-find-file-name (&optional prefer-project-root)
  (interactive "P")
  (if-let ((root (ol2-dwim-use-project-root prefer-project-root)))
      (ol2-find-file-name root "project")
    (ol2-find-file-name default-directory "cwd")))

(defun ol2-find-file-name (dir prompt-dir-part)
  (interactive)
  (let* ((candidates (split-string (shell-command-to-string "rg --files") "\n" t))
         (selected (completing-read
                    "Find file name: "
                    candidates
                    nil
                    t
                    nil
                    'ol-find-file-name
                    )))
    (find-file selected)))

(provide 'ol-new-find)
