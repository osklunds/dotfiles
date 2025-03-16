
;; -----------------------------------------------------------------------------
;; Project root and name of current buffer
;; -----------------------------------------------------------------------------

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

;; -----------------------------------------------------------------------------
;; List of projects
;; -----------------------------------------------------------------------------

(defvar ol-project-roots nil)

(defvar ol-projects-search-path '(("~/own_repos" . 1)
                                  ("~/others_repos" . 1)
                                  ("~/own_repos/dotfiles/.emacs_config/packages" . 1)
                                  ("~/Dropbox/Dokument")))

(defun ol-discover-projects ()
  (interactive)
  (setq ol-project-roots nil)
  (dolist (path ol-projects-search-path)
    (let* ((dir (car path))
           (depth (or (cdr-safe path) 0)))
      (ol-discover-projects-in-dir dir depth))))

(defun ol-discover-projects-in-dir (dir depth)
  (when (file-directory-p dir)
    (when-let* ((default-directory dir)
                (root (ol-project-root-compute)))
      (add-to-list 'ol-project-roots root))
    (when (> depth 0)
      (dolist (subdir (directory-files dir))
        (ol-discover-projects-in-dir (file-name-concat dir subdir) (- depth 1))))))

(defun ol-switch-to-project ()
  (interactive)
  (let ((project-root (completing-read
                       "Switch to project: "
                       ol-project-roots
                       nil ;; predicate
                       t ';; require-match
                       nil ;; initial input
                       'ol-switch-to-project)))
    (if (cl-member project-root ol-project-roots :test 'string-equal)
        (let ((default-directory project-root))
          (ol-dwim-find-file-name))
      (user-error "Bad project selection"))))

(provide 'ol-project)
