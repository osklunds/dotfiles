;; -*- lexical-binding: nil -*-

(require 'ol-evil)

(require 'project)

;; -----------------------------------------------------------------------------
;; Project root and name of current buffer
;; -----------------------------------------------------------------------------

(defvar ol-project-root-cache nil)
(defvar ol-project-name-cache nil)

(defun ol-project-clear-cache ()
  "If project info seems wrong for a buffer, try clearing the cache.
Recalculating is probably not a big deal. But if frequent calls from
modeline can be good to cache in a hashmap."
  (interactive)
  (setq ol-project-root-cache (make-hash-table :test 'equal))
  (setq ol-project-name-cache (make-hash-table :test 'equal)))

(ol-project-clear-cache)

(defun ol-project-root ()
  (let ((cached (gethash default-directory ol-project-root-cache 'not-found)))
    (if (equal cached 'not-found)
        (puthash default-directory (ol-project-root-compute) ol-project-root-cache)
      cached)))

(defun ol-project-root-compute ()
  (run-hook-with-args-until-success 'ol-project-root-functions))

(defvar ol-project-root-functions '(ol-project-marker-file-root
                                    ol-project-git-root
                                    ))

(defun ol-project-marker-file-root ()
  (when-let ((root (locate-dominating-file default-directory ".emacs-project")))
    (file-truename root)))

(defun ol-project-git-root ()
  (when-let ((root (locate-dominating-file default-directory ".git")))
    (file-truename root)))

(defun ol-project-name ()
  (let ((cached (gethash default-directory ol-project-name-cache 'not-found)))
    (if (equal cached 'not-found)
        (puthash default-directory (ol-project-name-compute) ol-project-name-cache)
      cached)))

(defun ol-project-name-compute ()
  (when-let ((root (ol-project-root)))
    (file-name-nondirectory (directory-file-name root))))

;; -----------------------------------------------------------------------------
;; List of projects
;; -----------------------------------------------------------------------------

(defvar ol-project-roots nil)

(defvar ol-projects-search-path '(("~/own_repos" . 1)
                                  ("~/others_repos" . 1)
                                  ("~/own_repos/dotfiles/.emacs_config" . 2)
                                  ("~/Dokument")))

(defun ol-discover-projects ()
  (interactive)
  (setq ol-project-roots nil)
  ;; If a buffer has incorrectly been assigned a project, give it a new chance
  (ol-project-clear-cache)
  (dolist (path ol-projects-search-path)
    (let* ((dir (file-truename (car path)))
           (depth (or (cdr-safe path) 0)))
      (ol-discover-projects-in-dir dir depth))))

(defun ol-discover-projects-in-dir (dir depth)
  (when (file-directory-p dir)
    (when-let* ((default-directory dir)
                (root (ol-project-root)))
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
    (ol-switch-to-project-by-root project-root)))

;; Set in ol-find-files.el to avoid circular dependency
(defvar ol-switch-to-project-action nil)

(defun ol-switch-to-project-by-root (root)
  (if (cl-member (file-truename root) ol-project-roots :test 'string-equal)
      (let ((default-directory root))
        (funcall ol-switch-to-project-action))
    (user-error "not a project")))

(ol-discover-projects)

;; -----------------------------------------------------------------------------
;; project.el integration
;; -----------------------------------------------------------------------------

(cl-defmethod project-root ((project (head ol-project)))
  (cdr project))

(defun project-ol-project (dir)
  (when-let* ((default-directory dir)
              (root (ol-project-root)))
    (cons 'ol-project root)))

(add-hook 'project-find-functions 'project-ol-project)

;; -----------------------------------------------------------------------------
;; Keybinds
;; -----------------------------------------------------------------------------

(ol-define-key ol-normal-leader-map "p p" 'ol-switch-to-project)
(ol-define-key ol-normal-leader-map "p d" 'ol-switch-to-dotfiles)
(ol-define-key ol-normal-leader-map "p s" 'ol-discover-projects)

(defun ol-switch-to-dotfiles ()
  (interactive)
  (ol-switch-to-project-by-root "~/own_repos/dotfiles/"))

(provide 'ol-project)
