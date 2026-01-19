;; -*- lexical-binding: nil -*-

;; -----------------------------------------------------------------------------
;; Merge Commit Analyzer (part of Merge Survival Knife)
;;------------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; Minor mode
;;;;----------------------------------------------------------------------------

(defvar mca-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode mca-mode
  "Minor mode for analying merge commits."
  :init-value nil
  :lighter "mca-mode"
  :keymap mca-mode-map
  :global t
  (cond
   (mca-mode (mca-start))
   (t        (mca-stop))))

;;;; ---------------------------------------------------------------------------
;;;; Main
;;;;----------------------------------------------------------------------------

(defvar mca-original-window-configuration nil)
(defvar mca-buffers nil)

(defun mca-start ()
  (interactive)
  (setq mca-original-window-configuration (current-window-configuration))

  (let* ((commit magit-buffer-revision-hash)
         (parents (magit-commit-parents commit)))
    (unless (length= parents 2)
      (user-error "Can only be used for commits with exactly 2 parents"))

    (pcase-let* ((`(,p1 ,p2) parents)
                 (merge-base (msk-merge-base p1 p2)))

      (delete-other-windows)
      (mca-create-diff-buffer "BASE..LOCAL" merge-base p1)

      (select-window (split-window-right))
      (mca-create-diff-buffer "BASE..REMOTE" merge-base p2)

      (select-window (split-root-window-below))
      (mca-create-diff-buffer "REMOTE..MERGED" p2 commit)

      (select-window (split-window-right))
      (mca-create-diff-buffer "LOCAL..MERGED" p1 commit)
      )))

(defun mca-create-diff-buffer (name commit1 commit2)
  (let ((magit-display-buffer-function
         (lambda (buffer) (display-buffer buffer '(display-buffer-same-window)))))
    (magit-diff-setup-buffer (concat commit1 ".." commit2) nil nil nil 'committed 'locked)
    (magit-section-hide-children magit-root-section)
    (magit-section-show (magit-current-section))
    (rename-buffer name))
  (add-to-list 'mca-buffers (current-buffer)))

(defun mca-stop ()
  (interactive)
  (dolist (buffer mca-buffers)
    (kill-buffer buffer))
  (setq mca-buffers nil)
  (set-window-configuration mca-original-window-configuration)
  (setq mca-original-window-configuration nil))

;; todos:
;; set buffer names to LOCAL, RMEOTE etc when doing vdiff
;; calculate a list of all files changed in both BASE..LOCAL and BASE..REMOTE
;; or LOCAL..MERGED and REMOTE..MERGED

(provide 'merge-commit-analyzer)
