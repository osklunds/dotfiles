
;; ---------------------------------------------------------------------------
;; Merge Survival Knife
;; ---------------------------------------------------------------------------

(require 'vdiff)
(require 'magit)
(require 'vdiff-magit)

;;;; ---------------------------------------------------------------------------
;;;; Minor mode
;;;; ---------------------------------------------------------------------------

(defvar msk-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode msk-mode
  "Minor mode for solving merge conflicts."
  :init-value nil
  :lighter "msk-mode"
  :keymap msk-mode-map
  :global t
  (cond
   (msk-mode (msk-start))
   (t        (msk-stop))))

;; 'conflict-area
;; 'entire-file
;; ('merge-commit merge-commit)
(defvar msk-variant nil)

(defvar msk-file nil)

(defun msk-mode-enable (&optional variant file)
  (interactive "P")
  (setq msk-variant (or variant 'conflict-area))
  (setq msk-file (or file (buffer-file-name) magit-buffer-file-name))
  (unless (or (equal msk-variant 'conflict-area) msk-file)
    (user-error "no file provided"))
  (msk-mode t))

(defun msk-mode-dwim ()
  (interactive)
  (cond
   ((and (equal major-mode 'magit-revision-mode)
         magit-buffer-revision-hash
         (msk-has-2-parents magit-buffer-revision-hash)
         (magit-current-file))
    (let ((rev magit-buffer-revision-hash))
      (msk-mode-enable (list 'merge-commit rev) (magit-current-file))))

   ((and (equal major-mode 'magit-revision-mode)
         magit-buffer-revision-hash
         (msk-has-2-parents magit-buffer-revision-hash))
    (mca-mode))

   ((msk-inside-conflict-p)
    (msk-mode-enable 'conflict-area))

   ((magit-merge-in-progress-p)
    (msk-mode-enable 'entire-file))

   (t (user-error "Couldn't read your mind"))))

(defun msk-inside-conflict-p ()
  (save-excursion
    (beginning-of-line)
    (let ((start-or-end
           (concat "\\(" msk-local-start-re "\\)\\|\\(" msk-remote-end-re "\\)")))
    (if (looking-at-p start-or-end)
        t
      (re-search-backward start-or-end nil 'no-error)
      (looking-at-p msk-local-start-re)))))

(provide 'merge-survival-knife)

(defun msk-cant-save-reminder ()
  (interactive)
  (user-error "Can't save temp buffers in msk-mode"))

;;;; ---------------------------------------------------------------------------
;;;; Constants
;;;; ---------------------------------------------------------------------------

(defconst msk-local-start-re "^<<<<<<<")
(defconst msk-local-end-re "^|||||||")
(defconst msk-remote-start-re "^=======")
(defconst msk-remote-end-re "^>>>>>>>")

;;;; ---------------------------------------------------------------------------
;;;; State
;;;; ---------------------------------------------------------------------------

(defvar msk-buffer-lookup-list nil)
(defvar msk-shown-diffs nil)

(defvar msk-buffers-to-kill nil)

(defvar msk-original-buffer nil)
(defvar msk-original-buffer-point nil)
(defvar msk-original-window-configuration nil)

(defvar msk-local-string nil)
(defvar msk-base-string nil)
(defvar msk-remote-string nil)
(defvar msk-merged-string nil)

(defvar msk-window-configs nil)

;; View
(defvar msk-left-top nil)
(defvar msk-right-top nil)
(defvar msk-left-bottom nil)
(defvar msk-right-bottom nil)
(defvar msk-bottom nil)

(defun msk-put-buffer (key value)
  (add-to-list 'msk-buffer-lookup-list `(,key . ,value)))

(defun msk-get-buffer (key)
  (cdr (assoc key msk-buffer-lookup-list 'string-equal)))

(msk-put-buffer "test" (current-buffer))
(cl-assert (equal (current-buffer) (msk-get-buffer "test")))
(setq msk-buffer-lookup-list nil)

;;;; ---------------------------------------------------------------------------
;;;; Start and stop
;;;; ---------------------------------------------------------------------------

(defun msk-start ()
  (interactive)
  (msk-cleanup)
  (msk-save-windows)
  (msk-save-original-pos)
  (setq msk-original-buffer (current-buffer))
  (msk-check-preconditions)
  (progn (msk-create-buffers)
         (msk-create-diffs)
         ;; Due to vdiff bug, need to skip refresh for the first diff
         (setq msk-skip-vdiff-refresh t)
         (msk-base-local)
         (setq msk-skip-vdiff-refresh nil)))

(defun msk-stop ()
  (interactive)
  (msk-save-solved-conflict)
  (msk-restore-windows)
  (msk-cleanup))

(defun msk-cleanup ()
  (setq msk-buffer-lookup-list nil)
  (setq msk-shown-diffs nil)

  (dolist (buffer msk-buffers-to-kill)
    (kill-buffer buffer))
  (setq msk-buffers-to-kill nil)
  
  (setq msk-original-buffer nil)
  (setq msk-original-buffer-point nil)
  (setq msk-original-window-configuration nil)

  (setq msk-local-string nil)
  (setq msk-base-string nil)
  (setq msk-remote-string nil)
  (setq msk-merged-string nil)

  (setq msk-window-configs nil)

  (setq msk-left-top nil)
  (setq msk-right-top nil)
  (setq msk-left-bottom nil)
  (setq msk-right-bottom nil)
  (setq msk-bottom nil))

(defun msk-save-original-pos ()
  (setq msk-original-buffer-point (point)))

(defun msk-save-windows ()
  (setq msk-original-window-configuration (current-window-configuration)))

(defun msk-restore-windows ()
  (if msk-original-window-configuration
      (set-window-configuration msk-original-window-configuration)
    (message "Warning: no window config found")))

;;;; ---------------------------------------------------------------------------
;;;; Preconditions
;;;;----------------------------------------------------------------------------

(defun msk-check-preconditions ()
  (pcase msk-variant
    ('conflict-area
     (unless (msk-inside-conflict-p)
       (user-error "No conflict found")))
    ('entire-file
     (unless (magit-merge-in-progress-p)
       (user-error "Not merging")))
    (`(merge-commit ,merge-commit)
     (unless (msk-has-2-parents merge-commit)
       (user-error "Selected commit doesn't have 2 parents")))))

(defun msk-has-2-parents (commit)
  (length= (magit-commit-parents commit) 2))

;;;; ---------------------------------------------------------------------------
;;;; Finding a conflict
;;;; ---------------------------------------------------------------------------

(defun msk-find-next-conflict ()
  (when (smerge-find-conflict)
    (re-search-backward msk-local-start-re)))

;;;; ---------------------------------------------------------------------------
;;;; Create buffers
;;;; ---------------------------------------------------------------------------

(defun msk-create-buffers ()
  (pcase msk-variant
    ('conflict-area (msk-create-string-buffers))
    (_ (msk-create-file-buffers))))

(defun msk-create-string-buffers ()
  (msk-populate-strings)
  (msk-create-string-buffer "LOCAL"  msk-local-string  t)
  (msk-create-string-buffer "BASE"   msk-base-string   t)
  (msk-create-string-buffer "REMOTE" msk-remote-string t)
  (msk-create-string-buffer "MERGED" msk-merged-string nil))

(defun msk-populate-strings ()
  (unless (msk-inside-conflict-p)
    (error "No conflict, bug"))
  (msk-find-next-conflict)
  (setq msk-local-string  (msk-string-between-regexp msk-local-start-re  msk-local-end-re    nil))
  (setq msk-base-string   (msk-string-between-regexp msk-local-end-re    msk-remote-start-re nil))
  (setq msk-remote-string (msk-string-between-regexp msk-remote-start-re msk-remote-end-re   nil))
  (setq msk-merged-string (msk-string-between-regexp msk-local-start-re  msk-remote-end-re   t)))

(defun msk-string-between-regexp (start end inclusive)
  (save-excursion
    (let* ((start-point nil)
           (end-point nil))
      (re-search-forward start)
      (unless inclusive
        (next-line))
      (beginning-of-line)
      (setq start-point (point))
      (re-search-forward end)
      (unless inclusive
        (previous-line))
      (end-of-line)
      (setq end-point (point))
      (buffer-substring-no-properties start-point end-point))))

(defun msk-create-string-buffer (name string read-only)
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (insert "\n") ;; workaround due to vdiff bug
      (insert string)
      (insert "\n") ;; vdiff wants all to end in newline
      (msk-put-buffer name buffer)
      (add-to-list 'msk-buffers-to-kill buffer))
    (msk-set-buffer-properties buffer read-only)))

(defun msk-set-buffer-properties (buffer read-only)
  (with-current-buffer buffer
    (display-line-numbers-mode t) ;; workaround due to unknwon bug
    (when read-only
      (read-only-mode))))

(defun msk-create-file-buffers ()
  (pcase-let ((`(,local-rev ,remote-rev ,base-rev ,merged-rev)
               (pcase msk-variant
                 ('entire-file
                  (list
                   "HEAD"
                   "MERGE_HEAD"
                   (msk-merge-base "HEAD" "MERGE_HEAD")
                   "{worktree}"))
                 (`(merge-commit ,merge-commit)
                  (pcase-let ((`(,p1 ,p2) (magit-commit-parents merge-commit)))
                    (list
                     p1
                     p2
                     (magit-commit-p (magit-git-string "merge-base" p1 p2))
                     merge-commit))))))
    (msk-create-file-buffer "LOCAL"  local-rev)
    (msk-create-file-buffer "BASE"   base-rev)
    (msk-create-file-buffer "REMOTE" remote-rev)
    (msk-create-file-buffer "MERGED" merged-rev)))

(defun msk-merge-base (c1 c2)
  (magit-commit-p (magit-git-string "merge-base" c1 c2)))

(defun msk-create-file-buffer (name rev)
  (let* ((original-buffer-list (buffer-list))
         (raw-buffer (magit-find-file-noselect rev msk-file))
         (buffer (make-indirect-buffer raw-buffer name 'clone)))
    (msk-put-buffer name buffer)
    (add-to-list 'msk-buffers-to-kill buffer)
    (unless (cl-member raw-buffer original-buffer-list)
      (add-to-list 'msk-buffers-to-kill raw-buffer))))

;;;; ---------------------------------------------------------------------------
;;;; Create diffs
;;;; ---------------------------------------------------------------------------

;; TODO: Create a "4 way diff" with BL and RM are on top, and BR and LM are on top
(defun msk-create-diffs ()
  ;; By inhibiting diff vdiff seems to work nicer. No more warnings about
  ;; sentinel and first diff not showing. It could be that when concurrent async
  ;; processes things are messed up.
  (setq vdiff--inhibit-diff-update t)
  (msk-create-diff "BASE"   "LOCAL"  t)
  (msk-create-diff "BASE"   "REMOTE" t)
  (msk-create-diff "LOCAL"  "REMOTE" t)
  (msk-create-diff "LOCAL"  "MERGED" nil)
  (msk-create-diff "REMOTE" "MERGED" nil)
  (setq vdiff--inhibit-diff-update nil))

(defun msk-create-diff (left right right-read-only)
  (let* ((left-name (msk-diff-name left right left))
         (right-name (msk-diff-name left right right))
         (left-buffer (make-indirect-buffer (msk-get-buffer left) left-name))
         (right-buffer (make-indirect-buffer (msk-get-buffer right) right-name)))
    (msk-set-buffer-properties left-buffer t)
    (msk-set-buffer-properties right-buffer right-read-only)

    (msk-put-buffer left-name left-buffer)
    (add-to-list 'msk-buffers-to-kill left-buffer)
    (msk-put-buffer right-name right-buffer)
    (add-to-list 'msk-buffers-to-kill right-buffer)

    (vdiff-buffers left-buffer right-buffer)))

(defun msk-diff-name (left right this)
  (concat this " (" (substring left 0 1) (substring right 0 1) ")"))

;;;; ---------------------------------------------------------------------------
;;;; Change views
;;;; ---------------------------------------------------------------------------

(defun msk-set-view (left-top right-top left-bottom right-bottom bottom)
  (setq msk-left-top left-top)
  (setq msk-right-top right-top)
  (setq msk-left-bottom left-bottom)
  (setq msk-right-bottom right-bottom)
  (setq msk-bottom bottom))

(defun msk-refresh-view ()
  (let ((window-config-key (format "%s %s %s %s %s"
                                   msk-left-top
                                   msk-right-top
                                   msk-left-bottom
                                   msk-right-bottom
                                   msk-bottom)))
    (if-let (window-config (plist-get msk-window-configs window-config-key 'string-equal))
        (set-window-configuration window-config)

      ;; Prepare - get to known state
      (delete-other-windows)

      (when msk-left-top
        (cl-assert msk-right-top)
        ;; left-top
        (switch-to-buffer (msk-get-buffer (msk-diff-name msk-left-top msk-right-top msk-left-top)))

        ;; right-top
        (select-window (split-window-right))
        (switch-to-buffer (msk-get-buffer (msk-diff-name msk-left-top msk-right-top msk-right-top)))

        (msk-initial-refresh msk-left-top msk-right-top))

      (when msk-left-bottom
        (cl-assert msk-right-bottom)

        ;; left-bottom
        (select-window (split-root-window-below))
        (switch-to-buffer (msk-diff-name msk-left-bottom msk-right-bottom msk-left-bottom))

        ;; right-bottom
        (select-window (split-window-right))
        (switch-to-buffer (msk-diff-name msk-left-bottom msk-right-bottom msk-right-bottom))

        (msk-initial-refresh msk-left-bottom msk-right-bottom))

      (when msk-bottom
        ;; bottom
        (select-window (split-root-window-below))
        (switch-to-buffer (msk-get-buffer msk-bottom)))

      (setq msk-window-configs (plist-put msk-window-configs
                                          window-config-key
                                          (current-window-configuration)
                                          'string-equal)))))

(defun msk-initial-refresh (left right)
  (let ((has-shown-key (concat "has-shown" left right)))
    (unless (cl-member has-shown-key msk-shown-diffs :test 'string-equal)
      (add-to-list 'msk-shown-diffs has-shown-key)
      (unless msk-skip-vdiff-refresh
        (vdiff-refresh)))))

(defvar msk-skip-vdiff-refresh nil)

(defun msk-base-local ()
  (interactive)
  (msk-set-view "BASE" "LOCAL" nil nil msk-bottom)
  (msk-refresh-view))

(defun msk-base-remote ()
  (interactive)
  (msk-set-view "BASE" "REMOTE" nil nil msk-bottom)
  (msk-refresh-view))

(defun msk-local-remote ()
  (interactive)
  (msk-set-view "LOCAL" "REMOTE" nil nil msk-bottom)
  (msk-refresh-view))

(defun msk-local-merged ()
  (interactive)
  (msk-set-view "LOCAL" "MERGED" nil nil msk-bottom)
  (msk-refresh-view))

(defun msk-remote-merged ()
  (interactive)
  (msk-set-view "REMOTE" "MERGED" nil nil msk-bottom)
  (msk-refresh-view))

(defun msk-local-changes-compare ()
  (interactive)
  (msk-set-view "BASE" "LOCAL" "REMOTE" "MERGED" msk-bottom)
  (msk-refresh-view))

(defun msk-remote-changes-compare ()
  (interactive)
  (msk-set-view "BASE" "REMOTE" "LOCAL" "MERGED" msk-bottom)
  (msk-refresh-view))

(defun msk-original-buffer ()
  (interactive)
  (if (string-equal msk-bottom "original")
      (setq msk-bottom nil)
    (setq msk-bottom "original"))
  (msk-set-view msk-left-top msk-right-top msk-left-bottom msk-right-bottom msk-bottom)
  (msk-refresh-view))

(defun msk-merged-buffer ()
  (interactive)
  (if (string-equal msk-bottom "MERGED")
      (setq msk-bottom nil)
    (setq msk-bottom "MERGED"))
  (msk-set-view msk-left-top msk-right-top msk-left-bottom msk-right-bottom msk-bottom)
  (msk-refresh-view))

;;;; ---------------------------------------------------------------------------
;;;; Saving the solved conflict
;;;; ---------------------------------------------------------------------------

(defun msk-save-solved-conflict ()
  (when (equal msk-variant 'conflict-area)
    (switch-to-buffer msk-original-buffer)
    (goto-char msk-original-buffer-point)
    (cl-assert (msk-find-next-conflict))
    (let* ((old-string msk-merged-string)
           (new-string (msk-get-buffer-solved-conflict-string)))
      (unless (string-equal old-string new-string)
        (cl-assert (= 1 (replace-string-in-region old-string new-string)))))))

(defun msk-get-buffer-solved-conflict-string ()
  (let ((string (with-current-buffer (msk-get-buffer "MERGED")
                  (buffer-substring-no-properties (point-min) (point-max)))))
    (unless (string-prefix-p "\n" string)
      (error "The merged string must begin with a newline"))
    (unless (string-suffix-p "\n" string)
      (error "The merged string must end with a newline"))
    (substring string 1 -1)))
  
