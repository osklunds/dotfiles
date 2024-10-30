
;; ---------------------------------------------------------------------------
;; Merge Survival Knife (WIP)
;; ---------------------------------------------------------------------------

(require 'vdiff)

;;;; ---------------------------------------------------------------------------
;;;; Minor mode
;;;; ---------------------------------------------------------------------------

(defvar msk-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode msk-mode
  "Minor mode for solving merge conflicts."
  :init-value nil
  :lighter " msk-mode"
  :keymap msk-mode-map
  :global t
  (cond
   (msk-mode (msk-start))
   (t        (msk-stop))))

;; 'conflict-area
;; 'entire-file
;; merge-commit
(defvar msk-variant nil)

(defvar msk-file nil)

(defun msk-mode-enable (&optional variant)
  (interactive "P")
  (setq msk-variant (or variant 'conflict-area))
  (setq msk-file (buffer-file-name))
  (msk-mode t))

(defun msk-mode-disable ()
  (interactive)
  (msk-mode -1))

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

;; TODOL Split more between state and separate vars. E.g. only temp buffers in
;; state list
(defvar msk-state nil)

(defvar msk-original-buffer nil)
(defvar msk-original-buffer-point nil)
(defvar msk-original-window-configuration nil)

;; TODO: Consider plist for these
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

(defun msk-put (key value)
  (put 'msk-state (intern key) value))

(defun msk-get (key)
  (get 'msk-state (intern key)))

(defun msk-list ()
  (symbol-plist 'msk-state))

(defun msk-clear-state ()
  (setplist 'msk-state nil))

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
         (msk-put "original" msk-original-buffer)
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
  (dolist (maybe-buffer (msk-list))
    (dolist (name '("BASE" "LOCAL" "REMOTE" "MERGED"))
      (when (bufferp maybe-buffer)
        (let ((bfn (buffer-name maybe-buffer)))
          (when (and bfn (string-match-p name bfn))
            (kill-buffer maybe-buffer))))))
  (msk-clear-state)
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
    ('conflict-area (unless (msk-find-next-conflict)
                      (user-error "No conflict found")))
    ('entire-file (unless (magit-merge-in-progress-p)
                    (user-error "Not merging")))))

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
    ('entire-file (msk-create-file-buffers))))

(defun msk-create-string-buffers ()
  (msk-populate-strings)
  (msk-create-string-buffer "LOCAL"  msk-local-string  t)
  (msk-create-string-buffer "BASE"   msk-base-string   t)
  (msk-create-string-buffer "REMOTE" msk-remote-string t)
  (msk-create-string-buffer "MERGED" msk-merged-string nil))

(defun msk-populate-strings ()
  (unless (looking-at-p msk-local-start-re)
    (error "Not looking at start, bug"))
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
      (msk-put name buffer))
    (msk-set-buffer-properties buffer read-only)))

(defun msk-set-buffer-properties (buffer read-only)
  (with-current-buffer buffer
    (display-line-numbers-mode t) ;; workaround due to unknwon bug
    (when read-only
      (read-only-mode))))

(defun msk-create-file-buffers ()
  (let* ((local-rev "HEAD")
         (remote-rev "MERGE_HEAD")
         (base-rev (magit-commit-p (magit-git-string "merge-base" local-rev remote-rev)))
         (merged-rev "{worktree}"))
    (msk-create-file-buffer "LOCAL"  local-rev)
    (msk-create-file-buffer "BASE"   base-rev)
    (msk-create-file-buffer "REMOTE" remote-rev)
    (msk-create-file-buffer "MERGED" merged-rev)))

(defun msk-create-file-buffer (name rev)
  (let* ((buffer-original (magit-find-file-noselect rev (buffer-file-name)))
         (buffer (make-indirect-buffer buffer-original name)))
    (msk-put name buffer)))

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
         (left-buffer (make-indirect-buffer (msk-get left) left-name))
         (right-buffer (make-indirect-buffer (msk-get right) right-name)))
    (msk-set-buffer-properties left-buffer t)
    (msk-set-buffer-properties right-buffer right-read-only)
    (msk-put left-name left-buffer)
    (msk-put right-name right-buffer)
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
        (switch-to-buffer (msk-get (msk-diff-name msk-left-top msk-right-top msk-left-top)))

        ;; right-top
        (select-window (split-window-right))
        (switch-to-buffer (msk-get (msk-diff-name msk-left-top msk-right-top msk-right-top)))

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
        (switch-to-buffer (msk-get msk-bottom)))

      (setq msk-window-configs (plist-put msk-window-configs
                                          window-config-key
                                          (current-window-configuration)
                                          'string-equal)))))

(defun msk-initial-refresh (left right)
  (let ((has-shown-key (concat "has-shown" left right)))
    (unless (msk-get has-shown-key)
      (msk-put has-shown-key t)
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
           (new-string (msk-get-solved-conflict-string)))
      (unless (string-equal old-string new-string)
        (cl-assert (= 1 (replace-string-in-region old-string new-string)))))))

(defun msk-get-solved-conflict-string ()
  (let ((string (with-current-buffer (msk-get "MERGED")
                  (buffer-substring-no-properties (point-min) (point-max)))))
    (unless (string-prefix-p "\n" string)
      (error "The merged string must begin with a newline"))
    (unless (string-suffix-p "\n" string)
      (error "The merged string must end with a newline"))
    (substring string 1 -1)))
  
