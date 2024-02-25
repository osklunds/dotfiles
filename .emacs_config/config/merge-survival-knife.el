
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
   (msk-mode (msk-mode-start))
   (t        (msk-mode-stop))))

(defun msk-mode-start ()
  (msk-start))

(defun msk-mode-stop ()
  (msk-stop))

(defun msk-mode-enable ()
  (interactive)
  (msk-mode t))

(defun msk-mode-disable ()
  (interactive)
  (msk-mode -1))

(provide 'msk-mode)

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
  (if (msk-find-next-conflict)
      (progn (msk-populate-strings)
             (msk-create-buffers)
             (msk-create-diffs)
             ;; Due to vdiff bug, need to skip refresh for the first diff
             (setq msk-skip-vdiff-refresh t)
             (msk-base-local)
             (setq msk-skip-vdiff-refresh nil))
    (message "No conflict found")))

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
  (msk-clear-state))

(defun msk-save-original-pos ()
  (msk-put "original-buffer" (current-buffer))
  (msk-put "original-point" (point)))

(defun msk-save-windows ()
  (msk-put "window-configuration" (current-window-configuration)))

(defun msk-restore-windows ()
  (if-let (windows (msk-get "window-configuration"))
      (set-window-configuration windows)
    (message "Warning: no window config found")))

;;;; ---------------------------------------------------------------------------
;;;; Finding a conflict
;;;; ---------------------------------------------------------------------------

(defun msk-find-next-conflict ()
  (when (smerge-find-conflict)
    (re-search-backward msk-local-start-re)))

;;;; ---------------------------------------------------------------------------
;;;; Populate the conflict strings
;;;; ---------------------------------------------------------------------------

(defun msk-populate-strings ()
  (unless (looking-at-p msk-local-start-re)
    (error "Not looking at start, bug"))
  (let* ((local  (msk-string-between-regexp msk-local-start-re  msk-local-end-re    nil))
         (base   (msk-string-between-regexp msk-local-end-re    msk-remote-start-re nil))
         (remote (msk-string-between-regexp msk-remote-start-re msk-remote-end-re   nil))
         (merged (msk-string-between-regexp msk-local-start-re  msk-remote-end-re   t)))
    (msk-put "local-string" local)
    (msk-put "base-string" base)
    (msk-put "remote-string" remote)
    (msk-put "merged-string" merged)
    (list local base remote merged)))

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

;;;; ---------------------------------------------------------------------------
;;;; Create buffers
;;;; ---------------------------------------------------------------------------

(defun msk-create-buffers ()
  (msk-create-buffer "LOCAL"  "local-string"  t)
  (msk-create-buffer "BASE"   "base-string"   t)
  (msk-create-buffer "REMOTE" "remote-string" t)
  (msk-create-buffer "MERGED" "merged-string" nil))

(defun msk-create-buffer (name string-key read-only)
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (insert "\n") ;; workaround due to vdiff bug
      (insert (msk-get string-key))
      (insert "\n") ;; vdiff wants all to end in newline
      (msk-put name buffer))
    (msk-set-buffer-properties buffer read-only)))

(defun msk-set-buffer-properties (buffer read-only)
  (with-current-buffer buffer
    (display-line-numbers-mode t) ;; workaround due to unknwon bug
    (when read-only
      (read-only-mode))))

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

(defvar msk-skip-vdiff-refresh nil)

(defun msk-change-view (left right)
  (let* ((left-buffer-name (msk-diff-name left right left))
         (right-buffer-name (msk-diff-name left right right))
         (pair-key (concat "has-shown" left right)))
    (delete-other-windows)
    (switch-to-buffer (msk-get left-buffer-name))
    (split-window-right)
    (other-window 1)
    (switch-to-buffer (msk-get right-buffer-name))
    (unless (msk-get pair-key)
      (msk-put pair-key t)
      (unless msk-skip-vdiff-refresh
        (vdiff-refresh)))))

(defun msk-base-local ()
  (interactive)
  (msk-change-view "BASE" "LOCAL"))

(defun msk-base-remote ()
  (interactive)
  (msk-change-view "BASE" "REMOTE"))

(defun msk-local-remote ()
  (interactive)
  (msk-change-view "LOCAL" "REMOTE"))

(defun msk-local-merged ()
  (interactive)
  (msk-change-view "LOCAL" "MERGED"))

(defun msk-remote-merged ()
  (interactive)
  (msk-change-view "REMOTE" "MERGED"))

(defun msk-original-buffer (&optional arg)
  (interactive "P")
  (if arg
        (select-window (split-root-window-below))
    (delete-other-windows))
  (switch-to-buffer msk-original-buffer))

;;;; ---------------------------------------------------------------------------
;;;; Saving the solved conflict
;;;; ---------------------------------------------------------------------------

(defun msk-save-solved-conflict ()
  (switch-to-buffer (msk-get "original-buffer"))
  (goto-char (msk-get "original-point"))
  (cl-assert (msk-find-next-conflict))
  (let* ((old-string (msk-get "merged-string"))
         (new-string (msk-get-solved-conflict-string)))
    (unless (string-equal old-string new-string)
      (cl-assert (= 1 (replace-string-in-region old-string new-string))))))

(defun msk-get-solved-conflict-string ()
  (let ((string (with-current-buffer (msk-get "MERGED")
                  (buffer-substring-no-properties (point-min) (point-max)))))
    (unless (string-prefix-p "\n" string)
      (error "The merged string must begin with a newline"))
    (unless (string-suffix-p "\n" string)
      (error "The merged string must end with a newline"))
    (substring string 1 -1)))
  
