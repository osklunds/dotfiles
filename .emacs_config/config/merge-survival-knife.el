
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

(provide 'merge-survival-knife)

(defun msk-cant-save-reminder ()
  (interactive)
  (user-error "Can't save temp buffers"))

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
(defvar msk-show-bottom-buffer nil)

(defvar msk-original-buffer nil)
(defvar msk-original-buffer-point nil)
(defvar msk-original-window-configuration nil)

(defvar msk-local-string nil)
(defvar msk-base-string nil)
(defvar msk-remote-string nil)
(defvar msk-merged-string nil)

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
  (msk-clear-state)
  (setq msk-original-buffer nil)
  (setq msk-original-buffer-point nil)
  (setq msk-original-window-configuration nil)

  (setq msk-local-string nil)
  (setq msk-base-string nil)
  (setq msk-remote-string nil)
  (setq msk-merged-string nil)

  (setq msk-show-bottom-buffer nil))

(defun msk-save-original-pos ()
  (setq msk-original-buffer-point (point)))

(defun msk-save-windows ()
  (setq msk-original-window-configuration (current-window-configuration)))

(defun msk-restore-windows ()
  (if msk-original-window-configuration
      (set-window-configuration msk-original-window-configuration)
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

;;;; ---------------------------------------------------------------------------
;;;; Create buffers
;;;; ---------------------------------------------------------------------------

(defun msk-create-buffers ()
  (msk-create-buffer "LOCAL"  msk-local-string  t)
  (msk-create-buffer "BASE"   msk-base-string   t)
  (msk-create-buffer "REMOTE" msk-remote-string t)
  (msk-create-buffer "MERGED" msk-merged-string nil))

(defun msk-create-buffer (name string read-only)
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

(defun msk-change-view (left right &optional hide-bottom)
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
        (vdiff-refresh)))
    (when hide-bottom
      (setq msk-show-bottom-buffer nil))
    (pcase msk-show-bottom-buffer
      ('original (progn
                   (msk-original-buffer t)
                   (other-window 2)))
      ('merged (progn
                 (msk-merged-buffer t)
                 (other-window 2)))
      (_ nil))))

(defun msk-base-local (&optional hide-bottom)
  (interactive "P")
  (msk-change-view "BASE" "LOCAL" hide-bottom))

(defun msk-base-remote (&optional hide-bottom)
  (interactive "P")
  (msk-change-view "BASE" "REMOTE" hide-bottom))

(defun msk-local-remote (&optional hide-bottom)
  (interactive "P")
  (msk-change-view "LOCAL" "REMOTE" hide-bottom))

(defun msk-local-merged (&optional hide-bottom)
  (interactive "P")
  (msk-change-view "LOCAL" "MERGED" hide-bottom))

(defun msk-remote-merged (&optional hide-bottom)
  (interactive "P")
  (msk-change-view "REMOTE" "MERGED" hide-bottom))

(defun msk-original-buffer (&optional arg)
  (interactive "P")
  (if arg
      (setq msk-show-bottom-buffer 'original)
    (setq msk-show-bottom-buffer nil))
  (if arg
      (progn
        (select-window (split-root-window-below))
          (switch-to-buffer msk-original-buffer))
    (if-let ((window (get-buffer-window msk-original-buffer)))
        (delete-window window)
      (delete-other-windows)
      (switch-to-buffer msk-original-buffer))))

(defun msk-merged-buffer (&optional keep-others)
  (interactive "P")
  (if keep-others
      (setq msk-show-bottom-buffer 'merged)
    (setq msk-show-bottom-buffer nil))
  (if keep-others
      (progn
        (select-window (split-root-window-below))
        (switch-to-buffer (msk-get "MERGED")))
    (if-let ((window (get-buffer-window (msk-get "MERGED"))))
        (delete-window window)
      (delete-other-windows)
      (switch-to-buffer (msk-get "MERGED")))))

;; TODO: There's lots of duplication here and with the normal change-view.
;; When I've evaluated whether this is useful, I'll try to do something about
;; it.
(defun msk-change-to-4-way-view (left-top right-top left-bottom right-bottom)
  (let* ((left-top-buffer-name (msk-diff-name left-top right-top left-top))
         (right-top-buffer-name (msk-diff-name left-top right-top right-top))
         (pair-key-top (concat "has-shown" left-top right-top))
         (left-bottom-buffer-name (msk-diff-name left-bottom right-bottom left-bottom))
         (right-bottom-buffer-name (msk-diff-name left-bottom right-bottom right-bottom))
         (pair-key-bottom (concat "has-shown" left-bottom right-bottom)))
    (delete-other-windows)

    (switch-to-buffer (msk-get left-top-buffer-name))
    (select-window (split-window-right))
    (switch-to-buffer (msk-get right-top-buffer-name))
    (select-window (split-root-window-below))
    (switch-to-buffer (msk-get left-bottom-buffer-name))
    (select-window (split-window-right))
    (switch-to-buffer (msk-get right-bottom-buffer-name))

    (unless (msk-get pair-key-top)
      (msk-put pair-key-top t)
      (unless msk-skip-vdiff-refresh
        (vdiff-refresh)))

    (unless (msk-get pair-key-bottom)
      (msk-put pair-key-bottom t)
      (unless msk-skip-vdiff-refresh
        (vdiff-refresh)))))

(defun msk-local-changes-compare ()
  (interactive)
  (msk-change-to-4-way-view "BASE" "LOCAL" "REMOTE" "MERGED"))

(defun msk-remote-changes-compare ()
  (interactive)
  (msk-change-to-4-way-view "BASE" "REMOTE" "LOCAL" "MERGED"))

;;;; ---------------------------------------------------------------------------
;;;; Saving the solved conflict
;;;; ---------------------------------------------------------------------------

(defun msk-save-solved-conflict ()
  (switch-to-buffer (msk-original-buffer))
  (goto-char msk-original-buffer-point)
  (cl-assert (msk-find-next-conflict))
  (let* ((old-string msk-merged-string)
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
  
