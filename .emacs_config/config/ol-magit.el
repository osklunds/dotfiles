
(require 'ol-util)
(require 'ol-evil)
(require 'ol-colors)

(require 'magit)
(require 'vdiff)

(ol-require-external "git")

;; To make sure leader works in magit buffers
(ol-define-key magit-mode-map "SPC" nil)
(ol-define-key magit-diff-mode-map "SPC" nil)

(setc magit-display-buffer-function 'ol-magit-display-buffer-traditional)

(defun ol-magit-display-buffer-traditional (buffer)
  "Display BUFFER the way this has traditionally been done."
  (display-buffer
   buffer (if (and (derived-mode-p 'magit-mode)
                   (not (memq (with-current-buffer buffer major-mode)
                              '(magit-process-mode
                                magit-revision-mode
                                magit-diff-mode
                                magit-stash-mode
                                magit-status-mode))))
              '(display-buffer-same-window)
            '(nil (inhibit-same-window . t))))) ; This is the line I changed

;;;; ---------------------------------------------------------------------------
;;;; Blame
;;;; ---------------------------------------------------------------------------

;; TODO it only works to cycle once, and even that cycling seems broken.
;; Maybe add more styles, for example the same but longer width.
;; TODO use same data format string as log margin, and same date color?
(setc magit-blame-styles
      '(
        (margin
         (margin-format . ("%C %s%f"))
         (margin-width  . 60)
         )
        )
      )

(ol-set-face 'magit-blame-margin :background "#e4e4e4")

(defun ol-magit-blame-run-process-args-advice (revision file args &optional lines)
  ;; To handle symlinks
  `(,revision ,(file-truename file) ,args ,lines))

(advice-add 'magit-blame-run-process
            :filter-args
            (lambda (args) (apply 'ol-magit-blame-run-process-args-advice args)))

(ol-define-normal-leader-key "gb" 'magit-blame-addition)

;;;; ---------------------------------------------------------------------------
;;;; Commit
;;;; ---------------------------------------------------------------------------

;; Start in insert state when doing commits in magit
(add-hook 'with-editor-mode-hook 'evil-insert-state)

;; To make sure the commit message is always uncluttered
(defun ol-git-commit-setup ()
  (let ((prefix "\n\n"))
    (unless (looking-at-p prefix)
      (insert "\n\n")
      (goto-char (point-min)))))

(add-hook 'git-commit-setup-hook 'ol-git-commit-setup)

;;;; ---------------------------------------------------------------------------
;;;; Status
;;;; ---------------------------------------------------------------------------

(setc magit-save-repository-buffers 'dontask)
(setc magit-status-initial-section nil)
(setc vdiff-magit-dwim-show-on-hunks t)

(defun ol-magit-set-simple-status-header ()
  (magit-set-header-line-format "Magit Status"))

(defun ol-magit-set-full-status-header ()
  (magit-set-header-line-format "Magit Full Status"))

(defun ol-magit-status-sections (full is-merging)
  (append
   '(ol-magit-set-simple-status-header
     magit-insert-untracked-files
     magit-insert-unstaged-changes)
   (when (or full (not is-merging))
     '(magit-insert-staged-changes))
   '(magit-insert-stashes)
   (when full
     (list 'ol-magit-set-full-status-header
           'magit-insert-unpushed-to-pushremote
           'magit-insert-unpushed-to-upstream
           'magit-insert-unpulled-from-pushremote
           'magit-insert-unpulled-from-upstream))))

(defun ol-magit-status (&optional full)
  (interactive "P")
  (setc magit-status-sections-hook
        (ol-magit-status-sections full (magit-merge-in-progress-p)))
  (magit-status-setup-buffer))

(ol-define-normal-leader-key "gs" 'ol-magit-status)

;;;; ---------------------------------------------------------------------------
;;;; Mode toggling
;;;; ---------------------------------------------------------------------------

(defvar ol-original-mode nil)
(make-local-variable 'ol-original-mode)

(defun ol-toggle-fundamental-mode ()
  (interactive)
  (let* ((original-mode ol-original-mode)
         (original-mode (if original-mode
                            original-mode
                          major-mode)))
    (if (equal major-mode 'fundamental-mode)
        (funcall original-mode)
      (fundamental-mode))
    (setq-local ol-original-mode original-mode)))

(ol-define-normal-leader-key "gt" 'ol-toggle-fundamental-mode)

;;;; ---------------------------------------------------------------------------
;;;; Diff
;;;; ---------------------------------------------------------------------------

(setc magit-diff-paint-whitespace nil)

;; TODO: set using transient instead
(defun ol-include-stat (&rest r)
  (add-to-list 'magit-buffer-diff-args "--stat"))

(advice-add 'magit-insert-revision-diff :before 'ol-include-stat)
(advice-add 'magit-insert-diff :before 'ol-include-stat)

(defconst ol-diff-green "#9fec9d")
(defconst ol-diff-dark-red "#e45649")
(defconst ol-diff-light-red "#f5d9d6")
(defconst ol-diff-dark-orange "#ffd787")
(defconst ol-diff-light-orange "#f6eee8")

(defun ol-magit-diff-set-face (face-to-set face-val)
  (ol-set-face face-to-set
               :background face-val
               :foreground 'unspecified))

(unless (display-graphic-p)
  (ol-magit-diff-set-face 'magit-diff-added-highlight   ol-diff-green)
  (ol-magit-diff-set-face 'magit-diff-added             ol-diff-green)
  (ol-magit-diff-set-face 'magit-diff-base-highlight    ol-diff-dark-orange)
  (ol-magit-diff-set-face 'magit-diff-base              ol-diff-dark-orange)
  (ol-magit-diff-set-face 'magit-diff-removed-highlight ol-diff-light-red)
  (ol-magit-diff-set-face 'magit-diff-removed           ol-diff-light-red))

;;;;;; -------------------------------------------------------------------------
;;;;;; In magit diff, vidff the highlighted file directly
;;;;;;--------------------------------------------------------------------------

;; todo: handle when nil file, i.e. in diff when file as added
(defun ol-magit-ediff-read-files (revA revB fileB)
  (let* ((fileA (magit--rev-file-name fileB revA revB))
         (fileA (or fileA fileB))) ;; if fileB didn't exist in revA, i.e was added since revA
    (list fileA fileB)))

(defun ol-vdiff-magit-dwim-advice (func &rest args)
  (cl-letf (((symbol-function 'magit-ediff-read-files)
             (lambda (&rest magit-ediff-read-files-args)
               (apply 'ol-magit-ediff-read-files magit-ediff-read-files-args))))
    (apply func args)))

(advice-add 'vdiff-magit-dwim :around 'ol-vdiff-magit-dwim-advice)

;; todo: do the above also for index and HEAD. If a file is renamed, diff
;; doesn't work. Same for ediff

;;;;;; -------------------------------------------------------------------------
;;;;;; Diffing all files
;;;;;; -------------------------------------------------------------------------

(defun ol-diff-all-files-main ()
  (interactive)
  (magit-diff-range (ol-merge-base-with-main)))

(ol-define-normal-leader-key "gdM" 'ol-diff-all-files-main)

(defun ol-diff-all-files-head ()
  (interactive)
  (magit-diff-range "HEAD"))

(ol-define-normal-leader-key "gdH" 'ol-diff-all-files-head)

;;;;;; -------------------------------------------------------------------------
;;;;;; Diffing the current file
;;;;;; -------------------------------------------------------------------------

(defun ol-diff-current-file-main ()
  (interactive)
  (ol-diff-current-file (ol-merge-base-with-main)))

(ol-define-normal-leader-key "gdm" 'ol-diff-current-file-main)

(defun ol-diff-current-file-head ()
  (interactive)
  (ol-diff-current-file "HEAD"))

(ol-define-normal-leader-key "gdh" 'ol-diff-current-file-head)

(defun ol-diff-current-file (rev-left &optional rev-right)
  (let* ((buffer-left (ol-get-revision-buffer-current-file rev-left))
         (buffer-right (if rev-right
                           (ol-get-revision-buffer-current-file rev-right)
                         (current-buffer))))
    (vdiff-buffers buffer-left buffer-right)))

(defun ol-get-revision-buffer-current-file (rev)
  (let* ((file (magit-current-file))
         ;; Assuming the file name didn't change between HEAD and worktree
         (file-in-rev (magit--rev-file-name file "HEAD" rev)))
    (ol-get-revision-buffer rev file-in-rev)))

(defun ol-get-revision-buffer (rev file)
  (magit-get-revision-buffer rev file (magit-find-file-noselect rev file)))

;;;; ---------------------------------------------------------------------------
;;;; Log
;;;; ---------------------------------------------------------------------------

;; trick: magit-diff-toggle-file-filter

(ol-set-face 'magit-log-date :foreground "#da8548")

;; TODO: Maybe these can be saved better with transient?
(defconst ol-magit-log-default-arguments '("-n256"))

;; TOOD: Set these using dwim command
(put 'magit-log-mode 'magit-log-default-arguments ol-magit-log-default-arguments)
(put 'magit-log-select-mode 'magit-log-default-arguments ol-magit-log-default-arguments)

(setc magit-log-margin '(t "%Y-%m-%d  %H:%M  " magit-log-margin-width nil 0))

(ol-define-normal-leader-key "gl" 'ol-git-log-dwim)

(defun ol-git-log-dwim (&optional arg)
  (interactive "P")
  (let* ((input (if arg
                    (read-from-minibuffer
                     "git log args (i)nclude commits in main, (t)his file only: ")
                  ""))
         (branch (magit-get-current-branch))
         (main (ol-main-branch))
         (ignore-rev (when (and (not (string-match-p "i" input))
                                (not (equal branch main)))
                       main))
         (file (when (string-match-p "t" input)
                 (buffer-file-name))))
    (ol-git-log branch :ignore-rev ignore-rev :file file)))

(defun ol-git-log (rev &rest args)
  (let* ((ignore-rev (plist-get args :ignore-rev))
         (file-arg (plist-get args :file))
         (file (cond ((null file-arg) nil)
                     (t (list file-arg))))
         (magit-log-args (append ol-magit-log-default-arguments
                                 (ol-make-ignore-rev-args ignore-rev))))
    (magit-log-setup-buffer (list rev) magit-log-args file)))

(defun ol-make-ignore-rev-args (ignore-rev)
  (when ignore-rev
    (list "--first-parent" "--not" ignore-rev "--not")))

;; Inspired by magit-log-header-line-sentence
;; TODO: simplify/change
(defun ol-magit-log-header-line (revs args files)
  (concat "Commits in "
          (mapconcat #'identity revs " ")
          (when-let ((index (cl-position "--not" args :test 'equal)))
            (concat ", but not in " (nth (+ index 1) args) (when files ",")))
          (and files (concat " touching "
                             (mapconcat #'identity files " ")))))

;; TODO: Maybe generalize this type of testing and use in more places?
(cl-assert (let ((expected "Commits in test, but not in main, touching colors.el")
                 (actual (ol-magit-log-header-line
                          (list "test")
                          (list "--first-parent" "--not" "main" "--not")
                          (list "colors.el"))))
             (equal expected actual)))

(setc magit-log-header-line-function 'ol-magit-log-header-line)

(defconst ol-not-in-main-branch-arg "--not-in-main-branch")

;; TODO: Replace existing instead
(transient-replace-suffix 'magit-log "=p" `(4 "=p" "First parent" "--first-parent"))
(transient-replace-suffix 'magit-log "=m" `(4 "=m" "Omit merges" "--no-merges"))

(transient-append-suffix 'magit-log "=p"
  `(4 "-m" "Hide commits in main/master" ,ol-not-in-main-branch-arg))

(defun ol-magit-process-git-arguments (args)
  (if (cl-find ol-not-in-main-branch-arg args :test 'string-equal)
      (flatten-tree (cl-substitute
                     `("--not" ,(ol-main-branch) "--not")
                     ol-not-in-main-branch-arg
                     args
                     :test 'string-equal))
    args))

(advice-add 'magit-process-git-arguments :filter-return 'ol-magit-process-git-arguments)

;;;; ---------------------------------------------------------------------------
;;;; Revision
;;;; ---------------------------------------------------------------------------

(setc magit-revision-insert-related-refs-display-alist
      '((follows . nil)
        (precedes . nil)
        (merged . nil)
        (contained . nil)))

(defun ol-magit-set-revision-header ()
  (magit-set-header-line-format (magit-rev-format "%B" magit-buffer-revision-hash)))

(add-hook 'magit-revision-sections-hook 'ol-magit-set-revision-header)

;; magit wash of the patches takes times, and if not relevant, disable it
(defvar ol-magit-disable-patches nil)

(defun ol-magit-disable-patches-advice (args)
  (if ol-magit-disable-patches
      (cl-remove "-p" args :test 'string-equal)
    args))

(advice-add 'magit-process-git-arguments :filter-return 'ol-magit-disable-patches-advice)

;; -----------------------------------------------------------------------------
;; Transient
;;------------------------------------------------------------------------------

(keymap-set transient-base-map "C-n" 'transient-quit-one)
(keymap-set transient-sticky-map "C-n" 'transient-quit-seq)

;; -----------------------------------------------------------------------------
;; Misc
;; -----------------------------------------------------------------------------

(defun ol-main-branch ()
  (let ((main-branch "main"))
    (if (ol-does-branch-exist main-branch)
        main-branch
      "master")))

(defun ol-does-branch-exist (branch)
  (let ((all-branches (shell-command-to-string "git branch --list"))
        (regex (concat "[ \\n]" branch "$")))
    (string-match-p regex all-branches)))

;; Valid assumption in this repo
(let ((default-directory (file-name-directory load-file-name)))
  (cl-assert (ol-does-branch-exist "main"))
  (cl-assert (not (ol-does-branch-exist "mai")))
  (cl-assert (not (ol-does-branch-exist "ain")))
  (cl-assert (not (ol-does-branch-exist "random"))))

(defun ol-merge-base-with-main ()
  (ol-merge-base (ol-main-branch) "HEAD"))

(defun ol-merge-base (c1 c2)
  (magit-commit-p (magit-git-string "merge-base" c1 c2)))

(provide 'ol-magit)
