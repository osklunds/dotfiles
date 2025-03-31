;; -*- lexical-binding: nil -*-

(require 'ol-util)
(require 'ol-basic-user-interface)
(require 'ol-magit)
(require 'ol-evil)

(require 'vdiff)
(require 'vdiff-magit)

(ol-require-external "diff")

(setc vdiff-subtraction-fill-char ? )

(setc vdiff-diff-algorithm 'git-diff-patience)

(setc vdiff-fold-padding 10)
;; I have it here for easy on-demand customization
(setq vdiff--after-change-refresh-delay 1)

(defun ol-vdiff-fold-string (n-lines first-line-text width)
  (format "   %d lines\n" n-lines))

(setc vdiff-fold-string-function 'ol-vdiff-fold-string)

(setc vdiff-magit-stage-is-2way t)

;; -----------------------------------------------------------------------------
;; Colors
;; -----------------------------------------------------------------------------

(ol-copy-face-fg-bg 'vdiff-closed-fold-face 'magit-diff-hunk-heading-highlight)

(defun ol-vdiff-set-face (face-to-set face-val)
  (ol-set-face face-to-set
               :inherit nil
               :extend t
               :background face-val
               :foreground 'unspecified))

;; Add
(ol-vdiff-set-face 'vdiff-addition-face ol-diff-green)
(ol-vdiff-set-face 'vdiff-refine-added ol-diff-green)

;; Delete
(ol-vdiff-set-face 'vdiff-subtraction-face ol-diff-dark-red)

;; Change
(ol-vdiff-set-face 'vdiff-refine-changed ol-diff-dark-orange)
(ol-vdiff-set-face 'vdiff-change-face ol-diff-light-orange)

;; -----------------------------------------------------------------------------
;; Synced scroll
;; -----------------------------------------------------------------------------

(defun ol-vdiff-fix-scroll ()
  (interactive)
  (vdiff--scroll-function))

;; TODO: Calling fix scroll automatically as part of vdiff next hunk doesn't
;; work. Probably related to the fixes/bugs I had to do for msk.

(advice-add 'vdiff-next-hunk :after (lambda (&rest r) (ol-vdiff-fix-scroll)))
(advice-add 'vdiff-previous-hunk :after (lambda (&rest r) (ol-vdiff-fix-scroll)))

;; -----------------------------------------------------------------------------
;; Truncate lines
;; -----------------------------------------------------------------------------

(advice-add 'vdiff-buffers :after (lambda (&rest r)
                                    (ol-enable-truncate-lines)
                                    (other-window 1)
                                    (ol-enable-truncate-lines)))

(defun ol-enable-truncate-lines ()
  (unless truncate-lines
    (toggle-truncate-lines)))

;; -----------------------------------------------------------------------------
;; Cleaning up buffers
;; -----------------------------------------------------------------------------

(defvar ol-vdiff-kill-buffers nil)

(defun ol-vdiff-new-args (buffer-a
                          buffer-b
                          &optional
                          rotate
                          on-quit
                          restore-windows-on-quit
                          kill-buffers-on-quit)
  (let* ((new-on-quit (lambda (buf-a buf-b)
                        (when ol-vdiff-kill-buffers
                          (kill-buffer buf-a)
                          (kill-buffer buf-b))
                        (setq ol-vdiff-kill-buffers nil)
                        (vdiff-magit--kill-temp-buffers buf-a buf-b)
                        (visual-line-mode 1)))
         (new-restore-windows-on-quit t)
         (new-kill-buffers-on-quit nil))
    (list buffer-a
          buffer-b
          rotate
          new-on-quit
          new-restore-windows-on-quit
          new-kill-buffers-on-quit)))

(advice-add 'vdiff-buffers :filter-args (lambda (args) (apply 'ol-vdiff-new-args args)))

(defun ol-vdiff-magit-stage-cleanup (file)
  (let* ((trailing-buf (or (magit-get-revision-buffer "HEAD" file)
                           (magit-find-file-noselect "HEAD" file))))
    (kill-buffer trailing-buf)))

(advice-add 'vdiff-magit-stage :after 'ol-vdiff-magit-stage-cleanup)

;; -----------------------------------------------------------------------------
;; Refine
;; -----------------------------------------------------------------------------

(setc vdiff-auto-refine t)

(defun ol-vdiff-refine-all-hunks ()
  (interactive)
  (setc vdiff-auto-refine t)
  (vdiff-refresh))

(defun ol-vdiff-remove-all-refinements ()
  (interactive)
  (setc vdiff-auto-refine nil)
  (vdiff-refresh))

;; -----------------------------------------------------------------------------
;; Keybinds
;; -----------------------------------------------------------------------------

(ol-define-key vdiff-mode-map "C-c" vdiff-mode-prefix-map)

(ol-define-key vdiff-mode-map "M-n" 'vdiff-next-hunk)
(ol-define-key vdiff-mode-map "M-p" 'vdiff-previous-hunk)
(ol-define-key vdiff-mode-map "M-l" 'ol-vdiff-fix-scroll)

(ol-define-key ol-normal-leader-map "b d" 'vdiff-buffers)

;; Hunk refinement
(ol-define-key vdiff-mode-map "C-c f" 'ol-vdiff-refine-all-hunks)
(ol-define-key vdiff-mode-map "C-c F" 'vdiff-refine-this-hunk)
(ol-define-key vdiff-mode-map "C-c x" 'ol-vdiff-remove-all-refinements)
(ol-define-key vdiff-mode-map "C-c X" 'vdiff-remove-refinements-in-hunk)

;; Magit integration
(ol-define-key magit-mode-map "e" 'vdiff-magit-dwim)
(ol-define-key magit-mode-map "E" 'vdiff-magit)
(transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
(transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
(transient-suffix-put 'magit-dispatch "E" :description "vdiff")
(transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit)

;; -----------------------------------------------------------------------------
;; Diff of selection 
;; -----------------------------------------------------------------------------

(defvar ol-selection1 nil)
(defvar ol-selection2 nil)

(defun ol-vdiff-select1 ()
  (interactive)
  (setq ol-selection1 (buffer-substring-no-properties (region-beginning)
                                                      (region-end)))
  (message "Selection1 saved")
  (evil-normal-state))

(defun ol-vdiff-select2 ()
  (interactive)
  (unless ol-selection1
    (user-error "Selection1 not made yet"))
  (setq ol-selection2 (buffer-substring-no-properties (region-beginning)
                                                      (region-end)))
  (evil-normal-state)
  (setq ol-vdiff-kill-buffers t)
  (vdiff-temp-files)
  ;; Assumes only two buffers
  (other-window 1)
  (insert ol-selection1)
  (other-window 1)
  (insert ol-selection2)
  (setq ol-selection1 nil)
  (setq ol-selection2 nil))

(ol-define-key ol-visual-leader-map "b s 1" #'ol-vdiff-select1)
(ol-define-key ol-visual-leader-map "b s 2" #'ol-vdiff-select2)
  
        


(provide 'ol-vdiff)
