;; -*- lexical-binding: t -*-

(require 'ol-util)
(require 'ol-basic-user-interface)
(require 'ol-evil)

(require 'vdiff)
(require 'vdiff-magit)

(ol-require-external "diff")

(setc vdiff-subtraction-fill-char ? )

(setc vdiff-diff-algorithm 'git-diff-patience)

(setc vdiff-fold-padding 10)
;; I have it here for easy on-demand customization
(setq vdiff--after-change-refresh-delay 1)

(defun ol-vdiff-fold-string (n-lines _first-line-text _width)
  (format "   %d lines\n" n-lines))

(setc vdiff-fold-string-function 'ol-vdiff-fold-string)

;; -----------------------------------------------------------------------------
;; Synced scroll
;; -----------------------------------------------------------------------------

(defun ol-vdiff-fix-scroll ()
  (interactive)
  (vdiff--scroll-function))

;; TODO: Calling fix scroll automatically as part of vdiff next hunk doesn't
;; work. Probably related to the fixes/bugs I had to do for msk.

(advice-add 'vdiff-next-hunk :after (lambda (&rest _) (ol-vdiff-fix-scroll)))
(advice-add 'vdiff-previous-hunk :after (lambda (&rest _) (ol-vdiff-fix-scroll)))

;; -----------------------------------------------------------------------------
;; Truncate lines
;; -----------------------------------------------------------------------------

(advice-add 'vdiff-buffers :after (lambda (&rest _)
                                    (save-window-excursion
                                      (ol-enable-truncate-lines)
                                      (other-window 1)
                                      (ol-enable-truncate-lines))))

(defun ol-enable-truncate-lines ()
  (unless truncate-lines
    (toggle-truncate-lines)))

;; -----------------------------------------------------------------------------
;; Cleaning up buffers
;; -----------------------------------------------------------------------------

(defvar ol-vdiff-buffers-before nil)

(defun ol-vdiff-save-buffers-before ()
  (when ol-vdiff-buffers-before
    (error "Buffers already saved"))
  (setq ol-vdiff-buffers-before (buffer-list)))

(defun ol-vdiff-on-quit (buffers-before buf-a buf-b)
  (dolist (buf (list buf-a buf-b))
    (unless (cl-member buf buffers-before)
      (kill-buffer buf))))

(defun ol-vdiff-buffers (buf-a buf-b)
  (unless ol-vdiff-buffers-before
    (error "Buffers not saved before"))
  (let* ((buffers-before ol-vdiff-buffers-before)
         (on-quit (apply-partially #'ol-vdiff-on-quit buffers-before)))
    (setq ol-vdiff-buffers-before nil)
    (vdiff-buffers buf-a buf-b nil on-quit t)
    ;; So that the buffer on the right is selected
    (other-window 1)))

(defun ol-vdiff-buffers-interactive ()
  (interactive)
  (ol-vdiff-save-buffers-before)
  (let* ((buf-a (get-buffer (read-buffer "Buffer 1: " (current-buffer))))
         (buf-b (get-buffer (read-buffer "Buffer 2: " (current-buffer)))))
    (ol-vdiff-buffers buf-a buf-b)))

;; -----------------------------------------------------------------------------
;; Cleaning up process buffers
;; -----------------------------------------------------------------------------

;; inside vdiff-quit, vdiff-session-process-buffer returns buffer names, not
;; buffers. But buffer-live-p only supports buffers
;; This it to prevent *vdiff* -123 buffers
(defun ol-vdiff-quit-bug-fix-advice (vdiff-quit-func)
  (let* ((old-buffer-live-p (symbol-function #'buffer-live-p)))
    (cl-letf (((symbol-function #'buffer-live-p)
               (lambda (buf)
                 (funcall old-buffer-live-p (get-buffer buf)))))
      (funcall vdiff-quit-func))))

(advice-add 'vdiff-quit :around #'ol-vdiff-quit-bug-fix-advice)

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
;; todo: move to where used

(ol-define-key vdiff-mode-map "C-c" vdiff-mode-prefix-map)

(ol-define-key vdiff-mode-map "M-n" 'vdiff-next-hunk)
(ol-define-key vdiff-mode-map "M-p" 'vdiff-previous-hunk)
(ol-define-key vdiff-mode-map "M-l" 'ol-vdiff-fix-scroll)

(ol-define-key ol-normal-leader-map "b d" #'ol-vdiff-buffers-interactive)

;; Hunk refinement
(ol-define-key vdiff-mode-map "C-c f" 'ol-vdiff-refine-all-hunks)
(ol-define-key vdiff-mode-map "C-c F" 'vdiff-refine-this-hunk)
(ol-define-key vdiff-mode-map "C-c x" 'ol-vdiff-remove-all-refinements)
(ol-define-key vdiff-mode-map "C-c X" 'vdiff-remove-refinements-in-hunk)

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

  (ol-vdiff-save-buffers-before)

  (let* ((buf-sel1 (generate-new-buffer "selection1"))
         (buf-sel2 (generate-new-buffer "selection2")))
    (with-current-buffer buf-sel1
      (insert ol-selection1))
    (with-current-buffer buf-sel2
      (insert ol-selection2))
    (setq ol-selection1 nil)
    (setq ol-selection2 nil)
    (ol-vdiff-buffers buf-sel1 buf-sel2)))

(ol-define-key ol-visual-leader-map "d s 1" #'ol-vdiff-select1)
(ol-define-key ol-visual-leader-map "d s 2" #'ol-vdiff-select2)

;; -----------------------------------------------------------------------------
;; Prevent updates
;; -----------------------------------------------------------------------------

(defun ol-vdiff-prevent-updates ()
  (interactive)
  (setq vdiff--inhibit-diff-update (not vdiff--inhibit-diff-update))
  (when (called-interactively-p nil)
    (message "Prevent updates. Now: %s" vdiff--inhibit-diff-update)))

(ol-define-key vdiff-mode-map "C-c U" #'ol-vdiff-prevent-updates)

(add-hook 'vdiff--cleanup-hook (lambda () (setq vdiff--inhibit-diff-update nil)))

(provide 'ol-vdiff)
