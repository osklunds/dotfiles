;; -*- lexical-binding: nil -*-

(require 'ol-util)
(require 'ol-evil)

(require 'merge-survival-knife)
(require 'merge-commit-analyzer)

;; To make sure smerge doesn't add refinements to conflicts
(setc diff-refine nil)

(defun ol-msk-original-buffer-fix-keybinds (&rest args)
  (evil-force-normal-state))

(advice-add 'msk-original-buffer :after 'ol-msk-original-buffer-fix-keybinds)

(defun ol-msk-mode-disable-dwim ()
  (interactive)
  (if msk-mode
      (msk-mode -1)
    (if vdiff-mode
        (vdiff-quit)
      (if mca-mode
          (mca-mode -1)
        (user-error "How did I get here?")))))

(ol-define-key ol-normal-leader-map "g m" 'msk-mode-dwim)

;; Don't use ol-evil-define-key, caused problems after changing to use 'clone
;; when make-indirect-buffer
(ol-define-key msk-mode-map "C-c q" 'ol-msk-mode-disable-dwim)
(ol-define-key mca-mode-map "C-c q" 'ol-msk-mode-disable-dwim)

(ol-define-key msk-mode-map "M-1" 'msk-base-local)
(ol-define-key msk-mode-map "M-2" 'msk-base-remote)
(ol-define-key msk-mode-map "M-3" 'msk-local-remote)
(ol-define-key msk-mode-map "M-4" 'msk-local-merged)
(ol-define-key msk-mode-map "M-5" 'msk-remote-merged)

(ol-define-key msk-mode-map "M-8" 'msk-local-changes-compare)
(ol-define-key msk-mode-map "M-9" 'msk-remote-changes-compare)

(ol-define-key msk-mode-map "M-m" 'msk-merged-buffer)
(ol-define-key msk-mode-map "M-o" 'msk-original-buffer)

(ol-define-key msk-mode-map "C-x C-s" 'msk-cant-save-reminder)

(defun ol-disable-patches-for-mca-mode (func &rest args)
  (let ((ol-magit-disable-patches t))
    (apply func args))
  )

(advice-add 'mca-start :around 'ol-disable-patches-for-mca-mode)

(provide 'ol-merge-survival-knife)
