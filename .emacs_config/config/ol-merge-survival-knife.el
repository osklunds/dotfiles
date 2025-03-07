
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

(ol-define-normal-leader-key "gm" 'msk-mode-dwim)
(ol-evil-define-key 'normal msk-mode-map "C-c q" 'ol-msk-mode-disable-dwim)
(ol-evil-define-key 'normal mca-mode-map "C-c q" 'ol-msk-mode-disable-dwim)

(ol-evil-define-key 'normal msk-mode-map "M-1" 'msk-base-local)
(ol-evil-define-key 'normal msk-mode-map "M-2" 'msk-base-remote)
(ol-evil-define-key 'normal msk-mode-map "M-3" 'msk-local-remote)
(ol-evil-define-key 'normal msk-mode-map "M-4" 'msk-local-merged)
(ol-evil-define-key 'normal msk-mode-map "M-5" 'msk-remote-merged)

(ol-evil-define-key 'normal msk-mode-map "M-8" 'msk-local-changes-compare)
(ol-evil-define-key 'normal msk-mode-map "M-9" 'msk-remote-changes-compare)

(ol-evil-define-key 'normal msk-mode-map "M-m" 'msk-merged-buffer)
(ol-evil-define-key 'normal msk-mode-map "M-o" 'msk-original-buffer)

(ol-evil-define-key 'motion msk-mode-map "C-x C-s" 'msk-cant-save-reminder)

(provide 'ol-merge-survival-knife)
