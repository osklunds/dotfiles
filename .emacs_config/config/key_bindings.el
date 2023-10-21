
;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; Wrappers
;;;; ---------------------------------------------------------------------------

(defun ol-define-key (map key fun)
  (define-key map (kbd key) fun))

(defun ol-global-define-key (key fun)
  (global-set-key (kbd key) fun))

;;;; ---------------------------------------------------------------------------
;;;; Leader
;;;; ---------------------------------------------------------------------------

(defvar ol-normal-leader-map (make-sparse-keymap))
(defvar ol-visual-leader-map (make-sparse-keymap))

(ol-define-key evil-normal-state-map "SPC" ol-normal-leader-map)
(ol-define-key evil-normal-state-map "C-SPC" ol-normal-leader-map)
(ol-define-key evil-visual-state-map "SPC" ol-visual-leader-map)
(evil-define-key 'insert term-raw-map (kbd "C-SPC") ol-normal-leader-map)

(defun ol-define-normal-leader-key (key fun)
  (ol-define-key ol-normal-leader-map key fun))

(defun ol-define-visual-leader-key (key fun)
  (ol-define-key ol-visual-leader-map key fun))

;;;; ---------------------------------------------------------------------------
;;;; Override
;;;; ---------------------------------------------------------------------------

;; Section copied from: https://emacs.stackexchange.com/a/358

(defvar ol-override-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode ol-override-mode
  "Minor mode for overriding keys"
  :init-value t
  :lighter " ol-override-mode"
  :keymap ol-override-mode-map)

;;;###autoload
(define-globalized-minor-mode global-ol-override-mode ol-override-mode ol-override-mode)

(add-to-list 'emulation-mode-map-alists `((ol-override-mode . ,ol-override-mode-map)))

;; Turn off the minor mode in the minibuffer
(defun turn-off-ol-override-mode ()
  (ol-override-mode -1))

(add-hook 'minibuffer-setup-hook #'turn-off-ol-override-mode)

(provide 'ol-override-mode)

(ol-override-mode t)

(defun ol-override-key (key fun)
  (ol-define-key ol-override-mode-map key fun))

;; ---------------------------------------------------------------------------
;; Evil
;; ---------------------------------------------------------------------------

(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(ol-define-key evil-emacs-state-map "<escape>" 'evil-normal-state)

;; Window movement
(define-key evil-motion-state-map (kbd "C-h") #'evil-window-left)
(define-key evil-motion-state-map (kbd "C-l") #'evil-window-right)

(define-key evil-normal-state-map (kbd "?") 'evil-ex-nohighlight)

;; ---------------------------------------------------------------------------
;; Find and replace
;; ---------------------------------------------------------------------------

(ol-define-visual-leader-key "R" 'ol-full-replace-visual-selection)
(ol-define-visual-leader-key "r" 'ol-from-here-replace-visual-selection)

(ol-define-normal-leader-key "R" 'ol-full-replace-symbol)
(ol-define-normal-leader-key "r" 'ol-from-here-replace-symbol)

;; ---------------------------------------------------------------------------
;; Windows and buffers
;; ---------------------------------------------------------------------------

(defun ol-split-window ()
  (interactive)
  (switch-to-buffer-other-window (current-buffer)))

(defun ol-force-split-window ()
  (interactive)
  (split-window-right)
  (evil-window-right 1))

(ol-override-key "M-d" 'ol-split-window)
(ol-override-key "M-e" 'ol-force-split-window)

(ol-define-key evil-normal-state-map "q" 'quit-window)

;; -----------------------------------------------------------------------------
;; Languages
;; -----------------------------------------------------------------------------

;;;;----------------------------------------------------------------------------
;;;; All languages
;;;; ---------------------------------------------------------------------------

;;;;;; -------------------------------------------------------------------------
;;;;;; Misc
;;;;;; -------------------------------------------------------------------------

(ol-define-normal-leader-key "fs" 'counsel-imenu)

;;;;;; -------------------------------------------------------------------------
;;;;;; Completion
;;;;;; -------------------------------------------------------------------------

(ol-define-key company-active-map "<return>" 'company-abort)
(ol-define-key company-active-map "<tab>" 'company-complete-selection)
(ol-define-key company-active-map "C-j" 'company-select-next)
(ol-define-key company-active-map "C-k" 'company-select-previous)

(ol-define-key prog-mode-map "<tab>" 'company-indent-or-complete-common)

;; ---------------------------------------------------------------------------
;; Projectile
;; ---------------------------------------------------------------------------

(ol-define-normal-leader-key "pp" 'projectile-switch-project)
(ol-define-normal-leader-key "ps" 'projectile-discover-projects-in-search-path)
(ol-define-normal-leader-key "pr" 'projectile-invalidate-cache)
(ol-define-normal-leader-key "pf" 'counsel-projectile-rg)

(ol-define-normal-leader-key "pd" 'ol-switch-to-dotfiles)

(defun ol-switch-to-dotfiles ()
  (interactive)
  (projectile-switch-project-by-name "~/dotfiles"))

(ol-override-key "M-q" 'projectile-find-file)
(ol-define-key projectile-mode-map "C-c p" 'projectile-command-map)

;; ---------------------------------------------------------------------------
;; Git
;; ---------------------------------------------------------------------------

(ol-define-normal-leader-key "gs" 'magit-status)
(ol-define-normal-leader-key "gb" 'magit-blame-addition)
(ol-define-normal-leader-key "gdM" 'ol-diff-all-files-main)
(ol-define-normal-leader-key "gdH" 'ol-diff-all-files-head)
(ol-define-normal-leader-key "gdm" 'ol-diff-current-file-main)
(ol-define-normal-leader-key "gdh" 'ol-diff-current-file-head)

;; To make sure leader works in magit buffers
(ol-define-key magit-mode-map "SPC" nil)
(ol-define-key magit-diff-mode-map "SPC" nil)

;;;; ---------------------------------------------------------------------------
;;;; Merge Survival Knife
;;;; ---------------------------------------------------------------------------

(ol-global-define-key "C-c 6" 'msk-merge-survival-knife-start)
(ol-global-define-key "C-c 7" 'msk-merge-survival-knife-stop)

;; TODO Only bind if merging
(ol-global-define-key "C-c 1" 'msk-base-local)
(ol-global-define-key "C-c 2" 'msk-base-remote)
(ol-global-define-key "C-c 3" 'msk-local-remote)
(ol-global-define-key "C-c 4" 'msk-local-merged)
(ol-global-define-key "C-c 5" 'msk-remote-merged)

;; ---------------------------------------------------------------------------
;; Org mode
;; ---------------------------------------------------------------------------

(ol-define-normal-leader-key "os" 'org-babel-demarcate-block)

(evil-define-key 'visual org-mode-map (kbd "g q") 'org-fill-paragraph)
(evil-define-key 'normal org-mode-map (kbd "g q q") 'org-fill-paragraph)

;; ---------------------------------------------------------------------------
;; Mac
;; ---------------------------------------------------------------------------

(when (ol-is-mac)
  (setq mac-option-key-is-
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'n))

;; ---------------------------------------------------------------------------
;; Ivy and Counsel
;; ---------------------------------------------------------------------------

(ol-override-key "C-j" 'ivy-switch-buffer)
(ol-global-define-key "C-x C-b" 'ivy-switch-buffer)

(ol-define-key ivy-minibuffer-map "TAB" 'ivy-alt-done)
(ol-define-key ivy-minibuffer-map "C-j" 'ivy-next-line)
(ol-define-key ivy-minibuffer-map "C-k" 'ivy-previous-line)

(ol-define-key ivy-switch-buffer-map "C-k" 'ivy-previous-line)
(ol-define-key ivy-switch-buffer-map "C-d" 'ivy-switch-buffer-kill)

(ol-global-define-key "M-x" 'counsel-M-x)
(ol-global-define-key "C-x C-f" 'counsel-find-file)

;; -----------------------------------------------------------------------------
;; Terminal
;; -----------------------------------------------------------------------------

(ol-define-normal-leader-key "tt" 'ol-term-named)

(evil-define-key 'insert term-raw-map (kbd "C-h") #'evil-window-left)
(evil-define-key 'insert term-raw-map (kbd "C-l") #'evil-window-right)
(evil-define-key 'insert term-raw-map (kbd "C-j") 'ivy-switch-buffer)
(evil-define-key 'insert term-raw-map (kbd "C-y") 'term-paste)
(evil-define-key 'insert term-raw-map (kbd "C-d") 'term-send-raw)
(evil-define-key 'insert term-raw-map (kbd "C-6") 'evil-switch-to-windows-last-buffer)

;; Hack to do it like this. If done directly, error about prefix key.
(defun ol-map-ctrl-c ()
  (evil-define-key 'insert term-raw-map (kbd "C-c") 'term-send-raw))
  
(add-hook 'term-mode-hook 'ol-map-ctrl-c)

;; Tip: Map help-command to C-m to be able to run it in insert mode. But If C-m,
;; RET seeems to become broken.
;; (evil-define-key 'insert term-raw-map (kbd "C-m") 'help-command)

;; -----------------------------------------------------------------------------
;; Vdiff
;; -----------------------------------------------------------------------------

(ol-define-key vdiff-mode-map "C-c" vdiff-mode-prefix-map)

(defun ol-vdiff-fix-scroll ()
  (interactive)
  (vdiff--scroll-function))

(ol-define-key vdiff-mode-map "M-n" 'vdiff-next-hunk)
(ol-define-key vdiff-mode-map "M-p" 'vdiff-previous-hunk)
(ol-define-key vdiff-mode-map "M-l" 'ol-vdiff-fix-scroll)
(ol-define-key vdiff-mode-map "C-c X" 'vdiff-refresh)

(define-key magit-mode-map "e" 'vdiff-magit-dwim)
(define-key magit-mode-map "E" 'vdiff-magit)
(transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
(transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
(transient-suffix-put 'magit-dispatch "E" :description "vdiff")
(transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit)

;; -----------------------------------------------------------------------------
;; Dired
;; -----------------------------------------------------------------------------

(ol-global-define-key "C-x d" 'ol-dired)

(evil-define-key 'normal dired-mode-map (kbd "o") 'dired-find-file)
(evil-define-key 'normal dired-mode-map (kbd "i") 'dired-up-directory)

;; Seems to be the only way override space
(evil-collection-define-key 'normal 'dired-mode-map " " nil)

;; -----------------------------------------------------------------------------
;; tar-mode
;; -----------------------------------------------------------------------------

(evil-define-key 'normal tar-mode-map (kbd "o") 'tar-extract)
(evil-define-key 'normal tar-mode-map (kbd "i") 'ol-tar-up-directory)

;; ---------------------------------------------------------------------------
;; Misc
;; ---------------------------------------------------------------------------

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(ol-override-key "M-h" 'help-command)

(defun ol-eval-region ()
  (interactive)
  (call-interactively 'eval-region)
  (message "eval-region"))

(defun ol-eval-buffer ()
  (interactive)
  (call-interactively 'eval-buffer)
  (message "eval-buffer"))

(ol-define-visual-leader-key "er" 'ol-eval-region)
(ol-define-normal-leader-key "eb" 'ol-eval-buffer)

(global-set-key (kbd "M-/") 'evilnc-comment-or-uncomment-lines)

(ol-define-normal-leader-key "gt" 'ol-toggle-fundamental-mode)

(ol-global-define-key "C-x C-s" 'ol-save-buffer)
