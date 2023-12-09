
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

(defmacro ol-evil-define-key (state map key fun)
  `(evil-define-key ',state ,map (kbd ,key) ,fun))

;;;; ---------------------------------------------------------------------------
;;;; Leader
;;;; ---------------------------------------------------------------------------

(defvar ol-normal-leader-map (make-sparse-keymap))
(defvar ol-visual-leader-map (make-sparse-keymap))

(ol-define-key evil-motion-state-map "SPC" ol-normal-leader-map)
(ol-define-key evil-motion-state-map "C-SPC" ol-normal-leader-map)
(ol-define-key evil-visual-state-map "SPC" ol-visual-leader-map)
(ol-evil-define-key insert term-raw-map "C-SPC" ol-normal-leader-map)

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

(define-key evil-motion-state-map (kbd "M-j") 'evil-scroll-line-down)
(define-key evil-motion-state-map (kbd "M-k") 'evil-scroll-line-up)

;; To work around a bug that prevents yank/delete full lines when doing
;; e.g. d2j. Affects also when a logical line fits the window width.
(ol-define-key evil-operator-state-map "j" 'evil-next-line)
(ol-define-key evil-operator-state-map "k" 'evil-previous-line)

;; I think this mapping makes more sense, to align down/j/{ and up/k/}
(ol-define-key evil-motion-state-map "}" 'evil-backward-paragraph)
(ol-define-key evil-motion-state-map "{" 'evil-forward-paragraph)

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

(ol-override-key "M-d" 'ol-split-window)
(ol-override-key "M-r" 'ol-force-split-window)

(ol-define-key evil-normal-state-map "q" 'quit-window)

;; -----------------------------------------------------------------------------
;; Languages
;; -----------------------------------------------------------------------------

;;;;----------------------------------------------------------------------------
;;;; All languages
;;;; ---------------------------------------------------------------------------

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
(ol-define-normal-leader-key "pf" 'ol-project-rg)

(ol-define-normal-leader-key "pd" 'ol-switch-to-dotfiles)

(defun ol-switch-to-dotfiles ()
  (interactive)
  (projectile-switch-project-by-name "~/dotfiles"))

(ol-override-key "M-q" 'ol-dwim-find-file-name)
(ol-override-key "M-e" 'ol-dwim-find-file-content)

;; ---------------------------------------------------------------------------
;; Git
;; ---------------------------------------------------------------------------

(ol-define-normal-leader-key "gs" 'ol-magit-status)
(ol-define-normal-leader-key "gS" 'ol-magit-full-status)
(ol-define-normal-leader-key "gb" 'magit-blame-addition)
(ol-define-normal-leader-key "gl" 'ol-git-log-current)
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

(ol-evil-define-key visual org-mode-map "g q" 'org-fill-paragraph)
(ol-evil-define-key normal org-mode-map "g q q" 'org-fill-paragraph)

(ol-evil-define-key insert org-mode-map "<tab>" 'org-metaright)
(ol-evil-define-key insert org-mode-map "<backtab>" 'org-metaleft)

(ol-evil-define-key normal org-mode-map "<tab>" 'org-cycle)

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

(ol-override-key "M-x" 'counsel-M-x)
(ol-global-define-key "C-x C-f" 'counsel-find-file)

;; -----------------------------------------------------------------------------
;; Terminal
;; -----------------------------------------------------------------------------

(ol-define-normal-leader-key "tt" 'ol-term-named)

(ol-evil-define-key insert term-raw-map "C-h" #'evil-window-left)
(ol-evil-define-key insert term-raw-map "C-l" #'evil-window-right)
(ol-evil-define-key insert term-raw-map "C-j" 'ivy-switch-buffer)
(ol-evil-define-key insert term-raw-map "C-y" 'term-paste)
(ol-evil-define-key insert term-raw-map "C-d" 'term-send-raw)
(ol-evil-define-key insert term-raw-map "C-6" 'evil-switch-to-windows-last-buffer)

;; Hack to do it like this. If done directly, error about prefix key.
(defun ol-map-ctrl-c ()
  (ol-evil-define-key insert term-raw-map "C-c" 'term-send-raw))
  
(add-hook 'term-mode-hook 'ol-map-ctrl-c)

(ol-evil-define-key
  'insert term-raw-map
  "<up>"
  (lambda () (interactive) (term-send-raw-string (kbd "C-p"))))

(ol-evil-define-key
  'insert term-raw-map
  "<down>"
  (lambda () (interactive) (term-send-raw-string (kbd "C-n"))))

(ol-evil-define-key
  'insert term-raw-map
  "<left>"
  (lambda () (interactive) (term-send-raw-string (kbd "C-b"))))

(ol-evil-define-key
  'insert term-raw-map
  "<right>"
  (lambda () (interactive) (term-send-raw-string (kbd "C-f"))))
  

;; -----------------------------------------------------------------------------
;; Vdiff
;; -----------------------------------------------------------------------------

(ol-define-key vdiff-mode-map "C-c" vdiff-mode-prefix-map)

(ol-define-key vdiff-mode-map "M-n" 'vdiff-next-hunk)
(ol-define-key vdiff-mode-map "M-p" 'vdiff-previous-hunk)
(ol-define-key vdiff-mode-map "M-l" 'ol-vdiff-fix-scroll)
(ol-define-key vdiff-mode-map "C-c f" 'ol-vdiff-refine-all-hunks)
(ol-define-key vdiff-mode-map "C-c F" 'vdiff-refine-this-hunk)
(ol-define-key vdiff-mode-map "C-c x" 'ol-vdiff-remove-all-refinements)
(ol-define-key vdiff-mode-map "C-c X" 'vdiff-remove-refinements-in-hunk)

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

(ol-evil-define-key 'normal dired-mode-map "o" 'dired-find-file)
(ol-evil-define-key 'normal dired-mode-map "i" 'dired-up-directory)

;; Seems to be the only way override space
(evil-collection-define-key 'normal 'dired-mode-map " " nil)

;; -----------------------------------------------------------------------------
;; tar-mode
;; -----------------------------------------------------------------------------

(ol-evil-define-key 'normal tar-mode-map "o" 'tar-extract)
(ol-evil-define-key 'normal tar-mode-map "i" 'ol-tar-up-directory)

;; -----------------------------------------------------------------------------
;; archive-mode
;; -----------------------------------------------------------------------------

(ol-evil-define-key 'normal archive-mode-map "o" 'archive-extract)

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

(ol-evil-define-key 'normal global-map "gr" 'revert-buffer-quick)

(ol-evil-define-key 'motion ivy-occur-grep-mode-map "o" 'ivy-occur-press)
(ol-evil-define-key 'motion ivy-occur-grep-mode-map "O" 'ivy-occur-press-and-switch)

(ol-define-normal-leader-key "fs" 'counsel-imenu)
(ol-define-normal-leader-key "mm" 'toggle-frame-maximized)
(ol-define-normal-leader-key "mt" 'ol-transpose-windows)
(ol-define-normal-leader-key "mc" 'ol-center-and-size-frame)
(ol-define-normal-leader-key "mw" 'ol-toggle-show-trailing-whitespace)
(ol-define-normal-leader-key "sc" 'ol-toggle-spelling)

;; TODO: Same if normal and read-only
(ol-define-key evil-motion-state-map "o" 'push-button)

;; -----------------------------------------------------------------------------
;; Mouse
;; -----------------------------------------------------------------------------

(defun ol-no-op ()
  (interactive))

(dolist (key '("<mouse-1>"
               "<mouse-2>"
               "<mouse-3>"
               "<down-mouse-1>"
               "<down-mouse-2>"
               "<down-mouse-3>"
               "<double-mouse-1>"
               "<double-mouse-2>"
               "<double-mouse-3>"
               "<triple-mouse-1>"
               "<triple-mouse-2>"
               "<triple-mouse-3>"
               "<drag-mouse-1>"
               "<drag-mouse-2>"
               "<drag-mouse-3>"
               "<mode-line> <down-mouse-1>"
               "<mode-line> <mouse-1>"))
  (global-set-key (kbd key) 'ol-no-op)
  (define-key button-map (kbd key) nil)
  (define-key evil-normal-state-map (kbd key) nil)
  (define-key evil-motion-state-map (kbd key) nil)
  (define-key evil-visual-state-map (kbd key) nil)
  (define-key evil-insert-state-map (kbd key) nil))

(setc mouse-1-click-follows-link nil)

;; -----------------------------------------------------------------------------
;; server
;; -----------------------------------------------------------------------------

(ol-define-key evil-normal-state-map "C-x #" #'ol-server-done)

