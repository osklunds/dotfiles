
;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; Wrappers
;;;; ---------------------------------------------------------------------------

;; To handle both GUI and terminal
(defun ol-map-key (key)
  (pcase key
    ('return  '("<return>" "RET"))
    ('tab     '("<tab>" "TAB"))
    ('backtab '("<backtab>" "S-TAB"))
    ('c-6     '("C-6" "C-^"))
    (_        `(,key))))

(defun ol-define-key (map key fun)
  (dolist (mapped-key (ol-map-key key))
    (define-key map (kbd mapped-key) fun)))

;; TODO: Delete this
(defun ol-global-set-key (key fun)
  (global-set-key (kbd key) fun))

(defun ol-evil-define-key (state map key fun)
  (dolist (mapped-key (ol-map-key key))
    (evil-define-key state map (kbd mapped-key) fun)))

;;;; ---------------------------------------------------------------------------
;;;; Leader
;;;; ---------------------------------------------------------------------------

(defvar ol-normal-leader-map (make-sparse-keymap))
(defvar ol-visual-leader-map (make-sparse-keymap))

(ol-define-key              evil-motion-state-map "SPC"   ol-normal-leader-map)
(ol-define-key              evil-motion-state-map "C-SPC" ol-normal-leader-map)
(ol-evil-define-key 'insert term-raw-map          "C-SPC" ol-normal-leader-map)
(ol-define-key              evil-visual-state-map "SPC"   ol-visual-leader-map)

(defun ol-define-normal-leader-key (key fun)
  (ol-define-key ol-normal-leader-map key fun))

(defun ol-define-visual-leader-key (key fun)
  (ol-define-key ol-visual-leader-map key fun))

;;;; ---------------------------------------------------------------------------
;;;; Override
;;;; ---------------------------------------------------------------------------

;; Overriding inspired by: https://emacs.stackexchange.com/a/358

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

;; Changing states
(ol-define-key evil-insert-state-map "C-g" 'evil-normal-state)

;; Window movement
(ol-define-key evil-motion-state-map "C-h" #'evil-window-left)
(ol-define-key evil-motion-state-map "C-l" #'evil-window-right)
(ol-define-key evil-insert-state-map "C-h" #'evil-window-left)
(ol-define-key evil-insert-state-map "C-l" #'evil-window-right)

;; Clear search highlights
(ol-define-key evil-normal-state-map "?" 'evil-ex-nohighlight)

;; Scolling
(ol-define-key evil-motion-state-map "M-j" 'evil-scroll-line-down)
(ol-define-key evil-motion-state-map "M-k" 'evil-scroll-line-up)

;; To work around a bug that prevents yank/delete full lines when doing
;; e.g. d2j. Affects also when a logical line fits the window width.
(ol-define-key evil-operator-state-map "j" 'evil-next-line)
(ol-define-key evil-operator-state-map "k" 'evil-previous-line)

;; I think this mapping makes more sense, to align down/j/{ and up/k/}
(ol-define-key evil-motion-state-map "}" 'evil-backward-paragraph)
(ol-define-key evil-motion-state-map "{" 'evil-forward-paragraph)

;; Movement
(ol-define-key evil-motion-state-map ")" 'evil-end-of-line-or-visual-line)
(ol-define-key evil-motion-state-map "^" 'evil-beginning-of-visual-line)
(ol-define-key evil-motion-state-map "0" 'evil-first-non-blank-of-visual-line) ;; Unclear why this isn't enough
(ol-define-key evil-normal-state-map "0" 'evil-first-non-blank-of-visual-line)
(ol-define-key evil-visual-state-map "0" 'evil-first-non-blank-of-visual-line)
(ol-define-key evil-operator-state-map "0" 'evil-first-non-blank-of-visual-line)

;; ---------------------------------------------------------------------------
;; Find and replace
;; ---------------------------------------------------------------------------

(ol-define-normal-leader-key "R" 'ol-full-replace-symbol)
(ol-define-normal-leader-key "r" 'ol-from-here-replace-symbol)

(ol-define-visual-leader-key "R" 'ol-full-replace-visual-selection)
(ol-define-visual-leader-key "r" 'ol-from-here-replace-visual-selection)

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

(ol-global-set-key "M-/" 'evilnc-comment-or-uncomment-lines)
(ol-evil-define-key 'normal prog-mode-map "gc" 'evilnc-comment-operator)

;;;;;; -------------------------------------------------------------------------
;;;;;; Completion
;;;;;; -------------------------------------------------------------------------

(ol-define-key company-active-map 'return 'company-abort)
(ol-define-key company-active-map "C-g" nil)
(ol-define-key company-active-map 'tab 'company-complete-selection)
(ol-define-key company-active-map "C-j" 'company-select-next)
(ol-define-key company-active-map "C-k" 'company-select-previous)

(ol-define-key prog-mode-map 'tab 'company-indent-or-complete-common)

;;;;----------------------------------------------------------------------------
;;;; Lisp
;;;; ---------------------------------------------------------------------------

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

;; ---------------------------------------------------------------------------
;; Projectile
;; ---------------------------------------------------------------------------

(ol-define-normal-leader-key "pp" 'projectile-switch-project)
(ol-define-normal-leader-key "ps" 'projectile-discover-projects-in-search-path)
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

(ol-define-normal-leader-key "gl" 'ol-git-log-dwim)

(ol-define-normal-leader-key "gdM" 'ol-diff-all-files-main)
(ol-define-normal-leader-key "gdH" 'ol-diff-all-files-head)
(ol-define-normal-leader-key "gdm" 'ol-diff-current-file-main)
(ol-define-normal-leader-key "gdh" 'ol-diff-current-file-head)

(ol-define-normal-leader-key "gt" 'ol-toggle-fundamental-mode)

;; To make sure leader works in magit buffers
(ol-define-key magit-mode-map "SPC" nil)
(ol-define-key magit-diff-mode-map "SPC" nil)

;; ---------------------------------------------------------------------------
;; Org mode
;; ---------------------------------------------------------------------------

(ol-evil-define-key 'visual org-mode-map "g q" 'org-fill-paragraph)
(ol-evil-define-key 'normal org-mode-map "g q q" 'org-fill-paragraph)

;; Indent and deindent lists
(ol-evil-define-key 'insert org-mode-map 'tab 'org-metaright)
(ol-evil-define-key 'insert org-mode-map 'backtab 'org-metaleft)

;; Toggle headers
(ol-evil-define-key 'normal org-mode-map 'tab 'org-cycle)

(ol-evil-define-key 'insert org-mode-map 'return 'ol-org-return)

;; ---------------------------------------------------------------------------
;; Ivy and Counsel
;; ---------------------------------------------------------------------------

(ol-override-key "C-j" 'ivy-switch-buffer)
(ol-global-set-key "C-x C-b" 'ivy-switch-buffer)

(ol-define-key ivy-minibuffer-map 'tab 'ol-ivy-done)
(ol-define-key ivy-minibuffer-map "M-u" 'universal-argument)
(ol-define-key ivy-minibuffer-map "C-j" 'ivy-next-line)
(ol-define-key ivy-minibuffer-map "C-k" 'ivy-previous-line)

(ol-define-key ivy-switch-buffer-map "C-k" 'ivy-previous-line)
(ol-define-key ivy-switch-buffer-map "C-d" 'ivy-switch-buffer-kill)

(ol-override-key "M-x" 'counsel-M-x)
(ol-global-set-key "C-x C-f" 'counsel-find-file)

(ol-evil-define-key 'motion ivy-occur-grep-mode-map "o" 'ivy-occur-press)
(ol-evil-define-key 'motion ivy-occur-grep-mode-map "O" 'ivy-occur-press-and-switch)
(ol-evil-define-key 'normal ivy-occur-mode-map "o" 'ivy-occur-press)
(ol-evil-define-key 'normal ivy-occur-mode-map "O" 'ivy-occur-press-and-switch)

;; -----------------------------------------------------------------------------
;; Terminal
;; -----------------------------------------------------------------------------

(ol-define-normal-leader-key "tt" 'ol-term-named)

;; Some normal state keybinds
(ol-evil-define-key 'insert term-raw-map "C-j" 'ivy-switch-buffer)
(ol-evil-define-key 'insert term-raw-map 'c-6 'evil-switch-to-windows-last-buffer)
(ol-evil-define-key 'insert term-raw-map "C-w k" 'evil-window-top)
(ol-evil-define-key 'insert term-raw-map "C-w j" 'evil-window-bottom)

;; Make the terminal experience more natural
(ol-evil-define-key 'insert term-raw-map "C-y" 'term-paste)
(ol-evil-define-key 'insert term-raw-map "C-d" 'term-send-raw)
(ol-evil-define-key 'insert term-raw-map "C-c" 'term-send-raw)

;; C-pnbf seem to more reliable in terminals in emacs, so remap arrow keys
(defmacro ol-define-term-key (from to)
  `(ol-evil-define-key 'insert
                       term-raw-map
                       ,from
                       (lambda () (interactive) (term-send-raw-string (kbd ,to)))))

(ol-define-term-key "<up>"    "C-p")
(ol-define-term-key "<down>"  "C-n")
(ol-define-term-key "<left>"  "C-b")
(ol-define-term-key "<right>" "C-f")

;; -----------------------------------------------------------------------------
;; Vdiff
;; -----------------------------------------------------------------------------

(ol-define-key vdiff-mode-map "C-c" vdiff-mode-prefix-map)

(ol-define-key vdiff-mode-map "M-n" 'vdiff-next-hunk)
(ol-define-key vdiff-mode-map "M-p" 'vdiff-previous-hunk)
(ol-define-key vdiff-mode-map "M-l" 'ol-vdiff-fix-scroll)

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
;; Dired
;; -----------------------------------------------------------------------------

(ol-global-set-key "C-x d" 'ol-dired)

(ol-evil-define-key 'normal dired-mode-map "o" 'dired-find-file)
(ol-evil-define-key 'normal dired-mode-map "i" 'dired-up-directory)

;; Seems to be the only way override space
(evil-collection-define-key 'normal 'dired-mode-map " " nil)

(ol-define-normal-leader-key "dh" (lambda () (interactive) (dired "~")))

;; -----------------------------------------------------------------------------
;; tar-mode
;; -----------------------------------------------------------------------------

;; Keybinds to mimic dired
(ol-evil-define-key 'normal tar-mode-map "o" 'tar-view)
(ol-evil-define-key 'normal tar-mode-map "i" 'ol-tar-up-directory)

;; -----------------------------------------------------------------------------
;; archive-mode
;; -----------------------------------------------------------------------------

;; Keybinds to mimic dired
(ol-evil-define-key 'normal archive-mode-map "o" 'archive-extract)
(ol-evil-define-key 'normal archive-mode-map "i" 'ol-archive-up-directory)

(ol-evil-define-key 'normal archive-mode-map "C" 'archive-copy-file)

;; ---------------------------------------------------------------------------
;; Misc
;; ---------------------------------------------------------------------------

(ol-define-key evil-insert-state-map 'tab 'ol-insert-tab)
(ol-evil-define-key 'insert term-raw-map 'tab (lambda () (interactive) (term-send-raw-string "\t")))
(ol-evil-define-key 'insert shell-mode-map 'tab 'completion-at-point)

(ol-override-key "M-:" 'eval-expression)
(ol-override-key "M-u" 'universal-argument)
(ol-override-key "M-h" 'help-command)
(ol-global-set-key "C-x C-s" 'ol-save-buffer)

(ol-evil-define-key 'normal global-map "gr" 'revert-buffer-quick)

(ol-global-set-key "C-x f" 'ol-find-file-empty)

;; TODO: Same if normal and read-only
(ol-define-key evil-motion-state-map "o" 'push-button)

;;;;----------------------------------------------------------------------------
;;;; Misc leader
;;;; ---------------------------------------------------------------------------

(ol-define-normal-leader-key "ms" 'ol-symbol-search)
(ol-define-normal-leader-key "mm" 'toggle-frame-maximized)
(ol-define-normal-leader-key "mt" 'ol-transpose-windows)
(ol-define-normal-leader-key "mc" 'ol-center-and-size-frame)
(ol-define-normal-leader-key "mw" 'ol-toggle-show-trailing-whitespace)
(ol-define-normal-leader-key "sc" 'ol-toggle-spelling)
(ol-define-normal-leader-key "bn" 'ol-print-buffer-file-name)
(ol-define-normal-leader-key "br" 'ol-rename-buffer)

;; -----------------------------------------------------------------------------
;; Compile/shell
;;------------------------------------------------------------------------------

(ol-define-key compilation-button-map "o" 'compile-goto-error)

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
               "<vertical-line> <down-mouse-1>"
               "<vertical-line> <mouse-1>"
               "<mode-line> <down-mouse-1>"
               "<mode-line> <mouse-1>"
               "<escape>"))
  (ol-global-set-key key 'ol-no-op)
  (ol-define-key button-map key nil)
  (ol-define-key ivy-minibuffer-map key nil)
  (ol-define-key ivy-occur-mode-map key nil)
  (ol-define-key ivy-occur-grep-mode-map key nil)
  (ol-define-key evil-normal-state-map key nil)
  (ol-define-key evil-motion-state-map key nil)
  (ol-define-key evil-visual-state-map key nil)
  (ol-define-key evil-insert-state-map key nil))

(setc mouse-1-click-follows-link nil)

;; -----------------------------------------------------------------------------
;; server
;; -----------------------------------------------------------------------------

(ol-define-key evil-normal-state-map "C-x #" #'ol-server-done)

;; -----------------------------------------------------------------------------
;; Merging
;; -----------------------------------------------------------------------------

(ol-define-normal-leader-key "gm" 'msk-mode-enable)
(ol-evil-define-key 'normal msk-mode-map "C-c q" 'msk-mode-disable)

(ol-evil-define-key 'normal msk-mode-map "M-1" 'msk-base-local)
(ol-evil-define-key 'normal msk-mode-map "M-2" 'msk-base-remote)
(ol-evil-define-key 'normal msk-mode-map "M-3" 'msk-local-remote)
(ol-evil-define-key 'normal msk-mode-map "M-4" 'msk-local-merged)
(ol-evil-define-key 'normal msk-mode-map "M-5" 'msk-remote-merged)
(ol-evil-define-key 'normal msk-mode-map "M-m" 'msk-original-buffer)

(ol-evil-define-key 'normal msk-mode-map "C-c l" 'smerge-keep-upper)
(ol-evil-define-key 'normal msk-mode-map "C-c r" 'smerge-keep-lower)
(ol-evil-define-key 'normal msk-mode-map "C-c b" 'smerge-keep-base)
(ol-evil-define-key 'normal msk-mode-map "C-c a" 'smerge-keep-all)

;; TODO evil define key would be better but didn't work
(ol-define-key smerge-mode-map "C-c n" 'smerge-next)
(ol-define-key smerge-mode-map "C-c p" 'smerge-prev)
(ol-define-key smerge-mode-map "C-c l" 'smerge-keep-upper)
(ol-define-key smerge-mode-map "C-c r" 'smerge-keep-lower)
(ol-define-key smerge-mode-map "C-c b" 'smerge-keep-base)
(ol-define-key smerge-mode-map "C-c a" 'smerge-keep-all)

