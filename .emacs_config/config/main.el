
;; -----------------------------------------------------------------------------
;; Helpers and preamble
;; -----------------------------------------------------------------------------

;; Taken from https://emacs.stackexchange.com/a/24658
(defun ol-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(defun ol-trace ()
  (interactive)
  (setq inhibit-trace nil)
  (call-interactively 'trace-function-background))

(defun ol-regexp-group (regexp string group)
  "Search STRING for REGEXP and return the match GROUP."
  (when (string-match regexp string)
    (match-string group string)))

(defmacro setc (var val)
  "Convenient version of customize-set-variable."
  `(customize-set-variable ',var ,val))

(defun ol-require-external (cmd)
  (cl-assert (executable-find cmd)))

;;;; ---------------------------------------------------------------------------
;;;; Keybinds
;;;;----------------------------------------------------------------------------

;; These must be set before evil is loaded
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-respect-visual-line-mode t)

(require 'evil)
(evil-mode t)

;;;;;; -------------------------------------------------------------------------
;;;;;; Wrappers
;;;;;; -------------------------------------------------------------------------

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

;; TODO: Can avoid 'normal instead of normal by doing a wrapper macro
;; that calls this fun instead
(defun ol-evil-define-key (state map key fun)
  (dolist (mapped-key (ol-map-key key))
    (evil-define-key state map (kbd mapped-key) fun)))

;;;;;; -------------------------------------------------------------------------
;;;;;; Leader
;;;;;; -------------------------------------------------------------------------

(defvar ol-normal-leader-map (make-sparse-keymap))
(defvar ol-visual-leader-map (make-sparse-keymap))

(ol-define-key evil-motion-state-map "SPC"   ol-normal-leader-map)
(ol-define-key evil-motion-state-map "C-SPC" ol-normal-leader-map)
(ol-define-key evil-visual-state-map "SPC"   ol-visual-leader-map)

(defun ol-define-normal-leader-key (key fun)
  (ol-define-key ol-normal-leader-map key fun))

(defun ol-define-visual-leader-key (key fun)
  (ol-define-key ol-visual-leader-map key fun))

;;;;;; -------------------------------------------------------------------------
;;;;;; Override
;;;;;; -------------------------------------------------------------------------

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

;;;; ---------------------------------------------------------------------------
;;;; Colors
;;;;----------------------------------------------------------------------------

(require 'doom-themes)
(load-theme 'doom-one-light t)

(defun ol-set-face (face &rest properties)
  (apply 'set-face-attribute (append (list face nil) properties)))

(defun ol-copy-face (to attribute from)
  (ol-set-face to attribute (face-attribute from attribute)))

(defun ol-copy-face-fg-bg (to from)
  (ol-copy-face to :foreground from)
  (ol-copy-face to :background from))

;; -----------------------------------------------------------------------------
;; Misc
;; -----------------------------------------------------------------------------

(setc enable-local-variables nil)

(defun ol-window-setup-hook ()
  (toggle-frame-maximized))

(add-hook 'window-setup-hook 'ol-window-setup-hook)

(ol-override-key "M-:" 'eval-expression)
(ol-override-key "M-u" 'universal-argument)
(ol-override-key "M-h" 'help-command)

(ol-evil-define-key 'normal global-map "gr" 'revert-buffer-quick)

;; TODO: Same if normal and read-only
(ol-define-key evil-motion-state-map "o" 'push-button)

(ol-define-normal-leader-key "mm" 'toggle-frame-maximized)

;; -----------------------------------------------------------------------------
;; File Management
;; -----------------------------------------------------------------------------

(defun ol-print-buffer-file-name ()
  (interactive)
  (message "%s" (buffer-file-name)))

(ol-define-normal-leader-key "bn" 'ol-print-buffer-file-name)

(defun ol-find-file-empty ()
  (interactive)
  (counsel-find-file nil ""))

(ol-global-set-key "C-x f" 'ol-find-file-empty)

;;;; ---------------------------------------------------------------------------
;;;; Backup
;;;; ---------------------------------------------------------------------------

;; No ~ files
(setc make-backup-files nil)

;; To prevent stutter when auto-saving. I use super-save and git to compensate
(setc auto-save-default nil)

;;;; ---------------------------------------------------------------------------
;;;; Save
;;;; ---------------------------------------------------------------------------

(require 'super-save)
(super-save-mode t)
(setq save-silently t)

(defun ol-save-buffer ()
  (interactive)
  (save-buffer)
  (message (format "Saved buffer: %s" (buffer-file-name))))

(ol-global-set-key "C-x C-s" 'ol-save-buffer)

(save-place-mode t)

;;;; ---------------------------------------------------------------------------
;;;; Auto revert
;;;; ---------------------------------------------------------------------------

(global-auto-revert-mode t)
(setc global-auto-revert-non-file-buffers t)
(setc auto-revert-verbose nil)
(setc revert-without-query '(".*"))

;; -----------------------------------------------------------------------------
;; Performance
;;------------------------------------------------------------------------------

(setc garbage-collection-messages nil)

(setq read-process-output-max (* 1024 1024)) ;; 1 MB

;; Supposedly can improve scroll performance
(setq auto-window-vscroll nil)

;; -----------------------------------------------------------------------------
;; User Interface
;; -----------------------------------------------------------------------------

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default show-trailing-whitespace nil)

(defun ol-toggle-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (message "Toggled show trailing. Now: %s" show-trailing-whitespace))

(ol-define-normal-leader-key "mw" 'ol-toggle-show-trailing-whitespace)

;; To make sure e.g. ♝ are monospaced
(set-fontset-font t 'symbol
                  (font-spec :family "DejaVu Sans Mono"))

(global-hl-line-mode)
(make-variable-buffer-local 'global-hl-line-mode)

;; The column at e.g. 80 chars
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(global-visual-line-mode t)
(setq-default visual-line-mode t)

(ol-set-face 'default :height 90)

(defconst ol-white "#ffffff") ;; ff works better than white in terminal
(defconst ol-black "#000000")

(ol-set-face 'default :foreground ol-black :background ol-white)
(ol-set-face 'font-lock-comment-face :foreground "#5f8700")
(ol-set-face 'font-lock-string-face :foreground "#d78700")

(unless (display-graphic-p)
  (ol-set-face 'lazy-highlight :background "#c2d3f7" :foreground ol-white)
  (ol-set-face 'hl-line :background "#eeeeee"))

;;;; ---------------------------------------------------------------------------
;;;; Reduce Clutter
;;;; ---------------------------------------------------------------------------

(setc inhibit-startup-screen t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

(setq visible-bell nil ring-bell-function #'ignore)

(setq frame-title-format "Emacs")

(setq mouse-highlight nil)
(setq show-help-function nil)
(setq command-error-function 'help-command-error-confusable-suggestions)

(setc native-comp-async-report-warnings-errors nil)

(setc warning-minimum-level :error)

(setc display-hourglass nil)

(setq-default fringe-indicator-alist
              '((continuation nil nil)
                (truncation left-arrow right-arrow)
                (overlay-arrow . right-triangle)
                (up . up-arrow)
                (down . down-arrow)
                (top top-left-angle top-right-angle)
                (bottom bottom-left-angle bottom-right-angle top-right-angle top-left-angle)
                (top-bottom left-bracket right-bracket top-right-angle top-left-angle)
                (empty-line . empty-line) (unknown . question-mark)))

(setc view-inhibit-help-message t)

;;;; ---------------------------------------------------------------------------
;;;; Line numbers
;;;; ---------------------------------------------------------------------------

(global-display-line-numbers-mode t)
(setc display-line-numbers-type 'relative)
(setc display-line-numbers-grow-only t)
(setc display-line-numbers-width-start 10000)

;;;; ---------------------------------------------------------------------------
;;;; No mouse
;;;;----------------------------------------------------------------------------

(require 'ivy)

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
;; Windows, buffers, frames
;; -----------------------------------------------------------------------------

(ol-define-key evil-normal-state-map "q" 'quit-window)

;;;; ---------------------------------------------------------------------------
;;;; Balanced windows
;;;; ---------------------------------------------------------------------------

(require 'balanced-windows)

(balanced-windows-mode)

;;;; ---------------------------------------------------------------------------
;;;; Only two windows
;;;; ---------------------------------------------------------------------------

(defvar ol-split-style 'vertical)

(defun ol-toggle-split-style ()
  (setq ol-split-style (if (eq ol-split-style 'vertical)
                           'horizontal
                         'vertical)))

(defun ol-split-based-on-style ()
  (if (eq ol-split-style 'vertical)
      (split-window-right)
    (split-window-below)))

(defun ol-split-window-sensibly (&optional window)
    (interactive)
    (let ((window (or window (selected-window))))
        (and (= 1 (count-windows))
                 (with-selected-window window
                     (ol-split-based-on-style)))))

(setc split-window-preferred-function #'ol-split-window-sensibly)

;;;; ---------------------------------------------------------------------------
;;;; Transposing
;;;; ---------------------------------------------------------------------------

(defun ol-transpose-windows ()
  (interactive)
  (if (not (equal (length (window-list)) 2))
      (message "Can't transpose if not exactly two windows")
    (ol-toggle-split-style)
    (let* ((this (selected-window))
           (other (next-window this))
           (left-top-selected (if (or (window-in-direction 'left)
                                      (window-in-direction 'above))
                                  nil
                                t)))
      (delete-window other)
      (ol-split-window-sensibly)
      (when left-top-selected
        (other-window 1))
      (switch-to-buffer (other-buffer))
      (other-window 1))))

(ol-define-normal-leader-key "mt" 'ol-transpose-windows)

;;;; ---------------------------------------------------------------------------
;;;; Splitting
;;;; ---------------------------------------------------------------------------

(defun ol-split-window ()
  (interactive)
  (let ((current-point (point))
        (current-window-start (window-start)))
    (switch-to-buffer-other-window (current-buffer))
    (set-window-point (selected-window) current-point)
    (set-window-start (selected-window) current-window-start)))

(ol-override-key "M-d" 'ol-split-window)

(defun ol-force-split-window ()
  (interactive)
  (split-window-right)
  (evil-window-right 1))

(ol-override-key "M-r" 'ol-force-split-window)

;; -----------------------------------------------------------------------------
;; Evil
;; -----------------------------------------------------------------------------

(evil-set-undo-system 'undo-redo)
(setc evil-want-C-u-scroll t)
(setc evil-search-module 'evil-search)
(setc evil-disable-insert-state-bindings t)
(setc evil-emacs-state-modes nil)
(setq evil-insert-state-cursor 'box)
(setc evil-want-Y-yank-to-eol t)

(require 'evil-collection)
(with-eval-after-load 'dired (evil-collection-dired-setup))
(with-eval-after-load 'magit (evil-collection-magit-setup))
(with-eval-after-load 'term (evil-collection-term-setup))
(with-eval-after-load 'ivy (evil-collection-ivy-setup))

(require 'evil-nerd-commenter)

;;;; ---------------------------------------------------------------------------
;;;; Words (don't come easy, to me)
;;;;----------------------------------------------------------------------------

(add-hook 'emacs-lisp-mode-hook (lambda () (modify-syntax-entry ?- "w")))
(add-hook 'after-change-major-mode-hook (lambda () (modify-syntax-entry ?_ "w")))

(defun ol-evil-ex-start-word-search-args-advice (unbounded direction count &optional symbol)
  `(t ,direction ,count ,symbol))

(advice-add 'evil-ex-start-word-search
            :filter-args
            (lambda (args) (apply 'ol-evil-ex-start-word-search-args-advice args)))

;;;; ---------------------------------------------------------------------------
;;;; Relative line jumps into jump list
;;;;----------------------------------------------------------------------------

(defun ol-evil-line-motion-add-to-jump-list-advice (&optional count)
  (when count
    (evil-set-jump)))

(advice-add 'evil-next-visual-line :before 'ol-evil-line-motion-add-to-jump-list-advice)
(advice-add 'evil-previous-visual-line :before 'ol-evil-line-motion-add-to-jump-list-advice)

;;;; ---------------------------------------------------------------------------
;;;; Operator tweaks
;;;;----------------------------------------------------------------------------

(defun ol-evil-operator-save-point-advice (func &rest args)
  (save-excursion
    (apply func args)))

(advice-add 'evil-indent :around 'ol-evil-operator-save-point-advice)
;; TODO: Doesn't work for the one below
(advice-add 'evilnc-comment-operator :around 'ol-evil-operator-save-point-advice)

(defun ol-evilnc-comment-operator-advice (start end type)
  ;; Always set type to 'line
  `(,start ,end 'line))

(advice-add 'evilnc-comment-operator
            :filter-args
            (lambda (args) (apply 'ol-evilnc-comment-operator-advice args)))

;;;; ---------------------------------------------------------------------------
;;;; Keybinds
;;;;----------------------------------------------------------------------------

;; Changing states
(ol-define-key evil-insert-state-map "C-n" 'evil-normal-state)
(ol-define-key evil-visual-state-map "C-n" 'evil-normal-state)

;; Window movement
(ol-define-key evil-motion-state-map "C-h" #'evil-window-left)
(ol-define-key evil-motion-state-map "C-l" #'evil-window-right)
(ol-define-key evil-insert-state-map "C-h" #'evil-window-left)
(ol-define-key evil-insert-state-map "C-l" #'evil-window-right)

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

(ol-define-key evil-insert-state-map 'tab 'ol-insert-tab)

;; -----------------------------------------------------------------------------
;; Text editing
;; -----------------------------------------------------------------------------

(setq-default tab-width 4)
(setq-default evil-shift-width 4)

(setq-default indent-tabs-mode nil)

(setq-default fill-column 80)

(defun ol-insert-tab ()
  (interactive)
  (insert "    "))

(defun ol-hide-chars ()
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'after-change-major-mode-hook 'ol-hide-chars)

;; -----------------------------------------------------------------------------
;; Find and replace
;; -----------------------------------------------------------------------------

(defconst ol-full-range "%")
(defconst ol-from-here-range ",$")

(defun ol-full-replace-visual-selection ()
  (interactive)
  (ol-replace-visual-selection ol-full-range))

(ol-define-visual-leader-key "R" 'ol-full-replace-visual-selection)

(defun ol-from-here-replace-visual-selection ()
  (interactive)
  (ol-replace-visual-selection ol-from-here-range))

(ol-define-visual-leader-key "r" 'ol-from-here-replace-visual-selection)

(defun ol-full-replace-symbol ()
  (interactive)
  (ol-replace-symbol ol-full-range))

(ol-define-normal-leader-key "R" 'ol-full-replace-symbol)

(defun ol-from-here-replace-symbol ()
  (interactive)
  (ol-replace-symbol ol-from-here-range))

(ol-define-normal-leader-key "r" 'ol-from-here-replace-symbol)

(defun ol-replace-symbol (range)
  (let ((text (thing-at-point 'symbol 'no-properties)))
    (ol-replace-text text range)))

(defun ol-replace-visual-selection (range)
  (let ((text (buffer-substring-no-properties (mark) (point))))
    (ol-replace-text text range)))

(defun ol-replace-text (text range)
  (let ((ex-command (format "%ss/%s/%s/gc" range text text)))
    (minibuffer-with-setup-hook
        (lambda () (backward-char 3))
      (evil-ex ex-command))))

(require 'evil-visualstar)

(global-evil-visualstar-mode)

(setc isearch-wrap-pause 'no-ding)
(setc isearch-lazy-count t)
(setc lazy-highlight-initial-delay 0)
(setc isearch-repeat-on-direction-change t)
(setc lazy-highlight-cleanup nil)

(defun ol-isearch-vim-like-forward ()
  (interactive)
  (evil-set-jump)
  (isearch-repeat-forward))

;; (ol-define-key evil-motion-state-map "n" 'ol-isearch-vim-like-forward)
(ol-define-key evil-motion-state-map "n" 'evil-search-next)

(defun ol-isearch-vim-like-backward ()
  (interactive)
  (evil-set-jump)
  (isearch-repeat-backward))

(ol-define-key evil-motion-state-map "N" 'evil-search-previous)

(defun ol-highlight-cleanup ()
  (interactive)
  ;; Doing both until I've migrated completely to iserach
  (lazy-highlight-cleanup t)
  (isearch-exit) ;; To remove the "current" highlight
  (evil-ex-nohighlight))

(ol-define-key evil-motion-state-map "?" 'ol-highlight-cleanup)
(ol-define-key evil-motion-state-map "/" 'evil-search-forward)

(defun ol-begin-search-word ()
  (interactive)
  (let ((string (evil-find-thing t 'evil-word)))
    (ol-begin-search string)))

(defun ol-begin-search (string)
  (isearch-forward nil 1)
  (isearch-yank-string string)
  (isearch-exit)
  (ol-isearch-vim-like-forward))

(ol-define-key evil-motion-state-map "*" 'evil-search-word-forward)

;; Inspiration from evil-visualstar
(defun ol-begin-search-selection (beg end)
  (when (evil-visual-state-p)
    (evil-exit-visual-state)
    (let ((selection (regexp-quote (buffer-substring-no-properties beg end))))
      (ol-begin-search selection))))

(evil-define-motion ol-visual-search (beg end)
  :jump t
  :repeat nil
  (interactive "<r>")
  (ol-visual-search beg end))

(ol-define-key evil-visual-state-map "*" 'evil-visualstar/begin-search-forward)

(defun ol-isearch-message-prefix (&rest args)
  (concat (isearch-lazy-count-format) " I-search: "))

(advice-add 'isearch-message-prefix :override 'ol-isearch-message-prefix)

;; -----------------------------------------------------------------------------
;; Ivy and counsel
;; -----------------------------------------------------------------------------

(require 'ivy)

(setc ivy-height 20)
(setc ivy-wrap t)

(ivy-mode t)

(defun ol-ivy-switch-buffer ()
  "Copy of ivy-switch-buffer, but allow visible buffers in preselect"
  (interactive)
  (let ((visible-ok t))
    (ivy-read "Switch to buffer: " #'internal-complete-buffer
              :keymap ivy-switch-buffer-map
              :preselect (buffer-name (other-buffer (current-buffer) visible-ok))
              :action #'ivy--switch-buffer-action
              :matcher #'ivy--switch-buffer-matcher
              :caller 'ivy-switch-buffer)))

(advice-add 'ivy-switch-buffer :override #'ol-ivy-switch-buffer)

(ol-override-key "C-j" 'ivy-switch-buffer)
(ol-global-set-key "C-x C-b" 'ivy-switch-buffer)

(require 'counsel)

(ivy-configure 'counsel-M-x
  :initial-input ""
  :display-transformer-fn #'counsel-M-x-transformer)

(ol-override-key "M-x" 'counsel-M-x)
(ol-global-set-key "C-x C-f" 'counsel-find-file)

(defun ol-ivy-done (&optional arg)
  (interactive "P")
  (if arg
      (ivy-partial-or-done)
    (ivy-alt-done)))

(defun ol-swiper--cleanup-advice (func &rest args)
  ;; So that swiper highlights are always cleaned up
  (let ((lazy-highlight-cleanup t))
    (apply func args)))

(advice-add 'swiper--cleanup :around 'ol-swiper--cleanup-advice)

(setc swiper-faces '(swiper-match-face-1
                     swiper-match-face-2
                     swiper-match-face-2
                     swiper-match-face-2))

(setq swiper-background-faces '(swiper-background-match-face-1
                                swiper-background-match-face-2
                                swiper-background-match-face-2
                                swiper-background-match-face-2))

(ol-define-key ivy-minibuffer-map 'tab 'ol-ivy-done)
(ol-define-key ivy-minibuffer-map "M-u" 'universal-argument)
(ol-define-key ivy-minibuffer-map "C-j" 'ivy-next-line)
(ol-define-key ivy-minibuffer-map "C-k" 'ivy-previous-line)

(ol-define-key ivy-switch-buffer-map "C-k" 'ivy-previous-line)
(ol-define-key ivy-switch-buffer-map "C-d" 'ivy-switch-buffer-kill)

(ol-evil-define-key 'motion ivy-occur-grep-mode-map "o" 'ivy-occur-press)
(ol-evil-define-key 'motion ivy-occur-grep-mode-map "O" 'ivy-occur-press-and-switch)
(ol-evil-define-key 'normal ivy-occur-mode-map "o" 'ivy-occur-press)
(ol-evil-define-key 'normal ivy-occur-mode-map "O" 'ivy-occur-press-and-switch)

;; -----------------------------------------------------------------------------
;; Find file name
;; -----------------------------------------------------------------------------

;; TODO: Prefix arg that means not following ignore
;; Could also consider transient for ripgrep
(defun ol-dwim-find-file-name ()
  "Search for file names."
  (interactive)
  (if-let ((root (ol-dwim-use-project-root)))
      (ol-project-find-file-name root)
    (ol-cwd-find-file-name)))

(defun ol-dwim-use-project-root ()
  (and (not (equal major-mode 'dired-mode)) (projectile-project-root)))

(defun ol-project-find-file-name (&optional root)
  "Search for file names in the current project."
  (interactive)
  (ol-find-file-name (or root (projectile-project-root)) "project"))

(defun ol-cwd-find-file-name ()
  "Search for file names in the current directory."
  (interactive)
  (ol-find-file-name default-directory "cwd"))

;; Inspired by counsel-file-jump
;; TODO: Make async like counsel-rg
(defun ol-find-file-name (directory prompt)
  (let* ((default-directory directory)
         (cmd-and-args (ol-find-file-name-command-and-args))
         (cmd (car cmd-and-args))
         (find-program cmd) ;; Overrides the program used for finding
         (find-program-args (cadr cmd-and-args)))
    (ivy-read (concat "Find file [" prompt "]: ")
              (counsel--find-return-list find-program-args)
              :action #'find-file
              :preselect (counsel--preselect-file)
              :require-match 'confirm-after-completion
              :history 'file-name-history
              :keymap counsel-file-jump-map
              :caller 'ol-find-file-name)))

(defun ol-find-file-name-command-and-args ()
  (let ((candidates all-ol-find-file-name-command-and-args)
        (result nil))
    (while (not result)
      (let* ((candidate (car candidates))
             (cmd (car candidate)))
        (if (executable-find cmd)
              (setq result candidate)
          (setq candidates (cdr candidates)))))
    result))

(defconst all-ol-find-file-name-command-and-args
  (list (list "rg" '("--files"))
        (list "find" counsel-file-jump-args)))

(ol-require-external "rg")

;; To handle rg returning error codes even if partial result
;; Inspired/copied from
;; https://github.com/doomemacs/doomemacs/issues/3038#issuecomment-832077836

(defun ol-counsel--call-advice (func &rest args)
  (let* ((old-fun (symbol-function #'process-file)))
    (cl-letf (((symbol-function 'process-file)
               (lambda (&rest process-file-args)
                 (apply old-fun process-file-args)
                 0)))
      (apply func args))))

(advice-add 'counsel--call :around 'ol-counsel--call-advice)

;; -----------------------------------------------------------------------------
;; Find file content
;; -----------------------------------------------------------------------------

(setc counsel-rg-base-command "\
rg \
--max-columns 240 \
--with-filename \
--no-heading \
--line-number \
--color never \
%s || true")

(defun ol-dwim-find-file-content (&optional arg)
  "Search for file content."
  (interactive "P")
  (if arg
      (if-let ((root (ol-dwim-use-project-root)))
          (ol-project-find-file-content root)
        (ol-cwd-find-file-content))
    (swiper)))

(defun ol-project-find-file-content (&optional root)
  "Search for file content in the current project."
  (interactive)
  (ol-find-file-content (or root (projectile-project-root)) "project"))

(defun ol-cwd-find-file-content ()
  "Search for file content in the current directory."
  (interactive)
  (ol-find-file-content default-directory "cwd"))

(defun ol-find-file-content (directory prompt)
  (if (file-remote-p directory)
      (ol-sync-find-file-content directory prompt)
    (ol-async-find-file-content directory prompt)))

(defun ol-async-find-file-content (directory prompt)
  (counsel-rg "" directory "" (concat "Find file content [" prompt "]: ")))

(defun ol-sync-find-file-content (directory prompt)
  (let* ((prompt (concat "Find file content sync [" prompt "]: "))
         (pattern (read-from-minibuffer prompt))
         (cmd (concat "rg --no-heading --line-number --with-filename \"" pattern "\""))
         (buffer (get-buffer-create "*ol-sync-find-file-content*")))
    (shell-command cmd buffer)
    (with-current-buffer buffer
      (setq default-directory directory)
      (compilation-mode t))
    (switch-to-buffer-other-window buffer)))

(ol-define-key compilation-button-map "o" 'compile-goto-error)

(defun ol-swiper--line-advice (func &rest args)
  (cl-letf (((symbol-function 'buffer-substring) 'buffer-substring-no-properties))
    (apply func args)))

(advice-add 'swiper--line :around 'ol-swiper--line-advice)

;; -----------------------------------------------------------------------------
;; Programming
;; -----------------------------------------------------------------------------

(defun ol-symbol-search (&optional arg)
  (interactive "P")
  (if (equal major-mode 'org-mode)
      (org-goto)
    (when arg
      (setq imenu--index-alist nil))
    (counsel-imenu)))

(ol-define-normal-leader-key "ms" 'ol-symbol-search)

(setc imenu-max-item-length 200)

(ol-global-set-key "M-/" 'evilnc-comment-or-uncomment-lines)
(ol-evil-define-key 'normal prog-mode-map "gc" 'evilnc-comment-operator)

;; -----------------------------------------------------------------------------
;; LSP
;; -----------------------------------------------------------------------------

(require 'lsp-mode)

;; Reduce noise
(setc lsp-enable-symbol-highlighting nil)
(setc lsp-modeline-code-actions-enable nil)
(setc lsp-modeline-diagnostics-enable nil)
(setc lsp-diagnostics-provider :none) ;; TODO: try out toggling diagnostics
(setc lsp-ui-sideline-enable nil)
(setc lsp-modeline-workspace-status-enable nil)
(setc lsp-lens-enable nil)
(setc lsp-ui-doc-enable nil)
(setc lsp-headerline-breadcrumb-enable nil)
(setc lsp-eldoc-enable-hover nil)
(setc lsp-signature-auto-activate nil)
(setc lsp-enable-snippet nil)
(setc flycheck-indication-mode nil)

(setc lsp-auto-guess-root t) ;; so that new files don't ask about project
(setc lsp-completion-provider :none) ;; to prevent overriding my own company backends
(setc lsp-response-timeout 10)
(setc lsp-enable-file-watchers nil) ;; to prevent "nested too deep" warning
(setc lsp-log-io nil) ;; Enable for easier debugging

(setc lsp-log-io nil)

;; -----------------------------------------------------------------------------
;; Abbreviations (for completions)
;; -----------------------------------------------------------------------------

;; Copied from https://stackoverflow.com/a/15389612
(defadvice expand-abbrev (after ol-expand-abbrev activate)
   ;; if there was an expansion
   (if ad-return-value
       ;; start idle timer to ensure insertion of abbrev activator
       ;; character (e.g. space) is finished
       (run-with-idle-timer 0 nil
                            (lambda ()
                              ;; if there is the string "@@" in the
                              ;; expansion then move cursor there and
                              ;; delete the string
                              (let ((cursor "@@"))
                                (if (search-backward cursor last-abbrev-location t)
                                    (delete-char (length cursor))))))))

(setc save-abbrevs 'silently)

;; -----------------------------------------------------------------------------
;; Completion
;; -----------------------------------------------------------------------------

(require 'company)
(require 'company-box)

(setc company-backends '((company-abbrev
                          :separate
                          company-capf
                          :separate
                          company-dabbrev-code)))

(defun ol-shell-company-backends ()
  (setq-local company-backends '((company-abbrev
                                  :separate
                                  company-dabbrev-code))))

(add-hook 'sh-mode-hook 'ol-shell-company-backends)

(setc company-minimum-prefix-length 1)
(setc company-idle-delay 0.0)
(setc company-selection-wrap-around t)
(setc company-tooltip-align-annotations t)

(setc company-dabbrev-minimum-length 2)
(setc company-dabbrev-other-buffers nil)
(setc company-dabbrev-code-other-buffers nil)
(setc company-dabbrev-code-everywhere t)
(setc company-dabbrev-code-modes t)

(add-hook 'text-mode-hook 'company-mode)
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'org-mode-hook 'company-mode)

(add-hook 'company-mode-hook 'company-box-mode)

(add-hook 'evil-insert-state-exit-hook 'company-abort)

;; Making ivy and company look consistent
(dolist (face '(ivy-minibuffer-match-face-1
                ivy-minibuffer-match-face-2
                ivy-minibuffer-match-face-3
                ivy-minibuffer-match-face-4))
  (ol-copy-face-fg-bg face 'company-tooltip-common)
  (ol-set-face face :weight 'bold)
  (ol-set-face face :background ol-white))

(defconst ol-completion-selection-color "#d7e4e8")

(ol-set-face 'ivy-current-match :weight 'bold)
(ol-set-face 'ivy-current-match :background ol-completion-selection-color)
(ol-set-face 'company-box-background :background ol-white)
(ol-set-face 'company-box-selection :background ol-completion-selection-color)

(ol-define-key company-active-map 'return 'company-abort)
(ol-define-key company-active-map "C-g" nil)
(ol-define-key company-active-map "C-n" nil)
(ol-define-key company-active-map 'tab 'company-complete-selection)
(ol-define-key company-active-map "C-j" 'company-select-next)
(ol-define-key company-active-map "C-k" 'company-select-previous)

(ol-define-key prog-mode-map 'tab 'company-indent-or-complete-common)

;; -----------------------------------------------------------------------------
;; Emacs Lisp
;; -----------------------------------------------------------------------------

(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  '(
    ("dbg" "(message \"oskar: %s\" @@)")
    ("sep1" ";; -----------------------------------------------------------------------------\n;; @@\n;;------------------------------------------------------------------------------")
    ("sep2" ";;;; ---------------------------------------------------------------------------\n;;;; @@\n;;;;----------------------------------------------------------------------------")
    ("sep3" ";;;;;; -------------------------------------------------------------------------\n;;;;;; @@\n;;;;;;--------------------------------------------------------------------------")
    ))

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

;; -----------------------------------------------------------------------------
;; Projectile
;; -----------------------------------------------------------------------------

(require 'projectile)

(setc projectile-project-search-path '(("~/own_repos" . 1)
                                       ("~/others_repos" . 1)
                                       ("~/own_repos/dotfiles/.emacs_config/packages" . 1)
                                       ("~/Dropbox/Dokument")))

(setc projectile-completion-system 'ivy)

(setc projectile-switch-project-action 'ol-dwim-find-file-name)

(require 'counsel-projectile)
(require 'projectile-ripgrep)

(setc ivy-more-chars-alist '((t . 1)))

(call-interactively 'projectile-mode)

(ol-define-normal-leader-key "pp" 'projectile-switch-project)
(ol-define-normal-leader-key "ps" 'projectile-discover-projects-in-search-path)
(ol-define-normal-leader-key "pd" 'ol-switch-to-dotfiles)

(defun ol-switch-to-dotfiles ()
  (interactive)
  (projectile-switch-project-by-name "~/dotfiles"))

(ol-override-key "M-q" 'ol-dwim-find-file-name)
(ol-override-key "M-e" 'ol-dwim-find-file-content)

;; -----------------------------------------------------------------------------
;; Git and Magit
;; -----------------------------------------------------------------------------

(require 'magit)
(ol-require-external "git")

;; To make sure leader works in magit buffers
(ol-define-key magit-mode-map "SPC" nil)
(ol-define-key magit-diff-mode-map "SPC" nil)

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
      (beginning-of-buffer))))

(add-hook 'git-commit-setup-hook 'ol-git-commit-setup)

;;;; ---------------------------------------------------------------------------
;;;; Status
;;;; ---------------------------------------------------------------------------

(setc magit-save-repository-buffers 'dontask)
(setc magit-status-initial-section nil)
(setc vdiff-magit-dwim-show-on-hunks t)

(defun ol-magit-set-simple-status-header ()
  (magit-set-header-line-format "Magit Status"))

(defconst ol-magit-status-simple-sections
  '(ol-magit-set-simple-status-header
    magit-insert-untracked-files
    magit-insert-unstaged-changes
    magit-insert-staged-changes
    magit-insert-stashes))

(defun ol-magit-set-full-status-header ()
  (magit-set-header-line-format "Magit Full Status"))

(defconst ol-magit-status-full-sections
  '(ol-magit-set-full-status-header
    magit-insert-unpushed-to-pushremote
    magit-insert-unpushed-to-upstream
    magit-insert-unpulled-from-pushremote
    magit-insert-unpulled-from-upstream))

(defun ol-magit-status ()
  (interactive)
  (setc magit-status-sections-hook ol-magit-status-simple-sections)
  (magit-status))

(ol-define-normal-leader-key "gs" 'ol-magit-status)

(defun ol-magit-full-status ()
  (interactive)
  (setc magit-status-sections-hook (append
                                    ol-magit-status-simple-sections
                                    ol-magit-status-full-sections))
  (magit-status))

(ol-define-normal-leader-key "gS" 'ol-magit-full-status)

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

(defun ol-diff-current-file (rev-other)
  (let* ((file (magit-current-file))
         (file-other (magit--rev-file-name file "HEAD" rev-other))
         (buffer-other (ol-get-revision-buffer rev-other file-other)))
    (vdiff-buffers buffer-other (current-buffer))))

(defun ol-get-revision-buffer (rev file)
  (magit-get-revision-buffer rev file (magit-find-file-noselect rev file)))

;;;; ---------------------------------------------------------------------------
;;;; Log
;;;; ---------------------------------------------------------------------------

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
                     (t (list file))))
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

(defun ol-magit-set-revision-header ()
  (magit-set-header-line-format (magit-rev-format "%B" magit-buffer-revision)))

(magit-add-section-hook 'magit-revision-sections-hook 'ol-magit-set-revision-header)
(remove-hook 'magit-revision-sections-hook 'magit-insert-revision-message)

;;;; ---------------------------------------------------------------------------
;;;; Merge conflicts
;;;; ---------------------------------------------------------------------------

(require 'smerge-mode)
(require 'merge-survival-knife)

;; To make sure smerge doesn't add refinements to conflicts
(setc diff-refine nil)

;; Copied/inspired from, to automatically start smerge
;; https://stumbles.id.au/auto-starting-emacs-smerge-mode-for-git.html
(defun vc-git-find-file-hook ()
  (when (save-excursion
      (goto-char (point-min))
      (re-search-forward "^<<<<<<< " nil t))
    (smerge-mode)))

(defun ol-smerge-set-face (face-to-set face-val)
  (ol-set-face face-to-set :background face-val :foreground ol-black :weight 'normal))

(ol-smerge-set-face 'smerge-base ol-diff-dark-orange)
(ol-smerge-set-face 'smerge-lower ol-diff-light-red)
(ol-smerge-set-face 'smerge-upper ol-diff-green)

;; Copied and modified from smerge-mode. This is to make sure markers override
;; keywords from other modes
(defconst smerge-font-lock-keywords
  '((smerge-find-conflict
     (0 smerge-markers-face prepend t)
     (1 smerge-upper-face prepend t)
     (2 smerge-base-face prepend t)
     (3 smerge-lower-face prepend t)
     (4 nil t t)
     (5 nil t t))))

(defun ol-msk-original-buffer-fix-keybinds (&rest args)
  (evil-force-normal-state))

(advice-add 'msk-original-buffer :after 'ol-msk-original-buffer-fix-keybinds)

(ol-define-normal-leader-key "gm" 'msk-mode-enable)
(ol-evil-define-key 'normal msk-mode-map "C-c q" 'msk-mode-disable)

(ol-evil-define-key 'normal msk-mode-map "M-1" 'msk-base-local)
(ol-evil-define-key 'normal msk-mode-map "M-2" 'msk-base-remote)
(ol-evil-define-key 'normal msk-mode-map "M-3" 'msk-local-remote)
(ol-evil-define-key 'normal msk-mode-map "M-4" 'msk-local-merged)
(ol-evil-define-key 'normal msk-mode-map "M-5" 'msk-remote-merged)

(ol-evil-define-key 'normal msk-mode-map "M-8" 'msk-local-changes-compare)
(ol-evil-define-key 'normal msk-mode-map "M-9" 'msk-remote-changes-compare)

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


;;;; ---------------------------------------------------------------------------
;;;; Helpers
;;;; ---------------------------------------------------------------------------

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

(defun ol-merge-base (rev-a rev-b)
  (magit-commit-p (magit-git-string "merge-base" rev-a rev-b)))

;; -----------------------------------------------------------------------------
;; Org mode
;; -----------------------------------------------------------------------------

(require 'org)
(require 'org-faces)

(setc org-ellipsis " ▾")

(setc org-src-preserve-indentation t)
(setc org-edit-src-content-indentation 0)

(add-to-list 'auto-mode-alist '("\\.org.txt\\'" . org-mode))

(setc org-goto-interface 'outline-path-completion)
(setc org-outline-path-complete-in-steps nil)

;; TODO: Do something similar for evil-open, i.e. o
(defun ol-org-return ()
  (interactive)
  (if (ol-org-in-item-p)
      (org-insert-item)
    (org-return)))

(defun ol-org-in-item-p ()
  (string-match-p "^ *-" (thing-at-point 'line t)))

(ol-set-face 'org-block :background
             (color-darken-name
              (face-attribute 'default :background) 3))

(ol-evil-define-key 'visual org-mode-map "g q" 'org-fill-paragraph)
(ol-evil-define-key 'normal org-mode-map "g q q" 'org-fill-paragraph)

;; Indent and deindent lists
(ol-evil-define-key 'insert org-mode-map 'tab 'org-metaright)
(ol-evil-define-key 'insert org-mode-map 'backtab 'org-metaleft)

;; Toggle headers
(ol-evil-define-key 'normal org-mode-map 'tab 'org-cycle)

(ol-evil-define-key 'insert org-mode-map 'return 'ol-org-return)

;; -----------------------------------------------------------------------------
;; Spelling
;; -----------------------------------------------------------------------------

(defun ol-toggle-spelling ()
  (interactive)
  (unless flyspell-mode
    (flyspell-buffer))
  (call-interactively 'flyspell-mode))

(ol-define-normal-leader-key "sc" 'ol-toggle-spelling)

;; -----------------------------------------------------------------------------
;; Terminal
;; -----------------------------------------------------------------------------

;; Notes for myself on terminals
;; You can only edit text in either line mode or char mode - never mixed. So
;; workflows could look like
;; Line mode: type text in the prompt and jump around using normal movement ops.
;; When you're done, press enter. One caveat is that I have to make evil insert
;; not enter char mode for this.
;; Char mode: like a regular terminal. Note, you can still use normal mode for
;; read only to other parts of the shell, then go to insert mode and paste using
;; C-y.
;; To begin with, I'll stick to char mode, as it's the most similar to
;; terminals I'm used to.
;; For line mode, shell works better than term/ansi-term. In shell, company mode
;; works but not in term.

(require 'term)

(defun ol-disable-cursorline-for-terms ()
  (when (equal major-mode 'term-mode)
      (setq global-hl-line-mode nil)))

(defun ol-enable-cursorline-for-terms ()
  (when (equal major-mode 'term-mode)
      (setq global-hl-line-mode t)))

(add-hook 'evil-insert-state-entry-hook 'ol-disable-cursorline-for-terms)
(add-hook 'evil-insert-state-exit-hook 'ol-enable-cursorline-for-terms)

(defun ol-set-term-buffer-maximum-size ()
  (setc term-buffer-maximum-size 10000000000))

(add-hook 'term-mode-hook 'ol-set-term-buffer-maximum-size)

;; Hack to do it like this. If done directly, colors aren't set it seems
(defun ol-set-term-colors ()
  ;; TODO Do this, setting all colors:
  ;; https://emacs.stackexchange.com/questions/28825/how-do-you-set-colors-for-term
  (ol-set-face 'term-color-black :foreground ol-black :background ol-white)
  (ol-set-face 'term :foreground ol-black :background ol-white))

(add-hook 'term-mode-hook 'ol-set-term-colors)

;; Some normal state keybinds
(ol-evil-define-key 'insert term-raw-map "C-j" 'ivy-switch-buffer)
(ol-evil-define-key 'insert term-raw-map 'c-6 'evil-switch-to-windows-last-buffer)
(ol-evil-define-key 'insert term-raw-map "C-w k" 'evil-window-top)
(ol-evil-define-key 'insert term-raw-map "C-w j" 'evil-window-bottom)

;; Make the terminal experience more natural
(ol-evil-define-key 'insert term-raw-map "C-y" 'term-paste)
(ol-evil-define-key 'insert term-raw-map "C-d" 'term-send-raw)
(ol-evil-define-key 'insert term-raw-map "C-c" 'term-send-raw)

(ol-evil-define-key 'insert term-raw-map 'tab (lambda ()
                                                (interactive)
                                                (term-send-raw-string "\t")))
(ol-evil-define-key 'insert shell-mode-map 'tab 'completion-at-point)

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

;;;; ---------------------------------------------------------------------------
;;;; Functions for opening a terminal
;;;;----------------------------------------------------------------------------

(defun ol-term ()
  (interactive)
  (ansi-term shell-file-name))

(defun ol-term-named (name &optional cmd-on-create)
  (interactive (list (read-string "Name: " nil nil "terminal")))
  (let* ((term-name (ol-name-to-term-buffer-name name))
         (existing-buffer (get-buffer term-name))
         (new-buffer (if existing-buffer
                         existing-buffer
                       (ol-term))))
    (switch-to-buffer new-buffer)
    (rename-buffer term-name)
    (when (and cmd-on-create (not existing-buffer))
      (process-send-string new-buffer (concat cmd-on-create "\n")))
    new-buffer))

(ol-define-normal-leader-key "tt" 'ol-term-named)

(defun ol-name-to-term-buffer-name (name)
  (concat "*" name "*"))

(setq kill-buffer-query-functions nil)
(setc confirm-kill-processes nil)

;;;; ---------------------------------------------------------------------------
;;;; Long lines
;;;;----------------------------------------------------------------------------

(defun ol-window-max-chars-per-line (oldfun &optional window face)
  (let* ((buffer (window-buffer window))
         (mm (with-current-buffer (window-buffer window) major-mode)))
    (if (eq mm 'term-mode)
        (progn
          ;; (message "Changing window-max-chars for term-mode window: %s" window)
          1000)
      ;; (message "Leaving normal window-max-chars for non term-mode window: %s" window)
      (apply oldfun (list window face)))))

(advice-add 'window-max-chars-per-line :around 'ol-window-max-chars-per-line)

;; -----------------------------------------------------------------------------
;; emacs server
;; -----------------------------------------------------------------------------
         
(defun ol-start-server ()
  (interactive)
  (unless (server-running-p)
    (setc server-name (ol-find-free-server-name))
    (setenv "EMACS_SERVER_NAME" server-name)
    (server-start)))

(defun ol-find-free-server-name ()
  (let* ((base-server-name "ol-server")
         (current-index 0)
         (found nil)
         (current-name nil))
    (while (not found)
      (setq current-name (format "%s-%d" base-server-name current-index))
      (if (server-running-p current-name)
          (setq current-index (+ current-index 1))
        (setq found t)))
    current-name))

(ol-start-server)

(defun ol-server-done ()
  (interactive)
  (save-buffer)
  (server-done))

(ol-define-key evil-normal-state-map "C-x #" #'ol-server-done)

;; -----------------------------------------------------------------------------
;; Vdiff
;; -----------------------------------------------------------------------------

(require 'vdiff)
(require 'vdiff-magit)

(setc vdiff-subtraction-fill-char ? )

(setc vdiff-diff-algorithm 'diff)
(ol-require-external "diff")

(setc vdiff-fold-padding 10)

(defun ol-vdiff-fold-string (n-lines first-line-text width)
  (format "   %d lines\n" n-lines))

(setc vdiff-fold-string-function 'ol-vdiff-fold-string)

(setc vdiff-magit-stage-is-2way t)

;;;; ---------------------------------------------------------------------------
;;;; Colors
;;;;----------------------------------------------------------------------------

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

;;;; ---------------------------------------------------------------------------
;;;; Synced scroll
;;;; ---------------------------------------------------------------------------

(defun ol-vdiff-fix-scroll ()
  (interactive)
  (vdiff--scroll-function))

;; TODO: Calling fix scroll automatically as part of vdiff next hunk doesn't
;; work. Probably related to the fixes/bugs I had to do for msk.

;;;; ---------------------------------------------------------------------------
;;;; Truncate lines
;;;; ---------------------------------------------------------------------------

(advice-add 'vdiff-buffers :after (lambda (&rest r)
                                    (ol-enable-truncate-lines)
                                    (other-window 1)
                                    (ol-enable-truncate-lines)))

(defun ol-enable-truncate-lines ()
  (unless truncate-lines
    (toggle-truncate-lines)))

;;;; ---------------------------------------------------------------------------
;;;; Cleaning up buffers
;;;; ---------------------------------------------------------------------------

(defun ol-vdiff-new-args (buffer-a
                          buffer-b
                          &optional
                          rotate
                          on-quit
                          restore-windows-on-quit
                          kill-buffers-on-quit)
  (let* ((new-on-quit (lambda (buf-a buf-b)
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

;;;; ---------------------------------------------------------------------------
;;;; Refine
;;;; ---------------------------------------------------------------------------

(setc vdiff-auto-refine t)

(defun ol-vdiff-refine-all-hunks ()
  (interactive)
  (setc vdiff-auto-refine t)
  (vdiff-refresh))

(defun ol-vdiff-remove-all-refinements ()
  (interactive)
  (setc vdiff-auto-refine nil)
  (vdiff-refresh))

;;;; ---------------------------------------------------------------------------
;;;; Keybinds
;;;;----------------------------------------------------------------------------

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
;; Ediff
;; -----------------------------------------------------------------------------

(require 'ediff)

(setc ediff-window-setup-function 'ediff-setup-windows-plain)
(setc ediff-split-window-function 'split-window-horizontally)

;; Copied from https://emacs.stackexchange.com/a/24602
(defun ol-disable-y-or-n-p (orig-fun &rest args)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (apply orig-fun args)))

(advice-add 'ediff-quit :around #'ol-disable-y-or-n-p)

;; These actually made some more sense once I understood them. In ediff, there's
;; a "current" diff, and "other" diffs. The currently selected diff is
;; highlighted using these "current" faces below. The non-selected other diffs
;; are highlighted alternatingly with the odd and even faces.

(ol-copy-face-fg-bg 'ediff-current-diff-A        'magit-diff-removed)
(ol-copy-face-fg-bg 'ediff-current-diff-B        'magit-diff-added)
(ol-copy-face-fg-bg 'ediff-current-diff-C        'magit-diff-added)
(ol-copy-face-fg-bg 'ediff-current-diff-Ancestor 'magit-diff-base)

(ol-copy-face-fg-bg 'ediff-fine-diff-A           'magit-diff-removed-highlight)
(ol-copy-face-fg-bg 'ediff-fine-diff-B           'magit-diff-added-highlight)
(ol-copy-face-fg-bg 'ediff-fine-diff-C           'magit-diff-added-highlight)
(ol-copy-face-fg-bg 'ediff-fine-diff-Ancestor    'magit-diff-base-highlight)

(ol-copy-face-fg-bg 'ediff-even-diff-A           'magit-diff-removed)
(ol-copy-face-fg-bg 'ediff-even-diff-B           'magit-diff-added)
(ol-copy-face-fg-bg 'ediff-even-diff-C           'magit-diff-added)
(ol-copy-face-fg-bg 'ediff-even-diff-Ancestor    'magit-diff-base)

(ol-copy-face-fg-bg 'ediff-odd-diff-A            'magit-diff-removed)
(ol-copy-face-fg-bg 'ediff-odd-diff-B            'magit-diff-added)
(ol-copy-face-fg-bg 'ediff-odd-diff-C            'magit-diff-added)
(ol-copy-face-fg-bg 'ediff-odd-diff-Ancestor     'magit-diff-base)

;; -----------------------------------------------------------------------------
;; Modeline
;; -----------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; Faces
;;;; ---------------------------------------------------------------------------

(defface ol-evil-normal-state-mode-line-face '() "")
(defface ol-evil-insert-state-mode-line-face '() "")
(defface ol-evil-visual-state-mode-line-face '() "")
(defface ol-evil-emacs-state-mode-line-face '() "")
(defface ol-evil-operator-state-mode-line-face '() "")
(defface ol-buffer-name-mode-line-face '() "")

(ol-set-face 'mode-line :overline 'unspecified :underline 'unspecified)

(if (display-graphic-p)
    (ol-set-face 'mode-line :background "#d7e4e8")
  (ol-set-face 'mode-line :background "#cccccc"))

(ol-set-face 'mode-line-inactive
             :background "#e9eded"
             :overline 'unspecified
             :underline 'unspecified)

(ol-set-face 'ol-buffer-name-mode-line-face
             :weight 'bold)

(ol-copy-face-fg-bg 'ol-evil-normal-state-mode-line-face 'font-lock-comment-face)
(ol-copy-face-fg-bg 'ol-evil-insert-state-mode-line-face 'font-lock-keyword-face)
(ol-copy-face-fg-bg 'ol-evil-visual-state-mode-line-face 'warning)
(ol-copy-face-fg-bg 'ol-evil-emacs-state-mode-line-face 'font-lock-builtin-face)

(dolist (face '(ol-evil-normal-state-mode-line-face
                ol-evil-insert-state-mode-line-face
                ol-evil-visual-state-mode-line-face
                ol-evil-emacs-state-mode-line-face
                ol-evil-operator-state-mode-line-face))
  (ol-set-face face :weight 'bold))

;;;; ---------------------------------------------------------------------------
;;;; Left part
;;;; ---------------------------------------------------------------------------

(defun ol-mode-line-left-part ()
  (quote ((:eval (ol-evil-segment))
          "  " (:eval (ol-buffer-name-segment))
          " " (:eval (ol-file-state-segment))
          " " "%l:%c"
          "" (:eval (ol-relative-position-segment)))))

(defun ol-evil-segment ()
  (let ((evil-face (cond ((evil-normal-state-p)   'ol-evil-normal-state-mode-line-face)
                         ((evil-insert-state-p)   'ol-evil-insert-state-mode-line-face)
                         ((evil-visual-state-p)   'ol-evil-visual-state-mode-line-face)
                         ((evil-emacs-state-p)    'ol-evil-emacs-state-mode-line-face)
                         ((evil-operator-state-p) 'ol-evil-operator-state-mode-line-face)
                         (t                       'ol-evil-normal-state-mode-line-face))))
    (propertize
     (concat
      (truncate-string-to-width (string-pad (upcase (symbol-name evil-state)) 9 32) 6))
     'face evil-face)))

(defun ol-buffer-name-segment ()
  (propertize "%b" 'face 'ol-buffer-name-mode-line-face))

(defun ol-file-state-segment ()
  (if buffer-read-only
      "%%%%"
    (if (buffer-modified-p) "*" "-")))

(defun ol-relative-position-segment ()
  (format "%4d%%%%%%%%" (/ (point) 0.01 (point-max))))
;; TODO the amount of % escaping above means there are too many layers

;;;; ---------------------------------------------------------------------------
;;;; Right part
;;;; ---------------------------------------------------------------------------

(defun ol-mode-line-right-part ()
  (quote ((:eval (ol-branch-name-segment))
          "  " ((:eval (ol-project-name-segment)))
          )))

;; TOOD: Handle when checked out commit or remote branch
(defun ol-branch-name-segment ()
  (if-let ((branch (magit-get-current-branch)))
      branch
    ""))

(defun ol-project-name-segment ()
  (let* ((name (projectile-project-name)))
     (if (string-equal name "-")
         ""
       name)))

;;;; ---------------------------------------------------------------------------
;;;; Putting it all together
;;;; ---------------------------------------------------------------------------

;; Modeline stuff copied (and then modified) from
;; https://www.reddit.com/r/emacs/comments/1333621/wrote_a_custom_modeline_with_some_help_from/
(defun ol-render-mode-line (left right)
  (let* ((left-formatted (format-mode-line left))
         (right-formatted (format-mode-line right))

         (total-width (- (window-total-width) 5))
         (available-width (- total-width (length left-formatted) 1))
         (align-format-string (format "%%s %%%ds " available-width))
         (formatted (format align-format-string left-formatted right-formatted))
         (truncated (truncate-string-to-width formatted total-width)))
    (concat "   " truncated "  ")))

(setq-default mode-line-format
              (quote ((:eval (ol-render-mode-line
                              (ol-mode-line-left-part)
                              (ol-mode-line-right-part))))))

;; Workaround to make sure also the messages buffer has the correct value
(with-current-buffer (get-buffer "*Messages*")
  (setq mode-line-format (default-value 'mode-line-format)))

;; -----------------------------------------------------------------------------
;; Dired
;; -----------------------------------------------------------------------------

(require 'dired)

(setc dired-kill-when-opening-new-dired-buffer t)
(ol-evil-define-key 'normal dired-mode-map "o" 'dired-find-file)
(ol-evil-define-key 'normal dired-mode-map "i" 'dired-up-directory)

;; Seems to be the only way override space
(evil-collection-define-key 'normal 'dired-mode-map " " nil)

(ol-define-normal-leader-key "dh" (lambda () (interactive) (dired "~")))


(defun ol-dired ()
  (interactive)
  (cond
   (tar-superior-buffer (switch-to-buffer tar-superior-buffer))
   (archive-superior-buffer (switch-to-buffer archive-superior-buffer))
   (t (dired default-directory))))

(ol-global-set-key "C-x d" 'ol-dired)

(setc dired-listing-switches "-Alh")
(setc dired-recursive-copies 'always)
(setc dired-recursive-deletes 'always)

(add-hook 'dired-after-readin-hook 'ol-dired-rename-hook)

;; TODO: Consider "dired: current file name<parent dirs>" instead
(defun ol-dired-rename-hook ()
  (let ((desired-name (ol-get-buffer-name-from-path "dired" dired-directory)))
    (unless (ol-buffer-name-matches (buffer-name) desired-name)
      (rename-buffer (generate-new-buffer-name desired-name)))))

(defun ol-get-buffer-name-from-path (prefix path)
  (concat prefix ": " (string-truncate-left
                       (string-replace (expand-file-name "~/") "~/" path)
                       40)))

(defun ol-buffer-name-matches (name desired-name)
  (let ((regexp (concat "^" (regexp-quote desired-name) "\\(<[0-9]>\\)?$")))
    (string-match-p regexp name)))

(cl-assert (ol-buffer-name-matches "some-name" "some-name"))
(cl-assert (ol-buffer-name-matches "some-name<2>" "some-name"))
(cl-assert (not (ol-buffer-name-matches "some-name-more" "some-name")))
(cl-assert (not (ol-buffer-name-matches "some-name" "some-name-more")))

;; -----------------------------------------------------------------------------
;; tar-mode
;; -----------------------------------------------------------------------------

(require 'tar-mode)

(defun ol-tar-up-directory ()
  (interactive)
  (if tar-superior-buffer
      (switch-to-buffer tar-superior-buffer)
    (ol-dired)))

;; Keybinds to mimic dired
(ol-evil-define-key 'normal tar-mode-map "o" 'tar-view)
(ol-evil-define-key 'normal tar-mode-map "i" 'ol-tar-up-directory)

;; -----------------------------------------------------------------------------
;; archive-mode
;;------------------------------------------------------------------------------

(require 'archive-mode)

(defun ol-archive-up-directory ()
  (interactive)
  (if archive-superior-buffer
      (switch-to-buffer archive-superior-buffer)
    (ol-dired)))

;; Keybinds to mimic dired
(ol-evil-define-key 'normal archive-mode-map "o" 'archive-extract)
(ol-evil-define-key 'normal archive-mode-map "i" 'ol-archive-up-directory)

(ol-evil-define-key 'normal archive-mode-map "C" 'archive-copy-file)
