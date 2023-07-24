
;; -----------------------------------------------------------------------------
;; General
;; -----------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; Config Management
;;;; ---------------------------------------------------------------------------

(setq debug-on-error t)

(setq custom-file (concat user-emacs-directory "/custom.el"))

(setq vc-follow-symlinks t)

;;;; ---------------------------------------------------------------------------
;;;; Package Management
;;;; ---------------------------------------------------------------------------

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Initialize package sources
(require 'package)

;;;; ---------------------------------------------------------------------------
;;;; Misc
;;;; ---------------------------------------------------------------------------

;; No ~ files
(setq make-backup-files nil)

(setq gc-cons-threshold 50000000)

;; -----------------------------------------------------------------------------
;; Key bindings
;; -----------------------------------------------------------------------------

;; Key bindings are placed early, because every section uses them.

;;;; ---------------------------------------------------------------------------
;;;; Evil
;;;; ---------------------------------------------------------------------------

;; :init is run before the package is loaded, and that's needed for some packages and options.
;;    :config is run after it has been loaded.

(use-package evil
  :init
(setq evil-search-module 'evil-search)
  ;; TODO find out why needed 
  (setq evil-want-integration t)
  ;; Use evil-collection instead for other packages
  (setq evil-want-keybinding nil)
  ;; Use C-u for scroll instead of universal argument  
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Window movement
  (define-key evil-normal-state-map (kbd "C-h") #'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-l") #'evil-window-right)

  (defun ol-no-op ()
    (interactive))

  ;; No arrow keys
  (define-key evil-normal-state-map (kbd "<left>") 'ol-no-op)
  (define-key evil-normal-state-map (kbd "<right>") 'ol-no-op)
  (define-key evil-normal-state-map (kbd "<down>") 'ol-no-op)
  (define-key evil-normal-state-map (kbd "<up>") 'ol-no-op)

  (define-key evil-insert-state-map (kbd "<left>") 'ol-no-op)
  (define-key evil-insert-state-map (kbd "<right>") 'ol-no-op)
  (define-key evil-insert-state-map (kbd "<down>") 'ol-no-op)
  (define-key evil-insert-state-map (kbd "<up>") 'ol-no-op)

  (evil-global-set-key 'motion (kbd "<left>") 'ol-no-op)
  (evil-global-set-key 'motion (kbd "<right>") 'ol-no-op)
  (evil-global-set-key 'motion (kbd "<down>") 'ol-no-op)
  (evil-global-set-key 'motion (kbd "<up>") 'ol-no-op)

  (define-key evil-normal-state-map (kbd "?") 'evil-ex-nohighlight)

  (evil-set-initial-state 'messages-buffer-mode 'normal)

  (setq evil-insert-state-cursor 'box))

(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (setq-default evil-symbol-word-search t))

(use-package evil-collection
  ;; :after means load this after evil has been loaded
  :after evil
  :config
  (evil-collection-init))

(evil-set-undo-system 'undo-redo)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;;; ---------------------------------------------------------------------------
;;;; Leader
;;;; ---------------------------------------------------------------------------

(use-package general
  :after evil
  :config
  (general-create-definer ol-leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

;;;; ---------------------------------------------------------------------------
;;;; Overriding keys
;;;; ---------------------------------------------------------------------------

(defun override-key (key fun)
  (general-define-key
   :states '(normal emacs)
   :keymaps 'override
   key fun))

(override-key "M-h" 'help-command)

;;;; ---------------------------------------------------------------------------
;;;; Mac
;;;; ---------------------------------------------------------------------------

(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'n)

;; -----------------------------------------------------------------------------
;; User Interface
;; -----------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; Reduce Clutter
;;;; ---------------------------------------------------------------------------

(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

(setq visible-bell nil
      ring-bell-function #'ignore)

;;;; ---------------------------------------------------------------------------
;;;; Line and column numbers
;;;; ---------------------------------------------------------------------------

(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(column-number-mode)
(global-hl-line-mode 1)

;;;; ---------------------------------------------------------------------------
;;;; Windows and buffers
;;;; ---------------------------------------------------------------------------

;; Things here need to be improved and investigated

(use-package balanced-windows
  :config
  (balanced-windows-mode))

;; Vertical splits
(setq split-width-threshold 100)
(setq split-height-threshold nil)

;; (add-to-list 'display-buffer-alist '("" (display-buffer-reuse-window
;;          display-buffer-same-window)))

(defun ol-split-window ()
  (interactive)
  (split-window-right)
  (evil-window-right 1))

(override-key "M-w" 'ol-split-window)
(override-key "M-e" 'delete-window)

;;;; ---------------------------------------------------------------------------
;;;; Misc
;;;; ---------------------------------------------------------------------------

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; -----------------------------------------------------------------------------
;; Editing
;; -----------------------------------------------------------------------------

(setq-default tab-width 4)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;;;; ---------------------------------------------------------------------------
;;;; Find and replace
;;;; ---------------------------------------------------------------------------

(defconst full-range "%")
(defconst from-here-range ",$")

(defun ol-full-replace-visual-selection ()
  (interactive)
  (ol-replace-visual-selection full-range))

(defun ol-from-here-replace-visual-selection ()
  (interactive)
  (ol-replace-visual-selection from-here-range))

(defun ol-full-replace-symbol ()
  (interactive)
  (ol-replace-symbol full-range))

(defun ol-from-here-replace-symbol ()
  (interactive)
  (ol-replace-symbol from-here-range))

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

(ol-leader-keys
  :keymaps 'visual
  "R" '(ol-full-replace-visual-selection :which-key "replace full visual selection")
  "r" '(ol-from-here-replace-visual-selection :which-key "replace from here visual selection"))

(ol-leader-keys
  :keymaps 'normal
  "R" '(ol-full-replace-symbol :which-key "replace full symbol")
  "r" '(ol-from-here-replace-symbol :which-key "replace from here symbol"))

(use-package evil-visualstar)

(global-evil-visualstar-mode)

;;;; ---------------------------------------------------------------------------
;;;; Which Key
;;;; ---------------------------------------------------------------------------

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 2))

;; -----------------------------------------------------------------------------
;; Ivy and counsel
;; -----------------------------------------------------------------------------

(use-package ivy
  :bind (("C-x C-b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (setq ivy-height 20)
  (ivy-mode 1))

(use-package ivy-rich
  :after (ivy counsel)
  :init
  (ivy-rich-mode 1))

(override-key "C-j" 'ivy-switch-buffer)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)))

;; -----------------------------------------------------------------------------
;; Languages
;; -----------------------------------------------------------------------------

;;;;----------------------------------------------------------------------------
;;;; All languages
;;;; ---------------------------------------------------------------------------

;;;;;; -------------------------------------------------------------------------
;;;;;; LSP
;;;;;; -------------------------------------------------------------------------

(use-package lsp-mode
  :config
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-diagnostics-provider :none)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-modeline-workspace-status-enable nil)
  (setq lsp-lens-enable nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-signature-auto-activate nil)
  )

(setq flycheck-indication-mode nil)

(setq lsp-log-io t)
;; TODO: Disable lsp diagnostics. Can use above log to inspect
;; TODO: Get functions from ivy-lsp

(use-package lsp-ivy
  :after lsp-mode)

(ol-leader-keys
  "ff" 'lsp-ivy-workspace-symbol)

;;;;;; -------------------------------------------------------------------------
;;;;;; Completion
;;;;;; -------------------------------------------------------------------------

(use-package company
  :after lsp-mode
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(add-hook 'prog-mode-hook 'company-mode)

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

;;;;;; -------------------------------------------------------------------------
;;;;;; Snippets
;;;;;; -------------------------------------------------------------------------

(use-package yasnippet)

(use-package yasnippet-snippets
  :after yasnippet)

(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;;;; ---------------------------------------------------------------------------
;;;; Language specific
;;;; ---------------------------------------------------------------------------

;;;;;; -------------------------------------------------------------------------
;;;;;; Haskell
;;;;;; -------------------------------------------------------------------------

(use-package haskell-mode)

(add-hook 'haskell-mode-hook #'lsp)

;;;;;; -------------------------------------------------------------------------
;;;;;; Rust
;;;;;; -------------------------------------------------------------------------

(use-package rust-mode
  :hook (rust-mode . lsp))

(defun ol-lsp-rust-analyzer--make-init-options (original)
  (let ((extra `(:workspace (:symbol (:search (:kind ,"all_symbols"))))))
    (append original extra)))

(advice-add 'lsp-rust-analyzer--make-init-options :filter-return #'ol-lsp-rust-analyzer--make-init-options)

;; -----------------------------------------------------------------------------
;; Theme and colors
;; -----------------------------------------------------------------------------

(use-package doom-themes)
(load-theme 'doom-one-light t)

;;Helper for completely copying another face.

;; TODO: unset all properties (foreground etc...) the proper way
(defun ol-copy-face-attribute (face-to-set face-to-copy-from)
  (set-face-attribute face-to-set nil
                      :inherit face-to-copy-from
                      :foreground nil
                      :background nil))

(set-face-attribute 'default nil :height 110)

;; -----------------------------------------------------------------------------
;; Projectile
;; -----------------------------------------------------------------------------

(use-package projectile
  :after counsel
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-enable-caching t)
  :init
  (when (file-directory-p "~/Programmering")
    (setq projectile-project-search-path '(("~/Programmering" . 2)))))

(use-package counsel-projectile
  :after (projectile counsel))

(use-package projectile-ripgrep
  :after projectile)

(setq ivy-more-chars-alist '((t . 1)))

(override-key "M-q" 'projectile-find-file)

(call-interactively 'projectile-mode)

(ol-leader-keys
  "pp" 'projectile-switch-project
  "pd" 'projectile-discover-projects-in-search-path
  "pf" 'counsel-projectile-rg)

;; -----------------------------------------------------------------------------
;; Git
;; -----------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; Magit
;;;; ---------------------------------------------------------------------------

(use-package magit)

(ol-leader-keys
  "gs" 'magit-status)

(set-face-attribute 'magit-blame-margin nil
                    :background "#e4e4e4")

;; TODO it only works to cycle once, and even that cycling seems broken.
;; Maybe add more styles, for example the same but longer width.
(setq magit-blame-styles
      '(
        (margin
         (margin-format . ("%C %s%f"))
         (margin-width  . 60)
         )
        )
      )

;; Start in insert state when doing commits in magit
(add-hook 'with-editor-mode-hook 'evil-insert-state)

;;;; ---------------------------------------------------------------------------
;;;; Diffs
;;;; ---------------------------------------------------------------------------

;; TODO: Use main first, if doesn't exist, use master
;; TODO: analyze if should use origin or not
;; TODO: Include summary of changes files and num lines
(defun ol-diff-main ()
  "Diff against the merge base with main/master"
  (interactive)
  (magit-diff-range "master..."))

;;;; ---------------------------------------------------------------------------
;;;; Merge Survival Knife
;;;; ---------------------------------------------------------------------------

(global-set-key (kbd "C-c 6") 'msk-merge-survival-knife-start)
(global-set-key (kbd "C-c 7") 'msk-merge-survival-knife-stop)

;; TODO Only bind if merging
(global-set-key (kbd "C-c 1") 'msk-base-local)
(global-set-key (kbd "C-c 2") 'msk-base-remote)
(global-set-key (kbd "C-c 3") 'msk-local-remote)
(global-set-key (kbd "C-c 4") 'msk-local-merged)
(global-set-key (kbd "C-c 5") 'msk-remote-merged)

(defvar msk-state ())

;; Copied and modified from magit.
(defun msk-merge-survival-knife-start ()
  (interactive)
  ;; TODO: Check if () first
  (setq msk-state ())
  (msk--put-value 'window-configuration (current-window-configuration))
  (let;; ((file (magit-current-file))
      (dir (magit-gitdir))
    (rev-local  (or (magit-name-branch "HEAD")
                    (magit-commit-p "HEAD")))
    (rev-remote  (cl-find-if (lambda (head)
                               (file-exists-p (expand-file-name head dir)))
                             '("MERGE_HEAD" "CHERRY_PICK_HEAD" "REVERT_HEAD")))
    (rev-remote  (or (magit-name-branch rev-remote)
                     (magit-commit-p rev-remote)))
    (rev-base  (magit-commit-p (magit-git-string "merge-base" rev-local rev-remote)))
    (file-local (magit--rev-file-name file rev-local rev-remote))
    (file-remote (magit--rev-file-name file rev-remote rev-local))
    (file-base (or (magit--rev-file-name file rev-base rev-local)
                   (magit--rev-file-name file rev-base rev-remote)))

    (buffer-local  (msk--get-revision-buffer rev-local  file-local))
    (buffer-remote (msk--get-revision-buffer rev-remote file-remote))
    (buffer-base   (msk--get-revision-buffer rev-base   file-base))
    (buffer-merged (current-buffer))

    (buffer-base-local (msk--ediff buffer-base buffer-local "BASE LOCAL"))
    (buffer-base-remote (msk--ediff buffer-base buffer-remote "BASE REMOTE"))
    (buffer-local-remote (msk--ediff buffer-local buffer-remote "LOCAL REMOTE"))
    (buffer-local-merged (msk--ediff buffer-local buffer-merged "LOCAL MERGED"))
    (buffer-remote-merged (msk--ediff buffer-remote buffer-merged "REMOTE MERGED")))
  
  (msk--put-value 'base buffer-base)
  (msk--put-value 'local buffer-local)
  (msk--put-value 'remote buffer-remote)
  
  (msk--put-value 'base-local buffer-base-local)
  (msk--put-value 'base-remote buffer-base-remote)
  (msk--put-value 'local-remote buffer-local-remote)
  (msk--put-value 'local-merged buffer-local-merged)
  (msk--put-value 'remote-merged buffer-remote-merged)
  )

(defun msk-merge-survival-knife-stop ()
  (interactive)
  ;; TOOD: Iterate instead
  (kill-buffer (msk--get-value 'base))
  (kill-buffer (msk--get-value 'local))
  (kill-buffer (msk--get-value 'remote))

  (kill-buffer (msk--get-value 'base-local))
  (kill-buffer (msk--get-value 'base-remote))
  (kill-buffer (msk--get-value 'local-remote))
  (kill-buffer (msk--get-value 'local-merged))
  (kill-buffer (msk--get-value 'remote-merged))

  (set-window-configuration (msk--get-value 'window-configuration))

  (setq msk-state ()))

(defun msk-base-local ()
  (interactive)
  (msk--compare-buffer-pair 'base-local))

(defun msk-base-remote ()
  (interactive)
  (msk--compare-buffer-pair 'base-remote))

(defun msk-local-remote ()
  (interactive)
  (msk--compare-buffer-pair 'local-remote))

(defun msk-local-merged ()
  (interactive)
  (msk--compare-buffer-pair 'local-merged))

(defun msk-remote-merged ()
  (interactive)
  (msk--compare-buffer-pair 'remote-merged))

(defun msk--compare-buffer-pair (ediff-control-buffer)
  (switch-to-buffer (msk--get-value ediff-control-buffer))
  (delete-other-windows)
  (ediff-recenter))

(defun msk--get-revision-buffer (rev file)
  (magit-get-revision-buffer rev file (magit-find-file-noselect rev file)))

(defun msk--ediff (bufferLeft bufferRight name)
  (let;; ((bufferName (format ";;Ediff %s;;" name))
      (rename-control-panel (lambda() (rename-buffer bufferName)))
    (not-dedicated-window (lambda() (set-window-dedicated-p (frame-selected-window) nil)))
    (startup-hooks (cons rename-control-panel (cons not-dedicated-window ())))
    )
  
  (ediff-buffers bufferLeft bufferRight startup-hooks))

(defun msk--put-value (key value)
  (setq msk-state (plist-put msk-state key value)))

(defun msk--get-value (key)
  (plist-get msk-state key))


;; Local
;; /  |  \
;; /   |   \
;; /    |    \
;; Base     |     Merged
;; \    |    / 
;; \   |   /
;; \  |  /
;; Remote

;; - Base-Local
;; - Base-Remote
;; - Local-Remote
;; - Local-Merged
;; - Remote-Merged

;; -----------------------------------------------------------------------------
;; Org mode
;; -----------------------------------------------------------------------------

(use-package org
  :config
  (setq org-ellipsis " ▾"))

(defun ol-org-mode-visual-fill ()
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; Idea: Center all buffers! Use 100 wide. Investigte how my vim, and emacs, line breaks
;; Or use /2 of available width if one buffer

(use-package visual-fill-column
  :hook (org-mode . ol-org-mode-visual-fill))

(setq org-src-preserve-indentation t)
(setq org-edit-src-content-indentation 0)

(set-face-attribute 'org-block nil :background
                    (color-darken-name
                     (face-attribute 'default :background) 3))

(ol-leader-keys
  "os" 'org-babel-demarcate-block :which-key "split code block")

;; -----------------------------------------------------------------------------
;; Terminal
;; -----------------------------------------------------------------------------

(evil-define-key 'insert term-raw-map (kbd "C-h") #'evil-window-left)
(evil-define-key 'insert term-raw-map (kbd "C-l") #'evil-window-right)
(evil-define-key 'insert term-raw-map (kbd "C-j") 'ivy-switch-buffer)

;; -----------------------------------------------------------------------------
;; Vdiff
;; -----------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; General
;;;; ---------------------------------------------------------------------------

(use-package vdiff)
(define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)

(setq vdiff-auto-refine t)
(setq vdiff-subtraction-fill-char ? )

(setq vdiff-fold-padding 10)

(defun ol-vdiff-fold-string (n-lines first-line-text width)
  (format "   %d lines\n" n-lines))

(setq vdiff-fold-string-function 'ol-vdiff-fold-string)

;;;; ---------------------------------------------------------------------------
;;;; Colors
;;;; ---------------------------------------------------------------------------

(ol-copy-face-attribute 'vdiff-addition-face 'magit-diff-added)
(ol-copy-face-attribute 'vdiff-refine-added 'magit-diff-added-highlight)
(ol-copy-face-attribute 'vdiff-change-face 'magit-diff-base)
(ol-copy-face-attribute 'vdiff-refine-changed 'magit-diff-base-highlight)
(ol-copy-face-attribute 'vdiff-subtraction-face 'magit-diff-removed)
(ol-copy-face-attribute 'vdiff-closed-fold-face 'magit-diff-hunk-heading-highlight)

;;;; ---------------------------------------------------------------------------
;;;; Magit integration
;;;; ---------------------------------------------------------------------------

(use-package vdiff-magit
  :after (vdiff magit))

(define-key magit-mode-map "e" 'vdiff-magit-dwim)
(define-key magit-mode-map "E" 'vdiff-magit)
(transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
(transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
(transient-suffix-put 'magit-dispatch "E" :description "vdiff")
(transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit)

;;;; ---------------------------------------------------------------------------
;;;; Magit diffing
;;;; ---------------------------------------------------------------------------

(defun ol-diff-on-quit (buffer-a buffer-b)
  (kill-buffer buffer-a))

(defun ol-diff-buffers (buffer-a buffer-b)
  (vdiff-buffers buffer-a buffer-b nil 'ol-diff-on-quit t nil))

(defun ol-diff-file-head ()
  (interactive)
  (let* ((file (magit-current-file))
         (rev-head "HEAD")
         (buffer-head (msk--get-revision-buffer rev-head file)))
    (ol-diff-buffers buffer-head (current-buffer))))

(defun ol-diff-file-main ()
  (interactive)
  (let* ((file (magit-current-file))
         (rev-main (ol-main-branch))
         (rev-main (magit-commit-p (magit-git-string "merge-base" "HEAD" rev-main)))
         (file-main (magit--rev-file-name file "HEAD" rev-main))
         (buffer-main (msk--get-revision-buffer rev-main file-main)))
    (ol-diff-buffers buffer-main (current-buffer))))

(defun ol-main-branch ()
  (let ((main-branch "main"))
    (if (ol-does-branch-exist main-branch)
        main-branch
      "master")))

(defun ol-does-branch-exist (branch)
  (equal (magit-rev-branch branch) branch))

;; -----------------------------------------------------------------------------
;; Ediff
;; -----------------------------------------------------------------------------

(require 'ediff)

;;;; ---------------------------------------------------------------------------
;;;; Misc
;;;; ---------------------------------------------------------------------------

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
;; Copied from https://emacs.stackexchange.com/a/24602
(defun disable-y-or-n-p (orig-fun &rest args)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (apply orig-fun args)))

(advice-add 'ediff-quit :around #'disable-y-or-n-p)

;;;; ---------------------------------------------------------------------------
;;;; Colors
;;;; ---------------------------------------------------------------------------

;; These actually made some more sense once I understood them. In ediff, there's a "current"
;; diff, and "other" diffs. The currently selected diff is highlighted using these
;; "current" faces below. The non-selected other diffs are highlighted alternatingly
;;with the odd and even faces.

(ol-copy-face-attribute 'ediff-current-diff-A        'magit-diff-removed)
(ol-copy-face-attribute 'ediff-current-diff-B        'magit-diff-added)
(ol-copy-face-attribute 'ediff-current-diff-C        'magit-diff-added)
(ol-copy-face-attribute 'ediff-current-diff-Ancestor 'magit-diff-base)

(ol-copy-face-attribute 'ediff-fine-diff-A        'magit-diff-removed-highlight)
(ol-copy-face-attribute 'ediff-fine-diff-B        'magit-diff-added-highlight)
(ol-copy-face-attribute 'ediff-fine-diff-C        'magit-diff-added-highlight)
(ol-copy-face-attribute 'ediff-fine-diff-Ancestor 'magit-diff-base-highlight)

(ol-copy-face-attribute 'ediff-even-diff-A        'magit-diff-removed)
(ol-copy-face-attribute 'ediff-even-diff-B        'magit-diff-added)
(ol-copy-face-attribute 'ediff-even-diff-C        'magit-diff-added)
(ol-copy-face-attribute 'ediff-even-diff-Ancestor 'magit-diff-base)

(ol-copy-face-attribute 'ediff-odd-diff-A        'magit-diff-removed)
(ol-copy-face-attribute 'ediff-odd-diff-B        'magit-diff-added)
(ol-copy-face-attribute 'ediff-odd-diff-C        'magit-diff-added)
(ol-copy-face-attribute 'ediff-odd-diff-Ancestor 'magit-diff-base)

;; -----------------------------------------------------------------------------
;; Modeline
;; -----------------------------------------------------------------------------

(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

(doom-modeline-def-segment proj-name
  (concat
   (doom-modeline-spc)
   (doom-modeline-display-text (format "%s" (projectile-project-name)))))

(doom-modeline-def-modeline 'ol-simple-line
  '(bar modals buffer-info buffer-position)
  '(major-mode vcs proj-name))

(doom-modeline-set-modeline 'ol-simple-line t)

(setq doom-modeline-icon nil)
(setq doom-modeline-buffer-encoding nil)
(setq doom-modeline-lsp nil)
(setq doom-modeline-env-version nil)
(setq doom-modeline-minor-modes nil)
(setq doom-modeline-lsp nil)

(dolist (face '(doom-modeline-evil-normal-state
                doom-modeline-evil-insert-state
                doom-modeline-evil-visual-state
                doom-modeline-evil-emacs-state))
  (set-face-attribute face nil :weight 'bold))

;; -----------------------------------------------------------------------------
;; Stuff that has to be in the end
;; -----------------------------------------------------------------------------

(set-face-attribute 'mode-line nil
                    :background "#D7E4E8"
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :background "#E9EDED"
                    :overline nil
                    :underline nil)

;; -----------------------------------------------------------------------------
;; Todos
;; -----------------------------------------------------------------------------

;; Must haves
(when nil "
- windows
  - window split function, so that always uses two windows, or same number as already shown. Or size all windows to same size.
- git diff short cuts
  - Current file to HEAD
  - Current file to baseline/master/main
  - Multiple files to HEAD
  - Multiples files to baseline/master/main
  - Multiple files to other commit
- Auto save
- ivy
  - in switch buffer, don't skip buffer visible in other window
- Go between prev commands in shell/terminal with arrow keys
- Native compiled emacs
- Emacs in a sandbox/without networking
- load-file-path instead of use-package
")

;; Nice to haves
(when nil "
- Switch to vertico/consult/marginella
  - Will fix LSP search for functions
  - Will fix live preview of ripgrep results
- Programming languages
  - LSP search for functions
  - Haskell tags
  - Rust tags
  - Improve/streamline company or other completion
  - Maybe if possible: company mode only in source blocks
- evil
  - Make magit ? show bindings for evil mode
  - Exit visual mode when search replace is done
- ivy
  - In switch buffer, find file, ripgrep, split selection to a new window
- Org mode
  - Images
  - Charts
  - References to other sections
- Use built-in modeline
- lines around cursorline
- Merge Survival Knife

")
