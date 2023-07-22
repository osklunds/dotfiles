
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

;;;; Leader

(use-package general
  :after evil
  :config
  (general-create-definer ol-leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

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


;;;; Mac

(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'n)

;;;; Misc

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defun override-key (key fun)
  (general-define-key
   :states '(normal emacs)
   :keymaps 'override
   key fun))

(override-key "C-j" 'ivy-switch-buffer)
(override-key "M-q" 'projectile-find-file)
(override-key "M-h" 'help-command)

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
;;;; Basics
;;;; ---------------------------------------------------------------------------

(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(column-number-mode)
(global-hl-line-mode 1)

(set-face-attribute 'default nil :height 110)


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

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 2))

;; -----------------------------------------------------------------------------
;; Editing
;; -----------------------------------------------------------------------------

(setq-default tab-width 4)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))


;; Ivy and counsel

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
  (ivy-mode 1))

(setq ivy-height 20)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)))

(use-package ivy-rich
  :after (ivy counsel)
  :init
  (ivy-rich-mode 1))

;; Languages
;;;; All languages
;;;;;; LSP

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

;;;;;; Completion

(use-package company
  :after (lsp-mode org-mode)
  :hook
  (lsp-mode . company-mode)
  (org-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))


;;;;;; Snippets

(use-package yasnippet)

(use-package yasnippet-snippets
  :after yasnippet)

(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; (defun ol-rust-mode-hook ()
;;   (setq-local company-backends
;;               '((company-capf company-yasnippet :separate))
;;        ))

;; (add-hook 'rust-mode-hook #'ol-rust-mode-hook)

;;;;;; Misc

;;;; Language specific
;;;;;; Haskell

(use-package haskell-mode)

(add-hook 'haskell-mode-hook #'lsp)

;;;;;; Rust

(use-package rust-mode
  :hook (rust-mode . lsp))

                                        ; Copied from lsp-mode (I think), will be adjusted
(defun ol-lsp-rust-analyzer--make-init-options ()
  "Init options for rust-analyzer"
  `(:diagnostics (:enable ,(lsp-json-bool lsp-rust-analyzer-diagnostics-enable)
                          :enableExperimental ,(lsp-json-bool lsp-rust-analyzer-diagnostics-enable-experimental)
                          :disabled ,lsp-rust-analyzer-diagnostics-disabled
                          :warningsAsHint ,lsp-rust-analyzer-diagnostics-warnings-as-hint
                          :warningsAsInfo ,lsp-rust-analyzer-diagnostics-warnings-as-info)
                 :imports (:granularity (:enforce ,(lsp-json-bool lsp-rust-analyzer-import-enforce-granularity)
                                                  :group ,lsp-rust-analyzer-import-granularity)
                                        :group ,(lsp-json-bool lsp-rust-analyzer-import-group)
                                        :merge (:glob ,(lsp-json-bool lsp-rust-analyzer-imports-merge-glob))
                                        :prefix ,lsp-rust-analyzer-import-prefix)
                 :lruCapacity ,lsp-rust-analyzer-lru-capacity
                 :checkOnSave (:enable ,(lsp-json-bool lsp-rust-analyzer-cargo-watch-enable)
                                       :command ,lsp-rust-analyzer-cargo-watch-command
                                       :extraArgs ,lsp-rust-analyzer-cargo-watch-args
                                       :allTargets ,(lsp-json-bool lsp-rust-analyzer-check-all-targets)
                                       :features ,lsp-rust-analyzer-checkonsave-features
                                       :overrideCommand ,lsp-rust-analyzer-cargo-override-command)
                 :files (:exclude ,lsp-rust-analyzer-exclude-globs
                                  :watcher ,(if lsp-rust-analyzer-use-client-watching "client" "notify")
                                  :excludeDirs ,lsp-rust-analyzer-exclude-dirs)
                 :cargo (:allFeatures ,(lsp-json-bool lsp-rust-all-features)
                                      :noDefaultFeatures ,(lsp-json-bool lsp-rust-no-default-features)
                                      :features ,lsp-rust-features
                                      :target ,lsp-rust-analyzer-cargo-target
                                      :runBuildScripts ,(lsp-json-bool lsp-rust-analyzer-cargo-run-build-scripts)
                                        ; Obsolete, but used by old Rust-Analyzer versions
                                      :loadOutDirsFromCheck ,(lsp-json-bool lsp-rust-analyzer-cargo-run-build-scripts)
                                      :autoreload ,(lsp-json-bool lsp-rust-analyzer-cargo-auto-reload)
                                      :useRustcWrapperForBuildScripts ,(lsp-json-bool lsp-rust-analyzer-use-rustc-wrapper-for-build-scripts)
                                      :unsetTest ,lsp-rust-analyzer-cargo-unset-test)
                 :rustfmt (:extraArgs ,lsp-rust-analyzer-rustfmt-extra-args
                                      :overrideCommand ,lsp-rust-analyzer-rustfmt-override-command
                                      :rangeFormatting (:enable ,(lsp-json-bool lsp-rust-analyzer-rustfmt-rangeformatting-enable)))
                 :inlayHints (:bindingModeHints ,(lsp-json-bool lsp-rust-analyzer-binding-mode-hints)
                                                :chainingHints ,(lsp-json-bool lsp-rust-analyzer-display-chaining-hints)
                                                :closingBraceHints (:enable ,(lsp-json-bool lsp-rust-analyzer-closing-brace-hints)
                                                                            :minLines ,lsp-rust-analyzer-closing-brace-hints-min-lines)
                                                :closureReturnTypeHints ,(lsp-json-bool lsp-rust-analyzer-display-closure-return-type-hints)
                                                :lifetimeElisionHints (:enable ,lsp-rust-analyzer-display-lifetime-elision-hints-enable
                                                                               :useParameterNames ,(lsp-json-bool lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names))
                                                :maxLength ,lsp-rust-analyzer-max-inlay-hint-length
                                                :parameterHints ,(lsp-json-bool lsp-rust-analyzer-display-parameter-hints)
                                                :reborrowHints ,lsp-rust-analyzer-display-reborrow-hints
                                                :renderColons ,(lsp-json-bool lsp-rust-analyzer-server-format-inlay-hints)
                                                :typeHints (:enable ,(lsp-json-bool lsp-inlay-hint-enable)
                                                                    :hideClosureInitialization ,(lsp-json-bool lsp-rust-analyzer-hide-closure-initialization)
                                                                    :hideNamedConstructor ,(lsp-json-bool lsp-rust-analyzer-hide-named-constructor)))
                 :completion (:addCallParenthesis ,(lsp-json-bool lsp-rust-analyzer-completion-add-call-parenthesis)
                                                  :addCallArgumentSnippets ,(lsp-json-bool lsp-rust-analyzer-completion-add-call-argument-snippets)
                                                  :postfix (:enable ,(lsp-json-bool lsp-rust-analyzer-completion-postfix-enable))
                                                  :autoimport (:enable ,(lsp-json-bool lsp-rust-analyzer-completion-auto-import-enable))
                                                  :autoself (:enable ,(lsp-json-bool lsp-rust-analyzer-completion-auto-self-enable)))
                 :callInfo (:full ,(lsp-json-bool lsp-rust-analyzer-call-info-full))
                 :procMacro (:enable ,(lsp-json-bool lsp-rust-analyzer-proc-macro-enable))
                 :rustcSource ,lsp-rust-analyzer-rustc-source
                 :linkedProjects ,lsp-rust-analyzer-linked-projects
                 :highlighting (:strings ,(lsp-json-bool lsp-rust-analyzer-highlighting-strings))
                 :workspace (:symbol (:search (:kind ,"all_symbols")))
                 :experimental (:procAttrMacros ,(lsp-json-bool lsp-rust-analyzer-experimental-proc-attr-macros))))

(advice-add 'lsp-rust-analyzer--make-init-options :override
            (lambda () (ol-lsp-rust-analyzer--make-init-options)))

;; Theme and colors

(use-package doom-themes)
(load-theme 'doom-one-light t)

;;Helper for completely copying another face.

;; TODO: unset all properties (foreground etc...) the proper way
(defun ol-copy-face-attribute (face-to-set face-to-copy-from)
  (set-face-attribute face-to-set nil
                      :inherit face-to-copy-from
                      :foreground nil
                      :background nil))

;; Projectile

(use-package projectile
  :after counsel
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Programmering")
    (setq projectile-project-search-path '(("~/Programmering" . 2))))
  (setq counsel-projectile-switch-project-action 'counsel-projectile-switch-project-action-dired)
  )

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package projectile-ripgrep
  :after projectile)

(setq ivy-more-chars-alist '((t . 1)))

(ol-leader-keys
  "pp" 'projectile-switch-project
  "pd" 'projectile-discover-projects-in-search-path
  "pf" 'counsel-projectile-rg)

;; Magit and git
;;;; Key bindings

(ol-leader-keys
  "gs" 'magit-status
  "gcc" 'ol-commit-all-ask :which-key "commit all files ask for msg"
  "gcm" 'ol-commit-all-minor-fixes :which-key "commit all files no msg")

;; TODO: Probaly don't need a separate fun for without arg
(defun ol-commit-all-msg (msg)
  (shell-command (format "git add -A; git commit -m \"%s\"" msg)))

(defun ol-commit-all-minor-fixes ()
  (interactive)
  (ol-commit-all-msg "Minor fixes"))

(defun ol-commit-all-ask ()
  (interactive)
  (ol-commit-all-msg (read-string "Commit message: ")))

;;;; Magit
(use-package magit)

(set-face-attribute 'magit-blame-margin nil
                    :background "#e4e4e4")
;; TODO: Possibly change org mode background to the above as well.

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

(add-hook 'with-editor-mode-hook 'evil-insert-state)

;;    Idea: have one style with date and summary, and others styles with e.g. hash and committer

;;;; Misc

;; TODO: Use main first, if doesn't exist, use master
;; TODO: analyze if should use origin or not
;; TODO: Include summary of changes files and num lines
(defun ol-diff-main ()
  "Diff against the merge base with main/master"
  (interactive)
  (magit-diff-range "master..."))

;;    TODO: Small helper that reads HEAD buffer of current buffer, and then runs vdiff on that

;;;; Merge Surival Knife

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

;; Reminder: M-RET for new bullet

;; - Base-Local
;; - Base-Remote
;; - Local-Remote
;; - Local-Merged
;; - Remote-Merged

;; Org mode
;; Set faces for heading levels

(defun ol/org-font-setup ()
  ;; I don't actually change any font sizes, but I keep this in case I change my mind.
  (dolist (face '((org-level-1 . 1.0)
                  (org-level-2 . 1.0)
                  (org-level-3 . 1.0)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :weight 'regular :height (cdr face))))


(use-package org
  :config
  (setq org-ellipsis " ▾")
  (ol/org-font-setup))

(defun ol/org-mode-visual-fill ()
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; Idea: Center all buffers! Use 100 wide. Investigte how my vim, and emacs, line breaks

(use-package visual-fill-column
  :hook (org-mode . ol/org-mode-visual-fill))

(setq org-src-preserve-indentation t)
(setq org-edit-src-content-indentation 0)


(set-face-attribute 'org-block nil :background
                    (color-darken-name
                     (face-attribute 'default :background) 3))

(ol-leader-keys
  "os" 'org-babel-demarcate-block :which-key "split code block")

;; Terminal

(evil-define-key 'insert term-raw-map (kbd "C-h") #'evil-window-left)
(evil-define-key 'insert term-raw-map (kbd "C-l") #'evil-window-right)
(evil-define-key 'insert term-raw-map (kbd "C-j") 'ivy-switch-buffer)

;; Vdiff
;;;; General

(use-package vdiff)
(define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)

(setq vdiff-auto-refine t)
(setq vdiff-subtraction-fill-char ? )

(setq vdiff-fold-padding 10)

(defun ol-vdiff-fold-string (n-lines first-line-text width)
  (format "   %d lines\n" n-lines))

(setq vdiff-fold-string-function 'ol-vdiff-fold-string)

;;;; Colors
(ol-copy-face-attribute 'vdiff-addition-face 'magit-diff-added)
(ol-copy-face-attribute 'vdiff-refine-added 'magit-diff-added-highlight)
(ol-copy-face-attribute 'vdiff-change-face 'magit-diff-base)
(ol-copy-face-attribute 'vdiff-refine-changed 'magit-diff-base-highlight)
(ol-copy-face-attribute 'vdiff-subtraction-face 'magit-diff-removed)
(ol-copy-face-attribute 'vdiff-closed-fold-face 'magit-diff-hunk-heading-highlight)

;;;; Magit integration

(use-package vdiff-magit
  :after (vdiff magit))

(define-key magit-mode-map "e" 'vdiff-magit-dwim)
(define-key magit-mode-map "E" 'vdiff-magit)
(transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
(transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
(transient-suffix-put 'magit-dispatch "E" :description "vdiff")
(transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit)

;; Ediff
;;;; Misc

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
;; Copied from https://emacs.stackexchange.com/a/24602
(defun disable-y-or-n-p (orig-fun &rest args)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (apply orig-fun args)))

(advice-add 'ediff-quit :around #'disable-y-or-n-p)

;;;; Colors
;;;;;; Used colors

(require 'ediff)

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

;;;;;; Old ways for colors

;; ;; -----------------------------------------------------------------------------
;; (set-face-attribute 'ediff-current-diff-A nil
;;       :inherit 'magit-diff-removed)
;; (set-face-attribute 'ediff-current-diff-B nil
;;       :inherit 'magit-diff-added)
;; (set-face-attribute 'ediff-current-diff-Ancestor nil
;;       :inherit 'magit-diff-base)
;; ;; Red so that I notice when it happens
;; (set-face-attribute 'ediff-current-diff-C nil
;;       :background "#ff0000")

;; ;; -----------------------------------------------------------------------------
;; (set-face-attribute 'ediff-even-diff-A nil
;;       :background "#85ff21")
;; (set-face-attribute 'ediff-even-diff-B nil
;;       :background "#21ff72")
;; (set-face-attribute 'ediff-even-diff-Ancestor nil
;;       :background "#21ffbc")
;; ;; Red so that I notice when it happens
;; (set-face-attribute 'ediff-even-diff-C nil
;;       :background "#ff0000")
;; (set-face-attribute 'ediff-odd-diff-A nil
;;       :inherit 'ediff-even-diff-A)
;; (set-face-attribute 'ediff-odd-diff-B nil
;;       :inherit 'ediff-even-diff-B)
;; (set-face-attribute 'ediff-odd-diff-C nil
;;       :inherit 'ediff-even-diff-C)
;; (set-face-attribute 'ediff-odd-diff-Ancestor nil
;;       :inherit 'ediff-even-diff-Ancestor)

;; ;; -----------------------------------------------------------------------------
;; (set-face-attribute 'ediff-fine-diff-A nil
;;       :inherit 'magit-diff-removed-highlight
;;       :foreground nil
;;       :background nil)
;; (set-face-attribute 'ediff-fine-diff-B nil
;;       :inherit 'magit-diff-added-highlight)
;; (set-face-attribute 'ediff-fine-diff-Ancestor nil
;;       :inherit 'magit-diff-base-highlight)
;; ;; Red so that I notice when it happens
;; (set-face-attribute 'ediff-fine-diff-C nil
;;       :background "#ff0000")

;; (set-face-attribute 'ediff-current-diff-A nil
;;       :background "#ff3021")
;; (set-face-attribute 'ediff-current-diff-B nil
;;       :background "#ff8921")
;; (set-face-attribute 'ediff-current-diff-C nil
;;       :background "#ffc421")
;; (set-face-attribute 'ediff-current-diff-Ancestor nil
;;       :background "#cfff21")

;; (set-face-attribute 'ediff-even-diff-A nil
;;       :background "#85ff21")
;; (set-face-attribute 'ediff-even-diff-B nil
;;       :background "#21ff72")
;; (set-face-attribute 'ediff-even-diff-C nil
;;       :background "#21ffbc")
;; (set-face-attribute 'ediff-even-diff-Ancestor nil
;;       :background "#21fff4")

;; (set-face-attribute 'ediff-fine-diff-A nil
;;       :background "#ff3021")
;; (set-face-attribute 'ediff-fine-diff-B nil
;;       :background "#21bcff")
;; (set-face-attribute 'ediff-fine-diff-C nil
;;       :background "#2176ff")
;; (set-face-attribute 'ediff-fine-diff-Ancestor nil
;;       :background "#6b21ff")

;; (set-face-attribute 'ediff-odd-diff-A nil
;;       :background "#b921ff")
;; (set-face-attribute 'ediff-odd-diff-B nil
;;       :background "#f421ff")
;; (set-face-attribute 'ediff-odd-diff-C nil
;;       :background "#ff21b5")
;; (set-face-attribute 'ediff-odd-diff-Ancestor nil
;;       :background "#ff2181")

;; TODO Put in a better place. For some reason, these settings are overwritten
;; if put earlier in the file

;; Modeline
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  )

(doom-modeline-def-segment proj-name
  (concat
   (doom-modeline-spc)
   (doom-modeline-display-text (format "P: %s" (projectile-project-name)))))

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
  (set-face-attribute face nil
                      :weight 'bold))

;; Stuff that has to be in the end

                                        ; General TODO: Move things here to a better place when you know how to make it work the proper way.

(set-face-attribute 'mode-line nil
                    :background "#D7E4E8"
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :background "#E9EDED"
                                        ;:box '(:line-width 8 :color "#565063")
                    :overline nil
                    :underline nil)

;; Todos

;; - Programming languages
;;   - LSP search for functions
;;   - Haskell tags
;;   - Rust tags
;;   - Improve/streamline company or other completion
;;   - Maybe if possible: company mode only in source blocks
;; - evil
;;   - evil redo
;;   - Understand evil search, and make it less "flickering"
;;   - Make magit ? show bindings for evil mode
;;   - Exit visual mode when search replace is done
;; - projectile
;;   - projectile-find-file (want) vs counsel-projectile-find-file (seems to be used) are not the same
;;   - Use fd or rg --files instead of find
;;   - Show ripgrep results in a new buffer (C-c C-o)
;; - windows
;;   - window split function, so that always uses two windows, or same number as already shown. Or size all windows to same size.
;;   - always make windows same size
;; - lines around cursorline
;; - git diff short cuts
;;   - Current file to HEAD
;;   - Current file to baseline/master/main
;;   - Multiple files to HEAD
;;   - Multiples files to baseline/master/main
;;   - Multiple files to other commit
;; - Auto save
;; - ivy
;;   - In switch buffer, find file, ripgrep, split selection to a new window
;;   - in switch buffer, don't skip buffer visible in other window
;;   - Go between prev commands in shell/terminal with arrow keys
;; - Org mode
;;   - Images
;;   - Charts
;;   - References to other sections
;; - Native compiled emacs
;; - Emacs in a sandbox/without networking
;; - load-file-path instead of use-package
