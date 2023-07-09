
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

(setq visible-bell       nil
      ring-bell-function #'ignore)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'n)

;; Initialize package sources
(require 'package)

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

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(use-package ivy
  :bind (("C-x C-b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(column-number-mode)
(global-display-line-numbers-mode t)
(global-hl-line-mode 1)

(use-package counsel)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history)))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  )

(doom-modeline-def-segment proj-name
  "Hej hej"
  (concat
   (doom-modeline-spc)
   (doom-modeline-display-text (format "P: %s" (projectile-project-name)))))

;; Define your custom doom-modeline
(doom-modeline-def-modeline 'ol-simple-line
  '(bar buffer-info buffer-position)
  '(major-mode vcs proj-name))

(doom-modeline-set-modeline 'ol-simple-line t)

;; Set default mode-line
;(add-hook 'doom-modeline-mode-hook
;          (lambda ()
;            (doom-modeline-set-modeline 'ol-simple-line 'default)))

;; Or disable other mode-lines
;(setq 'doom-modeline-mode-alist nil)

(setq doom-modeline-icon nil)
(setq doom-modeline-buffer-encoding nil)
(setq doom-modeline-lsp nil)
(setq doom-modeline-env-version nil)
(setq doom-modeline-minor-modes nil)
(setq doom-modeline-lsp nil)

(setq make-backup-files nil) ; stop creating ~ files

(use-package haskell-mode)

(add-hook 'haskell-mode-hook #'lsp)

(use-package rust-mode
  :hook (rust-mode . lsp))

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

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-ivy)

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

;; (use-package evil)

(use-package doom-themes)
(load-theme 'doom-one-light t)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 2))

(use-package projectile
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Programmering")
    (setq projectile-project-search-path '(("~/Programmering" . 2))))
  (setq projectile-switch-project-action 'projectile-dired)
  )

;; TODO: Map counsel-projectile-rg to something
;; This command is interactive
;; C-x C-o to open results in a buffer


(use-package projectile-ripgrep)

;(use-package counsel-projectile
;  :config (counsel-projectile-mode))

(use-package magit)

  ;; Set faces for heading levels

(defun ol/org-font-setup ()
(dolist (face '((org-level-1 . 1.5)
                  (org-level-2 . 1.4)
                  (org-level-3 . 1.3)
                  (org-level-4 . 1.2)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :weight 'regular :height (cdr face))))

(use-package org
  :config
  (setq org-ellipsis " ▾")
;  (ol/org-font-setup)
  )

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("●" "●" "●" "●" "●" "●" "●")))

(defun ol/org-mode-visual-fill ()
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t)
(visual-fill-column-mode 1))

; Idea: Center all buffers! Use 100 wide. Investigte how my vim, and emacs, line breaks

(use-package visual-fill-column
  :hook (org-mode . ol/org-mode-visual-fill))

(use-package evil-nerd-commenter)

(use-package yasnippet)
(use-package yasnippet-snippets)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; (defun ol-rust-mode-hook ()
;;   (setq-local company-backends
;;               '((company-capf company-yasnippet :separate))
;; 	      ))

;; (add-hook 'rust-mode-hook #'ol-rust-mode-hook)

(require 'ediff)

;; These actually made some more sense once I understood them. In ediff, there's a "current"
;; diff, and "other" diffs. The currently selected diff is highlighted using these
;; "current" faces below. The non-selected other diffs are highlighted alternatingly
;;with the odd and even faces.

;; TODO: unset all properties (foreground etc...) the proper way
(defun ol-set-ediff-face-attribute (ediff-face face-to-inherit)
  (set-face-attribute ediff-face nil
		      :inherit face-to-inherit
		      :foreground nil
		      :background nil))

(ol-set-ediff-face-attribute 'ediff-current-diff-A        'magit-diff-removed)
(ol-set-ediff-face-attribute 'ediff-current-diff-B        'magit-diff-added)
(ol-set-ediff-face-attribute 'ediff-current-diff-Ancestor 'magit-diff-base)

(ol-set-ediff-face-attribute 'ediff-fine-diff-A        'magit-diff-removed-highlight)
(ol-set-ediff-face-attribute 'ediff-fine-diff-B        'magit-diff-added-highlight)
(ol-set-ediff-face-attribute 'ediff-fine-diff-Ancestor 'magit-diff-base-highlight)

(ol-set-ediff-face-attribute 'ediff-even-diff-A        'magit-diff-removed)
(ol-set-ediff-face-attribute 'ediff-even-diff-B        'magit-diff-added)
(ol-set-ediff-face-attribute 'ediff-even-diff-Ancestor 'magit-diff-base)

(ol-set-ediff-face-attribute 'ediff-odd-diff-A        'magit-diff-removed)
(ol-set-ediff-face-attribute 'ediff-odd-diff-B        'magit-diff-added)
(ol-set-ediff-face-attribute 'ediff-odd-diff-Ancestor 'magit-diff-base)


;; ;; -----------------------------------------------------------------------------
;; (set-face-attribute 'ediff-current-diff-A nil
;; 		    :inherit 'magit-diff-removed)
;; (set-face-attribute 'ediff-current-diff-B nil
;; 		    :inherit 'magit-diff-added)
;; (set-face-attribute 'ediff-current-diff-Ancestor nil
;; 		    :inherit 'magit-diff-base)
;; ;; Red so that I notice when it happens
;; (set-face-attribute 'ediff-current-diff-C nil
;; 		    :background "#ff0000")

;; ;; -----------------------------------------------------------------------------
;; (set-face-attribute 'ediff-even-diff-A nil
;; 		    :background "#85ff21")
;; (set-face-attribute 'ediff-even-diff-B nil
;; 		    :background "#21ff72")
;; (set-face-attribute 'ediff-even-diff-Ancestor nil
;; 		    :background "#21ffbc")
;; ;; Red so that I notice when it happens
;; (set-face-attribute 'ediff-even-diff-C nil
;; 		    :background "#ff0000")
;; (set-face-attribute 'ediff-odd-diff-A nil
;; 		    :inherit 'ediff-even-diff-A)
;; (set-face-attribute 'ediff-odd-diff-B nil
;; 		    :inherit 'ediff-even-diff-B)
;; (set-face-attribute 'ediff-odd-diff-C nil
;; 		    :inherit 'ediff-even-diff-C)
;; (set-face-attribute 'ediff-odd-diff-Ancestor nil
;; 		    :inherit 'ediff-even-diff-Ancestor)

;; ;; -----------------------------------------------------------------------------
;; (set-face-attribute 'ediff-fine-diff-A nil
;; 		    :inherit 'magit-diff-removed-highlight
;; 		    :foreground nil
;; 		    :background nil)
;; (set-face-attribute 'ediff-fine-diff-B nil
;; 		    :inherit 'magit-diff-added-highlight)
;; (set-face-attribute 'ediff-fine-diff-Ancestor nil
;; 		    :inherit 'magit-diff-base-highlight)
;; ;; Red so that I notice when it happens
;; (set-face-attribute 'ediff-fine-diff-C nil
;; 		    :background "#ff0000")

;; (set-face-attribute 'ediff-current-diff-A nil
;; 		    :background "#ff3021")
;; (set-face-attribute 'ediff-current-diff-B nil
;; 		    :background "#ff8921")
;; (set-face-attribute 'ediff-current-diff-C nil
;; 		    :background "#ffc421")
;; (set-face-attribute 'ediff-current-diff-Ancestor nil
;; 		    :background "#cfff21")

;; (set-face-attribute 'ediff-even-diff-A nil
;; 		    :background "#85ff21")
;; (set-face-attribute 'ediff-even-diff-B nil
;; 		    :background "#21ff72")
;; (set-face-attribute 'ediff-even-diff-C nil
;; 		    :background "#21ffbc")
;; (set-face-attribute 'ediff-even-diff-Ancestor nil
;; 		    :background "#21fff4")

;; (set-face-attribute 'ediff-fine-diff-A nil
;; 		    :background "#ff3021")
;; (set-face-attribute 'ediff-fine-diff-B nil
;; 		    :background "#21bcff")
;; (set-face-attribute 'ediff-fine-diff-C nil
;; 		    :background "#2176ff")
;; (set-face-attribute 'ediff-fine-diff-Ancestor nil
;; 		    :background "#6b21ff")

;; (set-face-attribute 'ediff-odd-diff-A nil
;; 		    :background "#b921ff")
;; (set-face-attribute 'ediff-odd-diff-B nil
;; 		    :background "#f421ff")
;; (set-face-attribute 'ediff-odd-diff-C nil
;; 		    :background "#ff21b5")
;; (set-face-attribute 'ediff-odd-diff-Ancestor nil
;; 		    :background "#ff2181")

;; TODO Put in a better place. For some reason, these settings are overwritten
;; if put earlier in the file

(set-face-attribute 'mode-line nil
                    :background "#bfbfbf"
                    :overline nil
                    :underline nil)

  (set-face-attribute 'mode-line-inactive nil
                    :background "#e8e8e8"
                    ;:box '(:line-width 8 :color "#565063")
                    :overline nil
                    :underline nil)

(setq custom-file (concat user-emacs-directory "/custom.el"))
