
;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

;; Taken from https://emacs.stackexchange.com/a/24658
(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(defun ol-regexp-group (regexp string group)
  "Search STRING for REGEXP and return the match GROUP."
  (when (string-match regexp string)
    (match-string group string)))

(defmacro setc (var val)
  "Convenient version of customize-set-variable."
  `(customize-set-variable ',var ,val))

;; -----------------------------------------------------------------------------
;; General
;; -----------------------------------------------------------------------------

(setc enable-local-variables nil)

(defun ol-startup-hook ()
  (setq inhibit-trace nil)
  (toggle-frame-maximized))

(add-hook 'emacs-startup-hook 'ol-startup-hook)

;;;; ---------------------------------------------------------------------------
;;;; File Management
;;;; ---------------------------------------------------------------------------

;;;;;; -------------------------------------------------------------------------
;;;;;; Backup
;;;;;; -------------------------------------------------------------------------

;; No ~ files
(setc make-backup-files nil)

;; To prevent stutter when auto-saving. I use super-save and git to compensate
(setc auto-save-default nil)

;;;;;; -------------------------------------------------------------------------
;;;;;; Save
;;;;;; -------------------------------------------------------------------------

(require 'super-save)
(super-save-mode t)
(setq save-silently t)

(defun ol-save-buffer ()
  (interactive)
  (save-buffer)
  (message (format "Saved buffer: %s" (buffer-file-name))))

(save-place-mode t)

;;;;;; -------------------------------------------------------------------------
;;;;;; Auto revert
;;;;;; -------------------------------------------------------------------------

;; Disable auto revert while I experiment with performance
(global-auto-revert-mode nil)
(setc global-auto-revert-non-file-buffers t)
(setc auto-revert-verbose nil)

;;;; ---------------------------------------------------------------------------
;;;; Performance
;;;; ---------------------------------------------------------------------------

;; TODO: Since I disabled auto save, consider having a 30 second idle timer for
;; garbage collect. The manpage mentioned that garbage collection might happen
;; at the same time.

(setq gc-cons-threshold (* 10 800 1000)) ;; 10x the default
(setq gc-cons-percentage 0.4) ;; default is 0.1

(setc garbage-collection-messages t)

(setq read-process-output-max (* 1024 1024)) ;; 1 MB

;; Supposedly can improve scroll performance
(setq auto-window-vscroll nil)

;; -----------------------------------------------------------------------------
;; User Interface
;; -----------------------------------------------------------------------------

(fset 'yes-or-no-p 'y-or-n-p) ;; change all prompts to y or n

(setq-default show-trailing-whitespace nil)

(defun ol-toggle-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace))
  (message "Toggled show trailing. Now: %s" show-trailing-whitespace))

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

;;;; ---------------------------------------------------------------------------
;;;; Line and column numbers
;;;; ---------------------------------------------------------------------------

(global-display-line-numbers-mode t)
(setc display-line-numbers-type 'relative)
(setc display-line-numbers-grow-only t)
(setc display-line-numbers-width-start 10000)

(global-hl-line-mode)
(make-variable-buffer-local 'global-hl-line-mode)

(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;;;; ---------------------------------------------------------------------------
;;;; Windows, buffers, frames
;;;; ---------------------------------------------------------------------------

;;;;;; -------------------------------------------------------------------------
;;;;;; Balanced windows
;;;;;; -------------------------------------------------------------------------

(require 'balanced-windows)

(balanced-windows-mode)

;;;;;; -------------------------------------------------------------------------
;;;;;; Only two windows
;;;;;; -------------------------------------------------------------------------

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

;;;;;; -------------------------------------------------------------------------
;;;;;; Transposing
;;;;;; -------------------------------------------------------------------------

(defun ol-transpose-windows ()
  (interactive)
  (if (not (equal (length (window-list)) 2))
      (message "Can't transpose if not exactly two windows")
    (ol-toggle-split-style)
    (let* ((this (selected-window))
           (other (next-window this))
           (left-top-selected (if (or (window-in-direction 'left) (window-in-direction 'above))
                                  nil
                                t)))
      (delete-window other)
      (ol-split-window-sensibly)
      (when left-top-selected
        (other-window 1))
      (switch-to-buffer (other-buffer))
      (other-window 1))))

;;;;;; -------------------------------------------------------------------------
;;;;;; Frame size and position
;;;;;; -------------------------------------------------------------------------

(defun ol-set-frame-size ()
  (set-frame-height (selected-frame) 54)
  (set-frame-width (selected-frame) 240))

(defun ol-center-frame ()
  (modify-frame-parameters (selected-frame)
                           '((user-position . t) (top . 0.5) (left . 0.5))))

(defun ol-center-and-size-frame ()
  (interactive)
  (ol-set-frame-size)
  (ol-center-frame))

;;;;;; -------------------------------------------------------------------------
;;;;;; Splitting
;;;;;; -------------------------------------------------------------------------

(defun ol-split-window ()
  (interactive)
  (let ((current-point (point))
        (current-window-start (window-start)))
    (switch-to-buffer-other-window (current-buffer))
    (set-window-point (selected-window) current-point)
    (set-window-start (selected-window) current-window-start)))

(defun ol-force-split-window ()
  (interactive)
  (split-window-right)
  (evil-window-right 1))

;;;; ---------------------------------------------------------------------------
;;;; Misc
;;;; ---------------------------------------------------------------------------

(require 'rainbow-delimiters)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(global-visual-line-mode t)

;; -----------------------------------------------------------------------------
;; Key bindings/Evil
;; -----------------------------------------------------------------------------

;; These must be set before evil is loaded
(setq evil-want-integration t)
(setq evil-want-keybinding nil) ;; Use evil-collection instead for other packages
(setq evil-respect-visual-line-mode t)

(require 'evil)
(evil-mode t)

(setc evil-want-C-u-scroll t)
(setc evil-search-module 'evil-search)
(setc evil-disable-insert-state-bindings t)
(setc evil-emacs-state-modes nil)

(setq evil-insert-state-cursor 'box)

(add-hook 'emacs-lisp-mode-hook
          (lambda () (modify-syntax-entry ?- "w")))

(add-hook 'after-change-major-mode-hook
          (lambda () (modify-syntax-entry ?_ "w")))

(require 'evil-collection)

(with-eval-after-load 'dired (evil-collection-dired-setup))
(with-eval-after-load 'magit (evil-collection-magit-setup))
(with-eval-after-load 'term (evil-collection-term-setup))
(with-eval-after-load 'ivy (evil-collection-ivy-setup))

(evil-set-undo-system 'undo-redo)
(setc evil-want-Y-yank-to-eol t)

;; -----------------------------------------------------------------------------
;; Text editing
;; -----------------------------------------------------------------------------

(setq-default tab-width 4)
(setq-default evil-shift-width 4)

(setq-default indent-tabs-mode nil)

(setq-default fill-column 80)

(setq-default visual-line-mode t)

(require 'evil-nerd-commenter)

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

(require 'evil-visualstar)

(global-evil-visualstar-mode)

;; -----------------------------------------------------------------------------
;; Ivy and counsel
;; -----------------------------------------------------------------------------

(require 'ivy)
(setc ivy-height 20)
(ivy-mode t)

;; TODO: Maybe this can be solved by advising ivy-read instead. If
;; caller is ivy-switch-buffer, then change the preselect argument.
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

(require 'counsel)

(ivy-configure 'counsel-M-x
  :initial-input ""
  :display-transformer-fn #'counsel-M-x-transformer)

;;;; -------------------------------------------------------------------------
;;;; Find file name
;;;; -------------------------------------------------------------------------

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
  (let ((find-program (ol-find-file-name-command)))
    (counsel-require-program find-program)
    (let ((default-directory directory))
      (ivy-read (concat "Find file [" prompt "]: ")
                (counsel--find-return-list counsel-file-jump-args)
                :action #'find-file
                :preselect (counsel--preselect-file)
                :require-match 'confirm-after-completion
                :history 'file-name-history
                :keymap counsel-file-jump-map
                :caller 'ol-find-file-name))))

(defun ol-find-file-name-command ()
  ;; TODO DOn't have ol-find as a script, defne "rg --files || true" inline instead
  (let* ((preffered "ol-find"))
    (if (executable-find preffered (file-remote-p default-directory))
        preffered
      "find")))

;;;; -------------------------------------------------------------------------
;;;; Find file content
;;;; -------------------------------------------------------------------------

(setc counsel-rg-base-command "\
rg \
--max-columns 240 \
--with-filename \
--no-heading \
--line-number \
--color never \
%s || true")

(defun ol-dwim-find-file-content ()
  "Search for file content."
  (interactive)
  (if-let ((root (ol-dwim-use-project-root)))
      (ol-project-find-file-content root)
    (ol-cwd-find-file-content)))

(defun ol-project-find-file-content (&optional root)
  "Search for file content in the current project."
  (interactive)
  (ol-find-file-content (or root (projectile-project-root)) "project"))

(defun ol-cwd-find-file-content ()
  "Search for file content in the current directory."
  (interactive)
  (ol-find-file-content default-directory "cwd"))

(defun ol-find-file-content (directory prompt)
  (counsel-rg "" directory "" (concat "Find file content [" prompt "]: ")))

;; -----------------------------------------------------------------------------
;; Languages
;; -----------------------------------------------------------------------------

;;;; -------------------------------------------------------------------------
;;;; LSP
;;;; -------------------------------------------------------------------------

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
(setc lsp-response-timeout 4)
(setc lsp-enable-file-watchers nil) ;; to prevent "nested too deep" warning

;;;; -------------------------------------------------------------------------
;;;; Abbreviations (for completions)
;;;; -------------------------------------------------------------------------

;; Copied from https://stackoverflow.com/a/15389612
(defadvice expand-abbrev (after my-expand-abbrev activate)
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

(define-abbrev-table 'global-abbrev-table
  '(
    ("qwerty" "test-abbreviation cursor before@@after")
   ))

;;;; -------------------------------------------------------------------------
;;;; Completion
;;;; -------------------------------------------------------------------------

(require 'company)
(require 'company-box)

(setc company-backends '((company-abbrev
                          :separate
                          company-capf
                          :separate
                          company-dabbrev-code)))

(setc company-minimum-prefix-length 1)
(setc company-idle-delay 0.0)
(setc company-selection-wrap-around t)
(setc company-tooltip-align-annotations t)

(setc company-dabbrev-minimum-length 2)
(setc company-dabbrev-other-buffers nil)
(setc company-dabbrev-code-other-buffers nil)
(setc company-dabbrev-code-everywhere t)

(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'company-mode-hook 'company-box-mode)

(add-hook 'evil-insert-state-exit-hook 'company-abort)

;;;; ---------------------------------------------------------------------------
;;;; Emacs Lisp
;;;; ---------------------------------------------------------------------------

(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  '(
    ("dbg" "(message \"oskar: %s\" @@)")
    ))

;; -----------------------------------------------------------------------------
;; Theme
;; -----------------------------------------------------------------------------

(require 'doom-themes)

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

;; -----------------------------------------------------------------------------
;; Git and Magit
;; -----------------------------------------------------------------------------

(require 'magit)
(require 'magit-blame)

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

(setc magit-commit-show-diff t)

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

(defun ol-magit-full-status ()
  (interactive)
  (setc magit-status-sections-hook (append ol-magit-status-simple-sections ol-magit-status-full-sections))
  (magit-status))

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
    (setq-local ol-original-mode original-mode)
    ;; (message (format "Saved mode is: %s, current mode is: %s" ol-original-mode major-mode))
             ))

;;;; ---------------------------------------------------------------------------
;;;; Diff
;;;; ---------------------------------------------------------------------------

(setc magit-diff-paint-whitespace nil)

(defun ol-include-stat (&rest r)
  (add-to-list 'magit-buffer-diff-args "--stat"))

(advice-add 'magit-insert-revision-diff :before 'ol-include-stat)
(advice-add 'magit-insert-diff :before 'ol-include-stat)

;;;;;; ---------------------------------------------------------------------------
;;;;;; Diffing all files
;;;;;; ---------------------------------------------------------------------------

(defun ol-diff-all-files-main ()
  (interactive)
  (magit-diff-range (ol-merge-base-with-main)))

(defun ol-diff-all-files-head ()
  (interactive)
  (magit-diff-range "HEAD"))

;;;;;; ---------------------------------------------------------------------------
;;;;;; Diffing the current file
;;;;;; ---------------------------------------------------------------------------

(defun ol-diff-current-file-main ()
  (interactive)
  (ol-diff-current-file (ol-merge-base-with-main)))

(defun ol-diff-current-file-head ()
  (interactive)
  (ol-diff-current-file "HEAD"))

(defun ol-diff-current-file (rev-other)
  (let* ((file (magit-current-file))
         (file-other (magit--rev-file-name file "HEAD" rev-other))
         (buffer-other (ol-get-revision-buffer rev-other file-other)))
    (vdiff-buffers buffer-other (current-buffer))))

(defun ol-get-revision-buffer (rev file)
  (magit-get-revision-buffer rev file (magit-find-file-noselect rev file)))

;;;;;; ---------------------------------------------------------------------------
;;;;;; Helpers
;;;;;; ---------------------------------------------------------------------------

(defun ol-main-branch ()
  (let ((main-branch "main"))
    (if (ol-does-branch-exist main-branch)
        main-branch
      "master")))

(defun ol-does-branch-exist (branch)
  (equal (magit-rev-branch branch) branch))

(defun ol-merge-base-with-main ()
  (ol-merge-base (ol-main-branch) "HEAD"))

(defun ol-merge-base (rev-a rev-b)
  (magit-commit-p (magit-git-string "merge-base" rev-a rev-b)))

;;;; ---------------------------------------------------------------------------
;;;; Log
;;;; ---------------------------------------------------------------------------

;; TODO: Maybe these can be saved better with transient?
(defconst ol-magit-log-default-arguments '("-n256"))

(put 'magit-log-mode 'magit-log-default-arguments ol-magit-log-default-arguments)
(put 'magit-log-select-mode 'magit-log-default-arguments ol-magit-log-default-arguments)

(setc magit-log-margin '(t "%Y-%m-%d  %H:%M  " magit-log-margin-width nil 0))

;; TODO: git "rev" which is git log but only current file

;; TODO: More button isn't shown

(defun ol-git-log-dwim ()
  (interactive)
  (let* ((branch (magit-get-current-branch))
         (main (ol-main-branch))
         (ignore-rev (unless (equal branch main) main)))
    (ol-git-log branch ignore-rev)))

(defun ol-git-log-current (&optional ignore-rev)
  (interactive)
  (ol-git-log (magit-get-current-branch) ignore-rev))

(defun ol-git-log-other (&optional ignore-rev)
  (interactive)
  (ol-git-log (car (magit-log-read-revs)) ignore-rev))

(defun ol-git-log (rev &optional ignore-rev)
  (interactive)
  (let ((args (append (list rev)
                      ol-magit-log-default-arguments
                      (ol-make-ignore-rev-args ignore-rev))))
    (magit-log-other args nil)))

(defun ol-make-ignore-rev-args (ignore-rev)
  (when ignore-rev
    (list "--first-parent" "--not" ignore-rev)))

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

;; To make sure smerge doesn't add refinements to conflicts
(setc diff-refine nil)

;; Copied/inspired from
;; https://stumbles.id.au/auto-starting-emacs-smerge-mode-for-git.html
(defun vc-git-find-file-hook ()
  (when (save-excursion
      (goto-char (point-min))
      (re-search-forward "^<<<<<<< " nil t))
    (smerge-mode)))

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

;;;; ---------------------------------------------------------------------------
;;;; Spelling
;;;; ---------------------------------------------------------------------------

(defun ol-toggle-spelling ()
  (interactive)
  (unless flyspell-mode
    (flyspell-buffer))
  (call-interactively 'flyspell-mode))

;; -----------------------------------------------------------------------------
;; Terminal
;; -----------------------------------------------------------------------------

(require 'term)

(defun ol-disable-cursorline-for-terms ()
  (if (equal major-mode 'term-mode)
      (setq global-hl-line-mode nil)
    nil))

(defun ol-enable-cursorline-for-terms ()
  (if (equal major-mode 'term-mode)
      (setq global-hl-line-mode t)
    nil))

(add-hook 'evil-insert-state-entry-hook 'ol-disable-cursorline-for-terms)
(add-hook 'evil-insert-state-exit-hook 'ol-enable-cursorline-for-terms)

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

(defun ol-name-to-term-buffer-name (name)
  (concat "*" name "*"))

(setq kill-buffer-query-functions nil)
(setc confirm-kill-processes nil)

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

(defun ol-set-term-buffer-maximum-size ()
  (setc term-buffer-maximum-size 10000000000))

(add-hook 'term-mode-hook 'ol-set-term-buffer-maximum-size)

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

;;;; ---------------------------------------------------------------------------
;;;; emacs server
;;;; ---------------------------------------------------------------------------
         
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
  (server-done))

;; -----------------------------------------------------------------------------
;; Vdiff
;; -----------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; General
;;;; ---------------------------------------------------------------------------

(require 'vdiff)
(require 'vdiff-magit)

(setc vdiff-subtraction-fill-char ? )

(setc vdiff-diff-algorithm 'diff)

(setc vdiff-fold-padding 10)

(defun ol-vdiff-fold-string (n-lines first-line-text width)
  (format "   %d lines\n" n-lines))

(setc vdiff-fold-string-function 'ol-vdiff-fold-string)

(setc vdiff-magit-stage-is-2way t)

;;;; ---------------------------------------------------------------------------
;;;; Synced scroll
;;;; ---------------------------------------------------------------------------

(defun ol-vdiff-fix-scroll ()
  (interactive)
  (vdiff--scroll-function))

;; TODO: Calling fix scroll automatically as part of vdiff next hunk doesn't
;; work.

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

;; -----------------------------------------------------------------------------
;; Ediff
;; -----------------------------------------------------------------------------

(require 'ediff)

;;;; ---------------------------------------------------------------------------
;;;; Misc
;;;; ---------------------------------------------------------------------------

(setc ediff-window-setup-function 'ediff-setup-windows-plain)
(setc ediff-split-window-function 'split-window-horizontally)
;; Copied from https://emacs.stackexchange.com/a/24602
(defun disable-y-or-n-p (orig-fun &rest args)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (apply orig-fun args)))

(advice-add 'ediff-quit :around #'disable-y-or-n-p)


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

;;;; ---------------------------------------------------------------------------
;;;; Left part
;;;; ---------------------------------------------------------------------------

(defun ol-mode-line-left-part ()
  (quote (" " (:eval (ol-evil-segment))
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
          "  " ((:eval (ol-project-name-segment))))))

(defun ol-branch-name-segment ()
  (if-let ((file (or (buffer-file-name) default-directory)))
      (if-let ((branch-name vc-mode))
          (substring-no-properties branch-name 5)
        "")
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

         (available-width (- (window-width) (length left-formatted) 1))
         (align-format-string (format "%%s %%%ds " available-width)))
    (format align-format-string left-formatted right-formatted)))

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

(defun ol-dired ()
  (interactive)
  (dired default-directory))

(setc dired-listing-switches "-Alh")
(setc dired-recursive-copies 'always)
(setc dired-recursive-deletes 'always)

;; -----------------------------------------------------------------------------
;; tar
;; -----------------------------------------------------------------------------

(require 'tar-mode)

(defun ol-tar-up-directory ()
  (interactive)
  (if tar-superior-buffer
      (switch-to-buffer tar-superior-buffer)
    (message "No parent tar found")))

;; TODO Need to find a way to get the original name
;; (defun ol-tar-path ()
;;   (interactive)
;;   (ol-tar-path-inner tar-superior-buffer ""))

;; (defun ol-tar-path-inner (superior-buffer acc-path)
;;   (if superior-buffer
;;       (let* ((new-acc-path (
