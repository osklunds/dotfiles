
;; -----------------------------------------------------------------------------
;; General
;; -----------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; Helpers
;;;; ---------------------------------------------------------------------------

(defmacro setc (var val)
  `(customize-set-variable ',var ,val))

;;;; ---------------------------------------------------------------------------
;;;; File Management
;;;; ---------------------------------------------------------------------------

;; No ~ files
(setq make-backup-files nil)

(ol-require 'super-save)

(setq super-save-auto-save-when-idle t)
(super-save-mode t)

;; Copied from https://emacs.stackexchange.com/a/30032
(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

(defun ol-silent-save-buffer ()
  (interactive)
  (with-suppressed-message (save-buffer)))

;; Copied from super-save and modified t use ol-silent-save-buffer
(defun ol-super-save-command ()
  (when (super-save-p) (ol-silent-save-buffer)))

(advice-add 'super-save-command :override 'ol-super-save-command)

;;;; ---------------------------------------------------------------------------
;;;; Misc
;;;; ---------------------------------------------------------------------------

(setq gc-cons-threshold 50000000)

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

;; -----------------------------------------------------------------------------
;; Key bindings
;; -----------------------------------------------------------------------------

;; Key bindings are placed early, because every section uses them.

;;;; ---------------------------------------------------------------------------
;;;; Evil
;;;; ---------------------------------------------------------------------------

(setq evil-search-module 'evil-search)
;; TODO find out why needed 
(setq evil-want-integration t)
;; Use evil-collection instead for other packages
(setq evil-want-keybinding nil)
;; Use C-u for scroll instead of universal argument  
(setq evil-want-C-u-scroll t)

(ol-require 'evil)
(evil-mode t)

(evil-set-initial-state 'messages-buffer-mode 'normal)

(setq evil-insert-state-cursor 'box)

(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (setq-default evil-symbol-word-search t))

(ol-require 'evil-collection)
(evil-collection-init)
(evil-set-undo-system 'undo-redo)

;;;; ---------------------------------------------------------------------------
;;;; Leader
;;;; ---------------------------------------------------------------------------

(ol-require 'general)

;;;; ---------------------------------------------------------------------------
;;;; Mac
;;;; ---------------------------------------------------------------------------

(defun ol-is-mac ()
  (string= system-type "darwin"))

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

(global-hl-line-mode)
(make-variable-buffer-local 'global-hl-line-mode)

;;;; ---------------------------------------------------------------------------
;;;; Windows and buffers
;;;; ---------------------------------------------------------------------------

(ol-require 'balanced-windows)

(balanced-windows-mode)

(defun ol-split-window-sensibly (&optional window)
    (interactive)
    (let ((window (or window (selected-window))))
        (and (= 1 (count-windows))
                 (with-selected-window window
                     (split-window-right)))))

(setq split-window-preferred-function #'ol-split-window-sensibly)

;;;; ---------------------------------------------------------------------------
;;;; Misc
;;;; ---------------------------------------------------------------------------

(ol-require 'rainbow-delimiters)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'text-mode-hook 'rainbow-delimiters-mode)

;; -----------------------------------------------------------------------------
;; Text editing
;; -----------------------------------------------------------------------------

(setq-default tab-width 4)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)

(setq-default fill-column 80)

(ol-require 'evil-nerd-commenter)

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

(ol-require 'evil-visualstar)

(global-evil-visualstar-mode)

;;;; ---------------------------------------------------------------------------
;;;; Which Key
;;;; ---------------------------------------------------------------------------

(ol-require 'which-key)
(which-key-mode)
(setq which-key-idle-delay 2)

;; -----------------------------------------------------------------------------
;; Ivy and counsel
;; -----------------------------------------------------------------------------

(ol-require 'ivy)
(setq ivy-height 20)
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

(ol-require 'counsel)

(ol-require 'ivy-rich)
(ivy-rich-mode t)

;; -----------------------------------------------------------------------------
;; Languages
;; -----------------------------------------------------------------------------

;;;;----------------------------------------------------------------------------
;;;; All languages
;;;; ---------------------------------------------------------------------------

;;;;;; -------------------------------------------------------------------------
;;;;;; LSP
;;;;;; -------------------------------------------------------------------------

(ol-require 'lsp-mode)

(setq lsp-completion-provider :none)
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
(setq flycheck-indication-mode nil)

(setq lsp-log-io t)
;; TODO: Disable lsp diagnostics. Can use above log to inspect
;; TODO: Get functions from ivy-lsp

(ol-require 'lsp-ivy)

;;;;;; -------------------------------------------------------------------------
;;;;;; Abbreviations (for completions)
;;;;;; -------------------------------------------------------------------------

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

(setq save-abbrevs 'silently)

(define-abbrev-table 'global-abbrev-table
  '(
    ("qwerty" "test-abbreviation cursor before@@after")
   ))

;;;;;; -------------------------------------------------------------------------
;;;;;; Completion
;;;;;; -------------------------------------------------------------------------

(ol-require 'company)

;; TODO: Set more things using customize instead of setq
(customize-set-variable 'company-backends '((company-abbrev
                                             :separate
                                             company-capf
                                             :separate
                                             company-dabbrev-code))) ;; TODO make sure - is included for lisp
(customize-set-variable 'company-minimum-prefix-length 1)
(customize-set-variable 'company-idle-delay 0.0)
(customize-set-variable 'company-selection-wrap-around t)
(setq company-tooltip-align-annotations t)
(setq company-dabbrev-minimum-length 2)
(setq company-dabbrev-other-buffers nil)

(add-hook 'prog-mode-hook 'company-mode)

(ol-require 'company-box)

(add-hook 'company-mode-hook 'company-box-mode)

;;;; ---------------------------------------------------------------------------
;;;; Language specific
;;;; ---------------------------------------------------------------------------

;;;;;; -------------------------------------------------------------------------
;;;;;; Haskell
;;;;;; -------------------------------------------------------------------------

(ol-require 'haskell-mode)

(add-hook 'haskell-mode-hook 'lsp)

;;;;;; -------------------------------------------------------------------------
;;;;;; Rust
;;;;;; -------------------------------------------------------------------------

(ol-require 'rust-mode)

(add-hook 'rust-mode-hook 'lsp)

(defun ol-lsp-rust-analyzer--make-init-options (original)
  (let ((extra `(:workspace (:symbol (:search (:kind ,"all_symbols"))))))
    (append original extra)))

(advice-add 'lsp-rust-analyzer--make-init-options :filter-return #'ol-lsp-rust-analyzer--make-init-options)

(define-abbrev-table 'rust-mode-abbrev-table
  '(
    ("asdfg" "mode-spec-abbreviation@@test")
   ))

;; -----------------------------------------------------------------------------
;; Theme and colors
;; -----------------------------------------------------------------------------

(ol-require 'doom-themes)
(load-theme 'doom-one-light t)

;;Helper for completely copying another face.

;; TODO: unset all properties (foreground etc...) the proper way
(defun ol-copy-face-attribute (face-to-set face-to-copy-from)
  (set-face-attribute face-to-set nil
                      :inherit face-to-copy-from
                      :foreground nil
                      :background nil))

(if (ol-is-mac)
    (set-face-attribute 'default nil :height 110)
  (set-face-attribute 'default nil :height 90))

(set-face-attribute 'default nil :foreground "#000000" :background "#ffffff")

(set-face-attribute 'font-lock-comment-face nil :foreground "#50a14f")
(set-face-attribute 'font-lock-string-face nil :foreground "#d78700")

;; -----------------------------------------------------------------------------
;; Projectile
;; -----------------------------------------------------------------------------

(if (ol-is-mac)
    (setq projectile-project-search-path '(("~/Documents" ) ("~/Programmering" . 2)))
  (setq projectile-project-search-path '(("~/own_repos" . 1)
                                         ("~/others_repos" . 1)
                                         ("~/Dropbox/documents"))))

(setq projectile-enable-caching t)

(ol-require 'projectile)

(customize-set-variable 'projectile-completion-system 'ivy)

(setq projectile-switch-project-action 'projectile-commander)

(ol-require 'counsel-projectile)

(ol-require 'projectile-ripgrep)

(setq ivy-more-chars-alist '((t . 1)))

(call-interactively 'projectile-mode)

;; -----------------------------------------------------------------------------
;; Git
;; -----------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; Magit
;;;; ---------------------------------------------------------------------------

(ol-require 'magit)
(ol-require 'magit-blame)

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

(setq magit-diff-paint-whitespace nil)

;;;; ---------------------------------------------------------------------------
;;;; Diffs
;;;; ---------------------------------------------------------------------------

(defun ol-diff-main ()
  (interactive)
  (magit-diff-range (ol-main-branch)))

(defun ol-diff-head ()
  (interactive)
  (magit-diff-range "HEAD"))

(defun ol-include-stat (&rest r)
  (add-to-list 'magit-buffer-diff-args "--stat"))

(advice-add 'magit-insert-revision-diff :before 'ol-include-stat)
(advice-add 'magit-insert-diff :before 'ol-include-stat)

;;;; ---------------------------------------------------------------------------
;;;; Merge Survival Knife
;;;; ---------------------------------------------------------------------------

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

(ol-require 'org)
(ol-require 'org-faces)

(setq org-ellipsis " ▾")

(setq org-src-preserve-indentation t)
(setq org-edit-src-content-indentation 0)

(set-face-attribute 'org-block nil :background
                    (color-darken-name
                     (face-attribute 'default :background) 3))

(add-to-list 'auto-mode-alist '("\\.org.txt\\'" . org-mode))

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

;; TODO> Do this:
;; https://emacs.stackexchange.com/questions/28825/how-do-you-set-colors-for-term
(ol-copy-face-attribute 'term-color-black 'default)

;; -----------------------------------------------------------------------------
;; Vdiff
;; -----------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; General
;;;; ---------------------------------------------------------------------------

(ol-require 'vdiff)

(setq vdiff-auto-refine t)
(setq vdiff-subtraction-fill-char ? )

(setc vdiff-fold-padding 10)

(defun ol-vdiff-fold-string (n-lines first-line-text width)
  (format "   %d lines\n" n-lines))

(setq vdiff-fold-string-function 'ol-vdiff-fold-string)

(defun ol-vdiff-2way-layout-function (buffer-a buffer-b &optional rotate)
  (delete-other-windows)
  (switch-to-buffer buffer-a)
  (set-window-buffer
   (if rotate
       (split-window-vertically)
     (split-window-horizontally))
   buffer-b)
  (other-window 1))

(setc vdiff-2way-layout-function 'vdiff-2way-layout-function-default)

(advice-add 'vdiff-buffers :after (lambda (&rest r)
                                    (other-window 1)))
                                             

;;;; ---------------------------------------------------------------------------
;;;; Colors
;;;; ---------------------------------------------------------------------------

;; TODO: Only copy background
(ol-copy-face-attribute 'vdiff-addition-face 'magit-diff-added)
(ol-copy-face-attribute 'vdiff-refine-added 'magit-diff-added-highlight)
(ol-copy-face-attribute 'vdiff-change-face 'magit-diff-base)
(ol-copy-face-attribute 'vdiff-refine-changed 'magit-diff-base-highlight)
(ol-copy-face-attribute 'vdiff-subtraction-face 'magit-diff-removed)
(ol-copy-face-attribute 'vdiff-closed-fold-face 'magit-diff-hunk-heading-highlight)

;;;; ---------------------------------------------------------------------------
;;;; Magit integration
;;;; ---------------------------------------------------------------------------

(ol-require 'vdiff-magit)

(customize-set-variable 'vdiff-magit-dwim-show-on-hunks t)

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

(defun ol-does-branch-exist (branch)
  (equal (magit-rev-branch branch) branch))


(defun ol-main-branch ()
  (let ((main-branch "main"))
    (if (ol-does-branch-exist main-branch)
        main-branch
      "master")))

(defun ol-diff-file-main ()
  (interactive)
  (let* ((file (magit-current-file))
         (rev-main (ol-main-branch))
         (rev-main (magit-commit-p (magit-git-string "merge-base" "HEAD" rev-main)))
         (file-main (magit--rev-file-name file "HEAD" rev-main))
         (buffer-main (msk--get-revision-buffer rev-main file-main)))
    (ol-diff-buffers buffer-main (current-buffer))))

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

(ol-require 'doom-modeline)
(doom-modeline-mode t)

(doom-modeline-def-segment proj-name
  (concat
   (doom-modeline-spc)
   (doom-modeline-display-text (format "%s" (projectile-project-name)))
   (doom-modeline-spc)))

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
(setq doom-modeline-highlight-modified-buffer-name nil)

(dolist (face '(doom-modeline-evil-normal-state
                doom-modeline-evil-insert-state
                doom-modeline-evil-visual-state
                doom-modeline-evil-emacs-state))
  (set-face-attribute face nil :weight 'bold))

;; -----------------------------------------------------------------------------
;; Dired
;; -----------------------------------------------------------------------------

(setq dired-kill-when-opening-new-dired-buffer t)

(evil-define-key 'normal dired-mode-map (kbd "o") 'dired-find-file)
(evil-define-key 'normal dired-mode-map (kbd "i") 'dired-up-directory)

(when (ol-is-mac)       
  (setq dired-use-ls-dired nil))

(defun ol-dired ()
  (interactive)
  (dired default-directory))

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
;; Helpers
;; -----------------------------------------------------------------------------

;; Taken from https://emacs.stackexchange.com/a/24658
(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))
