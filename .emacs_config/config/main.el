
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

(require 'super-save)

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

(setq gc-cons-threshold (eval-when-compile (* 1024 1024 1024))) ;; 1 GB

(run-with-idle-timer 2 t (lambda () (garbage-collect)))

(setq read-process-output-max (* 1024 1024)) ;; 1 MB

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

(setq enable-local-variables nil)

;; -----------------------------------------------------------------------------
;; Key bindings
;; -----------------------------------------------------------------------------

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
(setq evil-disable-insert-state-bindings t)
(setq evil-respect-visual-line-mode t)

(require 'evil)
(evil-mode t)

(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'debugger-mode 'normal)
(evil-set-initial-state 'Custom-mode 'normal)

(setq evil-insert-state-cursor 'box)

(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (setq-default evil-symbol-word-search t))

(require 'evil-collection)

(with-eval-after-load 'dired (evil-collection-dired-setup))
(with-eval-after-load 'magit (evil-collection-magit-setup))
(with-eval-after-load 'term (evil-collection-term-setup))

(evil-set-undo-system 'undo-redo)

(setc scroll-margin 0)                          
(add-hook 'term-mode-hook                       
          (lambda ()                                 
            (make-local-variable 'scroll-margin)
            (setq scroll-margin 0)))   

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

(setq frame-title-format "Emacs")

;;;; ---------------------------------------------------------------------------
;;;; Line and column numbers
;;;; ---------------------------------------------------------------------------

(global-display-line-numbers-mode t)
(setc display-line-numbers-type 'relative)
(setc display-line-numbers-grow-only t)

(global-hl-line-mode)
(make-variable-buffer-local 'global-hl-line-mode)

(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;;;; ---------------------------------------------------------------------------
;;;; Windows and buffers
;;;; ---------------------------------------------------------------------------

(require 'balanced-windows)

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

(require 'rainbow-delimiters)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'text-mode-hook 'rainbow-delimiters-mode)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; -----------------------------------------------------------------------------
;; Text editing
;; -----------------------------------------------------------------------------

(setq-default tab-width 4)
(setq-default evil-shift-width 4)

(setq-default indent-tabs-mode nil)
(setq-default indent-line-function 'insert-tab)

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

(require 'ivy-rich)
(ivy-rich-mode t)

(require 'counsel)

(ivy-configure 'counsel-M-x
  :initial-input ""
  :display-transformer-fn #'counsel-M-x-transformer)

;; -----------------------------------------------------------------------------
;; Languages
;; -----------------------------------------------------------------------------

;;;; -------------------------------------------------------------------------
;;;; LSP
;;;; -------------------------------------------------------------------------

(require 'lsp-mode)

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

(setq save-abbrevs 'silently)

(define-abbrev-table 'global-abbrev-table
  '(
    ("qwerty" "test-abbreviation cursor before@@after")
   ))

;;;; -------------------------------------------------------------------------
;;;; Completion
;;;; -------------------------------------------------------------------------

(require 'company)
(require 'company-box)

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
(add-hook 'company-mode-hook 'company-box-mode)

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

(setc projectile-enable-caching t)
(setc projectile-generic-command "rg --files | tr '\\n' '\\0'")

(setc projectile-completion-system 'ivy)

(setc projectile-switch-project-action 'projectile-find-file)

(require 'counsel-projectile)
(require 'projectile-ripgrep)

(setq ivy-more-chars-alist '((t . 1)))

(call-interactively 'projectile-mode)

;; -----------------------------------------------------------------------------
;; Git
;; -----------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; Magit
;;;; ---------------------------------------------------------------------------

(require 'magit)
(require 'magit-blame)

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

(setq magit-save-repository-buffers 'dontask)

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

(require 'org)
(require 'org-faces)

(setq org-ellipsis " ▾")

(setq org-src-preserve-indentation t)
(setq org-edit-src-content-indentation 0)

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
(setq confirm-kill-processes nil)

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

;; -----------------------------------------------------------------------------
;; Vdiff
;; -----------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; General
;;;; ---------------------------------------------------------------------------

(require 'vdiff)

(setq vdiff-auto-refine nil)
(setq vdiff-subtraction-fill-char ? )

(setc vdiff-diff-algorithm 'diff)

(setc vdiff-fold-padding 10)

(defun ol-vdiff-fold-string (n-lines first-line-text width)
  (format "   %d lines\n" n-lines))

(setq vdiff-fold-string-function 'ol-vdiff-fold-string)

(advice-add 'vdiff-buffers :after (lambda (&rest r)
                                    (ol-enable-truncate-lines)
                                    (other-window 1)
                                    (ol-enable-truncate-lines)))

(defun ol-enable-truncate-lines ()
  (unless truncate-lines
    (toggle-truncate-lines)))

(defun ol-diff-on-quit (buffer-a buffer-b)
  (kill-buffer buffer-a))

(defun ol-vdiff-buffers-kill-only-leftmost (buffer-a buffer-b)
  (vdiff-buffers buffer-a buffer-b nil 'ol-diff-on-quit t nil))

;;;; ---------------------------------------------------------------------------
;;;; Magit integration
;;;; ---------------------------------------------------------------------------

(require 'vdiff-magit)

(setc vdiff-magit-stage-is-2way t)

;; Copy of vdiff, but modified to use ol-vdiff-buffers-kill-only-leftmost
(defun ol-vdiff-magit-show-working-tree (file)
  (interactive
   (list (magit-read-file-choice "Show changes in file"
                                 (magit-changed-files "HEAD")
                                 "No changed files")))
  (magit-with-toplevel
    (ol-vdiff-buffers-kill-only-leftmost
     (or (magit-get-revision-buffer "HEAD" file)
         (magit-find-file-noselect "HEAD" file))
     (or (get-file-buffer file) (find-file-noselect file)))))

(advice-add 'vdiff-magit-show-working-tree :override #'ol-vdiff-magit-show-working-tree)

;; Copy of vdiff, but modified to use ol-vdiff-buffers-kill-only-leftmost
(defun ol-vdiff-magit-show-unstaged (file)
  (interactive
   (list (magit-read-file-choice "Show unstaged changes for file"
                                 (magit-unstaged-files)
                                 "No unstaged files")))
  (magit-with-toplevel
    (ol-vdiff-buffers-kill-only-leftmost
     (or (get-buffer (concat file ".~{index}~"))
         (magit-find-file-index-noselect file t))
     (or (get-file-buffer file)
         (find-file-noselect file)))))

(advice-add 'vdiff-magit-show-unstaged :override #'ol-vdiff-magit-show-unstaged)

;; TODO: Make sure vdiff stage doesn't leave trailing buffers index and HEAD
;; Could check if they exist before, and if not, kill them on-quit.

;;;; ---------------------------------------------------------------------------
;;;; Magit diffing
;;;; ---------------------------------------------------------------------------

(defun ol-diff-file-head ()
  (interactive)
  (let* ((file (magit-current-file))
         (rev-head "HEAD")
         (buffer-head (msk--get-revision-buffer rev-head file)))
    (ol-vdiff-buffers-kill-only-leftmost buffer-head (current-buffer))))

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
    (ol-vdiff-buffers-kill-only-leftmost buffer-main (current-buffer))))

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


;; -----------------------------------------------------------------------------
;; Modeline
;; -----------------------------------------------------------------------------

;; Modeline stuff copied (and then modified) from
;; https://www.reddit.com/r/emacs/comments/1333621/wrote_a_custom_modeline_with_some_help_from/
(defun ol-mode-line-format (left right)
  "Return a string of `window-width' length. Containing LEFT, and RIGHT aligned
respectively."
  (let ((available-width (- (window-width) (length left) 1)))
    (format (format "%%s %%%ds " available-width) left right)))

(defface ol-evil-normal-state-mode-line-face '() "")
(defface ol-evil-insert-state-mode-line-face '() "")
(defface ol-evil-visual-state-mode-line-face '() "")
(defface ol-evil-emacs-state-mode-line-face '() "")
(defface ol-evil-operator-state-mode-line-face '() "")

(defface buffer-name-mode-line-face '()
  "Face for buffer name in mode-line.")

;; TODO: Split this into smaller functions
(setq-default
 mode-line-format
 '((:eval (ol-mode-line-format
           ;; left portion
           (format-mode-line
            (quote ("%e"
                    (:eval
                     (when (bound-and-true-p evil-local-mode)
                       (ol-evil-segment)))
                    "  "
                    (:eval (propertize " %b " 'face 'buffer-name-mode-line-face))
                    " " (:eval (ol-file-state-text))
                    " "
                    " %l:%c "
                    (:eval (format "%4d" (/ (point) 0.01 (point-max)))) "%%%%"
                    )))
           ;; right portion
           (format-mode-line (quote ((vc-mode vc-mode) ("  %e" (:eval (projectile-project-name))) ) ))))))

(defun ol-evil-segment ()
  (let ((evil-face (cond ((evil-normal-state-p)   'ol-evil-normal-state-mode-line-face)
                         ((evil-insert-state-p)   'ol-evil-insert-state-mode-line-face)
                         ((evil-visual-state-p)   'ol-evil-visual-state-mode-line-face)
                         ((evil-emacs-state-p)    'ol-evil-emacs-state-mode-line-face)
                         ((evil-operator-state-p) 'ol-evil-operator-state-mode-line-face)
                         (t                       'ol-evil-normal-state-mode-line-face))))
    (propertize
     (concat
      " "
      (truncate-string-to-width (string-pad (upcase (symbol-name evil-state)) 9 32) 6))
     'face evil-face)))

(defun ol-file-state-text ()
  (if buffer-read-only
      "%%%%"
    (if (buffer-modified-p) "*" "-")))

;; TODO Make the above formatting of states prettier

;; TODO: Make it look similar to what it looked like with doom
;; Left:  bar modals buffer-info buffer-position
;; Right: major-mode vcs proj-name

;; TODO: Make all this prettier (code and output), understand % and investigate
;; performance

;; -----------------------------------------------------------------------------
;; Dired
;; -----------------------------------------------------------------------------

(setq dired-kill-when-opening-new-dired-buffer t)

(when (ol-is-mac)       
  (setq dired-use-ls-dired nil))

(defun ol-dired ()
  (interactive)
  (dired default-directory))

(setq dired-listing-switches "-alh")

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

;; Taken from https://emacs.stackexchange.com/a/24658
(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))
