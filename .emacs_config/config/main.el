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
  `(customize-set-variable ',var ,val))

;; -----------------------------------------------------------------------------
;; General
;; -----------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; File Management
;;;; ---------------------------------------------------------------------------

;; No ~ files
(setq make-backup-files nil)

(require 'super-save)

(super-save-mode t)

(setq save-silently t)

(defun ol-save-buffer ()
  (interactive)
  (save-buffer)
  (message (format "Saved buffer: %s" (buffer-file-name))))


;;;; ---------------------------------------------------------------------------
;;;; Misc
;;;; ---------------------------------------------------------------------------

(setq gc-cons-threshold (eval-when-compile (* 1024 1024 1024))) ;; 1 GB

(defun ol-garbage-collect ()
  (message (format "Start garbage-collect at: %s gc-cons before: %s" (current-time-string) gc-cons-percentage))
  (garbage-collect)
  (message (format "Finish garbage-collect at: %s" (current-time-string))))

(run-with-idle-timer (* 2 60 60) t (lambda () (ol-garbage-collect)))

(setq read-process-output-max (* 1024 1024)) ;; 1 MB

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

(setq enable-local-variables nil)

(setc warning-minimum-level :error)

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

(require 'evil)
(evil-mode t)

(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'debugger-mode 'normal)
(evil-set-initial-state 'Custom-mode 'normal)
(evil-set-initial-state 'tar-mode 'normal)
(evil-set-initial-state 'archive-mode 'normal)

(setq evil-insert-state-cursor 'box)

(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (setq-default evil-symbol-word-search t))

(require 'evil-collection)

(with-eval-after-load 'dired (evil-collection-dired-setup))
(with-eval-after-load 'magit (evil-collection-magit-setup))
(with-eval-after-load 'term (evil-collection-term-setup))
(with-eval-after-load 'ivy (evil-collection-ivy-setup))

(evil-set-undo-system 'undo-redo)

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
(setc display-line-numbers-width-start 10000)

(global-hl-line-mode)
(make-variable-buffer-local 'global-hl-line-mode)

(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;;;; ---------------------------------------------------------------------------
;;;; Windows, buffers, frames
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

(defun ol-set-frame-size ()
  (set-frame-height (selected-frame) 44)
  (set-frame-width (selected-frame) 220))

(defun ol-center-frame ()
  (modify-frame-parameters (selected-frame)
                           '((user-position . t) (top . 0.5) (left . 0.5))))

(defun ol-center-and-size-frame ()
  (interactive)
  (ol-set-frame-size)
  (ol-center-frame))

(ol-center-and-size-frame)

;;;; ---------------------------------------------------------------------------
;;;; Misc
;;;; ---------------------------------------------------------------------------

(require 'rainbow-delimiters)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'text-mode-hook 'rainbow-delimiters-mode)

(global-visual-line-mode t)

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

(require 'counsel)

(ivy-configure 'counsel-M-x
  :initial-input ""
  :display-transformer-fn #'counsel-M-x-transformer)

;; TODO: can this be used with --files too?
(setc counsel-rg-base-command "\
rg \
--max-columns 240 \
--with-filename \
--no-heading \
--line-number \
--color never \
%s || true")

(defun ol-project-rg ()
  (interactive)
  (counsel-rg "" (projectile-project-root)))

;; -----------------------------------------------------------------------------
;; Languages
;; -----------------------------------------------------------------------------

;;;; -------------------------------------------------------------------------
;;;; LSP
;;;; -------------------------------------------------------------------------

(require 'lsp-mode)

(setq lsp-enable-symbol-highlighting nil)
(setq lsp-modeline-code-actions-enable nil)
(setq lsp-modeline-diagnostics-enable nil)
(setq lsp-diagnostics-provider :none)
(setq lsp-ui-sideline-enable nil)
(setq lsp-modeline-workspace-status-enable nil)
(setq lsp-lens-enable nil)
(setq lsp-ui-doc-enable nil)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-eldoc-enable-hover nil)
(setq lsp-signature-auto-activate nil)
(setq lsp-enable-snippet nil)

(setq flycheck-indication-mode nil)

(setc lsp-auto-guess-root t)

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
;; Git and Magit
;; -----------------------------------------------------------------------------

(require 'magit)
(require 'magit-blame)

;;;; ---------------------------------------------------------------------------
;;;; Blame
;;;; ---------------------------------------------------------------------------

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

;;;; ---------------------------------------------------------------------------
;;;; Commit
;;;; ---------------------------------------------------------------------------

;; Start in insert state when doing commits in magit
(add-hook 'with-editor-mode-hook 'evil-insert-state)

;; To make sure the commit message is always uncluttered
(defun ol-git-commit-setup ()
  (insert "\n\n")
  (beginning-of-buffer))

(add-hook 'git-commit-setup-hook 'ol-git-commit-setup)

;;;; ---------------------------------------------------------------------------
;;;; Status
;;;; ---------------------------------------------------------------------------

(setq magit-save-repository-buffers 'dontask)

;; TODO: Make a "full status" key that re-adds these
;; TODO: Add numstat perhaps
;; TODO: Consider setting list instead
;; Simplify magit-status for better performance
(remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
(remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
(remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
(remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
(remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
(remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)

(magit-add-section-hook 'magit-status-sections-hook 'ol-magit-insert-status-header)

(defun ol-magit-insert-status-header ()
  (magit-set-header-line-format "Magit Status"))

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

(setq magit-diff-paint-whitespace nil)

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
;;;; Merge Survival Knife (WIP)
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

(setc term-scroll-to-bottom-on-output t)

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
    (setq server-name (ol-find-free-server-name))
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

;; -----------------------------------------------------------------------------
;; Vdiff
;; -----------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; General
;;;; ---------------------------------------------------------------------------

(require 'vdiff)
(require 'vdiff-magit)

(setq vdiff-auto-refine nil)
(setq vdiff-subtraction-fill-char ? )

(setc vdiff-diff-algorithm 'diff)

(setc vdiff-fold-padding 10)

(defun ol-vdiff-fold-string (n-lines first-line-text width)
  (format "   %d lines\n" n-lines))

(setq vdiff-fold-string-function 'ol-vdiff-fold-string)

(setc vdiff-magit-stage-is-2way t)

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
  (if-let ((bfn (buffer-file-name)))
      (vc-git--symbolic-ref bfn)
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
;; Tab bar buffers
;; -----------------------------------------------------------------------------

(require 'cl-lib)

;;;; ---------------------------------------------------------------------------
;;;; State
;;;; ---------------------------------------------------------------------------

(defvar ol-buffers-mru '(nil nil nil nil nil))
(defvar ol-buffers-positions '(nil nil nil nil nil))

;;;; ---------------------------------------------------------------------------
;;;; Handling updates
;;;; ---------------------------------------------------------------------------

(defun ol-buffer-list-update ()
  (ol-new-buffer-displayed))

(defun ol-new-buffer-displayed ()
  (ol-clean-up-killed-buffers)
  (when (ol-is-relevant-buffer)
    (ol-new-relevant-buffer-displayed (current-buffer))))

(defun ol-clean-up-killed-buffers ()
  (dolist (index (number-sequence 1 (length ol-buffers-mru)))
    (let ((buffer (nth (- index 1) ol-buffers-mru)))
      ;; (message "buffer %s is live? %s" buffer (buffer-live-p buffer))
      (when (and buffer (not (buffer-live-p buffer)))
        ;; (message "cleaning up %s" buffer)
        (setq ol-buffers-mru (cl-substitute nil buffer ol-buffers-mru))
        (setq ol-buffers-positions (cl-substitute nil buffer ol-buffers-positions))
        (ol-buffers-consistency-check)))))

(defun ol-is-relevant-buffer ()
  ;; TODO: Use and not not not instead for higher performance
  (let* ((name (buffer-name)))
    (or (eq major-mode 'term-mode )
    (not (or (minibufferp)
             (ol-is-temporary-buffer)
             vdiff-mode
             (string-match-p "eldoc for" name)
             (string-match-p "COMMIT_EDITMSG" name)
             (string-match-p "\\*server\\*" name))))))

;; Inspired by vdiff-magit--kill-buffer-if-temporary
(defun ol-is-temporary-buffer ()
  (let ((buf-file (buffer-file-name)))
    (or (not buf-file) (not (file-exists-p buf-file)))))

(defun ol-new-relevant-buffer-displayed (new-buffer)
    (if (member new-buffer ol-buffers-mru)
        (ol-update-buffers-already-in-mru new-buffer)
      (ol-update-buffers-not-in-mru new-buffer)))
  ;; (message "now holds %s in order %s" ol-buffers-mru ol-buffers-positions))

(defun ol-update-buffers-already-in-mru (new-buffer)
  (setq ol-buffers-mru (delete new-buffer ol-buffers-mru))
  (setq ol-buffers-mru (add-to-list 'ol-buffers-mru new-buffer)))

(defun ol-update-buffers-not-in-mru (new-buffer)
  (ol-buffers-consistency-check)
  ;; (message "MRU: %s" ol-buffers-mru)
  ;; (message "Pos: %s" ol-buffers-positions)
  (let* ((buffer-to-evict (ol-find-buffer-to-evict)))
    ;; (message "Old %s" buffer-to-evict)
    (setq ol-buffers-mru (cl-delete buffer-to-evict ol-buffers-mru :count 1))
    (setq ol-buffers-mru (add-to-list 'ol-buffers-mru new-buffer))
    (setq ol-buffers-positions (cl-substitute
                                new-buffer
                                buffer-to-evict
                                ol-buffers-positions
                                :count 1)))
  ;; (message "MRU: %s" ol-buffers-mru)
  ;; (message "Pos: %s" ol-buffers-positions)
  (ol-buffers-consistency-check))

(defun ol-find-buffer-to-evict ()
  (if (member nil ol-buffers-mru)
      nil
    (car (last ol-buffers-mru))))

(defun ol-buffers-consistency-check ()
  ;; duplicates, length, same elements
  (dolist (buffer ol-buffers-mru)
    (cl-assert (member buffer ol-buffers-positions)))

  (dolist (buffer ol-buffers-positions)
    (cl-assert (member buffer ol-buffers-mru)))

  (cl-assert (= (length ol-buffers-mru) (length ol-buffers-positions))))

(add-hook 'buffer-list-update-hook 'ol-buffer-list-update)

;;;; ---------------------------------------------------------------------------
;;;; Formatting the string
;;;; ---------------------------------------------------------------------------

(defun ol-buffers-string ()
  (let* ((prefix "[Emacs]")
         (width-for-buffers (- (round (* 0.7 (frame-width))) (length prefix)))
         (buffer-part (ol-format-buffers width-for-buffers)))
    (concat prefix buffer-part)))

(defun ol-format-buffers (width-for-buffers)
  (let* ((num-buffers (length ol-buffers-mru))
         (indexes (number-sequence 1 num-buffers))
         (width-per-buffer (/ width-for-buffers num-buffers)))
    (apply 'concat (mapcar (lambda (index)
                             (ol-format-buffer index width-per-buffer))
                             indexes))))

(defun ol-format-buffer (index width)
  (let* ((buffer (nth (- index 1) ol-buffers-positions))
         (name (ol-buffer-name buffer))
         (align-format-string (format "  |  %%d  %%-%ds" width)))
    (truncate-string-to-width (format align-format-string index name) width)))

(defun ol-buffer-name (buffer)
  ;; TODO: Handle clean up of killed buffers. Maybe when updating lists,
  ;; iterate and check if some killed, then remove
  (if (and buffer (buffer-live-p buffer))
      (buffer-name buffer)
    ""))

(setq frame-title-format (quote ((:eval (ol-buffers-string)))))

;;;; ---------------------------------------------------------------------------
;;;; Keys
;;;; ---------------------------------------------------------------------------

(defun ol-buffers-switch-to (index)
  (interactive)
  (switch-to-buffer (nth (- index 1) ol-buffers-positions)))

;; -----------------------------------------------------------------------------
;; Dired
;; -----------------------------------------------------------------------------

(require 'dired)

(setq dired-kill-when-opening-new-dired-buffer t)

(when (ol-is-mac)       
  (setq dired-use-ls-dired nil))

(defun ol-dired ()
  (interactive)
  (dired default-directory))

(setq dired-listing-switches "-Alh")

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
