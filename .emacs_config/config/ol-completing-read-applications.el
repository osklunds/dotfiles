;; -*- lexical-binding: t -*-

(require 'ol-completing-read-framework)

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defun ol-dwim-use-project-root (&optional prefer-project-root)
  (let ((root (ol-project-root)))
    (cond
     ((and root prefer-project-root) root)
     ;; todo: don't have vterm here, but files aren't found if using project root
     ((cl-member major-mode '(dired-mode vterm-mode)) nil)
     (t root))))

(defun ol-can-use-rg ()
  (executable-find "rg" 'remote))

(defun ol-can-use-git ()
  (and (executable-find "git" 'remote)
       (locate-dominating-file default-directory ".git")))

(defun ol-can-use-gnu-cmd ()
  ;; Hopefully a not-so-bold assumption
  t)

;; -----------------------------------------------------------------------------
;; Sync applications
;; -----------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; File name
;;;; ---------------------------------------------------------------------------

(defun ol-dwim-find-file-name (&optional prefer-project-root)
  (interactive "P")
  (if-let ((root (ol-dwim-use-project-root prefer-project-root)))
      (ol-find-file-name root "project")
    (ol-find-file-name default-directory "cwd"))
  )

(defun ol-find-file-name (dir prompt-dir-part)
  (cl-destructuring-bind (name cmd _pred) (ol-find-file-name-method)
    (let* ((default-directory dir)
           ;; Don't use shell-command because some shells slow to start
           ;; due to bashrc, and also cleaner to skip the middle-man.
           (candidates (apply #'ol-process-lines-ignore-status cmd))
           (prompt (format "Find file name [%s %s]: " prompt-dir-part name))
           (selected (completing-read
                      prompt
                      candidates
                      nil ;; predicate
                      t ;; require-match
                      nil ;; initial-input
                      'ol-find-file-name-name
                      )))
      (find-file selected))))

(defun ol-process-lines-ignore-status (cmd &rest args)
  (if (file-remote-p default-directory)
      (apply #'ol-remote-process-lines-ignore-status cmd args)
    ;; If not remote, use process-lines-ignore-status because it seems
    ;; to be slightly faster
    (apply #'process-lines-ignore-status cmd args)))

(defun ol-remote-process-lines-ignore-status (cmd &rest args)
  "Variant of `process-lines-ignore-status' that works over tramp."
  (with-temp-buffer
    (apply #'process-file cmd nil (current-buffer) nil args)
    (split-string (buffer-substring-no-properties (point-min) (point-max))
                  "\n" t)))

(defconst ol-find-file-name-methods
  `(("rg" ("rg" "--files") ,#'ol-can-use-rg)
    ("git" ("git" "ls-files") ,#'ol-can-use-git)
    ("find" ("find" "." "-not" "(" "-path" "*.git/*" "-prune" ")") ,#'ol-can-use-gnu-cmd)))

(defun ol-find-file-name-method ()
  (cl-find-if (lambda (method) (funcall (nth 2 method))) ol-find-file-name-methods))

;;;; ---------------------------------------------------------------------------
;;;; File content
;;;; ---------------------------------------------------------------------------

;; todo: messes things up (recursive) when not called over tramp
(defun ol-sync-find-file-content (dir prompt-dir-part)
  (cl-destructuring-bind (name cmd _pred) (ol-find-file-content-method)
    (let* ((default-directory dir)
           (prompt (format "Find file content [sync %s %s]: " prompt-dir-part name))
           (pattern (read-from-minibuffer
                     prompt
                     nil ;; initial-contents
                     nil ;; keymap
                     nil ;; read
                     'ol-sync-find-file-content ;; history
                     ))
           ;; buffer-name important for two reasons:
           ;; 1. So that async reading doesn't re-use it
           ;; 2. So that it can be switched to
           (buffer-name (format "*ol-sync-find-file-content: %s*" pattern))
           (compilation-buffer-name-function (lambda (&rest _)
                                               buffer-name)))
      (cl-letf (;; To force sync
                ((symbol-function 'make-process) nil)
                ;; To make it preserve default-directory when remote
                ((symbol-function 'call-process) #'process-file))
        (grep (format "%s %s" cmd (ol-string-to-regex pattern))))
      (switch-to-buffer-other-window buffer-name))))

(defconst ol-rg-command "rg --color=always --smart-case --no-heading --line-number\
 --with-filename --null")
(defconst ol-git-grep-command "git --no-pager grep --color=always --line-number")
(defconst ol-grep-command "grep --color=always --extended-regexp --line-number\
 --binary-files=without-match --recursive --null")

(defconst ol-find-file-content-methods
  `(("rg" ,ol-rg-command ,#'ol-can-use-rg)
    ("git" ,ol-git-grep-command ,#'ol-can-use-git)
    ("grep" ,ol-grep-command ,#'ol-can-use-gnu-cmd)))

(defun ol-find-file-content-method ()
  (cl-find-if (lambda (method) (funcall (nth 2 method))) ol-find-file-content-methods))

;;;; ---------------------------------------------------------------------------
;;;; switch-to-buffer
;;;; ---------------------------------------------------------------------------

(defun ol-switch-to-buffer ()
  "Similar to `switch-to-buffer' but avoids face problems and puts current
buffer last."
  (interactive)
  (let* ((buffers (cl-remove-if (lambda (buffer)
                                  (or
                                   (eq buffer (current-buffer))
                                   (minibufferp buffer) 
                                   ))
                                (buffer-list)))
         (buffer-names (mapcar (lambda (buffer) (with-current-buffer buffer
                                                  (buffer-name)))
                               (append buffers (list (current-buffer)))))
         ;; Copied/modified from https://emacs.stackexchange.com/a/8177
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      `(metadata
                        (ol-extra-highlight-function . ,#'ol-switch-to-buffer-highlight-fn)
                        (ol-delete-action . ,#'ol-switch-to-buffer-delete-action)
                        (cycle-sort-function . ,#'identity)
                        (display-sort-function . ,#'identity))
                    (complete-with-action action buffer-names string pred))))
         (buffer (completing-read
                  "Switch to buffer: "
                  table)))
    (if (get-buffer buffer)
        (switch-to-buffer buffer)
      ;; if two buffers with same name but different <dir> suffix existed, one
      ;; is deleteed, then the remaining buffer changes name but not the one
      ;; among the candidates in completion.
      (message "%S doesn't exist (anymore), not switching"))))

(defun ol-switch-to-buffer-highlight-fn (candidate)
  ;; "when" version needed to fix bug when two buffers of same file name are
  ;; open, and one is deleted. The remaining one will change name from name<dir>
  ;; to name and hence not be found anymore
  (when-let* ((buffer (get-buffer candidate))
              (mode (buffer-local-value 'major-mode buffer)))
    (cond
     ;; todo: consider what to do if remote and dired
     ;; (find-file "/docker:tests-dotfiles-tramp-test-1:/")
     ((file-remote-p (buffer-local-value 'default-directory buffer))
      (ol-add-face-text-property candidate 'ol-remote-buffer-name-face))

     ((eq mode 'dired-mode)
      (ol-add-face-text-property candidate 'ol-dired-buffer-name-face))

     ((eq mode 'vterm-mode)
      (ol-add-face-text-property candidate 'ol-vterm-buffer-name-face))

     (t nil))))

(defun ol-add-face-text-property (str face)
  (add-face-text-property 0 (length str) face nil str))

(defface ol-dired-buffer-name-face
  '((default :weight bold :inherit 'font-lock-function-name-face))
  "Face for dired buffer name in `ol-switch-to-buffer'.")

(defface ol-vterm-buffer-name-face
  '((default :weight bold :inherit 'font-lock-type-face))
  "Face for vterm buffer name in `ol-switch-to-buffer'.")

(defface ol-remote-buffer-name-face
  '((default :foreground "#110099"))
  "Face for remote buffer name in `ol-switch-to-buffer'.")

(defun ol-switch-to-buffer-delete-action (selected)
  ;; Use "when" version as extra robustification, although unsure if needed
  (when-let ((buf (get-buffer selected)))
    (kill-buffer buf)))

;; -----------------------------------------------------------------------------
;; Async applications
;; -----------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; File content
;;;; ---------------------------------------------------------------------------

(defun ol-dwim-find-file-content (&optional prefer-project-root)
  (interactive "P")
  (if-let ((root (ol-dwim-use-project-root prefer-project-root)))
      (ol-find-file-content root "project")
    (ol-find-file-content default-directory "cwd"))
  )
(defun ol-find-file-content (dir prompt-dir-part)
  (if (file-remote-p dir)
      ;; Don't use async for remote since it opens so many tramp connections
      (ol-sync-find-file-content dir prompt-dir-part)
    (ol-async-find-file-content dir prompt-dir-part)))

(defun ol-async-find-file-content (dir prompt-dir-part)
  (cl-destructuring-bind (name cmd _pred) (ol-find-file-content-method)
    (let* ((default-directory dir)
           (prompt (format "Find file content [%s %s]: " prompt-dir-part name)))
      (ol-grep-helper prompt cmd))))

(defun ol-ripgrep (prompt)
  (ol-grep-helper prompt ol-rg-command))

(defun ol-git-grep (prompt)
  (ol-grep-helper prompt ol-git-grep-command))

(defun ol-grep (prompt)
  (ol-grep-helper prompt ol-grep-command))

(defun ol-grep-input-to-cmd (input)
  (let* ((split (ol-split-string-once input " -- "))
         (before (car split))
         (after-plain (cdr split))
         (after-as-regex (ol-string-to-regex after-plain))
         (after (if (and before (string-match-p "-F" before)) after-plain after-as-regex))
         (after-quoted (shell-quote-argument after)))
    (if before
        (concat before " -- " after-quoted)
      after-quoted)))

(ert-deftest ol-grep-input-to-cmd-test ()
  ;; Search for one term
  (ol-assert-equal "hej" (ol-grep-input-to-cmd "hej"))

  ;; Serach two terms with space wildcard
  (ol-assert-equal "a.\\*\\?b" (ol-grep-input-to-cmd "a b"))

  ;; Search for literal space
  (ol-assert-equal "a\\ b" (ol-grep-input-to-cmd "a  b"))

  ;; Specify option towards grep, a is the option b is the search term
  (ol-assert-equal "a -- b" (ol-grep-input-to-cmd "a -- b"))

  ;; Specify option towards grep and use literal -- in search term
  (ol-assert-equal "a -- b.\\*\\?--.\\*\\?c" (ol-grep-input-to-cmd "a -- b -- c"))

  ;; Option and two terms
  (ol-assert-equal "a -- b.\\*\\?c" (ol-grep-input-to-cmd "a -- b c"))

  ;; Search for literal --
  (ol-assert-equal " -- --" (ol-grep-input-to-cmd " -- --"))

  ;; Fixed string option
  (ol-assert-equal "-F -- a" (ol-grep-input-to-cmd "-F -- a"))
  (ol-assert-equal "-F -- a\\ b" (ol-grep-input-to-cmd "-F -- a b"))
  (ol-assert-equal "-F -- a\\ \\ b" (ol-grep-input-to-cmd "-F -- a  b"))
  )

(defun ol-split-string-once (string separator)
  (if-let ((pos (string-match separator string)))
      (cons (substring string 0 pos)
            (substring string (+ pos (length separator))))
    (cons nil string)))

(ert-deftest ol-split-string-once-test ()
  (ol-assert-equal `(,nil . "hej") (ol-split-string-once "hej" " -- "))
  (ol-assert-equal '("hej" . "hello") (ol-split-string-once "hej -- hello" " -- "))
  (ol-assert-equal '("hej " . "hello") (ol-split-string-once "hej  -- hello" " -- "))
  (ol-assert-equal '("hej" . " hello") (ol-split-string-once "hej --  hello" " -- "))
  (ol-assert-equal '("a" . "b -- c") (ol-split-string-once "a -- b -- c" " -- "))
  )

(defun ol-grep-helper (prompt args)
  (setq ol-async-goto-function #'compile-goto-error)
  (let* ((input-to-cmd
          (lambda (input)
            (concat args " " (ol-grep-input-to-cmd input)))))
    (ol-async-completing-read prompt input-to-cmd 'ol-grep)))

;;;; ---------------------------------------------------------------------------
;;;; File name
;;;; ---------------------------------------------------------------------------

(defun ol-dwim-async-find-file-name (&optional prefer-project-root)
  (interactive)
  (if-let ((root (ol-dwim-use-project-root prefer-project-root)))
      (ol-async-find-file-name root "project")
    (ol-async-find-file-name default-directory "cwd")))

;; todo: fix bug that sometimes, especially after insert option, no results are
;; shown
(defun ol-async-find-file-name (dir prompt-dir-part)
  (interactive)
  (setq ol-async-goto-function #'ol-async-goto-file-name)
  (let* ((default-directory dir)
         (prompt (format "Async find file name [%s]: " prompt-dir-part))
         (grep-use-headings nil))
    (ol-async-completing-read prompt #'ol-async-find-file-name-input-to-cmd
                              'ol-async-find-file-name)))

(defun ol-async-find-file-name-input-to-cmd (input)
  (let* ((split (ol-split-string-once input " -- "))
         (before (car split))
         (options (or before ""))
         (after-plain (cdr split))
         (after-as-regex (ol-string-to-regex after-plain))
         (after-quoted (shell-quote-argument after-as-regex)))
    (concat "rg --files " options " | rg " options " -- " after-quoted)))

(ert-deftest ol-async-find-file-name-input-to-cmd-test ()
  ;; one term
  (ol-assert-equal "rg --files  | rg  -- defun"
                   (ol-async-find-file-name-input-to-cmd "defun"))

  ;; wildcard
  (ol-assert-equal "rg --files  | rg  -- def.\\*\\?fun"
                   (ol-async-find-file-name-input-to-cmd "def fun"))

  ;; options
  (ol-assert-equal "rg --files o | rg -o -- defun"
                   (ol-async-find-file-name-input-to-cmd "-o -- defun"))
  )

(defun ol-async-goto-file-name ()
  (let ((file-name (buffer-substring-no-properties (line-beginning-position)
                                                   (line-end-position))))
    (find-file file-name)))


(provide 'ol-completing-read-applications)
