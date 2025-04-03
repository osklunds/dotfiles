;;;  -*- lexical-binding: t; -*-

(require 'ol-util)
(require 'ol-ert)
(require 'ol-project)

(require 'vertico)
(require 'embark)
(require 'consult)
(require 'orderless)
(require 'grep)

(vertico-mode)

;; -----------------------------------------------------------------------------
;; Vertico
;; -----------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; Key bindings
;;;; ---------------------------------------------------------------------------

(ol-define-key ol-override-map "C-j" 'switch-to-buffer)

(ol-define-key minibuffer-local-map "C-j" 'next-line)
(ol-define-key minibuffer-local-map "C-k" 'previous-line)
(ol-define-key minibuffer-local-map "C-n" 'minibuffer-keyboard-quit)
(ol-define-key minibuffer-local-map 'tab 'vertico-exit)
(ol-define-key minibuffer-local-map 'return 'vertico-exit-input)
(ol-define-key minibuffer-local-map "M-i" 'vertico-insert)
(ol-define-key minibuffer-local-map "M-o" 'embark-collect)


;;;; ---------------------------------------------------------------------------
;;;; Behavior
;;;; ---------------------------------------------------------------------------

(setc vertico-count 20)
(setc vertico-cycle t)

;; -----------------------------------------------------------------------------
;; Consult
;; -----------------------------------------------------------------------------

(setc consult-async-min-input 0)
(setc consult-async-split-style 'none)

;;;; ---------------------------------------------------------------------------
;;;; Helpers
;;;; ---------------------------------------------------------------------------

(defun ol-dwim-use-project-root (&optional prefer-project-root)
  (let ((root (ol-project-root)))
    (cond
     ((and root prefer-project-root) root)
     ;; todo: don't have vterm here, but files aren't found if using project root
     ((cl-member major-mode '(dired-mode vterm-mode)) nil)
     (t root))))

;;;; ---------------------------------------------------------------------------
;;;; Find file name
;;;; ---------------------------------------------------------------------------

;; todo: understand sync/async in these consult commands

(setc consult-find-args "find . -not ( -path *.git/* -prune )")
(setc consult-fd-args "fd --full-path --color=never --hidden --exclude *.git/*")

(defun ol-dwim-find-file-name (&optional prefer-project-root)
  (interactive "P")
  (if-let ((root (ol-dwim-use-project-root prefer-project-root)))
      (ol-find-file-name root "project")
    (ol-find-file-name default-directory "cwd"))
  )

(defconst ol-find-file-name-methods
  `(("rg" "rg --files" ,(lambda () (executable-find "rg" 'remote)))
    ("git" "git ls-files" ,(lambda ()
                             (and (executable-find "git" 'remote)
                                  (locate-dominating-file default-directory ".git"))))
    ("find" "find . -not ( -path *.git/* -prune )" (lambda () t))))

(defun ol-find-file-name-method ()
  (cl-find-if (lambda (method) (funcall (nth 2 method))) ol-find-file-name-methods))

;; todo: handle dir and initial like consult
(defun ol-find-file-name (dir prompt-dir-part)
  (cl-destructuring-bind (name cmd _pred) (ol-find-file-name-method)
    (let* ((default-directory dir)
           (candidates (split-string (shell-command-to-string cmd) "\n" t))
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

(ol-define-key ol-override-map "M-q" #'ol-dwim-find-file-name)

;;;; ---------------------------------------------------------------------------
;;;; Find file content
;;;; ---------------------------------------------------------------------------

(defun ol-dwim-find-file-content (&optional prefer-project-root)
  (interactive "P")
  (if-let ((root (ol-dwim-use-project-root prefer-project-root)))
      (ol-find-file-content root "project")
    (ol-find-file-content default-directory "cwd"))
  )

(ol-define-key ol-override-map "M-e" #'ol-dwim-find-file-content)

(defun ol-find-file-content (dir prompt-dir-part)
  (if (file-remote-p dir)
      ;; Don't use async for remote since it opens so many tramp connections
      (ol-sync-find-file-content dir prompt-dir-part)
    (ol-async-find-file-content dir prompt-dir-part)))

(defun ol-async-find-file-content (dir prompt-dir-part)
  (cl-destructuring-bind (name consult-func _pred) (ol-async-find-file-content-method)
    (let* ((default-directory dir)
           (prompt (format "Find file content [%s %s]: " prompt-dir-part name)))
      (cl-letf (((symbol-function 'consult--directory-prompt)
                 (lambda (&rest _args)
                   (list prompt '(".") default-directory))))
        (funcall consult-func)))))

(defun ol-async-find-file-content-method ()
  (cl-find-if (lambda (method) (funcall (nth 2 method))) ol-async-find-file-content-methods))

(defconst ol-async-find-file-content-methods
  `(("rg" consult-ripgrep ,(lambda () (executable-find "rg" 'remote)))
    ("git" consult-git-grep
     ,(lambda () (and (executable-find "git" 'remote)
                      (locate-dominating-file default-directory ".git"))))
    ("grep" consult-grep (lambda () t))))

(defun ol-sync-find-file-content (dir prompt-dir-part)
  (cl-destructuring-bind (name cmd _pred) (ol-sync-find-file-content-method)
    (let* ((default-directory dir)
           (prompt (format "Find file content [sync %s %s]: " prompt-dir-part name))
           (pattern (read-from-minibuffer
                     prompt
                     nil ;; initial-contents
                     nil ;; keymap
                     nil ;; read
                     'ol-sync-find-file-content ;; history
                     ))
           (buffer-name (format "*ol-sync-find-file-content: %s*" pattern))
           (buffer (get-buffer-create buffer-name)))
      (shell-command (format "%s %s" cmd pattern) buffer)
      (with-current-buffer buffer
        (setq default-directory dir)
        (grep-mode))
      (switch-to-buffer-other-window buffer))))

(defconst ol-sync-find-file-content-methods
  `(("rg" "rg --no-heading --line-number --with-filename"
     ,(lambda () (executable-find "rg" 'remote)))
    ("git" "git --no-pager grep" ,(lambda ()
                                    (and (executable-find "git" 'remote)
                                         (locate-dominating-file default-directory ".git"))))
    ("grep" "grep -I -r" (lambda () t))))

(defun ol-sync-find-file-content-method ()
  (cl-find-if (lambda (method) (funcall (nth 2 method))) ol-sync-find-file-content-methods))

;; -----------------------------------------------------------------------------
;; Orderless
;; -----------------------------------------------------------------------------

(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

(provide 'ol-vertico)
