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

;; Many things here are actually not consult, however, the are in the same area
;; as consult in the sense that they provide minibuffer read functions for
;; useful things. consult is not even a requirement for many functions here.
;; consult is mainly used for its async functionality and because e.g.
;; consult-ripgrep provides some extra features compared to running plain
;; rg async in the minibuffer.

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

(defun ol-can-use-rg ()
  (executable-find "rg" 'remote))

(defun ol-can-use-git ()
  (and (executable-find "git" 'remote)
       (locate-dominating-file default-directory ".git")))

(defun ol-can-use-gnu-cmd ()
  ;; Hopefully a not-so-bold assumption
  t)

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
(ol-define-key ol-override-map "M-q" #'ol-dwim-find-file-name)
(setq ol-switch-to-project-action #'ol-dwim-find-file-name)

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

(defun ol-find-file-name-method ()
  (cl-find-if (lambda (method) (funcall (nth 2 method))) ol-find-file-name-methods))

(defconst ol-find-file-name-methods
  `(("rg" "rg --files" ,#'ol-can-use-rg)
    ("git" "git ls-files" ,#'ol-can-use-git)
    ("find" "find . -not ( -path *.git/* -prune )" ,#'ol-can-use-gnu-cmd)))


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
  `(("rg" consult-ripgrep ,#'ol-can-use-rg)
    ("git" consult-git-grep ,#'ol-can-use-git)
    ("grep" consult-grep ,#'ol-can-use-gnu-cmd)))

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
  `(("rg" "rg --no-heading --line-number --with-filename" ,#'ol-can-use-rg)
    ("git" "git --no-pager grep" ,#'ol-can-use-git)
    ("grep" "grep -I -r" ,#'ol-can-use-gnu-cmd)))

(defun ol-sync-find-file-content-method ()
  (cl-find-if (lambda (method) (funcall (nth 2 method))) ol-sync-find-file-content-methods))

;;;; ---------------------------------------------------------------------------
;;;; Shell command
;;;; ---------------------------------------------------------------------------

;; Not ideal to call consult internal functions, but hopefully the API is stable
;; enough, considering the public consult functions need similar
;; functionality. If something breaks I can compare the code at this commit and
;; how consult functions using consult--dynamic-collection change, and hopefully
;; figure something out. Need to stay optimistic.  In the worst case, I can live
;; with sync shell command. As a longer-term todo, figure out how it works.

(defun ol-candidates (input)
  (let ((inhibit-message t))
    (split-string (shell-command-to-string (format "sleep 1; %s" input)) "\n" t)))

(consult--read
 (consult--dynamic-collection 'ol-candidates)
 :prompt "hej: ")

;; -----------------------------------------------------------------------------
;; Orderless
;; -----------------------------------------------------------------------------

(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

(provide 'ol-vertico)
