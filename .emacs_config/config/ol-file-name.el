;; -*- lexical-binding: t -*-

(require 'icomplete)

(require 'ol-project)

;; Called here to avoid circular dependency
(setq ol-switch-to-project-action #'ol-file-name)

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defun ol-dwim-root (prefer-project-root)
  (let* ((root (ol-project-root))

         (exception-modes '(
                            ;; So that can choose a custom dir to search in
                            dired-mode
                            ;; So that if running grep again from a grep buffer,
                            ;; the same dir is re-used
                            grep-mode)))
    (cond
     ((and root prefer-project-root) root)
     ((cl-member major-mode exception-modes) default-directory)
     (t root))))

(defun ol-can-use-rg ()
  (executable-find "rg" 'remote))

(defun ol-can-use-git ()
  (and (executable-find "git" 'remote)
       (locate-dominating-file default-directory ".git")))

(defun ol-can-use-gnu-cmd ()
  t)

;; -----------------------------------------------------------------------------
;; File name
;; -----------------------------------------------------------------------------

(ol-define-key ol-override-map "M-q" #'ol-file-name)

(defun ol-file-name (&optional prefer-project-root)
  (interactive "P")
  (let* ((default-directory (ol-dwim-root prefer-project-root))
         (cmd (ol-select-find-command))
         ;; Don't use shell-command because some shells slow to start
         ;; due to bashrc, and also cleaner to skip the middle-man.
         (candidates (apply #'ol-process-lines-ignore-status cmd))
         (prompt "File name: ")
         (selected (completing-read
                    prompt
                    candidates
                    nil ;; predicate
                    t ;; require-match
                    nil ;; initial-input
                    'ol-file-name
                    )))
    (find-file selected)))

(defun ol-process-lines-ignore-status (cmd &rest args)
  "Variant of `process-lines-ignore-status' that works over tramp."
  (with-temp-buffer
    (apply #'process-file cmd nil (current-buffer) nil args)
    (split-string (buffer-substring-no-properties (point-min) (point-max))
                  "\n" t)))

(defconst ol-select-file-name-command
  `((("rg" "--files") ,#'ol-can-use-rg)
    (("git" "ls-files") ,#'ol-can-use-git)
    (("find" "." "-not" "(" "-path" "*.git/*" "-prune" ")") ,#'ol-can-use-gnu-cmd)))

(defun ol-select-find-command ()
  (car (cl-find-if (lambda (method) (funcall (cadr method))) ol-select-file-name-command)))

(provide 'ol-file-name)
