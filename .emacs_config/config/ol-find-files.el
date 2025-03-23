
(require 'ol-util)
(require 'ol-ivy)
(require 'ol-evil)
(require 'ol-project)

(require 'projectile)

;; -----------------------------------------------------------------------------
;; Find file name
;; -----------------------------------------------------------------------------

(defun ol-dwim-find-file-name (&optional prefer-project-root)
  (interactive "P")
  (if-let ((root (ol-dwim-use-project-root prefer-project-root)))
      (ol-find-file-name root "project")
    (ol-find-file-name default-directory "cwd")))

(defun ol-dwim-use-project-root (&optional prefer-project-root)
  (let ((root (ol-fallback-project-root)))
    (cond
     ((and root prefer-project-root) root)
     ;; todo: don't have vterm here, but files aren't found if using project root
     ((cl-member major-mode '(dired-mode vterm-mode)) nil)
     (t root))))

;; Inspired by counsel-file-jump
(defun ol-find-file-name (directory prompt)
  (let* ((default-directory directory)
         (cmd-and-args (ol-find-file-name-command-and-args))
         (cmd (car cmd-and-args))
         (find-program cmd) ;; Overrides the program used for finding
         (find-program-args (cadr cmd-and-args)))
    (ivy-read (concat "Find file [" prompt "]: ")
              (counsel--find-return-list find-program-args)
              :action #'find-file-existing
              :preselect (counsel--preselect-file)
              :require-match 'confirm-after-completion
              :history 'file-name-history
              :keymap counsel-file-jump-map
              :caller 'ol-find-file-name)))

(defconst all-ol-find-file-name-command-and-args
  (list (list "rg" '("--files"))
        (list "find" counsel-file-jump-args)))

(ol-require-external "rg")

(defun ol-find-file-name-command-and-args ()
  (let ((candidates all-ol-find-file-name-command-and-args)
        (result nil))
    (while (not result)
      (let* ((candidate (car candidates))
             (cmd (car candidate)))
        (if (executable-find cmd)
            (setq result candidate)
          (setq candidates (cdr candidates)))))
    result))

;; To handle rg returning error codes even if partial result
;; Inspired/copied from
;; https://github.com/doomemacs/doomemacs/issues/3038#issuecomment-832077836

(defun ol-counsel--call-advice (func &rest args)
  (let* ((old-fun (symbol-function #'process-file)))
    (cl-letf (((symbol-function 'process-file)
               (lambda (&rest process-file-args)
                 (apply old-fun process-file-args)
                 0)))
      (apply func args))))

(advice-add 'counsel--call :around 'ol-counsel--call-advice)

;; -----------------------------------------------------------------------------
;; Find with arbitrary cmd
;; -----------------------------------------------------------------------------

;; for file name: --no-ignore is relevant
;; for file content: --ignore and globs and filetypes are relevant

;; Useful examples (todo: make them templates you can complete-read)
;; rg --files --no-ignore -- file name filtered
;; rg --no-ignore -g '*.el'
;; find . -name '*.el'

(defun ol-find-cmd (&optional prefer-project-root)
  (interactive "P")
  (let* ((root (ol-dwim-use-project-root prefer-project-root))
         (default-directory (or root default-directory))
         (prompt (if root "Cmd [project]: " "Cmd [cwd]: ")))
    (ivy-read prompt
              'ol-find-cmd-fn
              :dynamic-collection t
              :initial-input ""
              :action 'find-file-existing
              :require-match t
              :history 'ol-find-cmd
              :caller 'ol-find-cmd)))

(defun ol-find-cmd-fn (input)
  (let* ((split (counsel--split-command-args input))
         (after (car split))
         (before (cdr split))
         (cmd (if (string-equal after "")
                  (concat "(unset TERM; " before " )")
                (let* ((regex (counsel--grep-regex after))
                       (quoted (shell-quote-argument regex)))
                  (concat "(unset TERM; " before " | rg " quoted " )")))))
    (counsel--async-command cmd))
  nil)

(ol-define-key ol-normal-leader-map "m c" 'ol-find-cmd)

;; -----------------------------------------------------------------------------
;; Find file content
;; -----------------------------------------------------------------------------

(ol-define-key ol-normal-leader-map "m o" 'swiper)

(setc counsel-rg-base-command "\
rg \
--max-columns 240 \
--with-filename \
--no-heading \
--line-number \
--color never \
%s || true")

(defun ol-dwim-find-file-content (&optional prefer-project-root)
  (interactive "P")
  (if-let ((root (ol-dwim-use-project-root prefer-project-root)))
      (ol-find-file-content root "project")
    (ol-find-file-content default-directory "cwd")))

(defun ol-find-file-content (directory prompt)
  (if (file-remote-p directory)
      (ol-sync-find-file-content directory prompt)
    (ol-async-find-file-content directory prompt)))

(defun ol-async-find-file-content (directory prompt)
  (counsel-rg "" directory "" (concat "Find file content [" prompt "]: ")))

(defun ol-sync-find-file-content (directory prompt)
  (let* ((prompt (concat "Find file content sync [" prompt "]: "))
         (pattern (read-from-minibuffer prompt))
         (cmd (concat "rg --no-heading --line-number --with-filename \"" pattern "\""))
         (buffer (get-buffer-create "*ol-sync-find-file-content*")))
    (shell-command cmd buffer)
    (with-current-buffer buffer
      (setq default-directory directory)
      (compilation-mode t))
    (switch-to-buffer-other-window buffer)))

(ol-define-key compilation-button-map "o" 'compile-goto-error)

(defun ol-swiper--line-advice (func &rest args)
  (cl-letf (((symbol-function 'buffer-substring) 'buffer-substring-no-properties))
    (apply func args)))

(advice-add 'swiper--line :around 'ol-swiper--line-advice)

(setq swiper-use-visual-line nil)
(setq swiper-use-visual-line-p (lambda (a) nil))

(provide 'ol-find-files)
