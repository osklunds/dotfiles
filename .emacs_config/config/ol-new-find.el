
;;;  -*- lexical-binding: t; -*-

(require 'ol-projectile)
(require 'ol-ert)

;; -----------------------------------------------------------------------------
;; Common
;; -----------------------------------------------------------------------------

(defun ol2-dwim-use-project-root (&optional prefer-project-root)
  (let ((root (ol-fallback-project-root)))
    (cond
     ((and root prefer-project-root) root)
     ;; todo: don't have vterm here, but files aren't found if using project root
     ((cl-member major-mode '(dired-mode vterm-mode)) nil)
     (t root))))

(defun ol2-cmd-available (cmd-with-args)
  (executable-find (car cmd-with-args) 'remote))

(ert-deftest ol2-cmd-available-test ()
  (ol-assert     (ol2-cmd-available '("rg")))
  (ol-assert     (ol2-cmd-available '("rg" "--files")))
  (ol-assert     (ol2-cmd-available '("find")))
  (ol-assert-not (ol2-cmd-available '("findd")))
  )

;; -----------------------------------------------------------------------------
;; File name
;; -----------------------------------------------------------------------------

;; Even if rg is available locally, it might not be over tramp
(defconst ol2-find-file-cmds '(
                               ("rg" "--files")
                               ("find")
                               ))

(defun ol2-find-file-cmd ()
  (cl-find-if 'ol2-cmd-available ol2-find-file-cmds))

(ert-deftest ol2-find-file-cmd-test ()
  (ol-assert-equal '("rg" "--files") (ol2-find-file-cmd))
  )

(defun ol2-dwim-find-file-name (&optional prefer-project-root)
  (interactive "P")
  (if-let ((root (ol2-dwim-use-project-root prefer-project-root)))
      (ol2-find-file-name root "project")
    (ol2-find-file-name default-directory "cwd")))

(defun ol2-find-file-name (dir prompt-dir-part)
  (interactive)
  (let* ((cmd (ol2-find-file-cmd))
         (candidates (apply 'process-lines-ignore-status cmd))
         (prompt (format "Find file name [%s %s]: " prompt-dir-part cmd))
         (selected (completing-read
                    prompt
                    candidates
                    nil ;; predicate
                    t ;; require match
                    nil ;; initial input
                    'ol2-find-file-name ;; history
                    )))
    (find-file selected)))

(ol-define-normal-leader-key "e n" 'ol2-dwim-find-file-name)

;; -----------------------------------------------------------------------------
;; File content
;; -----------------------------------------------------------------------------

;; Even if rg is available locally, it might not be over tramp
(defconst ol2-find-file-content-cmds
  '(
    ("rg" "--no-heading" "--line-number" "--with-filename")
    ("git" "--no-pager" "grep" "--line-number")
    ("grep" "-H" "-r" "-n")
    ))

(defun ol2-find-file-content-cmd ()
  (cl-find-if 'ol2-cmd-available ol2-find-file-content-cmds))

(ert-deftest ol2-find-file-content-cmd-test ()
  (ol-assert-equal '("rg" "--no-heading" "--line-number" "--with-filename")
                   (ol2-find-file-content-cmd)))

(defun ol2-dwim-find-file-content (&optional prefer-project-root)
  (interactive "P")
  (if-let ((root (ol2-dwim-use-project-root prefer-project-root)))
      (ol2-find-file-content root "project")
    (ol2-find-file-content default-directory "cwd")))

(defvar ol2-find-file-content-current-cmd nil)

(defun ol2-find-file-content (dir prompt-dir-part)
  (interactive)
  (setq ol2-find-file-content-last-probe nil)
  (setq ol2-find-file-content-last-candidates nil)

  (let* ((cmd (ol2-find-file-content-cmd))
         (prompt (format "Find file content [%s %s]: " prompt-dir-part cmd)))
    (setq ol2-find-file-content-current-cmd cmd)

    (let* ((selected (completing-read
                      prompt
                      'ol2-find-file-content-collection
                      nil ;; predicate
                      t ;; require match
                      nil ;; initial input
                      'ol2-find-file-content ;; history
                      )))
      (find-file selected))))

(defun ol2-find-file-content-collection (probe _pred action)
  (cond
   ((eq (car-safe action) 'boundaries) nil)
   ((eq action 'metadata) nil)
   ((eq action t) (ol2-find-file-content-collection-1 probe))))

(defun ol2-find-file-content-collection-1 (probe)
  (let* ((inhibit-message t)
         (maybe-candidates
          (while-no-input
            (redisplay)
            `(finished . ,(ol2-find-file-content-collection-sync probe)))))
    (pcase maybe-candidates
      (`(finished . ,candidates) candidates)
      ('t nil)
      ('nil (error "todo"))))
  )

(defun ol2-find-file-content-collection-sync (probe)
  (let ((cmd (append ol2-find-file-content-current-cmd `(,probe))))
    (apply 'process-lines-ignore-status cmd)))

(ol-define-normal-leader-key "e c" 'ol2-dwim-find-file-content)

(provide 'ol-new-find)
