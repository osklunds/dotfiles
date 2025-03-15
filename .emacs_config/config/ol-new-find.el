
(require 'ol-projectile)
(require 'ol-ert)

;; -----------------------------------------------------------------------------
;; Root
;; -----------------------------------------------------------------------------

(defun ol2-dwim-use-project-root (&optional prefer-project-root)
  (let ((root (projectile-project-root)))
    (cond
     ((and root prefer-project-root) root)
     ;; todo: don't have vterm here, but files aren't found if using project root
     ((cl-member major-mode '(dired-mode vterm-mode)) nil)
     (t root))))

;; -----------------------------------------------------------------------------
;; File name
;; -----------------------------------------------------------------------------

;; Even if rg is available locally, it might not be over tramp
(defconst ol2-find-file-cmds '("rg --files"
                               "find"
                               ))

(defun ol2-cmd-available (cmd-with-args)
  (let ((cmd (car (split-string cmd-with-args " "))))
    (executable-find cmd 'remote)))

(ert-deftest ol2-cmd-available-test ()
  (ol-assert     (ol2-cmd-available "rg"))
  (ol-assert     (ol2-cmd-available "rg --files"))
  (ol-assert     (ol2-cmd-available "find"))
  (ol-assert-not (ol2-cmd-available "findd"))
  )

(defun ol2-find-file-cmd ()
  (cl-find-if 'ol2-cmd-available ol2-find-file-cmds))

(ert-deftest ol2-find-file-cmd-test ()
  (ol-assert-equal "rg --files" (ol2-find-file-cmd))
  )

(defun ol2-dwim-find-file-name (&optional prefer-project-root)
  (interactive "P")
  (if-let ((root (ol2-dwim-use-project-root prefer-project-root)))
      (ol2-find-file-name root "project")
    (ol2-find-file-name default-directory "cwd")))

(defun ol2-find-file-name (dir prompt-dir-part)
  (interactive)
  (let* ((cmd (ol2-find-file-cmd))
         (candidates (split-string (shell-command-to-string cmd) "\n" t))
         (prompt (format "Find file name [%s '%s']" prompt-dir-part cmd))
         (selected (completing-read
                    prompt
                    candidates
                    nil
                    t ;; require match
                    nil
                    'ol2-find-file-name
                    )))
    (find-file selected)))

(provide 'ol-new-find)
