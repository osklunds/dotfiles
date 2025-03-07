
(defun ol-main-branch ()
  (let ((main-branch "main"))
    (if (ol-does-branch-exist main-branch)
        main-branch
      "master")))

(defun ol-does-branch-exist (branch)
  (let ((all-branches (shell-command-to-string "git branch --list"))
        (regex (concat "[ \\n]" branch "$")))
    (string-match-p regex all-branches)))

;; Valid assumption in this repo
(let ((default-directory (file-name-directory load-file-name)))
  (cl-assert (ol-does-branch-exist "main"))
  (cl-assert (not (ol-does-branch-exist "mai")))
  (cl-assert (not (ol-does-branch-exist "ain")))
  (cl-assert (not (ol-does-branch-exist "random"))))

(defun msk-merge-base-with-main ()
  (msk-merge-base (ol-main-branch) "HEAD"))

(provide 'ol-git)
