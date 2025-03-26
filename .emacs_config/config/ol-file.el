
(require 'ol-util)
(require 'ol-evil)

(require 'counsel)

(defun ol-print-buffer-file-name ()
  (interactive)
  (message "%s" (buffer-file-name)))

(ol-define-key ol-normal-leader-map "b n" 'ol-print-buffer-file-name)

(defun ol-find-file-empty ()
  (interactive)
  ;; Need to override major-mode because if dired, counsel ignores initial dir
  (let ((major-mode 'fundamental-mode))
    (counsel-find-file nil "/")))

(ol-global-set-key "C-x f" 'ol-find-file-empty)

(setc large-file-warning-threshold (* 100 1000 1000))

;; ---------------------------------------------------------------------------
;; Backup
;; ---------------------------------------------------------------------------

;; No ~ files
(setc make-backup-files nil)

;; To prevent stutter when auto-saving. I use super-save and git to compensate
(setc auto-save-default nil)

;; ---------------------------------------------------------------------------
;; Save
;; ---------------------------------------------------------------------------
;; Inspired by super-save https://github.com/bbatsov/super-save

(defun ol-save-p ()
  (and buffer-file-name
       (buffer-modified-p (current-buffer))
       (file-writable-p buffer-file-name)))

(defun ol-save-silently ()
  (let ((inhibit-message t))
    (save-buffer)))

(defun ol-save-on-window-or-buffer-change ()
  ;; When vidff opens/closes, it's not enough with the last buffer,
  ;; so also take the second last and some extra for margin
  ;; window-old-buffer is for the case of buffer change without window change
  (dolist (buffer (cons (window-old-buffer) (cl-subseq (buffer-list) 0 4)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (ol-save-p)
          (ol-save-silently))))))

(add-hook 'ol-window-or-buffer-change-hook 'ol-save-on-window-or-buffer-change)

(save-place-mode t)

;; ---------------------------------------------------------------------------
;; Auto revert
;; ---------------------------------------------------------------------------

(global-auto-revert-mode t)
(setc global-auto-revert-non-file-buffers t)
(setc auto-revert-verbose nil)
(setc revert-without-query '(".*"))

(provide 'ol-file)
