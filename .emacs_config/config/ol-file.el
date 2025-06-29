;; -*- lexical-binding: nil -*-

(require 'ol-util)
(require 'ol-evil)

(defun ol-print-buffer-file-name ()
  (interactive)
  (message "%s" (buffer-file-name)))

(ol-define-key ol-normal-leader-map "b n" 'ol-print-buffer-file-name)

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
       (file-writable-p buffer-file-name)
       (not (file-remote-p buffer-file-name))))

(defun ol-auto-save (&rest _)
  (when (ol-save-p)
    (ol-save-silently)))

(defun ol-save-silently ()
  (ol-silent
    (save-buffer)))

(defun ol-save-on-window-or-buffer-change ()
  ;; When vidff opens/closes, it's not enough with the last buffer,
  ;; so also take the second last and some extra for margin
  ;; window-old-buffer is for the case of buffer change without window change
  (dolist (buffer (cons (window-old-buffer) (cl-subseq (buffer-list) 0 4)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (ol-auto-save)))))

(add-hook 'ol-window-or-buffer-change-hook 'ol-save-on-window-or-buffer-change)

(advice-add 'revert-buffer-quick :before #'ol-auto-save)

(save-place-mode t)

;; (run-with-timer 5 5 #'ol-auto-save)

(defvar ol-auto-save-buffer nil)
(defun ol-auto-save-timer ()
  (when (and ol-auto-save-buffer (buffer-live-p ol-auto-save-buffer))
    (with-current-buffer ol-auto-save-buffer
      (ol-auto-save)))
  (setq ol-auto-save-buffer (current-buffer)))

(run-with-timer 5 5 #'ol-auto-save-timer)


;; ---------------------------------------------------------------------------
;; Auto revert
;; ---------------------------------------------------------------------------

(global-auto-revert-mode t)
(setc global-auto-revert-non-file-buffers t)
(setc auto-revert-verbose nil)
(setc revert-without-query '(".*"))

(provide 'ol-file)
