;; -*- lexical-binding: nil -*-

(require 'ol-util)
(require 'ol-evil)

(require 'server)

(defun ol-start-server ()
  (interactive)
  (unless (server-running-p)
    (setc server-name (ol-find-free-server-name))
    (setenv "EMACS_SERVER_NAME" server-name)
    (server-start)))

(defun ol-find-free-server-name ()
  (let* ((base-server-name "ol-server")
         (current-index 0)
         (found nil)
         (current-name nil))
    (while (not found)
      ;; If not printing, emacs doesn't start if I don't press enter
      (setq current-name (format "%s-%d" base-server-name current-index))
      (message "Trying server name: %s" current-name)
      (if (server-running-p current-name)
          (setq current-index (+ current-index 1))
        (setq found t)))
    current-name))

(ol-start-server)

(defun ol-server-done ()
  (interactive)
  (save-buffer)
  (server-done))

(ol-define-key evil-normal-state-map "C-x #" #'ol-server-done)

(provide 'ol-emacs-server)
