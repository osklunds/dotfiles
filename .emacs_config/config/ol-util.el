;; -*- lexical-binding: nil -*-

(require 'trace)

;; Taken from https://emacs.stackexchange.com/a/24658
(defun ol-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(defun ol-trace ()
  (interactive)
  (setq inhibit-trace nil)
  (call-interactively 'trace-function-background))

(defun ol-regexp-group (regexp string group)
  "Search STRING for REGEXP and return the match GROUP."
  (when (string-match regexp string)
    (match-string group string)))

(eval-and-compile
  (defmacro setc (var val)
    "Convenient version of customize-set-variable."
    `(customize-set-variable ',var ,val)))

(defun ol-require-external (cmd)
  (cl-assert (executable-find cmd)))

;; Copied from https://emacs.stackexchange.com/a/24602
(defun ol-disable-y-or-n-p (orig-fun &rest args)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (apply orig-fun args)))

(defun ol-inhibit-message-advice (func &rest args)
  (let ((inhibit-message t))
    (apply func args)))

;; Inpired by https://github.com/minad/cape
;; todo: still empties echo area
(eval-and-compile
  (defmacro ol-silent (&rest body)
    (declare (indent 0))
    `(let* ((old-msg (current-message))
            ;; Prevents in messages buffer - needed for both body
            ;; and restore of message in echo area
            (message-log-max nil))
       (cl-letf (;; Prevents in echo area
                 (inhibit-message t))
         ,@body)
       ;; Need to restore message manually, the above isn't enough
       (when old-msg
         (message old-msg)))))

;; -----------------------------------------------------------------------------
;; Window/buffer changes
;;------------------------------------------------------------------------------

(defvar ol-window-or-buffer-change-hook nil)

(defun ol-window-or-buffer-change (&rest _r)
  (run-hooks 'ol-window-or-buffer-change-hook))

;; When window is changed
(add-hook 'window-selection-change-functions 'ol-window-or-buffer-change)

;; When window remains, but shows another buffer
(add-hook 'window-buffer-change-functions 'ol-window-or-buffer-change)

;; Mainly for save
(add-function :after after-focus-change-function 'ol-window-or-buffer-change)

(provide 'ol-util)
