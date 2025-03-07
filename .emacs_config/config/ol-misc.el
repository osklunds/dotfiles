
(require 'ol-util)

(setc enable-local-variables nil)

(defun ol-window-setup-hook ()
  (toggle-frame-maximized))

(add-hook 'window-setup-hook 'ol-window-setup-hook)

(ol-override-key "M-:" 'eval-expression)
(ol-override-key "M-u" 'universal-argument)
(ol-override-key "M-h" 'help-command)

(ol-evil-define-key 'normal global-map "gr" 'revert-buffer-quick)

(ol-define-key evil-motion-state-map "o" 'push-button)

(ol-define-normal-leader-key "mm" 'toggle-frame-maximized)

(defvar ol-before-plain-view nil)

(defun ol-plain-view ()
  "To make copy paste from non-GUI emacs simpler"
  (interactive)
  (if ol-before-plain-view
      (progn
        (pcase-let ((`(,window-config ,mode) ol-before-plain-view))
          (set-window-configuration window-config)
          (funcall mode)
          (display-line-numbers-mode 1))
        (setq ol-before-plain-view nil))
    (let ((window-config (current-window-configuration))
          (mode major-mode))
      (setq ol-before-plain-view (list window-config mode))
      (delete-other-windows)
      (text-mode)
      (display-line-numbers-mode -1))))

(ol-define-normal-leader-key "mp" 'ol-plain-view)

(defun ol-toggle-spelling ()
  (interactive)
  (unless flyspell-mode
    (flyspell-buffer))
  (call-interactively 'flyspell-mode))

(ol-define-normal-leader-key "sc" 'ol-toggle-spelling)

(setc ispell-check-comments 'exclusive)

(defun ol-shell-command (command)
  (interactive)
  (let ((bfn (generate-new-buffer-name (concat "*Shell Command Output: '" command "'*"))))
    (shell-command command bfn)))

(ol-global-set-key "M-!" 'shell-command)

;; Supposedly can improve scroll performance
(setq auto-window-vscroll nil)

;; -----------------------------------------------------------------------------
;; Window/buffer changes
;;------------------------------------------------------------------------------
;; todo: remove

(defvar ol-window-buffer-change-old-hook nil)
(defvar ol-window-buffer-change-new-hook nil)

(defun ol-window-buffer-change-old (&rest _r)
  (run-hooks 'ol-window-buffer-change-old-hook))

(defun ol-window-buffer-change-new (&rest _r)
  (run-hooks 'ol-window-buffer-change-new-hook))

(add-hook 'window-selection-change-functions 'ol-window-buffer-change-new)

;; window-selection-change-functions is actually run after, then it's too late,
;; so need these too
(dolist (cmd '(switch-to-buffer
               other-window
               windmove-up
               windmove-down
               windmove-left
               windmove-right
               next-buffer
               previous-buffer
               read-from-minibuffer
               display-buffer
               ))
  (advice-add cmd :before 'ol-window-buffer-change-old)
  (advice-add cmd :after 'ol-window-buffer-change-new))


(provide 'ol-misc)
