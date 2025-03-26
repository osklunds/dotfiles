;; -*- lexical-binding: t -*-

(require 'ol-util)
(require 'ol-evil)

;; Supposedly can improve scroll performance
(setq auto-window-vscroll nil)

(setq kill-buffer-query-functions nil)
(setc confirm-kill-processes nil)

(setc enable-local-variables nil)

(ol-define-key ol-override-map "M-:" 'eval-expression)
(ol-define-key ol-override-map "M-u" 'universal-argument)
(ol-define-key ol-override-map "M-h" 'help-command)

(ol-evil-define-key 'normal global-map "g r" 'revert-buffer-quick)

(ol-define-key evil-motion-state-map "o" 'push-button)

(ol-define-key ol-normal-leader-map "m m" 'toggle-frame-maximized)

;; -----------------------------------------------------------------------------
;; Plain view
;; -----------------------------------------------------------------------------

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

(ol-define-key ol-normal-leader-map "m p" 'ol-plain-view)

;; -----------------------------------------------------------------------------
;; Spelling
;; -----------------------------------------------------------------------------

(defun ol-toggle-spelling ()
  (interactive)
  (unless flyspell-mode
    (flyspell-buffer))
  (call-interactively 'flyspell-mode))

(ol-define-key ol-normal-leader-map "s c" 'ol-toggle-spelling)

(ol-define-key ol-normal-leader-map "s f" 'flyspell-auto-correct-word)

(setc ispell-check-comments 'exclusive)

;; -----------------------------------------------------------------------------
;; Shell command
;; -----------------------------------------------------------------------------

;; unfinished experiment
(defun ol-shell-command (command)
  (interactive)
  (let ((bfn (generate-new-buffer-name (concat "*Shell Command Output: '" command "'*"))))
    (shell-command command bfn)))

(ol-global-set-key "M-!" 'shell-command)

(provide 'ol-misc)
