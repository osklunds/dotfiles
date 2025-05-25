;; -*- lexical-binding: nil -*-

(require 'ol-util)
(require 'ol-evil)

(require 'xref)
(require 'tramp-cmds)

;; Setting to nil can supposedly can improve scroll performance
;; But setting to t means delete overlays in vdiff can be split both at
;; top and bottom!
(setq auto-window-vscroll t)

(setq kill-buffer-query-functions nil)
(setc confirm-kill-processes nil)

(setc enable-local-variables nil)

(ol-define-key ol-override-map "M-u" 'universal-argument)
(ol-define-key ol-override-map "M-h" 'help-command)

(ol-evil-define-key 'normal global-map "g r" 'revert-buffer-quick)

(ol-define-key evil-motion-state-map "o" 'push-button)

(ol-define-key ol-normal-leader-map "m m" 'toggle-frame-maximized)

(setq find-function-C-source-directory "/home/oskar/others_repos/emacs/src")

(defun ol-clean-tramp ()
  (interactive)
  (tramp-cleanup-all-connections)
  (tramp-cleanup-all-buffers))

(ol-define-key ol-normal-leader-map "m t" #'ol-clean-tramp)

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
(defun ol-shell-command-todo (command)
  (interactive)
  (let ((bfn (generate-new-buffer-name (concat "*Shell Command Output: '" command "'*"))))
    (shell-command command bfn)))

(ol-global-set-key "M-!" 'shell-command)

;; -----------------------------------------------------------------------------
;; Help mode
;; -----------------------------------------------------------------------------

;; todo: doesn't work
(ol-evil-define-key 'normal help-mode-map "o" #'push-button)

;; -----------------------------------------------------------------------------
;; imenu
;; -----------------------------------------------------------------------------

(setc imenu-flatten 'prefix)
(setc imenu-auto-rescan t)
(setc imenu-max-item-length 200)

(ol-define-key ol-normal-leader-map "m s" #'imenu)

;; -----------------------------------------------------------------------------
;; åäö
;; -----------------------------------------------------------------------------
;; This is what I wished I had everywhere in my computer

(ol-define-key evil-insert-state-map "M-["  (lambda () (interactive) (insert "å")))
(ol-define-key evil-insert-state-map "M-'"  (lambda () (interactive) (insert "ä")))
(ol-define-key evil-insert-state-map "M-;"  (lambda () (interactive) (insert "ö")))
(ol-define-key evil-insert-state-map "M-{"  (lambda () (interactive) (insert "Å")))
(ol-define-key evil-insert-state-map "M-\"" (lambda () (interactive) (insert "Ä")))
(ol-define-key evil-insert-state-map "M-:"  (lambda () (interactive) (insert "Ö")))
;; -----------------------------------------------------------------------------
;; xref
;; -----------------------------------------------------------------------------

(ol-evil-define-key 'normal xref--xref-buffer-mode-map "o" #'xref-goto-xref)

(provide 'ol-misc)
