;; -*- lexical-binding: nil -*-

(require 'ol-util)
(require 'ol-evil)

(require 'xref)
(require 'tramp-cmds)
(require 'profiler)

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
(setc imenu-space-replacement " ")

(defun ol-imenu ()
  "Variant of `imenu' that preserves order."
  (interactive)
  (minibuffer-with-setup-hook
      (lambda ()
        (setq-local completion-extra-properties
                    '(:cycle-sort-function identity :display-sort-function identity)))
    (call-interactively #'imenu)))

(ol-define-key ol-normal-leader-map "m s" #'ol-imenu)

;; -----------------------------------------------------------------------------
;; åäö
;; -----------------------------------------------------------------------------
;; This is what I wished I had everywhere in my computer

;; Not in termnal, conflicts with brackated paste
(when (display-graphic-p)
  (ol-define-key evil-insert-state-map "M-["  (lambda () (interactive) (insert "å")))
  (ol-define-key evil-insert-state-map "M-'"  (lambda () (interactive) (insert "ä")))
  (ol-define-key evil-insert-state-map "M-;"  (lambda () (interactive) (insert "ö")))
  (ol-define-key evil-insert-state-map "M-{"  (lambda () (interactive) (insert "Å")))
  (ol-define-key evil-insert-state-map "M-\"" (lambda () (interactive) (insert "Ä")))
  (ol-define-key evil-insert-state-map "M-:"  (lambda () (interactive) (insert "Ö"))))

;; -----------------------------------------------------------------------------
;; xref
;; -----------------------------------------------------------------------------

(ol-evil-define-key 'normal xref--xref-buffer-mode-map "o" #'xref-goto-xref)

;; -----------------------------------------------------------------------------
;; Profiler
;; -----------------------------------------------------------------------------

(defun ol-profile-start ()
  (interactive)
  (when (profiler-running-p)
    (profiler-stop))
  (profiler-start 'cpu))
(ol-define-key ol-normal-leader-map "m P" #'ol-profile-start)

(defun ol-profile-report ()
  (interactive)
  (profiler-stop)
  (profiler-report))
(ol-define-key ol-normal-leader-map "m r" #'ol-profile-report)

(provide 'ol-misc)
