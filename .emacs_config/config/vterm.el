
;; vterm is kept in a separate file until my experiments are done

;; -----------------------------------------------------------------------------
;; General
;;------------------------------------------------------------------------------

(require 'vterm)

(defun ol-vterm-disable-cursorline ()
  (when (equal major-mode 'vterm-mode)
      (setq global-hl-line-mode nil)))

(defun ol-vterm-enable-cursorline ()
  (when (equal major-mode 'vterm-mode)
    (setq global-hl-line-mode t)))

(add-hook 'evil-insert-state-entry-hook 'ol-vterm-disable-cursorline)
(add-hook 'evil-insert-state-exit-hook 'ol-vterm-enable-cursorline)

(add-to-list 'evil-insert-state-modes 'vterm-mode)
(setc vterm-max-scrollback 100000)

;; -----------------------------------------------------------------------------
;; Buffer name
;;------------------------------------------------------------------------------

;; TODO: Doing M-q for finding files doesn't work correctly in vterm buffers

(defvar-local ol-vterm-manually-renamed nil)

(defun ol-vterm-set-title (title)
  (unless ol-vterm-manually-renamed
    (let ((new-title (concat "term: " (ol-regexp-group ":\\(/.*\\)$" title 1))))
      (rename-buffer (string-truncate-left new-title 50)))))
;; TODO: Add dired prefix to dired buffers

(advice-add 'vterm--set-title :override 'ol-vterm-set-title)

(defun ol-rename-buffer ()
  (interactive)
  (setq-local ol-vterm-manually-renamed t)
  (call-interactively 'rename-buffer))

;; -----------------------------------------------------------------------------
;; Keybindings
;;------------------------------------------------------------------------------

(ol-evil-define-key insert vterm-mode-map "C-SPC" ol-normal-leader-map)

(ol-global-set-key "C-x t" 'vterm)

;; Some normal state keybinds
(ol-evil-define-key insert vterm-mode-map "C-j" 'ivy-switch-buffer)
(ol-evil-define-key insert vterm-mode-map "C-6" 'evil-switch-to-windows-last-buffer)

;; Make the terminal experience more natural
(ol-evil-define-key insert vterm-mode-map "C-y" 'vterm-yank)
(ol-evil-define-key insert vterm-mode-map "C-d" 'vterm--self-insert)
(ol-evil-define-key insert vterm-mode-map "C-c" 'vterm--self-insert)
