
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

;; -----------------------------------------------------------------------------
;; Opening a terminal
;;------------------------------------------------------------------------------

(defun ol-vterm ()
  (interactive)
  (let* ((name (ol-vterm-name default-directory))
         (name (generate-new-buffer-name name)))
    (ol-vterm-named name)))

(defun ol-vterm-name (dir)
  (file-name-nondirectory (directory-file-name dir)))

(defun ol-vterm-named (name)
  (let ((vterm-buffer-name name))
    (vterm)))


;; -----------------------------------------------------------------------------
;; Keybindings
;;------------------------------------------------------------------------------

(ol-define-normal-leader-key "tt" 'ol-term-named)

;; Some normal state keybinds
(ol-evil-define-key insert vterm-mode-map "C-h" #'evil-window-left)
(ol-evil-define-key insert vterm-mode-map "C-l" #'evil-window-right)
(ol-evil-define-key insert vterm-mode-map "C-j" 'ivy-switch-buffer)
(ol-evil-define-key insert vterm-mode-map "C-6" 'evil-switch-to-windows-last-buffer)

;; Make the terminal experience more natural
(ol-evil-define-key insert vterm-mode-map "C-y" 'vterm-yank)
(ol-evil-define-key insert vterm-mode-map "C-d" 'vterm--self-insert)
(ol-evil-define-key insert vterm-mode-map "C-c" 'vterm--self-insert)
