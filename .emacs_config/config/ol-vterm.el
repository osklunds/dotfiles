
(require 'ol-util)
(require 'ol-dired)
(require 'ol-evil)

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
(setc vterm-max-scrollback 2000)
(setc vterm-kill-buffer-on-exit nil)

(defun ol-vterm ()
  (interactive)
  (let* ((desired-name (ol-vterm-get-desired-buffer-name-from-path default-directory))
         (pred (lambda (buffer)
                 (ol-buffer-name-matches (buffer-name buffer) desired-name))))
    (if-let ((existing-buffer (seq-find pred (buffer-list))))
        (switch-to-buffer existing-buffer)
      (vterm))))

(defun ol-send-cmd-to-visible-vterm-buffers (cmd)
  (dolist (window (window-list))
    (with-current-buffer (window-buffer window)
      (when (eq major-mode 'vterm-mode)
        (vterm-send-string (concat cmd "\n"))))))

;; -----------------------------------------------------------------------------
;; Copying
;;------------------------------------------------------------------------------

(defun ol-vterm-disable-copy-mode ()
  (when (equal major-mode 'vterm-mode)
    (vterm-copy-mode -1)))

(defun ol-vterm-enable-copy-mode ()
  (when (equal major-mode 'vterm-mode)
    (vterm-copy-mode t)))

(add-hook 'evil-insert-state-entry-hook 'ol-vterm-disable-copy-mode)
(add-hook 'evil-insert-state-exit-hook 'ol-vterm-enable-copy-mode)

;; -----------------------------------------------------------------------------
;; Buffer name
;;------------------------------------------------------------------------------

(defvar-local ol-vterm-manually-renamed nil)

(defun ol-vterm-get-cwd-from-prompt (prompt)
  (file-name-as-directory (ol-regexp-group ":\\(/.*\\)$" prompt 1)))

(defun ol-vterm-get-desired-buffer-name-from-path (path)
  (ol2-get-buffer-name-from-path path "vterm"))

(defun ol-vterm-set-buffer-name (prompt)
  (unless ol-vterm-manually-renamed
    (let* ((current-name (buffer-name))
           (path (ol-vterm-get-cwd-from-prompt prompt))
           (desired-name (ol-vterm-get-desired-buffer-name-from-path path)))
      (unless (ol-buffer-name-matches current-name desired-name)
        (let ((new-name (generate-new-buffer-name desired-name)))
          (rename-buffer new-name))))))

(advice-add 'vterm--set-title :override 'ol-vterm-set-buffer-name)

(defun ol-rename-buffer ()
  (interactive)
  (setq-local ol-vterm-manually-renamed t)
  (call-interactively 'rename-buffer))

(ol-define-normal-leader-key "b r" 'ol-rename-buffer)

(defun ol-vterm-named ()
  (interactive)
  (vterm t)
  (ol-rename-buffer))

;; -----------------------------------------------------------------------------
;; Keybindings
;;------------------------------------------------------------------------------

(ol-evil-define-key 'insert vterm-mode-map 'tab 'vterm-send-tab)

(ol-evil-define-key 'insert vterm-mode-map "C-SPC" ol-normal-leader-map)

(ol-global-set-key "C-x t" 'ol-vterm)

;; Some normal state keybinds
(ol-evil-define-key 'insert vterm-mode-map "C-j" 'ivy-switch-buffer)
(ol-evil-define-key 'insert vterm-mode-map 'c-6 'evil-switch-to-windows-last-buffer)

;; Make the terminal experience more natural
(ol-evil-define-key 'insert vterm-mode-map "C-y" 'vterm-yank)
(ol-evil-define-key 'insert vterm-mode-map "C-d" 'vterm--self-insert)
(ol-evil-define-key 'insert vterm-mode-map "C-c" 'vterm--self-insert)

;; -----------------------------------------------------------------------------
;; Width
;;------------------------------------------------------------------------------

(setc vterm-min-window-width 1000)
(make-local-variable 'vterm-min-window-width)

(defun ol-vterm-set-width ()
  (interactive)
  (let ((width (read-from-minibuffer "vterm width (default: 1000): " nil nil 'read 'ol-vterm-width "1000")))
    (setq vterm-min-window-width width)))

(ol-evil-define-key 'insert vterm-mode-map "C-w" 'ol-vterm-set-width)

(provide 'ol-vterm)
