
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

(defun ol-vterm ()
  (interactive)
  (let ((desired-name (ol-vterm-get-desired-buffer-name-from-cwd default-directory))
        (pred (lambda (buffer)
                (ol-vterm-buffer-name-matches (buffer-name buffer) desired-name))))
    (if-let ((existing-buffer (seq-find pred (buffer-list))))
        (switch-to-buffer existing-buffer)
      (vterm))))

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
  (concat (ol-regexp-group ":\\(/.*\\)$" prompt 1) "/"))

(defun ol-vterm-get-desired-buffer-name-from-path (path)
  (ol-get-buffer-name-from-path ("vterm" path)))

(defun ol-vterm-buffer-name-matches (name desired-name)
  (let ((regexp (concat "^" (regexp-quote desired-name) "\\(<[0-9]>\\)?$")))
    (string-match-p regexp name)))

(cl-assert (ol-vterm-buffer-name-matches "some-name" "some-name"))
(cl-assert (ol-vterm-buffer-name-matches "some-name<2>" "some-name"))
(cl-assert (not (ol-vterm-buffer-name-matches "some-name-more" "some-name")))
(cl-assert (not (ol-vterm-buffer-name-matches "some-name" "some-name-more")))

(defun ol-vterm-set-buffer-name (prompt)
  (unless ol-vterm-manually-renamed
    (let* ((current-name (buffer-name))
           (path (ol-vterm-get-cwd-from-prompt prompt))
           (desired-name (ol-vterm-get-desired-buffer-name-from-path path)))
      (unless (ol-vterm-buffer-name-matches current-name desired-name)
        (let ((new-name (generate-new-buffer-name desired-name)))
          (rename-buffer new-name))))))

(advice-add 'vterm--set-title :override 'ol-vterm-set-buffer-name)

(defun ol-rename-buffer ()
  (interactive)
  (setq-local ol-vterm-manually-renamed t)
  (call-interactively 'rename-buffer))

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
