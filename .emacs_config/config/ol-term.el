
;; This file is deprecated now that I use vterm.

;; Notes for myself on terminals
;; You can only edit text in either line mode or char mode - never mixed. So
;; workflows could look like
;; Line mode: type text in the prompt and jump around using normal movement ops.
;; When you're done, press enter. One caveat is that I have to make evil insert
;; not enter char mode for this.
;; Char mode: like a regular terminal. Note, you can still use normal mode for
;; read only to other parts of the shell, then go to insert mode and paste using
;; C-y.
;; To begin with, I'll stick to char mode, as it's the most similar to
;; terminals I'm used to.
;; For line mode, shell works better than term/ansi-term. In shell, company mode
;; works but not in term.

(require 'term)

(defun ol-disable-cursorline-for-terms ()
  (when (equal major-mode 'term-mode)
    (setq global-hl-line-mode nil)))

(defun ol-enable-cursorline-for-terms ()
  (when (equal major-mode 'term-mode)
    (setq global-hl-line-mode t)))

(add-hook 'evil-insert-state-entry-hook 'ol-disable-cursorline-for-terms)
(add-hook 'evil-insert-state-exit-hook 'ol-enable-cursorline-for-terms)

(defun ol-set-term-buffer-maximum-size ()
  (setc term-buffer-maximum-size 10000000000))

(add-hook 'term-mode-hook 'ol-set-term-buffer-maximum-size)

;; Hack to do it like this. If done directly, colors aren't set it seems
(defun ol-set-term-colors ()
  ;; TODO Do this, setting all colors:
  ;; https://emacs.stackexchange.com/questions/28825/how-do-you-set-colors-for-term
  (ol-set-face 'term-color-black :foreground ol-black :background ol-white)
  (ol-set-face 'term :foreground ol-black :background ol-white))

(add-hook 'term-mode-hook 'ol-set-term-colors)

;; Some normal state keybinds
(ol-evil-define-key 'insert term-raw-map "C-j" 'ivy-switch-buffer)
(ol-evil-define-key 'insert term-raw-map 'c-6 'evil-switch-to-windows-last-buffer)

;; Make the terminal experience more natural
(ol-evil-define-key 'insert term-raw-map "C-y" 'term-paste)
(ol-evil-define-key 'insert term-raw-map "C-d" 'term-send-raw)
(ol-evil-define-key 'insert term-raw-map "C-c" 'term-send-raw)

(ol-evil-define-key 'insert term-raw-map 'tab (lambda ()
                                                (interactive)
                                                (term-send-raw-string "\t")))
(ol-evil-define-key 'insert shell-mode-map 'tab 'completion-at-point)

;; C-pnbf seem to more reliable in terminals in emacs, so remap arrow keys
(defmacro ol-define-term-key (from to)
  `(ol-evil-define-key 'insert
                       term-raw-map
                       ,from
                       (lambda () (interactive) (term-send-raw-string (kbd ,to)))))

(ol-define-term-key "<up>"    "C-p")
(ol-define-term-key "<down>"  "C-n")
(ol-define-term-key "<left>"  "C-b")
(ol-define-term-key "<right>" "C-f")

;;;; ---------------------------------------------------------------------------
;;;; Functions for opening a terminal
;;;;----------------------------------------------------------------------------

(defun ol-term ()
  (interactive)
  (ansi-term shell-file-name))

(defun ol-term-named (name &optional cmd-on-create)
  (interactive (list (read-string "Name: " nil nil "terminal")))
  (let* ((existing-buffer (get-buffer name))
         (new-buffer (if existing-buffer
                         existing-buffer
                       (vterm name))))
    (switch-to-buffer new-buffer)
    (with-current-buffer new-buffer
      (setq-local ol-vterm-manually-renamed t)
      (when (and cmd-on-create (not existing-buffer))
        (vterm-send-string (concat cmd-on-create "\n"))))
    new-buffer))

(ol-define-normal-leader-key "tt" 'ol-term-named)

(setq kill-buffer-query-functions nil)
(setc confirm-kill-processes nil)

;;;; ---------------------------------------------------------------------------
;;;; Long lines
;;;;----------------------------------------------------------------------------

(defun ol-window-max-chars-per-line (oldfun &optional window face)
  (let* ((buffer (window-buffer window))
         (mm (with-current-buffer (window-buffer window) major-mode)))
    (if (eq mm 'term-mode)
        (progn
          ;; (message "Changing window-max-chars for term-mode window: %s" window)
          1000)
      ;; (message "Leaving normal window-max-chars for non term-mode window: %s" window)
      (apply oldfun (list window face)))))

(advice-add 'window-max-chars-per-line :around 'ol-window-max-chars-per-line)

(provide 'ol-term)
