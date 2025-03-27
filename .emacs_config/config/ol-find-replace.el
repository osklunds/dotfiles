;; -*- lexical-binding: nil -*-

(require 'ol-util)
(require 'ol-basic-user-interface)
(require 'ol-evil)

(require 'evil-visualstar)
(require 'anzu)
(require 'evil-anzu)

(ol-set-face 'lazy-highlight :background "#ffff5f" :foreground ol-black)

;; -----------------------------------------------------------------------------
;; Replace commands
;; -----------------------------------------------------------------------------

(global-evil-visualstar-mode)

(defconst ol-full-range "%")
(defconst ol-from-here-range ",$")

(defun ol-full-replace-visual-selection ()
  (interactive)
  (ol-replace-visual-selection ol-full-range))

(ol-define-key ol-visual-leader-map "R" 'ol-full-replace-visual-selection)

(defun ol-from-here-replace-visual-selection ()
  (interactive)
  (ol-replace-visual-selection ol-from-here-range))

(ol-define-key ol-visual-leader-map "r" 'ol-from-here-replace-visual-selection)

(defun ol-full-replace-symbol ()
  (interactive)
  (ol-replace-symbol ol-full-range))

(ol-define-key ol-normal-leader-map "R" 'ol-full-replace-symbol)

(defun ol-from-here-replace-symbol ()
  (interactive)
  (ol-replace-symbol ol-from-here-range))

(ol-define-key ol-normal-leader-map "r" 'ol-from-here-replace-symbol)

(defun ol-replace-symbol (range)
  (let ((text (thing-at-point 'symbol 'no-properties)))
    (ol-replace-text text range)))

(defun ol-replace-visual-selection (range)
  (let ((text (buffer-substring-no-properties (mark) (point))))
    (ol-replace-text text range)))

(defun ol-replace-text (text range)
  (evil-set-jump)
  (let ((ex-command (format "%ss/%s/%s/gc" range text text)))
    (minibuffer-with-setup-hook
        (lambda () (backward-char 3))
      (evil-ex ex-command))))

;; -----------------------------------------------------------------------------
;; Number of search hits
;; -----------------------------------------------------------------------------

(global-anzu-mode t)

(setc anzu-cons-mode-line-p nil)

(defun ol-anzu--use-result-cache-p (func &rest args)
  (and anzu--cached-positions (apply func args)))

(advice-add 'anzu--use-result-cache-p :around 'ol-anzu--use-result-cache-p)

(defun ol-anzu-reset-cache (&rest _)
  (setq anzu--cached-positions nil))

(add-hook 'ol-window-or-buffer-change-hook 'ol-anzu-reset-cache)

;; Fixing case sensitive
(defun ol-anzu--case-fold-search--advice (&rest r)
  (eq (evil-ex-regex-case (nth 0 (or evil-ex-search-pattern '(""))) evil-ex-search-case)
      'insensitive))

(advice-add 'anzu--case-fold-search :override 'ol-anzu--case-fold-search--advice)

;; -----------------------------------------------------------------------------
;; Making Evil more similar to Vim
;; -----------------------------------------------------------------------------

(defvar ol-evil-is-searching nil)

(defun ol-update-evil-search (&rest _args)
  (if (and ol-evil-is-searching (not (eq major-mode 'minibuffer-mode)))
      (evil-ex-search-activate-highlight evil-ex-search-pattern)
    (evil-ex-nohighlight)))

(defun ol-update-evil-search-visible-buffers ()
  (dolist (window (window-list))
    (with-current-buffer (window-buffer window)
      (ol-update-evil-search))))

(add-hook 'ol-window-or-buffer-change-hook 'ol-update-evil-search)

(defun ol-evil-start-search-advice (&rest _args)
  (setq ol-evil-is-searching t)
  (ol-update-evil-search-visible-buffers))

(advice-add 'evil-ex-start-search :after 'ol-evil-start-search-advice)
(advice-add 'evil-ex-start-word-search :after 'ol-evil-start-search-advice)
(advice-add 'evil-ex-search-next :after 'ol-evil-start-search-advice)
(advice-add 'evil-ex-search-previous :after 'ol-evil-start-search-advice)

(defun ol-evil-stop-search ()
  (interactive)
  (setq ol-evil-is-searching nil)
  (ol-update-evil-search-visible-buffers))

(ol-define-key evil-motion-state-map "?" 'ol-evil-stop-search)
(ol-define-key evil-insert-state-map "M-/" 'ol-evil-stop-search)

;; -----------------------------------------------------------------------------
;; Don't move for first search
;; -----------------------------------------------------------------------------

(defun ol-dont-move-advice (func &rest args)
  (save-excursion
    (apply func args)
    (evil-ex-search-previous)))

(advice-add 'evil-ex-start-word-search :around 'ol-dont-move-advice)
(advice-add 'evil-visualstar/begin-search :around 'ol-dont-move-advice)

(provide 'ol-find-replace)
