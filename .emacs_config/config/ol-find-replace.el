;; -*- lexical-binding: nil -*-

(require 'ol-util)
(require 'ol-basic-user-interface)
(require 'ol-evil)

;; -----------------------------------------------------------------------------
;; Replace commands
;; -----------------------------------------------------------------------------

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
;; Search visual selection
;; -----------------------------------------------------------------------------

;; @credits https://github.com/bling/evil-visualstar.git
;; Copied/inspired from evil-visualstar, but simplified

(defun ol-begin-visual-search (beg end)
  (when (evil-visual-state-p)
    (evil-exit-visual-state)
    (let* ((selection (regexp-quote (buffer-substring-no-properties beg end)))
           (pattern (evil-ex-make-search-pattern selection)))
      (setq evil-ex-search-direction 'forward)
      (setq evil-ex-search-pattern pattern)
      (evil-ex-search-activate-highlight pattern))))

(evil-define-motion ol-begin-visual-search-motion (beg end)
  ""
  :jump t
  :repeat nil
  (interactive "<r>")
  (ol-begin-visual-search beg end))

(ol-define-key evil-visual-state-map "*" #'ol-begin-visual-search-motion)

;; -----------------------------------------------------------------------------
;; Number of search hits
;; -----------------------------------------------------------------------------

;; @credits https://github.com/emacsorphanage/anzu.git
;; @credits https://github.com/emacsorphanage/evil-anzu.git
;; Inspired by the above

(defun ol-search-count (string)
  (let* ((here (point))
         (total 0)
         (current 0)
         (case-fold-search (ol-case-fold string)))
    (save-excursion
      (goto-char (point-min))
      ;; For performance, can consider to skip regex
      (while (search-forward-regexp string nil t)
        (setq total (1+ total))
        (when (and (zerop current) (>= (match-end 0) here))
          (setq current total))))
    (cons current total)))

;; Fold means ignore case
(defun ol-case-fold (string)
  (eq (evil-ex-regex-case string evil-ex-search-case) 'insensitive))

(defun ol-search-count-evil-ex-search-pattern ()
  (let* ((string (evil-ex-pattern-regex evil-ex-search-pattern)))
    (ol-search-count string)))

;;;; ---------------------------------------------------------------------------
;;;; Caching the count
;;;; ---------------------------------------------------------------------------

(defvar ol-search-count-cache nil)

(defun ol-search-count-cached ()
  (or ol-search-count-cache
      (setq ol-search-count-cache (ol-search-count-evil-ex-search-pattern))))

(defun ol-invalidate-search-count-cache ()
  (setq ol-search-count-cache nil))

(add-hook 'ol-window-or-buffer-change-hook #'ol-invalidate-search-count-cache)

;; test
;; test
;; TEST
;; testabc

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
  (ol-invalidate-search-count-cache)
  (ol-update-evil-search-visible-buffers))

(advice-add 'evil-ex-start-search :after 'ol-evil-start-search-advice)
(advice-add 'evil-ex-start-word-search :after 'ol-evil-start-search-advice)
(advice-add 'evil-ex-search-next :after 'ol-evil-start-search-advice)
(advice-add 'evil-ex-search-previous :after 'ol-evil-start-search-advice)
(advice-add 'ol-begin-visual-search :after 'ol-evil-start-search-advice)

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

;; -----------------------------------------------------------------------------
;; Regex search
;; -----------------------------------------------------------------------------

(defvar ol-evil-regex-search t)

(defun ol-toggle-evil-regex-search ()
  (interactive)
  (setq ol-evil-regex-search (not ol-evil-regex-search))
  (message "ol-evil-regex-search: %s" ol-evil-regex-search))

(ol-define-key ol-normal-leader-map "s r" #'ol-toggle-evil-regex-search)

(defun ol-evil-ex-make-pattern-advice (org-fun regex &rest args)
  (let ((escaped-regex (if ol-evil-regex-search regex (regexp-quote regex))))
    (apply org-fun escaped-regex args)))

(advice-add 'evil-ex-make-pattern :around #'ol-evil-ex-make-pattern-advice)

;; -----------------------------------------------------------------------------
;; Case sensitive
;; -----------------------------------------------------------------------------

(defun ol-toggle-evil-ex-search-case ()
  (interactive)
  (setq evil-ex-search-case (if (eq evil-ex-search-case 'smart) 'sensitive 'smart))
  (message "evil-ex-search-case: %s" evil-ex-search-case))

(ol-define-key ol-normal-leader-map "s s" #'ol-toggle-evil-ex-search-case)

(provide 'ol-find-replace)
