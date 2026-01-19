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

;;;; ---------------------------------------------------------------------------
;;;; Cache issues
;;;; ---------------------------------------------------------------------------

(defun ol-anzu--use-result-cache-p (func &rest args)
  (and anzu--cached-positions (apply func args)))

(advice-add 'anzu--use-result-cache-p :around 'ol-anzu--use-result-cache-p)

(defun ol-anzu-reset-cache (&rest _)
  (setq anzu--cached-positions nil))

(add-hook 'ol-window-or-buffer-change-hook 'ol-anzu-reset-cache)

;;;; ---------------------------------------------------------------------------
;;;; Case issues test test TEST
;;;; ---------------------------------------------------------------------------

(defvar ol-anzu-search-term nil)

(defun ol-anzu--search-all-position-advice (str)
  (setq ol-anzu-search-term str))

(advice-add 'anzu--search-all-position :before #'ol-anzu--search-all-position-advice)

(defun ol-anzu--case-fold-search ()
  (eq (evil-ex-regex-case
       (or ol-anzu-search-term "") evil-ex-search-case)
      'insensitive))

;;;;;; -------------------------------------------------------------------------
;;;;;; Copied code from anzu.el
;;;;;; -------------------------------------------------------------------------

;; Copied from anzu.el commit 26fb50b429ee968eb944b0615dd0aed1dd66172c
;; but with anzu--case-fold-search changed to ol-anzu--case-fold-search
;; The reason is that anzu--case-fold-search is defsubst which makes it
;; impossible to advice and this fix the case issue for.

(defun anzu--search-all-position (str)
  (unless anzu--last-command
    (setq anzu--last-command last-command))
  (let ((input (anzu--transform-input str)))
    (if (not (anzu--validate-regexp input))
        anzu--cached-positions
      (save-excursion
        (goto-char (point-min))
        (let ((positions '())
              (count 0)
              (overflow nil)
              (finish nil)
              (search-func (if (anzu--use-migemo-p)
                               (lambda (word &optional bound noerror count)
                                 (with-no-warnings
                                   (migemo-forward word bound noerror count)))
                             #'re-search-forward))
              (case-fold-search (ol-anzu--case-fold-search)))
          (while (and (not finish) (funcall search-func input nil t))
            (push (cons (match-beginning 0) (match-end 0)) positions)
            (cl-incf count)
            (when (= (match-beginning 0) (match-end 0)) ;; Case of anchor such as "^"
              (if (eobp)
                  (setq finish t)
                (forward-char 1)))
            (when (and anzu-search-threshold (>= count anzu-search-threshold))
              (setq overflow t finish t)))
          (let ((result (anzu--construct-position-info count overflow (reverse positions))))
            (setq anzu--cached-positions (copy-sequence result))
            result))))))

(defun anzu--count-and-highlight-matched (buf str replace-beg replace-end
                                              use-regexp overlay-limit case-sensitive)
  (anzu--cleanup-markers)
  (setq str (anzu--convert-for-lax-whitespace str use-regexp))
  (if (not (anzu--validate-regexp str))
      anzu--cached-count
    (with-current-buffer buf
      (save-excursion
        (let* ((backward (> replace-beg replace-end))
               (overlay-beg (if backward (max replace-end overlay-limit) replace-beg))
               (overlay-end (if backward replace-beg (min replace-end overlay-limit))))
          (goto-char replace-beg)
          (let ((count 0)
                (overlayed 0)
                (finish nil)
                (cmp-func (if backward #'< #'>))
                (search-func (if backward #'re-search-backward #'re-search-forward))
                (step (if backward -1 1))
                (case-fold-search (if case-sensitive
                                      nil
                                    (ol-anzu--case-fold-search))))
            (while (and (not finish) (funcall search-func str replace-end t))
              (if anzu--region-noncontiguous
                  (when (cl-loop for (b . e) in anzu--region-noncontiguous
                                 thereis (and (>= (point) b) (<= (point) e)))
                    (cl-incf count))
                (cl-incf count))
              (let ((beg (match-beginning 0))
                    (end (match-end 0)))
                (when (= beg end)
                  (if (eobp)
                      (setq finish t)
                    (forward-char step)))
                (when (and replace-end (funcall cmp-func (point) replace-end))
                  (setq finish t))
                (when (and (not finish) (anzu2--put-overlay-p beg end overlay-beg overlay-end))
                  (cl-incf overlayed)
                  (anzu--add-overlay beg end))))
            (setq anzu--cached-count count)
            overlayed))))))

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
