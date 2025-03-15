
(require 'ol-evil)

(require 'isearch)

(setc isearch-wrap-pause 'no)
(setc isearch-repeat-on-direction-change t)
(setc isearch-lazy-count t)
(setc lazy-highlight-cleanup nil)
(setc lazy-highlight-initial-delay 0)
(setc lazy-highlight-buffer t)
(setc search-nonincremental-instead nil)

(defun ol-isearch-count-update ()
  (clrhash isearch-lazy-count-hash)
  (setq isearch-lazy-count-total 0)
  (let ((isearch-lazy-highlight-last-string "set"))
    (isearch-lazy-highlight-buffer-update))
  (message "oskar: %s" isearch-lazy-count-hash))

(defun ol-start-search ()
  (interactive)
  (isearch-forward))

(ol-define-key evil-normal-state-map "\\" 'ol-start-search)

(ol-define-key isearch-mode-map "n" 'isearch-repeat-forward)

(defun ol-switch-to-nonincremental-search ()
  (interactive)
  (isearch-done)
  (isearch-exit)
  )

(defun ol-stop-search ()
  (interactive)
  (lazy-highlight-cleanup t)
  (isearch-exit))

(ol-define-key evil-normal-state-map "|" 'ol-stop-search)

(defun ol-isearch-no-mark-saved-message (func &rest args)
  (let ((inhibit-message t))
    (apply func args)))

(setq inhibit-message nil)

(advice-add 'isearch-done :around 'ol-isearch-no-mark-saved-message)

(provide 'ol-search)
