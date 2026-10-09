;; -*- lexical-binding: t -*-

(require 'icomplete)
(require 'delsel) ;; for minibuffer-keyboard-quit

(require 'ol-completion-style)

;; -----------------------------------------------------------------------------
;; Basic settings
;; -----------------------------------------------------------------------------

(icomplete-vertical-mode t)
(setq icomplete-scroll t)
(setc icomplete-show-matches-on-no-input t)
(setc icomplete-compute-delay 0)
(setc icomplete-max-delay-chars 0)
(setc resize-mini-windows 'grow-only)
(setc icomplete-prospects-height 20)

;; -----------------------------------------------------------------------------
;; Keybinds
;; -----------------------------------------------------------------------------

;; Otherwise minibuffer-complete-word is called
(ol-define-key minibuffer-local-completion-map "SPC" nil)

(ol-define-key minibuffer-mode-map
               "C-n" #'minibuffer-keyboard-quit)

;; Need for icomplete too to override default keybind
(ol-define-key icomplete-vertical-mode-minibuffer-map
               "C-n" #'minibuffer-keyboard-quit)

(ol-define-key icomplete-vertical-mode-minibuffer-map
               "C-j" #'ol-icomplete-forward)

(ol-define-key icomplete-vertical-mode-minibuffer-map
               "M-j" #'ol-icomplete-forward-many)

(ol-define-key icomplete-vertical-mode-minibuffer-map
               "C-k" #'ol-icomplete-backward)

(ol-define-key icomplete-vertical-mode-minibuffer-map
               "M-k" #'ol-icomplete-backward-many)

(ol-define-key icomplete-vertical-mode-minibuffer-map
               'tab #'ol-icomplete-dwim-tab)

(ol-define-key icomplete-vertical-mode-minibuffer-map
               'return #'ol-icomplete-dwim-return)

(ol-define-key icomplete-vertical-mode-minibuffer-map
               "C-d" #'ol-icomplete-delete-action)

(ol-define-key icomplete-vertical-mode-minibuffer-map
               "M-i" #'ol-icomplete-insert-current-selection)

(ol-define-key icomplete-vertical-mode-minibuffer-map
               "DEL" #'ol-icomplete-dwim-del)

;; Don't want C-h to open help, and bind it to something useful instead
(ol-define-key icomplete-vertical-mode-minibuffer-map
               "C-h" #'ol-icomplete-dumb-del)

(ol-define-key icomplete-vertical-mode-minibuffer-map
               "~" #'ol-icomplete-dwim-tilde)

;; To prevent help menu from opening
(ol-define-key icomplete-vertical-mode-minibuffer-map
               "C-h" (lambda () (interactive)))

(defun ol-icomplete-forward ()
  (interactive)
  (unless (icomplete-forward-completions)
    (icomplete-vertical-goto-first)))

(defun ol-icomplete-forward-many ()
  (interactive)
  (dotimes (_ 10)
    (ol-icomplete-forward)))

(defun ol-icomplete-backward ()
  (interactive)
  (unless (icomplete-backward-completions)
    (icomplete-vertical-goto-last)))

(defun ol-icomplete-backward-many ()
  (interactive)
  (dotimes (_ 10)
    (ol-icomplete-backward)))

(defun ol-icomplete-dwim-tab ()
  "Exit with currently selected candidate. However, for `find-file' and the
likes, only exit if the current candidate is a file. If e.g. a directory or
tramp method, insert it instead."
  (interactive)
  (let* ((selection (ol-icomplete-current-selection)))
    (if (and (eq minibuffer-completion-table 'read-file-name-internal)
             ;; If no selection, it means no match, so use
             ;; icomplete-force-complete-and-exit instead to allow exit
             ;; without match
             selection
             (not (string= "./" selection))
             (or (directory-name-p selection)
                 (string-match-p ":$" selection)))
        (icomplete-force-complete)
      (icomplete-force-complete-and-exit))))

(defun ol-icomplete-dwim-return ()
  "Exit with current input."
  (interactive)
  (exit-minibuffer))

(defun ol-icomplete-insert-current-selection ()
  "Insert currently selected candidate."
  (interactive)
  (icomplete-force-complete))

(defun ol-icomplete-dumb-del ()
  (interactive)
  (delete-char -1))

(defun ol-icomplete-dwim-del ()
  "Delete char in minibuffer entry. However, for `find-file' and the likes,
if the entry ends with a directory separator, delete until the next directory
separator."
  (interactive)
  (if (and (eq minibuffer-completion-table 'read-file-name-internal)
           (directory-name-p (icomplete--field-string)))
      (progn
        (kill-region (point)
                     (progn
                       (search-backward "/" nil t 2)
                       (1+ (point))))
        (end-of-line))
    (ol-icomplete-dumb-del)))

(defun ol-icomplete-dwim-tilde ()
  (interactive)
  (if (and (eq minibuffer-completion-table 'read-file-name-internal)
           (directory-name-p (icomplete--field-string)))
      (insert (expand-file-name "~/"))
    (insert "~")))

(defun ol-icomplete-delete-action ()
  (interactive)
  (when-let* ((delete-action (ol-completion-metadata-get 'ol-delete-action)))
    (let ((selected (ol-icomplete-current-selection)))
      (when (funcall delete-action selected)
        (let* ((completions (ol-nmake-proper-list completion-all-sorted-completions))
               ;; Note: completions only contains the selected candidate and
               ;; the candidates behind, which is a bit surprising, but
               ;; it works for this use case.
               (was-last (length= completions 1))
               (new-completions (remove selected completions)))
          
          (setq completion-all-sorted-completions (append new-completions 0))
          (setq icomplete--scrolled-completions
                (remove selected icomplete--scrolled-completions))
          (icomplete-exhibit)
          (when was-last
            (ol-icomplete-backward)))))))

(defun ol-icomplete-current-selection ()
  (or (car icomplete--scrolled-completions)
      ;; If no scroll yet
      (car (completion-all-sorted-completions))))

;; -----------------------------------------------------------------------------
;; History
;; -----------------------------------------------------------------------------

;; Preferably I only want input in history, but for eval-expression,
;; icomplete isn't used, so log both input and selection
(setc history-add-new-input t)

(defun ol-add-input-to-minibuffer-history (&rest _)
  (let* ((input (minibuffer-contents-no-properties)))
    (add-to-history minibuffer-history-variable input)))

(advice-add 'icomplete-force-complete-and-exit :before
            #'ol-add-input-to-minibuffer-history)

(advice-add 'icomplete-force-complete :before
            #'ol-add-input-to-minibuffer-history)

;; -----------------------------------------------------------------------------
;; Highlight to EOL
;; -----------------------------------------------------------------------------

;; Not ideal to depend on internal 'icomplete-selected text property.
;; But at least, this functionality of highlight to end isn't too critical
(defun ol-icomplete--render-vertical-highlight-to-end (return)
  (let* ((lines (split-string return "\n"))
         (selected nil))
    (dolist (line lines)
      (when (get-text-property 0 'icomplete-selected line)
        (setq selected line)))
    (when selected
      (string-match (format "%s.*\n" (regexp-quote selected)) return)
      (let* ((m (match-data))
             (start (car m))
             (end (cadr m)))
        (add-face-text-property start end 'icomplete-selected-match nil return)))
    return))

(advice-add 'icomplete--render-vertical :filter-return
            #'ol-icomplete--render-vertical-highlight-to-end)

;;;; ---------------------------------------------------------------------------
;;;; Editable collection/grep buffer
;;;; ---------------------------------------------------------------------------

;; Inspired by wgrep

(defun ol-grep-read-only-mode ()
  (interactive)
  (unless (eq major-mode 'grep-mode)
    (user-error "Only works in grep-mode"))
  (if buffer-read-only
      (progn
        (use-local-map (make-sparse-keymap))
        (local-set-key (kbd "C-x C-q") #'ol-grep-read-only-mode)
        (read-only-mode -1))
    (ol-grep-apply-changes)
    (use-local-map grep-mode-map)
    (read-only-mode t)))

(defun ol-grep-apply-changes ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when-let* ((props (text-properties-at (point)))
                  (msg (plist-get props 'compilation-message))
                  (loc (compilation--message->loc msg))
                  (file-name (caar (compilation--loc->file-struct loc)))
                  (line-num (compilation--loc->line loc))
                  (line-text (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))))
        (when (string-match "^[^ ]+ [0-9]+:\\(.+\\)$" line-text)
          (let ((new-content (match-string 1 line-text)))
            ;; todo: don't run mode hooks etc
            (with-current-buffer (find-file-noselect file-name)
              (save-excursion
                (goto-char (point-min))
                (forward-line (1- line-num))
                (delete-region (line-beginning-position) (line-end-position))
                (insert new-content))
              (ol-save-silently)))))
      (forward-line 1)))
  (message "Applied grep changes"))

(provide 'ol-icomplete)
