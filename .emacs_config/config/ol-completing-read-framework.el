;; -*- lexical-binding: t -*-

(require 'ol-ert)
(require 'ol-colors)

(require 'icomplete)
(require 'delsel) ;; for minibuffer-keyboard-quit
(require 'grep)

(defvar ol-async-completing-read-active nil)
(defvar ol-async-buffer nil)
(defvar ol-async-candidates nil)
(defvar ol-async-timer nil)
(defvar ol-async-has-moved nil
  "If moving/scrolling in icomplete and then more candidates comes
the output is meesed up, so stop process when move.")
(defvar ol-async-goto-function nil)

;; -----------------------------------------------------------------------------
;; UI
;; -----------------------------------------------------------------------------

(icomplete-vertical-mode t)
(setq icomplete-scroll t)
(setc icomplete-show-matches-on-no-input t)
(setc icomplete-compute-delay 0)
(setc icomplete-max-delay-chars 0)
(setc resize-mini-windows 'grow-only)
(setc icomplete-prospects-height 20)

(defun ol-icomplete-forward ()
  (interactive)
  (ol-async-stop-process)
  (ol-async-stop-timer)
  (setq ol-async-has-moved t)
  (unless (icomplete-forward-completions)
    (icomplete-vertical-goto-first)))

(defun ol-icomplete-forward-many ()
  (interactive)
  (dotimes (_ 10)
    (ol-icomplete-forward)))

(defun ol-icomplete-backward ()
  (interactive)
  (ol-async-stop-process)
  (ol-async-stop-timer)
  (setq ol-async-has-moved t)
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
  ;; Can't check ol-async-buffer, because after ol-collect and restart inside
  ;; compilation buffer, ol-async-buffer is non-nil, and this function
  ;; incorrectly chooses the async clause here.
  (if ol-async-completing-read-active
      (ol-collect-1 (lambda (buffer)
                      (with-current-buffer buffer
                        (ol-async-goto-result))
                      (kill-buffer buffer)))
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
        (icomplete-force-complete-and-exit)))))

(defun ol-icomplete-dwim-return ()
  "Exit with current input."
  (interactive)
  (exit-minibuffer))

(defun ol-icomplete-insert-current-selection ()
  "Insert currently selected candidate."
  (interactive)
  (icomplete-force-complete))

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
    (delete-char -1)))

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

(set-face-attribute 'icomplete-selected-match nil
                    :background 'unspecified
                    :inherit 'ol-selection-face)

;; Not ideal to depend on internal 'icomplete-selected text property.
;; But at least, this functionality of highlight to end isn't too critical
(defun ol-icomplete--render-vertical-highlight-to-end (return)
  (let* ((lines (split-string return "\n"))
         (selected nil))
    (dolist (line lines)
      (when (get-text-property 0 'icomplete-selected line)
        (setq selected line)))
    ;; For e.g. async, there might not be anything selected
    (when selected
      (string-match (format "%s.*\n" (regexp-quote selected)) return)
      (let* ((m (match-data))
             (start (car m))
             (end (cadr m)))
        (add-face-text-property start end 'icomplete-selected-match nil return)))
    return))

(advice-add 'icomplete--render-vertical :filter-return
            #'ol-icomplete--render-vertical-highlight-to-end)

(defun ol-icomplete-insert-include-glob ()
  (interactive)
  (ol-icomplete-insert-glob "-g ''"))

(defun ol-icomplete-insert-exclude-glob ()
  (interactive)
  (ol-icomplete-insert-glob (concat "-g '!'")))

(defun ol-icomplete-insert-glob (glob)
  (ol-icomplete-maybe-insert-options-separator)
  (insert " ")
  (insert glob)
  (forward-char -1))

(defun ol-icomplete-insert-case-sensitive-option ()
  (interactive)
  (ol-icomplete-insert-option "-s"))

(defun ol-icomplete-insert-fixed-strings-option ()
  (interactive)
  (ol-icomplete-insert-option "-F"))

(defun ol-icomplete-insert-no-ignore-option ()
  (interactive)
  (ol-icomplete-insert-option "--no-ignore"))

(defun ol-icomplete-insert-option (option)
  (let* ((num-added-chars 0)
         (org-point (point))
         (options-len (1+ (length option))))
    (setq num-added-chars (+ (ol-icomplete-maybe-insert-options-separator) num-added-chars))
    (insert " ")
    (insert option)
    (setq num-added-chars (+ options-len num-added-chars))
    (goto-char org-point)
    (forward-char num-added-chars)))

(defun ol-icomplete-maybe-insert-options-separator ()
  (interactive)
  (let* ((num-added-chars
          (if (string-match-p " -- " (icomplete--field-string))
              0
            (goto-char (line-beginning-position))
            (insert " -- ")
            4)))
    (goto-char (line-beginning-position))
    (re-search-forward " -- ")
    (forward-char -4)
    num-added-chars))

;; -----------------------------------------------------------------------------
;; Style
;; -----------------------------------------------------------------------------

(defun ol-all-completions (string table pred _point)
  ;; bounds, prefix, prefix-length are needed for find-file
  (let* ((bounds (completion-boundaries string table pred ""))
         (prefix-length (car bounds))
         (prefix (substring string 0 prefix-length))
         (to-complete (substring string prefix-length))
         (regex (ol-string-to-regex to-complete))
         (completion-regexp-list (list regex))
         (completion-ignore-case (ol-ignore-case-p to-complete))
         (all (all-completions prefix table pred))
         )
    (setq completion-lazy-hilit-fn
          (apply-partially #'ol-highlight-completion regex completion-ignore-case))
    (when all
      (append all prefix-length))))

(defun ol-ignore-case-p (string)
  (string= string (downcase string)))

(defun ol-string-to-regex (string)
  (let* ((trimmed (string-trim string " " " "))
         (old nil)
         (new trimmed))
    (while (not (string= old new))
      (setq old new)
      ;; replace below only does one at a time so need to loop
      (setq new (replace-regexp-in-string "\\([^ ]\\) \\([^ ]\\)" "\\1.*?\\2" old)))
    (replace-regexp-in-string " \\( +\\)" "\\1" new)))

(ert-deftest ol-string-to-regex-test ()
  (ol-assert-equal "" (ol-string-to-regex ""))
  (ol-assert-equal "defun" (ol-string-to-regex "defun"))
  (ol-assert-equal "defun.*?my-fun" (ol-string-to-regex "defun my-fun"))
  (ol-assert-equal "defun my-fun" (ol-string-to-regex "defun  my-fun"))
  (ol-assert-equal "defun  my-fun" (ol-string-to-regex "defun   my-fun"))
  (ol-assert-equal "defun.*?my-fun" (ol-string-to-regex "defun my-fun "))
  (ol-assert-equal "a.*?b.*?c" (ol-string-to-regex "a b c"))
  (ol-assert-equal "a b c" (ol-string-to-regex "a  b  c"))
  )

(ert-deftest ol-all-completions-test ()
  (let ((candidates '("read-from-string"
                      "read-from-buffer"
                      "read-from-minibuffer"
                      "read"
                      "READ"
                      )))

    (ol-assert-equal '(
                       "read-from-string"
                       "read-from-buffer"
                       "read-from-minibuffer"
                       "read"
                       "READ"
                       )
                     (ol-all-completions "read" candidates nil nil))

    (ol-assert-equal '(
                       "read-from-buffer"
                       "read-from-minibuffer"
                       )
                     (ol-all-completions "read buffer" candidates nil nil))

    (ol-assert-equal '(
                       "read"
                       "READ"
                       )
                     (ol-all-completions "ead$" candidates nil nil))

    (ol-assert-equal '(
                       "read-from-minibuffer"
                       )
                     (ol-all-completions "read -[min]+" candidates nil nil))

    (ol-assert-equal nil (ol-all-completions "dummy" candidates nil nil))

    (ol-assert-equal '(
                       "READ"
                       )
                     (ol-all-completions "D" candidates nil nil))
    )
  )

(defun ol-try-completion (string table pred point)
  (let ((all (ol-all-completions string table pred point)))
    (cond
     ((null all) nil)
     ;; This caused the hard to find issue "Error running timer: (wrong-type-argument listp 0)
     ;; all is not a proper list. To trigger this, type M-x and type
     ;; "l time" and select list-timers. Maybe many buffers need to be open.
     ((eq (length (ol-nmake-proper-list all)) 1) string)
     (t string))))

;; This style is not just about matching, but also about highlights
(add-to-list 'completion-styles-alist
             '(ol ol-try-completion ol-all-completions "ol"))

(defun ol-highlight-completion (regex ignore-case candidate)
  (when (ol-completion-metadata-get 'ol-skip-normal-highlight)
    (ol-map-font-lock-face-to-face candidate)
    )
  (unless (ol-completion-metadata-get 'ol-skip-normal-highlight)
    (ol-normal-highlight-fn regex ignore-case candidate))
  (when-let ((fn (ol-completion-metadata-get 'ol-extra-highlight-function)))
    (funcall fn candidate))
  candidate)

(defun ol-completion-metadata-get (key)
  (let* ((md (completion-metadata
              ""
              minibuffer-completion-table
              minibuffer-completion-predicate)))
    (completion-metadata-get md key)))

(defun ol-normal-highlight-fn (regex ignore-case candidate)
  (let* ((case-fold-search ignore-case))
    (string-match regex candidate)
    (let* ((m (match-data))
           (start (car m))
           (end (cadr m)))
      (add-face-text-property start end 'ol-match-face nil candidate))))

;; todo: don't highlight char by char, do intervals for better performance
(defun ol-map-font-lock-face-to-face (string)
  (dolist (pos (number-sequence 0 (1- (length string))))
    (when-let ((prop (get-text-property pos 'font-lock-face string)))
      (add-face-text-property pos (1+ pos) prop nil string)))
  string)

(setq completion-lazy-hilit t)

(setc completion-styles '(ol))

;; So that 'ol style is used for everything
(setc completion-category-defaults nil)


;; -----------------------------------------------------------------------------
;; Async
;; -----------------------------------------------------------------------------

(setc grep-use-headings t)
;; Also avoids issues in async completion
(setc compilation-always-kill t)
(setc compilation-message-face nil)
;; In terminal, prevent scroll of buffer when clicking result
(setc compilation-context-lines t)

(defun ol-async-compilation-buffer-name-advice (name)
  (setq ol-async-buffer name))

(advice-add 'compilation-buffer-name :filter-return
            #'ol-async-compilation-buffer-name-advice)

(defun ol-async-stop-process ()
  (ol-silent
    (ignore-errors
      ;; Don't use kill-compilation, because it uses compilation-find-buffer
      ;; which in turn "finds" vterm buffers, so whenever the minibuffer exists
      ;; C-c is printed in vterm buffers if kill-compilation would be used.
      (when ol-async-buffer
        (interrupt-process (get-buffer-process ol-async-buffer))))))

(defun ol-async-stop-timer ()
  (when ol-async-timer
    (cancel-timer ol-async-timer))
  (setq ol-async-timer nil))

(defun ol-async-cleanup ()
  (ol-async-stop-process)
  (ol-async-stop-timer)
  (setq ol-async-buffer nil)
  (setq ol-async-candidates nil)
  (setq ol-async-has-moved nil)
  (ol-set-completion-all-sorted-completions nil))

(defun ol-set-completion-all-sorted-completions (completions)
  ;; Never set to nil to avoid "No match" which causes flicker
  ;; Also, icomplete requires an improper list with 0 at the end
  (setq completion-all-sorted-completions (append (or completions '("")) 0)))

(add-hook 'minibuffer-exit-hook #'ol-async-cleanup)

(defun ol-async-exhibit ()
  (when (and ol-async-buffer (not ol-async-has-moved))
    (with-current-buffer ol-async-buffer
      ;; todo: consider only appending new lines
      (let* ((lines (split-string (buffer-string) "\n" t))
             (relevant-lines (butlast (cdr (cdr (cdr lines)))))
             (width-limit (- (frame-width) 20))
             (trimmed-lines (mapcar (lambda (str)
                                      (string-limit str width-limit))
                                    relevant-lines)))
        (setq ol-async-candidates trimmed-lines)))
    (ol-set-completion-all-sorted-completions ol-async-candidates)
    (icomplete-exhibit))
  (ol-async-stop-timer))

;; Group exhibit due to process output to reduce flicker
(defun ol-async-delayed-exhibit ()
  (unless ol-async-timer
    (setq ol-async-timer
          (run-with-timer 0.1 nil #'ol-async-exhibit))))

(defun ol-on-compilation-filter-hook ()
  ;; to handle when e.g. grep is called not as part of async completion
  ;; todo: check ol-async-completing-read-active instead?
  (when ol-async-buffer
    (ol-async-delayed-exhibit)))

;; todo: only locally
(add-hook 'compilation-filter-hook #'ol-on-compilation-filter-hook)

(defun ol-compilation-handle-exit-advice (old-fun &rest args)
  ;; Adviced for two reasons: silence and empty buffer if no candidates
  (if ol-async-buffer
      (progn
        (unless ol-async-candidates
          (ol-set-completion-all-sorted-completions nil))
        (ol-async-delayed-exhibit)
        (ol-silent
          (apply old-fun args)))
    (apply old-fun args)))

(advice-add 'compilation-handle-exit :around #'ol-compilation-handle-exit-advice)

(defun ol-async-minibuffer-input-changed (input-to-cmd)
  (ol-async-stop-process)
  (ol-async-stop-timer)
  ;; Don't set completion-all-sorted-completions to empty since then there's
  ;; flicker in the display (easier to see if (sit-for 1) in
  ;; ol-async-exhibit). icomplete shows the candidates in
  ;; completion-all-sorted-completions before any input has arrived
  (setq ol-async-candidates nil)
  (let* ((input (minibuffer-contents-no-properties))
         (cmd (funcall input-to-cmd input)))
    (save-window-excursion
      (cl-letf (((symbol-function 'sit-for) (lambda (&rest _))))
        (grep cmd)))
    (cl-assert ol-async-buffer)))

(defun ol-async-completing-read (prompt input-to-cmd history)
  (ol-async-cleanup)
  (minibuffer-with-setup-hook
      (lambda ()
        (let* ((hook (lambda (&rest _)
                       (ol-async-minibuffer-input-changed input-to-cmd))))
          (add-hook 'after-change-functions hook nil 'local)))
    ;; use let to automatically have ol-async-completing-read-active be t
    ;; for the appropriate scope
    (let* ((ol-async-completing-read-active t)
           (table (lambda (string pred action)
                    (if (eq action 'metadata)
                        `(metadata
                          (ol-skip-normal-highlight . t))
                      (complete-with-action action '("") string pred)))))
      (completing-read prompt table
                       nil ;; predicate
                       nil ;; require-match
                       nil ;; initial-input
                       history))))

(defun ol-async-goto-result ()
  (interactive)
  (funcall ol-async-goto-function))

(defun ol-icomplete-print-async-debug-info ()
  (interactive)
  (message "ol-async-completing-read-active %s" ol-async-completing-read-active)
  (message "ol-async-buffer %s" ol-async-buffer)
  (message "ol-async-candidates %s" ol-async-candidates)
  (message "ol-async-timer %s" ol-async-timer)
  (message "ol-async-has-moved %s" ol-async-has-moved)
  (message "ol-async-goto-function %s" ol-async-goto-function))

;;;; ---------------------------------------------------------------------------
;;;; Application: file name
;;;; ---------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; Collection
;; -----------------------------------------------------------------------------
;; Inspired by https://github.com/oantolin/embark

(define-derived-mode ol-collect-mode fundamental-mode "ol-collect")

(defvar ol-collect-mode-map (make-sparse-keymap))

(defvar-local ol-collect-command nil)
(defvar-local ol-collect-buffer nil)

(defun ol-collect-record-this-command ()
  (setq ol-collect-command this-command)
  (setq ol-collect-buffer
        (if (and (minibufferp) (minibuffer-selected-window))
            (window-buffer (minibuffer-selected-window))
          (current-buffer))))

(add-hook 'minibuffer-setup-hook #'ol-collect-record-this-command)

(defun ol-collect-create-buffer (name)
  (let* ((buffer (generate-new-buffer name))
         (candidates (completion-all-sorted-completions))
         ;; Remember, these are buffer local
         (command ol-collect-command)
         (buf ol-collect-buffer))
    (with-current-buffer buffer
      (ol-collect-mode)
      (read-only-mode -1) 
      (setq ol-collect-command command)
      (setq ol-collect-buffer buf)
      (dolist (candidate (ol-nmake-proper-list candidates))
        (when (stringp candidate)
          (when completion-lazy-hilit-fn
            (funcall completion-lazy-hilit-fn candidate))
          (insert candidate)
          (insert "\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    buffer))

(defun ol-collect ()
  (interactive)
  (ol-collect-1 #'switch-to-buffer-other-window))

(defun ol-collect-1 (action)
  (ol-add-input-to-minibuffer-history)
  (let* ((name (format "*Collect: %s - %s*" ol-collect-command
                       (minibuffer-contents-no-properties)))
         (buffer (if ol-async-buffer
                     (let ((offset (+ 4 (length icomplete--scrolled-past))))
                       (with-current-buffer ol-async-buffer
                         (forward-line offset)
                         (rename-buffer name 'unique)))
                   (ol-collect-create-buffer name))))
    (run-at-time nil nil action buffer)
    (minibuffer-keyboard-quit)))

(defun ol-select ()
  (interactive)
  (let* ((candidate (string-trim-right (thing-at-point 'line t)))
         (command ol-collect-command)
         (buf ol-collect-buffer))
    ;; todo: doesn't work when 3 windows
    ;; (select-window (next-window (selected-window)))
    (with-current-buffer buf
      (minibuffer-with-setup-hook
          (lambda ()
            (delete-minibuffer-contents)
            (insert candidate)
            (add-hook 'post-command-hook #'exit-minibuffer nil t))
        (setq this-command command)
        (command-execute command)))))

;; Copied/modified from https://stackoverflow.com/a/28585107
(defun ol-nmake-proper-list (x)
  (let ((y (last x)))
    (setcdr y nil)
    x))

;; -----------------------------------------------------------------------------
;; Test
;; -----------------------------------------------------------------------------

(defun ol-dummy-completion ()
  (interactive)
  (let* ((delete-action (lambda (cand) (not (equal cand "b"))
                          ))
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      `(metadata
                        (ol-delete-action . ,delete-action))
                    (complete-with-action action '("a" "b" "c" "aa" "ab")
                                          string pred))))) 
    (completing-read "Dummy: "
                     table)))


(provide 'ol-completing-read-framework)
