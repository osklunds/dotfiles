;; -*- lexical-binding: t -*-

(require 'ol-evil)
(require 'ol-ert)
(require 'ol-colors)
(require 'ol-completing-read)

(require 'icomplete)
(require 'delsel) ;; for minibuffer-keyboard-quit
(require 'grep)

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

(ol-define-key icomplete-vertical-mode-minibuffer-map
               "~" #'ol-icomplete-dwim-tilde)

;; To prevent help menu from opening
(ol-define-key icomplete-vertical-mode-minibuffer-map
               "C-h" (lambda () (interactive)))

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
                        (compile-goto-error))
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

;; Otherwise minibuffer-complete-word is called
(ol-define-key minibuffer-local-completion-map "SPC" nil)

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
;; Sync applications
;; -----------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; switch-to-buffer
;;;; ---------------------------------------------------------------------------

(defun ol-switch-to-buffer ()
  "Similar to `switch-to-buffer' but avoids face problems and puts current
buffer last."
  (interactive)
  (let* ((buffers (cl-remove-if (lambda (buffer)
                                  (or
                                   (eq buffer (current-buffer))
                                   (minibufferp buffer) 
                                   ))
                                (buffer-list)))
         (buffer-names (mapcar (lambda (buffer) (with-current-buffer buffer
                                                  (buffer-name)))
                               (append buffers (list (current-buffer)))))
         ;; Copied/modified from https://emacs.stackexchange.com/a/8177
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      `(metadata
                        (ol-extra-highlight-function . ,#'ol-switch-to-buffer-highlight-fn)
                        (ol-delete-action . ,#'ol-switch-to-buffer-delete-action)
                        (cycle-sort-function . ,#'identity)
                        (display-sort-function . ,#'identity))
                    (complete-with-action action buffer-names string pred)))))
    (switch-to-buffer (completing-read
                       "Switch to buffer: "
                       table))))

(defun ol-switch-to-buffer-highlight-fn (candidate)
  ;; "when" version needed to fix bug when two buffers of same file name are
  ;; open, and one is deleted. The remaining one will change name from name<dir>
  ;; to name and hence not be found anymore
  (when-let* ((buffer (get-buffer candidate))
              (mode (buffer-local-value 'major-mode buffer)))
    (cond
     ;; todo: consider what to do if remote and dired
     ;; (find-file "/docker:tests-dotfiles-tramp-test-1:/")
     ((file-remote-p (buffer-local-value 'default-directory buffer))
      (ol-add-face-text-property candidate 'ol-remote-buffer-name-face))

     ((eq mode 'dired-mode)
      (ol-add-face-text-property candidate 'ol-dired-buffer-name-face))

     ((eq mode 'vterm-mode)
      (ol-add-face-text-property candidate 'ol-vterm-buffer-name-face))

     (t nil))))

(defun ol-add-face-text-property (str face)
  (add-face-text-property 0 (length str) face nil str))

(defface ol-dired-buffer-name-face
  '((default :weight bold :inherit 'font-lock-function-name-face))
  "Face for dired buffer name in `ol-switch-to-buffer'.")

(defface ol-vterm-buffer-name-face
  '((default :weight bold :inherit 'font-lock-type-face))
  "Face for vterm buffer name in `ol-switch-to-buffer'.")

(defface ol-remote-buffer-name-face
  '((default :foreground "#110099"))
  "Face for remote buffer name in `ol-switch-to-buffer'.")

(defun ol-switch-to-buffer-delete-action (selected)
  ;; Use "when" version as extra robustification, although unsure if needed
  (when-let ((buf (get-buffer selected)))
    (kill-buffer buf)))

(ol-define-key ol-override-map "C-j" #'ol-switch-to-buffer)

;; -----------------------------------------------------------------------------
;; Async applications
;; -----------------------------------------------------------------------------

(set-face-attribute 'match nil
                    :inherit 'ol-match-face
                    :foreground 'unspecified
                    :background 'unspecified)

(set-face-attribute 'compilation-line-number nil
                    :foreground 'unspecified
                    :underline nil
                    :inherit 'font-lock-string-face)

(setc grep-use-headings t)
;; Also avoids issues in async completion
(setc compilation-always-kill t)
(setc compilation-message-face nil)
;; In terminal, prevent scroll of buffer when clicking result
(setc compilation-context-lines t)

(defvar ol-async-completing-read-active nil)
(defvar ol-async-buffer nil)
(defvar ol-async-candidates nil)
(defvar ol-async-timer nil)
(defvar ol-async-has-moved nil
  "If moving/scrolling in icomplete and then more candidates comes
the output is meesed up, so stop process when move.")

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
  (ol-async-delayed-exhibit))

;; todo: only locally
(add-hook 'compilation-filter-hook #'ol-on-compilation-filter-hook)

(defun ol-compilation-handle-exit-advice (old-fun &rest args)
  ;; Adviced for two reasons: silence and empty buffer if no candidates
  (if (active-minibuffer-window)
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

(defun ol-ripgrep (prompt)
  (ol-grep-helper prompt ol-rg-command))

(defun ol-git-grep (prompt)
  (ol-grep-helper prompt ol-git-grep-command))

(defun ol-grep (prompt)
  (ol-grep-helper prompt ol-grep-command))

(defun ol-grep-input-to-cmd (input)
  (let* ((split (ol-split-string-once input " -- "))
         (before (car split))
         (after (cdr split))
         (regex (shell-quote-argument (ol-string-to-regex after))))
    (if before
        (concat before " -- " regex)
      regex)))

(ert-deftest ol-grep-input-to-cmd-test ()
  ;; Search for one term
  (ol-assert-equal "hej" (ol-grep-input-to-cmd "hej"))

  ;; Serach two terms with space wildcard
  (ol-assert-equal "a.\\*\\?b" (ol-grep-input-to-cmd "a b"))

  ;; Search for literal space
  (ol-assert-equal "a\\ b" (ol-grep-input-to-cmd "a  b"))

  ;; Specify option towards grep, a is the option b is the search term
  (ol-assert-equal "a -- b" (ol-grep-input-to-cmd "a -- b"))

  ;; Specify option towards grep and use literal -- in search term
  (ol-assert-equal "a -- b.\\*\\?\\--.\\*\\?c" (ol-grep-input-to-cmd "a -- b -- c"))

  ;; Search for literal --
  (ol-assert-equal " -- \\--" (ol-grep-input-to-cmd " -- --"))
  )

(defun ol-split-string-once (string separator)
  (if-let ((pos (string-match separator string)))
      (cons (substring string 0 pos)
            (substring string (+ pos (length separator))))
    (cons nil string)))

(ert-deftest ol-split-string-once-test ()
  (ol-assert-equal `(,nil . "hej") (ol-split-string-once "hej" " -- "))
  (ol-assert-equal '("hej" . "hello") (ol-split-string-once "hej -- hello" " -- "))
  (ol-assert-equal '("hej " . "hello") (ol-split-string-once "hej  -- hello" " -- "))
  (ol-assert-equal '("hej" . " hello") (ol-split-string-once "hej --  hello" " -- "))
  (ol-assert-equal '("a" . "b -- c") (ol-split-string-once "a -- b -- c" " -- "))
  )

(defun ol-grep-helper (prompt args)
  (let* ((input-to-cmd
          (lambda (input)
            (concat args " " (ol-grep-input-to-cmd input)))))
    (ol-async-completing-read prompt input-to-cmd 'ol-grep)))

(cl-defun ol-completing-read-shell-command (prompt history)
  (let* ((input-to-cmd (lambda (input) (concat "bash -ic \"" input "\"")))
         (grep-use-headings nil))
    (ol-async-completing-read prompt input-to-cmd history)))

;; -----------------------------------------------------------------------------
;; Collection
;; -----------------------------------------------------------------------------
;; Inspired by https://github.com/oantolin/embark

;; some ideas:
;; - vterm when running ls seems to show color codes. So maybe can do
;; similar for ripgrep?
;; - maybe ripgrep headings can work as well as in consult since grep-mode
;; seemed to have such support

(define-derived-mode ol-collect-mode fundamental-mode "ol-collect")

(defvar ol-collect-mode-map (make-sparse-keymap))

(ol-define-key icomplete-vertical-mode-minibuffer-map "M-o" #'ol-collect)
(ol-evil-define-key 'normal ol-collect-mode-map "RET" #'ol-select)
(ol-evil-define-key 'normal ol-collect-mode-map "o" #'ol-select)

(ol-evil-define-key 'normal compilation-button-map "o" #'compile-goto-error)
(ol-evil-define-key 'normal compilation-mode-map "o" #'compile-goto-error)
(ol-evil-define-key 'normal grep-mode-map "o" #'compile-goto-error)

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
    (select-window (next-window (selected-window)))
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

(ol-define-key ol-normal-leader-map "m d" #'ol-dummy-completion)

(provide 'ol-completing-read-own)
