;; -*- lexical-binding: t -*-

(require 'ol-evil)
(require 'ol-ert)
(require 'ol-colors)

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
  (unless (icomplete-forward-completions)
    (icomplete-vertical-goto-first)))

(defun ol-icomplete-backward ()
  (interactive)
  (unless (icomplete-backward-completions)
    (icomplete-vertical-goto-last)))

(ol-define-key icomplete-vertical-mode-minibuffer-map
               "C-n" #'minibuffer-keyboard-quit)
(ol-define-key icomplete-vertical-mode-minibuffer-map
               "C-j" #'ol-icomplete-forward)
(ol-define-key icomplete-vertical-mode-minibuffer-map
               "C-k" #'ol-icomplete-backward)
(ol-define-key icomplete-vertical-mode-minibuffer-map
               'tab #'icomplete-force-complete-and-exit)
(ol-define-key icomplete-vertical-mode-minibuffer-map
               'return #'icomplete-force-complete)

;; I only want input, and that is handled manually below
(setc history-add-new-input nil)

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
  (let* ((regex (ol-string-to-regex string))
         (completion-regexp-list (list regex))
         (completion-ignore-case (ol-ignore-case-p string)))
    (setq completion-lazy-hilit-fn
          (apply-partially #'ol-highlight-completion regex completion-ignore-case))
    (all-completions "" table pred)))

(defun ol-ignore-case-p (string)
  (string= string (downcase string)))

(defun ol-string-to-regex (string)
  (replace-regexp-in-string
   " \\( +\\)" "\\1"
   (replace-regexp-in-string
    "\\([^ ]\\) \\([^ ]\\)" "\\1.*?\\2"
    (string-trim string " " " "))))

(ert-deftest ol-string-to-regex-test ()
  (ol-assert-equal "" (ol-string-to-regex ""))
  (ol-assert-equal "defun" (ol-string-to-regex "defun"))
  (ol-assert-equal "defun.*?my-fun" (ol-string-to-regex "defun my-fun"))
  (ol-assert-equal "defun my-fun" (ol-string-to-regex "defun  my-fun"))
  (ol-assert-equal "defun  my-fun" (ol-string-to-regex "defun   my-fun"))
  (ol-assert-equal "defun.*?my-fun" (ol-string-to-regex "defun my-fun "))
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
     ((eq (length all) 1) string)
     (t string))))

;; This style is not just about matching, but also about highlights
(add-to-list 'completion-styles-alist
             '(ol ol-try-completion ol-all-completions "ol"))

(defun ol-highlight-completion (regex ignore-case candidate)
  (let* ((md (completion-metadata
              ""
              minibuffer-completion-table
              minibuffer-completion-predicate)))
    (when (completion-metadata-get md 'ol-skip-normal-highlight)
      (ol-map-font-lock-face-to-face candidate)
      )
    (unless (completion-metadata-get md 'ol-skip-normal-highlight)
      (ol-normal-highlight-fn regex ignore-case candidate))
    (when-let ((fn (completion-metadata-get md 'ol-extra-highlight-function)))
      (funcall fn candidate))
    candidate))

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

;; -----------------------------------------------------------------------------
;; Sync applications
;; -----------------------------------------------------------------------------
;; todo: move sync applications to ol-completing.el once verified
;; for the other frameworks

(defun ol-switch-to-buffer ()
  "Similar to `switch-to-buffer' but avoids face problems and skips
current buffer."
  (interactive)
  (let* ((buffers (cl-remove-if (lambda (buffer)
                                  (or
                                   (eq buffer (current-buffer))
                                   (minibufferp buffer) 
                                   ))
                                (buffer-list)))
         (buffer-names (mapcar (lambda (buffer) (with-current-buffer buffer
                                                  (buffer-name)))
                               buffers))
         ;; Copied/modified from https://emacs.stackexchange.com/a/8177
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      `(metadata
                        (ol-extra-highlight-function . ,#'ol-switch-to-buffer-highlight-fn)
                        (cycle-sort-function . ,#'identity)
                        (display-sort-function . ,#'identity))
                    (complete-with-action action buffer-names string pred)))))
    (switch-to-buffer (completing-read
                       "Switch to buffer: "
                       table))))

(defun ol-switch-to-buffer-highlight-fn (candidate)
  (let* ((buffer (get-buffer candidate))
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

(ol-define-key ol-override-map "C-j" #'ol-switch-to-buffer)

;; -----------------------------------------------------------------------------
;; Async applications
;; -----------------------------------------------------------------------------

(set-face-attribute 'match nil
                    :inherit 'ol-match-face
                    :foreground 'unspecified
                    :background 'unspecified)

(setc grep-use-headings t)
;; Also avoids issues in async completion
(setc compilation-always-kill t)

(defvar ol-async-buffer nil)
(defvar ol-async-candidates nil)
(defvar ol-async-timer nil)

(defun ol-async-compilation-buffer-name-advice (name)
  (setq ol-async-buffer name))

(advice-add 'compilation-buffer-name :filter-return
            #'ol-async-compilation-buffer-name-advice)

(defun ol-async-stop-process ()
  (ol-silent
    (ignore-errors
      (kill-compilation))))

(defun ol-async-stop-timer ()
  (when ol-async-timer
    (cancel-timer ol-async-timer))
  (setq ol-async-timer nil))

(defun ol-async-cleanup ()
  (ol-async-stop-process)
  (ol-async-stop-timer)
  (setq ol-async-buffer nil)
  (setq ol-async-candidates nil)
  ;; Don't set to nil to avoid "No match" which causes flicker
  (setq completion-all-sorted-completions '("" . 0)))

(add-hook 'minibuffer-exit-hook #'ol-async-cleanup)

(defun ol-async-exhibit ()
  (ol-async-stop-timer)
  (when ol-async-buffer
    (with-current-buffer ol-async-buffer
      ;; todo: consider only appending new lines
      (let* ((lines (split-string (buffer-string) "\n" t))
             (relevant-lines (butlast (cdr (cdr (cdr lines)))))
             (width-limit (- (frame-width) 20))
             (trimmed-lines (mapcar (lambda (str)
                                      (string-limit str width-limit))
                                    relevant-lines)))
        (setq ol-async-candidates trimmed-lines)))
    (setq completion-all-sorted-completions (append ol-async-candidates 0))
    (icomplete-exhibit)))

;; Group exhibit due to process output to reduce flicker
(defun ol-async-delayed-exhibit ()
  (unless ol-async-timer
    (setq ol-async-timer
          (run-with-timer 0.01 nil #'ol-async-exhibit))))

(defun ol-on-compilation-filter-hook ()
  (ol-async-delayed-exhibit))

;; todo: only locally
(add-hook 'compilation-filter-hook #'ol-on-compilation-filter-hook)

(defun ol-compilation-handle-exit-silence-advice (old-fun &rest args)
  (if (active-minibuffer-window)
      (ol-silent
        (apply old-fun args))
    (apply old-fun args)))

(advice-add 'compilation-handle-exit :around #'ol-compilation-handle-exit-silence-advice)

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
    (let* ((table (lambda (string pred action)
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
  (ol-grep-helper prompt '("rg" "--smart-case" "--no-heading")))

(defun ol-git-grep (prompt)
  (ol-grep-helper prompt '("git" "--no-pager" "grep" "-n" "-E")))

(defun ol-grep (prompt)
  (ol-grep-helper prompt '("grep" "-E" "-n" "-I" "-r")))

(defun ol-grep-helper (prompt args)
  (let* ((input-to-cmd
          (lambda (input)
            (concat 
             (string-join args " ")
             " "
             (shell-quote-argument (ol-string-to-regex input)))))
         (selection (ol-async-completing-read prompt input-to-cmd 'ol-grep)))
    (ol-open-grep-selection selection)))

(defun ol-open-grep-selection (selection)
  ;; example: .emacs_config/config/ol-file.el:44:(defun ol-save-silently ()
  ;; this works because the string above is invisible thanks to a text property
  (if (string-match "\\(.*\\):\\([0-9]+\\):" selection)
      (let* ((file (match-string 1 selection))
             (line (match-string 2 selection)))
        (unless (file-exists-p file)
          (user-error "No such file"))
        (find-file file)
        (goto-char (point-min))
        (forward-line (1- (string-to-number line))))
    (user-error "Couldn't find match")))

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
;; (ol-define-key icomplete-vertical-mode-minibuffer-map "M-e" #'embark-collect)
(ol-evil-define-key 'normal ol-collect-mode-map "RET" #'ol-select)

(defvar ol-collect-command nil)
(make-variable-buffer-local 'ol-collect-command)

(defun ol-collect-record-this-command ()
  (setq ol-collect-command this-command))

(add-hook 'minibuffer-setup-hook #'ol-collect-record-this-command)

(defun ol-collect-create-buffer (name command)
  (let* ((buffer (generate-new-buffer name))
         (candidates (completion-all-sorted-completions)))
    (with-current-buffer buffer
      (ol-collect-mode)
      (read-only-mode -1) 
      (setq ol-collect-command command)
      (dolist (candidate (ol-nmake-proper-list candidates))
        (when (stringp candidate)
          (insert candidate)
          (insert "\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    buffer))

(defun ol-collect ()
  (interactive)
  (let* ((command ol-collect-command)
         (name (format "*Collect: %s - %s*" command
                       (minibuffer-contents-no-properties)))
         (buffer (if ol-async-buffer
                     (progn
                       (with-current-buffer ol-async-buffer
                         (rename-buffer name 'unique)))
                   (ol-collect-create-buffer name command))))
    (run-at-time nil nil #'switch-to-buffer-other-window buffer)
    (minibuffer-keyboard-quit)))

(defun ol-select ()
  (interactive)
  (let* ((candidate (string-trim-right (thing-at-point 'line t)))
         (command ol-collect-command))
    (select-window (next-window (selected-window)))
    (minibuffer-with-setup-hook
        (lambda ()
          (delete-minibuffer-contents)
          (insert candidate)
          (add-hook 'post-command-hook #'exit-minibuffer nil t))
      (setq this-command command)
      (command-execute command))))

;; Copied/modified from https://stackoverflow.com/a/28585107
(defun ol-nmake-proper-list (x)
  (let ((y (last x)))
    (setcdr y nil)
    x))

(provide 'ol-completing-read-own)
