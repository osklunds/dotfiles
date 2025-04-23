;; -*- lexical-binding: t -*-

(require 'ol-evil)
(require 'ol-ert)
(require 'ol-colors)

(require 'embark)

(require 'icomplete)
(require 'delsel) ;; for minibuffer-keyboard-quit

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
  (let* ((parts (split-string string " ")))
    (apply #'concat (cdr (mapcan (lambda (part) (list ".*?" part)) parts)))))

(ert-deftest ol-string-to-regex-test ()
  (ol-assert-equal "" (ol-string-to-regex ""))
  (ol-assert-equal "defun" (ol-string-to-regex "defun"))
  (ol-assert-equal "defun.*?my-fun" (ol-string-to-regex "defun my-fun"))
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

(defvar ol-async-process nil)

(defun ol-stop-async-process ()
  (interactive)
  (when (and ol-async-process (eq (process-status ol-async-process) 'run))
    (kill-process ol-async-process))
  (setq ol-async-process nil))

(defun ol-cleanup-async ()
  (ol-stop-async-process)
  (setq ol-async-timer nil)
  (setq ol-async-candidates nil)
  ;; Don't set to nil to avoid "No match" which causes flicker
  (setq completion-all-sorted-completions '("" . 0)))

(add-hook 'minibuffer-exit-hook #'ol-cleanup-async)

(defvar ol-async-candidates nil)

;; todo: use sentinel too to detect when no matches
(defun ol-async-filter (proc output)
  (when (eq proc ol-async-process)
    ;; todo: don't assume that full line
    (let* ((lines (split-string output "\n" t)))
      (setq ol-async-candidates (append ol-async-candidates lines))
      (ol-async-delayed-exhibit))))

(defvar ol-async-timer nil)

;; Group exhibit due to process output to reduce flicker
(defun ol-async-delayed-exhibit ()
  (unless ol-async-timer
    (setq ol-async-timer
          (run-with-timer 0.01 nil #'ol-async-exhibit))))

(defun ol-async-exhibit ()
  (setq ol-async-timer nil)
  (setq completion-all-sorted-completions (append ol-async-candidates 0))
  (icomplete-exhibit))

(defun ol-async-minibuffer-input-changed (input-to-cmd)
  (ol-stop-async-process)
  ;; Don't set completion-all-sorted-completions to empty since then there's
  ;; flicker in the display (easier to see if (sit-for 1) in
  ;; ol-async-exhibit). icomplete shows the candidates in
  ;; completion-all-sorted-completions before any input has arrived
  (setq ol-async-candidates nil)
  (let* ((input (minibuffer-contents-no-properties))
         (cmd (funcall input-to-cmd input)))
    (setq ol-async-process
          (ignore-errors
            (make-process
             :name "ol-async-process"
             :command cmd
             :filter #'ol-async-filter)))))

(defun ol-async-completing-read (prompt input-to-cmd)
  (ol-cleanup-async)
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
      (completing-read prompt table))))

(defun ol-ripgrep (prompt)
  (ol-grep-helper prompt '("rg" "--color=never" "--smart-case"
                           "--no-heading" "--with-filename"
                           "--line-number")))

(defun ol-git-grep (prompt)
  (ol-grep-helper prompt '("git" "--no-pager" "grep" "-n" "-E")))

(defun ol-grep (prompt)
  (ol-grep-helper prompt '("grep" "-E" "-n" "-I" "-r")))

(defun ol-grep-helper (prompt args)
  (let* ((input-to-cmd (lambda (input) (append args (list (ol-string-to-regex input)))))
         (selection (ol-async-completing-read prompt input-to-cmd)))
    (ol-open-grep-selection selection)))

(defun ol-open-grep-selection (selection)
  ;; example: .emacs_config/config/ol-file.el:44:(defun ol-save-silently ()
  (if (string-match "\\(.*\\):\\([0-9]+\\):" selection)
      (let* ((file (match-string 1 selection))
             (line (match-string 2 selection)))
        (unless (file-exists-p file)
          (user-error "No such file"))
        (find-file file)
        (goto-char (point-min))
        (forward-line (1- (string-to-number line))))
    (user-error "Couldn't find match")))

(cl-defun ol-completing-read-shell-command (&key prompt history require-match)
  (let* ((input-to-cmd (lambda (input) (list "bash" "-c" input))))
    (ol-async-completing-read prompt input-to-cmd)))

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
(ol-define-key icomplete-vertical-mode-minibuffer-map "M-e" #'embark-collect)
(ol-evil-define-key 'normal ol-collect-mode-map "RET" #'ol-select)

(defvar ol-collect-command nil)
(make-variable-buffer-local 'ol-collect-command)

(defun ol-collect-record-this-command ()
  (setq ol-collect-command this-command))

(add-hook 'minibuffer-setup-hook #'ol-collect-record-this-command)

(defun ol-collect ()
  (interactive)
  (let* ((candidates (completion-all-sorted-completions))
         (name (format "*Collect: %s - %s*" ol-collect-command
                       (minibuffer-contents-no-properties)))
         (command ol-collect-command)
         (buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      ;; todo: don't hard code
      (if (memq command '(ol-dwim-find-file-content))
          (grep-mode)
        (ol-collect-mode))
      (read-only-mode -1) 
      (setq ol-collect-command command)
      (dolist (candidate (ol-nmake-proper-list candidates))
        (when (stringp candidate)
          (insert candidate)
          (insert "\n")))
      (goto-char (point-min))
      (read-only-mode 1))
    (run-at-time nil nil #'switch-to-buffer-other-window buffer)
    (minibuffer-keyboard-quit)))

(defun ol-select ()
  (interactive)
  (let* ((candidate (string-trim-right (thing-at-point 'line t)))
         (command ol-collect-command))
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
