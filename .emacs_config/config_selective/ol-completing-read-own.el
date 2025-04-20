;; -*- lexical-binding: t -*-

(require 'ol-evil)
(require 'ol-completion-style)

(require 'embark)

(require 'icomplete)
(require 'delsel) ;; for minibuffer-keyboard-quit

;; -----------------------------------------------------------------------------
;; icomplete-vertical
;; -----------------------------------------------------------------------------

(icomplete-vertical-mode t)
(setq icomplete-scroll t)
(setc icomplete-show-matches-on-no-input t)
(setc icomplete-compute-delay 0)
(setc icomplete-max-delay-chars 0)
(setc resize-mini-windows 'grow-only)
(setc icomplete-prospects-height 20)

(set-face-attribute 'icomplete-selected-match nil
                    :background "#dddddd"
                    )

(ol-define-key icomplete-vertical-mode-minibuffer-map "C-n" #'minibuffer-keyboard-quit)
(ol-define-key icomplete-vertical-mode-minibuffer-map "C-j" #'icomplete-forward-completions)
(ol-define-key icomplete-vertical-mode-minibuffer-map "C-k" #'icomplete-backward-completions)
(ol-define-key icomplete-vertical-mode-minibuffer-map 'tab
               #'icomplete-force-complete-and-exit)
(ol-define-key icomplete-vertical-mode-minibuffer-map 'return
               #'icomplete-force-complete)

;; Otherwise minibuffer-complete-word is called
(ol-define-key minibuffer-local-completion-map "SPC" nil)

(ol-define-key icomplete-vertical-mode-minibuffer-map "M-o" #'embark-collect)

(set-face-attribute 'completions-common-part nil
                    :foreground 'unspecified)

;; -----------------------------------------------------------------------------
;; Completion style
;; -----------------------------------------------------------------------------

(setc completion-styles '(ol))

;; -----------------------------------------------------------------------------
;; Faces
;; -----------------------------------------------------------------------------

;; Needed for switch-to-buffer. Still not perfect

(set-face-attribute 'completions-highlight nil
                    :inherit 'ol-match-face)

(set-face-attribute 'completions-first-difference nil
                    :inherit 'ol-match-face)

(set-face-attribute 'completions-common-part nil
                    :foreground 'unspecified
                    :inherit 'ol-match-face)

(set-face-attribute 'icomplete-selected-match nil
                    :background 'unspecified
                    :inherit 'ol-selection-face)

;; -----------------------------------------------------------------------------
;; Completing read wrappers
;; -----------------------------------------------------------------------------

;; Copied/modified from https://emacs.stackexchange.com/a/8177
(defun ol-presorted-completion-table (completions)
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata
          (cycle-sort-function . ,#'identity)
          (display-sort-function . ,#'identity))
      (complete-with-action action completions string pred))))

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
         (table (ol-presorted-completion-table buffer-names)))
    (switch-to-buffer (completing-read
                       "Switch to buffer: "
                       table))))

(ol-define-key ol-override-map "C-j" #'ol-switch-to-buffer)

;; -----------------------------------------------------------------------------
;; Async
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
    (completing-read prompt '(""))))

(defun ol-ripgrep (prompt)
  (let* ((input-to-cmd (lambda (input) (list "rg" (ol-string-to-regex input)))))
    (ol-async-completing-read prompt input-to-cmd)))

(defun ol-git-grep (prompt)
  (let* ((input-to-cmd (lambda (input) (list "git" "--no-pager" "grep" "-n" "-E"
                                             (ol-string-to-regex input)))))
    (ol-async-completing-read prompt input-to-cmd)))

(defun ol-grep (prompt)
  (let* ((input-to-cmd (lambda (input) (list "grep" "-E" "-n" "-I" "-r"
                                             (ol-string-to-regex input)))))
    (ol-async-completing-read prompt input-to-cmd)))

(provide 'ol-completing-read-own)
