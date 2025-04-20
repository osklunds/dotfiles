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

(defvar ol-async-candidates nil)

(defun ol-update-async-candidates (input)
  (let* ((cmd (split-string input " " t)))
    (setq ol-async-candidates
          (ignore-errors
            (apply #'process-lines-ignore-status cmd)))
    (setq completion-all-sorted-completions nil)
    (setq completion-all-sorted-completions (append ol-async-candidates 0))
    (icomplete-exhibit)
    ))

(defun ol-ripgrep ()
  (interactive)
  (minibuffer-with-setup-hook
      (lambda ()
        (let* ((hook (lambda (&rest _)
                       (ol-update-async-candidates (minibuffer-contents-no-properties))
                       nil 'local)))
          (add-hook 'after-change-functions hook nil 'local)
          ))
    (completing-read "hej: "
                     '("grep" "ripgrep"))))


(provide 'ol-completing-read-own)
