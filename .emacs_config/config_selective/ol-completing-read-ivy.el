;; -*- lexical-binding: nil -*-

(require 'ol-util)
(require 'ol-evil)
(require 'ol-colors)

(require 'ivy)
(require 'counsel)

;; -----------------------------------------------------------------------------
;; Ivy
;; -----------------------------------------------------------------------------

(ivy-mode t)

(setc ivy-height 20)
(setc ivy-wrap t)
(setc ivy-more-chars-alist '((t . 1)))

(defun ol-ivy-switch-buffer ()
  "Copy of ivy-switch-buffer, but allow visible buffers in preselect"
  (interactive)
  (let ((visible-ok t))
    (ivy-read "Switch to buffer: " #'internal-complete-buffer
              :keymap ivy-switch-buffer-map
              :preselect (buffer-name (other-buffer (current-buffer) visible-ok))
              :action #'ivy--switch-buffer-action
              :matcher #'ivy--switch-buffer-matcher
              :caller 'ivy-switch-buffer)))

;; Inspired by ivy
(defun ol-ivy-switch-buffer-transformer (str)
  (let* ((buffer (get-buffer str))
         (mode (buffer-local-value 'major-mode buffer)))
    (cond
     ((eq mode 'dired-mode) (ivy-append-face str 'font-lock-function-name-face))
     ((eq mode 'vterm-mode) (ivy-append-face str 'font-lock-type-face))
     (t str))))

(advice-add 'ivy-switch-buffer-transformer :override 'ol-ivy-switch-buffer-transformer)

;; -----------------------------------------------------------------------------
;; Counsel
;; -----------------------------------------------------------------------------

;; to avoid initial ^
(ivy-configure 'counsel-M-x
  :initial-input ""
  :display-transformer-fn #'counsel-M-x-transformer)

;; -----------------------------------------------------------------------------
;; Keybindings
;; -----------------------------------------------------------------------------

(ol-define-key ol-override-map "C-j" 'ol-ivy-switch-buffer)
(ol-global-set-key "C-x C-b" 'ol-ivy-switch-buffer)
(ol-define-key ol-override-map "M-x" 'counsel-M-x)
(ol-global-set-key "C-x C-f" 'counsel-find-file)

(ol-define-key ivy-minibuffer-map 'tab 'ivy-alt-done) ;; Exit, and use current selection
(ol-define-key ivy-minibuffer-map 'return 'ivy-immediate-done) ;; Exit, and use current input
(ol-define-key ivy-minibuffer-map "M-i" 'ivy-insert-current) ;; Insert the current candidate as input

(ol-define-key ivy-minibuffer-map "C-j" 'ivy-next-line)
(ol-define-key ivy-minibuffer-map "C-k" 'ivy-previous-line)
(ol-define-key ivy-minibuffer-map "C-n" 'minibuffer-keyboard-quit)
(ol-define-key minibuffer-local-map "C-n" 'minibuffer-keyboard-quit)

(ol-define-key ivy-switch-buffer-map "C-k" 'ivy-previous-line)
(ol-define-key ivy-switch-buffer-map "C-d" 'ivy-switch-buffer-kill)

;; Note ivy-occur only seems to work well for grep-like buffers
(ol-evil-define-key 'normal ivy-occur-grep-mode-map "o" 'ivy-occur-press)
(ol-evil-define-key 'normal ivy-occur-grep-mode-map "O" 'ivy-occur-press-and-switch)
(ol-evil-define-key 'normal ivy-occur-mode-map "o" 'ivy-occur-press)
(ol-evil-define-key 'normal ivy-occur-mode-map "O" 'ivy-occur-press-and-switch)

;; -----------------------------------------------------------------------------
;; Colors
;; -----------------------------------------------------------------------------

(dolist (face '(ivy-minibuffer-match-face-1
                ivy-minibuffer-match-face-2
                ivy-minibuffer-match-face-3
                ivy-minibuffer-match-face-4))
  (set-face-attribute face nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :weight 'unspecified
                      :inherit 'ol-match-face))

(set-face-attribute 'ivy-current-match nil
                    :inherit 'ol-selection-face
                    :background 'unspecified)

;; -----------------------------------------------------------------------------
;; Interface needed by ol-completing-read
;; -----------------------------------------------------------------------------

(cl-defun ol-completing-read-shell-command (&key prompt
                                                 history
                                                 require-match)
  (ivy-read prompt
            'ol-completing-read-shell-command-collection
            :dynamic-collection t
            :require-match require-match
            :history history
            :caller 'ol-async-completing-read))

(defun ol-completing-read-shell-command-collection (input)
  (let* ((cmd (concat "(unset TERM; " input " )")))
    (counsel--async-command cmd))
  nil)

(defun ol-ripgrep (prompt)
  (counsel-rg "" default-directory "" prompt))

(defun ol-git-grep (_prompt)
  ;; prompt not working yet
  (counsel-git-grep))

(defun ol-grep (_prompt)
  (user-error "Not working yet"))

(provide 'ol-completing-read-ivy)
