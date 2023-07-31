
;; ---------------------------------------------------------------------------
;; Leader
;; ---------------------------------------------------------------------------

(general-create-definer ol-leader-keys
  :keymaps '(normal insert visual emacs)
  ;; prefix seems to mean, only define if not overriding something existing
  :prefix "SPC"
  ;; global-prefix seems to mean, always define
  :global-prefix "C-SPC")

;;;; ---------------------------------------------------------------------------
;;;; Overriding keys
;;;; ---------------------------------------------------------------------------

(defun ol-override-key (key fun)
  (progn
    (general-define-key
     :states '(normal emacs)
     :keymaps 'override
     key fun)
    (general-define-key
     :states 'insert
     :keymaps 'term-raw-map
     key fun)))

;; ---------------------------------------------------------------------------
;; Evil
;; ---------------------------------------------------------------------------

(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

;; Window movement
(define-key evil-normal-state-map (kbd "C-h") #'evil-window-left)
(define-key evil-normal-state-map (kbd "C-l") #'evil-window-right)

(defun ol-no-op ()
  (interactive))

;; No arrow keys
(define-key evil-normal-state-map (kbd "<left>") 'ol-no-op)
(define-key evil-normal-state-map (kbd "<right>") 'ol-no-op)
(define-key evil-normal-state-map (kbd "<down>") 'ol-no-op)
(define-key evil-normal-state-map (kbd "<up>") 'ol-no-op)

(define-key evil-insert-state-map (kbd "<left>") 'ol-no-op)
(define-key evil-insert-state-map (kbd "<right>") 'ol-no-op)
(define-key evil-insert-state-map (kbd "<down>") 'ol-no-op)
(define-key evil-insert-state-map (kbd "<up>") 'ol-no-op)

(evil-define-key 'insert term-raw-map (kbd "<left>") 'term-send-left)
(evil-define-key 'insert term-raw-map (kbd "<right>") 'term-send-right)
(evil-define-key 'insert term-raw-map (kbd "<down>") 'term-send-down)
(evil-define-key 'insert term-raw-map (kbd "<up>") 'term-send-up)

(evil-global-set-key 'motion (kbd "<left>") 'ol-no-op)
(evil-global-set-key 'motion (kbd "<right>") 'ol-no-op)
(evil-global-set-key 'motion (kbd "<down>") 'ol-no-op)
(evil-global-set-key 'motion (kbd "<up>") 'ol-no-op)

(define-key evil-normal-state-map (kbd "?") 'evil-ex-nohighlight)

;; Clean insert state (maybe a bad idea? But if so, I should use
;; emacs keybindings in insert state instaed of vim's perhaps.)
;; TODO: These do not play well with term-mode

;; (let* ((letters '("a"
;;                   "b"
;;                ;; "c" Needed for magit commit confirm
;;                   "d"
;;                   "e"
;;                   "f"
;;                   "g"
;;                ;; "h" Needed for help
;;                   "i"
;;                   "k"
;;                   "l"
;;                   "m"
;;                   "n"
;;                   "o"
;;                   "p"
;;                   "q"
;;                   "r"
;;                   "s"
;;                   "t"
;;                   "u"
;;                   "v"
;;                   "w"
;;                   "x"
;;                   "y"
;;                   "z"))
;;        (letter-ctrls (mapcar (lambda (char) (format "C-%s" char)) letters))
;;        (other-keybinds '("C-@"
;;                    "S-<left>"
;;                    "S-<right>"
;;                    "<delete>"
;;                    "<insert>"))
;;        (keybinds (append letter-ctrls other-keybinds)))
;;   (dolist (keybind keybinds)
;;     (evil-define-key 'insert 'global (kbd keybind) 'ol-no-op)))

;; (evil-define-key 'insert 'global (kbd "<return>") 'newline)

;; ---------------------------------------------------------------------------
;; Find and replace
;; ---------------------------------------------------------------------------

(ol-leader-keys
  :keymaps 'visual
  "R" '(ol-full-replace-visual-selection :which-key "replace full visual selection")
  "r" '(ol-from-here-replace-visual-selection :which-key "replace from here visual selection"))

(ol-leader-keys
  :keymaps 'normal
  "R" '(ol-full-replace-symbol :which-key "replace full symbol")
  "r" '(ol-from-here-replace-symbol :which-key "replace from here symbol"))

;; ---------------------------------------------------------------------------
;; Windows and buffers
;; ---------------------------------------------------------------------------

(defun ol-split-window ()
  (interactive)
  (split-window-right)
  (evil-window-right 1))

(ol-override-key "M-w" 'ol-split-window)
(ol-override-key "M-e" 'delete-window)

(ol-override-key "C-j" 'ivy-switch-buffer)

;; ---------------------------------------------------------------------------
;; LSP
;; ---------------------------------------------------------------------------

(ol-leader-keys
  "ff" 'lsp-ivy-workspace-symbol)

;; ---------------------------------------------------------------------------
;; projectile
;; ---------------------------------------------------------------------------

(ol-leader-keys
  "pp" 'projectile-switch-project
  "pd" 'projectile-discover-projects-in-search-path
  "pf" 'counsel-projectile-rg)

(ol-override-key "M-q" 'projectile-find-file)

;; ---------------------------------------------------------------------------
;; Git
;; ---------------------------------------------------------------------------

(ol-leader-keys
  "gs" 'magit-status
  "gb" 'magit-blame-addition
  "gdM" 'ol-diff-main
  "gdH" 'ol-diff-head
  "gdm" 'ol-diff-file-main
  "gdh" 'ol-diff-file-head)

;; ---------------------------------------------------------------------------
;; Org mode
;; ---------------------------------------------------------------------------

(ol-leader-keys
  "os" 'org-babel-demarcate-block :which-key "split code block")

;; ---------------------------------------------------------------------------
;; Misc
;; ---------------------------------------------------------------------------

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(ol-override-key "M-h" 'help-command)
