
(require 'ol-util)

;; -----------------------------------------------------------------------------
;; Loading evil
;; -----------------------------------------------------------------------------

;; These must be set before evil is loaded
(defvar evil-want-integration t)
(defvar evil-want-keybinding nil)
(defvar evil-respect-visual-line-mode t)

(require 'evil)
(require 'evil-collection)
(require 'evil-nerd-commenter)
(require 'goto-chg)

(evil-mode t)

;; todo: understand all these keys better, including new keymap-set

;; -----------------------------------------------------------------------------
;; Keymap helpers
;; -----------------------------------------------------------------------------

;; To handle both GUI and terminal
;; e.g. <tab> is for GUI and TAB is for terminal
(defun ol-map-key (key)
  (pcase key
    ('return  '("<return>" "RET"))
    ('tab     '("<tab>" "TAB"))
    ('backtab '("<backtab>" "S-TAB"))
    ('c-6     '("C-6" "C-^"))
    (_        `(,key))))

(defun ol-define-key (map key fun)
  "Wrapper around `define-key' that automatically calls `kbd' and handles GUI
and terminal variations."
  (dolist (mapped-key (ol-map-key key))
    (define-key map (kbd mapped-key) fun)))

;; TODO: Delete this
(defun ol-global-set-key (key fun)
  (global-set-key (kbd key) fun))

;; TODO: Can avoid 'normal instead of normal by doing a wrapper macro
;; that calls this fun instead
(defun ol-evil-define-key (state map key fun)
  (dolist (mapped-key (ol-map-key key))
    (evil-define-key state map (kbd mapped-key) fun)))

;; -----------------------------------------------------------------------------
;; Leader keys
;; -----------------------------------------------------------------------------

(defvar ol-normal-leader-map (make-sparse-keymap))
(defvar ol-visual-leader-map (make-sparse-keymap))

(ol-define-key evil-motion-state-map "SPC" ol-normal-leader-map)
(ol-define-key evil-visual-state-map "SPC" ol-visual-leader-map)

(defun ol-define-normal-leader-key (key fun)
  (ol-define-key ol-normal-leader-map key fun))

(defun ol-define-visual-leader-key (key fun)
  (ol-define-key ol-visual-leader-map key fun))

;; -----------------------------------------------------------------------------
;; Override
;; -----------------------------------------------------------------------------

;; Overriding inspired by: https://emacs.stackexchange.com/a/358

(defvar ol-override-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode ol-override-mode
  "Minor mode for overriding keys"
  :init-value t
  :lighter " ol-override-mode"
  :keymap ol-override-mode-map)

;;;###autoload
(define-globalized-minor-mode global-ol-override-mode
  ol-override-mode
  ol-override-mode
  :group 'ol)

(add-to-list 'emulation-mode-map-alists `((ol-override-mode . ,ol-override-mode-map)))

;; Turn off the minor mode in the minibuffer
(defun turn-off-ol-override-mode ()
  (ol-override-mode -1))

(add-hook 'minibuffer-setup-hook #'turn-off-ol-override-mode)

(ol-override-mode t)

(defun ol-override-key (key fun)
  (ol-define-key ol-override-mode-map key fun))

;; -----------------------------------------------------------------------------
;; Main config
;; -----------------------------------------------------------------------------

(evil-set-undo-system 'undo-redo)
(setc evil-want-C-u-scroll t)
(setc evil-search-module 'evil-search)
(setc evil-disable-insert-state-bindings t)
(setc evil-emacs-state-modes nil)
(setc evil-motion-state-modes nil)
(setc evil-insert-state-modes nil)
(setq evil-insert-state-cursor 'box)
(setc evil-want-Y-yank-to-eol t)

;; at least for magit, disabling this enables normal keybinds in buffers.
;; can be something to play around with
(with-eval-after-load 'dired (evil-collection-dired-setup))
(with-eval-after-load 'magit (evil-collection-magit-setup))
(with-eval-after-load 'term (evil-collection-term-setup))
(with-eval-after-load 'ivy (evil-collection-ivy-setup))

;;;; ---------------------------------------------------------------------------
;;;; Words (don't come easy, to me)
;;;;----------------------------------------------------------------------------

;; Make - a word in emacs lisp mode
(add-hook 'emacs-lisp-mode-hook (lambda () (modify-syntax-entry ?- "w")))
;; Make _ a word in all modes
(add-hook 'after-change-major-mode-hook (lambda () (modify-syntax-entry ?_ "w")))

(defun ol-evil-ex-start-word-search-args-advice (_unbounded direction count &optional symbol)
  "Don't add word boundaries when searching for a word."
  `(t ,direction ,count ,symbol))

(advice-add 'evil-ex-start-word-search
            :filter-args
            (lambda (args) (apply 'ol-evil-ex-start-word-search-args-advice args)))

;;;; ---------------------------------------------------------------------------
;;;; Relative line jumps into jump list
;;;;----------------------------------------------------------------------------

(defun ol-evil-line-motion-add-to-jump-list-advice (&optional count)
  (when count
    (evil-set-jump)))

(advice-add 'evil-next-visual-line :before 'ol-evil-line-motion-add-to-jump-list-advice)
(advice-add 'evil-previous-visual-line :before 'ol-evil-line-motion-add-to-jump-list-advice)

;;;; ---------------------------------------------------------------------------
;;;; Operator tweaks
;;;;----------------------------------------------------------------------------

(defun ol-evil-operator-save-point-advice (func &rest args)
  (save-excursion
    (apply func args)))

(advice-add 'evil-indent :around 'ol-evil-operator-save-point-advice)

;; The above advice didn't work for comment operator. Point seems to be changed
;; before the underlying function is called.
(defun ol-evilnc-comment-operator ()
  (interactive)
  (save-excursion
    (call-interactively 'evilnc-comment-operator)))

(ol-evil-define-key 'normal prog-mode-map "gc" 'ol-evilnc-comment-operator)

(defun ol-evilnc-comment-operator-advice (start end type)
  ;; Always set type to 'line
  `(,start ,end 'line))

(advice-add 'evilnc-comment-operator
            :filter-args
            (lambda (args) (apply 'ol-evilnc-comment-operator-advice args)))

;;;; ---------------------------------------------------------------------------
;;;; Keybinds
;;;;----------------------------------------------------------------------------

(ol-define-key evil-normal-state-map "R" nil)

;; Normal state
(ol-define-key evil-insert-state-map "C-n" 'evil-normal-state)
(ol-define-key evil-visual-state-map "C-n" 'evil-normal-state)
(ol-define-key evil-insert-state-map "M-n" 'evil-execute-in-normal-state)
(ol-define-key evil-ex-completion-map "C-n" 'abort-recursive-edit)
(ol-define-key evil-ex-search-keymap "C-n" 'abort-recursive-edit)

;; Window movement
(ol-define-key evil-motion-state-map "C-h" #'evil-window-left)
(ol-define-key evil-motion-state-map "C-l" #'evil-window-right)
(ol-define-key evil-insert-state-map "C-h" #'evil-window-left)
(ol-define-key evil-insert-state-map "C-l" #'evil-window-right)

;; Scolling
(ol-define-key evil-motion-state-map "M-j" 'evil-scroll-line-down)
(ol-define-key evil-motion-state-map "M-k" 'evil-scroll-line-up)

;; To work around a bug that prevents yank/delete full lines when doing
;; e.g. d2j. Affects also when a logical line fits the window width.
(ol-define-key evil-operator-state-map "j" 'evil-next-line)
(ol-define-key evil-operator-state-map "k" 'evil-previous-line)

;; I think this mapping makes more sense, to align down/j/{ and up/k/}
(ol-define-key evil-motion-state-map "}" 'evil-backward-paragraph)
(ol-define-key evil-motion-state-map "{" 'evil-forward-paragraph)

;; Movement
(ol-define-key evil-motion-state-map ")" 'evil-end-of-line-or-visual-line)
(ol-define-key evil-motion-state-map "^" 'evil-beginning-of-visual-line)
(ol-define-key evil-motion-state-map "0" 'evil-first-non-blank-of-visual-line) ;; Unclear why this isn't enough
(ol-define-key evil-normal-state-map "0" 'evil-first-non-blank-of-visual-line)
(ol-define-key evil-visual-state-map "0" 'evil-first-non-blank-of-visual-line)
(ol-define-key evil-operator-state-map "0" 'evil-first-non-blank-of-visual-line)

(ol-define-key evil-insert-state-map 'tab 'ol-insert-tab)

;;;; ---------------------------------------------------------------------------
;;;; go to definition
;;;;----------------------------------------------------------------------------

(setc evil-goto-definition-functions '(evil-goto-definition-xref))

;;;; ---------------------------------------------------------------------------
;;;; Subword
;;;;----------------------------------------------------------------------------

;; TODO: Handle elisp having special symbols
;; TODO: Handle camel case
;; TODO: Probably re-use thing mechanics
;; TODO: forward and backward not consistent with beginning and end

(defun ol-subword-forward ()
  (interactive)
  (ol-subword 1))

(defun ol-subword-backward ()
  (interactive)
  (ol-subword -1))

(defun ol-subword (count)
  (let ((regexp (if (looking-at-p "[a-zA-Z0-9]")
                    "[^a-zA-Z0-9]"
                  "[a-zA-Z0-9]")))
    (search-forward-regexp regexp nil nil count))
  (when (> count 0)
    (backward-char count)))

(ol-define-key evil-motion-state-map "L" 'ol-subword-forward)
(ol-define-key evil-motion-state-map "H" 'ol-subword-backward)


(provide 'ol-evil)
