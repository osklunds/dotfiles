
(load-theme 'doom-one-light t)

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defun ol-set-face (face &rest properties)
  (apply 'set-face-attribute (append (list face nil) properties)))

(defun ol-copy-face (to attribute from)
  (ol-set-face to attribute (face-attribute from attribute)))

(defun ol-copy-face-fg-bg (to from)
  (ol-copy-face to :foreground from)
  (ol-copy-face to :background from))

;; -----------------------------------------------------------------------------
;; General
;; -----------------------------------------------------------------------------

(ol-set-face 'default :height 90)

(defconst ol-white "#ffffff") ;; ff works better than white in terminal
(defconst ol-black "#000000")

(ol-set-face 'default :foreground ol-black :background ol-white)
(ol-set-face 'font-lock-comment-face :foreground "#5f8700")
(ol-set-face 'font-lock-string-face :foreground "#d78700")

(unless (display-graphic-p)
  (ol-set-face 'hl-line :background "#eeeeee"))

;; -----------------------------------------------------------------------------
;; Evil
;;------------------------------------------------------------------------------

(unless (display-graphic-p)
  (ol-set-face 'lazy-highlight :background "#c2d3f7" :foreground ol-white))

;; -----------------------------------------------------------------------------
;; Org mode
;;------------------------------------------------------------------------------

(ol-set-face 'org-block :background
             (color-darken-name
              (face-attribute 'default :background) 3))

;; -----------------------------------------------------------------------------
;; Terminal
;;------------------------------------------------------------------------------

;; Hack to do it like this. If done directly, colors aren't set it seems
(defun ol-set-term-colors ()
  ;; TODO Do this, setting all colors:
  ;; https://emacs.stackexchange.com/questions/28825/how-do-you-set-colors-for-term
  (ol-set-face 'term-color-black :foreground ol-black :background ol-white)
  (ol-set-face 'term :foreground ol-black :background ol-white))

(add-hook 'term-mode-hook 'ol-set-term-colors)

;; -----------------------------------------------------------------------------
;; Modeline
;;------------------------------------------------------------------------------

(ol-set-face 'mode-line
             :overline 'unspecified
             :underline 'unspecified)

(if (display-graphic-p)
    (ol-set-face 'mode-line :background "#d7e4e8")
  (ol-set-face 'mode-line :background "#cccccc"))

(ol-set-face 'mode-line-inactive
             :background "#e9eded"
             :overline 'unspecified
             :underline 'unspecified)

(ol-set-face 'ol-buffer-name-mode-line-face
             :weight 'bold)

(ol-copy-face-fg-bg 'ol-evil-normal-state-mode-line-face 'font-lock-comment-face)
(ol-copy-face-fg-bg 'ol-evil-insert-state-mode-line-face 'font-lock-keyword-face)
(ol-copy-face-fg-bg 'ol-evil-visual-state-mode-line-face 'warning)
(ol-copy-face-fg-bg 'ol-evil-emacs-state-mode-line-face 'font-lock-builtin-face)

(dolist (face '(ol-evil-normal-state-mode-line-face
                ol-evil-insert-state-mode-line-face
                ol-evil-visual-state-mode-line-face
                ol-evil-emacs-state-mode-line-face
                ol-evil-operator-state-mode-line-face))
  (ol-set-face face :weight 'bold))

;; -----------------------------------------------------------------------------
;; Ivy and company consistency
;;------------------------------------------------------------------------------

(dolist (face '(ivy-minibuffer-match-face-1
                ivy-minibuffer-match-face-2
                ivy-minibuffer-match-face-3
                ivy-minibuffer-match-face-4))
  (ol-copy-face-fg-bg face 'company-tooltip-common)
  (ol-set-face face :weight 'bold)
  (ol-set-face face :background ol-white))

(defconst ol-completion-selection-color "#d7e4e8")

(ol-set-face 'ivy-current-match :weight 'bold)
(ol-set-face 'ivy-current-match :background ol-completion-selection-color)

(ol-set-face 'company-box-background :background ol-white)
(ol-set-face 'company-box-selection :background ol-completion-selection-color)

(setc swiper-faces '(swiper-match-face-1
                     swiper-match-face-2
                     swiper-match-face-2
                     swiper-match-face-2))

(setq swiper-background-faces '(swiper-background-match-face-1
                                swiper-background-match-face-2
                                swiper-background-match-face-2
                                swiper-background-match-face-2))

;; ---------------------------------------------------------------------------
;; Magit
;; ---------------------------------------------------------------------------

(ol-set-face 'magit-blame-margin :background "#e4e4e4")
(ol-set-face 'magit-log-date :foreground "#da8548")

(defconst ol-diff-green "#9fec9d")
(defconst ol-diff-dark-red "#e45649")
(defconst ol-diff-light-red "#f5d9d6")
(defconst ol-diff-dark-orange "#ffd787")
(defconst ol-diff-light-orange "#f6eee8")

(defun ol-magit-diff-set-face (face-to-set face-val)
  (ol-set-face face-to-set
               :background face-val
               :foreground 'unspecified))

(unless (display-graphic-p)
  (ol-magit-diff-set-face 'magit-diff-added-highlight   ol-diff-green)
  (ol-magit-diff-set-face 'magit-diff-added             ol-diff-green)
  (ol-magit-diff-set-face 'magit-diff-base-highlight    ol-diff-dark-orange)
  (ol-magit-diff-set-face 'magit-diff-base              ol-diff-dark-orange)
  (ol-magit-diff-set-face 'magit-diff-removed-highlight ol-diff-light-red)
  (ol-magit-diff-set-face 'magit-diff-removed           ol-diff-light-red))

;; ---------------------------------------------------------------------------
;; vdiff
;; ---------------------------------------------------------------------------

(ol-copy-face-fg-bg 'vdiff-closed-fold-face 'magit-diff-hunk-heading-highlight)

(defun ol-vdiff-set-face (face-to-set face-val)
  (ol-set-face face-to-set
               :inherit nil
               :extend t
               :background face-val
               :foreground 'unspecified))

;; Add
(ol-vdiff-set-face 'vdiff-addition-face ol-diff-green)
(ol-vdiff-set-face 'vdiff-refine-added ol-diff-green)

;; Delete
(ol-vdiff-set-face 'vdiff-subtraction-face ol-diff-dark-red)

;; Change
(ol-vdiff-set-face 'vdiff-refine-changed ol-diff-dark-orange)
(ol-vdiff-set-face 'vdiff-change-face ol-diff-light-orange)

;; ---------------------------------------------------------------------------
;; smerge
;; ---------------------------------------------------------------------------

(defun ol-smerge-set-face (face-to-set face-val)
  (ol-set-face face-to-set :background face-val :foreground ol-black :weight 'normal))

(ol-smerge-set-face 'smerge-base ol-diff-dark-orange)
(ol-smerge-set-face 'smerge-lower ol-diff-light-red)
(ol-smerge-set-face 'smerge-upper ol-diff-green)

;; ---------------------------------------------------------------------------
;; ediff
;; ---------------------------------------------------------------------------

;; These actually made some more sense once I understood them. In ediff, there's
;; a "current" diff, and "other" diffs. The currently selected diff is
;; highlighted using these "current" faces below. The non-selected other diffs
;; are highlighted alternatingly with the odd and even faces.

(ol-copy-face-fg-bg 'ediff-current-diff-A        'magit-diff-removed)
(ol-copy-face-fg-bg 'ediff-current-diff-B        'magit-diff-added)
(ol-copy-face-fg-bg 'ediff-current-diff-C        'magit-diff-added)
(ol-copy-face-fg-bg 'ediff-current-diff-Ancestor 'magit-diff-base)

(ol-copy-face-fg-bg 'ediff-fine-diff-A           'magit-diff-removed-highlight)
(ol-copy-face-fg-bg 'ediff-fine-diff-B           'magit-diff-added-highlight)
(ol-copy-face-fg-bg 'ediff-fine-diff-C           'magit-diff-added-highlight)
(ol-copy-face-fg-bg 'ediff-fine-diff-Ancestor    'magit-diff-base-highlight)

(ol-copy-face-fg-bg 'ediff-even-diff-A           'magit-diff-removed)
(ol-copy-face-fg-bg 'ediff-even-diff-B           'magit-diff-added)
(ol-copy-face-fg-bg 'ediff-even-diff-C           'magit-diff-added)
(ol-copy-face-fg-bg 'ediff-even-diff-Ancestor    'magit-diff-base)

(ol-copy-face-fg-bg 'ediff-odd-diff-A            'magit-diff-removed)
(ol-copy-face-fg-bg 'ediff-odd-diff-B            'magit-diff-added)
(ol-copy-face-fg-bg 'ediff-odd-diff-C            'magit-diff-added)
(ol-copy-face-fg-bg 'ediff-odd-diff-Ancestor     'magit-diff-base)
