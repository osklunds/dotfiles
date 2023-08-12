
(load-theme 'doom-one-light t)

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defun ol-set-face-attribute (face &rest properties)
  (apply 'set-face-attribute (append (list face nil) properties)))

;; TODO: unset all properties (foreground etc...) the proper way
(defun ol-inherit-face-attribute (face-to-update face-to-inherit-from)
  (ol-set-face-attribute face-to-update
                         :inherit face-to-inherit-from
                         :foreground nil
                         :background nil))

;; -----------------------------------------------------------------------------
;; General
;; -----------------------------------------------------------------------------

(if (ol-is-mac)
    (ol-set-face-attribute 'default :height 110)
  (ol-set-face-attribute 'default :height 90))

(ol-set-face-attribute 'default :foreground "#000000" :background "#ffffff")
(ol-set-face-attribute 'font-lock-comment-face :foreground "#50a14f")
(ol-set-face-attribute 'font-lock-string-face :foreground "#d78700")

(ol-set-face-attribute 'magit-blame-margin :background "#e4e4e4")

(ol-set-face-attribute 'org-block :background
                    (color-darken-name
                     (face-attribute 'default :background) 3))

;; Hack to do it like this. If done directly, colors aren't set it seems
(defun ol-set-term-colors ()
  ;; TODO Do this:
  ;; https://emacs.stackexchange.com/questions/28825/how-do-you-set-colors-for-term
  (ol-inherit-face-attribute 'term-color-black 'default)
  (ol-inherit-face-attribute 'term 'default))

(add-hook 'term-mode-hook 'ol-set-term-colors)

(dolist (face '(doom-modeline-evil-normal-state
                doom-modeline-evil-insert-state
                doom-modeline-evil-visual-state
                doom-modeline-evil-emacs-state))
  (ol-set-face-attribute face :weight 'bold))

(ol-set-face-attribute 'mode-line
                    :background "#D7E4E8"
                    :overline nil
                    :underline nil)

(ol-set-face-attribute 'mode-line-inactive
                    :background "#E9EDED"
                    :overline nil
                    :underline nil)

;; ---------------------------------------------------------------------------
;; vdiff
;; ---------------------------------------------------------------------------

(ol-inherit-face-attribute 'vdiff-closed-fold-face 'magit-diff-hunk-heading-highlight)

(defun ol-vdiff-set-face (face-to-set face-to-copy-from)
  (let ((face-val (face-attribute face-to-copy-from :background)))
    (ol-set-face-attribute face-to-set
                           :inherit nil
                           :extend t
                           :background face-val
                           :foreground nil)))

(ol-vdiff-set-face 'vdiff-addition-face 'magit-diff-added-highlight)
(ol-vdiff-set-face 'vdiff-refine-added 'magit-diff-added-highlight)
(ol-vdiff-set-face 'vdiff-change-face 'magit-diff-base)
(ol-vdiff-set-face 'vdiff-refine-changed 'magit-diff-base-highlight)
(ol-vdiff-set-face 'vdiff-subtraction-face 'magit-diff-removed-highlight)

;; ---------------------------------------------------------------------------
;; ediff
;; ---------------------------------------------------------------------------

;; These actually made some more sense once I understood them. In ediff, there's
;; a "current" diff, and "other" diffs. The currently selected diff is
;; highlighted using these "current" faces below. The non-selected other diffs
;; are highlighted alternatingly with the odd and even faces.

(ol-inherit-face-attribute 'ediff-current-diff-A        'magit-diff-removed)
(ol-inherit-face-attribute 'ediff-current-diff-B        'magit-diff-added)
(ol-inherit-face-attribute 'ediff-current-diff-C        'magit-diff-added)
(ol-inherit-face-attribute 'ediff-current-diff-Ancestor 'magit-diff-base)

(ol-inherit-face-attribute 'ediff-fine-diff-A           'magit-diff-removed-highlight)
(ol-inherit-face-attribute 'ediff-fine-diff-B           'magit-diff-added-highlight)
(ol-inherit-face-attribute 'ediff-fine-diff-C           'magit-diff-added-highlight)
(ol-inherit-face-attribute 'ediff-fine-diff-Ancestor    'magit-diff-base-highlight)

(ol-inherit-face-attribute 'ediff-even-diff-A           'magit-diff-removed)
(ol-inherit-face-attribute 'ediff-even-diff-B           'magit-diff-added)
(ol-inherit-face-attribute 'ediff-even-diff-C           'magit-diff-added)
(ol-inherit-face-attribute 'ediff-even-diff-Ancestor    'magit-diff-base)

(ol-inherit-face-attribute 'ediff-odd-diff-A            'magit-diff-removed)
(ol-inherit-face-attribute 'ediff-odd-diff-B            'magit-diff-added)
(ol-inherit-face-attribute 'ediff-odd-diff-C            'magit-diff-added)
(ol-inherit-face-attribute 'ediff-odd-diff-Ancestor     'magit-diff-base)
