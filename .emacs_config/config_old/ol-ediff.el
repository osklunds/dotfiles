
;; This file is deprecated now that I use vdiff

(require 'ediff)
(require 'ol-util)

(setc ediff-window-setup-function 'ediff-setup-windows-plain)
(setc ediff-split-window-function 'split-window-horizontally)

;; Copied from https://emacs.stackexchange.com/a/24602
(defun ol-disable-y-or-n-p (orig-fun &rest args)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (apply orig-fun args)))

(advice-add 'ediff-quit :around #'ol-disable-y-or-n-p)

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
