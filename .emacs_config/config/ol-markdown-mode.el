;; -*- lexical-binding: t -*-

(require 'ol-org-mode)

(require 'markdown-mode)

;; todo: understand how to open links

;; -----------------------------------------------------------------------------
;; Looks
;; -----------------------------------------------------------------------------

(setc markdown-hide-markup t)
(setc markdown-list-item-bullets '("•"))

(add-hook 'markdown-mode-hook 'variable-pitch-mode)

(set-face-attribute 'markdown-header-face nil
                    :font ol-variable-pitch-font
                    :foreground ol-black
                    :weight 'bold)

(dolist (face '((markdown-header-face-1 . 1.5)
                (markdown-header-face-2 . 1.4)
                (markdown-header-face-3 . 1.3)
                (markdown-header-face-4 . 1.2)
                (markdown-header-face-5 . 1.2)
                (markdown-header-face-6 . 1.2)))
  (set-face-attribute (car face) nil
                      :height (cdr face)))

(set-face-attribute 'markdown-bold-face nil
                    :foreground 'unspecified)
(set-face-attribute 'markdown-italic-face nil
                    :foreground 'unspecified)
(set-face-attribute 'markdown-code-face nil
                    :background 'unspecified
                    :inherit '(org-code))
(set-face-attribute 'markdown-strike-through-face nil
                    :inherit '(variable-pitch)
                    :background 'unspecified)
(set-face-attribute 'markdown-pre-face nil
                    :foreground 'unspecified
                    :inherit '(org-verbatim))
(set-face-attribute 'markdown-list-face nil
                    :foreground 'unspecified)

(defun ol-dont-hide-markup-for-headings (func &rest args)
  (let ((markdown-hide-markup nil))
    (apply func args)))

(advice-add 'markdown-fontify-headings :around #'ol-dont-hide-markup-for-headings)

;; -----------------------------------------------------------------------------
;; Emphasis
;; -----------------------------------------------------------------------------

(ol-evil-define-key 'normal markdown-mode-map "( b" #'markdown-insert-bold)
(ol-evil-define-key 'normal markdown-mode-map "( i" #'markdown-insert-italic)
(ol-evil-define-key 'normal markdown-mode-map "( c" #'markdown-insert-code)
(ol-evil-define-key 'normal markdown-mode-map "( s" #'markdown-insert-strike-through)

(provide 'ol-markdown-mode)
