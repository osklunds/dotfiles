;; -*- lexical-binding: nil -*-

(require 'ol-util)
(require 'ol-evil)
(require 'ol-colors)

(require 'org)
(require 'org-faces)
(require 'color)

(add-to-list 'auto-mode-alist '("\\.org.txt\\'" . org-mode))

(setc org-src-preserve-indentation t)
(setc org-edit-src-content-indentation 0)

;; Make imenu-like feature more convenient to use
(setc org-goto-interface 'outline-path-completion)
(setc org-outline-path-complete-in-steps nil)

(ol-evil-define-key 'visual org-mode-map "g q" 'org-fill-paragraph)
(ol-evil-define-key 'normal org-mode-map "g q q" 'org-fill-paragraph)

;; Toggle headers
(ol-evil-define-key 'normal org-mode-map 'tab 'org-cycle)

;; In the future, org seems to get some setting to set to fill width
(setc org-image-actual-width 600)

(define-abbrev-table 'org-mode-abbrev-table
  '(
    ("src" "#+BEGIN_SRC @@\n\n#+END_SRC")
    ))

;; -----------------------------------------------------------------------------
;; Lists
;; -----------------------------------------------------------------------------

;; TODO: Do something similar for evil-open, i.e. o
(defun ol-org-return ()
  (interactive)
  (if (ol-org-in-item-p)
      (org-insert-item)
    (org-return)))

(defun ol-org-in-item-p ()
  (string-match-p "^ *-" (thing-at-point 'line t)))

(ol-evil-define-key 'insert org-mode-map 'return 'ol-org-return)

;; Indent and deindent lists
(ol-evil-define-key 'insert org-mode-map 'tab 'org-metaright)
(ol-evil-define-key 'insert org-mode-map 'backtab 'org-metaleft)

;; -----------------------------------------------------------------------------
;; Looks
;; -----------------------------------------------------------------------------
;; Many things inspired by https://sophiebos.io/posts/prettifying-emacs-org-mode/

(setc org-ellipsis " ▾")

(ol-set-face 'org-block :background
             (color-darken-name
              (face-attribute 'default :background) 3))

(add-hook 'org-mode-hook 'variable-pitch-mode)

(set-face-attribute 'fixed-pitch nil :family "Source Code Pro" :height 90)
(set-face-attribute 'line-number nil :family "Source Code Pro" :height 90)
(set-face-attribute 'line-number-current-line nil :family "Source Code Pro" :height 90)
(set-face-attribute 'variable-pitch nil :family "Open Sans" :height 1.1 :weight 'normal)

(dolist (face '((org-level-1 . 1.4)
                (org-level-2 . 1.3)
                (org-level-3 . 1.2)
                (org-level-4 . 1.1)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil
                      :font "Source Sans Pro"
                      :foreground ol-black
                      :weight 'bold
                      :height (cdr face)))

(require 'org-indent)
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))

(set-face-attribute 'org-block nil            :foreground nil :inherit 'fixed-pitch :height 1.0)
(set-face-attribute 'org-code nil             :inherit '(shadow fixed-pitch) :height 1.0)
(set-face-attribute 'org-indent nil           :inherit '(org-hide fixed-pitch) :height 1.0)
(set-face-attribute 'org-verbatim nil         :inherit '(shadow fixed-pitch) :height 1.0)
(set-face-attribute 'org-special-keyword nil  :inherit '(font-lock-comment-face
                                                         fixed-pitch))
(set-face-attribute 'org-meta-line nil        :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil         :inherit 'fixed-pitch)

(setc org-hide-emphasis-markers t)

(provide 'ol-org-mode)
