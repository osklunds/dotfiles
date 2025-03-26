;; -*- lexical-binding: t -*-

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

(setc org-ellipsis " ▾")

(ol-set-face 'org-block :background
             (color-darken-name
              (face-attribute 'default :background) 3))

(provide 'ol-org-mode)
