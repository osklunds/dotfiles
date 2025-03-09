
(require 'ol-util)
(require 'ol-evil)
(require 'ol-colors)

(require 'org)
(require 'org-faces)
(require 'color)

(setc org-ellipsis " ▾")

(setc org-src-preserve-indentation t)
(setc org-edit-src-content-indentation 0)

(add-to-list 'auto-mode-alist '("\\.org.txt\\'" . org-mode))

(setc org-goto-interface 'outline-path-completion)
(setc org-outline-path-complete-in-steps nil)

;; TODO: Do something similar for evil-open, i.e. o
(defun ol-org-return ()
  (interactive)
  (if (ol-org-in-item-p)
      (org-insert-item)
    (org-return)))

(defun ol-org-in-item-p ()
  (string-match-p "^ *-" (thing-at-point 'line t)))

(ol-set-face 'org-block :background
             (color-darken-name
              (face-attribute 'default :background) 3))

(ol-evil-define-key 'visual org-mode-map "g q" 'org-fill-paragraph)
(ol-evil-define-key 'normal org-mode-map "g q q" 'org-fill-paragraph)

;; Indent and deindent lists
(ol-evil-define-key 'insert org-mode-map 'tab 'org-metaright)
(ol-evil-define-key 'insert org-mode-map 'backtab 'org-metaleft)

;; Toggle headers
(ol-evil-define-key 'normal org-mode-map 'tab 'org-cycle)

(ol-evil-define-key 'insert org-mode-map 'return 'ol-org-return)

;; In the future, org seems to get some setting to set to fill width
(setc org-image-actual-width 600)

(define-abbrev-table 'org-mode-abbrev-table
  '(
    ("src" "#+BEGIN_SRC @@\n\n#+END_SRC")
    ))

(provide 'ol-org-mode)
