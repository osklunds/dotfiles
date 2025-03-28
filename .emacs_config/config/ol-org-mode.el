;; -*- lexical-binding: nil -*-

(require 'ol-util)
(require 'ol-evil)
(require 'ol-colors)

(require 'org)
(require 'org-faces)
(require 'color)
(require 'org-indent)

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

;; In the future, org seems to get some setting to set to fill width
(setc org-image-actual-width 600)
(setc org-startup-with-inline-images t)
(setc org-hide-emphasis-markers t)
(setc org-ellipsis " ▾")

(ol-set-face 'org-block :background
             (color-darken-name
              (face-attribute 'default :background) 5))

(add-hook 'org-mode-hook 'variable-pitch-mode)

;; Same font as 'default
(defconst ol-fixed-pitch-font "Source Code Pro")

(set-face-attribute 'fixed-pitch nil :family ol-fixed-pitch-font :height 90)
(set-face-attribute 'line-number nil :family ol-fixed-pitch-font :height 90)
(set-face-attribute 'line-number-current-line nil :family ol-fixed-pitch-font :height 90)

(defconst ol-variable-pitch-font "Open Sans")

(set-face-attribute 'variable-pitch nil
                    :family ol-variable-pitch-font
                    :height 1.1
                    :weight 'normal)

(dolist (face '((org-level-1 . 1.4)
                (org-level-2 . 1.3)
                (org-level-3 . 1.2)
                (org-level-4 . 1.1)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil
                      :font ol-variable-pitch-font
                      :foreground ol-black
                      :weight 'bold
                      :height (cdr face)))

(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))

(set-face-attribute 'org-block nil
                    :foreground nil
                    :inherit 'fixed-pitch
                    :height 1.0)
(set-face-attribute 'org-code nil
                    :inherit '(shadow fixed-pitch)
                    :height 1.0)
(set-face-attribute 'org-indent nil
                    :inherit '(org-hide fixed-pitch)
                    :height 1.0)
(set-face-attribute 'org-verbatim nil
                    :inherit '(shadow fixed-pitch)
                    :height 1.0)
(set-face-attribute 'org-special-keyword nil
                    :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil
                    :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil
                    :inherit 'fixed-pitch)

;; -----------------------------------------------------------------------------
;; Emphasis
;; -----------------------------------------------------------------------------

(defun ol-org-bold ()
  (interactive)
  (org-emphasize ?*))

(ol-evil-define-key 'visual org-mode-map "C-b" 'ol-org-bold)

(defun ol-org-italics ()
  (interactive)
  (org-emphasize ?/))

(ol-evil-define-key 'visual org-mode-map "C-i" 'ol-org-italics)

(defun ol-org-verbatim ()
  (interactive)
  (org-emphasize ?=))

(ol-evil-define-key 'visual org-mode-map "C-v" 'ol-org-verbatim)

(defun ol-org-code ()
  (interactive)
  (org-emphasize ?~))

(ol-evil-define-key 'visual org-mode-map "C-c" 'ol-org-code)

;; todo: in normal, do the above for symbol at point

;; Copied/modified from https://emacs.stackexchange.com/a/59136
(defun ol-org-toggle-emphasis ()
  (interactive)
  (save-match-data
    (if (and (or (org-in-regexp org-emph-re 2) (org-in-regexp org-verbatim-re 2))
             (not (region-active-p)))
        (let ((beg (match-beginning 3))
              (end (match-end 4)))
          (when (and (>= (point) (1- beg))
                     (<= (point) (1+ end)))
            (save-excursion
              (goto-char end)
              (delete-char 1)
              (goto-char beg)
              (delete-char 1))))
      (call-interactively #'org-emphasize))))

(provide 'ol-org-mode)
