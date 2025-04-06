;; -*- lexical-binding: nil -*-

(require 'ol-util)
(require 'ol-evil)
(require 'ol-colors)

(require 'org)
(require 'org-faces)
(require 'color)
;; (require 'org-indent) if included, line-spacing causes issues for sliced images
(require 'org-sliced-images)

;; todo: understand how to open links

(add-to-list 'auto-mode-alist '("\\.org.txt\\'" . org-mode))

(setc org-src-preserve-indentation t)
(setc org-edit-src-content-indentation 0)

;; Make imenu-like feature more convenient to use
(setc org-goto-interface 'outline-path-completion)
(setc org-outline-path-complete-in-steps nil)

(ol-evil-define-key 'visual org-mode-map "g q" 'org-fill-paragraph)
(ol-evil-define-key 'normal org-mode-map "g q q" 'org-fill-paragraph)

(ol-evil-define-key 'normal org-mode-map "SPC m s" 'org-goto)

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
(setc org-ellipsis " ▾")

;; So that an image is more than just one line, makes scrolling much better
;; as images can be partially hidden
(org-sliced-images-mode)

(ol-set-face 'org-block :background
             (color-darken-name
              (face-attribute 'default :background) 5))

(add-hook 'org-mode-hook 'variable-pitch-mode)
;; Unfortunately, if line numbers are enabled line-spacing causes issues for sliced images
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

(defun ol-font-available-p (font)
  (when (cl-member font (font-family-list) :test 'string-equal)
    font))

;; todo: check in fonts
(defconst ol-fixed-pitch-font (or (ol-font-available-p "Source Code Pro")
                                  "DejaVu Sans Mono"))

(set-face-attribute 'default nil :family "Source Code Pro" :height 90)
(set-face-attribute 'fixed-pitch nil :family "Source Code Pro" :height 90)
(set-face-attribute 'line-number nil :family ol-fixed-pitch-font :height 90)
(set-face-attribute 'line-number-current-line nil :family ol-fixed-pitch-font :height 90)

(defconst ol-variable-pitch-font (or (ol-font-available-p "Open Sans")
                                     "DejaVu Sans"))

(set-face-attribute 'variable-pitch nil
                    :family ol-variable-pitch-font
                    :height 1.1
                    :weight 'normal)

(dolist (face '((org-level-1 . 1.5)
                (org-level-2 . 1.4)
                (org-level-3 . 1.3)
                (org-level-4 . 1.2)
                (org-level-5 . 1.2)
                (org-level-6 . 1.2)
                (org-level-7 . 1.2)
                (org-level-8 . 1.2)))
  (set-face-attribute (car face) nil
                      :font ol-variable-pitch-font
                      :foreground ol-black
                      :weight 'bold
                      :height (cdr face)))

(set-face-attribute 'org-block nil
                    :foreground 'unspecified
                    :inherit 'fixed-pitch
                    :height 1.1)
(set-face-attribute 'org-code nil
                    :inherit '(shadow fixed-pitch)
                    :height 1.1)
(set-face-attribute 'org-verbatim nil
                    :inherit '(shadow fixed-pitch)
                    :height 1.1)
(set-face-attribute 'org-special-keyword nil
                    :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil
                    :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil
                    :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil
                    :foreground ol-black
                    :inherit 'fixed-pitch)
;; To fix whitespace in table
(set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
;; -----------------------------------------------------------------------------
;; Emphasis
;; -----------------------------------------------------------------------------

;; todo: can find inspiration from markdown-mode how to do this well

(setc org-hide-emphasis-markers t)

;; todo: doesn't work when EOL
;; Copied/modified from https://emacs.stackexchange.com/a/59136
(defun ol-org-toggle-emphasis (char)
  (save-match-data
    ;; If inside some emphasis, delete it, and then toggle again if different char
    (if (and (or (org-in-regexp org-emph-re 2) (org-in-regexp org-verbatim-re 2))
             (not (region-active-p)))
        (let ((beg (match-beginning 3))
              (end (match-end 4))
              (same-char nil))
          (when (and (>= (point) (1- beg))
                     (<= (point) (1+ end)))
            (save-excursion
              (goto-char end)
              (setq same-char (eq char (char-after)))
              (delete-char 1)
              (goto-char beg)
              (delete-char 1))
            (unless same-char
              (ol-org-toggle-emphasis char))))
      ;; If not inside emphasis, emphasize until space char
      (save-excursion
        (re-search-backward " ")
        (forward-char)
        (let ((inhibit-message t))
          (set-mark-command nil))
        (re-search-forward " " nil nil 1)
        (backward-char)
        (setq deactivate-mark nil)
        (org-emphasize char)
        (deactivate-mark)
        ))))

(ol-evil-define-key 'normal org-mode-map "M-b"
                    (lambda () (interactive) (ol-org-toggle-emphasis ?*)))
(ol-evil-define-key 'normal org-mode-map "M-i"
                    (lambda () (interactive) (ol-org-toggle-emphasis ?/)))
(ol-evil-define-key 'normal org-mode-map "M-v"
                    (lambda () (interactive) (ol-org-toggle-emphasis ?=)))
(ol-evil-define-key 'normal org-mode-map "M-c"
                    (lambda () (interactive) (ol-org-toggle-emphasis ?~)))
(ol-evil-define-key 'normal org-mode-map "M-s"
                    (lambda () (interactive) (ol-org-toggle-emphasis ?+)))

;; TODO: how to do emphasis in a good way is not trivial.
(ol-evil-define-key 'visual org-mode-map "M-b"
                    (lambda () (interactive) (org-emphasize ?*)))
(ol-evil-define-key 'visual org-mode-map "M-i"
                    (lambda () (interactive) (org-emphasize ?/)))
(ol-evil-define-key 'visual org-mode-map "M-v"
                    (lambda () (interactive) (org-emphasize ?=)))
(ol-evil-define-key 'visual org-mode-map "M-c"
                    (lambda () (interactive) (org-emphasize ?~)))
(ol-evil-define-key 'visual org-mode-map "M-s"
                    (lambda () (interactive) (org-emphasize ?+)))


(provide 'ol-org-mode)
