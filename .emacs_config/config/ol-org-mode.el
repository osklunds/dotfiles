;; -*- lexical-binding: nil -*-

(require 'ol-util)
(require 'ol-evil)
(require 'ol-colors)
(require 'ol-file)

(require 'benchmark)
(require 'org)
(require 'org-faces)
(require 'org-indent)
(require 'ox)
(require 'color)
(ol-require-external "wget")

;; -----------------------------------------------------------------------------
;; Misc
;; -----------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.org.txt\\'" . org-mode))

(ol-evil-define-key 'visual org-mode-map "g q" 'org-fill-paragraph)
(ol-evil-define-key 'normal org-mode-map "g q q" 'org-fill-paragraph)

(define-abbrev-table 'org-mode-abbrev-table
  '(
    ("src" "#+begin_src @@\n\n#+end_src")
    ("imw" "#+attr_org: :width ")
    ))

;; So that tab completion works (corfu)
;; todo: running it directly doesn't have an effect. If I rerun manually
;; then works as expected. Maybe something loaded afterwards overrides. So
;; use this workaround.
(run-with-timer 0 nil #'ol-evil-define-key 'insert org-mode-map 'tab nil)
(run-with-timer 0 nil #'ol-define-key org-mode-map 'tab 'org-metaright)

;; -----------------------------------------------------------------------------
;; Paragraphs
;; -----------------------------------------------------------------------------

(defvar ol-original-paragraph-start paragraph-start)
(defvar ol-original-paragraph-separate paragraph-separate)

(defun ol-org-set-paragraphs ()
  (setq paragraph-start ol-original-paragraph-start)
  (setq paragraph-separate ol-original-paragraph-separate))

(add-hook 'org-mode-hook #'ol-org-set-paragraphs)

;; -----------------------------------------------------------------------------
;; Headers
;; -----------------------------------------------------------------------------

;; Make imenu-like feature more convenient to use
(setc org-goto-interface 'outline-path-completion)
(setc org-outline-path-complete-in-steps nil)

(ol-evil-define-key 'normal org-mode-map "SPC m s" 'org-goto)

;; Toggle headers
(ol-evil-define-key 'normal org-mode-map 'tab 'org-cycle)

;; -----------------------------------------------------------------------------
;; Lists
;; -----------------------------------------------------------------------------

;; TODO: Do something similar for evil-open, i.e. o
(defun ol-org-return ()
  (interactive)
  (cond
   ((ol-org-in-empty-item-p)
    (beginning-of-line)
    (kill-line)
    (newline))
   ((ol-org-in-item-p) (org-insert-item))
   (t (org-return))))

(defconst ol-org-item-re "^ *\\(-\\|\\([0-9]+.\\)\\)")

(defun ol-org-in-item-p ()
  (string-match-p ol-org-item-re (or (thing-at-point 'line t) "")))

(defun ol-org-in-empty-item-p ()
  (string-match-p (concat ol-org-item-re " *$") (or (thing-at-point 'line t) "")))

(ol-evil-define-key 'insert org-mode-map 'return 'ol-org-return)

;; Indent and deindent lists
(ol-evil-define-key 'insert org-mode-map 'tab 'org-metaright)
(ol-evil-define-key 'insert org-mode-map 'backtab 'org-metaleft)

(font-lock-add-keywords
 'org-mode
 '(("^[[:space:]]*\\(-\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(setc org-list-indent-offset 4)

(add-hook 'org-mode-hook #'org-indent-mode)

;; Needed so that sliced images aren't messed up with tiny white lines
;; between each line
(setc org-indent-indentation-per-level 0)

;; Show all * in headings
(setc org-indent-mode-turns-on-hiding-stars nil)

;; Prevent occasional double newline in lists
(setc org-blank-before-new-entry nil)

;; -----------------------------------------------------------------------------
;; Blocks
;; -----------------------------------------------------------------------------

(ol-set-face 'org-block :background
             (color-darken-name
              (face-attribute 'default :background) 5))

(setc org-src-preserve-indentation t)
(setc org-edit-src-content-indentation 0)

;; -----------------------------------------------------------------------------
;; Emphasis
;; -----------------------------------------------------------------------------

;; todo: can find inspiration from markdown-mode how to do this well
;; todo: can consider making emph evil operators so don't have to
;; be in visual state

(setc org-hide-emphasis-markers t)

(defun ol-org-toggle-hide-emphasis-markers ()
  (interactive)
  (setc org-hide-emphasis-markers (not org-hide-emphasis-markers))
  (ol-save-silently)
  (revert-buffer-quick))

(ol-define-key ol-normal-leader-map "o e" 'ol-org-toggle-hide-emphasis-markers)

;; -----------------------------------------------------------------------------
;; Links
;; -----------------------------------------------------------------------------

(defun ol-org-open-at-point-set-jump (&rest _args)
  (evil-set-jump))

(advice-add 'org-open-at-point :before #'ol-org-open-at-point-set-jump)

(provide 'ol-org-mode)
