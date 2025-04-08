;; -*- lexical-binding: nil -*-

(require 'ol-util)
(require 'ol-evil)
(require 'ol-colors)

(require 'org)
(require 'org-faces)
(require 'color)
(require 'org-sliced-images)
(require 'olivetti)

;; -----------------------------------------------------------------------------
;; Misc
;; -----------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.org.txt\\'" . org-mode))

(ol-evil-define-key 'visual org-mode-map "g q" 'org-fill-paragraph)
(ol-evil-define-key 'normal org-mode-map "g q q" 'org-fill-paragraph)

(define-abbrev-table 'org-mode-abbrev-table
  '(
    ("src" "#+BEGIN_SRC @@\n\n#+END_SRC")
    ))

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
    (message "oskar: %s" "empty")
    (beginning-of-line)
    (kill-line)
    (newline))
   ((ol-org-in-item-p)
    (message "oskar: %s" "item")
    (org-insert-item))
   (t
    (message "oskar: %s" "other")
    (org-return))))

(defun ol-org-in-item-p ()
  (string-match-p "^ *-" (thing-at-point 'line t)))

(defun ol-org-in-empty-item-p ()
  (string-match-p "^ *- *$" (thing-at-point 'line t)))

(ol-evil-define-key 'insert org-mode-map 'return 'ol-org-return)

;; Indent and deindent lists
(ol-evil-define-key 'insert org-mode-map 'tab 'org-metaright)
(ol-evil-define-key 'insert org-mode-map 'backtab 'org-metaleft)

(font-lock-add-keywords
 'org-mode
 '(("^[[:space:]]*\\(-\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(setc org-list-indent-offset 4)

;; -----------------------------------------------------------------------------
;; Fonts
;; -----------------------------------------------------------------------------
;; Many things inspired by https://sophiebos.io/posts/prettifying-emacs-org-mode/

(add-hook 'org-mode-hook 'variable-pitch-mode)

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
;; Images
;; -----------------------------------------------------------------------------

;; In the future, org seems to get some setting to set to fill width
(setc org-image-actual-width 600)
(setc org-startup-with-inline-images t)

;; So that an image is more than just one line, makes scrolling much better
;; as images can be partially hidden
(org-sliced-images-mode)

;; Unfortunately, if line numbers are enabled line-spacing causes issues for sliced images
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

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

(setc org-hide-emphasis-markers t)

(defun ol-org-toggle-hide-emphasis-markers ()
  (interactive)
  (setc org-hide-emphasis-markers (not org-hide-emphasis-markers))
  (ol-save-silently)
  (revert-buffer-quick))

(ol-define-key ol-normal-leader-map "o e" 'ol-org-toggle-hide-emphasis-markers)

(defconst ol-emphasis-border-regex " \\|,\\|\n\\|\\.")

;; Copied/modified from https://emacs.stackexchange.com/a/59136
(defun ol-org-toggle-emphasis (char)
  ;; If at space, don't allow toggling. But then move one forward, because if
  ;; at first char of word after another emphasized word, org-in-regexp returns t
  (unless (looking-at-p " ")
    (forward-char)
    ;; save-excursion doesn't work, similar issue here:
    ;; https://www.reddit.com/r/emacs/comments/s89ak1/help_understanding_saveexcursion/
    ;; But point needs to be adjusted with 1 anyway.
    (let ((point-pos (point)))
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
                (if same-char
                    ;; Only compensate if not toggling again
                    (goto-char (1- point-pos))
                  (ol-org-toggle-emphasis char))))
          ;; If not inside emphasis, emphasize until space char
          (re-search-backward ol-emphasis-border-regex)
          (forward-char)
          (let ((inhibit-message t))
            (set-mark-command nil))
          (re-search-forward ol-emphasis-border-regex nil nil 1)
          (backward-char)
          (setq deactivate-mark nil)
          (org-emphasize char)
          (deactivate-mark)
          (goto-char (1+ point-pos)))))
    ;; Compensate for the initial forward-char
    (backward-char)))

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
(ol-evil-define-key 'normal org-mode-map "M-l"
                    (lambda () (interactive) (ol-org-toggle-emphasis ?_)))

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
(ol-evil-define-key 'visual org-mode-map "M-l"
                    (lambda () (interactive) (org-emphasize ?_)))

;; -----------------------------------------------------------------------------
;; Links
;; -----------------------------------------------------------------------------

(defun ol-org-open-at-point-set-jump (&rest _args)
  (evil-set-jump))

(advice-add 'org-open-at-point :before #'ol-org-open-at-point-set-jump)

;; -----------------------------------------------------------------------------
;; Olivetti
;; -----------------------------------------------------------------------------

(add-hook 'org-mode-hook #'olivetti-mode)

(setc olivetti-style nil)

(provide 'ol-org-mode)
