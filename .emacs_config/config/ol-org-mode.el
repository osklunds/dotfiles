;; -*- lexical-binding: nil -*-

(require 'ol-util)
(require 'ol-evil)
(require 'ol-colors)

(require 'org)
(require 'org-faces)
(require 'org-indent)
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

(add-hook 'org-mode-hook #'org-indent-mode)

;; Needed so that sliced images aren't messed up with tiny white lines
;; between each line
(setc org-indent-indentation-per-level 0)

;; Show all * in headings
(setc org-indent-mode-turns-on-hiding-stars nil)

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

;; Need to keep height 1.0 to not mess up table alignment
(set-face-attribute 'org-block nil
                    :foreground 'unspecified
                    :inherit 'fixed-pitch
                    :height 1.0)
(set-face-attribute 'org-code nil
                    :inherit '(shadow fixed-pitch)
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
(set-face-attribute 'org-table nil
                    :foreground ol-black
                    :height 1.0
                    :inherit 'fixed-pitch)
;; To fix whitespace in table
(set-face-attribute 'org-formula nil :inherit 'fixed-pitch)

;; -----------------------------------------------------------------------------
;; Images
;; -----------------------------------------------------------------------------

(setc org-startup-with-inline-images t)

;; So that an image is more than just one line, makes scrolling much better
;; as images can be partially hidden
(org-sliced-images-mode)

;; Unfortunately, if line numbers are enabled line-spacing causes issues for sliced images
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

;; Set to nil so that the :width attribute is used in
;; #'org-display-inline-image--width
(setc org-image-actual-width nil)

;; Always clamp at 'fill-column as max width, but try to use :width if exists
(defun ol-org-display-inline-image--width-advice (old-fun &rest args)
  (let* ((width (apply old-fun args))
         (max-width (* fill-column (frame-char-width (selected-frame)))))
    (min max-width (or width max-width))))

(advice-add 'org-display-inline-image--width :around
            #'ol-org-display-inline-image--width-advice)

;; About width: can use ATTR_ORG: :width 50% etc to have relative fill-column

;;;; ---------------------------------------------------------------------------
;;;; Insertion
;;;; ---------------------------------------------------------------------------

(defun ol-org-insert-image (in-file)
  (unless (eq major-mode 'org-mode)
    (user-error "Only works in org-mode"))
  (unless buffer-file-name
    (user-error "buffer-file-name nil"))
  (unless (file-exists-p in-file)
    (user-error "in-file doesn't exist"))
  (let* ((ext (file-name-extension in-file))
         (date (format-time-string "%Y-%m-%d_%H:%M:%S"))
         (default-out-file
          (file-name-concat
           ""
           (concat (file-name-nondirectory buffer-file-name) ".images")
           (concat date (if ext (concat "." ext) ""))))
         (prompt (format "Save image as (default: %s): " default-out-file))
         (out-file (read-string
                    prompt
                    nil ;; initial-input
                    'ol-org-insert-image ;; history
                    default-out-file)))
    (when (directory-name-p out-file)
      (user-error "out-file is a dir-name"))
    (ol-create-dirs-if-needed (file-name-directory out-file))
    (copy-file in-file out-file)))

(defun ol-create-dirs-if-needed (dir)
  (unless (directory-name-p dir)
    (error "Not a directory name"))
  (ol-create-dirs-if-needed-1 (file-truename dir)))

(defun ol-create-dirs-if-needed-1 (dir)
  (unless (file-exists-p dir)
    (let* ((parent (file-name-directory (directory-file-name dir))))
      (ol-create-dirs-if-needed-1 parent)
      (make-directory dir))))

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
