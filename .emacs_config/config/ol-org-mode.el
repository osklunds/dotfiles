;; -*- lexical-binding: nil -*-

(require 'ol-util)
(require 'ol-evil)
(require 'ol-colors)
(require 'ol-file)

(require 'org)
(require 'org-faces)
(require 'org-indent)
(require 'ox)
(require 'color)
(require 'org-sliced-images)
(require 'olivetti)
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

(defun ol-insert-zero-width-space ()
  (interactive)
  (insert "​"))

(ol-define-key ol-normal-leader-map "o z" #'ol-insert-zero-width-space)

;; -----------------------------------------------------------------------------
;; Paragraphs
;; -----------------------------------------------------------------------------

(defvar ol-original-paragraph-start paragraph-start)
(defvar ol-original-paragraph-separate paragraph-separate)

;; Advice instead of changing definitions in org-mode. If changed,
;; org-insert-item sometimes inserts an extra newline
(defun ol-org-paragraph-advice (oldfun &rest args)
  (let ((paragraph-start ol-original-paragraph-separate)
        (paragraph-separate ol-original-paragraph-separate))
    (apply oldfun args)))
(advice-add 'evil-forward-paragraph :around #'ol-org-paragraph-advice)
(advice-add 'evil-backward-paragraph :around #'ol-org-paragraph-advice)

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
   ((ol-org-in-item-p)
    (org-insert-item))
   (t
    (org-return))))

(defun ol-org-in-item-p ()
  (string-match-p "^ *-" (or (thing-at-point 'line t) "")))

(defun ol-org-in-empty-item-p ()
  (string-match-p "^ *- *$" (or (thing-at-point 'line t) "")))

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
;; Try to parse attr_org manually because the first time an org file is
;; opened, old-fun doesn't seem to return the value specifed by attr_org. Only
;; handle the relative width case, because that's what I use most of the time.
(defun ol-org-display-inline-image--width-advice (old-fun link)
  (let* ((rel-width (ol-org-rel-image-width link))
         (width (funcall old-fun link))
         (max-width (* (min fill-column (- (window-total-width) 10))
                       (frame-char-width (selected-frame)))))
    (if rel-width
        (round (* rel-width max-width))
      (min max-width (or width max-width)))))

(defun ol-org-rel-image-width (link)
  (when-let* ((paragraph (org-element-lineage link 'paragraph))
              (attr-width (org-export-read-attribute :attr_org paragraph :width))
              (matched (ol-regexp-group "\\([0-9.]+\\)%" attr-width 1))
              (parsed (string-to-number matched)))
    (/ parsed 100.0)))

(advice-add 'org-display-inline-image--width :around
            #'ol-org-display-inline-image--width-advice)

;; About width: can use ATTR_ORG: :width 50% etc to have relative fill-column
;; But not with emacs -Q

;; To avoid issues with newlines etc
(defun ol-org-remove-inline-images ()
  (when (eq major-mode 'org-mode)
    (org-sliced-images-remove-inline-images)))

(add-hook 'before-revert-hook #'ol-org-remove-inline-images)

;;;; ---------------------------------------------------------------------------
;;;; Insertion
;;;; ---------------------------------------------------------------------------

(defun ol-org-insert-image-from-url (&optional url)
  (interactive)
  (let* ((url (or url (read-string
                       "URL of image to insert: "
                       nil ;; initial-input
                       'ol-org-insert-image-from-url))) ;; history
         (out-file (make-temp-file "ol-org-insert-image-from-url-"))
         (result (call-process "wget"
                               nil
                               nil
                               nil
                               url
                               "-O"
                               out-file)))
    (if (eq result 0)
        (let* ((ext (ol-infer-image-type out-file)))
          (rename-file out-file (concat out-file "." ext))
          (setq out-file (concat out-file "." ext))
          (ol-org-insert-image out-file))
      (user-error "Failed to download image"))))

(defun ol-infer-image-type (file)
  (let* ((info (shell-command-to-string
                (format "identify %s" file))))
    (ol-regexp-group
     (format "%s \\([a-zA-Z]+\\) [0-9]" file)
     info
     1)))

(ol-define-key ol-normal-leader-map "o i u" #'ol-org-insert-image-from-url)
(ol-evil-define-key 'insert org-mode-map "M-i u" #'ol-org-insert-image-from-url)

(defun ol-org-insert-image-from-clipboard ()
  (interactive)
  ;; Risk for deadlock if last copy was from emacs, since then xclip will wait
  ;; for emacs but emacs waits for xclip. Need to find a solution to that.
  (let* ((targets (process-lines "xclip" "-selection" "clipboard" "-t" "TARGETS" "-o")))
    (if (cl-member "image/png" targets :test #'string-equal)
        (let* ((out-file (make-temp-file "ol-org-insert-image-from-clipboard-" nil ".png"))
               (result (call-process "xclip"
                                     nil
                                     `(:file ,out-file)
                                     nil
                                     "-selection"
                                     "clipboard"
                                     "-t"
                                     "image/png"
                                     "-o")))
          (if (eq result 0)
              (ol-org-insert-image out-file)
            (user-error "Failed to get clipboard image")))
      (user-error "The clipboard doesn't seem to be an image"))))

(ol-define-key ol-normal-leader-map "o i c" #'ol-org-insert-image-from-clipboard)
(ol-evil-define-key 'insert org-mode-map "M-i c" #'ol-org-insert-image-from-clipboard)

(defun ol-org-insert-image (&optional in-file)
  (interactive)
  (let* ((in-file (or in-file (read-file-name
                               "Image to insert: "
                               nil ;; dir
                               nil ;; default-file-name
                               t)))) ;; must-match
    (unless (eq major-mode 'org-mode)
      (user-error "Only works in org-mode"))
    (unless buffer-file-name
      (user-error "buffer-file-name nil"))
    (unless (file-exists-p in-file)
      (user-error "in-file doesn't exist"))
    (let* ((ext (file-name-extension in-file))
           (default-file-name (format-time-string "%Y-%m-%d_%H:%M:%S"))
           (out-dir
            (file-name-concat
             "."
             (concat (file-name-nondirectory buffer-file-name) ".images")))
           (prompt (format "Saving image to '%s'. File name (default: %s): "
                           out-dir default-file-name))
           (user-file-name (read-string
                            prompt
                            nil ;; initial-input
                            'ol-org-insert-image ;; history
                            default-file-name))
           ;; Assuming the user doesn't enter an extension
           (file-name (concat user-file-name (if ext (concat "." ext) "")))
           (out-file (file-name-concat out-dir file-name)))
      (when (directory-name-p out-file)
        (user-error "Entered file name is a directory name"))
      (ol-create-dirs-if-needed (file-name-directory out-file))
      (copy-file in-file out-file)
      (insert (format "[[%s]]" out-file))
      (org-display-inline-images))))

(ol-define-key ol-normal-leader-map "o i f" #'ol-org-insert-image)
(ol-evil-define-key 'insert org-mode-map "M-i f" #'ol-org-insert-image)

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

;;;; ---------------------------------------------------------------------------
;;;; Kill without emphasis markers
;;;; ---------------------------------------------------------------------------
;; todo: make more sophisticated. However, my main use case is for copying
;; code/verbatim snippets to terminals.

(defvar ol-org-kill-without-emphasis-markers nil)

(defun ol-toggle-org-kill-without-emphasis-markers ()
  (interactive)
  (setq ol-org-kill-without-emphasis-markers (not ol-org-kill-without-emphasis-markers))
  (message "kill without emphasis: %s" ol-org-kill-without-emphasis-markers))

(ol-define-key ol-normal-leader-map "o t" #'ol-toggle-org-kill-without-emphasis-markers)

(defun ol-org-filter-buffer-substring-function (killed-string)
  (if ol-org-kill-without-emphasis-markers
      (replace-regexp-in-string "~\\|=" "" killed-string)
    killed-string))

(defun ol-add-kill-without-emphasis-function ()
  (add-function :filter-return
                (local 'filter-buffer-substring-function)
                #'ol-org-filter-buffer-substring-function))

(add-hook 'org-mode-hook #'ol-add-kill-without-emphasis-function)

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

(defun ol-org-fill-column (&rest _)
  (setq-local fill-column 120))

;; To make sure the correct fill-column is available when needed. org-mode-hook
;; wasn't enough.
(advice-add 'ol-org-display-inline-image--width-advice :before
            #'ol-org-fill-column)

(add-hook 'org-mode-hook #'ol-org-fill-column)

;; -----------------------------------------------------------------------------
;; Export
;; -----------------------------------------------------------------------------
;; C-e C-e then h then h to export to an html file

;; Copied/Modified from https://emacs.stackexchange.com/a/37031
;; and https://www.reddit.com/r/orgmode/comments/7dyywu/creating_a_selfcontained_html/
(defun ol-org-html--format-image (source attributes info)
  (let* ((source (replace-regexp-in-string (regexp-quote "%20") "" source nil 'literal))
         (ext (or (file-name-extension source) ""))
         (extra-ext (if (string= "svg" ext) "+xml" ""))
         (base64-content (base64-encode-string
                          (with-temp-buffer
                            (insert-file-contents-literally source)
                            (buffer-string))))
         (name (file-name-nondirectory source)))
    (format "<img src=\"data:image/%s%s;base64,%s\"%s />"
            ext
            extra-ext
            base64-content
            name)))

(advice-add 'org-html--format-image :override #'ol-org-html--format-image)

(provide 'ol-org-mode)
