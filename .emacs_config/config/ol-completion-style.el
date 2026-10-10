;; -*- lexical-binding: t -*-

(require 'icomplete)

;; -----------------------------------------------------------------------------
;; Defining the style
;; -----------------------------------------------------------------------------

(defun ol-all-completions (string table pred _point)
  ;; bounds, prefix, prefix-length are needed for find-file
  (let* ((bounds (completion-boundaries string table pred ""))
         (prefix-length (car bounds))
         (prefix (substring string 0 prefix-length))
         (to-complete (substring string prefix-length))
         (regex (ol-string-to-regex to-complete))
         (completion-regexp-list (list regex))
         (completion-ignore-case (ol-ignore-case-p to-complete))
         (all (all-completions prefix table pred))
         )
    (setq completion-lazy-hilit-fn
          (apply-partially #'ol-highlight-completion regex completion-ignore-case))
    (when all
      (append all prefix-length))))

(defun ol-ignore-case-p (string)
  (string= string (downcase string)))

(defun ol-string-to-regex (string)
  (let* ((trimmed (string-trim string " " " "))
         (old nil)
         (new trimmed))
    (while (not (string= old new))
      (setq old new)
      ;; replace below only does one at a time so need to loop
      (setq new (replace-regexp-in-string "\\([^ ]\\) \\([^ ]\\)" "\\1.*?\\2" old)))
    (replace-regexp-in-string " \\( +\\)" "\\1" new)))

(ol-assert-equal "" (ol-string-to-regex ""))
(ol-assert-equal "defun" (ol-string-to-regex "defun"))
(ol-assert-equal "defun.*?my-fun" (ol-string-to-regex "defun my-fun"))
(ol-assert-equal "defun my-fun" (ol-string-to-regex "defun  my-fun"))
(ol-assert-equal "defun  my-fun" (ol-string-to-regex "defun   my-fun"))
(ol-assert-equal "defun.*?my-fun" (ol-string-to-regex "defun my-fun "))
(ol-assert-equal "a.*?b.*?c" (ol-string-to-regex "a b c"))
(ol-assert-equal "a b c" (ol-string-to-regex "a  b  c"))

;; (let ((candidates '("read-from-string"
;;                     "read-from-buffer"
;;                     "read-from-minibuffer"
;;                     "read"
;;                     "READ"
;;                     )))

;;   (ol-assert-equal '(
;;                      "read-from-string"
;;                      "read-from-buffer"
;;                      "read-from-minibuffer"
;;                      "read"
;;                      "READ"
;;                      )
;;                    (ol-all-completions "read" candidates nil nil))

;;   (ol-assert-equal '(
;;                      "read-from-buffer"
;;                      "read-from-minibuffer"
;;                      )
;;                    (ol-all-completions "read buffer" candidates nil nil))

;;   (ol-assert-equal '(
;;                      "read"
;;                      "READ"
;;                      )
;;                    (ol-all-completions "ead$" candidates nil nil))

;;   (ol-assert-equal '(
;;                      "read-from-minibuffer"
;;                      )
;;                    (ol-all-completions "read -[min]+" candidates nil nil))

;;   (ol-assert-equal nil (ol-all-completions "dummy" candidates nil nil))

;;   (ol-assert-equal '(
;;                      "READ"
;;                      )
;;                    (ol-all-completions "D" candidates nil nil))
;;   )

(defun ol-try-completion (string table pred point)
  (let ((all (ol-all-completions string table pred point)))
    (cond
     ((null all) nil)
     ;; This caused the hard to find issue "Error running timer: (wrong-type-argument listp 0)
     ;; all is not a proper list. To trigger this, type M-x and type
     ;; "l time" and select list-timers. Maybe many buffers need to be open.
     ((eq (length (ol-nmake-proper-list all)) 1) string)
     (t string))))

;; Copied/modified from https://stackoverflow.com/a/28585107
(defun ol-nmake-proper-list (x)
  (let ((y (last x)))
    (setcdr y nil)
    x))

;; This style is not just about matching, but also about highlights
(add-to-list 'completion-styles-alist
             '(ol ol-try-completion ol-all-completions "ol"))

(defun ol-highlight-completion (regex ignore-case candidate)
  (when (ol-completion-metadata-get 'ol-skip-normal-highlight)
    (ol-map-font-lock-face-to-face candidate)
    )
  (unless (ol-completion-metadata-get 'ol-skip-normal-highlight)
    (ol-normal-highlight-fn regex ignore-case candidate))
  (when-let ((fn (ol-completion-metadata-get 'ol-extra-highlight-function)))
    (funcall fn candidate))
  candidate)

(defun ol-completion-metadata-get (key)
  (let* ((md (completion-metadata
              ""
              minibuffer-completion-table
              minibuffer-completion-predicate)))
    (completion-metadata-get md key)))

(defun ol-normal-highlight-fn (regex ignore-case candidate)
  (let* ((case-fold-search ignore-case))
    (string-match regex candidate)
    (let* ((m (match-data))
           (start (car m))
           (end (cadr m)))
      (add-face-text-property start end 'ol-match-face nil candidate))))

;; todo: don't highlight char by char, do intervals for better performance
(defun ol-map-font-lock-face-to-face (string)
  (dolist (pos (number-sequence 0 (1- (length string))))
    (when-let ((prop (get-text-property pos 'font-lock-face string)))
      (add-face-text-property pos (1+ pos) prop nil string)))
  string)

;; -----------------------------------------------------------------------------
;; Setting
;; -----------------------------------------------------------------------------

(setq completion-lazy-hilit t)

(setc completion-styles '(ol))

;; So that 'ol style is used for everything
(setc completion-category-defaults nil)

(provide 'ol-completion-style)
