;;;  -*- lexical-binding: t; -*-

(require 'ol-util)
(require 'ol-ert)

(require 'vertico)
(require 'embark)
(require 'consult)
(require 'orderless)

(require 'ivy)

(vertico-mode)
(ivy-mode -1)

;; -----------------------------------------------------------------------------
;; Keymaps
;; -----------------------------------------------------------------------------

(ol-define-key ol-override-map "C-j" 'switch-to-buffer)

(ol-define-key minibuffer-local-map "C-j" 'next-line)
(ol-define-key minibuffer-local-map "C-k" 'previous-line)
(ol-define-key minibuffer-local-map "C-n" 'minibuffer-keyboard-quit)
(ol-define-key minibuffer-local-map 'tab 'vertico-exit)
(ol-define-key minibuffer-local-map 'return 'vertico-exit-input)
(ol-define-key minibuffer-local-map "M-i" 'vertico-insert)
(ol-define-key minibuffer-local-map "M-o" 'embark-collect)


;; -----------------------------------------------------------------------------
;; Behavior
;; -----------------------------------------------------------------------------

(setc vertico-count 20)
(setc vertico-cycle t)

;; -----------------------------------------------------------------------------
;; Find file name
;; -----------------------------------------------------------------------------

(defun ol-find-file-name ()
  (interactive)
  (let* ((candidates (split-string (shell-command-to-string "rg --files") "\n" t))
         (selected (completing-read
                    "Find file name: "
                    candidates
                    nil
                    t
                    nil
                    'ol-find-file-name
                    )))
    (find-file selected)))
(ol-define-key ol-override-map "M-q" 'ol-find-file-name)

(defvar ol-find-file-content-last-probe nil)
(defvar ol-find-file-content-last-result nil)

(defun ol-find-file-content ()
  (interactive)
  (setq ol-find-file-content-last-probe nil)
  (setq ol-find-file-content-last-result nil)

  (let* (
         (selected (completing-read
                    "Find file name: "
                    'ol-find-file-content-collection
                    nil
                    t
                    nil
                    'ol-find-file-name
                    )))
    (find-file selected)))

(defun ol-find-file-content-collection (probe pred action)
  (let* (
         (inhibit-message t)
         (candidates (while-no-input
                       (redisplay)
                       (list 'result (process-lines-ignore-status "rg"
                                                                  "--no-heading"
                                                                  "--line-number"
                                                                  "--with-filename"
                                                                  probe))))
         (candidates-return
          (pcase candidates
            (`(result ,new-candidates)
             (progn
               (setq ol-find-file-content-last-probe probe)
               (setq ol-find-file-content-last-result new-candidates)
               new-candidates))
            ('t
             (cond
              ((string-equal probe ol-find-file-content-last-probe)
               ol-find-file-content-last-result)
              (t nil)))
            ('nil
             (error "todo"))))
         )
    
    (cond
     ((eq (car-safe action) 'boundaries) nil)
     ((eq action 'metadata) nil)
     ((eq action t) candidates-return))))

(ol-define-key ol-override-map "M-e" 'ol-find-file-content)

;; -----------------------------------------------------------------------------
;; Completion style
;; -----------------------------------------------------------------------------

(setq completion-styles '(ol orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

(defun ol-all-completions (string table pred _point)
  (let* ((regex (ol-string-to-regex string))
         (completion-regexp-list (list regex)))
    (all-completions "" table pred)))

(defun ol-string-to-regex (string)
  (let* ((parts (split-string string " ")))
    (apply #'concat (cdr (mapcan (lambda (part) (list ".*" part)) parts)))))

(ert-deftest ol-string-to-regex-test ()
  (ol-assert-equal "" (ol-string-to-regex ""))
  (ol-assert-equal "defun" (ol-string-to-regex "defun"))
  (ol-assert-equal "defun.*my-fun" (ol-string-to-regex "defun my-fun"))
  )

(ert-deftest ol-all-completions-test ()
  (let ((candidates '("read-from-string"
                      "read-from-buffer"
                      "read-from-minibuffer"
                      "read"
                      )))

    (ol-assert-equal '(
                       "read-from-string"
                       "read-from-buffer"
                       "read-from-minibuffer"
                       "read"
                       )
                     (ol-all-completions "read" candidates nil nil))

    (ol-assert-equal '(
                       "read-from-buffer"
                       "read-from-minibuffer"
                       )
                     (ol-all-completions "read buffer" candidates nil nil))

    (ol-assert-equal '(
                       "read"
                       )
                     (ol-all-completions "ead$" candidates nil nil))

    (ol-assert-equal '(
                       "read-from-minibuffer"
                       )
                     (ol-all-completions "read -[min]+" candidates nil nil))

    (ol-assert-equal nil (ol-all-completions "dummy" candidates nil nil))
    )
  )




(ol-string-to-regex "hej defun")


(defun ol-try-completion (string table pred point)
  ;; (message "try")
  nil)

(add-to-list 'completion-styles-alist
             '(ol ol-try-completion ol-all-completions "ol"))

(provide 'ol-vertico)
