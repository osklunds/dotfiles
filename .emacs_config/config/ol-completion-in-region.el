;; -*- lexical-binding: t -*-

(require 'ol-util)

;; -----------------------------------------------------------------------------
;; Snippet expansion
;; -----------------------------------------------------------------------------

;; Copied/modified from https://stackoverflow.com/a/15389612
(defun ol-expand-abbrev (expand-abbrev-return)
  ;; if there was an expansion
  (when expand-abbrev-return
    ;; start idle timer to ensure insertion of abbrev activator
    ;; character (e.g. space) is finished
    (run-with-idle-timer 0 nil
                         (lambda ()
                           ;; if there is the string "@@" in the
                           ;; expansion then move cursor there and
                           ;; delete the string
                           (let ((cursor "@@"))
                             (if (search-backward cursor last-abbrev-location t)
                                 (delete-char (length cursor)))))))
  expand-abbrev-return)

(advice-add 'expand-abbrev :filter-return 'ol-expand-abbrev)

(setc save-abbrevs 'silently)

;; -----------------------------------------------------------------------------
;; Silence dabbrev
;; -----------------------------------------------------------------------------

;; Due to dabbrev's message use, eldoc flickers, but only when there are no
;; compeltions

(defun ol-dabbrev-silence-advice (func &rest args)
  (cl-letf (((symbol-function 'message)
             (lambda (&rest _) nil)))
    (apply func args)))

(advice-add 'dabbrev--find-expansion :around #'ol-dabbrev-silence-advice)

(provide 'ol-completion-in-region)

