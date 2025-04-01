
(require 'ol-ert)

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
  (let ((all (ol-all-completions string table pred point)))
    (cond
     ((null all) nil)
     ((eq (length all) 1) string)
     (t string))))

(add-to-list 'completion-styles-alist
             '(ol ol-try-completion ol-all-completions "ol"))

(provide 'ol-completion-style)
