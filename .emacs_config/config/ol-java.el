;; -*- lexical-binding: nil -*-

(require 'ol-vterm)
(require 'ol-evil)

;; (require 'lsp-java)
(require 'which-func)
(require 'cc-mode)

(ol-require-external "javac")

;; (add-hook 'java-mode-hook #'lsp)

(define-abbrev-table 'java-mode-abbrev-table
  '(
    ("dbg" "System.out.printf(\"oskar: %s%n\", @@);")
    ))

(defun ol-java-run-current-test ()
  (interactive)
  (save-buffer)
  (when-let* ((test-name (which-function))
              (cmd (concat "mvn test -Dtest=\"#" test-name "\"\n")))
    (dolist (window (window-list))
      (with-current-buffer (window-buffer window)
        (when (eq major-mode 'vterm-mode)
          (vterm-send-string cmd))))))

(ol-evil-define-key 'normal java-mode-map "C-c r" 'ol-java-run-current-test)

(provide 'ol-java)
