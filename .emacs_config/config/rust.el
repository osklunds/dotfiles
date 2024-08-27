
(require 'rust-mode)
(ol-require-external "rust-analyzer")

(add-hook 'rust-mode-hook 'lsp)

(defun ol-lsp-rust-analyzer--make-init-options (original)
  (let ((extra `(:workspace (:symbol (:search (:kind ,"all_symbols"))))))
    (append original extra)))

(advice-add 'lsp-rust-analyzer--make-init-options :filter-return #'ol-lsp-rust-analyzer--make-init-options)

(setc lsp-rust-analyzer-completion-add-call-parenthesis nil)

(define-abbrev-table 'rust-mode-abbrev-table
  '(
    ("dbg" "println!(\"{:?}\", @@);")
    ("dbgg" "println!(\"\\n\\n  {:?}   \\n\\n\\n\\n\", @@);")
    ("ar" "// Arrange")
    ("ac" "// Act")
    ("as" "// Assert")
    ))

(defun ol-rust-name-of-function ()
  (if-let ((name (ol-rust-get-function-on-current-line)))
      name
    (save-excursion
      (rust-beginning-of-defun)
      (ol-rust-name-of-function))))

(defun ol-rust-get-function-on-current-line ()
  (save-excursion
    (beginning-of-line)
    (ol-regexp-group "^fn \\([a-zA-Z0-9_]+\\)" (thing-at-point 'line t) 1)))

(defun ol-rust-run-current-test ()
  (interactive)
  (save-buffer)
  (when-let* ((test-name (ol-rust-name-of-function))
              (cmd (concat "ct " test-name "\n")))
    (dolist (window (window-list))
      (with-current-buffer (window-buffer window)
        (when (eq major-mode 'vterm-mode)
          (vterm-send-string cmd))))))

(ol-evil-define-key 'normal rust-mode-map "C-c r" 'ol-rust-run-current-test)
