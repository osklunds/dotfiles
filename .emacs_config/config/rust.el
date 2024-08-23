
(require 'rust-mode)
(ol-require-external "rust-analyzer")

(add-hook 'rust-mode-hook 'lsp)

(defun ol-lsp-rust-analyzer--make-init-options (original)
  (let ((extra `(:workspace (:symbol (:search (:kind ,"all_symbols"))))))
    (append original extra)))

(advice-add 'lsp-rust-analyzer--make-init-options :filter-return #'ol-lsp-rust-analyzer--make-init-options)

(define-abbrev-table 'rust-mode-abbrev-table
  '(
    ("dbg" "println!(\"{:?}\", @@);")
    ("dbgg" "println!(\"\\n\\n  {:?}   \\n\\n\\n\\n\", @@);")
   ))
