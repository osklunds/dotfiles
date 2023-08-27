
(require 'rust-mode)

(add-hook 'rust-mode-hook 'eglot-ensure)

(defun ol-lsp-rust-analyzer--make-init-options (original)
  (let ((extra `(:workspace (:symbol (:search (:kind ,"all_symbols"))))))
    (append original extra)))

(advice-add 'lsp-rust-analyzer--make-init-options :filter-return #'ol-lsp-rust-analyzer--make-init-options)

(define-abbrev-table 'rust-mode-abbrev-table
  '(
    ("asdfg" "mode-spec-abbreviation@@test")
   ))
