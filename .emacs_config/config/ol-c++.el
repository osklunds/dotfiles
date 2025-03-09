
(require 'ol-util)

(ol-require-external "clangd")

(define-abbrev-table 'c++-mode-abbrev-table
  '(
    ("dbg" "std::cout << \"oskar: \" << @@ << std::endl;")
    ))

;; -----------------------------------------------------------------------------
;; LSP
;;------------------------------------------------------------------------------

;; (add-hook 'c++-mode-hook 'lsp)
;; (setc lsp-clangd-binary-path "clangd")

(provide 'ol-c++)
