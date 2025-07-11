;; -*- lexical-binding: nil -*-

(require 'ol-util)

(ol-require-external "clangd")

;; -----------------------------------------------------------------------------
;; Snippets
;; -----------------------------------------------------------------------------

(define-abbrev-table 'c++-mode-abbrev-table
  '(
    ("dbg" "std::cout << \"oskar: \" << @@ << std::endl;")
    ))

;; -----------------------------------------------------------------------------
;; lsp-mode
;;------------------------------------------------------------------------------

;; (add-hook 'c++-mode-hook 'lsp)
;; (setc lsp-clangd-binary-path "clangd")

(provide 'ol-c++)
