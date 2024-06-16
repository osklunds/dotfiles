
(require 'lsp-java)
(ol-require-external "javac")

(add-hook 'java-mode-hook #'lsp)

(define-abbrev-table 'java-mode-abbrev-table
  '(
    ("dbg" "System.out.printf(\"oskar: %s%n\", @@);")
    ))
