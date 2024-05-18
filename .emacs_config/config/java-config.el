
(require 'lsp-java)
(ol-require-external "javac")

(add-hook 'java-mode-hook #'lsp)
