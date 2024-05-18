
(require 'haskell-mode)
(require 'haskell-mode-autoloads)

(add-hook 'haskell-mode-hook 'lsp)

(setc haskell-indentation-layout-offset 4)
(setc haskell-indentation-starter-offset 4)
(setc haskell-indentation-left-offset 4)
(setc haskell-indentation-where-pre-offset 4)
(setc haskell-indentation-where-post-offset 4)
