
(require 'ol-util)

(require 'eglot)

(setc eglot-ignored-server-capabilities
      '(
        :hoverProvider
        ;; :completionProvider
        :signatureHelpProvider
        ;; :definitionProvider
        ;; :typeDefinitionProvider
        ;; :implementationProvider
        ;; :declarationProvider
        ;; :referencesProvider
        :documentHighlightProvider
        :documentSymbolProvider
        :workspaceSymbolProvider
        :codeActionProvider
        :codeLensProvider
        :documentFormattingProvider
        :documentRangeFormattingProvider
        :documentOnTypeFormattingProvider
        :renameProvider
        :documentLinkProvider
        :colorProvider
        :foldingRangeProvider
        :executeCommandProvider
        :inlayHintProvider
        ))

(setc eglot-stay-out-of '(companay
                          flymake
                          ))
(setc eglot-report-progress nil)

(provide 'ol-eglot)

