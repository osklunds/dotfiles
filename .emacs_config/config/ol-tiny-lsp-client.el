;; -*- lexical-binding: nil -*-

(require 'ol-util)
(require 'ol-evil)
(require 'ol-corfu)

(require 'tiny-lsp-client)

;; -----------------------------------------------------------------------------
;; Config
;; -----------------------------------------------------------------------------

(setc tlc-log-io t)
(setc tlc-log-stderr t)
(setc tlc-log-rust-debug t)
(setc tlc-log-emacs-debug t)
(setc tlc-log-to-stdio nil)
(setc tlc-debug-on-error t)

(add-hook 'tlc-mode-hook 'tlc-use-xref)

;; -----------------------------------------------------------------------------
;; Keybinds
;; -----------------------------------------------------------------------------

(ol-define-key ol-normal-leader-map "l l" 'tlc-open-log-file)
(ol-define-key ol-normal-leader-map "l i" 'tlc-info)
(ol-define-key ol-normal-leader-map "l r" 'tlc-restart-server)
(ol-define-key ol-normal-leader-map "l s" 'tlc-stop-server)

;; -----------------------------------------------------------------------------
;; Capf
;; -----------------------------------------------------------------------------

(defun ol-capf-tlc ()
  (cape-wrap-properties #'tlc-completion-at-point
                        :annotation-function (lambda (_) " tlc")))

(add-hook 'tlc-mode-hook
          (lambda ()
            (ol-set-capfs
             '(ol-capf-abbrev ol-capf-tlc ol-capf-dabbrev))))

;; -----------------------------------------------------------------------------
;; Language major modes
;; -----------------------------------------------------------------------------

(add-hook 'rust-mode-hook 'tlc-mode)

(provide 'ol-tiny-lsp-client)

