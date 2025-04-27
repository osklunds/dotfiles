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

(defun ol-add-tlc-mode-capf ()
  (setq-local completion-at-point-functions
              (list (cape-capf-super #'ol-capf-abbrev
                                     #'ol-capf-tlc
                                     #'ol-capf-dabbrev))))

(defun ol-capf-tlc ()
  (cape-wrap-properties #'tlc-completion-at-point
                        :annotation-function (lambda (_) "tlc")))

(add-hook 'tlc-mode-hook #'ol-add-tlc-mode-capf)

;; -----------------------------------------------------------------------------
;; Language major modes
;; -----------------------------------------------------------------------------

(add-hook 'rust-mode-hook 'tlc-mode)

(provide 'ol-tiny-lsp-client)

