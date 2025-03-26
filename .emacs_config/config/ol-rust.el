;; -*- lexical-binding: t -*-

(require 'ol-util)
(require 'ol-vterm)
(require 'ol-evil)
;; (require 'ol-eglot)

(require 'rust-mode)

(ol-require-external "rust-analyzer")
(ol-require-external "cargo")
(ol-require-external "rustfmt")

(defun ol-rust-format-buffer ()
  (interactive)
  (save-buffer)
  (rust-format-buffer)
  (save-buffer))

(ol-evil-define-key 'normal rust-mode-map "C-c C-q" 'ol-rust-format-buffer)

;; -----------------------------------------------------------------------------
;; LSP
;;------------------------------------------------------------------------------

;; (add-hook 'rust-mode-hook 'lsp)

;; (add-hook 'rust-mode-hook 'eglot-ensure)

;; it seems rust-mode's imenu is better than lsp's. So maybe don't need that
;; functionality for tlc

;; (defun ol-lsp-rust-analyzer--make-init-options (original)
;;   (let ((extra `(:workspace (:symbol (:search (:kind ,"all_symbols"))))))
;;     (append original extra)))

;; (advice-add 'lsp-rust-analyzer--make-init-options :filter-return #'ol-lsp-rust-analyzer--make-init-options)

;; (setc lsp-rust-analyzer-completion-add-call-parenthesis nil)

;; -----------------------------------------------------------------------------
;; Snippets
;;------------------------------------------------------------------------------

(define-abbrev-table 'rust-mode-abbrev-table
  '(
    ("dbg" "println!(\"oskar: {:?}\", @@);")
    ("dbgg" "println!(\"\\n\\n  {:?}   \\n\\n\\n\\n\", @@);")
    ("ar" "// Arrange")
    ("ac" "// Act")
    ("as" "// Assert")
    ("dp" "'{:?}'")
    ))

;; -----------------------------------------------------------------------------
;; Terminal integration
;;------------------------------------------------------------------------------

(defun ol-rust-cargo-check ()
  (interactive)
  (save-buffer)
  (ol-send-cmd-to-visible-vterm-buffers "cargo check --tests"))

(defun ol-rust-cargo-build ()
  (interactive)
  (save-buffer)
  (ol-send-cmd-to-visible-vterm-buffers "cargo build --tests"))

(ol-evil-define-key 'normal rust-mode-map "C-c C-c" 'ol-rust-cargo-check)
(ol-evil-define-key 'normal rust-mode-map "C-c C-b" 'ol-rust-cargo-build)

(ol-evil-define-key 'normal rust-mode-map "SPC SPC c" 'ol-rust-cargo-check)

;;;; ---------------------------------------------------------------------------
;;;; Run test
;;;;----------------------------------------------------------------------------

(defvar ol-rust-last-test-name nil)

(defun ol-rust-name-of-test ()
  (if-let ((name (ol-rust-get-function-on-current-line)))
      (save-excursion
        (forward-line -1)
        (beginning-of-line)
        (if (looking-at-p (regexp-quote "#[test]"))
            name
          nil))
    (save-excursion
      (rust-beginning-of-defun)
      (ol-rust-name-of-test))))

(defun ol-rust-get-function-on-current-line ()
  (save-excursion
    (beginning-of-line)
    (ol-regexp-group "^ *fn \\([a-zA-Z0-9_]+\\)" (thing-at-point 'line t) 1)))

(defun ol-rust-run-test (test-name)
  (save-buffer)
  (let ((cmd (concat "ct " test-name)))
    (setq ol-rust-last-test-name test-name)
    (ol-send-cmd-to-visible-vterm-buffers cmd)))

(defun ol-rust-run-current-test ()
  (interactive)
  (when-let* ((test-name (ol-rust-name-of-test)))
    (ol-rust-run-test test-name)))

(defun ol-rust-run-test-dwim ()
  (interactive)
  (if-let ((current (ol-rust-name-of-test)))
      (ol-rust-run-test current)
    (if ol-rust-last-test-name
        (ol-rust-run-test ol-rust-last-test-name)
      (user-error "No current or last test"))))

(ol-evil-define-key 'normal rust-mode-map "C-c e" 'ol-rust-run-test-dwim)
(ol-evil-define-key 'normal rust-mode-map "SPC SPC e" 'ol-rust-run-test-dwim)

(defun ol-rust-run-all-tests ()
  (interactive)
  (ol-send-cmd-to-visible-vterm-buffers "ct"))

(ol-evil-define-key 'normal rust-mode-map "C-c E" 'ol-rust-run-all-tests)
(ol-evil-define-key 'normal rust-mode-map "SPC SPC E" 'ol-rust-run-all-tests)

(provide 'ol-rust)
