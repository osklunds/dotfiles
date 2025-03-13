;;;  -*- lexical-binding: t; -*-

(require 'ol-util)

(require 'vertico)
(require 'embark)

(vertico-mode)

;; -----------------------------------------------------------------------------
;; Keymaps
;; -----------------------------------------------------------------------------

(ol-override-key "C-j" 'switch-to-buffer)

(ol-define-key minibuffer-local-map "C-j" 'next-line)
(ol-define-key minibuffer-local-map "C-k" 'previous-line)
(ol-define-key minibuffer-local-map "C-n" 'minibuffer-keyboard-quit)
(ol-define-key minibuffer-local-map 'tab 'vertico-exit)
(ol-define-key minibuffer-local-map 'return 'vertico-exit-input)
(ol-define-key minibuffer-local-map "M-i" 'vertico-insert)
(ol-define-key minibuffer-local-map "M-o" 'embark-collect)

;; -----------------------------------------------------------------------------
;; Behavior
;; -----------------------------------------------------------------------------

(setc vertico-count 20)
(setc vertico-cycle t)

;; -----------------------------------------------------------------------------
;; Find file name
;; -----------------------------------------------------------------------------

(defun ol-find-file-name ()
  (interactive)
  (let* ((candidates (split-string (shell-command-to-string "rg --files") "\n" t))
         (selected (completing-read
                    "Find file name: "
                    candidates
                    nil
                    t
                    nil
                    'ol-find-file-name
                    )))
    (find-file selected)))
(ol-override-key "M-q" 'ol-find-file-name)

(defvar ol-find-file-content-last-probe nil)
(defvar ol-find-file-content-last-result nil)

(defun ol-find-file-content ()
  (interactive)
  (setq ol-find-file-content-last-probe nil)
  (setq ol-find-file-content-last-result nil)

  (let* (
         (selected (completing-read
                    "Find file name: "
                    'ol-find-file-content-collection
                    nil
                    t
                    nil
                    'ol-find-file-name
                    )))
    (find-file selected)))

(defun ol-find-file-content-collection (probe pred action)
  (let* (
         (inhibit-message t)
         (candidates (while-no-input
                       (redisplay)
                       (list 'result (process-lines-ignore-status "rg"
                                                                  "--no-heading"
                                                                  "--line-number"
                                                                  "--with-filename"
                                                                  probe))))
         (candidates-return
          (pcase candidates
            (`(result ,new-candidates)
             (progn
               (setq ol-find-file-content-last-probe probe)
               (setq ol-find-file-content-last-result new-candidates)
               new-candidates))
            ('t
             (cond
              ((string-equal probe ol-find-file-content-last-probe)
               ol-find-file-content-last-result)
              (t nil)))
            ('nil
             (error "todo"))))
         )
    
    (cond
     ((eq (car-safe action) 'boundaries) nil)
     ((eq action 'metadata) nil)
     ((eq action t) candidates-return))))

(ol-override-key "M-e" 'ol-find-file-content)

(provide 'ol-vertico)
