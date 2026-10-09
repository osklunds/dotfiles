;; -*- lexical-binding: t -*-

(require 'ol-evil)
(require 'ol-icomplete)

(ol-define-key ol-override-map "C-j" #'ol-switch-to-buffer)

(defun ol-switch-to-buffer ()
  "Similar to `switch-to-buffer' but avoids face problems and puts current
buffer last."
  (interactive)
  (let* ((buffers (cl-remove-if (lambda (buffer)
                                  (or
                                   (eq buffer (current-buffer))
                                   (minibufferp buffer) 
                                   ))
                                (buffer-list)))
         (buffer-names (mapcar (lambda (buffer) (with-current-buffer buffer
                                                  (buffer-name)))
                               (append buffers (list (current-buffer)))))
         ;; Copied/modified from https://emacs.stackexchange.com/a/8177
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      `(metadata
                        (ol-extra-highlight-function . ,#'ol-switch-to-buffer-highlight-fn)
                        (ol-delete-action . ,#'ol-switch-to-buffer-delete-action)
                        (cycle-sort-function . ,#'identity)
                        (display-sort-function . ,#'identity))
                    (complete-with-action action buffer-names string pred))))
         (buffer (completing-read
                  "Switch to buffer: "
                  table)))
    (if (get-buffer buffer)
        (switch-to-buffer buffer)
      ;; if two buffers with same name but different <dir> suffix existed, one
      ;; is deleteed, then the remaining buffer changes name but not the one
      ;; among the candidates in completion.
      (message "%S doesn't exist (anymore), not switching" buffer))))

(defun ol-switch-to-buffer-highlight-fn (candidate)
  ;; "when" version needed to fix bug when two buffers of same file name are
  ;; open, and one is deleted. The remaining one will change name from name<dir>
  ;; to name and hence not be found anymore
  (when-let* ((buffer (get-buffer candidate))
              (mode (buffer-local-value 'major-mode buffer)))
    (cond
     ;; todo: consider what to do if remote and dired
     ;; (find-file "/docker:tests-dotfiles-tramp-test-1:/")
     ((file-remote-p (buffer-local-value 'default-directory buffer))
      (ol-add-face-text-property candidate 'ol-remote-buffer-name-face))

     ((eq mode 'dired-mode)
      (ol-add-face-text-property candidate 'ol-dired-buffer-name-face))

     ((eq mode 'vterm-mode)
      (ol-add-face-text-property candidate 'ol-vterm-buffer-name-face))

     (t nil))))

(defun ol-add-face-text-property (str face)
  (add-face-text-property 0 (length str) face nil str))

(defface ol-dired-buffer-name-face
  '((default :weight bold :inherit 'font-lock-function-name-face))
  "Face for dired buffer name in `ol-switch-to-buffer'.")

(defface ol-vterm-buffer-name-face
  '((default :weight bold :inherit 'font-lock-type-face))
  "Face for vterm buffer name in `ol-switch-to-buffer'.")

(defface ol-remote-buffer-name-face
  '((default :foreground "#110099"))
  "Face for remote buffer name in `ol-switch-to-buffer'.")

(defun ol-switch-to-buffer-delete-action (selected)
  ;; Use "when" version as extra robustification, although unsure if needed
  (when-let ((buf (get-buffer selected)))
    (kill-buffer buf)))

(provide 'ol-switch-to-buffer)
