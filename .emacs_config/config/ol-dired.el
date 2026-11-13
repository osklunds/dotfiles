;; -*- lexical-binding: nil -*-

(require 'ol-util)
(require 'ol-evil)

(require 'dired)
(require 'dired-x)
(require 'tar-mode)
(require 'arc-mode)

;; -----------------------------------------------------------------------------
;; General
;; -----------------------------------------------------------------------------

(setc dired-clean-confirm-killing-deleted-buffers nil)
(setc dired-auto-revert-buffer 'dired-directory-changed-p)

(defun ol-dired-home ()
  (interactive)
  (dired "~"))

(defun ol-dired ()
  (interactive)
  (cond
   (tar-superior-buffer (switch-to-buffer tar-superior-buffer))
   (archive-superior-buffer (switch-to-buffer archive-superior-buffer))
   (t (dired default-directory))))

;; No -v because then B comes before a
(setc dired-listing-switches "-Alh --time-style=long-iso")
(setc dired-recursive-copies 'always)
(setc dired-recursive-deletes 'always)

(advice-add 'wdired-exit :around 'ol-disable-y-or-n-p)

;; -----------------------------------------------------------------------------
;; Keybinds
;; -----------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; From evil-collection
;;;; ---------------------------------------------------------------------------

;; I use the same/similar/some keybinds as those from evil-collection.
;; https://github.com/emacs-evil/evil-collection
;; Credits to them!

;; General tip: look at their keybinds to find more useful commands to run

;; Upper keys for marked files
(ol-evil-define-key 'normal dired-mode-map "C" #'dired-do-copy)
(ol-evil-define-key 'normal dired-mode-map "D" #'dired-do-delete)
(ol-evil-define-key 'normal dired-mode-map "M" #'dired-do-chmod)
(ol-evil-define-key 'normal dired-mode-map "R" #'dired-do-rename)
(ol-evil-define-key 'normal dired-mode-map "Z" #'dired-do-compress)
(ol-evil-define-key 'normal dired-mode-map "!" #'dired-do-shell-command)
(ol-evil-define-key 'normal dired-mode-map "&" #'dired-do-async-shell-command)

;; Lower keys for not marked files
(ol-evil-define-key 'normal dired-mode-map "m" #'dired-mark)
(ol-evil-define-key 'normal dired-mode-map "u" #'dired-unmark)
(ol-evil-define-key 'normal dired-mode-map "W" #'browse-url-of-dired-file)
(ol-evil-define-key 'normal dired-mode-map "+" #'dired-create-directory)

;;;; ---------------------------------------------------------------------------
;;;; Own
;;;; ---------------------------------------------------------------------------

(ol-define-key dired-mode-map "SPC" nil)
(ol-define-key ol-normal-leader-map "d h" #'ol-dired-home)
(ol-global-set-key "C-x d" 'ol-dired)

(ol-evil-define-key 'normal dired-mode-map "o" #'dired-find-file)
(ol-evil-define-key 'normal dired-mode-map "i" #'dired-up-directory)
(ol-evil-define-key 'normal dired-mode-map "*" #'evil-ex-search-word-forward)

;; Do not use dired's next file, which doesn't preserve column, i.e.,
;; is different from the normal vanilla movement
(ol-evil-define-key 'normal dired-mode-map "j" nil)
(ol-evil-define-key 'normal dired-mode-map "k" nil)

(ol-evil-define-key 'normal dired-mode-map "S" 'dired-do-relsymlink)

;; -----------------------------------------------------------------------------
;; Default shell commands
;; -----------------------------------------------------------------------------

(setc dired-guess-shell-alist-user
      '(("\\.pcap$" "wireshark&")
        ("\\.\\(crt\\)\\|\\(pem\\)$" "openssl x509 -noout -text -in")
        ("\\.csr$" "openssl req -text -noout -verify -in")))

;; -----------------------------------------------------------------------------
;; Shell command buffers
;; -----------------------------------------------------------------------------

(defun ol-dired-set-shell-command-buffer-name (func &rest args)
  "Adds the command run to the name of the output buffer."
  (let* ((old-fun (symbol-function #'shell-command)))
    (cl-letf (((symbol-function 'shell-command)
               (lambda (command)
                 (funcall old-fun command
                          (concat "*Shell Command Output: '" command "'*")))))
      (apply func args))))

(advice-add 'dired-run-shell-command :around 'ol-dired-set-shell-command-buffer-name)

;; -----------------------------------------------------------------------------
;; Add to history
;; -----------------------------------------------------------------------------

(defun ol-add-dired-to-file-name-history ()
  (add-to-history 'file-name-history default-directory))

(add-hook 'dired-after-readin-hook #'ol-add-dired-to-file-name-history)

(provide 'ol-dired)
