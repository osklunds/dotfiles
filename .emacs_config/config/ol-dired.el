
(require 'ol-util)
(require 'ol-evil)
(require 'ol-ert)

(require 'dired)
(require 'dired-x)
(require 'tar-mode)
(require 'arc-mode)

;; -----------------------------------------------------------------------------
;; General
;; -----------------------------------------------------------------------------

(setc dired-kill-when-opening-new-dired-buffer t)
(setc dired-auto-revert-buffer 'dired-directory-changed-p)

(ol-evil-define-key 'normal dired-mode-map "o" 'dired-find-file)
(ol-evil-define-key 'normal dired-mode-map "i" 'dired-up-directory)

;; Seems to be the only way override space
(evil-collection-define-key 'normal 'dired-mode-map " " nil)
(ol-define-key dired-mode-map "SPC" nil)

(ol-define-normal-leader-key "dh" (lambda () (interactive) (dired "~")))

(ol-evil-define-key 'normal dired-mode-map "S" 'dired-do-relsymlink)

(defun ol-dired ()
  (interactive)
  (cond
   (tar-superior-buffer (switch-to-buffer tar-superior-buffer))
   (archive-superior-buffer (switch-to-buffer archive-superior-buffer))
   (t (dired default-directory))))

(ol-global-set-key "C-x d" 'ol-dired)

;; No -v because then B comes before a
(setc dired-listing-switches "-Alh --time-style=long-iso")
(setc dired-recursive-copies 'always)
(setc dired-recursive-deletes 'always)

(advice-add 'wdired-exit :around 'ol-disable-y-or-n-p)

;; -----------------------------------------------------------------------------
;; Buffer name
;; -----------------------------------------------------------------------------

(add-hook 'dired-after-readin-hook 'ol-dired-rename-hook)

(defun ol-dired-rename-hook ()
  (let* ((path (or dired-directory default-directory))
         (desired-name (ol-get-buffer-name-from-path dired-directory)))
    (unless (ol-buffer-name-matches (buffer-name) desired-name)
      (rename-buffer (generate-new-buffer-name desired-name)))))

;; todo: do /h/o/.e/main.el instead
(defun ol-get-buffer-name-from-path (path &optional prefix)
  (let* ((abbreviated-path (abbreviate-file-name path))
         (name (cond
                ((string-equal abbreviated-path "~/") "~/")
                ((string-equal abbreviated-path "~") "~/")
                ((string-equal abbreviated-path "/") "/")
                (t (let* ((path2 (directory-file-name abbreviated-path))
                          (current-dir (file-name-nondirectory path2))
                          (parent-dir (directory-file-name (file-name-directory path2)))
                          (truncated-parent-dir (string-truncate-left parent-dir 40)))
                     (concat current-dir "  <" truncated-parent-dir ">")))))
         (prefix2 (when prefix
                    (concat prefix ": "))))
    (concat prefix2 name)))

(ert-deftest ol-get-buffer-name-from-path-test ()
  (ol-assert-equal "iptables  </etc>" (ol-get-buffer-name-from-path "/etc/iptables"))
  (ol-assert-equal "etc  </>" (ol-get-buffer-name-from-path "/etc"))
  (ol-assert-equal "/" (ol-get-buffer-name-from-path "/"))
  (ol-assert-equal "~/" (ol-get-buffer-name-from-path "~/"))
  (ol-assert-equal "dired: iptables  </etc>"
                   (ol-get-buffer-name-from-path "/etc/iptables" "dired"))
  (ol-assert-equal "dired: ~/"
                   (ol-get-buffer-name-from-path "~/" "dired"))
  )

(defun ol-buffer-name-matches (name desired-name)
  (let ((regexp (concat "^" (regexp-quote desired-name) "\\(<[0-9]>\\)?$")))
    (string-match-p regexp name)))

(ert-deftest ol-buffer-name-matches-test ()
  (ol-assert (ol-buffer-name-matches "some-name" "some-name"))
  (ol-assert (ol-buffer-name-matches "some-name<2>" "some-name"))
  (ol-assert (not (ol-buffer-name-matches "some-name-more" "some-name")))
  (ol-assert (not (ol-buffer-name-matches "some-name" "some-name-more")))
  (ol-assert (not (ol-buffer-name-matches "some-name" "vterm: some-name")))
  )

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
  (let* ((old-fun (symbol-function #'shell-command)))
    (cl-letf (((symbol-function 'shell-command)
               (lambda (command)
                 (funcall old-fun command
                          (concat "*Shell Command Output: '" command "'*")))))
      (apply func args))))

(advice-add 'dired-run-shell-command :around 'ol-dired-set-shell-command-buffer-name)

(provide 'ol-dired)
