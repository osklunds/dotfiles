;; -*- lexical-binding: nil -*-

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
(setc dired-clean-confirm-killing-deleted-buffers nil)
(setc dired-auto-revert-buffer 'dired-directory-changed-p)

(ol-evil-define-key 'normal dired-mode-map "o" #'dired-find-file)
(ol-evil-define-key 'normal dired-mode-map "i" #'dired-up-directory)
(ol-evil-define-key 'normal dired-mode-map "*" #'evil-ex-search-word-forward)

;; Seems to be the only way override space
(evil-collection-define-key 'normal 'dired-mode-map " " nil)
(ol-define-key dired-mode-map "SPC" nil)

(ol-define-key ol-normal-leader-map "d h" (lambda () (interactive) (dired "~")))

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
         (desired-name (ol-get-buffer-name-from-path dired-directory "dired")))
    (unless (ol-buffer-name-matches (buffer-name) desired-name)
      (rename-buffer (generate-new-buffer-name desired-name)))))

(defun ol-get-buffer-name-from-path (path &optional prefix)
  (let* ((abbrev-path (abbreviate-file-name path))
         (parts (split-string abbrev-path "/"))
         (last-part (car (last parts 2)))
         ;; Special case to handle root
         (last-part2 (if (string= "" last-part) "/" last-part))
         ;; Special case to handle remote root
         (last-part3 (if (string-match-p ":" last-part2)
                         (concat (ol-shrink-remote-part last-part2) "/")
                       last-part2))
         (other-parts (butlast parts 2))
         (initial-parts (ol-shrink-parts other-parts))
         (new-parts (append initial-parts (list last-part3)))
         (full (string-join new-parts "/")))
    (if prefix
        (concat prefix ": " full)
      full)))

(defun ol-shrink-parts (parts)
  (mapcar (lambda (part)
            (cond
             ((equal "" part) "")
             ((string-match-p ":" part) (ol-shrink-remote-part part))
             (t (ol-shrink-part part))))
          parts))

(defun ol-shrink-remote-part (part)
  (let* ((parts (split-string part ":")))
    (string-join (mapcar #'ol-shrink-part parts) ":")))

(ert-deftest ol-shrink-remote-part-test ()
  (ol-assert-equal "doc:som:" (ol-shrink-remote-part "docker:some_host:")))

(defun ol-shrink-part (part)
  (substring part 0 (min 3 (length part))))

(ert-deftest ol-get-buffer-name-from-path-test ()
  (ol-assert-equal "/et/ipt/config" (ol-get-buffer-name-from-path "/et/iptables/config/"))
  (ol-assert-equal "/et/.ip/config" (ol-get-buffer-name-from-path "/et/.iptables/config/"))
  (ol-assert-equal "/etc/iptables" (ol-get-buffer-name-from-path "/etc/iptables/"))
  (ol-assert-equal "/etc" (ol-get-buffer-name-from-path "/etc/"))
  (ol-assert-equal "/" (ol-get-buffer-name-from-path "/"))
  (ol-assert-equal "~/repos" (ol-get-buffer-name-from-path "~/repos/"))
  (ol-assert-equal "~" (ol-get-buffer-name-from-path "~/"))
  (ol-assert-equal "dired: /etc/ipt/config"
                   (ol-get-buffer-name-from-path "/etc/iptables/config/" "dired"))

  (ol-assert-equal "/doc:som:/abc/iptables"
                   (ol-get-buffer-name-from-path "/docker:some_host:/abcdef/iptables/"))
  (ol-assert-equal "/doc:som:/"
                   (ol-get-buffer-name-from-path "/docker:some_host:/"))
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
