;; -*- lexical-binding: nil -*-

(require 'ol-basic-user-interface)
(require 'ol-magit)
(require 'ol-evil)

(require 'smerge-mode)

;; Copied/inspired from, to automatically start smerge
;; https://stumbles.id.au/auto-starting-emacs-smerge-mode-for-git.html
(defun vc-git-find-file-hook ()
  (when (save-excursion
          (goto-char (point-min))
          (re-search-forward "^<<<<<<< " nil t))
    (smerge-mode)))

(defun ol-smerge-set-face (face-to-set face-val)
  (ol-set-face face-to-set :background face-val :foreground ol-black :weight 'normal))

(ol-smerge-set-face 'smerge-base ol-diff-dark-orange)
(ol-smerge-set-face 'smerge-lower ol-diff-light-red)
(ol-smerge-set-face 'smerge-upper ol-diff-green)

;; Copied and modified from smerge-mode. This is to make sure markers override
;; keywords from other modes
(defconst smerge-font-lock-keywords
  '((smerge-find-conflict
     (0 smerge-markers-face prepend t)
     (1 smerge-upper-face prepend t)
     (2 smerge-base-face prepend t)
     (3 smerge-lower-face prepend t)
     (4 nil t t)
     (5 nil t t))))

(defun ol-smerge-keep-both ()
  (interactive)
  (smerge-match-conflict)
  (delete-region (match-end 3) (match-end 0))
  (delete-region (match-end 1) (match-beginning 3))
  (delete-region (match-beginning 0) (match-beginning 1))
  (smerge-auto-leave))

;; TODO evil define key would be better but didn't work
(ol-define-key smerge-mode-map "C-c n" 'smerge-next)
(ol-define-key smerge-mode-map "C-c p" 'smerge-prev)

;; C-c r also used by vdiff, so avoid conflict
(ol-define-key ol-normal-leader-map "c l" 'smerge-keep-upper)
(ol-define-key ol-normal-leader-map "c r" 'smerge-keep-lower)
(ol-define-key ol-normal-leader-map "c b" 'ol-smerge-keep-both)
(ol-define-key ol-normal-leader-map "c a" 'smerge-keep-all)

(provide 'ol-smerge)
