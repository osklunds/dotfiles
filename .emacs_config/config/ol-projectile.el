
(require 'ol-util)
(require 'ol-evil)
(require 'ol-project)

;; -----------------------------------------------------------------------------
;; Projectile
;; -----------------------------------------------------------------------------

(require 'projectile)
(require 'counsel-projectile)
(require 'projectile-ripgrep)

(setc projectile-completion-system 'ivy)
(setc ivy-more-chars-alist '((t . 1)))

(when ol-use-projectile
  (call-interactively 'projectile-mode))

;;;; ---------------------------------------------------------------------------
;;;; Project discovery
;;;;----------------------------------------------------------------------------

(setc projectile-project-search-path '(("~/own_repos" . 1)
                                       ("~/others_repos" . 1)
                                       ("~/own_repos/dotfiles/.emacs_config/packages" . 1)
                                       ("~/Dropbox/Dokument")))

(defun ol-projectile-discover ()
  (interactive)
  (projectile-clear-known-projects)
  (projectile-discover-projects-in-search-path))

(ol-define-normal-leader-key "ps" 'ol-projectile-discover)

(setc projectile-auto-discover nil)
(setc projectile-auto-update-cache nil)
(setc projectile-indexing-method 'alien)

;;;; ---------------------------------------------------------------------------
;;;; Project selection
;;;;----------------------------------------------------------------------------

(ol-define-normal-leader-key "pp" 'ol-fallback-switch-to-project-interactive)
(ol-define-normal-leader-key "pd" 'ol-switch-to-dotfiles)

(defun ol-switch-to-dotfiles ()
  (interactive)
  (ol-fallback-switch-to-project "~/own_repos/dotfiles/"))

(setc projectile-switch-project-action 'ol-dwim-find-file-name)

(defun ol-projectile-commander (&rest args)
  (projectile-dired))

(advice-add 'projectile-commander :override 'ol-projectile-commander)

;;;; ---------------------------------------------------------------------------
;;;; Commands within projects
;;;;----------------------------------------------------------------------------

(ol-override-key "M-q" 'ol-dwim-find-file-name)
(ol-override-key "M-e" 'ol-dwim-find-file-content)

(provide 'ol-projectile)
