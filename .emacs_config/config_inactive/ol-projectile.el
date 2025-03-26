
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

(ol-define-key ol-normal-leader-map "p s" 'ol-projectile-discover)

(setc projectile-auto-discover nil)
(setc projectile-auto-update-cache nil)
(setc projectile-indexing-method 'alien)

(provide 'ol-projectile)
