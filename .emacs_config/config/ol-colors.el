
(require 'jka-compr) ;; To avoid problem with recursive load error
(require 'faces)

(require 'doom-themes)

;; todo copy but only keep this one
(load-theme 'doom-one-light t)

(defun ol-set-face (face &rest properties)
  (apply 'set-face-attribute (append (list face nil) properties)))

(defun ol-copy-face (to attribute from)
  (ol-set-face to attribute (face-attribute from attribute)))

(defun ol-copy-face-fg-bg (to from)
  (ol-copy-face to :foreground from)
  (ol-copy-face to :background from))

(ol-set-face 'default :height 90)

(defconst ol-white "#ffffff") ;; ff works better than white in terminal
(defconst ol-black "#000000")

(ol-set-face 'default :foreground ol-black :background ol-white)
(ol-set-face 'font-lock-comment-face :foreground "#5f8700")
(ol-set-face 'font-lock-string-face :foreground "#d78700")

(unless (display-graphic-p)
  (ol-set-face 'lazy-highlight :background "#c2d3f7" :foreground ol-white)
  (ol-set-face 'hl-line :background "#eeeeee"))

(provide 'ol-colors)
