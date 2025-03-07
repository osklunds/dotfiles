
;; To avoid problem with recursive load error
(require 'jka-compr)
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

(provide 'ol-colors)
