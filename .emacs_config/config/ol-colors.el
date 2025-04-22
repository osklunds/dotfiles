;; -*- lexical-binding: nil -*-

(require 'jka-compr) ;; To avoid problem with recursive load error
(require 'faces)
(require 'hl-line)
(require 'doom-themes)

(load-theme 'doom-one-light t)

;; -----------------------------------------------------------------------------
;; Helpers (to be removed)
;; -----------------------------------------------------------------------------

(defun ol-set-face (face &rest properties)
  "Wrapper around `set-face-attribute' that sets for all frames."
  (apply 'set-face-attribute (append `(,face nil) properties)))

(defun ol-copy-face (to attribute from)
  "Sets `attribute' on `to' based on the value of `attribute' in `from'.
Maybe inherit would be cleaner to use."
  (ol-set-face to attribute (face-attribute from attribute)))

(defun ol-copy-face-fg-bg (to from)
  "Sets `:foreground' and `:background' on `to' based on the values in `from'
Maybe inherit would be cleaner to use."
  (ol-copy-face to :foreground from)
  (ol-copy-face to :background from))

;; -----------------------------------------------------------------------------
;; Color definitions
;; -----------------------------------------------------------------------------

(defconst ol-white "#ffffff") ;; ff works better than white in terminal
(defconst ol-black "#000000")

;; -----------------------------------------------------------------------------
;; Setting some faces
;; -----------------------------------------------------------------------------

(ol-set-face 'default :foreground ol-black :background ol-white)
(ol-set-face 'font-lock-comment-face :foreground "#5f8700")
(ol-set-face 'font-lock-string-face :foreground "#d78700")

(unless (display-graphic-p)
  (ol-set-face 'lazy-highlight :background "#c2d3f7" :foreground ol-white)
  (ol-set-face 'hl-line :background "#eeeeee"))

;; To prevent alignment issue in e.g. markdown-mode with variable-pitch
(set-face-attribute 'show-paren-match nil :weight 'bold)

;; -----------------------------------------------------------------------------
;; Own faces
;; -----------------------------------------------------------------------------

(defface ol-match-face
  '((default :weight bold :foreground "#4078f2" :background unspecified))
  "Face for matches in e.g. ivy and company.")

(defface ol-selection-face
  '((default :extend t :weight bold :background "#d7e4e8"))
  "Face for current selection in e.g. ivy and company.")

;; -----------------------------------------------------------------------------
;; Fonts
;; -----------------------------------------------------------------------------

(defun ol-font-available-p (font)
  (when (cl-member font (font-family-list) :test 'string-equal)
    font))

;; todo: check in fonts
(defconst ol-fixed-pitch-font (or (ol-font-available-p "Source Code Pro")
                                  "DejaVu Sans Mono"))

;; inheriting from default doesn't work, so need to specify manully for both
;; default and fixed-pitch
(set-face-attribute 'fixed-pitch nil :family ol-fixed-pitch-font :height 90)
(set-face-attribute 'default nil :family ol-fixed-pitch-font :height 90)
(set-face-attribute 'line-number nil :family ol-fixed-pitch-font)
(set-face-attribute 'line-number-current-line nil :family ol-fixed-pitch-font)

(defconst ol-variable-pitch-font (or (ol-font-available-p "Open Sans")
                                     "DejaVu Sans"))

(set-face-attribute 'variable-pitch nil
                    :family ol-variable-pitch-font
                    :height 1.1
                    :weight 'normal)

(provide 'ol-colors)
