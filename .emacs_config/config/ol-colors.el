;; -*- lexical-binding: nil -*-

(require 'jka-compr) ;; To avoid problem with recursive load error
(require 'faces)
(require 'hl-line)
(require 'magit)

;; -----------------------------------------------------------------------------
;; Theme
;; -----------------------------------------------------------------------------

;; Set custom-theme--listed-faces to (face-list)
;; Then run customize-create-theme
(load-theme 'ol t)

;; -----------------------------------------------------------------------------
;; Own faces
;; -----------------------------------------------------------------------------

(defface ol-candidate-face
  `((default :weight normal :foreground ,ol-black :background ,ol-white))
  "Face for candidates in e.g. ivy and company.")

(defface ol-match-face
  '((default :weight bold :foreground "#4078f2" :background unspecified))
  "Face for matches in e.g. ivy and company.")

(defface ol-selection-face
  '((default :extend t :weight bold :background "#d7e4e8"))
  "Face for current selection in e.g. ivy and company.")

;; -----------------------------------------------------------------------------
;; Fonts
;; -----------------------------------------------------------------------------

;; To make the first char of 竹馬 also use the Japanese font.
(set-fontset-font t 'japanese-jisx0208
                  (font-spec :family "Noto Sans CJK JP"))

;; To make sure e.g. ♝ are monospaced
;; So use DejaVu Sans Mono instead of Source Code Pro for this
(set-fontset-font t 'symbol
                  (font-spec :family "DejaVu Sans Mono"))

;; todo: check in fonts

(provide 'ol-colors)
