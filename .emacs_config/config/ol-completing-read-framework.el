;;;  -*- lexical-binding: t; -*-

;; I split my completing-read config into two files: framework and application.
;; The framework file has a dependency on e.g. vertico and consult, and is the
;; file that configures the external framework I use, sets up keybindgs for
;; functions I use directly. I might rename/split the framework file with one
;; for ivy/counsel as well, and maybe one for my own. The idea is that if I need
;; a new framework, the corresponding file needs to do the same keybindgs,
;; implement the same functions, etc as this one. Then the idea is that the
;; application file is framework agnostic by only using standard completing-read
;; functions.

(require 'ol-util)
(require 'ol-ert)

(require 'vertico)
(require 'embark)
(require 'consult)
(require 'consult-imenu)
(require 'orderless)

;; -----------------------------------------------------------------------------
;; Vertico
;; -----------------------------------------------------------------------------

(vertico-mode)

(setc vertico-count 20)
(setc vertico-cycle t)

;; For some reason, the orderless faces don't apply to switch-to-buffer until
;; more than one group is entered. However, for e.g. ol-dwim-find-file it looks
;; correct also with only one group. This fix also has the nice side effect
;; that everything between the first and last group is highlighed, which is what
;; I want.
(set-face-attribute 'completions-common-part nil
                    :foreground 'unspecified
                    :inherit 'orderless-match-face-0)
(set-face-attribute 'completions-first-difference nil
                    :foreground 'unspecified
                    :inherit 'orderless-match-face-0)

;; -----------------------------------------------------------------------------
;; Consult
;; -----------------------------------------------------------------------------

(setc consult-async-min-input 0)
(setc consult-async-split-style 'none)

(setc consult-find-args "find . -not ( -path *.git/* -prune )")
(setc consult-fd-args "fd --full-path --color=never --hidden --exclude *.git/*")



;; Not ideal to call consult internal functions, but hopefully the API is stable
;; enough, considering the public consult functions need similar
;; functionality. If something breaks I can compare the code at this commit and
;; how consult functions using consult--dynamic-collection change, and hopefully
;; figure something out. Need to stay optimistic.  In the worst case, I can live
;; with sync shell command. As a longer-term todo, figure out how it works.
(cl-defun ol-async-completing-read (&key collection
                                      prompt
                                      history
                                      require-match)
  (consult--read
   (consult--dynamic-collection collection)
   :prompt prompt
   :history 'ol-shell-command
   :require-match t
   ))

;; 1 -> (consult--join-regexps ("defun" "emacs") pcre)
;; 1 <- consult--join-regexps: "^(?=.*defun)(?=.*emacs)"
(defun ol-consult--join-regexps (regexps _type)
  (string-join regexps ".*"))

(set-face-attribute 'match nil
                    :foreground 'unspecified
                    :background 'unspecified
                    :inherit 'orderless-match-face-0)

;; -----------------------------------------------------------------------------
;; Orderless
;; -----------------------------------------------------------------------------

(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

(defun ol-orderfull (component)
  (string-join (split-string component) ".*"))

(setc orderless-component-separator #'list)
(setc orderless-matching-styles '(ol-orderfull))

;; -----------------------------------------------------------------------------
;; Keybindings
;; -----------------------------------------------------------------------------

(ol-define-key minibuffer-local-map "C-j" #'next-line)
(ol-define-key minibuffer-local-map "C-k" #'previous-line)
(ol-define-key minibuffer-local-map "C-n" #'minibuffer-keyboard-quit)
(ol-define-key minibuffer-local-map 'tab #'vertico-exit)
(ol-define-key minibuffer-local-map 'return #'vertico-exit-input)
(ol-define-key minibuffer-local-map "M-i" #'vertico-insert)
(ol-define-key minibuffer-local-map "M-o" #'embark-collect)
(ol-define-key minibuffer-local-map "M-e" #'embark-export)

(ol-define-key ol-override-map "C-j" #'switch-to-buffer)
(ol-define-key ol-normal-leader-map "m s" #'consult-imenu)


(provide 'ol-completing-read-framework)
