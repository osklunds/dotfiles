#!/bin/bash

# Script for compiling my config files one-by-one. This is useful to check if
# they are requiring their dependencies properly on their own and can be
# compiled. One trick is to run this command from compile mode in emacs.

DOTFILES_REPO=$(git rev-parse --show-toplevel)

common_setup="
(progn
    (setq ol-repo-root \"$DOTFILES_REPO\")

    (setq ol-emacs-dir (file-name-concat ol-repo-root \".emacs_config\"))
    
    (add-to-list 'load-path (file-name-concat ol-emacs-dir \"config\"))
    (add-to-list 'load-path (file-name-concat ol-emacs-dir \"config_selective\"))
    (add-to-list 'load-path (file-name-concat ol-emacs-dir \"packages_own\"))
    (let ((default-directory (file-name-concat ol-emacs-dir \"packages/\")))
      (normal-top-level-add-subdirs-to-load-path))
    (let ((default-directory (file-name-concat ol-emacs-dir \"packages_own/\")))
      (normal-top-level-add-subdirs-to-load-path))

    (setq load-prefer-newer t)
    (setq byte-compile-error-on-warn t)
)
"

for file in config/*.el; do
    echo "$file"
    emacs --batch \
          --eval "$common_setup" \
          --eval "(byte-recompile-file \"$file\" t 0)"
done

