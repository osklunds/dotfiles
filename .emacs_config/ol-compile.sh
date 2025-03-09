#!/bin/bash

# Script for compiling my config files one-by-one. This is useful to check if
# they are requiring their dependencies properly on their own and can be
# compiled. One trick is to run this command from compile mode in emacs.

common_setup="
(progn
    (setq load-path (append load-path
                            '(\"~/.emacs_config/config\")
                            '(\"~/.emacs_config/packages_own/\")
                            (file-expand-wildcards \"~/.emacs_config/packages_own/*\")
                            (file-expand-wildcards \"~/.emacs_config/packages/*\")
                            (file-expand-wildcards \"~/.emacs_config/packages/*/src/*\")
                            (file-expand-wildcards \"~/.emacs_config/packages/*/clients\")
                            (file-expand-wildcards \"~/.emacs_config/packages/*/lisp\"))
          )
    
    (setq byte-compile-error-on-warn t)
)
"

emacs --batch --eval "$common_setup"

for file in config/*.el; do
    echo "$file"
    emacs --batch \
          --eval "$common_setup" \
          --eval "(byte-recompile-file \"$file\" t 0)"
done

