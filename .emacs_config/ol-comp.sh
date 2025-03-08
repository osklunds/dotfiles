#!/bin/bash


for file in config/*.el; do
    echo "$file"
    emacs --batch \
          --load "ol-comp.el" \
          --eval "(byte-recompile-file \"$file\" t 0)" || break
done

