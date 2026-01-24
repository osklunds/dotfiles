#!/bin/bash

emacsclient --eval "$1" --socket-name "$EMACS_SERVER_NAME"
