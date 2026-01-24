

# Copied from https://stackoverflow.com/a/246128
get_script_dir()
{
    local SOURCE_PATH="${BASH_SOURCE[0]}"
    local SYMLINK_DIR
    local SCRIPT_DIR
    # Resolve symlinks recursively
    while [ -L "$SOURCE_PATH" ]; do
        # Get symlink directory
        SYMLINK_DIR="$( cd -P "$( dirname "$SOURCE_PATH" )" >/dev/null 2>&1 && pwd )"
        # Resolve symlink target (relative or absolute)
        SOURCE_PATH="$(readlink "$SOURCE_PATH")"
        # Check if candidate path is relative or absolute
        if [[ $SOURCE_PATH != /* ]]; then
            # Candidate path is relative, resolve to full path
            SOURCE_PATH=$SYMLINK_DIR/$SOURCE_PATH
        fi
    done
    # Get final script directory path from fully resolved source path
    SCRIPT_DIR="$(cd -P "$( dirname "$SOURCE_PATH" )" >/dev/null 2>&1 && pwd)"
    echo "$SCRIPT_DIR"
}

DOTFILES_REPO=$(get_script_dir)

export SHELL_SESSION_HISTORY=0
export BASH_SILENCE_DEPRECATION_WARNING=1
export GREP_OPTIONS='--color=always'
export LSP_USE_PLISTS=true
export RIPGREP_CONFIG_PATH="$DOTFILES_REPO/.ripgreprc"
export FZF_DEFAULT_COMMAND="rg --files"

export PATH="$DOTFILES_REPO/scripts:$PATH"

[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env"
[ -f "$HOME/.cargo/env" ] && source "$HOME/.cargo/env"
[ -d "$HOME/.cargo/bin" ] && export PATH="$HOME/.cargo/bin:$PATH"
[ -d "$HOME/.jdtls/bin" ] && export PATH="$HOME/.jdtls/bin:$PATH"

source "$DOTFILES_REPO/.aliases.sh"

# Temp disabled for docker reasons
# source "$DOTFILES_REPO/.emacs_config/packages/emacs-libvterm/etc/emacs-vterm-bash.sh"

# Docker
export BUILDKIT_PROGRESS=plain

# C++

export CXX=/usr/bin/clang++-20

# Different brackets to notice where I am
if [[ -n "$INSIDE_EMACS" ]]; then
    if [[ "$HOSTNAME" == "dev-env" ]]; then
        PS1='[$PWD] '
    else
        PS1='($PWD) '
    fi

    # Redefine cd after all symlink magic is over
    function cd () {
        builtin cd "$@"
        call_emacs.sh "(ol-vterm-shell-cwd-changed \"$(pwd)\")" &> /dev/null
    }
else
    if [[ "$HOSTNAME" == "dev-env" ]]; then
        PS1='$PWD]] '
    else
        PS1='$PWD)) '
    fi

    unset -f cd
fi
