
PS1='$PWD\$ '

# Copied from https://stackoverflow.com/a/246128
DOTFILES_REPO=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

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

source "$DOTFILES_REPO/.aliases.sh"

source "$DOTFILES_REPO/.emacs_config/packages/emacs-libvterm/etc/emacs-vterm-bash.sh"

# Docker
export BUILDKIT_PROGRESS=plain
