
PS1='$PWD\$ '

export SHELL_SESSION_HISTORY=0
export BASH_SILENCE_DEPRECATION_WARNING=1
export GREP_OPTIONS='--color=always'

export DOTFILES_REPO="$HOME/dotfiles"

[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env"
[ -f "$HOME/.cargo/env" ] && source "$HOME/.cargo/env"

source "$HOME/.aliases.sh"

export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"
export FZF_DEFAULT_COMMAND="rg --files"

