
PS1='$PWD\$ '

export SHELL_SESSION_HISTORY=0
export BASH_SILENCE_DEPRECATION_WARNING=1
export GREP_OPTIONS='--color=always'

[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env"
. "$HOME/.cargo/env"

source "$HOME/.aliases.sh"

