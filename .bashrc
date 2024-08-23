
PS1='$PWD\$ '

export SHELL_SESSION_HISTORY=0
export BASH_SILENCE_DEPRECATION_WARNING=1
export GREP_OPTIONS='--color=always'
export LSP_USE_PLISTS=true
export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"
export FZF_DEFAULT_COMMAND="rg --files"

export DOTFILES_REPO="$HOME/dotfiles"
export PATH="$DOTFILES_REPO/scripts:$PATH"

[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env"
[ -f "$HOME/.cargo/env" ] && source "$HOME/.cargo/env"
[ -d "$HOME/.cargo/bin" ] && export PATH="$HOME/.cargo/bin:$PATH"

source "$HOME/.aliases.sh"

# TODO: Specify this in emacs instead
source "$HOME/dotfiles/.emacs_config/packages/emacs-libvterm/etc/emacs-vterm-bash.sh"
