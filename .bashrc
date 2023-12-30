
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

source "$HOME/.aliases.sh"

# Copied from emacs-libvterm
function vterm_printf(){
    printf "\e]%s\e\\" "$1"
}

# Copied from emacs-libvterm
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    function clear(){
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    }
fi

# Copied from emacs-libvterm (but also modified)
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND; }"'echo -ne "\033]0;${PWD}\007"'

# Copied from emacs-libvterm
vterm_prompt_end(){
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}

# Copied from emacs-libvterm
PS1=$PS1'\[$(vterm_prompt_end)\]'
