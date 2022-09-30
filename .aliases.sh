
export GREP_OPTIONS='--color=always'

# Files
alias ll='ls -la'
alias ft='grep -nr . -ie '
alias ff='find . | grep -i '

# Git
alias co='git commit -a -m'
alias gs='git status'
alias gl='git log'
alias gc='git restore --staged $(git rev-parse --show-toplevel) && git checkout $(git rev-parse --show-toplevel) && git clean -f && gs'
alias gco='git checkout'
alias gd='git diff'
alias gpl='git pull'
alias gps='git push --all origin'
alias guc='git reset "HEAD^"'
alias ga='git add -A'
alias gm='git merge'

grt() {
    git revert --no-commit "$1"..HEAD && co "Reverted to $1"
}

alias gb='git branch'

# Aliases
alias ra='source ~/.bash_profile'

# Editors
alias e='vim'
