
# Files
alias ll='ls -la'
alias ft='grep -nr . -ie '

# Git
alias co='git commit -a -m'
alias gs='git status'
alias gl='git log'
alias gc='git checkout $(git rev-parse --show-toplevel) && git clean -f && gs'
alias gd='git diff'
alias gpl='git pull'
alias gps='git push'
alias guc='git reset "HEAD^"'

grt() {
    git revert --no-commit "$1"..HEAD && co "Reverted to $1"
}

# Aliases
alias ra='source ~/.bash_profile'

# Editors
alias e='vim'
