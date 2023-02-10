
# Files
alias ll='ls -la'
alias ft='grep -nr . -ie '
alias ff='find . | grep -i '

# Git
alias co='git commit -a -m'
alias coa='git add -A && co'
alias gs='git status'
alias gl='git log'
alias gc='git restore --staged $(git rev-parse --show-toplevel) && git checkout $(git rev-parse --show-toplevel) && git clean -f && gs'
alias gco='git checkout'
alias gd='git diff'
alias gpl='git pull'
alias gps='git push --all origin'
alias guc='git reset "HEAD^"'
alias ga='git add'
alias gm='git merge'

grt() {
    git revert --no-commit "$1"..HEAD && co "Reverted to $1"
}

alias gb='git branch'

# Editing dotfiles
alias ev='e ~/.vim/vimrc'
alias eb='e ~/.bashrc'
alias ea='e ~/.aliases.sh && ra'
alias ra='source ~/.aliases.sh'

# Editors
alias e='vim'

# Cargo
alias cb='cargo build'
alias ct='cargo test'
alias cr='cargo run'
alias cf='cargo fmt'
alias rt='./run_tests.sh'
