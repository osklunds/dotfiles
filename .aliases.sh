
# Files
alias ll='ls -la'
alias ft='rg'
alias ff='rg --files | rg'
alias cdr='cd ~/repos'

# Git

## Misc
alias ga='git add'
alias gA='ga -A'
alias gr='cd $(git rev-parse --show-toplevel)'

## Commit
alias co='gA && coa'
alias coa='git commit -m'

## Info
alias gs='git status'
alias gl='git log'
alias gd='git diff'
alias gdt='git difftool -t vimdiff'

## Push/pull
alias gpl='git pull'
alias gps='git push --all origin'
alias gsm='git submodule add --force'

## Clean up
alias guc='git reset "HEAD^"'
alias gc='git restore --staged $(git rev-parse --show-toplevel) && git checkout $(git rev-parse --show-toplevel) && git clean -f && gs'
alias gco='git checkout'
grt() {
    git revert --no-commit "$1"..HEAD && co "Reverted to $1"
}

## Branches
alias gm='git merge'
alias gmt='git mergetool -t sk'
alias gb='git branch'

# Editing dotfiles
alias ev='e ~/.vim/vimrc'
alias eb='e ~/.bashrc'
alias ea='e ~/.aliases.sh && ra'
alias ra='source ~/.bashrc'

# Editors
alias e='vim'
alias em='emacs-gtk &'

# Cargo
alias cb='cargo build'
alias ct='cargo test'
alias cr='cargo run'
alias cf='cargo fmt'
alias rt='./run_tests.sh'

# Misc
lc() {
    rg --files | rg "\.$1" | xargs wc -l
}

