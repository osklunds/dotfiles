
# Files
alias ll='ls -Alh --time-style=long-iso'
alias ft='rg'
alias ff='rg --files | rg'
alias cdr='cd ~/repos'
alias duh='du -sch .[!.]* * |sort -h'

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

# dotfiles
alias ra='source ~/.bashrc'
alias ec='edit_with_emacs'

# Rust
alias cb='cargo build'
alias cr='cargo run'
alias cf='\cargo fmt'
alias rt='./run_tests.sh'

if  command -v -- sort-cargo-errors > /dev/null 2>&1; then
    alias cargo='sort-cargo-errors'
fi

ct() {
    RUST_BACKTRACE=1 cargo test "$1" -- --show-output --test-threads 1 --color always
    # RUST_BACKTRACE=1 cargo test "$1" -- --nocapture --test-threads 1 --color always
    # RUST_BACKTRACE=1 cargo test "$1" -- --test-threads 1 --color always
}

# Misc
lc() {
    rg --files | rg "\.$1" | xargs wc -l
}

alias delete_all_docker_images='docker rmi -f $(docker images -aq)'

disk_space() {
    du -ach --max-depth=1 2> >(grep -v "Permission denied") | \
        sort -h | \
        grep -v "\.\$"
}

