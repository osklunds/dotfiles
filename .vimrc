
set number relativenumber
set ts=4 sw=4

set ignorecase
set colorcolumn=80
colorscheme one

let NERDTreeShowHidden=1
" au VimEnter *  NERDTree

autocmd BufNew * execute ":tabmove"

augroup autosave
     autocmd!
"     autocmd BufRead * if &filetype == "" | setlocal ft=text | endif
     autocmd FileType * autocmd TextChanged,InsertLeave * if &readonly == 0 | silent write | endif
augroup END

noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Left> <Nop>
noremap <Right> <Nop>

inoremap <Up> <Nop>
inoremap <Down> <Nop>
inoremap <Left> <Nop>
inoremap <Right> <Nop>

" Plugins will be downloaded under the specified directory.
call plug#begin(has('nvim') ? stdpath('data') . '/plugged' : '~/.vim/plugged')

" Declare the list of plugins.
Plug 'preservim/nerdtree'

" List ends here. Plugins become visible to Vim after this call.
call plug#end()

