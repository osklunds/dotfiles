
set number relativenumber
set ts=4
set sw=4

set ignorecase
set colorcolumn=80

set formatoptions-=r
set formatoptions-=o

let NERDTreeShowHidden=1

autocmd BufNew * execute ":tabmove"

set autoread

set noswapfile
let g:auto_save = 1  " enable AutoSave on Vim startup
let g:auto_save_silent = 1  " do not display the auto-save notification

noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Left> <Nop>
noremap <Right> <Nop>

inoremap <Up> <Nop>
inoremap <Down> <Nop>
inoremap <Left> <Nop>
inoremap <Right> <Nop>

set langmap=Ö:,-/,&^,€$

syntax enable
colorscheme morning
highlight Constant ctermbg=15
highlight Normal ctermbg=15

" Plugins will be downloaded under the specified directory.
call plug#begin(has('nvim') ? stdpath('data') . '/plugged' : '~/.vim/plugged')

" Declare the list of plugins.
Plug 'preservim/nerdtree'
Plug 'https://github.com/907th/vim-auto-save'
" List ends here. Plugins become visible to Vim after this call.
call plug#end()

