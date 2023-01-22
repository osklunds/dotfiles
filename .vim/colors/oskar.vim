" Inspired by Sublime's Breakers color scheme

" Prepration
hi clear Normal
set bg&

hi clear

if exists("syntax_on")
  syntax reset
endif

let colors_name = "oskar"

set termguicolors

" Colors
hi Normal guifg=#303030 guibg=#f9fafa

hi Statement guifg=#c695c6

hi Constant guifg=#f9ae58
hi String guifg=#99c794

hi Visual guibg=#e3e6e8

hi Identifier guifg=#6090c0
hi Function guifg=#6090c0

hi PreProc guifg=#6090c0

hi Type guifg=#f9ae58

hi Special guifg=#f97b58

hi Comment guifg=#909090
hi Todo guibg=#5fb4b4 guifg=#303030

" Rust specific
hi RustRedGroup guifg=#ec5f66
syn keyword RustRedGroup pub mut async
