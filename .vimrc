" ~/.vimrc
set encoding=utf-8
set shell=/bin/sh

set nocompatible              " be iMproved, required
set autoindent                " set auto indent
set ts=2                      " set indent to 2 spaces
set shiftwidth=2
set expandtab
set showmatch                 " show bracket matches
set ignorecase                " ignore case in searches
set hlsearch                  " highlight all search matches
set cursorline                " highlight current line
set smartcase                 " pay attention to case when caps are used
set incsearch                 " show search results as I type
set vb                        " enable visual bell (disable audio bell)
set ruler                     " show row and column in footer
set scrolloff=2               " minimum lines above/below cursor
set laststatus=2              " always show last status
set list listchars=tab:··,trail:· " show extra space characters
set nofoldenable              " disable code folding
set clipboard=unnamed         " use the system clipboard
set backspace=2               " Disable the delete button (backspace) in normal mode
set number                    " Enable the line numbers
" set numberwidth=5
set noswapfile                " Disable swap files creation

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" My Bundles
Plugin 'scrooloose/nerdtree'
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-endwise'
Plugin 'ctrlpvim/ctrlp.vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

" Color scheme
color Tomorrow-Night

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if (&t_Co > 2 || has("gui_running")) && !exists("syntax_on")
  syntax on
endif

" Set font
if has("gui_running")
  set guifont=Monaco
end

" Get off my lawn
nnoremap <Left> :echoe "Use h"<CR>
nnoremap <Right> :echoe "Use l"<CR>
nnoremap <Up> :echoe "Use k"<CR>
noremap <Down> :echoe "Use j"<CR>

" Nerdtree conf
map <C-n> :NERDTreeToggle<CR>

" CtrlP configuration
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_max_height = 10
let g:ctrlp_working_path_mode = 0
let g:ctrlp_match_window_reversed = 0
