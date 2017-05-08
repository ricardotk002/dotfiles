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
set relativenumber            " show relative line numbers
" set number                    " Enable the line numbers
set noswapfile                " Disable swap files creation

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" My Bundles
Plugin 'chriskempson/base16-vim'
Plugin 'scrooloose/nerdtree'
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-endwise'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-surround'
Plugin 'airblade/vim-gitgutter'
Plugin 'godlygeek/tabular'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'mileszs/ack.vim'
" Plugin 'othree/yajs.vim'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'
" Plugin 'rizzatti/dash.vim'
Plugin 'w0rp/ale'
" Plugin 'morhetz/gruvbox'

" Open new split panes naturally to the right and bottom
set splitbelow
set splitright

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

" Switch syntax highlighting on, when the terminal has colors
if (&t_Co > 2 || has("gui_running")) && !exists("syntax_on")
  syntax on
endif

" set background dark and colocrscheme
set background=dark
let base16colorspace=256
" colorscheme gruvbox
colorscheme base16-default
" color Tomorrow-Night

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

" ctrlp configuration
let g:ctrlp_map = '<c-p>'
let g:ctrlp_max_height = 15
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_match_window_reversed = 0
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*/node_modules/*

" hint to keep lines short
if exists('+colorcolumn')
  set colorcolumn=80
endif

" set leader key to comma
let mapleader = ","

" JSX
let g:jsx_ext_required = 0 " Allow JSX in normal JS files
" let g:syntastic_javascript_checkers = ['eslint']
" let g:syntastic_ruby_checkers = ['rubocop']
" let g:syntastic_ignore_files = ['^/usr/', '*node_modules*', '*vendor*', '*build*', '*LOCAL*', '*BASE', '*REMOTE*']
