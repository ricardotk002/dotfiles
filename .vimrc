set shell=/bin/sh
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" My Bundles
Plugin 'wincent/command-t'
Plugin 'scrooloose/nerdtree'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

" Defaults
set backspace=2           " Disable the delete button (backspace) in normal mode
set number                " Enable the line numbers
set numberwidth=5
set noswapfile            " Disable swap files creation
set ruler                 " Show the cursor position all the time
set laststatus=2          " Always display the status line
set showcmd               " Display incomplete commands
set cursorline
hi cursorline cterm=none term=none

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
