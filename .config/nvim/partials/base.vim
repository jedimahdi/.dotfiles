set nocompatible              " be iMproved, required
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Core Functionality (general settings, keyboard shortcuts)
 """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" deal with swps and backups here
" create backups
set backup
" tell vim where to put its backup files
set backupdir=~/tmp/vim
" tell vim where to put swap files
set dir=~/tmp/vim
set timeoutlen=1000        " speed vim up
set ttimeoutlen=0          " https://stackoverflow.com/questions/37644682/why-is-vim-so-slow/37645334
set ttyfast                " Rendering
set tw=500
" Disable Autocommenting
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
" map jk to esc
" :imap jk <Esc>

" save with zz
" nnoremap zz :update<cr>

" set clipboard to easily copy from vim and paste into OSx
set clipboard=unnamed

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Text, tab and indent related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" set relativenumber

" Indentation
set tabstop=2
set shiftwidth=2
set ts=2 sts=2 sw=2 " 2 spaces tabs
set expandtab " Tabs -> Spaces
set autoindent " Follow indent on next line
set cindent " Auto indent braces

" Blink cursor on error instead of beeping (grr)
set visualbell
set t_vb=

" Set encoding
set encoding=utf-8  " The encoding displayed.
set fileencoding=utf-8  " The encoding written to file.

"Better line wrapping
set wrap
set linebreak
set textwidth=0
set wrapmargin=0

" Highlight current line
set cursorline

" Allow mouse
set mouse=a

" Hide Mode line
set noruler
set laststatus=0
set noshowcmd

" Automatically write the file when switching buffers.
set autowriteall

" Theme settings 
set background=dark
colorscheme tomorrow-night
" colorscheme one
" colorscheme apprentice

if (has("termguicolors"))
  set termguicolors
endif
if (has('nvim'))
  let $NVIM_TUI_ENABLE_TRUE_COLOR = 1
endif

let g:material_theme_style = 'ocean'
let g:material_terminal_italics = 1


" Syntax and colors
set t_Co=256
let base16colorspace=256 " https://github.com/chriskempson/base16-vim
syntax enable

" Persist undo over buffer switches and exits
" :silent call system('mkdir -p ' . $HOME . '/.vim/undo')
" set undofile
" set undodir=$HOME/.vim/undo
" set undolevels=1000
" set undoreload=10000

" always uses spaces instead of tab characters
filetype plugin indent on

" from readme
" if hidden is not set, TextEdit might fail.
set hidden " Some servers have issues with backup files, see #649 set nobackup set nowritebackup " Better display for messages set cmdheight=2 " You will have bad experience for diagnostic messages when it's default 4000.
set updatetime=300

" don't give |ins-completion-menu| messages.
set shortmess+=c

" always show signcolumns
set signcolumn=yes

let g:python3_host_prog = '/usr/bin/python'
let g:python_host_prog = '/usr/bin/python2'
let g:ruby_host_prog = '~/.gem/ruby/2.6.0/bin/neovim-ruby-host'
let g:loaded_ruby_provider = 0
