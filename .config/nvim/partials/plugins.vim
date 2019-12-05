" Specify a directory for plugins
call plug#begin('~/.vim/plugged')

" Themes
Plug 'mhartington/oceanic-next'
Plug 'kaicataldo/material.vim'
Plug 'ryanoasis/vim-devicons'
Plug 'rakr/vim-one'
Plug 'sainnhe/edge'
Plug 'romainl/Apprentice'
Plug 'NLKNguyen/papercolor-theme'
Plug 'joshdick/onedark.vim'


" Language Syntax Support
Plug 'pangloss/vim-javascript' "JS highlighting
Plug 'mxw/vim-jsx' "JSX syntax highlighting
Plug 'posva/vim-vue'

" Tools
Plug 'mitermayer/vim-prettier'
" Plug 'jiangmiao/auto-pairs' "Autocomplete brackets. 
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'tomtom/tcomment_vim'
Plug 'tpope/vim-fugitive' "Git tools
Plug 'tpope/vim-surround'
Plug 'tpope/vim-vinegar'
Plug 'farmergreg/vim-lastplace'                             " Restore cursor position
Plug 'terryma/vim-multiple-cursors'

Plug 'neoclide/coc.nvim', {'branch': 'release'} "autocompletion
Plug 'scrooloose/nerdtree', {'on': 'NERDTreeToggle'} "Nerdtree


" Plug 'ervandew/supertab'

"Plug 'prettier/vim-prettier', { 'do': 'yarn install' }

Plug 'christoomey/vim-tmux-navigator'

"Plug 'HerringtonDarkholme/yats.vim' " TS Syntax

" Initialize plugin system
call plug#end()
