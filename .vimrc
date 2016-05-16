set nocompatible
set backspace=2
set backspace=indent,eol,start
set expandtab
set tabstop=2
set softtabstop=2
set shiftwidth=2
set autoindent
set smarttab
set textwidth=80
set nowrap
set number
set showmatch
set ignorecase
set hlsearch
set incsearch
set undolevels=1000
set nobackup
set noswapfile
set mouse=a
set wildmenu
set showcmd
set nostartofline
set ruler
set cursorline
set laststatus=2
set visualbell
set t_vb=
set cmdheight=1
set notimeout ttimeout ttimeoutlen=200
set t_Co=256


syntax on


" colorscheme xterm16

if (exists('+colorcolumn'))
    set colorcolumn=80
    highlight ColorColumn ctermbg=9
endif


" Mappings (not related to plugins) start from here...

map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>


" Custom functions start from here....

func! WordProcessorMode()
 setlocal textwidth=80
 setlocal smartindent
 setlocal spell spelllang=en_gb
 setlocal noexpandtab
endfu

com! WP call WordProcessorMode()


" Plugins start from here...

" vundle
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'flazz/vim-colorschemes'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'jistr/vim-nerdtree-tabs'
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'scrooloose/syntastic'
Plugin 'bronson/vim-trailing-whitespace'
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
Plugin 'JamshedVesuna/vim-markdown-preview'
call vundle#end()
filetype plugin indent on

" nerdtree
map <C-n> :NERDTreeTabsToggle<CR>
let g:nerdtree_tabs_open_on_console_startup=1

" airline
let g:airline#extensions#tabline#enabled=1

" syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" vim-markdown
let g:vim_markdown_math = 1
