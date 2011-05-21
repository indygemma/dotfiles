" ~/.vimrc
" 
" Author: Conrad Indiono
"
" How to Install:
"
" 1) install Vundle via
"
"   git clone http://github.com/gmarik/vundle.git ~/.vim/vundle.git
"
" 2) in vim call
"
"   :BundleInstall!
"
set nocompatible
filetype off

set rtp+=~/.vim/vundle.git/
call vundle#rc()

Bundle 'tpope/vim-fugitive'

filetype plugin on
