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

""" Bundle List START

Bundle 'tpope/vim-fugitive'
Bundle 'flazz/vim-colorschemes'
Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/nerdtree'
Bundle 'Lokaltog/vim-easymotion'
" Fuzzyfinder requires 'L9' as dependency
Bundle 'L9'
Bundle 'FuzzyFinder'

" To properly install "command-t" don't forget to compile the C-extension via
"
"   $ cd ~/.vim/bundle/command-t/ruby/command-t
"   $ ruby extconf.h
"   $ make
"
Bundle 'git://git.wincent.com/command-t.git'

""" Bundle List END

syntax on
syntax enable
filetype on
filetype plugin on
filetype plugin indent on " enable file-level indenting with gg=G
set tabstop=4
set smarttab
set shiftwidth=4
set autoindent
"set smartindent
set expandtab
set number
set incsearch
set hlsearch
set hidden
set ignorecase
set smartcase
set title

set showcmd " show typed-in commands in normal mode

""" =======
""" GENERAL {
""" =======

    let mapleader = ","

    " autoreload vimrc on save
    "if has("autocmd")
    "    autocmd bufwritepost .vimrc source $MYVIMRC
    "endif
    " source vimrc with ,s
    map <Leader>s :source $MYVIMRC<cr>
    " open up .vimrc in a vertical split with ,v
    map <Leader>vv <c-w><c-v>:e $MYVIMRC<cr>

    " highlight current line
    au WinLeave * set nocursorline
    au WinEnter * set cursorline
    set cursorline

    " toggle paste mode with <F4>
    set pastetoggle=<F4>

    " display tabs and spaces
    set listchars=tab:»·,trail:· " show trailing spaces as a circle. turn off with 'set nolist'
    set list

    " copy/paste with the system clipboard
    "map ^P "+gP
    "map ^C "+y
    " yank and paste work with the system clipboard transparently
    set clipboard=unnamed

    set history=1000

    set wildmenu
    set wildmode=list:longest

    set scrolloff=5 " scroll at 5 lines before going up or down

    set visualbell

    " set the backup dir to declutter working directory.
    " two ending slashes means, full path to the actual filename
    silent! !mkdir -p ~/.vim/backup
    silent! !mkdir -p ~/.vim/swap
    set backup
    set backupdir=~/.vim/backup//
    set directory=~/.vim/swap//

""" }

""" ===========
""" GUI options {
""" ===========
    set guioptions-=m
    set guioptions-=T
    set guifont=Bitstream\ Vera\ Sans\ Mono\ 10
""" }

""" =============
""" Color Schemes {
""" =============
    set background=dark
    "colorscheme ir_black
    "colorscheme mustang
    colorscheme molokai
    " another good one is
    "set background=light
    "colorscheme zen
""" }

""" ============
""" Key Mappings {
""" ============
    nnoremap ' `
    nnoremap ` '
    map <C-down> ddp
    map <C-Up> dd<Up>P

    " if your forget "sudo vim <file>", get prompted for the password inside vim
    cmap w!! %!sudo tee > /dev/null %

    " change to a buffer
    map <A-b> :FufBuffer<cr>

    " map Cmd-T
    map <A-t> :CommandT<cr>
    "map <A-t> :FufFile<cr>

    " map NerdTree
    map ö :NERDTreeToggle<cr>

    " ctrl-s saves the current document
    map <C-s> :w<cr>

    " movement quicker jump between windows
    map <C-j> <C-W>j
    map <C-k> <C-W>k
    map <C-h> <C-W>h
    map <C-l> <C-W>l

    map <C-=> <C-W>=
    map <C->> <C-W>>
    map <C-<> <C-W><
    map <C--> <C-W>-
    map <C-+> <C-W>+

    " buffer management
    map <right> :bn<CR>
    map <left>  :bp<CR>
    map <Leader>bd  :bd<CR>

    " change directory to the directory of the current file
    map <Leader>cd :cd %:p:h<cr>

    """ custom markdown
    map <leader>u i<task><esc>A</task><esc>
    map <leader>U :s/<[\/]*task>//g<cr>

    """ jump around faster with alt-k/-j
    map <a-k> 5k<cr>
    map <a-j> 5j<cr>
    map <a-l> 5l<cr>
    map <a-h> 5h<cr>

    """ delete whole words in insert mode with ctrl + backspace
    imap <S-Backspace> <C-[>diwi

    " quicker saving
    map W :w<cr>

    """ bicycle repair man commands
    vmap <Leader>e :BikeExtract<cr>
    map <Leader>bu :BikeUndo<cr>

    " open a scratch file in vertical split (requires scratch.vim)
    "map _s :Sscratch<cr>

""" }

""" =================
""" Window Management
""" =================

" from http://vim.wikia.com/wiki/Maximize_window_and_return_to_previous_split_structure
nnoremap <C-W>O :call MaximizeToggle ()<CR>
nnoremap <C-W>o :call MaximizeToggle ()<CR>
nnoremap <C-W><C-O> :call MaximizeToggle ()<CR>

function! MaximizeToggle()
    if exists("s:maximize_session")
        exec "source " . s:maximize_session
        call delete(s:maximize_session)
        unlet s:maximize_session
        let &hidden=s:maximize_hidden_save
        unlet s:maximize_hidden_save
    else
        let s:maximize_hidden_save = &hidden
        let s:maximize_session = tempname()
        set hidden
        exec "mksession! " . s:maximize_session
        only
    endif
endfunction

""" ===========
""" Status Line
""" ===========
    " Always hide the statusline
    set laststatus=2

    " Format the statusline
    set statusline=\ %{HasPaste()}%F%m%r%h\ %w\ \ CWD:\ %r%{CurDir()}%h\ \ \ Line:\ %l/%L:%c\ %{fugitive#statusline()}\ %P
    "set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P


    function! CurDir()
        " warning. not crossplatform enough
        let curdir = substitute(getcwd(), '/home/conrad/', "~/", "g")
        return curdir
    endfunction

    function! HasPaste()
        if &paste
            return 'PASTE MODE  '
        else
            return ''
        endif
    endfunction


""" =======
""" Autocmd {
""" =======
    autocmd BufRead *.as set filetype=actionscript
    autocmd FileType python set ft=python.django
    autocmd FileType html set ft=htmldjango.html
    autocmd FileType shpaml set fletype=html
    autocmd BufRead *.shpaml set filetype=html
    autocmd BufRead *.less set filetype=css
    autocmd BufRead *.wsdl set filetype=xml
    autocmd BufRead *.hx set filetype=haxe
    autocmd BufRead,BufNewFile *.json set filetype=json
    autocmd BufRead,BufNewFile *.py compiler nose
    autocmd BufRead,BufNewFile *.mlua set ft=lua
    autocmd BufRead,BufNewFile *.viki set ft=viki
""" }

""" =======
""" Clojure {
""" =======
    let vimclojure#HighlightBuiltins = 1 " Highlight Clojure's builtins
    let vimclojure#ParenRainbow = 1      " Rainbow parentheses
    let vimclojure#NailgunClient = "/home/conrad/clojure/ng"
    let vimclojure#WantNailgun = 1
    let vimclojure#SplitSize = 10        " make the window panel a bit smaller
""" }

""" ========
""" Quickfix
""" ========
    """ quickly open/close quickfix window
    command! -bang -nargs=? QFix call QFixToggle(<bang>0)
    function! QFixToggle(forced)
        if exists("g:qfix_win") && a:forced == 0
            cclose
            unlet g:qfix_win
        else
            copen 10
            let g:qfix_win = bufnr("$")
        endif
    endfunction
    nmap \\ :QFix<cr>

""" ========================================================================
""" Jump to last cursor when opening a file. Ignore when file is commit log.
""" ========================================================================
    autocmd BufReadPost * call SetCursorPosition()
    function! SetCursorPosition()
        if &filetype !~ 'commit\c'
            if line("'\"") > 0 && line("'\"") <= line("$")
                exe "normal g`\""
            endif
        end
    endfunction

""" ================================================
""" Highlight inconsistencies mixing tabs and spaces
""" ================================================
    highlight BadSpacing term=standout ctermbg=cyan
    augroup Spacing
        autocmd!
        " Highlight tabulators and trailing spaces (nasty bastards)
        autocmd BufNewFile,BufReadPre * match BadSpacing /\(\t\|  *$\)/
        " Only highlight trailing space in tab-filled formats
        autocmd FileType help,make match BadSpacing /  *$/
    augroup END

""" ===========
""" Visual Mode
""" ===========
    " allow * and # search in visual mode
    vnoremap <silent> *  :call VisualSearch("f")<CR>
    vnoremap <silent> #  :call VisualSearch("#")<CR>
    vnoremap <silent> gv :call VisualSearch("gv")<CR>

    function! CmdLine(str)
        exe "menu Foo.Bar :" . a:str
        emenu Foo.Bar
        unmenu Foo
    endfunction 

    " From an idea by Michael Naumann
    function! VisualSearch(direction) range
        let l:saved_reg = @"
        execute "normal! vgvy"

        let l:pattern = escape(@", '\\/.*$^~[]')
        let l:pattern = substitute(l:pattern, "\n$", "", "")

        if a:direction == 'b'
            execute "normal ?" . l:pattern . "^M"
        elseif a:direction == 'gv'
            call CmdLine("vimgrep " . '/'. l:pattern . '/' . ' **/*.')
        elseif a:direction == 'f'
            execute "normal /" . l:pattern . "^M"
        endif

        let @/ = l:pattern
        let @" = l:saved_reg
    endfunction

""" =======
""" Folding
""" =======
    if version >= 600
        set foldenable
        set foldmethod=marker
        set foldlevel=100 " do not autoload. still able to fold manually
        set foldmarker=\ {{{,\ }}}
    endif

    nnoremap <s-space> za
    vnoremap <s-space> zf

""" ===============
""" Minibufexpl.vim
""" ===============
    let g:miniBufExplMapWindowNavVim = 1

""" =======
""" IPython
""" =======
" Interaction with IPython's log
function! ReadOutputFromIpythonLog(filename)
python << EOF
import vim
filename = vim.eval("a:filename")
f = open(filename,"r")
data = f.read()
f.close()
row,col = vim.current.window.cursor
for line in data.split("\n"):
    if line.startswith("#[Out]#"):
        line = line[8:] + "\n"
        vim.current.buffer[row-1] = line
        row += 1
        vim.current.buffer.append("")
# now the flush the file
f = open(filename, "w")
f.write("")
f.close()
EOF
endfunction
nnoremap <leader>o :call ReadOutputFromIPythonLog("log")<cr>

""" ======
""" Pastie
""" ======
    " let pastie open up in a buffer, instead of a tab/window
    let g:pastie_destination = 'buffer'

""" ====
""" Ack
""" ====
    " set ack-grep as vim's default external grep program
    "set grepprg=ack-grep\ -i

    " command! -nargs=+ SilentGrep execute 'silent grep! <nargs>' | copen 33
    map ä :Ack 
    " text grep
    "map + :grep --text 

""" ==========
""" Scratchpad
""" ==========
    func! ToggleScratchpad()
        if !exists("g:ToggleScratchpad")
            let g:ToggleScratchpad = 1
            above pedit +"e ~/.scratchpad"
        else
            unlet g:ToggleScratchpad
            pclose
        endif
    endfunc
    map _s :call ToggleScratchpad()<cr>

""" ========
""" pyflakes
""" ========
""" disable quickfix support for pyflakes
let g:pyflakes_use_quickfix = 0

"""""""""""""""""""""""""""""""
""" Viki Bindings
"""""""""""""""""""""""""""""""
""" Custom key mappings

" add current file to the list of vikitasks' files
noremap <leader>va :VikiTasksAdd<CR>

" create a new todo list
noremap <leader>vt :VikiTasks!<CR>

" get a list of the fils
noremap <leader>vf :VikiTasksFiles<CR>

let g:vikiOpenFileWith_ANY    = "silent !gnome-open %{FILE}"
"let g:vikiOpenUrlWith_mailto = 'thunderbird -compose %{URL}'
"let g:vikiOpenUrlWith_ANY    = "silent !firefox %{URL}"
let g:vikiOpenUrlWith_ANY     = "silent !chromium-browser %{URL}"

"""
""" Easymotion
"""
"let g:EasyMotion_leader_key = '<Leader>m'

""" NOTES
" To install vimballs, load the .vba.gz file in vim and execute
"
"     :UseVimball ~/.vim/bundle/bundle-name-for-the-vim-plugin
"
" This will extract the contents into the directory for pathogen to load

" TODO: closetag.vim
" TODO: minibufexpl
" TODO: markdown
" TODO: eclim
"
" FUNNY: make typewriter sound in insert mode
function! PlaySound()
    silent! exec '!aplay ~/.vim/support/typewriter-key-1.wav 2&>1 /dev/null &'
endfunction
"autocmd CursorMovedI * call PlaySound()
filetype plugin on
