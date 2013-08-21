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
Bundle 'gmarik/vundle'

Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-repeat'
" this didn't work, just like taglist.vim
Bundle 'SuperTab-continued'
Bundle 'flazz/vim-colorschemes'
Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/nerdtree'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'msanders/snipmate.vim'
Bundle 'sontek/rope-vim'
Bundle 'mileszs/ack.vim'
" Fuzzyfinder requires 'L9' as dependency
Bundle 'L9'
Bundle 'FuzzyFinder'
Bundle 'vim-scripts/TaskList.vim'
Bundle 'Rip-Rip/clang_complete'
Bundle 'ervandew/supertab'
Bundle 'kien/ctrlp.vim'
Bundle 'lukerandall/haskellmode-vim'
Bundle 'hsitz/VimOrganizer'
Bundle 'calendar.vim'
Bundle 'NarrowRegion'
" for neco-ghc and ghcmod-vim
Bundle 'Shougo/vimshell'
" for neco-ghc and ghcmod-vim....need extra step: 'make -f make_mac.mak'
Bundle 'Shougo/vimproc'
set rtp+=~/.vim/bundle/vimproc
" requires 'cabal install ghc-mod'
Bundle 'ujihisa/neco-ghc'
set rtp+=~/.vim/bundle/neco-ghc
Bundle 'eagletmt/ghcmod-vim'
Bundle 'Shougo/neocomplcache'
set rtp+=~/.vim/bundle/neocomplcache
Bundle 'thinca/vim-quickrun'
" requires cd into bundle and 'cabal configure && cabal build && cabal install'
Bundle 'bitc/lushtags'
set rtp+=~/.vim/bundle/lushtags
Bundle 'majutsushi/tagbar'
Bundle 'scratch.vim'
Bundle 'Lokaltog/vim-powerline'
Bundle 'eagletmt/ghcmod-vim'
Bundle 'Shougo/vimproc'
Bundle 'vim-pandoc/vim-pandoc'

" for clojure
Bundle 'guns/vim-clojure-static'
Bundle 'tpope/vim-fireplace'
Bundle 'kien/rainbow_parentheses.vim'

" install 'exuberant-ctags' to activate this
"Bundle 'taglist.vim'

" To properly install "command-t" don't forget to compile the C-extension via
"
"   $ cd ~/.vim/bundle/command-t/ruby/command-t
"   $ ruby extconf.h
"   $ make
"
Bundle 'git://git.wincent.com/command-t.git'
Bundle 'Align'
Bundle 'matchit'

""" Bundle List END

syntax on
syntax enable
filetype on
filetype plugin on
filetype plugin indent on " enable file-level indenting with gg=G
set mouse=a
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
    "set listchars=tab:»·,trail:· " show trailing spaces as a circle. turn off with 'set nolist'
    "set list

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
    set guioptions-=r " remove the graphical scrollbars
    set guioptions-=L
    "set guifont=Bitstream\ Vera\ Sans\ Mono\ 10
    set guifont=Monaco:h12
    "set guifont=Inconsolata-dz:h12
""" }

""" =============
""" Color Schemes {
""" =============
    "colorscheme ir_black
    "colorscheme mustang
    colorscheme molokai
    "set background=light
    " another good one is
    "set background=dark
    "colorscheme zen
    "colorscheme desert
    ":color evening
    "colorscheme vilight
    "colorscheme codeburn
    colorscheme custom_molokai
    "colorscheme oceanlight
    "colorscheme oceanblack
    "colorscheme jellybeans
    "colorscheme darkspectrum
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
    map <F6> :FufBuffer<cr>

    " map Cmd-T
    map <A-t> :CommandT<cr>
    "map <F5> :CommandT<cr>
    "map <A-t> :FufFile<cr>

    " map NerdTree
    map ö :NERDTreeToggle<cr>
    map <Leader>nf :NERDTreeFind<cr>

    " map TagBar
    map ü :TagbarToggle<cr>

    " map QuickRun
    map + :QuickRun<cr>

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
    "map <right> :bnext<CR>
    map <S-l> :tabnext<cr>
    "map <left>  :bprevious<CR>
    map <S-h>   :tabprevious<CR>
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
    map <S-F10> :TODOs<cr>



""" }

""" =======================
""" Custom to do-list buffer
""" =======================
let TODOBufferName = "~/Dropbox/__VIM_TODO__.txt"
let TODOToggled = -1

function! TodoListToggle(new_win)
    if g:TODOToggled == 1
        call TodoListClose()
        let g:TODOToggled = -1
    else
        call TodoListOpen(a:new_win)
        let g:TODOToggled = 1
    endif
endfunction

function! TodoListClose()
    let bufnum = bufnr(g:TODOBufferName)
    if bufnum != -1
        exe "bd " . bufnum
    endif
endfunction

function! TodoListOpen(new_win)
    let split_win = a:new_win
    if !split_win && modified
        let split_win = 1
    endif

    let todo_bufnum = bufnr(g:TODOBufferName)
    if todo_bufnum == -1
        " open a new todo buffer
        if split_win
            exe "new " . g:TODOBufferName
        else
            exe "edit " . g:TODOBufferName
        endif
    else
        " todo buffer is already created. Check whether it is open
        " in one of the windows
        let todo_winnum = bufwinnr(todo_bufnum)
        if todo_winnum != -1
            " Jump to the window which has the scratch buffer if we
            " are not already in that window
            if winnr() != todo_winnum
                exe todo_winnum . "wincmd w"
            endif
        else
            " create a new todo buffer
            if split_win
                exe "split +buffer" . todo_bufnum
            else
                exe "buffer " . todo_bufnum
            endif
        endif
    endif
endfunction

command! -nargs=0 TODO call TodoListToggle(0)
command! -nargs=0 TODOs call TodoListToggle(1)

""" =============
""" Abbreviations
""" =============
ab ipdb import ipdb;ipdb.set_trace()

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

function! MarkWindowSwap()
    let g:markedWinNum = winnr()
endfunction

function! DoWindowSwap()
    "Mark destination
    let curNum = winnr()
    let curBuf = bufnr( "%" )
    exe g:markedWinNum . "wincmd w"
    "Switch to source and shuffle dest->source
    let markedBuf = bufnr( "%" )
    "Hide and open so that we aren't prompted and keep history
    exe 'hide buf' curBuf
    "Switch to dest and shuffle source->dest
    exe curNum . "wincmd w"
    "Hide and open so that we aren't prompted and keep history
    exe 'hide buf' markedBuf 
endfunction

nmap <silent> <leader>mw :call MarkWindowSwap()<CR>
nmap <silent> <leader>pw :call DoWindowSwap()<CR>

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
    autocmd BufRead *.hamlet set filetype=html
    autocmd BufRead *.less set filetype=css
    autocmd BufRead *.lucius set filetype=css
    autocmd BufRead *.wsdl set filetype=xml
    autocmd BufRead *.hx set filetype=haxe
    autocmd BufRead *.julius set filetype=javascript
    autocmd BufRead *.md set filetype=markdown
    autocmd BufRead,BufNewFile *.json set filetype=json
    "autocmd BufRead,BufNewFile *.py compiler ose
    autocmd BufRead,BufNewFile *.mlua set ft=lua
    autocmd BufRead,BufNewFile *.viki set ft=viki
    au! BufRead,BufWrite,BufWritePost,BufNewFile *.org 
    au BufEnter *.org call org#SetOrgFileType()

    " setup haskell
    "autocmd FileType hs setlocal omnifunc=necoghc#omnifunc
    autocmd BufRead *.hs map tt :GhcModType<cr>
    autocmd BufRead *.hs map tl :GhcModCheckAndLintAsync<cr>
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

    " remember folding and cursor position when closing/opening buffers
    autocmd BufWinLeave *.* mkview
    autocmd BufWinEnter *.* silent loadview 

    nnoremap <space> za
    vnoremap <space> zf

""" ===============
""" Minibufexpl.vim
""" ===============
    let g:miniBufExplMapWindowNavVim = 1

""" ==========
""" snipmate
""" ==========
    let g:snips_author = "Conrad Indiono"

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
    let g:ackprg="ack -H --nocolor --nogroup --column"
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


"""
""" RopeVim
"""
let g:ropevim_enable_shortcuts = 1

"""
""" pep8
"""
"let g:pep8_map='<leader>8'

""" NOTES
" To install vimballs, load the .vba.gz file in vim and execute
"
"     :UseVimball ~/.vim/bundle/bundle-name-for-the-vim-plugin
"
" This will extract the contents into the directory for pathogen to load

"""
""" CtrlP
"""
let g:ctrlp_working_path_mode = 0
" 0 - Don't manage working directory
" 1 - Current buffer's parent directory
" 2 - First parent with .git,.svn etc.

"""
""" haskell-mode
"""
" Configure browser for haskell_doc.vim
let g:haddock_browser = "open"
let g:haddock_browser_callformat = "%s %s"

function! SetToCabalBuild()
    if glob("*.cabal") != ''
        let a = system( 'grep "/\* package .* \*/"  dist/build/autogen/cabal_macros.h' )
        let b = system( 'sed -e "s/\/\* /-/" -e "s/\*\///"', a )
        let pkgs = "-hide-all-packages " .  system( 'xargs echo -n', b )
        let hs = "import Distribution.Dev.Interactive\n"
        let hs .= "import Data.List\n"
        let hs .= 'main = withOpts [""] error return >>= putStr . intercalate " "'
        let opts = system( 'runhaskell', hs )
        let b:ghc_staticoptions = opts . ' ' . pkgs
    else
        let b:ghc_staticoptions = '-Wall -fno-warn-name-shadowing'
    endif
    execute 'setlocal makeprg=' . g:ghc . '\ ' . escape(b:ghc_staticoptions,' ') .'\ -e\ :q\ %'
    let b:my_changedtick -=1
endfunction

"autocmd BufEnter *.hs,*.lhs :call SetToCabalBuild()
"autocmd BufEnter *.hs compiler ghc

""" VimOrganizer
let g:calendar_navi = ''
let g:org_todo_setup = "TODO | DONE"

"""
""" Swap Buffers inside window splits
"""
function! MarkWindowSwap()
    let g:markedWinNum = winnr()
endfunction

function! DoWindowSwap()
    "Mark destination
    let curNum = winnr()
    let curBuf = bufnr( "%" )
    exe g:markedWinNum . "wincmd w"
    "Switch to source and shuffle dest->source
    let markedBuf = bufnr( "%" )
    "Hide and open so that we aren't prompted and keep history
    exe 'hide buf' curBuf
    "Switch to dest and shuffle source->dest
    exe curNum . "wincmd w"
    "Hide and open so that we aren't prompted and keep history
    exe 'hide buf' markedBuf 
endfunction

nmap <silent> <leader>mw :call MarkWindowSwap()<CR>
nmap <silent> <leader>pw :call DoWindowSwap()<CR>

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

hi CursorLine term=reverse cterm=none ctermbg=4

set encoding=utf8

""" neocomplcache
"let g:neocomplcache_enable_at_startup = 1

""" tagbar
let g:tagbar_ctags_bin = "/usr/bin/ctags"

""" custom WIP helpers

function! TodoFVCollect()
    execute "normal ma"
    execute ":'a,$g/@/m'a"
endfunction
map <Leader>l :call TodoFVCollect()<CR>

"
" for powerline
" explicitly tell vim that the terminal has 256 colors
set t_Co=256
let g:Powerline_symbols = 'fancy'

let g:haddock_browser = "/usr/bin/chromium-browser"

imap jj <Esc>
