" Todo:"{{{
" fix smartinput rules
" add unconditional paste?
" fix airline in term
" set up yanking properly in unite
" arpeggio
"}}}
" An important note:
" since nnoremap doesn't work when binding to <Plug> mappings, there is often <leader>; instead of :
" fixes complaining about undefined tcomment variable
set runtimepath+=~/.vim/bundle/tcomment_vim
set runtimepath+=~/.vim/colors
" turns off vi compatibility mode for full vim functionality; set first
set nocompatible

"" Don't save backups of *.gpg files"{{{
set backupskip+=*.gpg

augroup encrypted
  au!
  " Disable swap files, and set binary file format before reading the file
  " To avoid that parts of the file is saved to .viminfo when yanking or
  " deleting, empty the 'viminfo' option.
  autocmd BufReadPre,FileReadPre *.gpg
    \ setlocal noswapfile noundofile noshelltemp history=0 viminfo= bin
  " Decrypt the contents after reading the file, reset binary file format
  " and run any BufReadPost autocmds matching the file name without the .gpg
  " extension
  autocmd BufReadPost,FileReadPost *.gpg
    \ execute "'[,']!gpg --decrypt --default-recipient-self" |
    \ setlocal nobin |
    \ execute "doautocmd BufReadPost " . expand("%:r")
  " Set binary file format and encrypt the contents before writing the file
  autocmd BufWritePre,FileWritePre *.gpg
    \ setlocal bin |
    \ '[,']!gpg --encrypt --default-recipient-self
  " After writing the file, do an :undo to revert the encryption in the
  " buffer, and reset binary file format
  autocmd BufWritePost,FileWritePost *.gpg
    \ silent u |
    \ setlocal nobin
augroup END

"}}}

" vim as password manager"{{{
" using truecrypt container with blowfish encrypted text file... makes sense? probably not
" use the vpass alias with .encrypted_vimrc not this (no loading of plugins); I guess this is useful if accidentally open the buffer

" no backup or writebackup for vault files
set backupskip+=*.vault

augroup vaultencrypted
  au!
  " Disable swap files, saving to disk of undo history, writing to disk of commands
  " To avoid that parts of the file is saved to .viminfo when yanking or deleting, empty the 'viminfo' option.
    " \ setlocal noswapfile noundofile noshelltemp history=0 viminfo=
  autocmd BufReadPre,FileReadPre,BufEnter *.vault
    \ setlocal noswapfile cm=blowfish noundofile noshelltemp viminfo= 
augroup END

autocmd BufEnter *.vault nmap <buffer> yy yi{

"}}}

" Experimental"{{{
" for pterosaur; was this fixed?
inoremap qq <esc>

" retrain to stop using caps layer in vim
nnoremap <up> <nop>
nnoremap <left> <nop>
" nnoremap <right> <nop>
nnoremap <down> <nop>
nnoremap <End> <nop>
nnoremap <Home> <nop>
inoremap <up> <nop>
inoremap <left> <nop>
inoremap <right> <nop>
inoremap <down> <nop>
inoremap <End> <nop>
inoremap <Home> <nop>

nmap s <nop>
nmap r <nop>
if has("gui_running")
" wm experementation"{{{
" nnoremap <silent> <leader>a :silent !bspc window -f left && xsendkey p && bspc window -f last<cr>

" "r" is redraw"{{{
" worskpace/Destkop switch"{{{
nnoremap <silent> ra :silent !bspc desktop -f ^1<cr>
nnoremap <silent> rr :silent !bspc desktop -f ^2<cr>
nnoremap <silent> rs :silent !bspc desktop -f ^3<cr>
nnoremap <silent> rt :silent !bspc desktop -f ^4<cr>
nnoremap <silent> rd :silent !bspc desktop -f ^5<cr>
nnoremap <silent> rh :silent !bspc desktop -f ^6<cr>
nnoremap <silent> rn :silent !bspc desktop -f ^7<cr>
nnoremap <silent> re :silent !bspc desktop -f ^8<cr>
nnoremap <silent> ri :silent !bspc desktop -f ^9<cr>
nnoremap <silent> ro :silent !bspc desktop -f ^10<cr>
"}}}
" move to destkop"{{{
nnoremap <silent> Ra :silent !bspc window -d ^1<cr>
nnoremap <silent> Rr :silent !bspc window -d ^2<cr>
nnoremap <silent> Rs :silent !bspc window -d ^3<cr>
nnoremap <silent> Rt :silent !bspc window -d ^4<cr>
nnoremap <silent> Rd :silent !bspc window -d ^5<cr>
nnoremap <silent> Rh :silent !bspc window -d ^6<cr>
nnoremap <silent> Rn :silent !bspc window -d ^7<cr>
nnoremap <silent> Re :silent !bspc window -d ^8<cr>
nnoremap <silent> Ri :silent !bspc window -d ^9<cr>
nnoremap <silent> Ro :silent !bspc window -d ^10<cr>
"}}}

" moving windows within desktop"{{{
" move to biggest
nnoremap <silent> rcm :silent !bspc window -s biggest<cr>
" directions
nnoremap <silent> rch :silent !bspc window -s left<cr>
nnoremap <silent> rcn :silent !bspc window -s down<cr>
nnoremap <silent> rce :silent !bspc window -s up<cr>
nnoremap <silent> rci :silent !bspc window -s right<cr>
" circulate
nnoremap <silent> r. :silent !bspc desktop -C forward<cr>
nnoremap <silent> r, :silent !bspc desktop -C backward<cr>
"}}}

"resize"{{{
nnoremap <silent> rmh :silent !~/bin/resize.sh left<cr>
nnoremap <silent> rmn :silent !~/bin/resize.sh down<cr>
nnoremap <silent> rme :silent !~/bin/resize.sh up<cr>
nnoremap <silent> rmi :silent !~/bin/resize.sh right<cr>
"}}}
" open urxvt
nnoremap <silent> ru :silent !urxvt &<cr>

" close window
nnoremap <silent> rx :silent !bspc window -c<cr>
"}}}

" s becomes select/Show/settings"{{{
"select
nnoremap <silent> sh :silent !bspc window -f left<cr>
nnoremap <silent> sn :silent !bspc window -f down<cr>
nnoremap <silent> se :silent !bspc window -f up<cr>
nnoremap <silent> si :silent !bspc window -f right<cr>
nnoremap <silent> sl :silent !bspc window -f last<cr>

" monocle toggle
nnoremap <silent> st :silent !bspc desktop -l next<cr>
nnoremap <silent> ss :silent !bspc window -t sticky<cr>
nnoremap <silent> sf :silent !bspc window -t fullscreen<cr>

" gap up and down
nnoremap <silent> su :silent !bspc config -d focused window_gap $((`bspc config -d focused window_gap` - 4 ))<cr>
nnoremap <silent> sU :silent !bspc config -d focused window_gap $((`bspc config -d focused window_gap` + 4 ))<cr>

" preselect"{{{
nnoremap <silent> sph :silent !bspc window -p left<cr>
nnoremap <silent> spn :silent !bspc window -p down<cr>
nnoremap <silent> spe :silent !bspc window -p up<cr>
nnoremap <silent> spi :silent !bspc window -p right<cr>
nnoremap <silent> spx :silent !bspc window -p cancel<cr>
nnoremap <silent> spd :silent !bspc desktop -c<cr>
"}}}
"}}}

"}}}
else
" tmux experimentation"{{{
" "r" is redraw"{{{
" window switching"{{{
nnoremap <silent> ra :silent !tmux select-window -t 1<cr>
nnoremap <silent> rr :silent !tmux select-window -t 2<cr>
nnoremap <silent> rs :silent !tmux select-window -t 3<cr>
nnoremap <silent> rt :silent !tmux select-window -t 4<cr>
nnoremap <silent> rd :silent !tmux select-window -t 5<cr>
nnoremap <silent> rh :silent !tmux select-window -t 6<cr>
nnoremap <silent> rn :silent !tmux select-window -t 7<cr>
nnoremap <silent> re :silent !tmux select-window -t 8<cr>
nnoremap <silent> ri :silent !tmux select-window -t 9<cr>
nnoremap <silent> ro :silent !tmux select-window -t 10<cr>
"}}}
" resize panes"{{{
nnoremap <silent> rmh :silent !tmux resize-pane -L 10<cr>
nnoremap <silent> rmn :silent !tmux resize-pane -D 10<cr>
nnoremap <silent> rme :silent !tmux resize-pane -U 10<cr>
nnoremap <silent> rmi :silent !tmux resize-pane -R 10<cr>
"}}}
" circulate
" previous
nnoremap <silent> r, :silent !tmux swap-pane -U<cr>
" next
nnoremap <silent> r. :silent !tmux swap-pane -D<cr>

" new session
nnoremap <silent> r_ :silent !tmux new-session<cr>

" new window
nnoremap <silent> rc :silent !tmux new-window<cr>
" kill pane
nnoremap <silent> rx :silent !tmux kill-pane<cr>
" last window
nnoremap <silent> rl :silent !tmux last-window<cr>
" split windows
nnoremap <silent> r/ :silent !tmux split-window -h<cr>
nnoremap <silent> r- :silent !tmux split-window<cr>

" break pane
nnoremap <silent> r! :silent !tmux break-pane<cr>
"}}}

" "s" is select"{{{
" panes"{{{
" directions
nnoremap <silent> sh :silent !tmux select-pane -L<cr>
nnoremap <silent> sn :silent !tmux select-pane -D<cr>
nnoremap <silent> se :silent !tmux select-pane -U<cr>
nnoremap <silent> si :silent !tmux select-pane -R<cr>
" last
nnoremap <silent> sl :silent !tmux select-pane -l<cr>
" select layout
nnoremap <silent> sv :silent !tmux select-layout main-vertical<cr>

" toggle "monocle" (zoom)
nnoremap <silent> st :silent !tmux resize-pane -Z<cr>

" bspwm
" bspwm monocle (for dropdown terms)
nnoremap <silent> sm :silent !bspc desktop -l monocle && bspc window -t floating<cr>
nnoremap <silent> sf :silent !bspc window -t fullscreen<cr>

"}}}

" select session
nnoremap <silent> ss :silent !tmux choose-client<cr>
"}}}
"}}}
endif

"}}}

" plugin unmaps"{{{
" table mode
autocmd VimEnter * silent! nunmap <leader>tt
" replace camelcase's ib map
autocmd VimEnter * omap ib <Plug>(textobj-anyblock-i)|xmap ib <Plug>(textobj-anyblock-i)
"}}}
" #==============================
" # General {{{
" #==============================
" General General "{{{
" error bells are off by default
" default ; i.e. will show # of lines selected in visual
set showcmd
" (show matching brackets; default)
set showmatch

set foldmethod=marker

set modeline

" enable utf8
set encoding=utf8
set termencoding=utf-8

" filetype based indentation..
filetype plugin indent on

" automatically use indentation from previous line
set autoindent

" Keep 2000 lines of command line history.
set history=2000

" keeps buffer contents in memory (undo history doesn't go away if change buffers)
set hidden
" persistent undo history (even if close buffer)
set undofile " Save undo's after file closes
set undodir=~/.vim/undo,/tmp " where to save undo histories
set undolevels=3000 " How many undos
set undoreload=3000 " number of lines to save for undo

" http://vim.wikia.com/wiki/Word_wrap_without_line_breaks
" wrap lines visually when reach the edge
set wrap

" disable automatic insertion of newline characters
set textwidth=0
set wrapmargin=0

" display tabs and certain whitespace
set list
" listchars ;show tabs; show at end of line; ☬⚛⚜⚡☥☣
set listchars=tab:\!\ ,nbsp:☣,eol:¬

"relative numbers except current line (rnu); using numbers vim plugin as well
set number
set relativenumber

" text formatting; get rid of tc; no autowrap of comments or based on textwidth
set formatoptions=rw
" set formatoptions-=tc
" r - insert comment after enter while in insert
" leaving off o which adds comment char on o

" won't redraw while executing macros, registers, and commands that have not been typed (not default)
" for example, stops flickering when have up and down mapped to c-o gk and gj in insert
set lazyredraw

" (when do things like gg, will move cursor to sol)
set startofline

" at least 5 lines show below and above cursor
set scrolloff=5

" open folds on jumping to marks
set foldopen+=mark

" turn off timeout; default is 1000
set notimeout
" this messes up airline in term vim ^
" set timeoutlen=50

" return to last edit position when opening files
autocmd BufReadPost *
            \ if line("'\"") > 0 && line("'\"") <= line("$") |
            \ exe "normal! g`\"" |
            \ endif

" http://sheerun.net/2014/03/21/how-to-boost-your-vim-productivity/
" vp doesn't replace paste buffer
function! RestoreRegister()
  let @" = s:restore_reg
  return ''
endfunction
function! s:Repl()
  let s:restore_reg = @"
  return "p@=RestoreRegister()\<cr>"
endfunction
vmap <silent> <expr> p <sid>Repl()

"}}}

" Session, saving, swap, backup settings"{{{
" to prevent annoyance for now:
set noswapfile
" save swap files here; // to avoid collisions (files with same name will be named based on path)
" set directory=~/.vim/swap//

" auto save on focus lost if buffer changed (error if unnamed.. don't use untitled buffers)
au FocusLost * update

" autowriteall; save buffer if changed when use various commands (switching buffers, quit, exit, etc.)
set awa

"}}}

" Sourcing vimrc"{{{
" from vim wiki
augroup AutoReloadVimRC
  au!
  " automatically reload vimrc when it's saved
  au BufWritePost ~/dotfiles/vim/.vimrc so ~/.vimrc
augroup END

"}}}

" Command mode"{{{
" when tab completing from command line will show
set wildmenu

" changes tab behaviour in command line (i.e. list makes every possible choice popup:)
set wildmode:full
"}}}

" Searching"{{{
set hlsearch

" this causes arrow keys to make As and Bs in insert/normal mode in terminal vim
if has("gui_running")
	nnoremap <esc> :noh<cr><esc>
	inoremap <esc> <esc>:noh<cr>
endif
" don't move when search
set noincsearch
" go back to beginning of buffer once reach end
set wrapscan

" when searching lower case, will match any case
set ignorecase
" won't ignore case if search with upper case
set smartcase
"}}}

" Spacing/Indentation Stuff "{{{
" super tab for conversion
" Only hard tab for indent:
set noexpandtab "default; don't convert to spaces
set shiftwidth=4
set tabstop=4
set softtabstop=0
" I like tabs
set smarttab

" smart tabs plugin; use tabs only for indent; spaces for allignment
" will insert spaces when use tab not at beginning of line

"}}}

"}}}
" #==============================
" # Specific Filetype Settings {{{
" #==============================
" can also add file specific settings to ~/.vim/after/ftplugin; html.vim or python.vim for example; use setlocal instead
" http://stackoverflow.com/questions/1889602/multiple-vim-configurations

" Text file settings"{{{

" comment my textfiles with octothorpe
call tcomment#DefineType('text', '# %s' )

augroup text_autocommands
	au!
	" remove listchars from txt files in favour of better wrapping (not cutting off halfway in between a word) for long lines
	autocmd BufEnter *.txt setlocal nolist
	autocmd BufEnter *.txt setlocal lbr
	" set showbreak=···\ " Line break indicator.

	" comment graying in text files
	autocmd BufEnter *.txt highlight text guifg=gray
	autocmd BufEnter *.txt match text /#.*/

	" fix zf (folding now uses right comment char)
	autocmd BufEnter *.txt setlocal commentstring=#\ %s

	" Enable spell check for text files
	autocmd BufNewFile,BufRead *.txt setlocal spell spelllang=en
augroup END

"}}}

" Tex file settings"{{{
autocmd BufEnter *.tex setlocal nolist
autocmd BufEnter *.tex setlocal lbr
autocmd BufEnter *.tex setlocal textwidth=0

"}}}

" markdown
autocmd FileType mkd setlocal spell spelllang=en

" python
autocmd FileType python setlocal tabstop=4 softtabstop=4 shiftwidth=4 textwidth=80 smarttab expandtab
" indent folding for python
"au BufEnter *.py set foldmethod=indent

" pentadactyl
call tcomment#DefineType('pentadactyl', '" %s' )
autocmd BufNewFile,BufRead *.pentadactylrc,*penta set filetype=pentadactyl

" comment char for .conf files
let g:commentChar = {
\ 'conf': '#'
\}
"}}}
" #==============================
" # Appearance"{{{
" #==============================
" syntax highlighting works when put at end but not here..
" syntax on

colorscheme seoul256
set t_Co=256
if has("gui_running")
	set guicursor=a:blinkon900-blinkoff600 " Slow down cursor blinking speed
	" colorscheme molokai
	colorscheme gruvbox
	set background=dark
	set guifont=Inconsolata\ 11
endif

" Airline theme "{{{
" airline always present
set laststatus=2

" http://stackoverflow.com/questions/114431/fast-word-count-function-in-vim
function! WordCount()
  let s:old_status = v:statusmsg
  let position = getpos(".")
  exe ":silent normal g\<c-g>"
  let stat = v:statusmsg
  let s:word_count = 0
  if stat != '--No lines in buffer--'
    let s:word_count = str2nr(split(v:statusmsg)[11])
    let v:statusmsg = s:old_status
  end
  call setpos('.', position)
  return s:word_count 
endfunction

autocmd VimEnter * call s:airline_sections_custom()
function! s:airline_sections_custom()
	" add current session name to statusline
	"http://stackoverflow.com/questions/11374047/adding-the-current-session-filename-in-the-statusline add current session name to statusline
	let g:airline_section_x = airline#section#create(['filetype', ' %{fnamemodify(v:this_session, ":t")}'])
	let g:airline_section_z = airline#section#create(['%02p%% : %l: %c', ' %{WordCount()}'])
endfunction

" my custom combination theme:
let g:airline_theme='darkfox'
" tabline won't work with taboo
" let g:airline#extensions#tabline#enabled = 1
" let g:airline#extensions#tabline#left_sep = ' '
" let g:airline#extensions#tabline#left_alt_sep = '|'

" old vim-powerline symbols; needs fixing
let g:airline_left_sep = '⮀'
" let g:airline_left_alt_sep = '⮁'
let g:airline_right_sep = '⮂'
" let g:airline_right_alt_sep = '⮃'
" below resulting in error on startup; font problem?
" let g:airline_symbols.branch = '⭠'
" let g:airline_symbols.readonly = '⭤'

" turn off mixed indent, trailing space
let g:airline_section_warning = ''

" vcs integration
let g:airline#extensions#branch#enabled = 1
" syntastic
let g:airline#extensions#syntastic#enabled = 1
"}}}

" terminal like tabs (maybe not as ugly: use -=e):
" A autoyank contents of visual mode to + register
" c use console dialogues instead of popups for simple choices
" also remove menu bar (m), T (toolbar)
if has("gui_running")
	set guioptions=P,c,e
" set guioptions-=m,T,e
" kind of fixes white bar appearing at bottom; seems to work
	set guiheadroom=40
endif

" Tab stuff
" limit amount of tabs for vim:
" set tabpagemax=15

"}}}
" #==============================
" # General Mappings/ Bindings and Settings "{{{
" #==============================
" make colemak t more useful
let mapleader = "t"

" Colemak/Navigation Mappings"{{{
" modified from here:
" https://github.com/bunnyfly/dotfiles/blob/master/vimrc
" http://forum.colemak.com/viewtopic.php?id=1808
" My change: keep i and don't have a dedicated right
" l for 'last' instead of line

noremap n gj|noremap e gk|nnoremap gn j|nnoremap ge k

" don't just place in the middle; open folds recursively 
nnoremap <silent> k nzOzz|nnoremap <silent> K NzOzz
" xnoremap <silent> K :<c-u>call <SID>next('N', 1)<BAR>if &hlsearch<BAR>set hlsearch<BAR>endif<cr>

" BOL/EOL/Join Lines; took out l to ^ in favor of l for <c-o>
" nnoremap L $|nnoremap <C-l> J
nnoremap L <c-i>
" l for last
nnoremap l <c-o>
" h for beginning of line
" nnoremap h 0

" High/Low/Mid.
" never use gm or select mode; would rather have go to middle of buffer not line
noremap gh H|noremap gm M|noremap gl L

nnoremap j e|noremap J E

" keep in visual
vnoremap n j
vnoremap e k
vnoremap i l

" fold navigation for colemak
nnoremap ze zk
nnoremap zn zj
""}}}

" Other General Mappings"{{{
" change defaults"{{{
nnoremap ; q:i
nnoremap : ;
nnoremap <leader>; :
" use escape in "normal" to exit:
autocmd CmdwinEnter * nnoremap <buffer> <esc> <c-w>c
" enter in "normal" to execute command under cursor and re-enter q:
autocmd CmdwinEnter * nnoremap <buffer> <cr> <cr>q:

vnoremap ; q:i
vnoremap t; :
vnoremap : ;
" Y like D
nnoremap Y y$
" yank paragraph
" http://hashrocket.com/blog/posts/8-great-vim-mappings
nnoremap yp yap<S-}>p

" because I hate Q; apply macro
nnoremap Q @q
vnoremap Q :norm @q<cr>
" ma=ip`a

" Sane redo.
noremap U <C-r>

" I use a more than A
nnoremap a A
nnoremap A a

" I use V more than v.. going to try this; can also double tap v now for visual
nnoremap v V|nnoremap V v|nnoremap <leader>v <c-v>

" I don't ever use ga; map to go to end of line (works with wrapping)
" nnoremap ga g$
" swap
nnoremap g0 g^
nnoremap g^ g0

" camelcase motion as default
map <silent> w <Plug>CamelCaseMotion_w
map <silent> b <Plug>CamelCaseMotion_b
map <silent> j <Plug>CamelCaseMotion_e
sunmap w
sunmap b
sunmap j

" default iw
omap <silent> iw <Plug>CamelCaseMotion_iw
xmap <silent> iw <Plug>CamelCaseMotion_iw
" using ib with anyblock
" omap <silent> ib <Plug>CamelCaseMotion_ib
" xmap <silent> ib <Plug>CamelCaseMotion_ib
omap <silent> ie <Plug>CamelCaseMotion_ie
xmap <silent> ie <Plug>CamelCaseMotion_ie

"}}}
" better text file long line nav (use with lazy redraw); up and down between wraps
inoremap <Down> <C-o>gj
inoremap <Up> <C-o>gk

" jump up and down
nnoremap <Plug>MuchUp <c-u>
nnoremap <Plug>MuchDown <c-d>
nmap <leader>k <Plug>MuchDown<leader>;silent! call repeat#set("\<Plug>MuchDown", v:count)<cr>
nmap <leader>o <Plug>MuchUp<leader>;silent! call repeat#set("\<Plug>MuchUp", v:count)<cr>
vnoremap <leader>k <c-d>|vnoremap <leader>o <c-u>

" save; save on insert leaver?
nnoremap <leader>s :w<cr>
nnoremap <leader>q :q<cr>

" control backspace behaviour
inoremap ¸ <c-w>
cnoremap ¸ <c-w>
nnoremap ¸ daw
inoremap .¸ <c-u>

" paste in insert
inoremap .yp <c-r>+

" keep cursor at beginning of line after hitting enter; fix
inoremap <return> <return><home>

" source vimrc
nnoremap <leader>. :so ~/.vimrc<cr>
" source current buffer
nnoremap g. :so %<cr>

"'x-ray' view; cuc and cul
nnoremap <leader>x :set cursorcolumn!<cr>:set cursorline!<cr>
"}}}

" _Spell correct"{{{
nnoremap <leader>z 1z=
" repeatable correct last or next mispelled word
nnoremap <Plug>CorrectNextMispell ]s1z=
nnoremap <Plug>CorrectPreviousMispell [s1z=
" add word to spellfile
nnoremap <Plug>AddNextMispell ]szg
nnoremap <Plug>AddPreviousMispell [szg
" goto sleep is useless
" goto next mispelled word and correct with first option
nmap gsc <Plug>CorrectNextMispell<leader>;silent! call repeat#set("\<Plug>CorrectNextMispell", v:count)<cr>
nmap gSc <Plug>CorrectPreviousMispell<leader>;silent! call repeat#set("\<Plug>CorrectPreviousMispell", v:count)<cr>
" go to the next mispelled word and add to spellfile
nmap gsa <Plug>AddNextMispell<leader>;silent! call repeat#set("\<Plug>AddNextMispell", v:count)<cr>
nmap gSa <Plug>AddPreviousMispell<leader>;silent! call repeat#set("\<Plug>AddPreviousMispell", v:count)<cr>
" mark word a wrong
" nnoremap gsm zw
"}}}

" Plugin Specific"{{{
" NeoBundle stuff"{{{
nnoremap <leader>bi :Unite neobundle/install<cr>
nnoremap <leader>bc :NeoBundleClean<cr>
" neobundle update
nnoremap <leader>bu :Unite neobundle/update<cr>
nnoremap <leader>bs :Unite neobundle/search<cr>
nnoremap <leader>bl :NeoBundleList<cr>
"}}}

" Git Related"{{{
" Follow symlinks when opening a file"{{{
" Sources:
"  - https://github.com/tpope/vim-fugitive/issues/147#issuecomment-7572351
"  - http://www.reddit.com/r/vim/comments/yhsn6/is_it_possible_to_work_around_the_symlink_bug/c5w91qw
" Echoing a warning does not appear to work:
"   echohl WarningMsg | echo "Resolving symlink." | echohl None |
function! MyFollowSymlink(...)
  let fname = a:0 ? a:1 : expand('%')
  if getftype(fname) != 'link'
    return
  endif
  let resolvedfile = fnameescape(resolve(fname))
  exec 'file ' . resolvedfile
endfunction
command! FollowSymlink call MyFollowSymlink()

autocmd BufReadPost * call MyFollowSymlink(expand('<afile>'))
"}}}

" Fugitive"{{{
nnoremap <leader>ga :Gwrite<cr>
nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gc :Gcommit<cr>
nnoremap <leader>gd :Gdiff<cr>
" get previous commit
" nnoremap <leader>gr :Gread<cr>
nnoremap <leader>gm :Gmove<space>
" nnoremap <leader>gR :Gremove<cr>
" git rm --cached

" https://github.com/PonderingGrower/dotfiles/blob/master/.vimrc
augroup fugitive_settings
    autocmd!
	" same bindings for merging diffs as in normal mode
    autocmd BufRead fugitive://* xnoremap <buffer> dp :diffput<cr>
    autocmd BufRead fugitive://* xnoremap <buffer> do :diffget<cr>
	" easy diff update
    autocmd BufRead fugitive://* xnoremap <buffer> du :diffupdate<cr>
	" bindings for git status window
	autocmd FileType gitcommit nmap <buffer> n <c-n>|nmap <buffer> e <c-p>
augroup END
"}}}

" gitgutter"{{{
" don't set up default mappings
let g:gitgutter_map_keys = 0
nnoremap <leader>gg :GitGutterToggle<cr>
nnoremap <leader>gG :GitGutterLineHighlightsToggle<cr>
" hunks don't work well for me; use :Gdiff instead
" nmap <Leader>gh <Plug>GitGutterStageHunk
" nmap <Leader>gH <Plug>GitGutterRevertHunk
" repeatable hunk navigation
nmap <leader>gn <Plug>GitGutterNextHunkzO<leader>;silent! call repeat#set("\<Plug>GitGutterNextHunkzO", v:count)<cr>
nmap <leader>ge <Plug>GitGutterPrevHunkzO<leader>;silent! call repeat#set("\<Plug>GitGutterPrevHunkzO", v:count)<cr>
"}}}

" vim-signify settings "{{{
" " which vcs and order
" let g:signify_vcs_list = [ 'git' ]
" let g:signify_disable_by_default = 0
" "
" nmap <leader>gn <plug>(signify-next-hunk)
" nmap <leader>ge <plug>(signify-prev-hunk)
" let g:signify_mapping_toggle_highlight = '<leader>gh'
"}}}
"}}}

" Gundo
nnoremap <leader>u :GundoToggle<CR>
let g:gundo_map_move_older="n"
let g:gundo_map_move_newer="e"

" tcomment
map <leader>c <C-_><C-_>

" Tabular
" vnoremap <leader>a :Tabularize<space>
" :Tabularize /=
" :Tabularize /=\zs

" Easy Allign
" Start interactive EasyAlign in visual mode (e.g. vip<Enter>)
vmap <leader>a <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. <Leader>aip)
nmap <Leader>a <Plug>(EasyAlign)

" table mode
let g:table_mode_map_prefix = '<leader>m'
let g:table_mode_toggle_map = 'm'

" Emmet vim
map <leader>y <C-y>,

" from bling/dotvim
" syntastic "{{{
let g:syntastic_error_symbol = '✗'
let g:syntastic_style_error_symbol = '✠'
let g:syntastic_warning_symbol = '∆'
let g:syntastic_style_warning_symbol = '≈'
"}}}

" vim-session:"{{{
" quickly open session
" nnoremap <leader>ss :OpenSession
let g:session_autosave_periodic='yes'
let g:session_autosave='yes'
" automatically load  ~/.vim/sessions/default.vim
let g:session_autoload='yes'
"}}}
" sessions"{{{
" function! s:session_load_lock()
" 	if filereadable(expand("~/.vim/sessions/main.vim.lock"))
" 		echo "main.vim is locked"
" 	else
" 		silent! source ~/.vim/sessions/main.vim
" 		!touch ~/.vim/sessions/main.vim.lock
" 		au VimLeavePre * !rm ~/.vim/sessions/main.vim.lock
" 	endif
" endfunction
"}}}

" Sneak settings"{{{
" 1 means use ignorecase or smartcase if set (have smartcase set)
let g:sneak#use_ic_scs = 1
" use streak mode (easy motion highlighting) when > 1 match on screen
let g:sneak#streak = 1
" s to go to next S for previous
let g:sneak#s_next = 1
let g:sneak#textobject_z = 0

" hi link SneakPluginTarget ErrorMsg
hi link SneakStreakTarget ErrorMsg
" hi link SneakStreakMask Comment
" nmap f <Plug>Sneak_s
" visual mode
xmap f <Plug>Sneak_s
xmap F <Plug>Sneak_S
" operator-pending-mode
omap f <Plug>Sneak_s
omap F <Plug>Sneak_S
" nice since have "l" mapped to <c-o>

" colemak chars for sneak mode; take out i for insert; d out to delete
let g:sneak#target_labels = "arsthneowfpluy/ARSTDHNEIOFPLUY"

nmap f <Plug>SneakForward
nmap F <Plug>SneakBackward
"}}}

" VimWiki (using like TabsOutliner with pentadactyl)"{{{
let g:vimwiki_list = [{
    \    'path':                       '~/vimwiki/',
    \    'path_html':                  '~/vimwikihtml/',
    \    'maxhi':                      1,
    \    'css_name':                   'style.css',
    \    'auto_export':                0,
    \    'nested_syntaxes':            {},
    \    'html_header':                '',
    \    'html_footer':                '',
    \    'syntax':                     'default',
    \    'index':                      'index',
    \    'ext':                        '.wiki',
    \    'temp':                       0
    \    }]
let g:vimwiki_camel_case = 0                   " Don't automatically make CamelCase words links.
"}}}

" Calendar Settings
let g:calendar_google_calendar = 1
let g:calendar_google_task = 1
" nnoremap <cr>c :Calendar -position=tab<cr>

autocmd FileType calendar call s:calendar_settings()
function! s:calendar_settings()
" colemak nav
nmap <buffer> h <Plug>(calendar_left)
nmap <buffer> n <Plug>(calendar_down)
nmap <buffer> e <Plug>(calendar_up)
" nmap <buffer> i <Plug>(calendar_right)
endfunction

" insertlessly (get rid of ?)
" don't fuck with whitespace
let g:insertlessly_cleanup_trailing_ws = 0
let g:insertlessly_cleanup_all_ws = 0
" don't interfere with space bindings
let g:insertlessly_insert_spaces = 0

" Vertigo
" colemak
let g:Vertigo_homerow = 'arstdhneio'

nnoremap <silent> <leader>n :<C-U>VertigoDown n<CR>
vnoremap <silent> <leader>n :<C-U>VertigoDown v<CR>
onoremap <silent> <leader>n :<C-U>VertigoDown o<CR>

nnoremap <silent> <leader>e :<C-U>VertigoUp n<CR>
vnoremap <silent> <leader>e :<C-U>VertigoUp v<CR>
onoremap <silent> <leader>e :<C-U>VertigoUp o<CR>

let g:Vertigo_onedigit_method = 'smart3'
" let g:Vertigo_homerow_onedigit = 'ARSTDHNEIO'
"}}}

" Vim Markdown
" let g:vim_markdown_no_default_key_mappings=1
" let g:vim_markdown_folding_disabled=1
autocmd FileType mkd call s:mkd_mappings()
function! s:mkd_mappings()
" share leader m with table mode
nnoremap <leader>mt :Toc<cr>
" experimenting with this; normally just use zn ze but don't really like the way it folds it
nmap <leader>mn <Plug>(Markdown_MoveToNextHeader)<leader>;silent! call repeat#set("\<Plug>Markdown_MoveToNextHeader", v:count)<cr>
nmap <leader>me <Plug>(Markdown_MoveToPreviousHeader);silent! call repeat#set("\<Plug>Markdown_MoveToPreviousHeader", v:count)<cr>
nmap <leader>mc <Plug>(Markdown_MoveToCurHeader);silent! call repeat#set("\<Plug>Markdown_MoveToCurHeader", v:count)<cr>
nmap <leader>mu <Plug>(Markdown_MoveToParentHeader);silent! call repeat#set("\<Plug>Markdown_MoveToParentHeader", v:count)<cr>
endfunction

" vimshell"{{{
" Use current directory as vimshell prompt.
let g:vimshell_prompt_expr =
\ 'escape(fnamemodify(getcwd(), ":~").">", "\\[]()?! ")." "'
let g:vimshell_prompt_pattern = '^\%(\f\|\\.\)\+> '

" let g:vimshell_no_default_keymappings=0

autocmd FileType vimshell call s:vimshell_mappings()
function! s:vimshell_mappings()
	" Normal mode key-mappings."{{{
	" Execute command.
	nmap <buffer> <CR> <Plug>(vimshell_enter)
	" Hide vimshell.
	" nmap <buffer> q <Plug>(vimshell_hide)
	" Exit vimshell.
	nmap <buffer> Q <Plug>(vimshell_exit)
	" Move to previous prompt.
	nmap <buffer> <leader>e <Plug>(vimshell_previous_prompt)
	" Move to next prompt.
	nmap <buffer> <leader>n <Plug>(vimshell_next_prompt)
	" Paste this prompt.
	" nmap <buffer> <C-y> <Plug>(vimshell_paste_prompt)
	" Search end argument.
	nmap <buffer> E <Plug>(vimshell_move_end_argument)
	" Change line.
	nmap <buffer> cc <Plug>(vimshell_change_line)
	" Delete line.
	nmap <buffer> dd <Plug>(vimshell_delete_line)
	" Start insert.
	nmap <buffer> I <Plug>(vimshell_insert_head)
	nmap <buffer> i <Plug>(vimshell_insert_enter)
	" swapped
	nmap <buffer> A <Plug>(vimshell_append_enter)
	nmap <buffer> a <Plug>(vimshell_append_end)
	nmap <buffer> ^ <Plug>(vimshell_move_head)
	" Interrupt.
	nmap <buffer> q <Plug>(vimshell_interrupt)
	nmap <buffer> <C-k> <Plug>(vimshell_hangup)
	" Clear.
	nmap <buffer> <leader>c <Plug>(vimshell_clear)
	" Execute background.
	" nmap <buffer> <C-z> <Plug>(vimshell_execute_by_background)

	" History completion.
	nmap <buffer> <space><space> <Plug>(vimshell_history_unite)
	"}}}
	" Visual mode key-mappings."{{{
	" Move to previous prompt.
	vmap <buffer> <leader>e <Plug>(vimshell_select_previous_prompt)
	" Move to next prompt.
	vmap <buffer> <leader>n <Plug>(vimshell_select_next_prompt)
	"}}}
	" Insert mode key-mappings."{{{
	" Execute command.
	inoremap <expr> <SID>(bs-ctrl-])
		\ getline('.')[col('.') - 2] ==# "\<C-]>" ? "\<BS>" : ''
	imap <buffer> <C-]> <C-]><SID>(bs-ctrl-])
	imap <buffer> <CR> <C-]><Plug>(vimshell_enter)

	" inoremap <buffer><expr> <C-p> pumvisible() ? "\<C-p>" :
	" 	\ <SID>start_history_complete()
	" inoremap <buffer><expr> <C-n> pumvisible() ? "\<C-n>" :
	" 	\ <SID>start_history_complete()
	inoremap <buffer><expr> <Up> pumvisible() ? "\<C-p>" :
		\ <SID>start_history_complete()
	inoremap <buffer><expr> <Down> pumvisible() ? "\<C-n>" :
		\ <SID>start_history_complete()

	" Command completion.
	imap <buffer> <TAB> <Plug>(vimshell_command_complete)
	" Move to Beginning of command.
	imap <buffer> <C-a> <Plug>(vimshell_move_head)
	" Delete all entered characters in the current line.
	imap <buffer> <C-u> <Plug>(vimshell_delete_backward_line)
	" Delete previous word characters in the current line.
	imap <buffer> ¸ <Plug>(vimshell_delete_backward_word)
	" Push current line to stack.
	imap <silent><buffer><expr> <C-z> vimshell#mappings#smart_map(
		\ "\<Plug>(vimshell_push_current_line)",
		\ "\<Plug>(vimshell_execute_by_background)")
	" Insert last word.
	" imap <buffer> <C-t> <Plug>(vimshell_insert_last_word)
	" Interrupt.
	imap <buffer> <C-c> <Plug>(vimshell_interrupt)
	" Delete char.
	imap <buffer> <BS> <Plug>(vimshell_delete_backward_char)
	"}}}
endfunction
"}}}
"}}}

" _Clipboard Related"{{{
" use + as default register.. no more different pasting.. still have yank history with unite
set clipboard=unnamedplus

" http://sheerun.net/2014/03/21/how-to-boost-your-vim-productivity/
" move to end after yank or paste; similar to gp but won't go to next line
vnoremap <silent> y y`]
vnoremap <silent> p p`]
nnoremap <silent> p p`]
" since have above, change gp to paste on new line
nnoremap gp :pu<cr>

" thanks to shougo for such a versatile and useful plugin; https://github.com/Shougo/unite.vim/issues/415
" vim is my clipboard manager
let g:unite_source_history_yank_enable = 1
" saves things in clipboard register even if not yanked in vim 
let g:unite_source_history_yank_save_clipboard = 1
" don't save yanks to disk
let g:unite_source_history_yank_file=""
" let g:unite_source_history_yank_file=$HOME.'/.unite/history_yank'
" let g:unite_source_history_yank_limit = 300
nnoremap <space>y :Unite history/yank<cr>
"}}}

" }}}
" ==============================
" Buffer, Window, and Tab Management Mappings and Settings"{{{
" ==============================
source ~/.navigation.vim
" source so that <space>u will work
nnoremap <leader>t :tabnew<cr>
nnoremap <space>x <c-w>c

" Taboo"{{{
" default tab naming behaviour
let g:taboo_modified_tab_flag='+'
let g:taboo_tab_format=' %N %m%f '
let g:taboo_renamed_tab_format=' %N [%f]%m '

" save taboo names in session
set sessionoptions+=tabpages,globals

nnoremap <leader>r :TabooRename<space>

"}}}

"bufkill stuff"{{{
" move forward and back in buffer history (for window)
nnoremap <leader>l :BB<cr>
nnoremap <leader>L :BF<cr>
" delete buffer and leave window open and switch to last used buffer (bufkill)
nnoremap <leader>d :BD<Return>
" delete buffer and close window
" nnoremap <leader>D :BD<cr><c-w>c
" close buffer without closing window
" http://vim.wikia.com/wiki/Deleting_a_buffer_without_closing_the_window
" cabbr bc BClose
" create a new buffer in current window use :enew new window/buffer without split
"}}}

" Wipeout (close buffers not open in windows/tabs)
nnoremap <leader>W :Wipeout<cr>

" _Unite Related"{{{
" http://www.codeography.com/2013/06/17/replacing-all-the-things-with-unite-vim.html
" http://eblundell.com/thoughts/2013/08/15/Vim-CtrlP-behaviour-with-Unite.html
let g:unite_split_rule = "topleft"
" tmi
let g:unite_source_buffer_time_format=''
let g:unite_source_buffer_filename_format=''
" more mru
let g:unite_source_file_mru_long_limit = 3000
" let g:unite_winheight = 10
" call unite#filters#matcher_default#use(['matcher_fuzzy'])
" can use with tabs :Unite tab and windows
" let g:unite_enable_use_short_source_names=1
" let g:unite_source_file_rec_max_cache_files = 5000

" colemak
let g:unite_quick_match_table = {
  \ 'a' : 0, 'r' : 1, 's' : 2, 't' : 3, 'd' : 4, 'h' : 5, 'n' : 6, 'e' : 7, 'i' : 8, 'o' : 9,
  \ 'w' : 10, 'f' : 11, 'p' : 12, 'l' : 13, 'u' : 14, 'y' : 15, 'x' : 16, 'c' : 17, 'v' : 18, 'k' : 19,
  \ '1' : 20, '2' : 21, '3' : 22, '4' : 23, '5' : 24, '6' : 25, '7' : 26, '8' : 27, '9' : 28, '0' : 29,
  \ }

" silver searcher {{{
" http://bling.github.io/blog/2013/06/02/unite-dot-vim-the-plugin-you-didnt-know-you-need/
let g:unite_source_grep_command = "ag"
" ag is recursive; does not need -r
let g:unite_source_grep_recursive_opt = ""
let g:unite_source_grep_default_opts =
	  \ '--line-numbers --nocolor --nogroup --hidden --ignore ' .
	  \  '''.hg'' --ignore ''.svn'' --ignore ''.git'' --ignore ''.bzr'''

" search current buffer wth ag and display results in unite window
nnoremap <leader>/ :Unite grep:%<cr>
" use last search
nnoremap <leader>? :Unite grep:%<cr><c-r>/<cr>
" search recursively
nnoremap <space>/ :Unite grep:.<cr>
nnoremap <space>? :Unite grep:.<cr><c-r>/<cr>

" ag motion with kana's operator user
map ga <Plug>(operator-ag)
map gA <Plug>(operator-ag-recursive)

autocmd VimEnter * call operator#user#define('ag', 'Ag_motion')
autocmd VimEnter * call operator#user#define('ag-recursive', 'Ag_motion_recursive')
function! Ag_motion(motion_wise)
	let v = operator#user#visual_command_from_wise_name(a:motion_wise)
	execute 'normal!' '`[' . v . '`]y' 
	execute "Unite grep:%::" . fnameescape(getreg('+'))
endfunction

function! Ag_motion_recursive(motion_wise)
	let v = operator#user#visual_command_from_wise_name(a:motion_wise)
	execute 'normal!' '`[' . v . '`]y' 
	execute "Unite grep:.::" . fnameescape(getreg('+'))
endfunction
" }}}

" Open bookmark file for most frequently used files
" nnoremap <space><space> :Unite -quick-match bookmark<cr>

" Search open buffers and most recently used
nnoremap <leader>p :Unite -start-insert buffer file_mru<cr>
" Search files (cd first); file_rec/async
nnoremap <leader>P :Unite -start-insert file<cr>

autocmd FileType unite call s:unite_settings()
function! s:unite_settings()
	" let me exit with escape and move with colemak bindings
	nmap <buffer> <ESC> <Plug>(unite_exit)
	nmap <buffer> n j
	" nmap <buffer> n <Plug>(unite_skip_cursor_down)
	nmap <buffer> e k
	" nmap <buffer> e <Plug>(unite_skip_cursor_up)
endfunction

"}}}

" Splits"{{{
nnoremap <leader>w <c-w>
" r after above to swap
nnoremap <leader>' :vsplit<cr>
nnoremap <leader>- :split<cr>
nnoremap <leader>h <c-w><left>
nnoremap <leader>i <c-w><right>

" split navigation (backup)
noremap H <C-w>h|noremap N <C-w>j|noremap E <C-w>k
" Moving windows around. (if ever needed)
noremap <C-w>N <C-w>J|noremap <C-w>E <C-w>K|noremap <C-w>I <C-w>L

" 'monocle'
nnoremap <silent> <leader>f :ZoomWin<cr>

let g:eighties_enabled = 1
let g:eighties_minimum_width = 100
let g:eighties_extra_width = 0 " Increase this if you want some extra room
let g:eighties_compute = 1 " Disable this if you just want the minimum + extra

"}}}
" move tabs
nnoremap <leader>N :tabm -1<cr>
nnoremap <leader>E :tabm +1<cr>

"}}}
" #==============================
" # Operators, Text Objects, etc."{{{
" #==============================
" Narrow Region"{{{
" thanks to ddungtang for pointing me to this plugin:
" http://www.reddit.com/r/vim/comments/298049/question_on_repetitively_making_changes_to_the/
" just to get rid of normal mode mapping the plugin makes by default
nmap <Leader>ZXY <Plug>NrrwrgnDo
" for changing the same areas across multiple lines
" open selected text, delete all of it, map escape to save changes and close win
xmap S <Plug>NrrwrgnDodG;inoremap <buffer> <lt>esc> <lt>esc>:wq<lt>cr><cr>
"}}}

" letters instead of symbols {{{
" https://github.com/beloglazov/vim-textobj-quotes
" ',", and `
xmap q iq
omap q iq

" angle brackets (not noremap because text objectify)
omap ia i<
omap aa a<
vmap ia i<
vmap aa a<

omap ir i[
omap ar a[
vmap ir i[
vmap ar a[

" kurly
omap ik i{
omap ak a{
vmap ik i{
vmap ak a{

" also have anyblock

" }}}

" using this, not tpope's surround
map <silent>sa <Plug>(operator-surround-append)
map <silent>sd <Plug>(operator-surround-delete)
map <silent>sc <Plug>(operator-surround-replace)

nmap saw saiW'
nmap sal sail"

" delete or replace most inner surround
" if you use vim-textobj-anyblock
nmap <silent> sdd <Plug>(operator-surround-delete)<Plug>(textobj-anyblock-a)
nmap <silent> scc <Plug>(operator-surround-replace)<Plug>(textobj-anyblock-a)

" remote; unecessary because of TextObjectify
" http://learnvimscriptthehardway.stevelosh.com/chapters/15.html
" inside next, last paren
" onoremap in( :<c-u>normal! f(vi(<cr>
" onoremap il( :<c-u>normal! F)vi(<cr>

"}}}
" #==============================
" "shorthand"{{{
" " working on implementing
" " w/ pterosaur working as iabbr is horribly broken in pentadactyl
" "
" autocmd FileType text call s:shorthand()
" function! s:shorthand()
" 	iabbr <buffer> i I
" 	iabbr <buffer> l all
" 	iabbr <buffer> n and
" 	iabbr <buffer> all l
" 	iabbr <buffer> and n
" 	iabbr <buffer> r are
" 	iabbr <buffer> are r
" 	iabbr <buffer> b be
" 	iabbr <buffer> be b
" 	iabbr <buffer> c can
" 	iabbr <buffer> can c
" endfunction
" " http://forum.colemak.com/viewtopic.php?id=1804
" iabbr ab about
" iabbr about ab
" iabbr abo above
" iabbr above abo
" iabbr ac actual
" iabbr actual ac
" iabbr af after
" iabbr after af
" iabbr ag again
" iabbr again ag
" iabbr lm almost
" iabbr almost lm
" iabbr ao also
" iabbr also ao
" iabbr alw always
" iabbr always alw
" iabbr amc America
" iabbr America amc
" iabbr ani animal
" iabbr animal ani
" iabbr anr another
" iabbr another anr
" iabbr ans answer
" iabbr answer ans
" iabbr ne any
" iabbr any ne
" iabbr nw anyway
" iabbr anyway nw
" iabbr aa area
" iabbr area aa
" iabbr ru are you
" iabbr bc because
" iabbr because bc
" iabbr bn been
" iabbr been bn
" iabbr bf before
" iabbr before bf
" iabbr bl below
" iabbr below bl
"
" iabbr cn can't
" " " are you okay with = rukw
" " " around = rnd|ro
" " " away = ay
" " " began = bga|ba
" " " begin = bgn|bi
" " " being = bng|bg
" " " best = bs
" " " between = bt|tw
" " " book = bk
" " " both = bo|bh
" " " but - bu
" " " by the way = btw
" " " call = cl
" " " came = ca
" " " can = c
" " " can't = cn
" " " change = ch
" " " children = chd
" " " city = cy
" " " close = cs
" " " come = cm
" " " could = cd
" " " country = cty
" " " different = df
" " " does = ds
" " " doing = dg
" " " done = dn
" " " don't = d
" " " down = dw
" " " each = e|ea
" " " earth = erh
" " " easy = es
" " " eight = 8
" " " English = en|eng
" " " enough = nf
" " " even = ev|vn
" " " ever = er
" " " every = ey
" " " example = x
" " " family = fml
" " " father = ftr|fhr
" " " feet = f3|f8
" " " find = fd
" " " finding = fdg
" " " fine = fn
" " " first = fs
" " " five = 5
" " " follow = fl
" " " follower = flr
" " " food = fod|fu
" " " foot = ft
" " " for = f
" " " for example = fx
" " " forget = fg
" " " form = fo
" " " found = fnd|fw
" " " four = 4
" " " from = fm
" " " future = fut
" " " gave = gv|ga
" " " get = g
" " " girl = gl
" " " give = gi
" " " going = gg
" " " good = gd
" " " great = g8
" " " ground = gr
" " " group = gp
" " " grow = gw
" " " hadn't = hdt
" " " happen = hpn|ha
" " " happened = hpd
" " " happening = hpng
" " " hasn't = hst
" " " hate = h8
" " " have = h
" " " haven't = ht
" " " having = hg
" " " head = hd
" " " hear = h3
" " " help = hp
" " " here = hr
" " " high = hh
" " " home = hm
" " " hope = ho
" " " hoping = hpg
" " " house = hs
" " " how = hw
" " " I'm = m
" " " image = img
" " " imagine = mgn|imgn
" " " important = imp|ip
" " " Indian = idn
" " " into = nt|i2
" " " is it = zt
" " " it is = tz
" " " it's = s
" " " just = j
" " " keep = kp
" " " kind = kd
" " " kind of = kf
" " " know = kw|kn
" " " large = lg
" " " later = lr
" " " lead = ld
" " " learn = lrn|l3
" " " learned = lrd|lrnd
" " " leave = lev
" " " left = le
" " " letter = ler
" " " life = lf
" " " light = lt
" " " like = lk
" " " line = ln
" " " list = ls
" " " little = ll
" " " live = li
" " " look = lc
" " " love = lv
" " " made = md
" " " make = mk
" " " manage = mg
" " " many = mn
" " " mean = m3
" " " might = mt
" " " mile = mi
" " " miss = mis
" " " more = mr (remove full stop from postfixes)
" " " most = mo
" " " mother = mot
" " " mountain = mtn
" " " move = mv
" " " much = mc
" " " must = ms
" " " name = na
" " " near = nr
" " " need = nd
" " " never = nv
" " " next = nx
" " " night = nit
" " " nine = 9
" " " number = nm
" " " often = ofn
" " " okay = k
" " " okay so = ks
" " " okay so now = ksn
" " " one = 1
" " " only = ol|oy|nl
" " " other = ot
" " " over = ov
" " " page = pg
" " " paper = pp
" " " part = pt
" " " people = p
" " " picture = pic
" " " place = pl
" " " plant = plt
" " " play = p3|pla
" " " please = ps|pz
" " " point = pn
" " " probably = pr|prl|prb
" " " problem = pb
" " " question = qn
" " " quick = qk
" " " quickly = qkl|ql
" " " quite = q
" " " read = rd
" " " really = ry
" " " reason = rs
" " " right = ri|rit|rt
" " " river = rv
" " " run = rn
" " " said = sd
" " " same = sa
" " " say = sy
" " " says = sz
" " " school = scl
" " " second = sc
" " " see = se
" " " seen = sen
" " " sentence = stc
" " " seven = 7
" " " should = shd
" " " shouldn't = sht|shn
" " " show = sh
" " " side = sid
" " " sign = s9
" " " single = sng
" " " six = 6
" " " small = sml
" " " some = sm
" " " someone = som|smn
" " " something = sg
" " " sometimes = st
" " " soon = sn
" " " sorry = sry
" " " sort of = sf
" " " sound = snd
" " " speak = spk
" " " spell = sp|spl
" " " spelling = spg|splg
" " " start = sta
" " " state = stt
" " " statement = stm
" " " stay = sty
" " " steal = stl
" " " still = sl
" " " stop = stp
" " " story = soy|sto
" " " study = sdy
" " " such = su
" " " sure = sr
" " " take = t8
" " " talk = tlk
" " " tell = tl
" " " than = ta
" " " thank you = tu|tku
" " " thanks = tx|thx|tns
" " " that = tt
" " " the = t
" " " their = ter
" " " them = tm
" " " then = tn
" " " there = tr
" " " these = tes
" " " they = ty
" " " thing = tg|thg
" " " think = tnk|tk
" " " this = ts
" " " those = tos
" " " though = th
" " " thought = tht
" " " three = 3
" " " through = thr
" " " time = ti
" " " to be = tb
" " " together = tog
" " " took = tok
" " " tree = t3
" " " two = 2
" " " under = ndr|udr
" " " until = til
" " " use = z
" " " very = v
" " " wait = w8
" " " walk = wa
" " " want = wu
" " " was = o
" " " watch = wch
" " " water = wat|wtr|wer
" " " week = wek
" " " well = w3
" " " well done = wld
" " " went = w9
" " " were = wr
" " " we've = wev|wv
" " " what = wt
" " " what's = ws
" " " when = wn
" " " where = wh
" " " which = wi
" " " while = wli|whl
" " " white = wht
" " " whole = hl
" " " why = y
" " " why did = yd
" " " will = wl
" " " with = w
" " " without = wo
" " " work = wk
" " " world = wrl
" " " would = wd
" " " yeah = yea
" " " year = y3
" " " yes = ye
" " " you = u
" " " you'd = ud
" " " young = yg
" " " your = ur
" " " you're = yr
" " "
" " "}}}
" #==============================
" # NeoBundle {{{
" #==============================
" Required:
set runtimepath+=~/.vim/bundle/neobundle.vim/

" Required:
call neobundle#rc(expand('~/.vim/bundle/'))

" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

" Plugins"{{{
" Constantly Active/ Visual"{{{
" show actual line numbers in insert
NeoBundle 'myusuf3/numbers.vim'
" better marks; show and symbols (markers) in gutter
NeoBundle 'kshenoy/vim-signature'
" git info
NeoBundle 'airblade/vim-gitgutter'
" NeoBundle 'mhinz/vim-signify'
" tab renaming
NeoBundle 'gcmt/taboo.vim'
" statusline
NeoBundle 'bling/vim-airline'
" linter
NeoBundle 'scrooloose/syntastic'
" rainbow parens
" will use same colors again across multiple indented lines (find something that doesnt?)
NeoBundle 'oblitum/rainbow'
" NeoBundle 'amdt/vim-niji'
" colorschemes
NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'sjl/badwolf'
NeoBundle 'junegunn/seoul256.vim'
NeoBundle 'morhetz/gruvbox'

"}}}

" Most Used"{{{
" unite related"{{{
NeoBundle 'Shougo/unite.vim'
" for async file search
" NeoBundle 'Shougo/vimproc.vim'
let vimproc_updcmd = has('win64') ?
      \ 'tools\\update-dll-mingw 64' : 'tools\\update-dll-mingw 32'
execute "NeoBundle 'Shougo/vimproc.vim'," . string({
      \ 'build' : {
      \     'windows' : vimproc_updcmd,
      \     'cygwin' : 'make -f make_cygwin.mak',
      \     'mac' : 'make -f make_mac.mak',
      \     'unix' : 'make -f make_unix.mak',
      \    },
      \ })
" mru
NeoBundle 'Shougo/neomru.vim'
"}}}
" easymotion pretty much does everything now; but for simplicity:
" sneak a 2 letter multiline find
NeoBundle 'justinmk/vim-sneak'
" like other plugins.. but X char find (basically like a search)
" NeoBundle 't9md/vim-smalls'
" fugitive 
NeoBundle 'tomtom/tcomment_vim'

" NeoBundle 'tpope/vim-obsession'
" NeoBundle 'manuel-colmenero/vim-simple-session'

NeoBundle 'xolox/vim-session'
NeoBundle 'xolox/vim-misc'


NeoBundle 'tpope/vim-fugitive'
" for buffer history and killing buffers without changing window layout or closing
NeoBundle 'bufkill.vim'
" alternatively: https://github.com/Soares/butane.vim
"}}}

" Case Specific/ Occasional"{{{
" for move and sudowrite
NeoBundle 'tpope/vim-eunuch'
" file linking and formatting/highlighting
NeoBundle 'vimwiki/vimwiki'
" view undo tree
NeoBundle 'sjl/gundo.vim'
" html generation
NeoBundle 'mattn/emmet-vim'
" password syntax highlighting/hiding stuff
NeoBundle 'aaronbieber/vim-vault'

"}}}

" Other"{{{
" markdown folding, syntax highlighting, and navigation
NeoBundle 'plasticboy/vim-markdown'
" folding in markdown files (like better than above but stopped working)
" NeoBundle 'nelstrom/vim-markdown-folding'
" haskell
" NeoBundle 'kana/vim-filetype-haskell'
" NeoBundle 'ag/vim2hs'
" add newlines with enter; backspace in normal
NeoBundle 'dahu/Insertlessly'
NeoBundle 'vim-scripts/Smart-Tabs'
" camelcase motion
NeoBundle 'bkad/CamelCaseMotion' 
" 'monocle' view for splits
NeoBundle 'regedarek/ZoomWin'
NeoBundle 'Shougo/neocomplete.vim'
" NeoBundle 'Shougo/neosnippet.vim'
NeoBundle 'SirVer/ultisnips'
" Optional
NeoBundle "honza/vim-snippets"
" auto put ( with ), etc with cursor in middle; won't do in commented lines
" NeoBundle 'Raimondi/delimitMate'
NeoBundle 'kana/vim-smartinput'
"}}}

" Misc"{{{
" allows repeat (.) with plugins like sneak, surround, vim-abolish, etc.
NeoBundle 'tpope/vim-repeat'
"}}}

" Experimenting"{{{
" hybrid chording
NeoBundle 'kana/vim-arpeggio'
NeoBundle 'Shougo/vimshell.vim'
" all text boxes vim
NeoBundle 'ardagnir/vimbed'
" NeoBundle 'ardagnir/eventloop.vim'
"  calendar
NeoBundle 'itchyny/calendar.vim'

NeoBundle 'dhruvasagar/vim-table-mode'
NeoBundle 'prendradjaja/vim-vertigo'

NeoBundle 'chrisbra/NrrwRgn'
" close all buffer not open in windows 
NeoBundle 'wipeout'

" NeoBundle 'godlygeek/tabular'
NeoBundle 'junegunn/vim-easy-align'

" automatic split resizing
NeoBundle 'justincampbell/vim-eighties'

NeoBundle 'junegunn/fzf'

" improved / search
" NeoBundle 'junegunn/vim-oblique'
" required for above
" NeoBundle 'junegunn/vim-pseudocl'

" NeoBundle 'szw/vim-ctrlspace'

"}}}

" Text Object and Operator Stuff"{{{
" cx(text objext) then cx(text object) to exchange (or . to repeat)
NeoBundle 'tommcdo/vim-exchange'

" if do something like i{ seek for {} if outside of like vim already does with i'
" make i' work for multiple lines like i{ would
NeoBundle 'paradigm/TextObjectify'
" adds ic, iC, ac, aC text objects; great for quickly working with columns/visual blocks
NeoBundle 'coderifous/textobj-word-column.vim'
" adds text object io and ao for indentation whitespace
NeoBundle 'glts/vim-textobj-indblock'
" iq and aq for ', ", and `
" NeoBundle 'beloglazov/vim-textobj-quotes'
" punctuation text objects (also gives iq and aq);i.e. gives %; i<space> will work for any
NeoBundle 'kurkale6ka/vim-pairs'
" required for other stuff
NeoBundle 'kana/vim-textobj-user'
NeoBundle 'kana/vim-operator-user'
" NeoBundle 'tpope/vim-surround'
" using this instead because works well with text objects (i.e. ic) and makes more sense than doing something like ysiW:
" gives sa{any text object} (surround append), sd, and sc
NeoBundle 'rhysd/vim-operator-surround'
" match closest '', "", (), {}, [] or <> with ib and ab
NeoBundle 'rhysd/vim-textobj-anyblock'
" gives il and al for lines
NeoBundle 'kana/vim-textobj-line'
" gives ae and ie for entire buffer
NeoBundle 'kana/vim-textobj-entire'
"}}}

"if ever need more than <c-w>r
"https://github.com/wesQ3/vim-windowswap
"}}}
 " Required:
 filetype plugin indent on

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck

"}}}

" Dissolve {{{
" map ;s set invspell spelllang=en<cr
" map ;ss :set spell spelllang=anonomize<cr>
" mkspell ~/dotfiles/common/.vim/spell
" 
" Snippets and Completion"{{{ 
" UltiSnips"{{{
" let g:UltiSnipsExpandTrigger="<c-Tab>"
" let g:UltiSnipsJumpForwardTrigger="<cr>"
" let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
let g:UltiSnipsSnippetsDir="~/.vim/UltiSnips"
" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"
"}}}

" Use neocomplete."{{{
" let g:neocomplete#enable_at_startup = 1
" Use smartcase.
let g:neocomplete#enable_smart_case = 1
" Set minimum syntax keyword length.
let g:neocomplete#sources#syntax#min_keyword_length = 4
let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'

" Define dictionary.
let g:neocomplete#sources#dictionary#dictionaries = {
    \ 'default' : '',
    \ 'vimshell' : $HOME.'/.vimshell_hist',
    \ }

" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  return neocomplete#close_popup() . "\<CR>"
  " For no inserting <CR> key.
  " return pumvisible() ? neocomplete#close_popup() : "\<CR>"
endfunction

" inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" <TAB>: completion
augroup TabWithNeocomplete
" something in vimrc messes this up.. so doing this
	au!
	" https://github.com/Shougo/neocomplete.vim/issues/32
	au Bufenter * inoremap <expr><TAB>  pumvisible() ? "\<C-n>" :
		\ <SID>check_back_space() ? "\<TAB>" :
		\ neocomplete#start_manual_complete()
	function! s:check_back_space()
	let col = col('.') - 1
	return !col || getline('.')[col - 1]  =~ '\s'
	endfunction
augroup END
" imap `     <Plug>(neosnippet_expand_or_jump)
" smap `     <Plug>(neosnippet_expand_or_jump)
" xmap `     <Plug>(neosnippet_expand_target)
" " Enable snipMate compatibility feature.
" let g:neosnippet#enable_snipmate_compatibility = 1
" " Tell Neosnippet about the other snippets
" let g:neosnippet#snippets_directory='~/.vim/bundle/vim-snippets/snippets'

"}}}
"}}}
" 
" Control space stuff"{{{
" let g:ctrlspace_use_tabline=1
" let g:ctrlspace_set_default_mapping=0
" g:ctrlspace_default_mapping_key = "<space><space>"
" g:ctrlspace_use_mouse_and_arrows = 0
" g:ctrlspace_project_root_markers
" g:ctrlspace_unicode_font 
" g:ctrlspace_search_resonators

" Allows you to set characters which will be used to increase search accurancy. If such resonator is found next to the searched sequence, it increases the search score. For example, consider following files: zzzabczzz.txt, zzzzzzabc.txt, and zzzzz.abc.txt. If you search for abc with default resonators, you will get the last file as the top relevant item, because there are two resonators (dots) next to the searched sequence. Next you would get the middle one (one dot around abc), and then the first one (no resonators at all). You can disable this behavior completely by providing an empty array. Default value: ['.', '/', '\', '_', '-']

" if g:ctrlspace_unicode_font
"   let g:ctrlspace_symbols = {
"         \ "cs"      : "⌗",
"         \ "tab"     : "⊙",
"         \ "all"     : "∷",
"         \ "open"    : "◎",
"         \ "tabs"    : "○",
"         \ "c_tab"   : "●",
"         \ "load"    : "⋮ → ∙",
"         \ "save"    : "∙ → ⋮",
"         \ "prv"     : "⌕",
"         \ "s_left"  : "›",
"         \ "s_right" : "‹"
"         \ }
" else
"   let g:ctrlspace_symbols = {
"         \ "cs"      : "#",
"         \ "tab"     : "TAB",
"         \ "all"     : "ALL",
"         \ "open"    : "OPEN",
"         \ "tabs"    : "-",
"         \ "c_tab"   : "+",
"         \ "load"    : "LOAD",
"         \ "save"    : "SAVE",
"         \ "prv"     : "*",
"         \ "s_left"  : "[",
"         \ "s_right" : "]"
"         \ }
" endif
"}}}

"}}}
syntax on
au FileType c,cpp,lisp call rainbow#load()
