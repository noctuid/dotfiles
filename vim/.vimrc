" Todo:
" add smartinput rules
" lock space like other keys
" set up yanking properly in unite

" fixes complaining about undefined tcomment variable
set runtimepath+=~/.vim/bundle/tcomment_vim
set runtimepath+=~/.vim/colors
" turns off vi compatibility mode for full vim functionality; set first
set nocompatible

" Experimental"{{{
" retrain to stop using caps layer in vim
nnoremap <up> <nop>
nnoremap <left> <nop>
" nnoremap <right> <nop>
nnoremap <down> <nop>
nnoremap <End> <nop>
nnoremap <Home> <nop>
" inoremap <up> <nop>
" inoremap <left> <nop>
" inoremap <right> <nop>
" inoremap <down> <nop>
" inoremap <End> <nop>
" inoremap <Home> <nop>

if has("gui_running")
" wm experementation"{{{

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
nnoremap <silent> sd :silent !bspc config -d focused window_gap $((`bspc config -d focused window_gap` + 4 ))<cr>

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
"}}}

" select session
nnoremap <silent> ss :silent !tmux choose-client<cr>
"}}}
"}}}
endif

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
" persistent undo history
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

" at least 5 lines show below and above cursor; experimenting with cursor position
set scrolloff=5

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
" save on insert leave (if changed; fix for unite)
" au InsertLeave * update

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

" source .vimrc on enter; this still slows things down significantly on startup; change to quickvimrc?
" autocmd vimenter * source ~/dotfiles/vim/.vimrc

" too slow; change to quickvimrc?
" augroup AutoReloadVimRC2
" au!

" au BufReadPost * so ~/dotfiles/vim/.vimrc
" augroup END

"}}}

" Command mode"{{{
" when tab completing from command line will show
set wildmenu

" changes tab behaviour in command line (i.e. list makes every possible choice popup:)
set wildmode:full
"}}}

" Searching"{{{
" stop highlighting on escape
set hlsearch
nnoremap <esc> :noh<cr><esc>

" this causes arrow keys to make As and Bs in insert mode in terminal vim
if has("gui_running")
	inoremap <esc> <esc>:noh<cr>
endif
" don't move when search
set noincsearch
" go back to beginning once reach end
set wrapscan

" when searching lower case, will match any case
set ignorecase
" won't ignore case if search with upper case
set smartcase
"}}}

"}}}
" #==============================
" # Specific Filetype Settings"{{{
" #==============================
" can also add file specific settings to ~/.vim/after/ftplugin; html.vim or python.vim for example; use setlocal instead
" see http://stackoverflow.com/questions/1889602/multiple-vim-configurations

" Text file settings"{{{
" remove listchars from txt files in favour of better wrapping (not cutting off halfway in between a word) for long lines
autocmd BufEnter *.txt setlocal nolist
autocmd BufEnter *.txt setlocal lbr
" set showbreak=···\ " Line break indicator.

" comment graying in text files
autocmd BufEnter *.txt highlight text guifg=gray
autocmd BufEnter *.txt match text /#.*/

" fix zf (folding now uses right comment char)
" autocmd BufEnter *.txt setlocal comments=:#
autocmd BufEnter *.txt setlocal commentstring=#\ %s

" comment my textfiles with octothorpe
call tcomment#DefineType('text', '# %s' )
" spell check text files
autocmd BufEnter *.txt set spell

" spelling
" if v:version >= 700
" " Enable spell check for text files
" autocmd BufNewFile,BufRead *.txt setlocal spell spelllang=en
" endif

"}}}

" Tex file settings"{{{
autocmd BufEnter *.tex setlocal nolist
autocmd BufEnter *.tex setlocal lbr
autocmd BufEnter *.tex setlocal textwidth=0

" spell check tex files
autocmd BufEnter *.tex set spell

"}}}

call tcomment#DefineType('pentadactyl', '" %s' )
autocmd BufNewFile,BufRead *.pentadactylrc,*.penta set filetype=pentadactyl
let g:commentChar = {
\ 'conf': '#'
\}
"}}}
" #==============================
" # Appearance"{{{
" #==============================
" syntax highlighting works when put at end but not here..
" syntax on

" see vivify; favourites: darkrobot, railscasts, **molokai, etc.
colorscheme darkrobot
set t_Co=256
if has("gui_running")
	set guicursor=a:blinkon900-blinkoff600 " Slow down cursor blinking speed
	colorscheme molokai
	set background=dark
	set guifont=Inconsolata\ 11
endif

" Airline theme "{{{
" airline always present
set laststatus=2
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
" set tabpagemax=50

"}}}
" #==============================
" # Spacing/Indentation Stuff "{{{
" #==============================
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

" "}}}
" #==============================
" # Folding"{{{
" #==============================
set foldmethod=marker
" indent folding for python
"au BufEnter *.py set foldmethod=indent
"}}}
" #==============================
" # General Mappings/ Bindings and Settings "{{{
" #==============================
" focus on not using modifiers or hard to reach keys; may add more modifiers in future when remapping progresses for dual and I get more thumb keys
" main leader/prefix keys (in order of ease): t (not worthy of such a great colemak position and I never except for dt and such which is not affected) and space (about as easy)
" let mapleader = "\<space>"
let mapleader = "t"

" Colemak/Navigation Mappings"{{{
" modified from here:
" https://github.com/bunnyfly/dotfiles/blob/master/vimrc
" http://forum.colemak.com/viewtopic.php?id=1808
" My change: keep i and don't have a dedicated right
" l for 'last' instead of line

noremap n gj|noremap e gk|nnoremap gn j|nnoremap ge k

" don't just place in the middle; open folds too
nnoremap <silent> k nzozz|nnoremap <silent> K Nzozz

" BOL/EOL/Join Lines; took out l to ^ in favor of l for <c-o>
" nnoremap L $|nnoremap <C-l> J
nnoremap L <c-i>
" l for last
nnoremap l <c-o>
" h for beginning of line
" nnoremap h 0

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
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;
" Y like D
nnoremap Y y$
" Sane redo.
noremap U <C-r>

" I use a more than A
nnoremap a A
nnoremap A a

" I use V more than v.. going to try this; can also double tap v now for visual
nnoremap v V|nnoremap V v

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
omap <silent> ib <Plug>CamelCaseMotion_ib
xmap <silent> ib <Plug>CamelCaseMotion_ib
omap <silent> ie <Plug>CamelCaseMotion_ie
xmap <silent> ie <Plug>CamelCaseMotion_ie

" use much more frequently
nnoremap dw daw
nnoremap cw caw
nnoremap yw yaw
nnoremap dW daW
nnoremap cW caW
nnoremap yW yaW

"}}}

" better text file long line nav (use with lazy redraw); up and down between wraps
inoremap <Down> <C-o>gj
inoremap <Up> <C-o>gk

" jump up and down
nnoremap <leader>k <c-d>|nnoremap <leader>o <c-u>
vnoremap <leader>k <c-d>|vnoremap <leader>o <c-u>

" Better save mapping; still not there yet
inoremap § <esc>:w<cr>
nnoremap § :w<cr>
" try to use this instead
nnoremap <leader>s :w<cr>

" control backspace behaviour
inoremap ¸ <c-w>
cnoremap ¸ <c-w>
nnoremap ¸ daw
" inoremap ... <c-u>

" keep cursor at beginning of line after hitting enter; fix
inoremap <return> <return><home>

" source vimrc
nnoremap <leader>. :so ~/.vimrc<CR>
"}}}

" operator, text object, etc. table:
" t- to
" z- sneak
" s- surrounding
" x- exchange
" aw, iw (camelcase)
" iq- quote
" s- sentence
" space (for punctuation)

" remote
" http://learnvimscriptthehardway.stevelosh.com/chapters/15.html
" inside next, last paren
onoremap in( :<c-u>normal! f(vi(<cr>
onoremap il( :<c-u>normal! F)vi(<cr>

" leader table"{{{
" interestingly enough.. can have single letter and multiple letters (ex b and bn after leader ; delay though)
"bc- neobundle clean
"bu- neobundle update
"c- tcomment
"d- BD
"D- close window too
"e- next tab
"E- next buffer in history
"g- fugitive bindings
"i- neobundle install
"l- back in buffer history
"L- forward in buffer history
"n- previous tab
"N- previous buffer in history
"p- Unite mru and buffer
"P- unite file
"q- :q
"s- save
"tn- taboo open
"tr- taboo rename
"tt- tabnew
"u- gundo
"x- 'xray' view
".- source vimrc

"}}}

"space table
" space arstdhneio; 1-10gt or 11-20gt
" wfp to switch between "sections"
" u- show unite bookmarks for tab name
" U- add unite bookmark
" y- yank history

"'x-ray' view; cuc and cul
nnoremap <leader>x :set cursorcolumn!<cr>:set cursorline!<cr>

" Plugin Specific"{{{
" NeoBundle stuff"{{{
nnoremap <leader>bi :Unite neobundle/install<cr>
nnoremap <leader>bc :NeoBundleClean<cr>
" neobundle update
nnoremap <leader>bu :Unite neobundle/update<cr>
nnoremap <leader>bs :Unite neobundle/search<cr>
nnoremap <leader>bl :NeoBundleList<cr>
"}}}

" Fugitive/git"{{{
nnoremap <leader>ga :Gwrite<cr>
nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gc :Gcommit<cr>
nnoremap <leader>gr :Gread<cr>
"}}}

" Snippets and Completion"{{{ 
" UltiSnips"{{{
" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
"
let g:UltiSnipsSnippetsDir="~/.vim/UltiSnips"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"
"}}}

" Use neocomplete."{{{
let g:neocomplete#enable_at_startup = 1
" Use smartcase.
let g:neocomplete#enable_smart_case = 1
" Set minimum syntax keyword length.
let g:neocomplete#sources#syntax#min_keyword_length = 3
let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'

inoremap ` <c-u>
"}}}

"}}}

" Gundo
nnoremap <leader>u :GundoToggle<CR>
let g:gundo_map_move_older="n"
let g:gundo_map_move_newer="e"

" tcomment
map <leader>c <C-_><C-_>

" Emmet vim
map <leader>y <C-y>,

" from bling/dotvim
" syntastic "{{{
let g:syntastic_error_symbol = '✗'
let g:syntastic_style_error_symbol = '✠'
let g:syntastic_warning_symbol = '∆'
let g:syntastic_style_warning_symbol = '≈'
"}}}

" session management:"{{{
" quickly open session
nnoremap <leader>ss :OpenSession
" vim-session options (auto save on exit, open saved on open)
let g:session_autosave_periodic='yes'
let g:session_autosave='yes'
let g:session_autoload='yes'
"}}}

" Sneak settings"{{{
" 1 means use ignorecase or smartcase if set (have smartcase set)
let g:sneak#use_ic_scs = 1
" use streak mode (easy motion highlighting) when > 1 match on screen
let g:sneak#streak = 1
" s to go to next S for previous
let g:sneak#s_next = 1
let g:sneak#textobject_z = 1

" hi link SneakPluginTarget ErrorMsg
hi link SneakStreakTarget ErrorMsg
" hi link SneakStreakMask Comment
" nmap f <Plug>Sneak_s
" operator-pending-mode
" omap z <Plug>Sneak_s
" omap Z <Plug>Sneak_S
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

" insertlessly
" don't fuck with whitespace
let g:insertlessly_cleanup_trailing_ws = 0
let g:insertlessly_cleanup_all_ws = 0
" don't interfere with space bindings
let g:insertlessly_insert_spaces = 0
"}}}

" _Clipboard Related"{{{
" use + as default register.. no more different pasting.. still have yank history with unite
set clipboard=unnamedplus

" http://sheerun.net/2014/03/21/how-to-boost-your-vim-productivity/
" move to end after yank or paste; maybe change this
vnoremap <silent> y y`]
vnoremap <silent> p p`]
nnoremap <silent> p p`]

" thanks to shougo for such a versatile and useful plugin; https://github.com/Shougo/unite.vim/issues/415
" vim is my clipboard manager
let g:unite_source_history_yank_enable = 1
let g:unite_source_history_yank_save_clipboard = 1
let g:unite_source_history_yank_limit = 300
" nnoremap <space>y :Unite register history/yank<cr>
nnoremap <space>y :Unite history/yank<cr>
"}}}

" }}}
" ==============================
" Buffer and Tab Management Mappings and Settings"{{{
" ==============================
" access to hundreds of files in 2-5 keystrokes via bookmarks and folder specific file search based on tab name/Topic; no significant memorization required
"see unite
"tabs"{{{

" Taboo"{{{
" default tab naming behaviour
let g:taboo_modified_tab_flag='+'
let g:taboo_tab_format=' %N %m%f '
let g:taboo_renamed_tab_format=' %N [%f]%m '

" save taboo names in session
set sessionoptions+=tabpages,globals

nnoremap <leader>r :TabooRename<space>
" nnoremap <leader>tn :TabooOpen<space>

" source so that <space>u will work
nnoremap <leader>t :tabnew<cr>
"}}}

" quicker tab navigation"{{{
nnoremap N gT
nnoremap E gt
" can add <buffer> and then source vimrc on bufenter instead but that causes a huge slowdown
nnoremap <space>w 1gt:source ~/.quickvimrc<cr>
nnoremap <space>f 11gt:source ~/.quickvimrc<cr>
nnoremap <space>p 12gt:source ~/.quickvimrc<cr>
if tabpagenr() < 11
	nnoremap <space>a 1gt
	nnoremap <space>r 2gt
	nnoremap <space>s 3gt
	nnoremap <space>t 4gt
	nnoremap <space>d 5gt
	nnoremap <space>h 6gt
	nnoremap <space>n 7gt
	nnoremap <space>e 8gt
	nnoremap <space>i 9gt
	nnoremap <space>o 10gt
elseif tabpagenr() >= 10
	nnoremap <space>a 11gt
	nnoremap <space>r 12gt
	nnoremap <space>s 13gt
	nnoremap <space>t 14gt
	nnoremap <space>d 15gt
	nnoremap <space>h 16gt
	nnoremap <space>n 17gt
	nnoremap <space>e 18gt
	nnoremap <space>i 19gt
	nnoremap <space>o 20gt
endif

"}}}
"}}}
nnoremap <leader>q :q<cr>

"buffkill stuff"{{{
" move forward and back in buffer history (for window)
nnoremap <leader>l :BB<cr>
nnoremap <leader>L :BF<cr>
" delete buffer and leave window open and switch to last used buffer (bufkill)
nnoremap <leader>d :BD<Return>
" delete buffer and close window
nnoremap <leader>D :BD<cr><c-w>c
" close buffer without closing window
" http://vim.wikia.com/wiki/Deleting_a_buffer_without_closing_the_window
" cabbr bc BClose
" create a new buffer in current window use :enew new window/buffer without split
"}}}

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

" http://bling.github.io/blog/2013/06/02/unite-dot-vim-the-plugin-you-didnt-know-you-need/
nnoremap <space>/ :Unite ag:.<cr>

" Open bookmark file for most frequently used files
nnoremap <space><space> :Unite -quick-match bookmark<cr>

" Search open buffers and most recently used
nnoremap <leader>p :Unite -start-insert buffer file_mru<cr>
" Search files (cd first); file_rec/async
nnoremap <leader>P :Unite -start-insert file<cr>

autocmd FileType unite call s:unite_settings()
function! s:unite_settings()
" let me exit with escape and move with colemak bindings
nmap <buffer> <ESC> <Plug>(unite_exit)
nmap <buffer> n j
nmap <buffer> e k
endfunction

" Thanks to Ingo Karkat for answering my question ; http://stackoverflow.com/questions/21125170/grabbing-the-current-tab-name
cnoreabbr <expr> tabname t:taboo_tab_name
cnoreabbr <expr> buffername expand('%:t')

cabbr writ cd ~/ag-sys/Else/everything/\#Another/
cabbr dot cd ~/dotfiles
cabbr ubmark cd ~/.unite/bookmark/
cabbr ag cd ~/ag-sys/
cabbr else cd ~/ag-sys/Else/everything/
cabbr cdranger cd ~/.config/ranger
cabbr cdbin cd ~/bin

" <space>u for bookmark file corresponding to tab name
 
" resource to auto change mapping (otherwise won't cycle after first time)
"stop complaining if not named
if exists("t:taboo_tab_name")

	" open the bookmarks folder for the tab name; takes care of all frequently accessed files
	nnoremap <buffer> <space>u :Unite -quick-match bookmark:tabname<C-]><cr>
	"  add a file to the bookmarks folder for the tab name/category
	nnoremap <buffer> <space>U :UniteBookmarkAdd<cr>tabname<c-]><cr>buffername<c-]>

	" folder instead of bookmarks (no reason to make bookmarks that correspond exactly to folder
	if t:taboo_tab_name == 'conf-ranger'
		nnoremap <buffer> <space>u :cdranger<C-]><cr>:Unite -quick-match file<cr>
	elseif t:taboo_tab_name == 'bin'
		cd ~/bin
		nnoremap <buffer> <space>u :Unite -quick-match file<cr>

	" 2 way cycle between 2 main categories
	elseif t:taboo_tab_name == "main"
		nnoremap <buffer> <space>c :TabooRename mainl<cr>:so ~/.quickvimrc<cr>
	elseif t:taboo_tab_name == "mainl"
		nnoremap <buffer> <space>c :TabooRename main<cr>:so ~/.quickvimrc<cr>

	" conf cycle
	elseif t:taboo_tab_name == "config"
		nnoremap <buffer> <space>c :TabooRename conf-music<cr>:so ~/.quickvimrc<cr>
	elseif t:taboo_tab_name == "conf-music"
		nnoremap <buffer> <space>c :TabooRename conf-mail<cr>:so ~/.quickvimrc<cr>
	elseif t:taboo_tab_name == "conf-mail"
		nnoremap <buffer> <space>c :TabooRename conf-other<cr>:so ~/.quickvimrc<cr>
	elseif t:taboo_tab_name == "conf-other"
		nnoremap <buffer> <space>c :TabooRename config<cr>:so ~/.quickvimrc<cr>

	elseif t:taboo_tab_name == "dot"
		nnoremap <buffer> <space>u :cd ~/dotfiles<cr>:Unite file<cr>
	endif
endif
"}}}

" Split bindings"{{{
nnoremap <leader>w <c-w>
" r after above to swap
nnoremap <leader>/ :vsplit<cr>
nnoremap <leader>- :split<cr>
" already have N and E set to switch between splits
nnoremap <a-n> <c-w><left>
nnoremap <a-e> <c-w><right>
" resize windows (relative.. and dwm.vim is very lacking
nnoremap <leader>h :vertical resize -8<cr>
nnoremap <leader>i :vertical resize +8<cr>
nnoremap <leader>e <c-w><right>
nnoremap <leader>n <c-w><left>

" 'monocle'
nnoremap <silent> <leader>m :ZoomWin<cr>

"}}}
" move tabs
nnoremap <leader>N :tabm -1<cr>
nnoremap <leader>E :tabm +1<cr>

"}}}
" #==============================
" "shorthand"{{{
" " working on implementing; not a fan of configuring autokey; this is much easier; use vim and penta for 95% of typing.. will probably add to weechat as well
" " get pterosaur working as iabbr is horribly broken in pentadactyl
" add for only txt files
"
iabbr i I
" http://forum.colemak.com/viewtopic.php?id=1804
iabbr ab about
iabbr about ab
iabbr abo above
iabbr above abo
iabbr ac actual
iabbr actual ac
iabbr af after
iabbr after af
iabbr ag again
iabbr again ag
iabbr l all
iabbr all l
iabbr lm almost
iabbr almost lm
iabbr ao also
iabbr also ao
iabbr alw always
iabbr always alw
iabbr amc America
iabbr America amc
iabbr n and
iabbr and n
iabbr ani animal
iabbr animal ani
iabbr anr another
iabbr another anr
iabbr ans answer
iabbr answer ans
iabbr ne any
iabbr any ne
iabbr nw anyway
iabbr anyway nw
iabbr r are
iabbr are r
iabbr aa area
iabbr area aa
iabbr ru are you
iabbr b be
iabbr be b
iabbr bc because
iabbr because bc
iabbr bn been
iabbr been bn
iabbr bf before
iabbr before bf
iabbr bl below
iabbr below bl

iabbr c can
iabbr can c
iabbr cn can't
" " are you okay with = rukw
" " around = rnd|ro
" " away = ay
" " began = bga|ba
" " begin = bgn|bi
" " being = bng|bg
" " best = bs
" " between = bt|tw
" " book = bk
" " both = bo|bh
" " but - bu
" " by the way = btw
" " call = cl
" " came = ca
" " can = c
" " can't = cn
" " change = ch
" " children = chd
" " city = cy
" " close = cs
" " come = cm
" " could = cd
" " country = cty
" " different = df
" " does = ds
" " doing = dg
" " done = dn
" " don't = d
" " down = dw
" " each = e|ea
" " earth = erh
" " easy = es
" " eight = 8
" " English = en|eng
" " enough = nf
" " even = ev|vn
" " ever = er
" " every = ey
" " example = x
" " family = fml
" " father = ftr|fhr
" " feet = f3|f8
" " find = fd
" " finding = fdg
" " fine = fn
" " first = fs
" " five = 5
" " follow = fl
" " follower = flr
" " food = fod|fu
" " foot = ft
" " for = f
" " for example = fx
" " forget = fg
" " form = fo
" " found = fnd|fw
" " four = 4
" " from = fm
" " future = fut
" " gave = gv|ga
" " get = g
" " girl = gl
" " give = gi
" " going = gg
" " good = gd
" " great = g8
" " ground = gr
" " group = gp
" " grow = gw
" " hadn't = hdt
" " happen = hpn|ha
" " happened = hpd
" " happening = hpng
" " hasn't = hst
" " hate = h8
" " have = h
" " haven't = ht
" " having = hg
" " head = hd
" " hear = h3
" " help = hp
" " here = hr
" " high = hh
" " home = hm
" " hope = ho
" " hoping = hpg
" " house = hs
" " how = hw
" " I'm = m
" " image = img
" " imagine = mgn|imgn
" " important = imp|ip
" " Indian = idn
" " into = nt|i2
" " is it = zt
" " it is = tz
" " it's = s
" " just = j
" " keep = kp
" " kind = kd
" " kind of = kf
" " know = kw|kn
" " large = lg
" " later = lr
" " lead = ld
" " learn = lrn|l3
" " learned = lrd|lrnd
" " leave = lev
" " left = le
" " letter = ler
" " life = lf
" " light = lt
" " like = lk
" " line = ln
" " list = ls
" " little = ll
" " live = li
" " look = lc
" " love = lv
" " made = md
" " make = mk
" " manage = mg
" " many = mn
" " mean = m3
" " might = mt
" " mile = mi
" " miss = mis
" " more = mr (remove full stop from postfixes)
" " most = mo
" " mother = mot
" " mountain = mtn
" " move = mv
" " much = mc
" " must = ms
" " name = na
" " near = nr
" " need = nd
" " never = nv
" " next = nx
" " night = nit
" " nine = 9
" " number = nm
" " often = ofn
" " okay = k
" " okay so = ks
" " okay so now = ksn
" " one = 1
" " only = ol|oy|nl
" " other = ot
" " over = ov
" " page = pg
" " paper = pp
" " part = pt
" " people = p
" " picture = pic
" " place = pl
" " plant = plt
" " play = p3|pla
" " please = ps|pz
" " point = pn
" " probably = pr|prl|prb
" " problem = pb
" " question = qn
" " quick = qk
" " quickly = qkl|ql
" " quite = q
" " read = rd
" " really = ry
" " reason = rs
" " right = ri|rit|rt
" " river = rv
" " run = rn
" " said = sd
" " same = sa
" " say = sy
" " says = sz
" " school = scl
" " second = sc
" " see = se
" " seen = sen
" " sentence = stc
" " seven = 7
" " should = shd
" " shouldn't = sht|shn
" " show = sh
" " side = sid
" " sign = s9
" " single = sng
" " six = 6
" " small = sml
" " some = sm
" " someone = som|smn
" " something = sg
" " sometimes = st
" " soon = sn
" " sorry = sry
" " sort of = sf
" " sound = snd
" " speak = spk
" " spell = sp|spl
" " spelling = spg|splg
" " start = sta
" " state = stt
" " statement = stm
" " stay = sty
" " steal = stl
" " still = sl
" " stop = stp
" " story = soy|sto
" " study = sdy
" " such = su
" " sure = sr
" " take = t8
" " talk = tlk
" " tell = tl
" " than = ta
" " thank you = tu|tku
" " thanks = tx|thx|tns
" " that = tt
" " the = t
" " their = ter
" " them = tm
" " then = tn
" " there = tr
" " these = tes
" " they = ty
" " thing = tg|thg
" " think = tnk|tk
" " this = ts
" " those = tos
" " though = th
" " thought = tht
" " three = 3
" " through = thr
" " time = ti
" " to be = tb
" " together = tog
" " took = tok
" " tree = t3
" " two = 2
" " under = ndr|udr
" " until = til
" " use = z
" " very = v
" " wait = w8
" " walk = wa
" " want = wu
" " was = o
" " watch = wch
" " water = wat|wtr|wer
" " week = wek
" " well = w3
" " well done = wld
" " went = w9
" " were = wr
" " we've = wev|wv
" " what = wt
" " what's = ws
" " when = wn
" " where = wh
" " which = wi
" " while = wli|whl
" " white = wht
" " whole = hl
" " why = y
" " why did = yd
" " will = wl
" " with = w
" " without = wo
" " work = wk
" " world = wrl
" " would = wd
" " yeah = yea
" " year = y3
" " yes = ye
" " you = u
" " you'd = ud
" " young = yg
" " your = ur
" " you're = yr
" "
" "}}}
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
" gitgutter
NeoBundle 'airblade/vim-gitgutter'
" tab renaming
NeoBundle 'gcmt/taboo.vim'
" statusline
NeoBundle 'bling/vim-airline'
" linter
NeoBundle 'scrooloose/syntastic'
"}}}

" Most Used"{{{
" unite related"{{{
NeoBundle 'Shougo/unite.vim'
" for async file search
NeoBundle 'Shougo/vimproc.vim'
" mru
NeoBundle 'Shougo/neomru.vim'
"}}}
" sneak
NeoBundle 'justinmk/vim-sneak'
" fugitive 
NeoBundle 'tomtom/tcomment_vim'
NeoBundle 'xolox/vim-session'
" required by vim-session
NeoBundle 'xolox/vim-misc'
NeoBundle 'tpope/vim-fugitive'
" for buffer history and killing buffers without changing window layout or closing
NeoBundle 'bufkill.vim'
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

"}}}

" Other"{{{
" folding in markdown files
NeoBundle 'nelstrom/vim-markdown-folding'
" add newlines with enter; backspace in normal
NeoBundle 'dahu/Insertlessly'
NeoBundle 'vim-scripts/Smart-Tabs'
" camelcase motion
NeoBundle 'bkad/CamelCaseMotion' 
" 'monocle' view for splits
NeoBundle 'regedarek/ZoomWin'
NeoBundle 'Shougo/neocomplete.vim'
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
NeoBundle 'ardagnir/pterosaur'
NeoBundle 'itchyny/calendar.vim'
"}}}

" Text Object and Operator Stuff"{{{
" punctuation text objects
NeoBundle 'kurkale6ka/vim-pairs'
NeoBundle 'tommcdo/vim-exchange'
NeoBundle 'tpope/vim-surround'
" NeoBundle 'rhysd/vim-operator-surround'
" required for above
" NeoBundle 'kana/vim-operator-user'
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
" poor man's dual role I guess: (for people who remap control is remapped to caps or thumb key)
" inoremap <c> Escape
" Other people's stuff I have stolen:
" Uncomment the following to have Vim jump to the last position when
" reopening a file
" if has("autocmd")
"   au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
" endif

" nmap cp :let @" = expand("%")<cr>
" can do ctrl r then % in insert mode instead

" map ;s set invspell spelllang=en<cr
" map ;ss :set spell spelllang=anonomize<cr>
" mkspell ~/dotfiles/common/.vim/spell

" " Restore cursor position to where it was before
" augroup JumpCursorOnEdit
" au!
" autocmd BufReadPost *
" \ if expand("<afile>:p:h") !=? $TEMP |
" \ if line("'\"") > 1 && line("'\"") <= line("$") |
" \ let JumpCursorOnEdit_foo = line("'\"") |
" \ let b:doopenfold = 1 |
" \ if (foldlevel(JumpCursorOnEdit_foo) > foldlevel(JumpCursorOnEdit_foo - 1)) |
" \ let JumpCursorOnEdit_foo = JumpCursorOnEdit_foo - 1 |
" \ let b:doopenfold = 2 |
" \ endif |
" \ exe JumpCursorOnEdit_foo |
" \ endif |
" \ endif
" " Need to postpone using "zv" until after reading the modelines.
" autocmd BufWinEnter *
" \ if exists("b:doopenfold") |
" \ exe "normal zv" |
" \ if(b:doopenfold > 1) |
" \ exe "+".1 |
" \ endif |
" \ unlet b:doopenfold |
" \ endif
" augroup END

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Other Colemak Arrow-Based Mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Switch panes... I don't really have enough space to do more than a vsplit; so just NE
" noremap H <C-w>h|noremap I <C-w>l|noremap N <C-w>j|noremap E <C-w>k
noremap N <c-w>h|noremap E <c-w>l
" Moving windows around.
  noremap <C-w>N <C-w>J|noremap <C-w>E <C-w>K|noremap <C-w>I <C-w>L
" High/Low/Mid.
" noremap <C-e> H|noremap <C-n> L|noremap <C-m> M
" Scroll up/down.
" noremap zn <C-y>|noremap ze <C-e>

" +/- increment and decrement.
nnoremap + <C-a>|nnoremap - <C-x>
" Jump to exact mark location with ' instead of line.
" noremap ' `|noremap ` '
" zT/zB is like zt/zb, but scrolls to the top/bottom quarter of the screen.
" nnoremap <expr> zT 'zt' . winheight(0)/4 . '<C-y>'
" nnoremap <expr> zB 'zb' . winheight(0)/4 . '<C-e>'

"}}}
syntax on
