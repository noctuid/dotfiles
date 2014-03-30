" Todo:
" .plugin to create index based on structure and title
" .swp files
" I functionality back?
" replace i with s finally?
" get delimit.. to not do "" if right before another word 
" get space to lock until next keypress (can press space, wait however many seconds and then press finish the sequence without it dissappearing)
" fix heading crap
" commentstring in ftplugin; list of added .. pentadactyl.vim
" set up yanking properly in unite

" remap cw and such to ciw
" sound bindings.. and brightness

" Experimental"{{{
" retrain to stop using caps layer in vim
" nnoremap <up> <nop>
" nnoremap <left> <nop>
" nnoremap <right> <nop>
" nnoremap <down> <nop>
" nnoremap <End> <nop>
" nnoremap <Home> <nop>
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
	nnoremap <silent> rmh :silent !~/bin/less_wm/resize_left.sh<cr>
	nnoremap <silent> rmn :silent !~/bin/less_wm/resize_down.sh<cr>
	nnoremap <silent> rme :silent !~/bin/less_wm/resize_up.sh<cr>
	nnoremap <silent> rmi :silent !~/bin/less_wm/resize_right.sh<cr>
	"}}}
	" open urxvt
	nnoremap <silent> ru :silent !urxvt &<cr>

	"}}}

	" s becomes select/Show/settings"{{{
	"select
	nnoremap <silent> sh :!bspc window -f left<cr><esc>
	nnoremap <silent> sn :!bspc window -f down<cr><esc>
	nnoremap <silent> se :!bspc window -f up<cr><esc>
	nnoremap <silent> si :!bspc window -f right<cr><esc>
	nnoremap <silent> sl :silent !bspc window -f last<cr><esc>

	" monocle toggle
	nnoremap <silent> st :silent !bspc desktop -l next<cr>
	nnoremap <silent> ss :!bspc window -t sticky<cr><esc>
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
" more terminal bindings

" fixes complaining about undefined tcomment variable
set runtimepath+=~/.vim/bundle/tcomment_vim
set runtimepath+=~/.vim/colors

" turns off vi bompatibility mode for full vim functionality; set first
set nocompatible

" poor man's dual role I guess: (for people who remap control is remapped to caps or thumb key)
" inoremap <c> Escape

" Dissolve {{{
" Other people's stuff I have stolen:
" Uncomment the following to have Vim jump to the last position when
" reopening a file
" if has("autocmd")
"   au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
" endif

" vimcryption : vim -x file; set cryptmethod=blowfish

" Fix email paragraphs (not sure where i got this from.. deletes newlines?)
" nnoremap <leader>par :%s/^>$//<CR>

" map ;s set invspell spelllang=en<cr
" map ;ss :set spell spelllang=anonomize<cr>
" mkspell ~/dotfiles/common/.vim/spell

" http://stackoverflow.com/questions/9458294/open-url-under-cursor-in-vim-with-browser
" no need with vim wiki for now...
" nmap <leader>g :call Google()<CR>
" fun! Google()
"     let keyword = expand("<cword>")
"     let url = "http://www.google.com/search?q=" . keyword
"     let path = "C:/Program Files/Mozilla Firefox/"
"     exec 'silent !"' . path . 'firefox.exe" ' . url
" endfun

" " Restore cursor position to where it was before
" augroup JumpCursorOnEdit
"    au!
"    autocmd BufReadPost *
"             \ if expand("<afile>:p:h") !=? $TEMP |
"             \   if line("'\"") > 1 && line("'\"") <= line("$") |
"             \     let JumpCursorOnEdit_foo = line("'\"") |
"             \     let b:doopenfold = 1 |
"             \     if (foldlevel(JumpCursorOnEdit_foo) > foldlevel(JumpCursorOnEdit_foo - 1)) |
"             \        let JumpCursorOnEdit_foo = JumpCursorOnEdit_foo - 1 |
"             \        let b:doopenfold = 2 |
"             \     endif |
"             \     exe JumpCursorOnEdit_foo |
"             \   endif |
"             \ endif
"    " Need to postpone using "zv" until after reading the modelines.
"    autocmd BufWinEnter *
"             \ if exists("b:doopenfold") |
"             \   exe "normal zv" |
"             \   if(b:doopenfold > 1) |
"             \       exe  "+".1 |
"             \   endif |
"             \   unlet b:doopenfold |
"             \ endif
" augroup END


"}}}
" #==============================
" # General {{{
" #==============================
" error bells are off by default
" default ; i.e. will show # of lines selected in visual
set showcmd
" (show matching brackets ; default)
set showmatch

" enable utf8
set encoding=utf8
set termencoding=utf-8

" keeps buffer contents in memory
" undo history doesn't go away if change buffers
set hidden

" filetype based indentation.. 
filetype plugin indent on

" automatically use indentation from previous line
set autoindent

" Keep 2000 lines of command line history.
set history=2000                     

" persistent undo history
set undofile " Save undo's after file closes
set undodir=~/.vim/undo,/tmp " where to save undo histories
set undolevels=2000 " How many undos
set undoreload=2000 " number of lines to save for undo

" http://vim.wikia.com/wiki/Word_wrap_without_line_breaks
" wrap lines visually when reach the edge
set wrap 
" wrap at breakat instead of last character that fits on screen ; can't use with list :(
" set linebreak
" set nolist  " list disables linebreak

" disable automatic insertion of newline characters
set textwidth=0
set wrapmargin=0

" display tabs and certain whitespace
set list
" listchars ;show tabs; show at end of line; ☬⚛⚜⚡☥☣
" set listchars=tab:  ,nbsp:☣,eol:¬
" set listchars=tab:\|\ ,nbsp:☣,eol:¬
set listchars=tab:\!\ ,nbsp:☣,eol:¬

" got rid of tab:>- and replaced with escaped space (using indent guides instead now; not any more ; U+2002 unicode whitespace works as well

" return to last edit position when opening files
autocmd BufReadPost *
            \ if line("'\"") > 0 && line("'\"") <= line("$") |
            \ exe "normal! g`\"" |
            \ endif

"relative numbers except current line (rnu)
set number
set relativenumber

" text formatting; get rid of tc; no autowrap of comments or based on textwidth
set formatoptions=rw
" set formatoptions-=tc
" r - insert comment after enter while in insert
" leaving off o which does comment when press o (this was annoying me)

" for example, stops flickering when have up and down mapped to c-o gk and gj in insert
" won't redraw while executing macros, registers, and commands that have not been typed (not default)
set lazyredraw

" (when do things like gg, will move cursor to sol)
set startofline

" at least 5 lines show below and above cursor ; experimenting with cursor position
set scrolloff=5

" Session, saving swap, backup settings"{{{
" to prevent constant annoyance for now:
set noswapfile

" save swap files here; // to avoid collisions (files with same name will be named based on path)
set directory=~/.vim/swap//

" save all changed, titled buffers on focus lost; won't notifiy of errors
" au FocusLost * !silent wa
" same but only the file that has been changed: (error if unnamed.. don't use untitled buffers)
au FocusLost * update

" autowriteall; save buffer if changed when use various commands (switching buffers, quit, exit, etc.)
set awa

"}}}

" Sourcing vimrc"{{{
" Could just combine these..
" from vim wiki; changed from $MYVIMRC; problem because of symlinking
augroup AutoReloadVimRC
  au!
  " automatically reload vimrc when it's saved
  au BufWritePost ~/dotfiles/vim/.vimrc so ~/.vimrc
augroup END

" auto so vimrc on bufread; lack of sucess here...
" works but prevents syntax highlighting from coming on
" augroup AutoReloadVimRC2
"   au!
"   au BufRead * so ~/dotfiles/vim/.vimrc
" augroup END

" source .vimrc on enter; this still slows things down significantly on startup
" autocmd vimenter * source ~/dotfiles/vim/.vimrc
" too slow
" augroup AutoReloadVimRC2
"   au!
"
"   au BufReadPost * so ~/dotfiles/vim/.vimrc
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
inoremap <esc> <esc>:noh<cr>

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
" can also add file specific settings to ~/.vim/after/ftplugin; html.vim or
" python.vim for example; use setlocal instead
" or better yet, use auto commands
" also see http://stackoverflow.com/questions/1889602/multiple-vim-configurations

" Text file settings"{{{
" remove listchars from txt files in favour of better wrapping (not cutting off halfway in between a word)
" usually have many long lines in txt files
autocmd BufEnter *.txt setlocal nolist
autocmd BufEnter *.txt setlocal lbr
"   set showbreak=···\                   " Line break indicator.

" comment graying in text files
autocmd BufEnter *.txt highlight text guifg=gray
autocmd BufEnter *.txt match text /#.*/
" fix zf
" autocmd BufEnter *.txt setlocal comments=:# 
autocmd BufEnter *.txt setlocal commentstring=#\ %s
" augroup textcommentstring
"   au!
"   au BufEnter *.txt setlocal commentstring=#\ %s
" augroup END

" comment my textfiles with octothorpe
call tcomment#DefineType('text',              '# %s'             )
" spell check text files
autocmd BufEnter *.txt set spell

" spelling
" if v:version >= 700
"   " Enable spell check for text files
"   autocmd BufNewFile,BufRead *.txt setlocal spell spelllang=en
" endif

"}}}

call tcomment#DefineType('pentadactyl',              '" %s'             )
autocmd BufNewFile,BufRead *.pentadactylrc,*.penta set filetype=pentadactyl
let g:commentChar = {
	\   'conf': '#'
	\}
"}}}
" #==============================
" # Appearance"{{{
" #==============================
" syntax higlighting.. not working automatically.. put at very end and suddenly working
" syntax on

set modeline
set guicursor=a:blinkon900-blinkoff600  " Slow down cursor blinking speed

" see vivify; favourites: darkrobot (green insert), railscasts, **molokai, etc.
set t_Co=256
colorscheme molokai
set background=dark
set guifont=Inconsolata\ 11

" May be necessary for 256 colors
" set t_AB=^[[48;5;%dm
" set t_AF=^[[38;5;%dm

" Airline theme (I like molokai, wombat, murmur)"{{{
set laststatus=2
"" fav: bubblegum, laederon, murmur, understated, :
" like normal mode of zenburn,  
" my combo theme:
let g:airline_theme='darkfox'
let g:airline#extensions#tabline#enabled = 1

" old vim-powerline symbols
let g:airline_left_sep = '⮀'
" let g:airline_left_alt_sep = '⮁'
let g:airline_right_sep = '⮂'
" let g:airline_right_alt_sep = '⮃'
" below resulting in error on startup
" let g:airline_symbols.branch = '⭠'
" let g:airline_symbols.readonly = '⭤'

" turn off mixed indent, trailing space.. later syntastic
let g:airline_section_warning = ''

" vcs integration
" let g:airline#extensions#branch#enabled = 1
" syntastic
" let g:airline#extensions#syntastic#enabled = 1
"}}}

" terminal like tabs (maybe not as ugly: use -=e):
" A autoyank contents of visual mode to + register
" c use console dialogues instead of popups for simple choices
" also remove menu bar (m), T (toolbar)
set guioptions=e,P
set guioptions-=m,T
" maybe fixes white bar appearing at bottom; seems to work
set guiheadroom=40

" Tab stuff
" limit amount of tabs for vim:
" set tabpagemax=20

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
" set foldmethod=indent
" The vnoremap is good for manual mapping. In your ftplugin, add set foldmethod=indent for Python. Then, in your .vimrc, set foldmethod=manual. Now, you can fold in both Python and non-Python with spacebar mapping
" set foldnestmax=2
" nnoremap <Space> za
" vnoremap <Space> zf
"}}}
" #==============================
" # General Mappings/ Bindings and Settings "{{{
" #==============================
" focus on not using modifiers or hard to reach keys; may add more modifiers in future when remapping progresses for dual and I get more thumb keys
" main leader/prefix keys (in order of ease): t (not worthy of such a great colemak position and I never use), space (about as easy), comma (not at the point where I need another)
" let mapleader = "\<space>"
let mapleader = "t"
" map <space> <leader>
  
" Colemak/Navigation Mappings"{{{
" modified from here:
" https://github.com/bunnyfly/dotfiles/blob/master/vimrc
" http://forum.colemak.com/viewtopic.php?id=1808
" My change: keep i and don't have a dedicated right; continue use s for sneak.. 
" l for 'last' instead of line
" if find want i for right, remap i to s, sneak to f and f to l or get rid of f altogether

" took out l/i and keeping i for insert.. do I ever use right? well I'll stop
noremap n gj|noremap e gk|nnoremap gn j|nnoremap ge k
" In(s)ert. The default s/S is synonymous with cl/cc and is not very useful.
" nnoremap s i|noremap S I
" Last search.
" don't just place in the middle; open any folds too
nnoremap k nzozz|noremap K Nzozz
" BOL/EOL/Join Lines; took out l to ^ in favor of l for <c-o>
" logic.. caps l is set to home which is ^ already; 0 is easy enough and also have I
" nnoremap L $|nnoremap <C-l> J
nnoremap L <c-i>
" l for last
nnoremap l <c-o>
" h for beginning of line
nnoremap h 0

" _r_ = inneR text objects.
" onoremap r i
nnoremap j e|noremap J E

vnoremap n j
vnoremap e k
vnoremap i l

" replace K; H and L are pretty useless
" what of I

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


""}}}

" Other General Mappings"{{{
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;
" Y like D
nnoremap Y y$
" Sane redo.
noremap U <C-r>

nnoremap tw <c-w>c
" better text file long line nav (use with lazy redraw); up and down between wraps
inoremap <Down> <C-o>gj
inoremap <Up> <C-o>gk

nnoremap <leader>k <c-d>|nnoremap <leader>o <c-u>
vnoremap <leader>k <c-d>|vnoremap <leader>o <c-u>

" Better save mapping; still not there yet
inoremap <ctrl>s <esc>:w<cr>
nnoremap <ctrl>s :w<cr>
inoremap § <esc>:w<cr>
" nnoremap § :w<cr>
" try to use this instead
nnoremap <leader>s :w<cr>
" maybe don't do this though:
" inoremap <leader>s <esc>:w<cr>

" control backspace behaviour
inoremap ¸ <c-w>
cnoremap ¸ <c-w>

" I use a more than A (which I almost never use)
nnoremap a A
nnoremap A a

" I probably use V more than v.. going to try this
nnoremap v V|nnoremap V v

" Quickly edit/reload the vimrc file
" nnoremap <silent> <leader>ov :e $MYVIMRC<CR>
nnoremap <leader>. :so ~/.vimrc<CR>

"}}}

" leader table"{{{
" interestingly enough.. can have single letter and multiple letters (ex b and bn after leader ; delay though)
"bc- bundle clean
"bu- ls
"c- tcomment
"d- BD
"D- close window too
"e- next tab
"f- testing
"i- bundle install
"E- next buffer in history
"n- previous tab
"N- previous buffer in history
"p- Unite mru n buffer
"P- unite file
"q- :q
"s source vimcrc
"tn- taboo open
"tr- taboo rename
"tt- tabnew
"u- gundo 
"x- 'xray' view
"y- snipmate


"}}}
"space table
" space arstdhneio; 1-10gt

" comma table
" section table; other symbols; or remap control
" ;
" :/cabbr table
" consider using h as a mode/prefix or other letters don't use much 

" Plugin Specific"{{{
" vundle stuff
nnoremap <leader>bi :BundleInstall<cr>
nnoremap <leader>bc :BundleClean<cr>
" bundle update
nnoremap <leader>bu :BundleInstall!<cr>

" Gundo
nnoremap <leader>u :GundoToggle<CR>
"   let g:gundo_right=1
"   let g:gundo_preview_bottom=0
"   let g:gundo_close_on_revert=1
"   let g:gundo_map_move_older="n"
"   let g:gundo_map_move_newer="e"
"   let g:gundo_width=45
"   let g:gundo_preview_height=10

" tcomment
map <leader>c <C-_><C-_>

" Emmet vim
" map ty <C-y>,

" quickly open session
nnoremap <leader>ss :OpenSession


" session management:
" vim-session options (auto save on exit, open saved on open)
let g:session_autosave_periodic='yes'
let g:session_autosave='yes'
let g:session_autoload='yes'

" Arpeggioinoremap rsi hello
" steno with vim
" Arpeggioinoremap ri <esc>:!bspc window -f right<cr> 

" nnoremap rri <esc>:!bspc window -f right<cr> 


" Godlen View settings"{{{
" " 1. split to tiled windows
" nmap <silent> <C-L>  <Plug>GoldenViewSplit
"
" " 2. quickly switch current window with the main pane
" " and toggle back
" nmap <silent> <F8>   <Plug>GoldenViewSwitchMain
" nmap <silent> <S-F8> <Plug>GoldenViewSwitchToggle
"
" " 3. jump to next and previous window
" nmap <silent> <C-N>  <Plug>GoldenViewNext
" nmap <silent> <C-P>  <Plug>GoldenViewPrevious
"}}}

" Sneak settings
" 1 means use ignorecase or smartcase if set (have smartcase set)
let g:sneak#use_ic_scs = 1
" use streak mode (easy motion highlighting) when > 1 match on screen
let g:sneak#streak = 1
" s to go to next S for previous
let g:sneak#s_next = 1
" g:sneak#textobject_z = 1
" hi link SneakPluginTarget ErrorMsg
hi link SneakStreakTarget ErrorMsg
" hi link SneakStreakMask Comment
" nmap f <Plug>Sneak_s
    " operator-pending-mode
    " omap z <Plug>Sneak_s
    " omap Z <Plug>Sneak_S
" since have all mapped to <c-o>

" colemak chars for sneak mode; take out i for insert; d out to delete
let g:sneak#target_labels = "arsthneowfpluy/ARSTDHNEIOFPLUY"

" for if using s as i
" nnoremap f <plug>SneakForward
" nnoremap F <plug>SneakBackward
" otherwise
nmap f <Plug>SneakForward
nmap F <Plug>SneakBackward

" VimWiki (using like TabsOutliner with pentadactyl)
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
" maybe change this
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
" access to hundreds of files in 2-5 keystrokes; no great memorization required
"see unite
"tabs"{{{
" default tab naming behaviour
let g:taboo_modified_tab_flag='+'
let g:taboo_tab_format=' %N %m%f '
let g:taboo_renamed_tab_format=' %N [%f]%m '

" save taboo names in session
set sessionoptions+=tabpages,globals

nnoremap <leader>r :TabooRename<space>
" nnoremap <leader>tn :TabooOpen<space>

nnoremap <leader>t :tabnew<cr>
nnoremap <leader>q :q<cr>

nnoremap N gT
nnoremap E gt

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
nnoremap <space>w 11gt
nnoremap <space>f 12gt
nnoremap <space>p 13gt
nnoremap <space>l 14gt
"}}}

"buffkill stuff"{{{
" move forward and back in buffer history
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
let g:unite_source_buffer_time_format=''
let g:unite_source_buffer_filename_format=''
" more mru
let g:unite_source_file_mru_long_limit = 3000
" let g:unite_winheight = 10
" call unite#filters#matcher_default#use(['matcher_fuzzy'])
" Todo: 
" get arstdhneo for buffer quick-match ; esc to get out of
" can use with tabs :Unite tab and windows
" let g:unite_enable_use_short_source_names=1
" let g:unite_source_file_rec_max_cache_files = 5000

" colemak
let g:unite_quick_match_table = {
      \     'a' : 0, 'r' : 1, 's' : 2, 't' : 3, 'd' : 4, 'h' : 5, 'n' : 6, 'e' : 7, 'i' : 8, 'o' : 9,
      \     'w' : 10, 'f' : 11, 'p' : 12, 'l' : 13, 'u' : 14, 'y' : 15, 'x' : 16, 'c' : 17, 'v' : 18, 'k' : 19,
      \     '1' : 20, '2' : 21, '3' : 22, '4' : 23, '5' : 24, '6' : 25, '7' : 26, '8' : 27, '9' : 28, '0' : 29,
      \ }

" http://bling.github.io/blog/2013/06/02/unite-dot-vim-the-plugin-you-didnt-know-you-need/
" nnoremap <space>/ :Unite ag:.<cr>

" Open bookmark file for most frequently used files
nnoremap <space><space> :Unite -quick-match bookmark<cr>

" Search open buffers and most recently used
nnoremap <leader>p :Unite -start-insert buffer file_mru<cr>
" Search files (cd first); file_rec/async
nnoremap <leader>P :Unite -start-insert file<cr>
cabbr writ ~/ag-sys/Else/everything/\#Another/ 
cabbr dot ~/dotfiles
cabbr ubmark ~/.unite/bookmark/
cabbr ag cd ~/ag-sys/
cabbr else cd ~/ag-sys/Else/everything/

autocmd FileType unite call s:unite_settings()
function! s:unite_settings()
  " let me exit with escape and move with colemak bindings
  nmap <buffer> <ESC> <Plug>(unite_exit)
  nmap <buffer> n j
  nmap <buffer> e k
endfunction

" nmap cp :let @" = expand("%")<cr>
" can do ctrl r then % in insert mode instead

" Thanks to Ingo Karkat for answering my question ; http://stackoverflow.com/questions/21125170/grabbing-the-current-tab-name
cnoreabbr <expr> tabname t:taboo_tab_name
cnoreabbr <expr> buffername expand('%:t')
" for use in tabs named in taboo:
" have a bookmark file for tab names; organize buffers by group (i.e. tab for config; name with taboo and that name matched to a bookmark file)

" doesn't work without sourcing vimrc (or with autocommand to); trying out sourcing first
nnoremap <buffer> <space>u :so ~/.vimrc<cr>:Unite -quick-match bookmark:tabname<C-]><cr>
nnoremap <buffer> <space>U :UniteBookmarkAdd<cr>tabname<c-]><cr>buffername<c-]>

" cycle to next sub category; c for cycle
" resource to auto change mapping (otherwise won't cycle af first time)
"stop complaining if not named
if exists("t:taboo_tab_name")
	if t:taboo_tab_name == "config"
		nnoremap <buffer> <space>c :TabooRename conf-music<cr>:so ~/.vimrc<cr>
	elseif t:taboo_tab_name == "conf-music"
		nnoremap <buffer> <space>c :TabooRename mail<cr>:so ~/.vimrc<cr>
	elseif t:taboo_tab_name == "conf-mail"
		nnoremap <buffer> <space>c :TabooRename config<cr>:so ~/.vimrc<cr>
	elseif t:taboo_tab_name == "conf-ranger"
		nnoremap <buffer> <space>c :TabooRename remap<cr>:so ~/.vimrc<cr>
		" nnoremap <buffer> <space>u :cd ~/dotfiles<cr>:Unite file<cr>
	endif
endif

" old way of doing things:
" if bufname("%") == "/home/angelic_sedition/dotfiles/common/.mpd/mpd.conf"
" 	nnoremap <buffer> <space>u :Unite bookmark:config
" elseif bufname("%") == "/home/angelic_sedition/dotfiles/terminal/.Xdefaults"
" 	nnoremap <buffer> <space>u :Unite bookmark:config
" else
" 	nnoremap <buffer> <space>u :Unite file
" endif

"}}}

" Split bindings
nnoremap <leader>w <c-w>
nnoremap <leader>/ :vsplit<cr>
nnoremap <leader>- :split<cr>
" already have N and E set to switch between splits
nnoremap <a-n> <c-w><left>
nnoremap <a-e> <c-w><right>
" resize windows (relative.. and dwm.vim is very lacking
nnoremap <leader>h :vertical resize -5<cr>
nnoremap <leader>i :vertical resize +5<cr>
nnoremap <leader>e <c-w><right>
nnoremap <leader>n <c-w><left>
" add swap splits

" move tabs
nnoremap <leader>N :tabm -1<cr>
nnoremap <leader>E :tabm +1<cr>

"'x-ray' view; cuc and cul
nnoremap <leader>x :set cursorcolumn<cr>:set cursorline<cr>
nnoremap <leader>x :set cursorcolumn!<cr>:set cursorline!<cr>

"}}}
" #==============================
"shorthand"{{{
" working on implementing; not a fan of configuring autokey; this is much easier; use vim and penta for 95% of typing.. will probably add to weechat as well
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
" are you okay with = rukw
" around = rnd|ro
" away = ay 
" began = bga|ba
" begin = bgn|bi
" being = bng|bg
" best = bs
" between = bt|tw
" book = bk
" both = bo|bh
" but - bu
" by the way = btw
" call = cl
" came = ca
" can = c
" can't = cn
" change = ch
" children = chd
" city = cy
" close = cs
" come = cm
" could = cd
" country = cty
" different = df
" does = ds
" doing = dg
" done = dn
" don't = d
" down = dw
" each = e|ea
" earth = erh
" easy = es
" eight = 8
" English = en|eng
" enough = nf
" even = ev|vn
" ever = er
" every = ey
" example = x
" family = fml
" father = ftr|fhr
" feet = f3|f8
" find = fd
" finding = fdg
" fine = fn
" first = fs
" five = 5
" follow = fl
" follower = flr
" food = fod|fu
" foot = ft
" for = f
" for example = fx
" forget = fg
" form = fo
" found = fnd|fw
" four = 4
" from = fm
" future = fut
" gave = gv|ga
" get = g
" girl = gl
" give = gi 
" going = gg
" good = gd
" great = g8
" ground = gr
" group = gp
" grow = gw
" hadn't = hdt
" happen = hpn|ha
" happened = hpd
" happening = hpng
" hasn't = hst
" hate = h8
" have = h
" haven't = ht
" having = hg
" head = hd
" hear = h3
" help = hp
" here = hr
" high = hh
" home = hm
" hope = ho
" hoping = hpg
" house = hs
" how = hw
" I'm = m
" image = img
" imagine = mgn|imgn
" important = imp|ip
" Indian = idn
" into = nt|i2
" is it = zt
" it is = tz
" it's = s
" just = j
" keep = kp
" kind = kd
" kind of = kf
" know = kw|kn
" large = lg
" later = lr
" lead = ld
" learn = lrn|l3
" learned = lrd|lrnd
" leave = lev
" left = le
" letter = ler
" life = lf
" light = lt
" like = lk
" line = ln
" list = ls
" little = ll
" live = li
" look = lc
" love = lv
" made = md
" make = mk
" manage = mg
" many = mn
" mean = m3
" might = mt
" mile = mi
" miss = mis
" more = mr (remove full stop from postfixes)
" most = mo
" mother = mot
" mountain = mtn
" move = mv
" much = mc
" must = ms
" name = na
" near = nr
" need = nd
" never = nv
" next = nx
" night = nit
" nine = 9
" number = nm
" often = ofn
" okay = k
" okay so = ks
" okay so now = ksn
" one = 1
" only = ol|oy|nl
" other = ot
" over = ov
" page = pg
" paper = pp
" part = pt
" people = p
" picture = pic
" place = pl
" plant = plt
" play = p3|pla
" please = ps|pz
" point = pn 
" probably = pr|prl|prb
" problem = pb
" question = qn
" quick = qk
" quickly = qkl|ql
" quite = q
" read = rd
" really = ry
" reason = rs
" right = ri|rit|rt
" river = rv
" run = rn
" said = sd
" same = sa
" say = sy
" says = sz
" school = scl
" second = sc
" see = se
" seen = sen
" sentence = stc
" seven = 7
" should = shd
" shouldn't = sht|shn
" show = sh
" side = sid
" sign = s9
" single = sng
" six = 6
" small = sml
" some = sm
" someone = som|smn
" something = sg
" sometimes = st
" soon = sn
" sorry = sry
" sort of = sf
" sound = snd
" speak = spk
" spell = sp|spl
" spelling = spg|splg
" start = sta
" state = stt
" statement = stm
" stay = sty
" steal = stl
" still = sl
" stop = stp
" story = soy|sto
" study = sdy
" such = su
" sure = sr
" take = t8
" talk = tlk
" tell = tl
" than = ta
" thank you = tu|tku
" thanks = tx|thx|tns
" that = tt
" the = t
" their = ter
" them = tm
" then = tn
" there = tr
" these = tes
" they = ty
" thing = tg|thg
" think = tnk|tk
" this = ts
" those = tos
" though = th
" thought = tht
" three = 3
" through = thr
" time = ti
" to be = tb
" together = tog
" took = tok
" tree = t3
" two = 2
" under = ndr|udr
" until = til
" use = z
" very = v
" wait = w8
" walk = wa
" want = wu
" was = o
" watch = wch
" water = wat|wtr|wer
" week = wek
" well = w3
" well done = wld
" went = w9
" were = wr
" we've = wev|wv
" what = wt
" what's = ws
" when = wn
" where = wh
" which = wi
" while = wli|whl
" white = wht
" whole = hl
" why = y
" why did = yd
" will = wl
" with = w
" without = wo
" work = wk
" world = wrl
" would = wd
" yeah = yea
" year = y3
" yes = ye
" you = u
" you'd = ud
" young = yg
" your = ur
" you're = yr
" 
"}}}
" #==============================
"=====================Vundle====================="{{{
filetype off                   " required!

"required
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle; required
Bundle 'gmarik/vundle'

Bundle 'nelstrom/vim-markdown-folding'
" hybrid chording
Bundle 'kana/vim-arpeggio'

" show actual line numbers in insert
Bundle 'myusuf3/numbers.vim'

" for move and sudowrite
Bundle 'tpope/vim-eunuch'
" messing around with
Bundle 'Shougo/vimshell.vim'

" better marks; show and symbols (markers)
Bundle 'kshenoy/vim-signature'
" split 
Bundle 'zhaocai/GoldenView.Vim'
" view undo tree 
Bundle 'sjl/gundo.vim'
" file linking and formatting/highlighting
Bundle 'vimwiki/vimwiki' 
" navigation
" sneak is generally faster or just as fast
Bundle 'justinmk/vim-sneak'
" Bundle 'Lokaltog/vim-easymotion'
" Bundle 'goldfeld/vim-seek'

" allows repeat (.) with plugins like sneak, surround, vim-abolish, etc.
Bundle 'tpope/vim-repeat'
" add newlines with enter
Bundle 'dahu/Insertlessly'
Bundle 'tomtom/tcomment_vim'
Bundle 'bling/vim-airline'
Bundle 'tpope/vim-surround'
Bundle 'vim-scripts/Smart-Tabs'
Bundle 'itchyny/calendar.vim'
Bundle 'xolox/vim-session'
" required by vim-session
Bundle 'xolox/vim-misc'
" tab renaming
Bundle 'gcmt/taboo.vim'
" for buffer history and killing buffers without changing window layout or closing
Bundle 'bufkill.vim'
" html generation
Bundle 'mattn/emmet-vim'
" auto put ( with ), etc with cursor in middle; won't do in commented lines
Bundle 'Raimondi/delimitMate'

" all text boxes vim
Bundle 'ardagnir/pterosaur'

" Shougo
Bundle 'Shougo/neocomplete.vim'
" Use neocomplete.
" let g:neocomplete#enable_at_startup = 1

" tab for insert completion" 
" Bundle 'ervandew/supertab'

" buffer, mru, and file search
" no more fuzzy finder, lusty juggler, minbufexpl, buffer explorer, command t, etc. ; just unite
Bundle 'Shougo/unite.vim'
" for async file search
Bundle 'Shougo/vimproc.vim'
" mru 
Bundle 'Shougo/neomru.vim'

"SnipMate
Bundle "MarcWeber/vim-addon-mw-utils"
Bundle "tomtom/tlib_vim"
Bundle "garbas/vim-snipmate"
" Optional:
Bundle "honza/vim-snippets"

" Dead"{{{
"vim-bufferline pretty much the same thing
" Bundle "buftabs"
" don't need anything complicated and like unite better than yankstack cycling
" Bundle 'maxbrunsfeld/vim-yankstack'
" Bundle  'YankRing'

" getting rid of nerdtree
" Bundle 'scrooloose/nerdtree'
" Bundle 'kien/ctrlp.vim'

" coloured indents; changed mind
" Bundle 'nathanaelkane/vim-indent-guides'
" indentline maybe instead of 

" non-GitHub repos
"fast file opening
"Bundle 'git://git.wincent.com/command-t.git'

" interesting idea ; would like better if could get rid of flashing
" Bundle 'vim-scripts/SearchComplete'
"}}}
" Git repos on your local machine (i.e. when working on your own plugin)
"Bundle 'file:///Users/gmarik/path/to/plugin'
" ...
" NOTE: comments after Bundle commands are not allowed.
"}}}
" #==============================
" # Graveyard"{{{
" #==============================

" Seek"{{{
" like remote stuff with seek, but uninstalled for now because was messing with tcomment
" prevent seek from overriding sneak
" let g:SeekKey = '<Space>'
" let g:SeekBackKey = '<S-Space>'
"
" let g:SeekCutShortKey = '-'
" let g:SeekBackCutShortKey = '+'
"
" let g:seekJumpRemoteInnerKey = 'r'
" let g:seekBackJumpRemoteInnerKey = 'R'
"
" let g:seekJumpPresentialInnerKey = 'p'
" let g:seekBackJumpPresentialInnerKey = 'P'
"
"
" let g:seekJumpPresentialAroundKey = 'o'
" let g:seekBackJumpPresentialAroundKey = 'O'
"
" let g:seekJumpRemoteAroundKey = 'u'
" let g:seekBackJumpPresentialAroundKey = 'U'
"
" " let g:SeekKeys = '<Space> <S-Space> - + <Leader>p <Leader>P' <Leader>r <Leader>R <Leader>o <Leader>O <Leader>u <Leader>U'
" " let g:SeekKeys = '<space> <s-space> '
" " // will not change jump keys.
"
" let g:seek_enable_jumps = 1
" diffs have do and dp ; below will overide
" let g:seek_enable_jumps_in_diff = 1
"}}}
" if reinstall tabbar"{{{
" nnoremap <leader>tb :TbToggle<cr>
" TabBar behaviour
" to put below:
"let g:Tb_SplitBelow=0
" max number of lines for window (0 is as many as needed):
" let g:Tb_MaxSize=2
"TabBar won't auto open (unless x eligible buffersbuffers); seems to have no effect
" let g:TB_MoreThanOne=0
"}}}

" nmap <Leader>bb :ls<CR>:buffer<Space>
" mappings"{{{
" Control s to save (stop using control unless decide to remap control to better location)
" :nmap <c-s> :w<cr>
" :imap <c-s> <esc>:w<cr>a
" :imap <c-s> <Esc><c-s>

"nerd tree"{{{
"autocmd vimenter * NERDTree
"open nerdtree more easily; ? for info
" map <leader>nt :NERDTreeToggle<CR>
" user defined commands must start with a capital letter
" command! NT NERDTreeToggle
" or with an abbreviation
" cabbr nt NERDTreeToggle
"}}}

" CtrlP"{{{
" invoke ctrlp with ctrlp
" map <C-P> :CtrlPMixed<CR>
" map <C-p> :CtrlPBuffer<CR>
" map <leader>p :CtrlPBuffer<cr>

" let g:ctrlp_working_path_mode = 0
" nnoremap <leader>P :CtrlP /run/media/angelic_sedition/MBLWK\ Drive/<cr>
"}}}
" switching between s
" map <C-Tab> :bnext<cr>
" map <C-S-Tab> :bprevious<cr>e
"
" nnoremap <leader>bu :buffers<cr>
" nnoremap <leader>n :bprevious<cr>
" nnoremap <leader>e :bnext<cr>

 "}}}


" " :IndentGuidesToggle
" " toggle with leader ig
" " don't highlight spaces, just tabs
" let g:indent_guides_space_guides = 0
" " auto enable
" let g:indent_guides_enable_on_vim_startup = 1
" " ignore:
" let g:indent_guides_exclude_filetypes = ['help', 'nerdtree']
" " turn off default mapping
" let g:indent_guides_default_mapping = 0
" nnoremap <Leader>ig :IndentGuidesToggle<cr>

"old"{{{
iabbr ## #==========<Space>{{{<cr>#-------------------------------------------------<cr><cr>#}}}<esc>kkkf
" iabbr .# #==========<space>{{{<cr><cr>#}}}<esc>kkf
" iabbr #. #=========="}}}

 " buftabs
"{{{
" BufTabs behaviour
" don't show directories
" let g:buftabs_only_basename=1
" let g:buftabs_separator = " "
" let g:buftabs_marker_start = "["
" let g:buftabs_marker_end = "]"
" let g:buftabs_marker_modified = "!"
" "}}}
" 
" 
" "}}}
 
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

" Heading Crap (still needs revision..)
iabbr @n #==============================<cr>#{{{<cr><esc>I#==============================<cr><esc>I<cr><home>#<esc>kkka

iabbr @e #=================<cr>#{{{<cr><esc>I#=================<cr><esc>I<cr><home>#<esc>kkka

iabbr @i #============{{{<cr><home><cr><home>#<esc>kk 


" title and folding text expansion; switch to snippets
" Main heading "{{{
" iabbr ## #========================================<cr>#<space>{{{<cr>#========================================<cr>}}}<esc>I<esc>kkli
" Alt Main heading
iabbr #. #==============================<cr>#<space>{{{<cr>#==============================<cr><cr>#}}}<esc>I<esc>kkkli
" Sub 1
iabbr 1# #=================<cr>#<space>{{{<cr>#=================<cr><cr>#}}}<esc>k<bs>kA<left><left><left><left>
" Sub 2
iabbr 2# #============<space>{{{<cr><cr>}}}<esc>kkf{i<left>
" Sub 3
iabbr 3# #=======
"Sub 4
iabbr 4# #==
"sub 5: #"}}}
"vim specific
iabbr #v ========================================<cr>{{{<cr>========================================<cr>#}}}<esc>VkkktcjI<esc>li

iabbr #t #==============================<cr>#<space><cr>#==============================<esc>I<esc><cr>V2kzf

syntax on
