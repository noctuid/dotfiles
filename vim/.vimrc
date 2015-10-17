" Todo:
" don't count capitilization errors as spelling errors?
" tab for snippets and indentation and auto-completion?
" visual repeat setup
" octodown instead of livedown
" http://smalltalk.gnu.org/blog/bonzinip/emacs-ifying-vims-autoindent
" instead of autochdir?
" autocmd BufReadPost * if &ft != 'help' | silent! cd %:p:h | endif
" Plugins:
" https://github.com/gelguy/Cmd2.vim
" skybison and vim-swoop
" https://github.com/mhinz/vim-startify
" https://github.com/t9md/vim-transform

" Wishes:
" listchars working with linebreak
" W13 fix
" async linter update; async git singify update
" decent repl integration with new terminal in neovim
" real-time visualization of block editing (not that important)

" Notes:
" see https://gist.github.com/romainl/9ecd7b09a693816997ba
" to check what is setting certain mappings and settings-
" :verbose map
" :verbose set
" Insert prefixes: . and , (as usually followed by a space; can cause problems)
" chord would be better..
" having a ~/.vimrc turns off compatable; :h 'cp'; with -u can use -N

" fixes complaining about undefined tcomment variable
set runtimepath+=~/.vim/bundle/tcomment_vim

" #==============================
" # Experimental {{{
" #==============================
" Arpeggio {{{
" otherwise vim complains
autocmd VimEnter * call s:arpeggio_maps()

" works great with low timeout except during lag (due to neocomplete mostly)
function! s:arpeggio_maps()
	Arpeggio inoremap st <c-w>
	Arpeggio inoremap ie <end>
	Arpeggio inoremap ne <esc>
	Arpeggio inoremap se <cr>
	" Arpeggio inoremap wf <c-r>+
endfunction

let g:arpeggio_timeoutlen=11

" }}}

" better text file long line nav (use with lazy redraw); up and down between wraps
inoremap <Down> <C-o>gj
inoremap <Up> <C-o>gk

" not viable due to vim abbrevs expanding even with letters before them
" and sometimes even when have typed stuff before the abbrev
" letter symbols; short letter abbrevs {{{
" " don't use a, x, y, z, i (for several reasons), or j
" inoreabbr e =
" inoreabbr pe +=
" inoreabbr r &
" inoreabbr rr &&
" inoreabbr c :
" inoreabbr q ?
" " plus, time*, modulo
" inoreabbr p +
" inoreabbr t *
" inoreabbr m %
" " e.g. dollar operator
" inoreabbr d $
" " e.g.type t/ to get ~/
" inoreabbr t ~
" " comparison
" inoreabbr ee ==
" inoreabbr g >
" inoreabbr ge >=
" inoreabbr l <
" inoreabbr le <=

" problems so far:
" if backspace and then type one of these sequences will expand; may be better to use only in program files and not in config files either (because of mappings)
" e.g.
" inoreabbr eg e.g.

" }}}

" make a submode for this...
" nnoremap w zj
" nnoremap b zk

augroup sCustom
	au!
	" move by paragraph (empty line)
	au FileType * nnoremap <buffer> s }|nnoremap <buffer> S {
	" move by sentence
	au FileType doto,text,mkd nmap <buffer> s <Plug>(textobj-sentence-move-n)|nmap <buffer> S <Plug>(texobj-sentence-move-p):call repeat#set("\<Plug>(textobj-sentence-move-p)", v:count)<cr>
augroup END

nmap r <nop>
nnoremap cd r
if has("gui_running")
" WM Experimentation {{{
nnoremap <silent> <space><space> :silent !bspc window -f left && xsendkey p && bspc window -f last<cr>
" "r" is Redraw {{{
" worskpace/Destkop switch {{{
nnoremap <silent> ra :silent !wm_action inpw ra<cr>
nnoremap <silent> rr :silent !wm_action inpw rr<cr>
nnoremap <silent> rs :silent !wm_action inpw rs<cr>
nnoremap <silent> rt :silent !wm_action inpw rt<cr>
nnoremap <silent> rd :silent !wm_action inpw rd<cr>
nnoremap <silent> rh :silent !wm_action inpw rh<cr>
" nnoremap <silent> rn :silent !wm_action dsel 7<cr>
" nnoremap <silent> re :silent !wm_action dsel 8<cr>
nnoremap <silent> rn :silent !wm_action inpw rn<cr>
nnoremap <silent> re :silent !wm_action inpw re<cr>
nnoremap <silent> ri :silent !wm_action inpw ri<cr>
nnoremap <silent> ro :silent !wm_action inpw ro<cr>

" }}}

" move to destkop {{{
nnoremap <silent> Ra :silent !wm_action inpw Ra<cr>
nnoremap <silent> Rr :silent !wm_action inpw Rr<cr>
nnoremap <silent> Rs :silent !wm_action inpw Rs<cr>
nnoremap <silent> Rt :silent !wm_action inpw Rt<cr>
nnoremap <silent> Rd :silent !wm_action inpw Rd<cr>
nnoremap <silent> Rh :silent !wm_action inpw Rh<cr>
nnoremap <silent> Rn :silent !wm_action inpw Rn<cr>
nnoremap <silent> Re :silent !wm_action inpw Re<cr>
nnoremap <silent> Ri :silent !wm_action inpw Ri<cr>
nnoremap <silent> Ro :silent !wm_action inpw 0Ro<cr>

" }}}

" moving windows within desktop {{{
" move to biggest
nnoremap <silent> rcm :silent !wm_action inpw rcm<cr>
" directions
nnoremap <silent> rch :silent !wm_action inpw rch<cr>
nnoremap <silent> rcn :silent !wm_action inpw rcn<cr>
nnoremap <silent> rce :silent !wm_action inpw rce<cr>
nnoremap <silent> rci :silent !wm_action inpw rci<cr>
" circulate
nnoremap <silent> r. :silent !wm_action inpw r.<cr>
nnoremap <silent> r, :silent !wm_action inpw r,<cr>

" }}}

" resize {{{
nnoremap <silent> rmh :silent !wm_action inpw rmh<cr>
nnoremap <silent> rmn :silent !wm_action inpw rmn<cr>
nnoremap <silent> rme :silent !wm_action inpw rme<cr>
nnoremap <silent> rmi :silent !wm_action inpw rmi<cr>

" }}}

" take from old s<keys> bindings {{{
" last window
nnoremap <silent> ru :silent !wm_action inpw ru<cr>
" monocle toggle
nnoremap <silent> rk :silent !wm_action inpw rk<cr>
nnoremap <silent> rf :silent !wm_action inpw rf<cr>
nnoremap <silent> ry :silent !wm_action inpw ry<cr>

" }}}

" last destkop
nnoremap <silent> rl :silent !wm_action inpw rl<cr>
" close window
nnoremap <silent> rx :silent !wm_action inpw rx<cr>

" }}}
" }}}
else
" Tmux Experimentation {{{
" "r" is Redraw {{{
" window switching {{{
nnoremap <silent> ra :silent !wm_action inpt ra<cr>:redraw!<cr>
nnoremap <silent> rr :silent !wm_action inpt rr<cr>:redraw!<cr>
nnoremap <silent> rs :silent !wm_action inpt rs<cr>:redraw!<cr>
nnoremap <silent> rt :silent !wm_action inpt rt<cr>:redraw!<cr>
nnoremap <silent> rd :silent !wm_action inpt rd<cr>:redraw!<cr>
nnoremap <silent> rh :silent !wm_action inpt rh<cr>:redraw!<cr>
nnoremap <silent> rn :silent !wm_action inpt rn<cr>:redraw!<cr>
nnoremap <silent> re :silent !wm_action inpt re<cr>:redraw!<cr>
nnoremap <silent> ri :silent !wm_action inpt ri<cr>:redraw!<cr>
nnoremap <silent> ro :silent !wm_action inpt ro<cr>:redraw!<cr>
" }}}
" resize panes {{{
nnoremap <silent> rmh :silent !wm_action inpt rmh<cr>
nnoremap <silent> rmn :silent !wm_action inpt rmn<cr>
nnoremap <silent> rme :silent !wm_action inpt rme<cr>
nnoremap <silent> rmi :silent !wm_action inpt rmi<cr>
" }}}
" circulate
" previous
nnoremap <silent> r, :silent !wm_action inpt r,<cr>
" next
nnoremap <silent> r. :silent !wm_action inpt r.<cr>

" new window
nnoremap <silent> rc :silent !wm_action inpt rc<cr>:redraw!<cr>
" kill pane
nnoremap <silent> rx :silent !wm_action inpt rx<cr>
" last window
nnoremap <silent> rl :silent !wm_action inpt rl<cr>:redraw!<cr>
" split windows
nnoremap <silent> r/ :silent !wm_action inpt r/<cr>:redraw!<cr>
nnoremap <silent> r- :silent !wm_action inpt r-<cr>:redraw!<cr>

" break pane
nnoremap <silent> r! :silent !wm_action inpt rbang<cr>

" take from old s<keys> bindings {{{
" last pane
nnoremap <silent> ru :silent !wm_action inpt ru<cr>:redraw!<cr>
" zoomed pane toggle
nnoremap <silent> rk :silent !wm_action inpt rk<cr>
" fullscreen
nnoremap <silent> rf :silent !wm_action inpt rf<cr>
" sticky
nnoremap <silent> ry :silent !wm_action inpt ry<cr>:redraw!<cr>
" main-vertical layout
nnoremap <silent> rv :silent !wm_action inpt rv<cr>:redraw!<cr>

" }}}
" }}}
" }}}
endif

" }}}
" #==============================
" # General/ Vim Settings {{{
" #==============================
" General General  {{{
" utf8 as default encoding
set encoding=utf8

" 2000 lines of command line history.
set history=2000

" keep buffer contents in memory (e.g. undo history doesn't go away if change buffers and not saving undo history to disk)
set hidden

" no rodent
set mouse=c

" automatically cd to dir current file is in
set autochdir

" default; :%s/// replaces one; /g replace all; /gg replace one
set nogdefault

" can select past eol in visual block if one line is longer
set virtualedit=block

" default for vim; read "vim: settings:" in files
set modeline

" allow backspacing over autoindent, linebreaks, and start of insert (2)
set backspace=indent,eol,start

" default; when do things like gg, will move cursor to sol
set startofline

" turn off timeout; default is 1000
" fix airline mode changes without screwing up remaps of default letter keys (without this, using escape to exit insert mode won't change airline to display normal mode immediately in the terminal)
" ttimeout only applies to keycodes (so changes to default vim keys that can't be unmapped won't be a problem, e.g. using r in multikey/prefix bindings)
set notimeout ttimeout ttimeoutlen=10

" generally use marker folds
set foldmethod=marker
set foldcolumn=2
" open folds when jumping to marks
set foldopen+=mark

set splitright nosplitbelow

" Text Formatting {{{
" default; disable automatic insertion of newline characters
set textwidth=0 wrapmargin=0

" :h fo-table
" get rid of tc; no auto hardwrap (insert newline) of comments or text based on textwidth
" having textwidth set overrides having tc in formatoptions
" a - automatic formatting of paragraphs (e.g. if <esc> at eol with trailing whitespace, will join the lines)
" j- remove comment char when joining lines
" l- don't break lines in insert mode
" r - insert comment after enter while in insert
" q - allow formatting of comments with gq
" w - trailing whitespace indicates a paragraph continues on the next line 
" o - adds comment char on o when line (above) is commented
set formatoptions=ljrq

" don't add two spaces when joining a line that ends with.,?, or !
set nojoinspaces

" }}}

" return to/restore last edit position when opening files
" http://vim.wikia.com/wiki/Restore_cursor_to_file_position_in_previous_editing_session
augroup resCur
	au!
	au BufReadPost * call setpos(".", getpos("'\""))|normal! zv
augroup END

" }}}

" Backup, Swap, Undo Settings {{{
" // to avoid collisions (files with same name will be named based on path)
" set directory=~/.vim/swap//
" swapfiles are annoying
set noswapfile

" persistent undo history (even if close buffer)
" Save undo's after file closes
set undofile
" where to save undo histories
set undodir=~/.vim/undo//
" How many undos
set undolevels=2000
" save whole buffer for undo on reload if number of lines is smaller than
set undoreload=10001

" backup before overwriting file and keep backup file
set writebackup backup
" backup dir does not have // :(
set backupdir=~/.vim/tmp
set backupcopy=auto
" my important text files are under git; not worried about losing old versions
set backupskip+=*EDITMSG,*.txt

augroup keepSomeBackups
	au!
	" changes backup extension before writing file so old backup isn't deleted if has been a minute
	au BufWritePre * call NewInitBex()
	" delete files older than two days
	" http://askubuntu.com/questions/413529/delete-files-older-than-one-year-on-linux
	" if put .vim/tmp/*, wouldn't match dot files
	au BufWritePost * silent !find ~/.vim/tmp* -mtime +2 -delete &
augroup END

" http://vim.wikia.com/wiki/Keep_incremental_backups_of_edited_files
function! NewInitBex()
	let &bex = '-' . strftime("(%m_%d)-{%H_%M}~")
endfunction

" https://bitbucket.org/sjl/dotfiles/src/tip/vim/vimrc#cl-207
" make folders automatically if they don't already exist
if !isdirectory(expand(&undodir))
	call mkdir(expand(&undodir), "p")
endif
if !isdirectory(expand(&backupdir))
	call mkdir(expand(&backupdir), "p")
endif
" if !isdirectory(expand(&directory))
"     call mkdir(expand(&directory), "p")
" endif

" }}}

" Writing/Saving Settings {{{
" save buffer if changed when using various commands (switching buffers, quit, exit, etc.)
set autowriteall
" don't autoreload a file when changed outside of vim; prompt
set noautoread

" auto save all changed buffers
augroup autoSave
	au!
	au InsertLeave,FocusLost,BufEnter * silent! wa
augroup END

" }}}

" Command Mode {{{
" when tab completing from command line will show
set wildmenu
" https://bitbucket.org/sjl/dotfiles/src/tip/vim/vimrc#cl-153
set wildignore+=.hg,.git,.svn                    " Version control
set wildignore+=*.aux,*.out,*.toc                " LaTeX intermediate files
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg   " binary images
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest " compiled object files
set wildignore+=*.spl                            " compiled spelling word lists
set wildignore+=*.pyc                            " Python byte code
" changes tab behaviour in command line (i.e. list makes every possible choice popup)
set wildmode:full

" }}}

" Searching {{{
" highlight search time
set hlsearch
" fine I'll incsearch
set incsearch
" go back to beginning of buffer once reach end
set wrapscan
" when searching lower case, will match any case
" won't ignore case if search with upper case
set ignorecase smartcase

" }}}

" Auto-Sourcing .vimrc {{{
" https://github.com/bling/vim-airline/issues/539
function! RefreshUI()
  if exists(':AirlineRefresh')
    AirlineRefresh
  else
    " Clear & redraw the screen, then redraw all statuslines.
    redraw!
    redrawstatus!
  endif
endfunction

" from vim wiki
augroup autoReloadVimRC
	au!
	" automatically reload vimrc when it's saved
	au BufWritePost ~/dotfiles/vim/.vimrc so ~/.vimrc | call RefreshUI()
augroup END

" }}}

" }}}
" #==============================
" # Specific Filetype/Indentation Settings {{{
" #==============================
" Indentation
" will use settings in path_to_vim_install/indent/<filetype>.vim
" don't set smartindent or cindent with; can set autoindent though
filetype plugin indent on
" use previous line's indentation with <cr>, o, O
set autoindent

" Notes {{{
" :retab for conversion from tabs to spaces; :retab! for spaces to tabs
" see help and http://vim.wikia.com/wiki/Indenting_source_code
" expandtab   - replace tab with spaces (noexpandtab is default)
" tabstop     - how many columns a tab counts for visually
" softtabstop - how many columns vim uses when you hit Tab in insert mode (default: 0/off); if not set, the tabstop value is used
" shiftwidth  - how many columns text is indented with <<, >>, and == and with automatic indentation
" smarttab    -use shiftwidth instead of softtabtop forwhen determining what to do with <tab> and <bs> at the start of line and insert; it repurposes softtabstop (or tabstop, if softtabstop is 0) to be used for determining number of spaces to insert/delete elsewhere (not at sol)
" also, setting softtabstop to negative values with smarttab seems to mess up the behaviour instead of using the shiftwidth value; none of this is confusing
" http://tedlogan.com/techblog3.html
" "If softtabstop is less than tabstop and expandtab is not set, vim will use a combination of tabs and spaces to make up the desired spacing. If softtabstop equals tabstop and expandtab is not set, vim will always use tabs. When expandtab is set, vim will always use the appropriate number of spaces."
" Example: if tabstop is 8, softtabstop is 4 and noexpandtab and nosmarttab are set,
" the first tab will insert 4 spaces, second will make a tab that is visually 8 columns
" if shiftwidth is also 4, >> from start of line will first create 4 spaces then a tab of visual length 8
" now, if smarttab is set, shiftwidth will be used for <tab> and <bs> at the start of the line, and softtabstop will be used elsewhere

" }}}
set shiftwidth=4 tabstop=4 softtabstop=4
set noexpandtab
set smarttab

" Filetype Specific
" default commentstring (will be used by tcomment)
set commentstring=#\ %s

" pentadactyl
augroup pentadactyl
	au!
	au BufNewFile,BufRead *.pentadactylrc,*penta setlocal filetype=pentadactyl
augroup END

" for vim-dotoo
augroup orgdotoo
	au!
	au BufEnter,BufNewFile,BufRead *.org setlocal filetype=dotoo
augroup END

" See:
" ./.vim/after/ftplugin
" ./.vim/after/syntax

" }}}
" #==============================
" # Encryption Related {{{
" #==============================
" Edit gpg Files {{{
" http://vim.wikia.com/wiki/Edit_gpg_encrypted_files
set backupskip+=*.gpg
augroup gpg_encrypted
	au!
	" Disable swap files, and set binary file format before reading the file
	" To avoid that parts of the file is saved to .viminfo when yanking or
	" deleting, empty the 'viminfo' option.
	au BufReadPre,FileReadPre *.gpg
		\ setlocal noswapfile noundofile noshelltemp history=0 viminfo= bin
	" Decrypt the contents after reading the file, reset binary file format
	" and run any BufReadPost autocmds matching the file name without the .gpg
	" extension
	au BufReadPost,FileReadPost *.gpg
		\ execute "'[,']!gpg --decrypt --default-recipient-self" |
		\ setlocal nobin |
		\ execute "doautocmd BufReadPost " . expand("%:r")
	" Set binary file format and encrypt the contents before writing the file
	au BufWritePre,FileWritePre *.gpg
		\ setlocal bin |
		\ '[,']!gpg --encrypt --default-recipient-self
	" After writing the file, do an :undo to revert the encryption in the
	" buffer, and reset binary file format
	au BufWritePost,FileWritePost *.gpg
		\ silent u |
		\ setlocal nobin
augroup END

" }}}

" Vault Files (password storage) {{{
" have switched primarily to using pass (http://www.passwordstore.org/)
" see vpass alias in ~/.zshrc; using .encrypted_vimrc, not this (no loading of plugins); I guess this is useful if I somehow accidentally open a file in another vim session

" no backup or writebackup for vault files
set backupskip+=*.vault

augroup vaultEncrypted
	au!
	" disable swap files, saving to disk of undo history, writing to disk of commands, and saving thigns to .viminfo
	au BufReadPre,FileReadPre,BufEnter *.vault
		\ setlocal noswapfile cm=blowfish noundofile noshelltemp viminfo= history=0
	" yanking pass
	au BufEnter *.vault nnoremap <buffer> yy yiB
augroup END

" }}}
" }}}
" #==============================
" # Appearance {{{
" #==============================
syntax on

augroup rainbowParens
	au!
	au FileType c,cpp call rainbow#load()
augroup END

" Gvim v. Vim Settings
if has("gui_running")
	" Slow down cursor blinking speed
	set guicursor=a:blinkon1200-blinkoff800
	" ultra anxiety...
	" set guicursor=a:blinkon100-blinkoff50
	colorscheme gruvbox
	" colorscheme molokai
	" colorscheme badwolf
	" set guifont=Inconsolata-dz\ for\ Powerline\ 11
	set guifont=Fira\ Mono\ 9
	" remove menubar (m), toolbar (t), gui tabs (e), and scrollbars
	" c use console dialogues instead of popups for simple choices
	" a automatically puts visually selected text to primary (*)
	set guioptions=ca
	" kind of fixes white bar appearing at bottom; seems to work.. a bit
	" also see ./.gtkrc-2.0.mine
	set guiheadroom=0
else
	colorscheme seoul256
endif

" (G)vim Settings {{{
" default; no screen flashes/beeps
set noerrorbells

"relative numbers except current line (rnu); using numbers vim plugin as well
set number relativenumber

" set titlestring based on file
set title

" default; show mode; show keys in bottom right
set showcmd showmode

" highlight matching brackets, parens, etc.
set showmatch
" Make < and > as well as match pairs. https://github.com/EHartC/dot-vimrc/blob/master/vim%20config/styling.vim
set matchpairs+=<:>

" won't redraw while executing macros, registers, and commands that have not been typed (not default)
" for example, stops flickering when have up and down mapped to c-o gk and gj in insert
set lazyredraw

" don't highlight past certain length; slows down vim on long lines
set synmaxcol=750

" at least 5 lines shown below and above cursor
set scrolloff=5

" filler lines to keep text position; start diff with vertical splits; don't ignore changes in case and whitespace
set diffopt=filler,vertical

" http://vim.wikia.com/wiki/Word_wrap_without_line_breaks
" softwrap lines visually when reach the edge
set wrap

" display tabs and certain whitespace
set list
" ☬⚛⚜⚡☥☣
set listchars=tab:\!\ ,nbsp:☣,eol:¬,extends:❯,precedes:❮

" don't indent wrapped lines
set nobreakindent
" set showbreak=↪\  

" show trailing whitespace in normal mode
" https://bitbucket.org/sjl/dotfiles/src/tip/vim/vimrc#cl-141
augroup trailing
	au!
	" listchars doesn't work with linebreak (which fucking sucks)
	" http://stackoverflow.com/questions/6496778/vim-run-autocmd-on-all-filetypes-except
	let blacklist = ['text', 'tex']
	au InsertEnter * if index(blacklist, &ft) < 0 | set listchars-=trail:⌴
	au InsertLeave * if index(blacklist, &ft) < 0 | set listchars+=trail:⌴
augroup END

" }}}

" Airline theme  {{{
" https://github.com/bling/vim-airline/issues/539
" statusline always present
set laststatus=2

" http://stackoverflow.com/questions/114431/fast-word-count-function-in-vim
" also see http://naperwrimo.org/wiki/index.php?title=Vim_for_Writers
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

au VimEnter * call s:airline_sections_custom()

function! s:airline_sections_custom()
	" add current session name to statusline
	"http://stackoverflow.com/questions/11374047/adding-the-current-session-filename-in-the-statusline add current session name to statusline
	let g:airline_section_x = airline#section#create(['filetype', ' %{fnamemodify(v:this_session, ":t")}'])
	" let g:airline_section_z = airline#section#create(["%{noscrollbar#statusline(20,' ','▓',['▐'],['▌'])}", '%02p%% : %l: %c', ' %{WordCount()}w'])
	let g:airline_section_z = airline#section#create(['%02p%% : %l: %c', ' %{WordCount()}w'])
endfunction

" my custom combination theme:
let g:airline_theme='darkfox'

" powerline symbols
let g:airline_left_sep = ''
" let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
" let g:airline_right_alt_sep = ''

if !exists('g:airline_symbols')
	let g:airline_symbols = {}
endif

let g:airline_powerline_fonts = 1
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''

" turn off mixed indent, trailing space
let g:airline_section_warning = ''

" nrrwrgn integration
let g:airline#extensions#nrrwrgn#enabled = 1
" vcs integration
let g:airline#extensions#branch#enabled = 1
" show summary of changed hunks (gitgutter and vim-signify)
let g:airline#extensions#hunks#enabled = 1
" syntastic integration
let g:airline#extensions#syntastic#enabled = 1
" tabline w/ taboo integration
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#taboo#enabled = 1
" show tab number instead of number of splits
let g:airline#extensions#tabline#tab_nr_type = 1

" }}}

" }}}
" #==============================
" # General Mappings/ Bindings {{{
" #==============================
" _Colemak/Navigation Mappings/Improvements {{{
" make colemak t more useful
let g:mapleader = "t"

" modified from here:
" https://github.com/bunnyfly/dotfiles/blob/master/vimrc
" http://forum.colemak.com/viewtopic.php?id=1808
" My changes:
" .keep i and don't have a dedicated right
" .l for 'last' instead of line

" up/down, including softwraps
noremap n gj|noremap e gk|nnoremap gn j|nnoremap ge k
" get back default gn
nnoremap gk gn
xnoremap gk gn
" keep in visual
xnoremap n j|xnoremap e k

" High/Low/Mid.
" I never use gm or select mode; would rather have go to middle of window not line
noremap gh H|noremap gm M|noremap gl L

" l for last
nnoremap l <c-o>zvzz
nnoremap L <c-i>zvzz

" Keep the cursor in place while joining lines
nnoremap j mzJ`z
" https://bitbucket.org/sjl/dotfiles/src/tip/vim/vimrc#cl-398
" Split line (sister to [J]oin lines)
nnoremap <cr> i<cr><esc>^mwgk:silent! s/\v +$//<cr>:noh<cr>`w

" quickfix nav
augroup quickfix
	au!
	au FileType qf call s:qf_settings()
augroup END

function! s:qf_settings()
	nnoremap <buffer> n j
	nnoremap <buffer> e k
endfunction

" Fold Navigation/Creation {{{
" put comment char at sol (before text) with zf and add space between comment char and marker
" fix this
" xnoremap zf zf$3hr<space>:TComment mode=K<cr>zo]za<space><esc>[zzc

" swapping folds with tommcdo's exchange and kana's fold text object
nmap zE vazXzkvazX
nmap zN vazXzjvazX

" jump
nnoremap ze zk:silent! call repeat#set("zk", v:count)<cr>
nnoremap zn zj:silent! call repeat#set("zj", v:count)<cr>
nnoremap <leader>fe zk:silent! call repeat#set("zk", v:count)<cr>
nnoremap <leader>fn zj:silent! call repeat#set("zj", v:count)<cr>
nnoremap <leader>fu ]z:silent! call repeat#set("]z", v:count)<cr>
nnoremap <leader>fd [z:silent! call repeat#set("[z", v:count)<cr>

" open/close
nnoremap <tab> za
nnoremap <leader>fh zmzz:silent! call repeat#set("zmzz", v:count)<cr>
nnoremap <leader>fi zrzz:silent! call repeat#set("zrzz", v:count)<cr>
nnoremap <leader>fo zRzz:silent! call repeat#set("zRzz", v:count)<cr>
nnoremap <leader>fk zMzz:silent! call repeat#set("ZMzz", v:count)<cr>

" https://bitbucket.org/sjl/dotfiles/src/tip/vim/vimrc#cl-651
" put current line at center and close surrounding folds
nnoremap zl mzzMzvzz`z
" }}}

" Search {{{
" noremap k nzozz|noremap K Nzozz
nmap k <Plug>(Oblique-n)
nmap K <Plug>(Oblique-N)
" get rid of visual mode mappings for n
xmap Qq <Plug>(Oblique-n)
xmap QQ <Plug>(Oblique-N)
" map k to do visual star instead
xmap k <Plug>(Oblique-*)
xmap K <Plug>(Oblique-#)

augroup oblique
	au!
	" center and open enough folds after search, star, and repeat
	au User Oblique       normal! zvzz
	au User ObliqueStar   normal! zvzz
	au User ObliqueRepeat normal! zvzz
augroup END

" }}}

" }}}

" Modified Defaults {{{
" commandline
" I never really need , and ;
nnoremap ; :
xnoremap ; :
nnoremap <leader>; q:i
xnoremap <leader>; q:i
augroup cmdwin
	au!
	" use escape in "normal" to exit cmdwin:
	au CmdwinEnter * nnoremap <buffer> <esc> <c-w>c
	" enter in "normal" to execute command under cursor and re-enter q:
	au CmdwinEnter * nnoremap <buffer> <cr> <cr>q:
augroup END

" using sneak, so unecessary
" this, uncommented, is also annoying with nmaps/plugs (e.g. can't nmap <somekey> <Plug>(something):somecommand<cr>)
" nnoremap : ;

" Y like D
nnoremap Y y$
" http://hashrocket.com/blog/posts/8-great-vim-mappings
" yank paragraph
nnoremap yp yap<S-}>p

" Sane redo.
noremap U <C-r>

" I use a more than A
nnoremap a A
nnoremap A a

" visual block
nnoremap <leader>v <c-v>
xnoremap v V

" because I hate Q; apply macro
nnoremap Q @q
xnoremap Q :norm @q<cr>

" centering an opening folds
nnoremap G Gzvzz

" https://bitbucket.org/sjl/dotfiles/src/tip/vim/vimrc#cl-579
" move to last change (gi moves to last insert point)
nnoremap gI `.a

" go to file with optional line number, open folds, and center
nnoremap gf gFzvzz

" use m for "mode" or filetype specific bindings
" nnoremap M m

" }}}

" Other General Mappings {{{
" jump up and down
nnoremap <leader>k <c-d>zz:silent! call repeat#set("<c-d>zz", v:count)<cr>
nnoremap <leader>o <c-u>zz:silent! call repeat#set("<c-u>zz", v:count)<cr>
xnoremap <leader>k <c-d>zz
xnoremap <leader>o <c-u>zz

" control backspace behaviour
inoremap ¸ <c-w>
cnoremap ¸ <c-w>
nnoremap ¸ daw
inoremap .uu <c-u>

" paste in insert and command mode
inoremap .yp <c-r>+
cnoremap .yp <c-r>+

" source vimrc
nnoremap <leader>. :so ~/.vimrc<cr>
" source current buffer
nnoremap g. :so %<cr>
" source line or selection; https://bitbucket.org/sjl/dotfiles/src/tip/vim/vimrc#cl-405
nnoremap <leader>S ^vg_y:execute @@<cr>:echo 'Sourced line.'<cr>
xnoremap <leader>S y:execute @@<cr>:echo 'Sourced selection.'<cr>

"'x-ray' view; cuc and cul
nnoremap <leader>x :set cursorcolumn!\|set cursorline!<cr>

" open with rifle
function! OpenCurrentLine()
	" https://github.com/gotbletu/shownotes/blob/master/mlocate_vdiscover_vim_locate.txt
	" grab current line
	let line = getline (".")
	" remove leading whitespace
	let line = substitute(line, '^\s', '', "g")
	" add qoutes around the current line to avoid spaces/symbols issues
	let line = substitute(line, '^\(.*\)$', '"\1"', "g")
	" changed to open with rifle (don't use with dirs/text files; use gf that)
	exec "!rifle" line '>&/dev/null &'
endfunction
" bind function to a hotkey
nnoremap <leader>go :call OpenCurrentLine()<CR><CR>

" }}}

" _Clipboard/Yank/Paste Related {{{
" use +/clipboard as default register
set clipboard=unnamedplus

" http://sheerun.net/2014/03/21/how-to-boost-your-vim-productivity/
" move to end after yank or paste; similar to gp but won't go to next line
xnoremap <silent> y y`]
xnoremap <silent> p p`]
nnoremap <silent> p p`]
" since have above, change gp to paste on new line
nnoremap gp :pu<cr>
nnoremap gy V`]

" https://github.com/Shougo/unite.vim/issues/415
let g:unite_source_history_yank_enable = 1
" saves things in clipboard register even if not yanked in vim
let g:unite_source_history_yank_save_clipboard = 1
" don't save yanks to disk
let g:unite_source_history_yank_file=""
" let g:unite_source_history_yank_limit = 300

nnoremap <space>y :Unite history/yank<cr>
" }}}

" }}}
" #==============================
" # Commands/ Shell {{{
" #==============================
" get rid of trailing whitespace; don't remember where got this from
command! Tr normal! mz:%s/\s\+$//<cr>:let @/=''<cr>`z

" copy current file path to clipboard
command! Yp let @+=expand("%:p")

" Shell Specific
nnoremap <a-1> :let @+=expand("%:p")<cr>:!xterm -e "<c-r>+" &<cr>

" mappings for in navigation.vim
command! ViewTxtPdf :!prose2pdf -v % &<cr>
" command! CurrentChapterStats :!

" " }}}
" #==============================
" # Plugin Unmaps {{{
" #==============================
autocmd VimEnter * silent! nunmap <leader>tt
" replace camelcase's ib map
autocmd VimEnter * omap ib <Plug>(textobj-anyblock-i)|xmap ib <Plug>(textobj-anyblock-i)

" }}}
" #==============================
" # Buffer, Window, and Tab Management Mappings and Settings {{{
" #==============================
nnoremap <space>q 1gt
nnoremap <space>w 11gt

" re-setup session/tabs and close other buffers
nnoremap ,ks 1gt:tabonly<cr>:only<cr>:e ~/.vimrc<cr>:vsplit ~/.zshrc<cr>:tabe ~/ag-sys/else/log.org<cr>:tabe ~/ag-sys/else/arch_and_program_info.org<cr>:vsplit ~/ag-sys/else/everything_index.org<cr>:TabooRename main<cr>:tabe ~/ag-sys/else/browse.org<cr>:TabooRename browse<cr>:tabe ~/.navigation.vim<cr>:TabooRename config<cr>:tabe ~/dotfiles/post_install/post_install.sh<cr>:TabooRename bin<cr>:tabe ~/ag-sys/else/remapping.org<cr>:TabooRename remap<cr>:tabe ~/ag-sys/else/consume/book_notes.org<cr>:vsplit ~/ag-sys/else/music/listen/music.org<cr>:TabooRename cons<cr>:tabe ~/ag-sys/else/scrawl/prose/pots/draft_a.org<cr>:vsplit ~/ag-sys/else/scrawl/prose/pots/plot.org<cr>:TabooRename wr<cr>:tabnew<cr>:TabooRename wr<cr>:tabnew<cr>:Wipeout<cr>2gt

" repls; vimshell has some annoyances though (lack of visual updating when window not active, weird cursor blinking, errors, etc.)
nnoremap ,kl :TabooRename prog<cr>:VimShellInteractive --split='vsplit' sbcl<cr>
nnoremap ,kp :TabooRename prog<cr>:VimShellInteractive --split='vsplit' python<cr>
nnoremap ,kh :TabooRename prog<cr>:VimShellInteractive --split='vsplit' ghci<cr>

source ~/.navigation.vim

augroup navigationSourcing
	au!
	au BufEnter * so ~/.navigation.vim
augroup END

" General Quickmarks {{{
nnoremap ,a :e ~/ag-sys/else/arch_and_program_info.org<cr>
nnoremap ,b :e ~/ag-sys/else/browse.org<cr>
nnoremap ,B :e ~/.config/bspwm/bspwmrc<cr>
nnoremap ,c :e ~/.config/ranger/rc.conf<cr>
nnoremap ,d :e ~/ag-sys/else/scrawl/ideas.org<cr>
nnoremap ,E :e ~/.emacs.d/awaken.org<cr>
nnoremap ,g :e ~/.pentadactyl/groups.penta<cr>
nnoremap ,i :e ~/ag-sys/else/interaction.org<cr>
nnoremap ,I :e ~/dotfiles/post_install/post_install.txt<cr>
nnoremap ,j :e ~/ag-sys/else/journal.org<cr>
nnoremap ,l :e ~/ag-sys/else/log.org<cr>
nnoremap ,m :e ~/.muttrc<cr>
nnoremap ,M :e ~/ag-sys/else/music/listen/music.org<cr>
nnoremap ,n :e ~/.navigation.vim<cr>
nnoremap ,p :e ~/.pentadactylrc<cr>
nnoremap ,P :e ~/ag-sys/else/policy.org<cr>
nnoremap ,r :e ~/ag-sys/else/remapping.org<cr>
nnoremap ,R :e ~/.README.md<cr>
nnoremap ,t :e ~/.tmux.conf<cr>
nnoremap ,v :e ~/.vimrc<cr>
nnoremap ,w :e ~/ag-sys/else/workflow.org<cr>
nnoremap ,x :e ~/.xinitrc<cr>
nnoremap ,y :Unite -start-insert buffer file_mru<cr>.yml<esc>
nnoremap ,z :e ~/.zshrc<cr>
nnoremap ,2 :e ~/ag-sys/else/20xx.org<cr>

" }}}

" Tabs {{{
nnoremap <leader>t :tabnew<cr>
" go to last tab
nnoremap <space>l :TWcmd tcm p<cr>
" move tabs
nnoremap <leader>N :tabm -1<cr>
nnoremap <leader>E :tabm +1<cr>

" Taboo {{{
" default tab naming behaviour
let g:taboo_modified_tab_flag='+'
let g:taboo_tab_format='%m%f'
let g:taboo_renamed_tab_format='[%l]%m'

" save taboo names in session
set sessionoptions+=tabpages,globals

nnoremap <leader>r :TabooRename<space>

" }}}
" }}}

" Windows/Splits {{{
nnoremap <leader>q :q<cr>
nnoremap <space>x <c-w>c
nnoremap <leader>' :vsplit<cr>
nnoremap <leader>- :split<cr>
nnoremap <leader>h <c-w><left>
nnoremap <leader>i <c-w><right>
nnoremap N <c-w><down>
nnoremap E <c-w><up>
" vim window maximize (using instead of ZoomWin); still accessible (see border)
nnoremap sm :TWcmd wcm m<cr>

" Wipeout (close buffers not open in windows)
nnoremap <leader>W :Wipeout<cr>

" Moving windows around. (if ever needed)
nnoremap <C-w>N <C-w>J|nnoremap <C-w>E <C-w>K|nnoremap <C-w>I <C-w>L

" vim-eighties
let g:eighties_enabled = 1
let g:eighties_minimum_width = 110
let g:eighties_extra_width = 0 " Increase this if you want some extra room
let g:eighties_compute = 1 " Disable this if you just want the minimum + extra

" https://github.com/talek/obvious-resize

" }}}

" Bufkill {{{
" move forward and back in buffer history (for window)
nnoremap <leader>l :BB<cr>
nnoremap <leader>L :BF<cr>
" delete buffer and leave window open and switch to last used buffer (bufkill)
nnoremap <leader>d :BD<Return>

" }}}

" }}}
" #==============================
" # Operators, Motions, etc. {{{
" #==============================
" more convenient motions
" going to a byte is useless to me
nnoremap <silent> go }:silent! call repeat#set("}", v:count)<cr>
nnoremap <silent> gO {:silent! call repeat#set("{", v:count)<cr>

" camelcase motion as default {{{
map <silent> w <Plug>CamelCaseMotion_w
map <silent> b <Plug>CamelCaseMotion_b
map <silent> j <Plug>CamelCaseMotion_e
sunmap w
sunmap b
sunmap j

" default iw (camelcase's iw deletes spaces like aw and newline chars; ie is closer to default iw)
omap <silent> iw <Plug>CamelCaseMotion_ie
xmap <silent> iw <Plug>CamelCaseMotion_ie

" }}}

" vcs hunks {{{
omap ih <plug>(signify-motion-inner-pending)
xmap ih <plug>(signify-motion-inner-visual)
omap ah <plug>(signify-motion-outer-pending)
xmap ah <plug>(signify-motion-outer-visual)

" }}}

" operator-surround {{{
" using this, not tpope's surround
map <silent>sa <Plug>(operator-surround-append)
map <silent>sd <Plug>(operator-surround-delete)
map <silent>sc <Plug>(operator-surround-replace)

" delete or replace most inner surround
nmap <silent> sdd <Plug>(operator-surround-delete)<Plug>(textobj-anyblock-a)
nmap <silent> scc <Plug>(operator-surround-replace)<Plug>(textobj-anyblock-a)

nmap saw saiW'
nmap sae sa$"
nmap sal sail"

" }}}

" sentence text object {{{
let g:textobj#sentence#abbreviations = [
	\ '[ABCDIMPSUabcdegimpsv]',
	\ 'l[ab]', '[eRr]d', 'Ph', '[Ccp]l', '[Lli]n', '[cn]o',
	\ '[Oe]p', '[DJMSh]r', '[MVv]s', '[CFMPScfpw]t',
	\ 'alt', '[Ee]tc', 'div', 'es[pt]', '[Ll]td', 'min',
	\ '[MD]rs', '[Aa]pt', '[Aa]ve?', '[Ss]tr?',
	\ '[Aa]ssn', '[Bb]lvd', '[Dd]ept', 'incl', 'Inst', 'Prof', 'Univ',
	\ ]

let g:textobj#sentence#select = 's'
let g:textobj#sentence#move_p = '('
let g:textobj#sentence#move_n = ')'
"
augroup textobjSentence
	au!
	au FileType markdown,mkd call textobj#sentence#init()
	au FileType rst call textobj#sentence#init()
	au FileType text call textobj#sentence#init()
augroup END

" }}}

" comment text object {{{
" nice for moving and formatting comments (gqax)
let g:textobj_comment_no_default_key_mappings = 1
xmap ax <Plug>(textobj-comment-a)
omap ax <Plug>(textobj-comment-a)

" }}}

" narrow region {{{
" thanks to ddungtang for pointing me to this plugin:
" http://www.reddit.com/r/vim/comments/298049/question_on_repetitively_making_changes_to_the/
" just to get rid of normal mode mapping the plugin makes by default
nmap <Leader>ZXY <Plug>NrrwrgnDo
" for changing the same areas across multiple lines
" open selected text, delete all of it, map escape to save changes and close win
xmap S <Plug>NrrwrgnDodG;inoremap <buffer> <lt>esc> <lt>esc>:wq<lt>cr><cr>

" }}}

" letters instead of symbols {{{
" paRens
omap ir i(
omap ar a(
xmap ir i(
xmap ar a(

" angle brackets (not noremap because text objectify)
omap ia i<
omap aa a<
xmap ia i<
xmap aa a<

" sqUare
omap iu i[
omap au a[
xmap iu i[
xmap au a[

" kurly
omap ik i{
omap ak a{
xmap ik i{
xmap ak a{

" shorten when possible (e.g. not for word) for i or a
" https://github.com/beloglazov/vim-textobj-quotes
" ',", and `
omap q iq
xmap q iq
omap r i(
xmap r i(
omap u i[
xmap u i[
omap k i{
" k for viual star

" also have anyblock

" }}}

" }}}
" #==============================
" # Plugin Bindings and Settings {{{
" #==============================
" Snippets and Completion {{{ 
" UltiSnips {{{
let g:UltiSnipsExpandTrigger       = ",e"
let g:UltiSnipsJumpForwardTrigger  = ",f"
let g:UltiSnipsJumpBackwardTrigger = ",b"
" let g:UltiSnipsSnippetsDir="~/.vim/UltiSnips"

" }}}

" requires if_lua
if !has('nvim')
" NeoComplete {{{
let g:neocomplete#enable_at_startup = 0
" borrow from bundle bindings for now...
nnoremap <leader>bt :NeoCompleteToggle<cr>
"toggle neocomplete in insert (i.e. for file name completion)
inoremap ,nt <c-o>:NeoCompleteToggle<cr>

" scroll through menu
" inoremap <expr><down> pumvisible() ? "\<C-n>" : "\<down>"
" inoremap <expr><up> pumvisible() ? "\<C-p>" : "\<up>"

augroup tabWithNeocomplete
	" prevent tab binding from being overriden with autocmd
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

" for backwards in completion menus
inoremap <s-Tab> <c-p>

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

augroup neoCompleteSettings
	au!
	au FileType * NeoCompleteLock
	" au FileType java NeoCompleteEnable
	" Enable omni completion.
	au FileType css setlocal omnifunc=csscomplete#CompleteCSS
	au FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
	au FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
	au FileType python setlocal omnifunc=pythoncomplete#Complete
	au FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
augroup END

" }}}
endif

" }}}

" _Unite Related {{{
" http://www.codeography.com/2013/06/17/replacing-all-the-things-with-unite-vim.html
" http://eblundell.com/thoughts/2013/08/15/Vim-CtrlP-behaviour-with-Unite.html
let g:unite_split_rule = "topleft"
" tmi
let g:unite_source_buffer_time_format     = ''
let g:unite_source_buffer_filename_format = ''
" more mru
let g:unite_source_file_mru_long_limit      = 3000
let g:unite_source_file_rec_max_cache_files = 5000

" fuzzy matching; sorting
" call unite#filters#matcher_default#use(['matcher_fuzzy'])
" call unite#filters#sorter_default#use(['sorter_rank'])

" colemak
let g:unite_quick_match_table = {
	\ 'a' : 0, 'r' : 1, 's' : 2, 't' : 3, 'd' : 4, 'h' : 5, 'n' : 6, 'e' : 7, 'i' : 8, 'o' : 9,
	\ 'w' : 10, 'f' : 11, 'p' : 12, 'l' : 13, 'u' : 14, 'y' : 15, 'x' : 16, 'c' : 17, 'v' : 18, 'k' : 19,
	\ '1' : 20, '2' : 21, '3' : 22, '4' : 23, '5' : 24, '6' : 25, '7' : 26, '8' : 27, '9' : 28, '0' : 29,
	\ }

" Search open buffers and most recently used
" only reason for p is because used to use control-p
nnoremap <leader>p :Unite -start-insert buffer file_mru<cr>
" Search files; this is overriden for certain tabs in ~/.navigation.vim
nnoremap <leader>P :cd ~/dotfiles\|Unite -start-insert file_rec/async<cr>
" search with locate; overriden for certain tabs in ~/.navigation.vim
nnoremap <space>p :Unite -start-insert locate<cr>
" source for outline of file (e.g. function jumping)
nnoremap <space>U :Unite -start-insert outline<cr>
" for folds in file (e.g. dotoo/org headings)
nnoremap <space>u :Unite -start-insert fold<cr>

" searching; silver searcher {{{
" http://bling.github.io/blog/2013/06/02/unite-dot-vim-the-plugin-you-didnt-know-you-need/
let g:unite_source_grep_command = "ag"
" ag is recursive; does not need -r
let g:unite_source_grep_recursive_opt = ""
let g:unite_source_grep_default_opts =
	\ '--smart-case --line-numbers --nocolor --nogroup --hidden --ignore ' .
	\  '''.hg'' --ignore ''.svn'' --ignore ''.git'' --ignore ''.bzr'''

" search current buffer wth ag and display results in unite window
" nnoremap <leader>/ :Unite grep:%<cr>
" interactive search (instead of typing search term before; real time results)
nnoremap <leader>/ :Unite line -start-insert<cr>
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

augroup unite
	au!
	au FileType unite call s:unite_settings()
augroup END

function! s:unite_settings()
	" let me exit with escape and move with colemak bindings
	nmap <buffer> <ESC> <Plug>(unite_exit)
	nmap <buffer> n j
	" nmap <buffer> n <Plug>(unite_skip_cursor_down)
	nmap <buffer> e k
	" nmap <buffer> e <Plug>(unite_skip_cursor_up)
	" opening with rifle action
	let rifleAction = { 'is_selectable' : 1 }
	function! rifleAction.func(items)
		for item in a:items
			execute '!rifle ' . item.word
		endfor
	endfunction
	call unite#custom#action('file,cdable', 'rifle', rifleAction)
endfunction

" }}}

" Vimfiler {{{
" use instead of netrw (e.g. with gf or if use vim to open a dir)
" use with vinegar
let g:vimfiler_as_default_explorer = 1

augroup vimfiler
	au!
	au FileType vimfiler call s:vimfiler_mappings()
	au FileType vimfiler NeoCompleteDisable
augroup END

function! s:vimfiler_mappings()
	" colemak
	map <buffer> n <Plug>(vimfiler_loop_cursor_down)
	map <buffer> e <Plug>(vimfiler_loop_cursor_up)
	map <buffer> gn <Plug>(vimfiler_jump_last_child)
	map <buffer> ge <Plug>(vimfiler_jump_first_child)
	map <buffer> i <Plug>(vimfiler_smart_l)
	map <buffer> o <Plug>(vimfiler_edit_file)
	map <buffer> q <Plug>(vimfiler_exit)
	map <buffer> <tab> <Plug>(vimfiler_expand_tree)
	map <buffer> t <leader>
endfunction

" }}}

" _Git Related {{{
" https://bitbucket.org/sjl/dotfiles/src/tip/vim/vimrc#cl-233
" Highlight VCS conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

" Follow Symlinks When Opening a File {{{
" Sources:
"  - https://github.com/tpope/vim-fugitive/issues/147#issuecomment-7572351
"  - http://www.reddit.com/r/vim/comments/yhsn6/is_it_possible_to_work_around_the_symlink_bug/c5w91qw
" this isn't as good as emacs' (setq vc-follow-symlinks "t") because have to use :w! to save after...

function! MyFollowSymlink(...)
	let fname = a:0 ? a:1 : expand('%')
		if getftype(fname) != 'link'
			return
		endif
	let resolvedfile = fnameescape(resolve(fname))
	exec 'file ' . resolvedfile
endfunction
command! FollowSymlink call MyFollowSymlink()

augroup followSym
	au!
	au BufReadPost * call MyFollowSymlink(expand('<afile>'))
augroup END

" }}}

" Fugitive {{{
" git add
nnoremap <leader>ga :Gwrite<cr>
" interactive status
" cc to commit; ca to ammend
nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gc :Gcommit<cr>
nnoremap <leader>gp :Git push<cr>
" for staging parts
nnoremap <leader>gd :Gvdiff<cr>
" nnoremap <leader>gb :Git branch<Space>
nnoremap <leader>go :Git checkout<Space>
nnoremap <leader>gl :Clam git log<cr>gg
" http://mislav.uniqpath.com/2014/02/hidden-documentation/
nnoremap <leader>gB :Gblame<cr>
nnoremap <leader>gb :Gbrowse<cr>
" get previous commit version of buffer (can just undo)
nnoremap <leader>gr :Gread<cr>
nnoremap <leader>gm :Gmove<space>
" this will delete on disk
" nnoremap <leader>gR :Gremove<cr>
" git rm --cached

" https://github.com/PonderingGrower/dotfiles/blob/master/.vimrc
augroup fugitiveSettings
	au!
	" same bindings for merging diffs as in normal mode
	au BufRead fugitive://* call s:diff_bindings()
	au FileType fugitiveblame call s:blame_bindings()
	" bindings for git status window
	au FileType gitcommit nmap <buffer> n <c-n>|nmap <buffer> e <c-p>
augroup END

function! s:blame_bindings()
	nnoremap <buffer> <cr> :Gbrowse<cr>
	nmap p P
endfunction

function! s:diff_bindings()
	xnoremap <buffer> dp :diffput<cr>
	xnoremap <buffer> do :diffget<cr>
	xnoremap <buffer> du :diffupdate<cr>
endfunction

" }}}

" vim-signify settings  {{{
" which vcs and order
let g:signify_vcs_list              = [ 'git', 'hg' ]
let g:signify_disable_by_default    = 0
" eating clipboard again?
" let g:signify_update_on_focusgained = 1
let g:signify_sign_change           = '~'

nmap <leader>gn <plug>(signify-next-hunk):silent! call repeat#set("\<Plug>(signify-next-hunk)", v:count)<cr>
nmap <leader>ge <plug>(signify-prev-hunk):silent! call repeat#set("\<Plug>(signify-prev-hunk)", v:count)<cr>
let g:signify_mapping_toggle_highlight = '<leader>gh'

" }}}

" }}}

" Visual {{{
" vim-signature and vim-bookmarks {{{
" marks are letters, markers are 0-9 (which correspond to shifted symbols)
" not using g:SignatureMap for mn and me, because want repeatable
" ugly, but works
nmap me `[:silent! call repeat#set("`[", v:count)<cr>
nmap mn `]:silent! call repeat#set("`]", v:count)<cr>

" not tied to a letter; probably won't use anotations often or ever
" mm to toggle mark; mi to add anotation; overriding mn
let g:bookmark_sign = '♥'
" center when jumping to a bookmark
let g:bookmark_center = 1
let g:bookmark_no_default_key_mappings = 1
nnoremap m/ :Unite mark vim_bookmarks<cr>

" }}}

" Goyo & Limelight {{{
" toggle goyo
nnoremap <leader>gy :Goyo<cr>
" settings
let g:goyo_width = 120
" limelight
function! GoyoBefore()
	Limelight
endfunction

function! GoyoAfter()
	Limelight!
endfunction

let g:goyo_callbacks = [function('GoyoBefore'), function('GoyoAfter')]

" }}}

" Syntastic {{{
" from bling/dotvim
let g:syntastic_error_symbol = '✗'
let g:syntastic_style_error_symbol = '✠'
let g:syntastic_warning_symbol = '∆'
let g:syntastic_style_warning_symbol = '≈'

" }}}

" }}}

" For Specific FileTypes {{{
" Emmet-vim {{{
imap ,he <Plug>(emmet-expand-abbr)
" just for html and css
let g:user_emmet_install_global = 0

augroup emmet
	au!
	au FileType html,css EmmetInstall
augroup END

" }}}

" Vim Markdown {{{
let g:markdown_fold_style = 'stacked'
" let g:vim_markdown_no_default_key_mappings=1
" let g:vim_markdown_folding_disabled=0

augroup mkdown
	au!
	au FileType mkd call s:mkd_mappings()
augroup END

function! s:mkd_mappings()
	" share leader m with table mode
	nnoremap <leader>mt :Toc<cr>
	" preview
	nnoremap <leader>mp :call LivedownPreview()<CR>
	" experimenting with this; normally just use zn ze but don't really like the way it folds it
	nmap <leader>mn <Plug>(Markdown_MoveToNextHeader):silent! call repeat#set("\<Plug>Markdown_MoveToNextHeader", v:count)<cr>
	nmap <leader>me <Plug>(Markdown_MoveToPreviousHeader):silent! call repeat#set("\<Plug>Markdown_MoveToPreviousHeader", v:count)<cr>
	nmap <leader>mc <Plug>(Markdown_MoveToCurHeader):silent! call repeat#set("\<Plug>Markdown_MoveToCurHeader", v:count)<cr>
	nmap <leader>mu <Plug>(Markdown_MoveToParentHeader):silent! call repeat#set("\<Plug>Markdown_MoveToParentHeader", v:count)<cr>
endfunction

" }}}

" }}}

" 'New' FileTypes (wiki, calendar, org) {{{
" VimWiki (using like TabsOutliner with pentadactyl) {{{
" tww to go to index
" tws select from multiple wikis

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

" Don't automatically make CamelCase words links.
let g:vimwiki_camel_case = 0

augroup vimwiki
	au!
	au FileType vimwiki call s:vimwiki_keys()
augroup END

function! s:vimwiki_keys()
	nmap <buffer> i <Plug>VimwikiFollowLink
	nmap <buffer> h <Plug>VimwikiGoBackLink
endfunction

" }}}

" dotoo {{{
" maybe use with below for quick keys like behaviour
" https://github.com/kana/vim-submode
let g:dotoo#agenda#files = ['~/ag-sys/else/everything/log.org']
" don't clock captures
let g:dotoo#capture#clock = 0

let g:dotoo#parser#todo_keywords = [
	\ 'TODO',
	\ 'NEXT',
	\ 'WAITING',
	\ 'MEETING',
	\ 'DONE']

" default
let g:dotoo#capture#templates = [
	\ ['t', 'Todo', ['* TODO %?',
	\                'DEADLINE: [%(strftime(g:dotoo#time#datetime_format))]']],
	\ ['n', 'Note', '* %? :NOTE:'],
	\ ['m', 'Meeting', '* MEETING with %? :MEETING:'],
	\ ['p', 'Phone call', '* PHONE %? :PHONE:'],
	\ ['h', 'Habit', ['* NEXT %?',
	\                'SCHEDULED: [%(strftime(g:dotoo#time#date_day_format)) +1m]',
	\                ':PROPERTIES:',
	\                ':STYLE: habit',
	\                ':REPEAT_TO_STATE: NEXT',
	\                ':END:']]
	\ ]

" https://github.com/dhruvasagar/vim-dotoo/issues/6
" fix color alloc errors
let g:dotoo_todo_keyword_faces = [
	\ ['TODO', [':foreground #d46a6a', ':weight bold']],
	\ ['NEXT', [':foreground #d49a6a', ':weight bold']],
	\ ['DONE', [':foreground #0d4d4d', ':weight bold']],
	\ ['WAITING', [':foreground #55aa55', ':weight bold']],
	\ ['HOLD', [':foreground #669999', ':weight bold']],
	\ ['CANCELLED', [':foreground #552700', ':weight bold']],
	\ ['MEETING', [':foreground #ffaaaa', ':weight bold']],
	\ ['PHONE', [':foreground #aa6c39', ':weight bold']]
	\ ]

" }}}
" }}}

" Allignment {{{
" Easy Allign {{{
" Start interactive EasyAlign in visual mode (e.g. vip<Enter>)
xmap <leader>a <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. <Leader>aip)
nmap <Leader>a <Plug>(EasyAlign)

" }}}

" Table Mode {{{
let g:table_mode_map_prefix = '<leader>m'
let g:table_mode_toggle_map = 'm'

" }}}

" }}}

" Shell Related {{{
" Vimshell {{{
" Use current directory as vimshell prompt.
let g:vimshell_prompt_expr =
\ 'escape(fnamemodify(getcwd(), ":~").">", "\\[]()?! ")." "'
let g:vimshell_prompt_pattern = '^\%(\f\|\\.\)\+> '
let g:vimshell_popup_command = 'vsplit'

" let g:vimshell_no_default_keymappings=0

augroup vimshell
	au!
	au FileType vimshell call s:vimshell_mappings()
augroup END

function! s:vimshell_mappings()
	" Normal mode key-mappings. {{{
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
	" }}}
	" Visual mode key-mappings. {{{
	" Move to previous prompt.
	xmap <buffer> <leader>e <Plug>(vimshell_select_previous_prompt)
	" Move to next prompt.
	xmap <buffer> <leader>n <Plug>(vimshell_select_next_prompt)
	" }}}
	" Insert mode key-mappings. {{{
	" Execute command.
	inoremap <expr> <SID>(bs-ctrl-])
		\ getline('.')[col('.') - 2] ==# "\<C-]>" ? "\<BS>" : ''
	imap <buffer> <C-]> <C-]><SID>(bs-ctrl-])
	imap <buffer> <CR> <C-]><Plug>(vimshell_enter)

	" inoremap <buffer><expr> <C-p> pumvisible() ? "\<C-p>" :
	" 	\ <SID>start_history_complete()
	" inoremap <buffer><expr> <C-n> pumvisible() ? "\<C-n>" :
	" 	\ <SID>start_history_complete()
	" inoremap <buffer><expr> <Up> pumvisible() ? "\<C-p>" :
	" 	\ <SID>start_history_complete()
	" inoremap <buffer><expr> <Down> pumvisible() ? "\<C-n>" :
	" 	\ <SID>start_history_complete()
	" imap <up> <Plug>(vimshell_history_neocomplete)
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
	" }}}
endfunction

" }}}

" Clam {{{
let g:clam_winpos = 'topleft'

" }}}

" }}}

" Other {{{
" NeoBundle {{{
nnoremap <leader>bi :Unite neobundle/install<cr>
nnoremap <leader>bu :NeoBundleUpdate!<cr>
nnoremap <leader>bs :Unite neobundle/search<cr>
nnoremap <leader>bl :NeoBundleList<cr>

" broswerlink maps <leader>bc ...
augroup ovewriteBLMap
	au!
	au VimEnter * nnoremap <leader>bc :NeoBundleClean<cr>
	au BufWritePost ~/dotfiles/vim/.vimrc nnoremap <leader>bc :NeoBundleClean<cr>
augroup END

" }}}

" Gundo {{{
nnoremap <leader>u :GundoToggle<CR>
let g:gundo_map_move_older = "n"
let g:gundo_map_move_newer = "e"

" }}}

" tcomment {{{
nnoremap <leader>c :TComment<cr>
xnoremap <leader>c :TComment<cr>
nnoremap <leader>C :TCommentBlock<cr>
xnoremap <leader>C :TCommentBlock<cr>

call tcomment#DefineType('text', '# %s' )
call tcomment#DefineType('lisp', ';; %s' )
call tcomment#DefineType('pentadactyl', '" %s' )
" don't use block comments for single lines by default
call tcomment#DefineType('java', '// %s' )
call tcomment#DefineType('c', '// %s' )

" }}}

" vim-session: {{{
" quickly open session
nnoremap <leader>ss :OpenSession
let g:session_autosave_periodic='yes'
let g:session_autosave='yes'
" automatically load  ~/.vim/sessions/default.vim
let g:session_autoload='yes'
" let g:session_persist_globals = ['&sessionoptions']
" call add(g:session_persist_globals, 'g:Taboo_tabs')

" }}}

" Sneak Settings {{{
" 1 means use ignorecase or smartcase if set (have smartcase set)
let g:sneak#use_ic_scs    = 1
" use streak mode (easy motion highlighting) when > 1 match on screen
let g:sneak#streak        = 1
" use sneak key to go to next (and back) (f and F for me)
let g:sneak#s_next        = 1
let g:sneak#textobject_z  = 0
" colemak chars for sneak mode; take out i for insert; d out to delete; take out f for go to next
let g:sneak#target_labels = "arsthneowpluy/ARSTDHNEIOFPLUY"

" mappings
nmap f <Plug>SneakForward
nmap F <Plug>SneakBackward
xmap f <Plug>Sneak_s
xmap F <Plug>Sneak_S
omap f <Plug>Sneak_s
omap F <Plug>Sneak_S

hi link SneakStreakTarget ErrorMsg

" }}}

" Vertigo {{{
" colemak
let g:Vertigo_homerow          = 'arstdhneio'
let g:Vertigo_onedigit_method  = 'smart3'

nnoremap <silent> <leader>n :<C-U>VertigoDown n<CR>
xnoremap <silent> <leader>n :<C-U>VertigoDown v<CR>
onoremap <silent> <leader>n :<C-U>VertigoDown o<CR>

nnoremap <silent> <leader>e :<C-U>VertigoUp n<CR>
xnoremap <silent> <leader>e :<C-U>VertigoUp v<CR>
onoremap <silent> <leader>e :<C-U>VertigoUp o<CR>

" }}}

" _Spell correct <leader>s mappings {{{
" correct current word with first suggestion
nnoremap <leader>z 1z=:silent! call repeat#set(":spellr<cr>", v:count)<cr>

" goto sleep is useless; go to next mispelled
nnoremap gs ]s:silent! call repeat#set("]s", v:count)<cr>
nnoremap ]s ]s:silent! call repeat#set("]s", v:count)<cr>
nnoremap [s [s:silent! call repeat#set("[s", v:count)<cr>

" repeatable correct next or current (if is current word under cursor is mispelled) mispelled word
nnoremap <Plug>CorrectNextMispell T<space>b]s1z=
nnoremap <leader>sc T<space>b]s1z=:silent! call repeat#set("\<Plug>CorrectNextMispell", v:count)<cr>

" add word to spellfile (having a count will add to next spellfile e.g. 2zg)
nnoremap <leader>sa ]szg:silent! call repeat#set("]szg", v:count)<cr>

" mark word a wrong; zw; zuw; not sure if <leader>sm or zw is preferable
nnoremap <leader>sm zw

" lexical
let g:lexical#spelllang = ['en_us','en_ca',]
let g:lexical#thesaurus = ['~/.vim/thesaurus/mthesaur.txt',]
let g:lexical#dictionary = ['/usr/share/dict/words',]

augroup lexical
	au!
	au FileType markdown,mkd call lexical#init()
	au FileType text call lexical#init()
	au FileType rst call lexical#init()
augroup END

" correct with popup (except doesn't suck like z=)
let g:lexical#spell_key = '<leader>ss'
let g:lexical#dictionary_key = '<leader>sk'
inoremap ,sk <c-x><c-k>
let g:lexical#thesaurus_key = '<leader>st'

" vim-online-thesaurus
let g:online_thesaurus_map_keys = 0
nnoremap <leader>sw :OnlineThesaurusCurrentWord<CR>

" }}}

" auto-pairs {{{
" use closing bracket to jump outside even if not next char
let g:AutoPairsFlyMode = 0
let g:AutoPairsShortcutBackInsert = '<M-b>'

" }}}

" }}}

" }}}
" #==============================
" # Neovim Specific {{{
" #==============================
if has('nvim')
" neomake
augroup neomake
	au!
	au BufWritePost,CursorHold *.py,*.sh,*.c Neomake
augroup END

" https://github.com/neovim/neovim/issues/1172#issuecomment-74602464
function! MoveToTabOnLeft()
    let curtab = tabpagenr()
    let tabonleft = curtab - 1
    exe tabonleft."tabnext"
endfunction

augroup tabonleft
    au!
    au TabClosed * call MoveToTabOnLeft()
augroup END

endif

" }}}
" #==============================
" # NeoBundle {{{
" #==============================
" NeoBundle Automatic Installation {{{
if !isdirectory(expand("$HOME/.vim/bundle/neobundle.vim"))
	!curl https://raw.githubusercontent.com/Shougo/neobundle.vim/master/bin/install.sh > install.sh && echo "run the install.sh script to install neobundle"
endif

" }}}

" Required:
set runtimepath+=~/.vim/bundle/neobundle.vim/

" Required:
call neobundle#begin(expand('~/.vim/bundle/'))

" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

" Plugins {{{
" Constantly Active/ Visual {{{
" show non-relative line numbers in insert
NeoBundle 'myusuf3/numbers.vim'

" mark navigation; show marks/markers in gutter
NeoBundle 'kshenoy/vim-signature'
" bookmarks
NeoBundle 'MattesGroeger/vim-bookmarks'

" vcs info
 NeoBundle 'mhinz/vim-signify'

" tab renaming
NeoBundle 'gcmt/taboo.vim'

" automatic split resizing
NeoBundle 'justincampbell/vim-eighties'

" statusline
NeoBundle 'bling/vim-airline'

if has('nvim')
	" async for neovim
	NeoBundle 'benekastah/neomake'
	NeoBundleLazy 'scrooloose/syntastic'
else
	" linter
	NeoBundle 'scrooloose/syntastic'
	" can't use async so no reason to be able to source
	NeoBundleFetch 'benekastah/neomake'
endif

" substitute preview (half of emacs functionality)
NeoBundle "osyo-manga/vim-over"

" rainbow parens
NeoBundle 'oblitum/rainbow'
" NeoBundle 'amdt/vim-niji'

" distraction free writing
NeoBundle 'junegunn/goyo.vim'
" try distraction free https://github.com/DanielFGray/DistractionFree.vim
NeoBundle 'junegunn/limelight.vim'

" colorschemes
NeoBundle 'morhetz/gruvbox'
NeoBundle 'romainl/Apprentice'
NeoBundle 'junegunn/seoul256.vim'
NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'sjl/badwolf'

NeoBundle 'nice/sweater'
NeoBundle 'whatyouhide/vim-gotham'
NeoBundle 'shawncplus/skittles_berry"'
NeoBundle 'idbrii/vim-sandydune'

" }}}

" Unite Related {{{
" https://github.com/Shougo/unite.vim/wiki/unite-plugins
NeoBundle 'Shougo/unite.vim'

" for async file search
NeoBundle 'Shougo/vimproc.vim', {
\ 'build' : {
\     'windows' : 'tools\\update-dll-mingw',
\     'cygwin' : 'make -f make_cygwin.mak',
\     'mac' : 'make -f make_mac.mak',
\     'linux' : 'make',
\     'unix' : 'gmake',
\    },
\ }

" extra sources:
" mru
NeoBundle 'Shougo/neomru.vim'
" ahead of https://github.com/h1mesuke/unite-outline
" opens unite with functions as lines
NeoBundle 'Shougo/unite-outline'
" unite source for marks (also see vim-bookmarks which gives vim_bookmarks)
NeoBundle 'tacroe/unite-mark'
" locate
NeoBundle 'ujihisa/unite-locate'
" for fold text/ navigation
NeoBundle 'osyo-manga/unite-fold'

" }}}

" Case/Filetype Specific {{{
" git
NeoBundle 'tpope/vim-fugitive'

" html generation
NeoBundle 'mattn/emmet-vim'
" reload html page displayed in browser when changes
NeoBundle 'jaxbot/browserlink.vim'

" markdown folding, syntax highlighting, and navigation
NeoBundle 'plasticboy/vim-markdown'
" NeoBundle 'nelstrom/vim-markdown-folding'
" markdown preview
NeoBundle 'shime/vim-livedown'

" haskell
NeoBundle 'kana/vim-filetype-haskell'
" NeoBundle 'ag/vim2hs'
NeoBundle "Twinside/vim-hoogle"

" rust highlighting and indentation
NeoBundle 'wting/rust.vim'

" repl interaction with tmux
NeoBundle 'benmills/vimux'

" for move and sudowrite commands
NeoBundle 'tpope/vim-eunuch'

" view undo tree
NeoBundle 'sjl/gundo.vim'

" password syntax highlighting/hiding stuff
NeoBundle 'aaronbieber/vim-vault'

" wikis
NeoBundle 'vimwiki/vimwiki'

" openscad syntax highlighting
NeoBundle 'torrancew/vim-openscad'

" }}}

" Writing {{{
" smarter sentence text object (kana's text object user is a dependency)
NeoBundle 'reedes/vim-textobj-sentence'
NeoBundle 'reedes/vim-lexical'
NeoBundle 'beloglazov/vim-online-thesaurus'
NeoBundle 'reedes/vim-textobj-quote'

" Rst
NeoBundle 'Rykka/riv.vim'
NeoBundle 'Rykka/InstantRst'

" }}}

" Other {{{
" compiling/ running code
NeoBundle 'tpope/vim-dispatch'
NeoBundle 'JarrodCTaylor/vim-shell-executor'

" shell
NeoBundle 'Shougo/vimshell.vim'
NeoBundle 'sjl/clam.vim'

" easymotion pretty much does everything now, but for simplicity using seak
" sneak: a 2 letter multiline find
NeoBundle 'justinmk/vim-sneak'

" like better than other commenters by far
NeoBundle 'tomtom/tcomment_vim'
" NeoBundle 'tpope/vim-commentary'
" NeoBundle 'scrooloose/nerdcommenter'

" session management
NeoBundle 'xolox/vim-session'
NeoBundle 'xolox/vim-misc'
" NeoBundle 'tpope/vim-obsession'
" NeoBundle 'manuel-colmenero/vim-simple-session'

" for buffer history and killing buffers without changing window layout or closing
NeoBundle 'bufkill.vim'
" alternatively: https://github.com/Soares/butane.vim
" close all buffers not open in windows 
NeoBundle 'wipeout'

" using for tab movement history
NeoBundle 'yssl/twcmd.vim'

" allignment
NeoBundle 'junegunn/vim-easy-align'
" NeoBundle 'godlygeek/tabular'
NeoBundle 'dhruvasagar/vim-table-mode'

" home row line jumping
NeoBundle 'prendradjaja/vim-vertigo'

" completion and snippets
if has('nvim')
	" no lua support currently
	NeoBundleFetch 'Shougo/neocomplete.vim'
else
	NeoBundle 'Shougo/neocomplete.vim'
endif
NeoBundle 'SirVer/ultisnips'
" Optional
NeoBundle "honza/vim-snippets"

" smart auto closing parens, quotes, etc.
NeoBundle 'jiangmiao/auto-pairs'
" NeoBundle 'kana/vim-smartinput'

" automatic ending in vimscript (endfunction), bash, etc.
NeoBundle 'tpope/vim-endwise'

" allows repeat (.) with plugin mappings
NeoBundle 'tpope/vim-repeat'
NeoBundle 'vim-scripts/visualrepeat'

" neither of these are usable for me
" NeoBundle 'terryma/vim-multiple-cursors'
" NeoBundle 'paradigm/vim-multicursor"'

" }}}

" Experimenting {{{
NeoBundle 'tpope/vim-vinegar'

" chording
NeoBundle 'kana/vim-arpeggio'

" file manager
NeoBundle "Shougo/vimfiler.vim"

" all text boxes vim
NeoBundle 'ardagnir/vimbed'


" improved "/" search
NeoBundle 'junegunn/vim-oblique'
" required for above
NeoBundle 'junegunn/vim-pseudocl'

" latex
" NeoBundle "https://github.com/vim-scripts/TeX-9"
"
" japanese input; mapping something up in search...
" NeoBundle "https://github.com/tyru/eskk.vim"

" org mode
NeoBundle 'dhruvasagar/vim-dotoo'

" }}}

" Operators and Text Objects {{{
" required for other stuff
NeoBundle 'kana/vim-textobj-user'
NeoBundle 'kana/vim-operator-user'

" camelcase motion
NeoBundle 'bkad/CamelCaseMotion' 

" cx operator to exchange; X for visual
NeoBundle 'tommcdo/vim-exchange'

" edit selected region in separate window
NeoBundle 'chrisbra/NrrwRgn'

" if do something like i{ seek for {} if outside of like vim already does with i'
" make i' work for multiple lines like i{ would
NeoBundle 'paradigm/TextObjectify'

" adds ic, iC, ac, aC text objects; great for quickly working with columns/visual blocks
NeoBundle 'coderifous/textobj-word-column.vim'

" adds text object io and ao for indentation whitespace
NeoBundle 'glts/vim-textobj-indblock'

" text objects for comments
NeoBundle 'glts/vim-textobj-comment'

" iq and aq for ', ", and `
" NeoBundle 'beloglazov/vim-textobj-quotes'

" haven't found to work well
"https://github.com/lucapette/vim-textobj-underscore

" punctuation text objects (also gives iq and aq);i.e. gives %; i<space> will work for any
NeoBundle 'kurkale6ka/vim-pairs'

" using this instead because works well with text objects (i.e. ic) and makes more sense than doing something like ysiW:
" gives sa{any text object} (surround append), sd, and sc
" see http://www.reddit.com/r/vim/comments/2qozgi/vimoperatorsurround_operator_to_surround_a_text/cn8xcbf
NeoBundle 'rhysd/vim-operator-surround'
" better for html tag features:
" NeoBundle 'tpope/vim-surround'

" match closest '', "", (), {}, [] or <> with ib and ab
NeoBundle 'rhysd/vim-textobj-anyblock'

" gives il for lines (like ^yg_)
NeoBundle 'kana/vim-textobj-line'

" gives ae and ie for entire buffer
NeoBundle 'kana/vim-textobj-entire'

" give az and iz for folds
NeoBundle 'kana/vim-textobj-fold'

" gives things like a= for after equal sign
NeoBundle 'junegunn/vim-after-object'

" }}}

" future..? {{{
" https://github.com/AndrewRadev/splitjoin.vim
" https://noahfrederick.com/log/vim-and-progressive-enhancement/
" nnoremap <silent> J :<C-u>call <SID>try('SplitjoinJoin',  'J')<CR>
" nnoremap <silent> S :<C-u>call <SID>try('SplitjoinSplit', "r\015")<CR>
" function! s:try(cmd, default)
"   if exists(':' . a:cmd) && !v:count
"     let tick = b:changedtick
"     execute a:cmd
"     if tick == b:changedtick
"       execute join(['normal!', a:default])
"     endif
"   else
"     execute join(['normal! ', v:count, a:default], '')
"   endif
" endfunction

" }}}

" }}}

call neobundle#end()
" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck

" }}}
" #==============================
