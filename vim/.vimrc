" Todo:
" .everything in TO
" .plugin to create index based on structure and title
" .stop using gui tabs
" .swp files

" fixes complaining about undefined tcomment variable
set runtimepath+=~/.vim/bundle/tcomment_vim
set runtimepath+=~/.vim/colors

" turns off vi bompatibility mode for full vim functionality; set first
set nocompatible

" poor man's dual role I guess (where control is remapped to caps or thumb key)
" inoremap <c> Escape

" dissolve"{{{
" Uncomment the following to have Vim jump to the last position when
" reopening a file
" if has("autocmd")
"   au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
" endif

" vimcryption : vim -x file; set cryptmethod=blowfish

" Fix email paragraphs (I have no idea what this isfor)
" nnoremap <leader>par :%s/^>$//<CR>

" map ;s set invspell spelllang=en<cr
" map ;ss :set spell spelllang=anonomize<cr>
" mkspell ~/dotfiles/common/.vim/spell

inoremap <Down> <C-o>gj
inoremap <Up> <C-o>gk

" http://stackoverflow.com/questions/9458294/open-url-under-cursor-in-vim-with-browser
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
" default:
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

set history=2000                     " Keep 1000 lines of command line history.

" Session, saving swap, backup settings"{{{
" to prevent constant annoyance for now:
set noswapfile

" save swap files here; // to avoid collisions (files with same name will be named based on path)
set directory=~/.vim/swap//

" save all changed, titled buffers on focus lost; won't notifiy of errors
" au FocusLost * !silent wa
" same but only the file that has been changed:
au FocusLost * update

" autowriteall; save buffer if changed when use various commands (switching buffers, quit, exit, etc.)
set awa

"}}}

" Sourcing vimrc"{{{

" from vim wiki; changed from $MYVIMRC; problem because of symlinking
augroup AutoReloadVimRC
  au!
  " automatically reload vimrc when it's saved
  au BufWritePost ~/dotfiles/vim/.vimrc so ~/.vimrc
augroup END

" auto so vimrc on bufread; lack of sucess here...
augroup AutoReloadVimRC2
  au!
  au BufRead so ~/dotfiles/vim/.vimrc
augroup END

" source .vimrc on enter
autocmd vimenter * source ~/dotfiles/vim/.vimrc

"}}}

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
" r - insert comment after enter while in insert
" leaving off o which does comment when press o (this was annoying me)

" for example, stops flickering when have up and down mapped to c-o gk and gj in insert
" won't redraw while executing macros, registers, and commands that have not been typed (not default)
set lazyredraw

" Command mode"{{{
" when tab completing from command line will show
set wildmenu

" changes tab behaviour in command line (i.e. list makes every possible choice popup:)
set wildmode:full
"}}}

" (when do things like gg, will move cursor to sol)
set startofline

" at least 5 lines show below and above cursor ; experimenting with cursor position
set scrolloff=5

" Searching"{{{
" stop highlighting on escape
set hlsearch
nnoremap <esc> :noh<cr><esc>
inoremap <esc> <esc>:noh<cr>

" don't move when search
set noincsearch
" go back to beginning
set wrapscan 

" when searching lower case, will match any case
set ignorecase
" won't ignore case if search with upper case
set smartcase
"}}}

" persistent undo history
set undofile " Save undo's after file closes
set undodir=~/.vim/undo,/tmp " where to save undo histories
set undolevels=2000 " How many undos
set undoreload=2000 " number of lines to save for undo

" http://vim.wikia.com/wiki/Word_wrap_without_line_breaks
" wrap lines visually when reach the edge
set wrap " wrap at breakat instead of last character that fits on screen ; can't use with list :(
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
autocmd BufEnter *.txt set nolist
autocmd BufEnter *.txt set lbr
"   set showbreak=···\                   " Line break indicator.

" comment graying in text files
highlight text guifg=gray
autocmd BufEnter *.txt match text /#.*/

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

" terminal like tabs (not as ugly: use -=e):
" A autoyank contents of visual mode to + register
" c use console dialogues instead of popups for simple choices
" also remove menu bar (m), T (toolbar)
set guioptions=e,P
set guioptions-=m,T
" maybe fixes white bar appearing at bottom; maybe seems to?
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
" # Mappings/ Bindings (with accompanying plugin settings)"{{{
" #==============================
" use ne en as mapps; ee
" easy motion maybe be useful for a single key instead of 3 to start
  
nnoremap dy db

" colemak/nav mappings modified from here:"{{{
" https://github.com/bunnyfly/dotfiles/blob/master/vimrc
" http://forum.colemak.com/viewtopic.php?id=1808
" My change: keep i and don't have a dedicated right; continue use s for sneak.. 
" l for 'last' instead of line
" if find want i for right, remap sneak to f and f to l

" HNEI arrows. Swap 'gn'/'ge' and 'n'/'e'.
" took out l/i and keeping i for insert.. do I ever use right? well I'll stop
nnoremap n gj|nnoremap e gk|nnoremap gn j|nnoremap ge k
" In(s)ert. The default s/S is synonymous with cl/cc and is not very useful.
" nnoremap s i|noremap S I
" Last search.
" don't just place in the middle; open any folds too
nnoremap k nzozz|noremap K Nzozz
" BOL/EOL/Join Lines; took out l to ^ in favor of l for <c-o>
" logic.. caps l is set to home which is ^ already
" nnoremap L $|nnoremap <C-l> J
nnoremap L <c-i>
" l for last
nnoremap l <c-o>
" if start using i for l, change l to f instead of above and uncomment below
" _r_ = inneR text objects.
" onoremap r i
" EOW.
" TODO: I never use this. Use for something else?
nnoremap j e|noremap J E

vnoremap n j
vnoremap e k

" replace K; H and L are pretty useless
" what of I

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Other Colemak Arrow-Based Mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Switch panes... eh lose I; maybe change
  noremap H <C-w>h|noremap I <C-w>l|noremap N <C-w>j|noremap E <C-w>k
" Moving windows around.
  noremap <C-w>N <C-w>J|noremap <C-w>E <C-w>K|noremap <C-w>I <C-w>L
" High/Low/Mid.
  noremap <C-e> H|noremap <C-n> L|noremap <C-m> M
" Scroll up/down.
" noremap zn <C-y>|noremap ze <C-e>

" +/- increment and decrement.
nnoremap + <C-a>|nnoremap - <C-x>
" Jump to exact mark location with ' instead of line.
" noremap ' `|noremap ` '
" zT/zB is like zt/zb, but scrolls to the top/bottom quarter of the screen.
nnoremap <expr> zT 'zt' . winheight(0)/4 . '<C-y>'
nnoremap <expr> zB 'zb' . winheight(0)/4 . '<C-e>'


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" " Diff
"   nnoremap <silent> <Leader>dt :diffthis<CR>
"   nnoremap <silent> <Leader>do :diffoff<CR>
"   nnoremap <silent> <Leader>dd :call DiffToggle()<CR>
"     function! DiffToggle()
"       if &diff
"         diffoff
" 
"       else
"         diffthis
"       endif
"     :endfunction
" 
" 

" """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" " GUndo
" """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"   let g:gundo_right=1
"   let g:gundo_preview_bottom=0
"   let g:gundo_close_on_revert=1
"   let g:gundo_map_move_older="n"
"   let g:gundo_map_move_newer="e"
"   let g:gundo_width=45
"   let g:gundo_preview_height=10


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" VikWiki
"
" To mirror VimWiki from Dropbox folder: ln -s ~/Dropbox/vimwiki ~/.vimwiki
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  let g:vimwiki_list = [{
    \    'path':                       '~/.vimwiki/',
    \    'path_html':                  '~/.vimwikihtml/',
    \    'maxhi':                      1,
    \    'css_name':                   'style.css',
    \    'auto_export':                0,
    \    'diary_index':                'diary',
    \    'diary_link_fmt':             '%Y-%m-%d',
    \    'diary_link_count':           4,
    \    'diary_header':               'Diary',
    \    'diary_rel_path':             'diary/',
    \    'nested_syntaxes':            {},
    \    'html_header':                '',
    \    'html_footer':                '',
    \    'syntax':                     'default',
    \    'index':                      'index',
    \    'ext':                        '.wiki',
    \    'temp':                       0
    \    }]
  let g:vimwiki_camel_case = 0                   " Don't automatically make CamelCase words links.
  noremap <C-S-M-q> @<Plug>VimwikiNextLink       " Avoid <Tab> jumping to next link.
"}}}

" General Mappings
nnoremap ; :
nnoremap : ;
" Y like D
nnoremap Y y$
" Sane redo.
noremap U <C-r>
" +/- increment and decrement.

" Better save mapping; still not there yet
inoremap <ctrl>s <esc>:w<cr>
nnoremap <ctrl>s :w<cr>
inoremap § <esc>:w<cr>
nnoremap § :w<cr>

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
"E- next tab in history
"n- previous tab
"N- previous tab in history
"nt- nerdtree
"p- ctrlpbuffer
"P- ctrlp file with usb as directory
"q- :q
"ov open vimrc
"s source vimcrc
"tb- toggle tabbar
"tn- taboo open
"tr- taboo rename
"tt- tabnew
"u- unite
"x- 'xray' view
"y- snipmate


"}}}
"space tabble

" comma table
" section table; other symbols; or remap control
" ;
" :/cabbr table
" consider using h as a mode/prefix or other letters don't use much 


" vundle stuff
nnoremap <leader>i :BundleInstall<cr>
nnoremap <leader>bc :BundleClean<cr>

" tcomment
map <leader>c <C-_><C-_>
" fix .conf commenting (txt not working)
let g:commentChar = {
	\   'conf': '#',
	\   'pentadactylrc': '"'
	\}

" Emmet vim
" map <leader>y <C-y>,

" quickly open session
nnoremap <leader>ss :OpenSession

" suboptimal to snippets.. but I want to do in vimrc
" delimitmate

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
iabbr #v ========================================<cr>{{{<cr>========================================<cr>#}}}<esc>Vkkk<leader>cjI<esc>li

iabbr #t #==============================<cr>#<space><cr>#==============================<esc>I<esc><cr>V2kzf


" about = ab
" above = abo|av
" actual = ac
" after = af
" again = ag 
" all = l
" almost = lm
" along = al
" also = ao 
" always = aw
" America = amc
" and = n 
" animal = ani|ai
" another = anr|ae
" answer = ans|ar
" any = ne
" anyway = nw
" are = r
" area = aa
" are you = ru
" are you okay with = rukw
" around = rnd|ro
" away = ay 
" be = b 
" because = bc
" been = bn 
" before = bf
" began = bga|ba
" begin = bgn|bi
" being = bng|bg
" below = bl 
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

" copying with putty just actual text:  :set nonu nolist foldcolumn=0
"
" stick with this?
" inoremap <c-v> <esc>:set paste<cr>"+p<esc>:set nopaste<cr>
" inoremap  <c-v> <esc>"+p
" nnoremap <space>v "+p



" Sneak settings
" 1 means use ignorecase or smartcase if set (have smartcase set)
let g:sneak#use_ic_scs = 1
let g:sneak#streak = 1
" for if using s as i
" nnoremap f <plug>SneakForward
" nnoremap F <plug>SneakBackward
" otherwise
nmap s <Plug>SneakForward
nmap S <Plug>SneakBackward

"==============================
" Buffer and Tab Management Mappings and Settings"{{{
"==============================
" Unite Mappings"{{{
" add it so that can check what is open from bookmarks

" use + as default register.. no more different pasting.. still have yank history with unite
set clipboard=unnamedplus

" use unite as clipboard manager
" thanks to shougo for such a versatile and useful plugin; https://github.com/Shougo/unite.vim/issues/415
let g:unite_source_history_yank_enable = 1
let g:unite_source_history_yank_save_clipboard = 1
let g:unite_source_history_yank_limit = 300
" nnoremap <space>y :Unite register history/yank<cr>
nnoremap <space>y :Unite history/yank<cr>

 nnoremap <space><space> :Unite -quick-match bookmark<cr>

nnoremap <leader>p :Unite -start-insert buffer file_mru<cr>
" nnoremap <space>u :Unite -start-insert buffer<cr>
nnoremap <leader>P :Unite -start-insert file_rec/async<cr>
cabbr writ ~/ag-sys/Else/everything/\#Another/ 

autocmd FileType unite call s:unite_settings()
function! s:unite_settings()
  nmap <buffer> <ESC> <Plug>(unite_exit)
endfunction



"fo table
" nmap cp :let @" = expand("%")<cr>
" can do ctrl r then % in insert mode instead

" Thanks to Ingo Karkat for answering my question ; http://stackoverflow.com/questions/21125170/grabbing-the-current-tab-name
cnoreabbr <expr> tabname t:taboo_tab_name
cnoreabbr <expr> buffername expand('%:t')
" for use in tabs named in taboo:
nnoremap <buffer> <space>u :Unite -quick-match bookmark:tabname<C-]><cr>
nnoremap <buffer> <space>U :UniteBookmarkAdd<cr>tabname<c-]><cr>buffername<c-]>
" adding new bookmark files to for default file
cabbr ubmark ~/.unite/bookmark/

" cycle to next sub category; c for cycle
" resource to auto change mapping

"stop complaining if not named
if exists("t:taboo_tab_name")
	if t:taboo_tab_name == "config"
		nnoremap <buffer> <space>c :TabooRename conf-music<cr>:so ~/.vimrc<cr>
	elseif t:taboo_tab_name == "conf-music"
		nnoremap <buffer> <space>c :TabooRename mail<cr>:so ~/.vimrc<cr>
	elseif t:taboo_tab_name == "mail"
		nnoremap <buffer> <space>c :TabooRename config<cr>:so ~/.vimrc<cr>
	endif
endif

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

" nmap <Leader>bb :ls<CR>:buffer<Space>

let g:unite_split_rule = "topleft"
" let g:unite_winheight = 10

" nnoremap <space>z :Unite -no-split -start-insert buffer tab file_mru directory_mru<cr>

" call unite#filters#matcher_default#use(['matcher_fuzzy'])
" http://www.codeography.com/2013/06/17/replacing-all-the-things-with-unite-vim.html
" http://eblundell.com/thoughts/2013/08/15/Vim-CtrlP-behaviour-with-Unite.html
" autocmd FileType unite call s:unite_settings()
"
" function! s:unite_settings()
  " let b:SuperTabDisabled=1
  " imap <buffer> <C-j>   <Plug>(unite_select_next_line)
  " imap <buffer> <C-k>   <Plug>(unite_select_previous_line)
  " imap <silent><buffer><expr> <C-x> unite#do_action('split')
  " imap <silent><buffer><expr> <C-v> unite#do_action('vsplit')
  " imap <silent><buffer><expr> <C-t> unite#do_action('tabopen')

  " nmap <buffer> <ESC> <Plug>(unite_exit)
" endfunction


" old way of doing things:
" if bufname("%") == "/home/angelic_sedition/dotfiles/common/.mpd/mpd.conf"
" 	nnoremap <buffer> <space>u :Unite bookmark:config
" elseif bufname("%") == "/home/angelic_sedition/dotfiles/terminal/.Xdefaults"
" 	nnoremap <buffer> <space>u :Unite bookmark:config
" else
" 	nnoremap <buffer> <space>u :Unite file
" endif

" http://bling.github.io/blog/2013/06/02/unite-dot-vim-the-plugin-you-didnt-know-you-need/
" nnoremap <space>/ :Unite ag:.<cr>

let g:unite_source_buffer_time_format=''
let g:unite_source_buffer_filename_format=''
" todo
" get arstdhneo for buffer quick-match ; esc to get out of
" file_mru.. buffer mru automaticarrly
" allows for bookmarks
	" let g:unite_source_history_yank_enable = 1
	" nnoremap <leader>y :<C-u>Unite history/yank<CR>
" multiple bookmarks make own sourec
" unite-sources
" can use with tabs :Unite tab and windows
" -no-split
" let g:unite_enable_use_short_source_names=1
" unite actions
"}}}

"'x-ray' view; cuc and cul
nnoremap <leader>x :set cursorcolumn<cr>:set cursorline<cr>
nnoremap <leader>x :set cursorcolumn!<cr>:set cursorline!<cr>

"tabs
nnoremap <leader>tt :tabnew<cr>
nnoremap <leader>q :q<cr>

"buffkill stuff
" move forward and back in buffer history
" maybe replace window navigation
nnoremap <leader>N :BB<cr>
nnoremap <leader>E :BF<cr>
" delete buffer and leave window open and switch to last used buffer (bufkill)
nnoremap <leader>d :BD<Return>
" delete buffer and close window
nnoremap <leader>D :BD<cr><c-w>c
" close buffer without closing window
" http://vim.wikia.com/wiki/Deleting_a_buffer_without_closing_the_window
" cabbr bc BClose
" create a new buffer in current window use :enew new window/buffer without split




" default tab naming behaviour
let g:taboo_modified_tab_flag='+'
let g:taboo_tab_format=' %N %m%f '
let g:taboo_renamed_tab_format=' %N [%f]%m '

nnoremap <leader>tr :TabooRename<space>
nnoremap <leader>tn :TabooOpen<space>

" Gundo
nnoremap <leader>u :GundoToggle<CR>


" Other Plugin Settings
" session management:
" don't need to turn of swp because now will load up all that arent'
" automatically deleted
" vim-session options (auto save on exit, open saved on open)
let g:session_autosave_periodic='yes'
let g:session_autosave='yes'
let g:session_autoload='yes'




"}}}


" Calendar Settings
let g:calendar_google_calendar = 1
let g:calendar_google_task = 1

" insertlessly
let g:insertlessly_cleanup_trailing_ws = 0
let g:insertlessly_cleanup_all_ws = 0

" Quickly edit/reload the vimrc file
nnoremap <silent> <leader>ov :e $MYVIMRC<CR>
nnoremap <leader>s :so ~/.vimrc<CR>
" $MYVIMRC<CR>
"}}}

" temp? graveyard
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
"=====================Vundle=====================
filetype off                   " required!

"required
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle; required
Bundle 'gmarik/vundle'

"Apearance
Bundle 'chriskempson/base16-vim'

" don't need anything complicated and like unite better than yankstack cycling
" Bundle 'maxbrunsfeld/vim-yankstack'
" Bundle  'YankRing'

" allows repeat (.) with plugins like sneak, surround, vim-abolish, etc.
Bundle 'tpope/vim-repeat'

" navigation
" sneak is generally faster or just as fast
Bundle 'justinmk/vim-sneak'
Bundle 'Lokaltog/vim-easymotion'
" Bundle 'goldfeld/vim-seek'

" add newlines with enter
Bundle 'dahu/Insertlessly'

" getting rid of nerdtree
" Bundle 'scrooloose/nerdtree'

Bundle 'tomtom/tcomment_vim'
Bundle 'bling/vim-airline'
Bundle 'tpope/vim-surround'

Bundle 'xolox/vim-session'
" required by vim-session
Bundle 'xolox/vim-misc'

Bundle 'itchyny/calendar.vim'

" tab for insert completion
" Bundle 'ervandew/supertab'

" buffer, mru, and file search
" Bundle 'kien/ctrlp.vim'
" no more fuzzy finder, lusty juggler, minbufexpl, buffer explorer, command t, etc. ; just unite
Bundle 'Shougo/unite.vim'
" for async file search
Bundle 'Shougo/vimproc.vim'

" tab renaming
Bundle 'gcmt/taboo.vim'
" for buffer history and killing buffers without changing window layout or closing
Bundle 'bufkill.vim'

" html generation
Bundle 'mattn/emmet-vim'


" auto put () with (, etc with cursor in middle; won't do in commented lines
Bundle 'Raimondi/delimitMate'


" coloured indents; changed mind
" Bundle 'nathanaelkane/vim-indent-guides'
" indentline maybe instead of 

"SnipMate
Bundle "MarcWeber/vim-addon-mw-utils"
Bundle "tomtom/tlib_vim"
Bundle "garbas/vim-snipmate"
" Optional:
Bundle "honza/vim-snippets"

" vim-scripts repos
"vim-bufferline pretty much the same thing
" Bundle "buftabs"


" better marks; show and symbols (markers)
Bundle 'kshenoy/vim-signature'

" view undo tree 
Bundle 'sjl/gundo.vim'

" non-GitHub repos
"fast file opening
"Bundle 'git://git.wincent.com/command-t.git'

Bundle 'vim-scripts/Smart-Tabs'

" interesting idea ; would like better if could get rid of flashing
" Bundle 'vim-scripts/SearchComplete'

" Git repos on your local machine (i.e. when working on your own plugin)
"Bundle 'file:///Users/gmarik/path/to/plugin'
" ...

" filetype plugin indent on     " required!
"
" Brief help
" :BundleList          - list configured bundles
" :BundleInstall(!)    - install (update) bundles
" :BundleSearch(!) foo - search (or refresh cache first) for foo
" :BundleClean(!)      - confirm (or auto-approve) removal of unused bundles
"
" see :h vundle for more details or wiki for FAQ
" NOTE: comments after Bundle commands are not allowed.

" #==============================
" # Graveyard
" #==============================

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
syntax on
