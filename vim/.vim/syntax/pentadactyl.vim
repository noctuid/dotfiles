" Vim syntax file
" Language:         Pentadactyl configuration file
" Maintainer:       Doug Kearns <dougkearns@gmail.com>

" TODO: make this pentadactyl specific - shared dactyl config?

if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

syn include @javascriptTop syntax/javascript.vim
unlet b:current_syntax

syn include @cssTop syntax/css.vim
unlet b:current_syntax

syn match pentadactylCommandStart "\%(^\s*:\=\)\@<=" nextgroup=pentadactylCommand,pentadactylAutoCmd

syn keyword pentadactylCommand ab[breviate] ab[clear] addo[ns] bN[ext] b[uffer] ba[ck] bd[elete] beep bf[irst] bl[ast] bma[rk]
    \ bmarks bn[ext] bp[revious] br[ewind] bufd[o] buffers bun[load] bw[ipeout] ca[bbrev] cabc[lear] cd chd[ir] cm[ap] cmapc[lear]
    \ cno[remap] colo[rscheme] com[mand] comc[lear] contexts cu[nmap] cuna[bbrev] delbm[arks] delc[ommand] delm[arks] delmac[ros]
    \ delqm[arks] dels[tyle] dia[log] dl do[autocmd] doautoa[ll] downl[oads] ec[ho] echoe[rr] echom[sg] em[enu] exe[cute]
    \ exta[dd] extd[isable] extde[lete] exte[nable] extens[ions] exto[ptions] extp[references] exts exu[sage] files fini[sh]
    \ fo[rward] frameo[nly] fw h[elp] helpa[ll] ha[rdcopy] hi[ghlight] hist[ory] hs ia[bbrev] iabc[lear] im[ap] imapc[lear]
    \ ino[remap] iu[nmap] iuna[bbrev] javas[cript] js ju[mps] keepa[lt] let loadplugins lpl ls ma[rk] macros map mapc[lear] marks
    \ mes[sages] messc[lear] mkp[entadactylrc] nm[ap] nmapc[lear] nno[remap] no[remap] noh[lsearch] norm[al] nu[nmap] o[pen]
    \ optionu[sage] pa[geinfo] pagest[yle] pas pref[erences] prefs pw[d] q[uit] qa[ll] qma[rk] qmarks quita[ll] re[draw]
    \ re[load] reloada[ll] res[tart] run runt[ime] sa[nitize] sav[eas] sb[ar] sb[open] sbcl[ose] scrip[tnames] se[t] setg[lobal]
    \ setl[ocal] sideb[ar] sil[ent] so[urce] st[op] stopa[ll] sty[le] styd[isable] styled[isable] stye[nable] stylee[nable]
    \ styt[oggle] stylet[oggle] tN[ext] t[open] tab taba[ttach] tabN[ext] tabc[lose] tabd[o] tabde[tach] tabdu[plicate] tabfir[st]
    \ tabl[ast] tabm[ove] tabn[ext] tabnew tabo[nly] tabopen tabp[revious] tabr[ewind] tabs tbh[ide] tbs[how] tbt[oggle] time
    \ tn[ext] toolbarh[ide] toolbars[how] toolbart[oggle] tp[revious] u[ndo] una[bbreviate] undoa[ll] unl[et] unm[ap] verb[ose]
    \ ve[rsion] vie[wsource] viu[sage] vm[ap] vmap[clear] vno[remap] vu[nmap] w[rite] wc[lose] win[open] winc[lose] wind[ow]
    \ winon[ly] wo[pen] wq wqa[ll] xa[ll] zo[om]
    \ contained

syn match pentadactylCommand "!" contained

syn keyword pentadactylAutoCmd au[tocmd] contained nextgroup=pentadactylAutoEventList skipwhite

syn keyword pentadactylAutoEvent BookmarkAdd ColorScheme DOMLoad DownloadPost Fullscreen LocationChange PageLoadPre PageLoad
    \ PrivateMode Sanitize ShellCmdPost Enter LeavePre Leave
    \ contained

syn match pentadactylAutoEventList "\(\a\+,\)*\a\+" contained contains=pentadactylAutoEvent

syn region pentadactylSet matchgroup=pentadactylCommand start="\%(^\s*:\=\)\@<=\<\%(setl\%[ocal]\|setg\%[lobal]\|set\=\)\=\>"
    \ end="$" keepend oneline contains=pentadactylOption,pentadactylString

syn keyword pentadactylOption act activate altwildmode au autocomplete awim cd cdpath complete cpt defsearch ds editor eht
    \ ei enc encoding eventignore extendedhinttags fenc fh fileencoding followhints go guioptions helpfile hf hi hin hintinputs
    \ hintkeys hintmatching hinttags hinttimeout history hk hm ht hto laststatus loadplugins lpl ls maxitems messages msgs newtab
    \ nextpattern pa pageinfo popups pps previouspattern rtp runtimepath sanitizeitems sanitizetimespan scr scroll sh shcf shell
    \ shellcmdflag showstatuslinks showtabline si ssli stal sts suggestengines titlestring urlseparator vbs verbose wia wic wig
    \ wildanchor wildcase wildignore wildmode wildoptions wildsort wim wis wop wordseparators wsp
    \ contained nextgroup=pentadactylSetMod

" toggle options
syn match pentadactylOption "\<\%(no\|inv\)\=\%(banghist\|bh\|errorbells\|eb\|exrc\|ex\|fullscreen\|fs\|ignorecase\|ic\)\>!\="
    \ contained nextgroup=pentadactylSetMod
syn match pentadactylOption "\<\%(no\|inv\)\=\%(incsearch\|is\|insertmode\|im\|hlsearch\|hls\|jsd\|jsdebugger\)\>!\="
    \ contained nextgroup=pentadactylSetMod
syn match pentadactylOption "\<\%(no\|inv\)\=\%(linksearch\|lks\|more\|online\|pornmode\|private\|showmode\|smd\)\>!\="
    \ contained nextgroup=pentadactylSetMod
syn match pentadactylOption "\<\%(no\|inv\)\=\%(smartcase\|scs\|strictfocus\|sf\|online\|visualbell\|vb\|usermode\|um\)\>!\="
    \ contained nextgroup=pentadactylSetMod

syn match pentadactylSetMod "\%(\<[a-z_]\+\)\@<=&" contained

syn region pentadactylJavaScript start="\%(^\s*\%(javascript\|js\)\s\+\)\@<=" end="$" contains=@javascriptTop keepend oneline
syn region pentadactylJavaScript matchgroup=pentadactylJavaScriptDelimiter
    \ start="\%(^\s*\%(javascript\|js\)\s\+\)\@<=<<\s*\z(\h\w*\)"hs=s+2 end="^\z1$" contains=@javascriptTop fold

let s:cssRegionStart = '\%(^\s*sty\%[le]!\=\s\+\%(-\%(n\|name\)\%(\s\+\|=\)\S\+\s\+\)\=[^-]\S\+\s\+\)\@<='
execute 'syn region pentadactylCss start="' . s:cssRegionStart . '" end="$" contains=@cssTop keepend oneline'
execute 'syn region pentadactylCss matchgroup=pentadactylCssDelimiter'
    \ 'start="' . s:cssRegionStart . '<<\s*\z(\h\w*\)"hs=s+2 end="^\z1$" contains=@cssTop fold'

syn match pentadactylNotation "<[0-9A-Za-z-]\+>"

syn match   pentadactylComment +".*$+ contains=pentadactylTodo,@Spell
syn keyword pentadactylTodo FIXME NOTE TODO XXX contained

syn region pentadactylString start="\z(["']\)" end="\z1" skip="\\\\\|\\\z1" oneline

syn match pentadactylLineComment +^\s*".*$+ contains=pentadactylTodo,@Spell

" NOTE: match vim.vim highlighting group names
hi def link pentadactylAutoCmd               pentadactylCommand
hi def link pentadactylAutoEvent             Type
hi def link pentadactylCommand               Statement
hi def link pentadactylComment               Comment
hi def link pentadactylJavaScriptDelimiter   Delimiter
hi def link pentadactylCssDelimiter          Delimiter
hi def link pentadactylNotation              Special
hi def link pentadactylLineComment           Comment
hi def link pentadactylOption                PreProc
hi def link pentadactylSetMod                pentadactylOption
hi def link pentadactylString                String
hi def link pentadactylTodo                  Todo

let b:current_syntax = "pentadactyl"

let &cpo = s:cpo_save
unlet s:cpo_save
