" Vim filetype plugin file
" Language:    Pentadactyl configuration file
" Maintainer:  Doug Kearns <dougkearns@gmail.com>
" Version:     hg6884

if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

let s:cpo_save = &cpo
set cpo&vim

let b:undo_ftplugin = "setl com< cms< fo< ofu< | unlet! b:browsefilter"

setlocal comments=:\"
setlocal commentstring=\"%s
setlocal formatoptions-=t formatoptions+=croql
setlocal omnifunc=syntaxcomplete#Complete

if has("gui_win32") && !exists("b:browsefilter")
    let b:browsefilter = "Pentadactyl Config Files (*.penta)\t*.penta\n" .
        \ "All Files (*.*)\t*.*\n"
endif

let &cpo = s:cpo_save
unlet s:cpo_save
