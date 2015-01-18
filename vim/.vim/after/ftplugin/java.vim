setlocal tabstop=4 softtabstop=4 shiftwidth=4
setlocal expandtab
setlocal autoindent
" for anonymous classes
setlocal cinoptions+=j1

let java_highlight_debug=1

" abbreviations
inoreabbr <buffer> psvm public static void main(String[] args)

" vim-dispatch
let b:dispatch = 'javac %'
