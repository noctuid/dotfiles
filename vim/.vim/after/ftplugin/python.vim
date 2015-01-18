setlocal tabstop=4 softtabstop=4 shiftwidth=4
setlocal expandtab
setlocal autoindent
setlocal foldmethod=indent
" :h fo-table
setlocal formatoptions=croq
setlocal textwidth=80

" mappings/abbreviations
inoreabbr <buffer> shb #!/usr/bin/env python3
inoreabbr <buffer> nmm if __name__== "__main__": main()

nnoremap <buffer> ,kr :!xterm -e "%:p;read -n 1" &<cr>
