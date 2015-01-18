inoreabbr <buffer> ioh #include <stdio.h>
inoreabbr <buffer> mnfn int main(int argc, char *argv[])
nnoremap <buffer> ,kr :!gcc -Wall -g -o %:r %:p<cr>:!xterm -e "%:p:r;read -n 1" &<cr>
