" https://github.com/tpope/vim-markdown
" Copyright © Tim Pope. Distributed under the same terms as Vim itself. See :help license.
" I've just taken the highlighting for bold and italics and added \n as an end
" so that won't change a bunch of lines below if there's no matching

syn region markdownItalic start="\S\@<=\*\|\*\S\@=" end="\S\@<=\*\|\*\S\@=\|\n" keepend contains=markdownLineStart
syn region markdownItalic start="\S\@<=_\|_\S\@=" end="\S\@<=_\|_\S\@=\|\n" keepend contains=markdownLineStart
syn region markdownBold start="\S\@<=\*\*\|\*\*\S\@=" end="\S\@<=\*\*\|\*\*\S\@=\|\n" keepend contains=markdownLineStart,markdownItalic
syn region markdownBold start="\S\@<=__\|__\S\@=" end="\S\@<=__\|__\S\@=\|\n" keepend contains=markdownLineStart,markdownItalic
syn region markdownBoldItalic start="\S\@<=\*\*\*\|\*\*\*\S\@=" end="\S\@<=\*\*\*\|\*\*\*\S\@=\|\n" keepend contains=markdownLineStart
syn region markdownBoldItalic start="\S\@<=___\|___\S\@=" end="\S\@<=___\|___\S\@=\|\n" keepend contains=markdownLineStart

hi def link markdownItalic                htmlItalic
hi def link markdownBold                  htmlBold
hi def link markdownBoldItalic            htmlBoldItalic
hi def link markdownCodeDelimiter         Delimiter
