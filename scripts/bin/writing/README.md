# Simple Text File Markup Specification

| Symbols   |                                                   |
| --------- | ------------------------------------------------- |
| \#        | comment character                                 |
| \_text\_  | italics                                           |
| \*text\*  | bold                                              |
| ^text     | chapter title                                     |
| +text     | subtext to chapter title                          |
| ..        | placeholder; replaced with underscores            |
| ...       | ellipsis; replaced with actual ellipsis character |


## Viewing in vim
Comments, italics, and emboldened text are visible with vim syntax.

See
`vim/.vimrc`
`vim/.vim/syntax/text.vim`

## Viewing as pdf
For generating a pdf, I quickly threw together a hideous bash script without thinking. It was slow (20 seconds for around 8k words) and badly written (checks every character one at a time), so I threw together a python implementation (which was much easier and takes essentially no time for the 8k word file I tested it on).

They both work by creating an intermediate `output.tex` file using a template. I've included an example pdf generated with `prose2pdf -e example.txt`.

See `output.pdf`

####May add :
- block quote
- subscript and superscript
- another quote nesting level

#### Notes
Every line that isn't a chapter title or subtext is a paragraph. Leading whitespace and newlines don't matter. 

Quotations should be surrounded by either " or ' unless in section/chapter headings (left quote is ` and right quote is ' then). Dirtytalk is used to format the quotations properly. One level of nested quotes is supported. The number of quotes must match, and single quotes preceded by a letter (e.g. Jack's inflamed sense of rejection) are ignorned.

Only hashes, backslashes, underscores, asterisks, and ^ or + at the start of the line need to be escaped to be specified as the literal characters. Other characters should not be escaped.
