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

## Word Usage Analysis and Word/Sentence/Paragraph Counting
The `writing_stats` script will print the number of times words appear in a file ordered by frequency. It optionally accepts a list of words to ignore or a list of words to only check for. It also allows specifying a chapter range to limit the check to. This could be used, for example, to check that unusual words aren't used too often, especially in close proximity.

The script can also instead print information about the number of words, sentences, paragraphs, and chapters. A chapter range is also allowed with this --count flag/option. It can also output a valid markdown table giving information for each chapter. Example:

![Alt text](https://raw.github.com/angelic-sedition/dotfiles/master/scripts/bin/writing/writing_stats_table.png "table output")

| Chapter	| Words 	| Sentences	| Paragraphs	|
| ------------- | ------------- | ------------- | ------------- |
|	1	|	54	|	8	|	3	|
|	2	|	37	|	3	|	3	|
| Total:	|	91	|	11	|	6	|

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

Quotations should be surrounded by either " or ' unless in section/chapter headings (left quote is ` and right quote is ' then). Dirtytalk is used to format the quotations properly (to curly quotes). One level of nested quotes is supported. The number of quotes must match, and single quotes preceded by a letter (e.g. Jack's inflamed sense of rejection) are ignorned. [vim-textobj-quote](https://github.com/reedes/vim-textobj-quote) may also be worth looking at

Only hashes, backslashes, underscores, asterisks, and ^ or + at the start of the line need to be escaped to be specified as the literal characters. Other characters should not be escaped.
