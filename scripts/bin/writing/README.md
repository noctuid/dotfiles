## Org Mode Based Syntax
I've switched to using org mode and vim-dotoo for writing because of features like clocking, capture, and a clearer outline structure (as opposed to many fold markers, though they are easier to deal with since they are commented and more region specific with an end marker). This also means the markup syntax used by the `prose2pdf` script is more widely used, and I don't have to do setup bold, underline, and italic display in the editor myself. Emacs can export to pdf/LaTeX already, but I've already written this and would still need a script to convert to "smart" quotes among other things (I prefer not to have smart quotes in files). It should be noted that there are typographical quote plugins for both [emacs](https://github.com/jorgenschaefer/typoel) and [vim](https://github.com/reedes/vim-textobj-quote) though. I may end up using emacs' export at least partially if I ever want things like parts.

There are also a few differences with org mode syntax. There is no strikethrough or verbatim for one. Also, if you leave off a closing symbol (e.g. for bold), it will extend to the end of the line.

These scripts are kind of ugly right now. I'm still not sure if they are how I'm going to end up doing things, but I may update them to be more sane/accurate in the future.

# Simple Text File Markup Specification

| Symbols   |                                                   |
| --------- | ------------------------------------------------- |
| \#        | comment character                                 |
| \#\#\#    | comment character for all preceding text          |
| \_text\_  | underline                                         |
| /text/    | italics                                           |
| \*text\*  | bold                                              |
| \* text   | chapter title (asterisk at sol; can specify num)  |
| \*\* text | subtext to chapter title (one more asterisk)      |
| ..        | placeholder; replaced with underscores            |
| ...       | ellipsis; replaced with actual ellipsis character |

## Word Usage Analysis and Word/Sentence/Paragraph Counting
The `writing_stats` script will print the number of times words appear in a file ordered by frequency. It optionally accepts a list of words to ignore or a list of words to only check for. It also allows specifying a chapter range to limit the check to. This could be used, for example, to check that unusual words aren't used too often, especially in close proximity. However, something like [wordlint](https://github.com/gbgar/Wordlint) or [diction](https://aur.archlinux.org/packages.php?ID=21963) might better serve this purpose.

The script can also instead print information about the number of words, sentences (taking into account abbreviations), paragraphs (lines), and chapters. A chapter range is also allowed. It can also output a valid markdown table giving information for each chapter (which will also be aligned in the terminal). Example:

![Alt text](https://raw.github.com/angelic-sedition/dotfiles/master/scripts/bin/writing/writing_stats_table.png "table output")

| Chapter       | Words         | Sentences     | Paragraphs    |
| ------------- | ------------- | ------------- | ------------- |
|       1       |       162     |       19      |       13      |
|       2       |       67      |       8       |       6       |
| Total:        |       229     |       27      |       19      |

## Viewing as pdf
For generating a pdf, I quickly threw together a hideous bash script without thinking. It was slow (20 seconds for around 8k words) and badly written (checked every character one at a time), so I threw together a python implementation which was much easier/quicker to do and takes essentially no time for anything I've tested it on.

It creates a `output.tex` file using a template. I've included an example pdf generated with `prose2pdf -e -l 1 example.txt`.

See `output.pdf`

#### May add:
- block quote
- subscript and superscript
- another quote nesting level
- use emacs export for creating the body, so don't have to worry about some of this

#### Notes
Every line that isn't a chapter title or subtext is a paragraph. Leading whitespace and newlines don't matter.

To be considered a chapter or section heading, asterisks must be followed by a space like in org mode. Note that using multiple asterisks in a row anywhere besides a heading will potentially mess up what is emboldened on that line. When using bold, the asterisk must be directly before a letter and not a space. Also note that if the number of asterisks used for a chapter is not 3, the number must be specified for `prose2pdf` and `writing_stats` to work properly.

Quotations are denoted by either " or ' unless in section/chapter headings (left quote is ` and right quote is ' then). Dirtytalk is used to format the quotations properly (to curly quotes). One level of nested quotes is supported. The number of quotes must match, and single quotes preceded by a letter (e.g. Jack's inflamed sense of rejection) are ignored. The previously mentioned plugins could be used to automatically type typographical quotes instead.

Only hashes, backslashes, underscores, asterisks, and forward slashes need to be escaped to be specified as the literal characters. Other characters should not be escaped.
