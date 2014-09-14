#!/bin/sh
# [3.9.14] created
# My Replacement for TabsOutliner with Pentadactyl (url saving and organization for later viewing)
# bound in .pentadactylrc for saving of urls to vimwiki files (for easy opening and viewing in vim)
# also bound so that when close a tab with d any corresponding urls will be removed from list (like in TO)

cd ~/vimwiki
url=$(xsel -b)

if [ "$1" == "add" ]; then
	# make the topic file if it does not exist and append the url to it
	if [ -f ~/vimwiki/$2.wiki ];
	then
		echo $url >> ~/vimwiki/$2.wiki
	else
		touch ~/vimwiki/$2.wiki
		echo $url >> ~/vimwiki/$2.wiki
	fi
elif [ "$1" == "delete" ]; then
	# find all files and create array for them
	# http://stackoverflow.com/questions/8768420/how-to-convert-command-output-to-an-array-line-by-line-in-bash
	IFS=$'\n'; files=( $(find ~/vimwiki/ -type f) )
	for file in "${files[@]}"
	do
		# then delete any url matches in any of them with sed along with newline; first use sed to escape the url
		# http://stackoverflow.com/questions/407523/escape-a-string-for-sed-search-pattern
		# use "" for variable with sed...http://askubuntu.com/questions/76808/how-to-use-variables-in-sed
		sed -i.bak "s/$(echo $url | sed -e 's/\\/\\\\/g' -e 's/\//\\\//g' -e 's/&/\\\&/g')//g" "$file"
		# remove empty lines (not going to put another sed above...)
		# http://stackoverflow.com/questions/16414410/delete-empty-lines-using-sed
		sed -i.bak '/^\s*$/d' $file
		# remove bak files
		rm ~/vimwiki/*.bak
	done
elif [ "$1" == "open_win" ]; then
	bspc rule -a termite -o floating=true center=true
	termite -e "vim -c 'nnoremap q :q<cr>' index.wiki" &
fi

# empty index and add all wiki files to
cat /dev/null > index.wiki
for file in *.wiki
do
	# get rid of wiki extension
	item=${file/\.wiki/}
	printf '%s\n' "$item" >> index.wiki
done
