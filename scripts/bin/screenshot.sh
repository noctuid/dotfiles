#!/bin/bash

# get window title
# remove punctuation (e.g.forward slashes) and replace whitespace with underscores
title=$(xtitle | awk -F "-" '{ gsub(/[[:punct:]]/, "") gsub(/\s/, "_"); print $1}')

if [[ $1 == select ]]; then
	# if use -s won't be able to use $W and $H in name
	eval "$(slop)"
	maim -g "$G" ~/Move/Screenshots/"$(date +%Y.%m.%d_%H:%M:%S)"_-_"$title"_"$W"x"$H".png
else
	maim ~/Move/Screenshots/"$(date +%Y.%m.%d_%H:%M:%S)"_-_"$title".png
fi
