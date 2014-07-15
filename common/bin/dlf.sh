#!/bin/sh
# happy fun download time
cd ~/database/move

site=$1
url=$2

if [ "$site" == "fakku" ]; then
	# mkdir -p fakku && cd fakku
	# fakku.py (renamed without the .py or change below) in path: https://github.com/darkfeline/fakku_downloader
	# it broke; using comic
	# fakku $url &
elif [ "$site" == "gel" ]; then
	# in aur https://code.google.com/p/danbooru-v7sh-grabber/
	mkdir -p gelbooru && cd gelbooru
	tag=${url##*=}
	echo $tag
	danbooru-grabber -e gelbooru -dfn post:id_post:widthxpost:height -d "$tag"
elif [ "$site" == "4chan" ]; then
	# https://github.com/ypocat/4chan
	shit=$(xtitle)
	# remove titlestring
	thread_title=${shit%-*}
	# remove slashes
	thread_title_dir=$(echo "$thread_title" | sed "s/\///g")
	mkdir -p 4chan/"$thread_title_dir" && cd 4chan/"$thread_title_dir"
	if [ "$3" == "watch" ]; then
		4chan $url
	else
		4chan -s $url
	fi
elif [ "$site" == "comic" ]; then
	# use dta with webcomic reader; downside- have to name folder (upside.. works with hundreds of sites)
	# https://github.com/ameboide/webcomic_reader
	# move away from content
	xdotool mousemove 1316 708
	# right click and open dta window
	xdotool click 3 && xdotool mousemove_relative -- 0 -180 && sleep 0.1 && xdotool click 1
	sleep 0.3
	# highlight download dir for changing; then just hit enter
	xdotool mousemove 282 505 && xdotool mousedown 1 && xdotool mousemove 475 505 && xdotool mouseup 1
else
	bspc rule -a termite -o floating=true center=true
	termite -e "/bin/zsh -c 'xdo resize -w +300 && xdo move -x -150 && xdo resize -h +200 && xdo move -y -100 && ranger'" &
fi
