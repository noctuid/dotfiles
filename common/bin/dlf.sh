#!/bin/sh
# happy fun download time
cd ~/database/move

site=$1
url=$2

if [ "$site" == "fakku" ]; then
	mkdir -p fakku && cd fakku
	# fakku.py (renamed without the .py or change below) in path: https://github.com/darkfeline/fakku_downloader
	fakku $url &
elif [ "$site" == "gel" ]; then
	# in aur https://code.google.com/p/danbooru-v7sh-grabber/
	mkdir -p gelbooru && cd gelbooru
	tag=${url##*=}
	echo $tag
	danbooru-grabber -e gelbooru -dfn post:id_post:widthxpost:height -d "$tag"
elif [ "$site" == "4chan" ]; then
	shit=$(xtitle)
	# remove titlestring
	thread_title=${shit%-*}
	# remove slashes
	thread_title_dir=$(echo "$thread_title" | sed "s/\///g")
	mkdir -p 4chan/"$thread_title_dir" && cd 4chan/"$thread_title_dir"
	# https://github.com/ypocat/4chan
	if [ "$3" == "watch" ]; then
		4chan $url
	else
		4chan -s $url
	fi
else
	bspc rule -a termite -o floating=true center=true
	termite -e "/bin/zsh -c 'xdo resize -w +300 && xdo move -x -150 && xdo resize -h +200 && xdo move -y -100 && ranger'" &
fi
