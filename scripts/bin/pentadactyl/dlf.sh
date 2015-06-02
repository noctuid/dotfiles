#!/bin/bash
# happy fun download time

site=$1
url=$2

takedir() {
	mkdir -p "$1"
	cd "$1"
}

takedir ~/database/move

# if [[ $site == fakku ]]; then
	# mkdir -p fakku && cd fakku
	# fakku.py (renamed without the .py or change below) in path: https://github.com/darkfeline/fakku_downloader
	# it broke; using comic
	# fakku $url &
if [[ $site == gel ]]; then
	# in aur https://code.google.com/p/danbooru-v7sh-grabber/
	takedir gelbooru
	tag=${url##*=}
	danbooru-grabber -e gelbooru -dfn post:id_post:widthxpost:height -d "$tag"
elif [[ $site == *chan ]]; then
	thread=$(curl "$url")
	if [[ $site == 4chan ]]; then 
		# downloads images, gifs, and webms in thread
		# alternatively https://github.com/youurayy/4chan for features like watching
		title=$(echo "$thread" | grep -o '<meta name="description" content="[^"]*' | sed -e "s/^.*content=\"//" -e "s/ -.*//")
		takedir 4chan/"${title//[\/ ]/_}"
		echo "$thread" | grep -o '<a class="fileThumb" href="//i.4cdn.org/[^"]*' | sed "s%^.*href=\"%https:%" > tmp_urls
	else
		title=$(echo "$thread" | grep -o '<title>[^<]*' | sed 's/^<title>//')
		takedir 8chan/"${title//[\/ ]/_}"
		echo "$thread" | grep -o '<a href="https://media.8ch.net/tech/src/[^"]*' | sed "s/<a href=\"//" | uniq > tmp_urls
	fi
	aria2c -i tmp_urls
	rm tmp_urls
elif [[ $site == comic ]]; then
	# use dta with webcomic reader; downside- have to name folder (upside.. works with hundreds of sites)
	# https://github.com/ameboide/webcomic_reader
	# use emenu to open dta
	sleep 0.3
	# highlight download dir for changing; then just hit enter; this is crap.. but it works
	xdotool mousemove 275 522 && xdotool mousedown 1 && xdotool mousemove 551 530 && xdotool mouseup 1
fi
