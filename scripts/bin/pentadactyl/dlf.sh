#!/bin/bash
# TODO:
# - rest and jq?
# - sorted? or sort before piping into uniq
# - automate comic downloads

site=$1
url=$2

takedir() {
	mkdir -p "$1"
	cd "$1"
}

takedir ~/database/move

if [[ $site == *chan ]]; then
	thread=$(curl "$url")
	if [[ $site == 4chan ]]; then 
		# downloads images, gifs, and webms in thread
		# alternatively https://github.com/youurayy/4chan for features like watching
		title=$(echo "$thread" | grep -o '<meta name="description" content="[^"]*' | \
			sed -e "s/^.*content=\"//" -e "s/ -.*//")
		takedir 4chan/"${title//[\/ ]/_}"
		echo "$thread" | grep -o '<a class="fileThumb" href="//i.4cdn.org/[^"]*' | \
			sed "s%^.*href=\"%https:%" > tmp_urls
	else
		title=$(echo "$thread" | grep -o '<title>[^<]*' | sed 's/^<title>//')
		takedir 8chan/"${title//[\/ ]/_}"
		# doesn't work on boards that don't have the media.8ch part
		# haven't encountered any such boards that I would want to use
		echo "$thread" | grep -oE '<a href="https://media.8ch.net/[[:lower:]]*/src/[^"]*' | \
			sed "s/<a href=\"//" | uniq > tmp_urls
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
