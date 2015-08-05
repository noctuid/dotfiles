#!/bin/bash
# TODO:
# find out max page instead requiring it (look at class="page-sep")

# Downloads all threads for a forumotion subform
# (give the url for the last page in the subforum as the argument)
# Won't download past 999 posts in one thread (not relevant to my usage of it)
# Only tested on one forum

url=$1
base_url=${url//\.com*/\.com\/}
save_urls=""

# internet required
tries=0
while [[ -z $(ping -c 1 www.google.com) ]] && [[ $tries -lt 3 ]]; do
	notify-send "Internet required to backup a forumotion subforum."
	sleep 120
	((tries++))
done

# curl the urls of all subforum pages
if echo "$url" | grep -qE '\.com/f[0-9]+?p[0-9]+?'; then
	url_path=${url//*\.com\//}
	subforum_num=$(echo "$url_path" | awk -F "-" 'gsub("p[0-9]+?","") {print $1}')
	post_num=$(echo "$url_path" | awk -F "p|-" '{print $2}')
	subforum_title=$(echo "$url_path" | sed -r 's/^f[0-9]{1,5}p[0-9]{2,3}//')
	current_post=0
	while [[ $current_post -lt $post_num ]]; do
		if [[ $current_post -eq 0 ]]; then
			new_url=$base_url${subforum_num}$subforum_title
		else
			new_url=$base_url${subforum_num}p$current_post$subforum_title
		fi
		save_urls+=$(curl "$new_url" | \
			grep -oE '<a href="[[:alnum:][:punct:]]+?"><img class="sprite-icon_reply"')
		# assumes 50 threads a page for the forum settings
		((current_post+=50))
	done
fi
save_urls+=$(curl "$url" | grep -oE '<a href="[[:alnum:][:punct:]]+?"><img class="sprite-icon_reply"')

# clear a previous list
> urls.txt

# get the urls of every thread
while read -r line; do
	title=$(echo "$line" | sed -e 's%<a href="/%%' -e 's/"><img class=.*//')
	multiple_pages=$(echo "$title" | grep -oE "[0-9]{1,5}p[0-9]{2,3}")
	if [[ -n $multiple_pages ]]; then
		thread_num=$(echo "$title" | awk -F "-" 'gsub("p[0-9]+?","") {print $1}')
		post_num=$(echo "$title" | awk -F "p|-" '{print $2}')
		thread_title=$(echo "$title" | sed -r 's/^t[0-9]{1,5}p[0-9]{2,3}//')
		current_post=0
		while [[ $current_post -le $post_num ]]; do
			echo "$base_url${thread_num}p$current_post$thread_title" >> urls.txt
			# assumes 15 posts a page for the forum settings
			((current_post+=15))
		done
	else
		echo "$base_url$title" >> urls.txt
	fi
done <<< "$save_urls"

uniq < urls.txt > unique_urls.txt
if [[ -s unique_urls.txt ]]; then
	aria2c -i unique_urls.txt
else
	notify-send "Forumotion backup failed."
	exit 1
fi
